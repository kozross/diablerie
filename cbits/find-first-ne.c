#include <stddef.h>
#include <stdint.h>

static inline ptrdiff_t find_first_nonzero (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len) {
  // We go four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    // We read, and OR together, four blocks. If this is nonzero, then there was
    // a nonzero byte somewhere.
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const result = (*big_ptr) |
                            (*(big_ptr + 1)) |
                            (*(big_ptr + 2)) |
                            (*(big_ptr + 3));
    if (result != 0) {
      // Dig manually.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 32;
  }
  // If we got this far, finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
      return ptr - src;
    }
    ptr++;
  }
  // If we got this far, we found nothing.
  return -1;
}

// Fill every 8-bit 'lane' with the same value.
static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

static inline ptrdiff_t find_first_mismatch (uint8_t const * const src,
                                             size_t const off,
                                             size_t const len,
                                             uint8_t const byte) {
  // We go four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  uint64_t const matches = broadcast(byte);
  for (size_t i = 0; i < big_strides; i++) {
    // For each block we read, we XOR with the same byte in all positions. This
    // will be 0 if we match, and nonzero otherwise. Then we OR four such blocks
    // together; if the result is nonzero, we have a mismatch somewhere.
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const result = ((*big_ptr) ^ matches) |
                            ((*(big_ptr + 1)) ^ matches) |
                            ((*(big_ptr + 2)) ^ matches) |
                            ((*(big_ptr + 3)) ^ matches);
    if (result != 0) {
      // Dig manually.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) != byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 32;
  }
  // If we got this far, finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != byte) {
      return ptr - src;
    }
    ptr++;
  }
  // If we got this far, we found nothing.
  return -1;
}

ptrdiff_t find_first_ne (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0x00) {
    return find_first_nonzero(src, off, len);
  }
  return find_first_mismatch(src, off, len, byte);
}
