#include <stddef.h>
#include <stdint.h>

static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

static inline ptrdiff_t find_first_non_ascii (uint8_t const * const src,
                                              size_t const off,
                                              size_t const len) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // We process two 64-bit word at a time.
  // That's 8 bytes times 2 = 16.
  size_t big_strides = len / 16;
  size_t const small_strides = len % 16;
  uint64_t const mask = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const result = ((*big_ptr) & mask) | ((*(big_ptr + 1)) & mask);
    if (result != 0) {
      for (size_t j = 0; j < 16; j++) {
        if ((*ptr) > 0x7F) {
          return ptr - src;
        }
      }
    }
    ptr += 16;
  }
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > 0x7F) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_nonzero (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  size_t const big_strides = len / 16;
  size_t const small_strides = len % 16;
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    if (((*big_ptr) | (*(big_ptr + 1))) != 0) {
      for (size_t j = 0; j < 16; j++) {
        if (*(ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 16;
  }
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
      return ptr - src;
    }
    ptr++;
  }
  return -1;
}

static inline ptrdiff_t find_first_slow (uint8_t const * const src,
                                         size_t const off,
                                         size_t const len,
                                         uint8_t const byte) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < len; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  return -1;
}

ptrdiff_t find_first_gt (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0x00) {
    return find_first_nonzero(src, off, len);
  }
  else if (byte == 0xFF) {
    return -1;
  }
  else if (byte == 0x7F) {
    return find_first_non_ascii(src, off, len);
  }
  else {
    return find_first_slow(src, off, len, byte);
  }
  /*
  if (byte < 0x7F) {
    return find_first_small(src, off, len, byte);
  }
  else if (byte == 0x7F) {
    return find_first_non_ascii(src, off, len);
  }
  else {
    return find_first_slow(src, off, len, byte);
  }
  */
}
