#include <stdint.h>
#include <stddef.h>

// Fill every 'lane' with the same value.
static inline uint64_t broadcast (uint8_t const x) {
  return x * 0x0101010101010101ULL;
}

// Based on https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
static inline ptrdiff_t find_first_zero (uint8_t const * const src,
                                         size_t const off,
                                         size_t const len) {
  size_t const big_strides = len / 8;
  size_t const little_strides = len % 8;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  uint64_t const mask_01 = broadcast(0x01);
  uint64_t const mask_80 = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const input = *big_ptr;
    if ((((input) - mask_01) & ~input & mask_80) != 0) {
      for (size_t j = 0; j < 8; j++) {
        if ((*ptr) == 0) {
          return ptr - src;
        }
        ptr++;
      }
    } else {
      ptr += 8;
    }
  }
  for (size_t i = 0; i < little_strides; i++) {
    if ((*ptr) == 0) {
      return ptr - src;
    }
    ptr++;
  }
  // We missed.
  return -1;
}

ptrdiff_t find_first_lt (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0x00) {
    return -1;
  }
  if (byte == 0x01) {
    return find_first_zero(src, off, len);
  }
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < len; i++) {
    if ((*ptr) < byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We missed.
  return -1;
}
