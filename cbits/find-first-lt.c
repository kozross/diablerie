#include <stdint.h>
#include <stddef.h>

// 1. load r1 address
// 2. load r2 anotherAddresss
// 3. add r1 r2 r3 | [r1] + [r2] -> [r3]
// 4. add r3 r1 r2 | [r3] + [r1] -> [r2]
// 5. add r3 r1 r1 | [r3] + [r1] -> [r1]
//
// Imagined order: 1 -> 2 -> 3 -> 4 -> 5 (5 steps)
// (Possible) real order 1: 1,2 -> 3 -> 4, 5 (3 steps)

// Fill every 'lane' with the same value.
static inline uint64_t broadcast (uint8_t const x) {
  return x * 0x0101010101010101ULL;
}

// Based on https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
static inline ptrdiff_t find_first_zero (uint8_t const * const src,
                                         size_t const off,
                                         size_t const len) {
  size_t const big_strides = len / 16;
  size_t const little_strides = len % 16;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  uint64_t const mask_01 = broadcast(0x01);
  uint64_t const mask_80 = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const inputs[2] = {
      *big_ptr,
      *(big_ptr + 1)
    };
    uint64_t const results[2] = {
      (((inputs[0]) - mask_01) & ~inputs[0] & mask_80) != 0,
      (((inputs[1]) - mask_01) & ~inputs[1] & mask_80) != 0 
    };
    if (results[0] || results[1]) {
      for (size_t j = 0; j < 16; j++) {
        if ((*ptr) == 0) {
          return ptr - src;
        }
        ptr++;
      }
    } else {
      ptr += 16;
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
