#include <stddef.h>
#include <stdint.h>

static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

static inline ptrdiff_t find_last_zero (uint8_t const * const src,
                                        size_t const off,
                                        size_t const len) {
  // We go four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  // Start at the end
  uint8_t const * ptr = (uint8_t const *)&(src[off + len - 1]);
  // We use the method described in "Bit Twiddling Hacks".
  // Source: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
  uint64_t const mask = broadcast(0x7F);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)(ptr - 7);
    uint64_t const inputs[4] = {
        *big_ptr,
        *(big_ptr - 1),
        *(big_ptr - 2),
        *(big_ptr - 3)
    };
    uint64_t const tmps[4] = {
      (inputs[0] & mask) + mask,
      (inputs[1] & mask) + mask,
      (inputs[2] & mask) + mask,
      (inputs[3] & mask) + mask
    };
    uint64_t const result = (~(tmps[0] | inputs[0] | mask)) | 
                            (~(tmps[1] | inputs[1] | mask)) |
                            (~(tmps[2] | inputs[2] | mask)) |
                            (~(tmps[3] | inputs[3] | mask));
    // Any bits set means we found a match.
    if (result != 0) {
      // Search backwards until we find the match.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) == 0) {
          return ptr - src;
        }
        ptr--;
      }
    }
    ptr -= 32;
  }
  // If we still haven't found anything, finish the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == 0) {
      return ptr - src;
    }
    ptr--;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_last_eq_nonzero (uint8_t const * const src,
                                              size_t const off,
                                              size_t const len,
                                              uint8_t const byte) {
  // We go four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  // Start at the end
  uint8_t const * ptr = (uint8_t const *)&(src[off + len - 1]);
  // We use the method described in "Bit Twiddling Hacks".
  // Source: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
  uint64_t const matches = broadcast(byte);
  uint64_t const mask = broadcast(0x7F);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)(ptr - 7);
    uint64_t const inputs[4] = {
        (*big_ptr) ^ matches,
        (*(big_ptr - 1)) ^ matches,
        (*(big_ptr - 2)) ^ matches,
        (*(big_ptr - 3)) ^ matches
    };
    uint64_t const tmps[4] = {
        (inputs[0] & mask) + mask,
        (inputs[1] & mask) + mask,
        (inputs[2] & mask) + mask,
        (inputs[3] & mask) + mask
    };
    uint64_t const result = (~(tmps[0] | inputs[0] | mask)) |
                            (~(tmps[1] | inputs[1] | mask)) |
                            (~(tmps[2] | inputs[2] | mask)) |
                            (~(tmps[3] | inputs[3] | mask)) ;
    // Any bits set means we found a match.
    if (result != 0) {
      // Search backwards until we find the match.
      #pragma GCC unroll 8
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) == byte) {
          return ptr - src;
        }
        ptr--;
      }
    }
    ptr -= 32;
  }
  // If we still haven't found anything, finish the slow way.
  #pragma GCC unroll 8
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      return ptr - src;
    }
    ptr--;
  }
  // We failed to find.
  return -1;
}

ptrdiff_t find_last_eq (uint8_t const * const src,
                        size_t const off,
                        size_t const len,
                        int const byte) {
  if (byte == 0x00) {
    return find_last_zero(src, off, len);
  }
  return find_last_eq_nonzero(src, off, len, byte);
}
