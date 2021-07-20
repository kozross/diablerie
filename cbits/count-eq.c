#include <stdint.h>
#include <stddef.h>

// Fill every 8 byte 'lane' with the same value.
static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

size_t count_eq (uint8_t const * const src,
                 size_t const off,
                 size_t const len,
                 int const byte) {
  size_t total = 0;
  size_t const big_strides = len / 64;
  size_t const small_strides = len % 64;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // We use the method described in "Bit Twiddling Hacks".
  // Source: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
  uint64_t const matches = broadcast(byte);
  uint64_t const mask = broadcast(0x7F);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const inputs[8] = {
        (*big_ptr) ^ matches,
        (*(big_ptr + 1)) ^ matches,
        (*(big_ptr + 2)) ^ matches,
        (*(big_ptr + 3)) ^ matches,
        (*(big_ptr + 4)) ^ matches,
        (*(big_ptr + 5)) ^ matches,
        (*(big_ptr + 6)) ^ matches,
        (*(big_ptr + 7)) ^ matches,
    };
    uint64_t const tmps[8] = {
        (inputs[0] & mask) + mask,
        (inputs[1] & mask) + mask,
        (inputs[2] & mask) + mask,
        (inputs[3] & mask) + mask,
        (inputs[4] & mask) + mask,
        (inputs[5] & mask) + mask,
        (inputs[6] & mask) + mask,
        (inputs[7] & mask) + mask
    };
    uint64_t const results[8] = {
      ~(tmps[0] | inputs[0] | mask),
      ~(tmps[1] | inputs[1] | mask) >> 1,
      ~(tmps[2] | inputs[2] | mask) >> 2,
      ~(tmps[3] | inputs[3] | mask) >> 3,
      ~(tmps[4] | inputs[4] | mask) >> 4,
      ~(tmps[5] | inputs[5] | mask) >> 5,
      ~(tmps[6] | inputs[6] | mask) >> 6,
      ~(tmps[7] | inputs[7] | mask) >> 7
    };
    total += __builtin_popcountll(results[0] | 
                                  results[1] | 
                                  results[2] |
                                  results[3] |
                                  results[4] |
                                  results[5] |
                                  results[6] |
                                  results[7]);
    ptr += 64;
  }
  // Finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}
