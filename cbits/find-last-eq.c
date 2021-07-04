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
/*
#include <stddef.h>
#include <stdint.h>

#if (__x86_64__ || (__i386__ && __SSE2__))
#include <emmintrin.h>
#include <immintrin.h>

__attribute__((target("avx,avx2")))
static ptrdiff_t find_last_byte_avx2 (uint8_t const * const src,
                                      size_t const off,
                                      size_t const len,
                                      int const byte) {
  __m256i matches = _mm256_set1_epi8(byte);
  // Our stride is 8 SIMD registers at a time.
  // That's 32 bytes times 8 = 256.
  size_t big_strides = len / 256;
  size_t small_strides = len % 256;
  uint8_t* ptr = (uint8_t*)&(src[off + len - 1]);
  // Big strides first.
  for (size_t i = 0; i < big_strides; i++) {
    __m256i results = _mm256_setzero_si256();
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      // Load and compare. Given that we have 0xFF in any matching lane, by
      // taking the maximum, we ensure that we keep this information in the
      // accumulator.
      __m256i input = _mm256_loadu_si256((__m256i*)(ptr - 31));
      ptr -= 32;
      results = _mm256_max_epu8(results, _mm256_cmpeq_epi8(input, matches));
    }
    // Evacuate the MSB of each lane. If any bits are set, we found a match
    // _somewhere_.
    int result = _mm256_movemask_epi8(results);
    if (result != 0) {
      // Reset to the end of the block, then find it manually.
      ptr += 256;
      for (size_t j = 0; j < 256; j++) {
        if ((*ptr) == byte) {
          return ptr - src;
        }
        ptr--;
      }
    }
  }
  // If we still haven't found anything, finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      return ptr - src;
    }
    ptr--;
  }
  // We failed to find.
  return -1;
}

static ptrdiff_t find_last_byte_sse2 (uint8_t const * const src,
                                      size_t const off,
                                      size_t const len,
                                      int const byte) {
  __m128i matches = _mm_set1_epi8(byte);
  // Our stride is 8 SIMD registers at a time.
  // That's 16 bytes times 8 = 128.
  size_t big_strides = len / 128;
  size_t small_strides = len % 128;
  uint8_t* ptr = (uint8_t*)&(src[off + len - 1]);
  // Big strides first
  for (size_t i = 0; i < big_strides; i++) {
    __m128i results = _mm_setzero_si128();
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      // Load and compare. Given that we have 0xFF in any matching lane, by
      // taking the maximum, we ensure that we keep this information in the
      // accumulator.
      __m128i input = _mm_loadu_si128((__m128i*)(ptr - 15));
      ptr -= 16;
      results = _mm_max_epu8(results, _mm_cmpeq_epi8(input, matches));
    }
    // Evacuate the MSB of each lane. If any bits are set, we found a match
    // _somewhere_.
    int result = _mm_movemask_epi8(results);
    if (result != 0) {
      // Reset to the end of the block, then find it manually.
      ptr += 128;
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) == byte) {
          return ptr - src;
        }
        ptr--;
      }
    }
  }
  // If we still haven't found anything, finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      return ptr - src;
    }
    ptr--;
  }
  // We failed to find.
  return -1;
}

ptrdiff_t find_last_byte (uint8_t const * const src,
                          size_t const off,
                          size_t const len,
                          int const byte) {
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return find_last_byte_avx2(src, off, len, byte);
  }
  else {
    return find_last_byte_sse2(src, off, len, byte);
  }
}
#elif __ARM_NEON
#include <arm_neon.h>
#include <stdbool.h>

#if (__ARM_ARCH == 8)
static inline uint8_t horizontal_max (uint8x16_t src) {
  return vmaxvq_u8(src);
}
#else
static inline uint8_t horizontal_max (uint8x16_t src) {
  #pragma GCC unroll 4
  for (size_t i = 0; i < 4; i++) {
    src = vpmaxq_u8(src, src);
  }
  return vgetq_lane_u8(src, 0);
}
#endif

ptrdiff_t find_last_byte (uint8_t const * const src,
                          size_t const off,
                          size_t const len,
                          int const byte) {
  uint8x16_t matches = vdupq_n_u8(byte);
  // Our stride is 8 SIMD registers at a time.
  // That's 16 bytes times 8 = 128.
  size_t big_strides = len / 128;
  size_t small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off + len - 1]);
  // Big strides first.
  for (size_t i = 0; i < big_strides; i++) {
    uint8x16_t results = vdupq_n_u8(0);
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      // Load and compare.
      // Since we get 0xFF (which is 255) in a matching lane, we can accumulate
      // using maximum (as we only care if we have a match somewhere).
      results = vmaxq_u8(results,
                         vceqq_u8(matches,
                                  vld1q_u8(ptr - 15)));
      ptr -= 16;
    }
    // Horizontally max the results. This will be 0 (aka false) if we found no
    // matches, and 1 (aka true) if we found something.
    bool any_matches = horizontal_max(results);
    if (any_matches) {
      // Reset to the end of the block.
      ptr += 128;
      // Dig through the last block by hand.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) == byte) {
          return ptr - src;
        }
        ptr--;
      }
    }
  }
  // If we still haven't found anything, finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      return ptr - src;
    }
    ptr--;
  }
  // We failed to find.
  return -1;
}
#else
static inline uint64_t broadcast (uint8_t byte) {
  return byte * 0x0101010101010101ULL;
}

ptrdiff_t find_last_byte (uint8_t const * const src,
                          size_t const off,
                          size_t const len,
                          int const byte) {
  // We go a 64-bit word at a time.
  size_t big_strides = len / 8;
  size_t small_strides = len % 8;
  // Start at the end
  uint8_t* ptr = (uint8_t*)&(src[off + len - 1]);
  // We use the method described in "Bit Twiddling Hacks".
  // Source: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
  uint64_t matches = broadcast(byte);
  uint64_t mask = broadcast(0x7f);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t* big_ptr = (uint64_t*)(ptr - 7);
    uint64_t input = (*big_ptr) ^ matches;
    uint64_t tmp = (input & mask) + mask;
    uint64_t result = ~(tmp | input | mask);
    // Any bits set means we've found a match
    if (result != 0) {
      ptrdiff_t offset = __builtin_clzll(result) / 8;
      return (ptr - offset) - src;
    }
    ptr -= 8;
  }
  // If we still haven't found anything, finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      return ptr - src;
    }
    ptr--;
  }
  // We failed to find
  return -1;
}
#endif
*/
