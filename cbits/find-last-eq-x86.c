#include <stddef.h>
#include <stdint.h>
#include <pmmintrin.h>
#include <immintrin.h>

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_last_eq_avx2 (uint8_t const * const src,
                                           size_t const off,
                                           size_t const len,
                                           uint8_t const byte) {
  __m256i matches = _mm256_set1_epi8(byte);
  // Our stride is 8 SIMD registers at a time.
  // That's 32 bytes times 8 = 256.
  size_t big_strides = len / 256;
  size_t small_strides = len % 256;
  uint8_t* ptr = (uint8_t*)&(src[off + len - 1]);
  // Big strides first
  for (size_t i = 0; i < big_strides; i++) {
    __m256i const * big_ptr = (__m256i*)(ptr - 15);
    __m256i inputs[8] = {
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 1), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 2), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 3), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 4), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 5), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 6), matches),
      _mm256_cmpeq_epi8(_mm256_loadu_si256(big_ptr - 7), matches)
    };
    __m256i results = 
      _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(inputs[0], 
                                                      inputs[1]),
                                      _mm256_or_si256(inputs[2],
                                                      inputs[3])),
                      _mm256_or_si256(_mm256_or_si256(inputs[4],
                                                      inputs[5]),
                                      _mm256_or_si256(inputs[6],
                                                      inputs[7])));
    // Evacuate the MSB of each lane. If any bits are set, we found a match
    // _somewhere_.
    int result = _mm256_movemask_epi8(results);
    if (result != 0) {
      // Find manually, digging backwards.
      for (size_t j = 0; j < 256; j++) {
        if ((*ptr) == byte) {
          return ptr - src;
        }
        ptr--;
      }
    }
    ptr -= 256;
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

static inline ptrdiff_t find_last_eq_sse2 (uint8_t const * const src,
                                           size_t const off,
                                           size_t const len,
                                           uint8_t const byte) {
  __m128i matches = _mm_set1_epi8(byte);
  // Our stride is 8 SIMD registers at a time.
  // That's 16 bytes times 8 = 128.
  size_t big_strides = len / 128;
  size_t small_strides = len % 128;
  uint8_t* ptr = (uint8_t*)&(src[off + len - 1]);
  // Big strides first
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i*)(ptr - 15);
    __m128i inputs[8] = {
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 1), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 2), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 3), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 4), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 5), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 6), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr - 7), matches)
    };
    __m128i results = _mm_or_si128(_mm_or_si128(_mm_or_si128(inputs[0], 
                                                             inputs[1]),
                                                _mm_or_si128(inputs[2],
                                                             inputs[3])),
                                   _mm_or_si128(_mm_or_si128(inputs[4],
                                                             inputs[5]),
                                                _mm_or_si128(inputs[6],
                                                             inputs[7])));
    // Evacuate the MSB of each lane. If any bits are set, we found a match
    // _somewhere_.
    int result = _mm_movemask_epi8(results);
    if (result != 0) {
      // Find manually, digging backwards.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) == byte) {
          return ptr - src;
        }
        ptr--;
      }
    }
    ptr -= 128;
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

ptrdiff_t find_last_eq (uint8_t const * const src,
                        size_t const off,
                        size_t const len,
                        int const byte) {
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return find_last_eq_avx2(src, off, len, byte);
  }
  return find_last_eq_sse2(src, off, len, byte);
}
