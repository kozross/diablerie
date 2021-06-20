#include <stdint.h> 
#include <stddef.h> 
#if __AVX2__ && !LINTERIEUR_LEGACY_SSE
#include <immintrin.h>

size_t count_bytes_eq (uint8_t* ba, size_t off, size_t len, int w8) {
  __m256i totals = _mm256_setzero_si256();
  __m256i mask = _mm256_set1_epi8(w8);
  size_t big_steps = len / 256;
  size_t small_steps = len % 256;
  uint8_t* ptr = &(ba[off]);
  for (size_t i = 0; i < big_steps; i++) {
    __m256i input[8];
    input[0] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[1] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[2] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[3] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[4] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[5] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[6] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    input[7] = 
      _mm256_cmpeq_epi8(mask, _mm256_loadu_si256((__m256i const*)ptr));
    ptr += 32;
    __m256i summed = _mm256_add_epi8(_mm256_add_epi8(_mm256_add_epi8(input[0], input[1]),
                                                     _mm256_add_epi8(input[2], input[3])),
                                     _mm256_add_epi8(_mm256_add_epi8(input[4], input[5]),
                                                     _mm256_add_epi8(input[6], input[7])));
    totals = _mm256_add_epi64(totals, _mm256_sad_epu8(_mm256_abs_epi8(summed), 
                                                      _mm256_setzero_si256()));
  }
  size_t total = _mm256_extract_epi64(totals, 0) + 
              _mm256_extract_epi64(totals, 1) + 
              _mm256_extract_epi64(totals, 2) +
              _mm256_extract_epi64(totals, 3);
  for (size_t i = 0; i < small_steps; i++) {
    if ((*ptr) == w8) {
      total++;
    }
    ptr++;
  }
  return total;
}
#elif __SSE4_1__
#include <smmintrin.h>

int count_bytes_eq (uint8_t* ba, int off, int len, int w8) {
  __m128i totals = _mm_setzero_si128();
  __m128i mask = _mm_set1_epi8(w8);
  size_t big_steps = len / 128;
  size_t small_steps = len % 128;
  uint8_t* ptr = &(ba[off]);
  for (size_t i = 0; i < big_steps; i++) {
    __m128i input[8];
    input[0] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[1] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[2] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[3] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[4] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[5] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[6] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    input[7] = _mm_cmpeq_epi8(mask, _mm_loadu_si128((__m128i const*)ptr));
    ptr += 16;
    __m128i summed = _mm_add_epi8(_mm_add_epi8(_mm_add_epi8(input[0], input[1]),
                                               _mm_add_epi8(input[2], input[3])),
                                  _mm_add_epi8(_mm_add_epi8(input[4], input[5]),
                                               _mm_add_epi8(input[6], input[7])));
    totals = _mm_add_epi64(totals, _mm_sad_epu8(_mm_abs_epi8(summed),
                                                _mm_setzero_si128()));
  }
  int total = _mm_extract_epi64(totals, 0) + 
              _mm_extract_epi64(totals, 1);
  for (size_t i = 0; i < small_steps; i++) {
    if ((*ptr) == w8) {
      total++;
    }
    ptr++;
  }
  return total;
}
#elif __ARM_NEON
#include <arm_neon.h>

size_t count_bytes_eq (uint8_t const * const src,
                       size_t const off,
                       size_t const len,
                       int const byte) {
  // Keep two counters at once.
  // This reduces the need to evacuate SIMD registers during bulk work.
  uint64x2_t totals = vdupq_n_u64(0);
  uint8x16_t matches = vdupq_n_u8(byte);
  // Our stride is 8 SIMD registers at once.
  // That's 16 bytes times 8 = 128
  size_t big_strides = len / 128;
  size_t small_strides = len % 128;
  uint8_t* ptr = (uint8_t*)&(src[off]);
  // Big strides first, using our SIMD accumulators.
  for (size_t i = 0; i < big_strides; i++) {
    uint8x16_t acc = vdupq_n_u8(0);
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      // Load, then compare.
      // This fills any lane which matches our byte with 0xFF.
      // We turn this into 0x1 with a right shift by 7 bits.
      acc = vaddq_u8(acc, vshrq_n_u8(vceqq_u8(vld1q_u8(ptr), matches), 7));
      ptr += 16;
    }
    // Collect our block accumulator and stuff it into our SIMD counters.
    totals = vaddq_u64(totals, vpaddlq_u32(vpaddlq_u16(vpaddlq_u8(acc))));
  }
  // Evacuate our SIMD counters.
  size_t total = vpaddd_u64(totals);
  // Tongue-crawl anything left.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}
#else
static uint64_t broadcast (uint8_t w8) {
  return w8 * 0x0101010101010101ULL;
}

size_t count_bytes_eq (uint8_t* ba, size_t off, size_t len, int w8) {
  size_t total = 0;
  size_t big_steps = len / 8;
  size_t small_steps = len % 8;
  uint8_t* ptr = &(ba[off]);
  uint64_t mask = broadcast(w8);
  uint64_t mask2 = 0x7f7f7f7f7f7f7f7fULL;
  for (size_t i = 0; i < big_steps; i++) {
    uint64_t* big_ptr = (uint64_t*)ptr;
    uint64_t input = (*big_ptr) ^ mask;
    uint64_t tmp = (input & mask2) + mask2;
    uint64_t result = ~(tmp | input | mask2);
    total += __builtin_popcountll(result);
    ptr += 8;
  }
  for (size_t i = 0; i < small_steps; i++) {
    if ((*ptr) == w8) {
      total++;
    }
    ptr++;
  }
  return total;
}
#endif
