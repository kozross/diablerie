#include <stdint.h> 
#include <stddef.h> 

#if (__x86_64__ || (__i386__ && __SSE2__))
#include <emmintrin.h>
#include <immintrin.h>

__attribute__((target("avx2")))
static size_t count_bytes_eq_avx2 (uint8_t const * const src,
                                   size_t const off,
                                   size_t const len,
                                   int const byte) {
  // Keep four counters at once.
  // This reduces the need to evacuate SIMD registers during bulk work.
  __m256i totals = _mm256_setzero_si256();
  __m256i matches = _mm256_set1_epi8(byte);
  // Our stride is 8 SIMD registers at once.
  // That's 32 bytes times 8 = 256.
  size_t big_strides = len / 256;
  size_t small_strides = len % 256;
  uint8_t* ptr = (uint8_t*)&(src[off]);
  // Big strides first, using our SIMD accumulators.
  for (size_t i = 0; i < big_strides; i++) {
    __m256i acc = _mm256_setzero_si256();
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      __m256i input = _mm256_loadu_si256((__m256i*)ptr);
      ptr += 32;
      // This gives 0xFF in the lane on a match, which is -1.
      // We accumulate using _subtraction_, as x - (- 1) = x + 1.
      acc = _mm256_sub_epi8(acc, _mm256_cmpeq_epi8(matches, input));
    }
    // Stuff the accumulator into our counters.
    totals = _mm256_add_epi64(totals, 
                              _mm256_sad_epu8(acc, 
                                              _mm256_setzero_si256()));
  }
  // Evacuate our counters.
  size_t total = _mm256_extract_epi64(totals, 0) + 
                 _mm256_extract_epi64(totals, 1) +
                 _mm256_extract_epi64(totals, 2) +
                 _mm256_extract_epi64(totals, 3);
  // Finish the job the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}

static size_t count_bytes_eq_sse2 (uint8_t const * const src,
                                   size_t const off,
                                   size_t const len,
                                   int const byte) {
  // Keep two counters at once.
  // This reduces the need to evacuate SIMD registers during bulk work.
  __m128i totals = _mm_setzero_si128();
  __m128i matches = _mm_set1_epi8(byte);
  // Our stride is 8 SIMD registers at once.
  // That's 16 bytes times 8 = 128.
  size_t big_strides = len / 128;
  size_t small_strides = len % 128;
  uint8_t* ptr = (uint8_t*)&(src[off]);
  // Big strides first, using our SIMD accumulators.
  for (size_t i = 0; i < big_strides; i++) {
    __m128i acc = _mm_setzero_si128();
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      __m128i input = _mm_loadu_si128((__m128i*)ptr);
      ptr += 16;
      // This gives 0xFF in the line on a match, which is -1.
      // We accumulate using _subtraction_, as x - (- 1) = x + 1.
      acc = _mm_sub_epi8(acc, _mm_cmpeq_epi8(matches, input));
    }
    // Stuff the accumulator into our counters.
    totals = _mm_add_epi64(totals,
                           _mm_sad_epu8(acc,
                                        _mm_setzero_si128()));
  }
  // Evacuate our counters
  __m128i lows = _mm_unpacklo_epi64(totals, totals);
  __m128i highs = _mm_unpackhi_epi64(totals, totals);
  __m128i counted = _mm_add_epi64(lows, highs);
  size_t total = (size_t)_mm_movepi64_pi64(counted);
  // Finish the job the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}

size_t count_bytes_eq (uint8_t const * const src,
                       size_t const off,
                       size_t const len,
                       int const byte) {
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return count_bytes_eq_avx2(src, off, len, byte);
  }
  else {
    return count_bytes_eq_sse2(src, off, len, byte);
  }
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
    int8x16_t acc = vdupq_n_s8(0);
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      // Load, then compare.
      // This fills any lane which matches our byte with 0xFF, which is -1.
      // Thus, we can accumulate using _subtraction_.
      acc = vsubq_s8(acc, vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr), matches)));
      ptr += 16;
    }
    // Reinterpret, then stuff into our SIMD counters.
    uint8x16_t final = vreinterpretq_u8_s8(acc);
    totals = vaddq_u64(totals, vpaddlq_u32(vpaddlq_u16(vpaddlq_u8(final))));
  }
  // Evacuate our SIMD counters.
  size_t total = vgetq_lane_u64(totals, 0) + vgetq_lane_u64(totals, 1);
  // Finish the job one byte at a time.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}
#else
static inline uint64_t broadcast (uint8_t byte) {
  return byte * 0x0101010101010101ULL;
}

size_t count_bytes_eq (uint8_t* src, size_t off, size_t len, int byte) {
  size_t total = 0;
  // We go a 64-bit word at a time.
  size_t big_strides = len / 8;
  size_t small_strides = len % 8;
  uint8_t* ptr = (uint8_t*)&(src[off]);
  // We use the method described in "Bit Twiddling Hacks".
  // Source: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
  uint64_t matches = broadcast(byte);
  uint64_t mask = broadcast(0x7f);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t* big_ptr = (uint64_t*)ptr;
    uint64_t input = (*big_ptr) ^ matches;
    uint64_t tmp = (input & mask) + mask;
    uint64_t result = ~(tmp | input | mask);
    total += __builtin_popcountll(result);
    ptr += 8;
  }
  // Finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}
#endif
