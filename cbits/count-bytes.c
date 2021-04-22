#include <stdint.h> 
#include <stddef.h> 
#ifdef __AVX2__
#include <immintrin.h>

int count_bytes_eq (uint8_t* ba, int off, int len, int w8) {
  int total = 0;
  size_t big_steps = len / 128;
  size_t small_steps = len % 128;
  uint8_t* ptr = &(ba[off]);
  __m256i mask = _mm256_set1_epi8(w8);
  for (size_t i = 0; i < big_steps; i++) {
    __m256i input = _mm256_loadu_si256((__m256i const*)ptr);
    ptr += 32;
    __m256i input2 = _mm256_loadu_si256((__m256i const*)ptr);
    ptr += 32;
     __m256i input3 = _mm256_loadu_si256((__m256i const*)ptr);
    ptr += 32;
      __m256i input4 = _mm256_loadu_si256((__m256i const*)ptr);
    ptr += 32;
    __m256i result = _mm256_cmpeq_epi8(input, mask);
    int packed = _mm256_movemask_epi8(result);
    total += __builtin_popcount(packed);
    __m256i result2 = _mm256_cmpeq_epi8(input2, mask);
    int packed2 = _mm256_movemask_epi8(result2);
    total += __builtin_popcount(packed2);
    __m256i result3 = _mm256_cmpeq_epi8(input3, mask);
    int packed3 = _mm256_movemask_epi8(result3);
    total += __builtin_popcount(packed3);
    __m256i result4 = _mm256_cmpeq_epi8(input4, mask);
    int packed4 = _mm256_movemask_epi8(result4);
    total += __builtin_popcount(packed4);
  }
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

int count_bytes_eq (uint8_t* ba, int off, int len, int w8) {
  int total = 0;
  size_t big_steps = len / 32;
  size_t small_steps = len % 32;
  uint8_t* ptr = &(ba[off]);
  uint8x16_t mask = vdupq_n_u8(w8);
  for (size_t i = 0; i < big_steps; i++) {
    uint8x16x2_t input = vld1q_u8_x2(ptr);
    ptr += 32;
    total += vaddvq_u8(vshrq_n_u8(vceqq_u8(input.val[0], mask), 7));
    total += vaddvq_u8(vshrq_n_u8(vceqq_u8(input.val[1], mask), 7));
  }
  for (size_t i = 0; i < small_steps; i++) {
    if ((*ptr) == w8) {
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

int count_bytes_eq (uint8_t* ba, int off, int len, int w8) {
  int total = 0;
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
