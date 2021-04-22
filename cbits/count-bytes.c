#include <stdint.h> 
#include <stddef.h> 
#ifdef __ARM_NEON
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
/*
    uint8x16_t input1 = vld1q_u8(ptr);
    ptr += 16;
    uint8x16_t compared1 = vceqq_u8(input1, mask);
    uint8x16_t shifted1 = vshrq_n_u8(compared1, 7);
    total += vaddvq_u8(shifted1);
    uint8x16_t input2 = vld1q_u8(ptr);
    ptr += 16;
    uint8x16_t compared2 = vceqq_u8(input2, mask);
    uint8x16_t shifted2 = vshrq_n_u8(compared2, 7);
    total += vaddvq_u8(shifted2);
    uint8x16_t input3 = vld1q_u8(ptr);
    ptr += 16;
    uint8x16_t compared3 = vceqq_u8(input3, mask);
    uint8x16_t shifted3 = vshrq_n_u8(compared3, 7);
    total += vaddvq_u8(shifted3);
    uint8x16_t input4 = vld1q_u8(ptr);
    ptr += 16;
    uint8x16_t compared4 = vceqq_u8(input4, mask);
    uint8x16_t shifted4 = vshrq_n_u8(compared4, 7);
    total += vaddvq_u8(shifted4);
    */
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
