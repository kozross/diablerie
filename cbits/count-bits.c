#include <stddef.h>
#include <stdint.h>

#ifdef __ARM_NEON
#include <arm_neon.h>

int count_bits_set (uint8_t* ba, int off, int len) {
  uint64x2_t totals = vdupq_n_u64(0);
  size_t big_steps = len / 64;
  size_t small_steps = len % 64;
  uint8_t* ptr = &(ba[off]);
  for (size_t i = 0; i < big_steps; i++) {
    uint8x16x4_t input = vld1q_u8_x4(ptr);
    ptr += 64;
    input.val[0] = vcntq_u8(input.val[0]);
    input.val[1] = vcntq_u8(input.val[1]);
    input.val[2] = vcntq_u8(input.val[2]);
    input.val[3] = vcntq_u8(input.val[3]);
    uint8x16_t summed = vaddq_u8(vaddq_u8(input.val[0], input.val[1]),
                                  vaddq_u8(input.val[2], input.val[3]));
    uint64x2_t final = vpaddlq_u32(vpaddlq_u16(vpaddlq_u8(summed)));
    totals = vaddq_u64(final, totals);
  }
  int total = vpaddd_u64(totals);
  for (size_t i = 0; i < small_steps; i++) {
    total += __builtin_popcount(*ptr);
    ptr++;
  }
  return total;
}
#else
int count_bits_set (uint8_t* ba, int off, int len) {
  int total = 0;
  size_t big_steps = len / 8;
  size_t small_steps = len % 8;
  uint8_t* ptr = &(ba[off]);
  for (size_t i = 0; i < big_steps; i++) {
    total += __builtin_popcountll(*(uint64_t*)ptr);
    ptr += 8;
  }
  for (size_t i = 0; i < small_steps; i++) {
    total += __builtin_popcount(*ptr);
    ptr++;
  }
  return total;
}
#endif
