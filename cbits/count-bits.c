#include <stddef.h>
#include <stdint.h>

#ifdef __ARM_NEON
#include <arm_neon.h>

int count_bits_set (uint8_t* ba, int off, int len) {
  int total = 0;
  size_t big_steps = len / 32;
  size_t small_steps = len % 32;
  uint8_t* ptr = &(ba[off]);
  for (size_t i = 0; i < big_steps; i++) {
    uint8x16x2_t input = vld1q_u8_x2(ptr);
    ptr += 32;
    total += vaddvq_u8(vcntq_u8(input.val[0]));
    total += vaddvq_u8(vcntq_u8(input.val[1]));
  }
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
