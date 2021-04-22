#include <stddef.h>
#include <stdint.h>

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
