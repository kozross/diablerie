#include <stddef.h>
#include <stdint.h>

static inline uint64_t broadcast (uint8_t byte) {
  return byte * 0x0101010101010101ULL;
}

ptrdiff_t find_last_byte (uint8_t const * const src,
                          size_t off,
                          size_t len,
                          int byte) {
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
      break;
    }
    ptr--;
  }
  // We failed to find
  return -1;
}
