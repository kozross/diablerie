#include <stdint.h>
#include <stddef.h>

ptrdiff_t find_first_lt (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < len; i++) {
    if ((*ptr) < byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We missed.
  return -1;
}
