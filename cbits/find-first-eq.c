#include <string.h>
#include <stdint.h>
#include <stddef.h>

static inline ptrdiff_t find_first_eq_fallback (uint8_t const * const src,
                                                size_t const off,
                                                size_t const len,
                                                uint8_t const byte) {
  uint8_t const * res = memchr(&(src[off]), byte, len);
  if (res == NULL) {
    return -1;
  }
  return res - src;
}

#if (_POSIX_C_SOURCE >= 200809L)
ptrdiff_t find_first_eq (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0x00) {
    size_t res = strnlen((char const *)&(src[off]), len);
    if (res == len) {
      return -1;
    }
    return res;
  }
  return find_first_eq_fallback(src, off, len, byte);
}
#else
ptrdiff_t find_first_eq (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  return find_first_eq_fallback(src, off, len, byte);
}
#endif
