#include <string.h>
#include <stdint.h>
#include <stddef.h>

ptrdiff_t find_first_byte (uint8_t* ba, size_t off, size_t len, int w8) {
  void* s = &(ba[off]);
  uint8_t* res = memchr (s, w8, len);
  if (res == NULL) {
      // search missed
      return -1;
  }
  return res - ba;
}
