#include <string.h>
#include <stdint.h>
#include <stddef.h>

ptrdiff_t find_first_byte (uint8_t* ba, int off, int len, int w8) {
  void* s = &(ba[off]);
  uint8_t* res = memchr (s, w8, len);
  if (res == NULL) {
      // search missed
      return -1;
  }
  return res - ba;
}
