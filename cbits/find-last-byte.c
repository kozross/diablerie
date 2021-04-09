#define _GNU_SOURCE

#include <stdint.h>
#include <string.h>
#include <stddef.h>

int find_last_byte (uint8_t* ba, int off, int len, int w8) {
  void* s = &(ba[off]);
  uint8_t* res = memrchr (s, w8, len);
  if (res == NULL) {
    // search missed
    return -1;
  }
  ptrdiff_t diff = res - ba;
  return diff;
}
