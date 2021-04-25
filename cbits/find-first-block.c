#define _GNU_SOURCE

#include <stddef.h>
#include <stdint.h>
#include <string.h>

ptrdiff_t find_first_block (uint8_t* h, int hoff, int hlen, uint8_t* n, int noff, int nlen) {
  if (nlen == 0 || nlen > hlen) {
    return -1;
  }
  void* haystack = &(h[hoff]);
  void* needle = &(n[noff]);
  uint8_t* res = memmem(haystack, hlen, needle, nlen);
  if (res == NULL) {
    // search missed
    return -1;
  }
  return res - h;
}
