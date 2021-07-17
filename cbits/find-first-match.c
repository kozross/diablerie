#include <stddef.h>
#include <stdint.h>
#include <string.h>

// Based on https://mischasan.wordpress.com/2012/02/04/sse2-and-bndm-string-search/
static inline uint8_t* bndm (uint8_t const * const needle,
                             size_t const needle_off,
                             size_t const needle_len,
                             uint8_t const * ptr,
                             size_t const block_len) {
}

static inline uint8_t const * memchrcmp (uint8_t const * const needle,
                                  size_t const needle_off,
                                  size_t const needle_len,
                                  uint8_t const * ptr,
                                  size_t remaining) {
  while (remaining >= needle_len) {
    if (memcmp(ptr, &(needle[needle_off]), needle_len) == 0) {
      return ptr;
    }
    uint8_t* next_ptr = memchr(ptr + 1, needle[needle_off], remaining - 1);
    if (next_ptr == NULL) {
      // Nothing more possible to find.
      return NULL;
    }
    remaining -= (next_ptr - ptr);
    ptr = next_ptr;
  }
  // We failed to find.
  return NULL;
}

ptrdiff_t find_first_match (uint8_t const * const needle,
                            size_t const needle_off,
                            size_t const needle_len,
                            uint8_t const * const haystack,
                            size_t const haystack_off,
                            size_t const haystack_len) {
  if (haystack_len == 0 || needle_len > haystack_len) {
    return -1;
  }
  if (needle_len == 0) {
    return 0;
  }
  // Skip ourselves forward to the first match to the beginning of our needle.
  uint8_t const * ptr = 
    memchr(&(haystack[haystack_off]), needle[needle_off], haystack_len);
  if (ptr == NULL) {
    return -1;
  }
  if (needle_len == 1) {
    return ptr - haystack;
  }
  uint8_t const * result;
  // Find the size of the area where our needle _might_ lie.
  size_t block_len = (haystack + haystack_off + haystack_len) - ptr;
  if (needle_len <= 64) {
    result = bndm(needle, needle_off, needle_len, ptr, block_len);
  }
  else {
    result = memchrcmp(needle, needle_off, needle_len, ptr, block_len);
  }
  if (result == NULL) {
    return -1;
  }
  return result - haystack;
}
