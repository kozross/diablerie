#include <stddef.h>
#include <stdint.h>
#include <string.h>

// Based on
// https://mischasan.wordpress.com/2012/02/04/sse2-and-bndm-string-search, but
// with code you can actually read.
static inline ptrdiff_t bndm (uint8_t const * const needle,
                              size_t const needle_len,
                              uint8_t const * haystack,
                              size_t const haystack_len) {
  uint64_t compiled[256] = {};
  for (size_t i = 0; i < needle_len; i++) {
    // Set the bit corresponding to the byte at i, in LSB order.
    compiled[needle[i]] |= (1UL << (needle_len - 1UL - i));
  }
  int remaining = 0;
  for (size_t i = 0; i <= (haystack_len - needle_len); i += remaining) {
    // Get the mask corresponding to the _last_ byte.
    uint64_t mask = compiled[haystack[i + needle_len - 1]];
    remaining = needle_len;
    while (mask != 0) {
      remaining--;
      // If we go through everything with a non-zero mask, that's a match.
      if (remaining == 0) {
        return i;
      }
      // Shift along and AND with the mask for preceding character. This will
      // only be nonzero if we have a match.
      mask = (mask << 1UL) & compiled[haystack[i + remaining - 1]];
    }
  }
  // We failed to find anything.
  return -1;
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
  uint8_t const * needle_start = &(needle[needle_off]);
  uint8_t const * haystack_start = &(haystack[haystack_off]);
  // Skip ourselves forward to the first match to the beginning of our needle.
  uint8_t const * ptr = memchr(haystack_start, 
                               needle_start[0], 
                               haystack_len);
  if (ptr == NULL) {
    return -1;
  }
  if (needle_len == 1) {
    // We already found it.
    return ptr - haystack;
  }
  // Find the size of the area where our needle _might_ lie.
  size_t remaining = (haystack_start + haystack_len) - ptr;
  // If it's not too long, use BNDM.
  if (needle_len <= 64) {
    ptrdiff_t result = bndm(needle_start,
                            needle_len,
                            ptr,
                            remaining);
    if (result == -1) {
      return -1;
    }
    return (ptr - haystack) + result;
  }
  while (remaining >= needle_len) {
    // Check for a match at the start of the current area, bounce if we found.
    if (memcmp(ptr, needle_start, needle_len) == 0) {
      return ptr - haystack;
    }
    uint8_t const * next_ptr = memchr(ptr + 1, needle_start[0], remaining - 1);
    if (next_ptr == NULL) {
      // Nothing more possible to find.
      return -1;
    }
    // Note that we're skipping.
    remaining -= (next_ptr - ptr);
    ptr = next_ptr;
  }
  // We failed to find.
  return -1;
}
