#include <stddef.h>
#include <stdint.h>
#include <string.h>

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
  uint8_t const * ptr = 
    memchr(haystack_start, needle_start[0], haystack_len);
  if (ptr == NULL) {
    return -1;
  }
  if (needle_len == 1) {
    return ptr - haystack;
  }
  // Find the size of the area where our needle _might_ lie.
  size_t remaining = (haystack_start + haystack_len) - ptr;
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
