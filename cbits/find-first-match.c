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

// Fill every 8-bit 'lane' with the same value.
static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

// Based on http://0x80.pl/articles/simd-strfind.html#swar
static inline ptrdiff_t mula (uint8_t const * const needle,
                              size_t const needle_len,
                              uint8_t const * haystack,
                              size_t const haystack_len) {
  uint64_t const mask_first = broadcast(needle[0]);
  uint64_t const mask_last = broadcast(needle[needle_len - 1]);
  uint64_t const mask_low_bits = broadcast(0x7F);
  uint64_t const mask_ones = broadcast(0x01);
  uint64_t const mask_highest = broadcast(0x80);
  uint64_t const * ptr_first = (uint64_t const *)haystack;
  uint64_t const * ptr_last = (uint64_t const *)(haystack + needle_len - 1);
  for (size_t i = 0; i < haystack_len; i += 8) {
    uint64_t const matches = ((*ptr_first) ^ mask_first) | ((*ptr_last) ^ mask_last);
    uint64_t const temp = (~matches & mask_low_bits) + mask_ones;
    uint64_t const temp2 = ~matches & mask_highest;
    uint64_t zeroes = temp & temp2;
    size_t j = 0;
    while (zeroes != 0) {
      // Check if we have a high bit set in our byte, indicating a match at both
      // ends.
      if ((zeroes & 0x80) != 0) {
        // Since we already know that we match at both ends, we only need to
        // check the parts in the middle.
        uint8_t const * ptr = (uint8_t const *)(ptr_first) + j + 1;
        if (memcmp(ptr, needle + 1, needle_len - 2) == 0) {
          // Found at this position, offset by j bytes.
          return i + j;
        }
      }
      // Shift out a byte.
      zeroes >>= 8;
      // Indicate that it's the next byte we're interested in.
      j++;
    }
    ptr_first++;
    ptr_last++;
  }
  // We failed to find.
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
  ptrdiff_t result;
  // If it's not too long, use BNDM.
  if (needle_len <= 64) {
    result = bndm(needle_start, needle_len, ptr, remaining);
  }
  // Otherwise, use Mula.
  else {
    result = mula(needle_start, needle_len, ptr, remaining);
  }
  if (result == -1) {
    // We failed to find.
    return -1;
  }
  return (ptr - haystack) + result;
}
