#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <emmintrin.h>

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

// Based on http://0x80.pl/articles/simd-strfind.html#sse-avx2
static inline ptrdiff_t mula_sse2 (uint8_t const * const needle,
                                   size_t const needle_len,
                                   uint8_t const * haystack,
                                   size_t const haystack_len) {
  __m128i const mask_first = _mm_set1_epi8(needle[0]);
  __m128i const mask_last = _mm_set1_epi8(needle[needle_len - 1]);
  // Our stride is 16 bytes at a time.
  size_t const big_strides = (haystack_len - needle_len + 1) / 16;
  size_t const small_strides = (haystack_len - needle_len + 1) % 16;
  uint8_t const * ptr = (uint8_t const *)haystack;
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const input_start = 
      _mm_loadu_si128((__m128i const *)ptr);
    __m128i const input_end = 
      _mm_loadu_si128((__m128i const *)(ptr + needle_len - 1));
    uint16_t results = 
      _mm_movemask_epi8(_mm_and_si128(_mm_cmpeq_epi8(input_start, mask_first),
                                      _mm_cmpeq_epi8(input_end, mask_last)));
    while (results != 0) {
      size_t pos = __builtin_ctz(results);
      if (memcmp(ptr + pos + 1, needle + 1, needle_len - 2) == 0) {
        return (i * 16) + pos;
      }
      // Clear the bit we just found if we missed.
      results &= (results - 1);
    }
    ptr += 16;
  }
  // If we got this far, check what remains using the naive method.
  for (size_t i = 0; i < small_strides; i++) {
    if (memcmp(ptr, needle, needle_len) == 0) {
      return ptr - haystack;
    }
    ptr++;
  }
  // We missed.
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
  if (needle_len == haystack_len) {
    if (memcmp(haystack_start, needle_start, needle_len) == 0) {
      return 0;
    }
    return -1;
  }
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
    result = mula_sse2(needle_start, needle_len, ptr, remaining);
  }
  if (result == -1) {
    // We missed.
    return -1;
  }
  return (ptr - haystack) + result;
}
