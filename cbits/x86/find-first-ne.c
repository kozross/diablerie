#include <stddef.h>
#include <stdint.h>
#include <emmintrin.h>

static inline ptrdiff_t find_first_nonzero_sse2 (uint8_t const * const src,
                                                 size_t const off,
                                                 size_t const len) {
  // We process 8 SSE registers' worth of data at a time.
  // That's 8 times 16 bytes = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  __m128i all_ones = _mm_set1_epi8(0xFF);
  for (size_t i = 0; i < big_strides; i++) {
    // We read, and OR together, all eight reads. If this is nonzero, then there
    // was a nonzero byte somewhere.
    __m128i const * big_ptr = (__m128i const *)ptr;
    __m128i const results = 
      _mm_or_si128(_mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr),
                                             _mm_loadu_si128(big_ptr + 1)),
                                _mm_or_si128(_mm_loadu_si128(big_ptr + 2),
                                             _mm_loadu_si128(big_ptr + 3))),
                   _mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr + 4),
                                             _mm_loadu_si128(big_ptr + 5)),
                                _mm_or_si128(_mm_loadu_si128(big_ptr + 6),
                                             _mm_loadu_si128(big_ptr + 7))));
    // Compare inequal with zero will leave 0xFF in any byte that's not zero.
    // If we evacuate the MSBs, we will have all-zero if we found no mismatches,
    // and something else otherwise.
    //
    // Since SSE has no NEQ instruction, we do an equality with 0, then XOR with
    // all-1s.
    __m128i cond = _mm_xor_si128(all_ones,
        _mm_cmpeq_epi8(results, _mm_setzero_si128()));
    if (_mm_movemask_epi8(cond) != 0) {
      // Dig manually.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 128;
  }
  // If we got this far, finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
      return ptr - src;
    }
    ptr++;
  }
  // We missed.
  return -1;
}

static inline ptrdiff_t find_first_mismatch_sse2 (uint8_t const * const src,
                                                  size_t const off,
                                                  size_t const len,
                                                  uint8_t const byte) {
  // We process 8 SSE registers' worth of data at a time.
  // That's 8 times 16 bytes = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  __m128i const matches = _mm_set1_epi8(byte);
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i const *)ptr;
    // Read and compare with target.
    __m128i const inputs[8] = {
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 1)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 2)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 3)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 4)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 5)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 6)),
      _mm_cmpeq_epi8(matches, _mm_loadu_si128(big_ptr + 7))
    };
    // By ANDing together, we preserve any 0x00, which indicate a mismatch.
    __m128i const result = 
      _mm_and_si128(_mm_and_si128(_mm_and_si128(inputs[0],
                                                inputs[1]),
                                  _mm_and_si128(inputs[2],
                                                inputs[3])),
                    _mm_and_si128(_mm_and_si128(inputs[4],
                                                inputs[5]),
                                  _mm_and_si128(inputs[6],
                                                inputs[7])));
    // If we never saw a mismatch, all MSBs will be set. Thus, the evacuation
    // will tell us.
    if (_mm_movemask_epi8(result) != 0xFFFF) {
      // Dig manually.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) != byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 128;
  }
  // If we got this far, finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We missed.
  return -1;
}

ptrdiff_t find_first_ne (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0x00) {
    return find_first_nonzero_sse2(src, off, len);
  }
  return find_first_mismatch_sse2(src, off, len, byte);
}
