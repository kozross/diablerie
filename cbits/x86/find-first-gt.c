#include <stdint.h>
#include <stddef.h>
#include <emmintrin.h>
#include <immintrin.h>

// Unsigned greater-than comparison.
// Based on https://stackoverflow.com/a/24234695
static inline __m128i _mm_cmpgt_epu8(__m128i const x, __m128i const y) {
  return _mm_cmpgt_epi8(_mm_xor_si128(x, _mm_set1_epi8(0x80)),
                        _mm_xor_si128(y, _mm_set1_epi8(0x80)));
}

__attribute__((target("avx2")))
static inline __m256i _mm256_cmpgt_epu8(__m256i const x, __m256i const y) {
  return _mm256_cmpgt_epi8(_mm256_xor_si256(x, _mm256_set1_epi8(0x80)),
                           _mm256_xor_si256(y, _mm256_set1_epi8(0x80)));
}

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_first_non_ascii_avx2 (uint8_t const * const src,
                                                   size_t const off,
                                                   size_t const len) {
  // We process 8 SIMD registers' worth of data at once.
  // That's 32 bytes times 8 = 256.
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m256i const * big_ptr = (__m256i const *)ptr;
    // We're hunting for anything with the MSB set. Given that bitwise OR
    // preserves 1-bits, we will only have an MSB set in any lane if that lane
    // in any of our inputs was at least 0x80.
    __m256i const results = 
        _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(_mm256_lddqu_si256(big_ptr),
                                                        _mm256_lddqu_si256(big_ptr + 1)),
                                        _mm256_or_si256(_mm256_lddqu_si256(big_ptr + 2),
                                                        _mm256_lddqu_si256(big_ptr + 3))),
                        _mm256_or_si256(_mm256_or_si256(_mm256_lddqu_si256(big_ptr + 4),
                                                        _mm256_lddqu_si256(big_ptr + 5)),
                                        _mm256_or_si256(_mm256_lddqu_si256(big_ptr + 6),
                                                        _mm256_lddqu_si256(big_ptr + 7))));
    // Due to the above, we can evacuate the MSBs directly. If we end up with
    // 0x0000, then we found nothing that's over 0x7F.
    if (_mm256_movemask_epi8(results) != 0) {
      // Dig manually.
      for (size_t j = 0; j < 256; j++) {
        if ((*ptr) > 0x7F) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 256;
  }
  // If we get this far and haven't found anything, finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > 0x7F) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_non_ascii_sse2 (uint8_t const * const src,
                                                   size_t const off,
                                                   size_t const len) {
  // We process 8 SIMD registers' worth of data at once.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i const *)ptr;
    // We're hunting for anything with the MSB set. Given that bitwise OR
    // preserves 1-bits, we will only have an MSB set in any lane if that lane
    // in any of our inputs was at least 0x80.
    __m128i const results = 
        _mm_or_si128(_mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr),
                                               _mm_loadu_si128(big_ptr + 1)),
                                  _mm_or_si128(_mm_loadu_si128(big_ptr + 2),
                                               _mm_loadu_si128(big_ptr + 3))),
                     _mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr + 4),
                                               _mm_loadu_si128(big_ptr + 5)),
                                  _mm_or_si128(_mm_loadu_si128(big_ptr + 6),
                                               _mm_loadu_si128(big_ptr + 7))));
    // Due to the above, we can evacuate the MSBs directly. If we end up with
    // 0x0000, then we found nothing that's over 0x7F.
    if (_mm_movemask_epi8(results) != 0) {
      // Dig manually.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) > 0x7F) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 128;
  }
  // If we get this far and haven't found anything, finish the rest slow.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > 0x7F) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_first_nonzero_avx2 (uint8_t const * const src,
                                                 size_t const off,
                                                 size_t const len) {
  // We process 8 SIMD registers' worth of data at once.
  // That's 32 bytes times 8 = 256.
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m256i const * big_ptr = (__m256i const *)ptr;
    // Since bitwise OR preserves 1-bits, the only way this could end up being
    // all-zero is if everything is a zero.
    __m256i const results = 
        _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(_mm256_lddqu_si256(big_ptr),
                                                        _mm256_lddqu_si256(big_ptr + 1)),
                                        _mm256_or_si256(_mm256_lddqu_si256(big_ptr + 2),
                                                        _mm256_lddqu_si256(big_ptr + 3))),
                        _mm256_or_si256(_mm256_or_si256(_mm256_lddqu_si256(big_ptr + 4),
                                                        _mm256_lddqu_si256(big_ptr + 5)),
                                        _mm256_or_si256(_mm256_lddqu_si256(big_ptr + 6),
                                                        _mm256_lddqu_si256(big_ptr + 7))));
    // This returns 1 if results is all-zero, and 1 otherwise. The only way
    // we're all-zeroes is if we never saw a zero.
    if (_mm256_testz_si256(results, results) == 0) {
      // Dig manually.
      for (size_t j = 0; j < 256; j++) {
        if ((*ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 256;
  }
  // If we get this far and don't find anything, finish the search slowly.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_nonzero_sse2 (uint8_t const * const src,
                                                 size_t const off,
                                                 size_t const len) {
  // We process 8 SIMD registers' worth of data at once.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i const *)ptr;
    // Since bitwise OR preserves 1-bits, the only way this could end up being
    // all-zero is if everything is a zero.
    __m128i const results = 
        _mm_or_si128(_mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr),
                                               _mm_loadu_si128(big_ptr + 1)),
                                  _mm_or_si128(_mm_loadu_si128(big_ptr + 2),
                                               _mm_loadu_si128(big_ptr + 3))),
                     _mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr + 4),
                                               _mm_loadu_si128(big_ptr + 5)),
                                  _mm_or_si128(_mm_loadu_si128(big_ptr + 6),
                                               _mm_loadu_si128(big_ptr + 7))));
    // To ensure an evacuation that's sensible, we compare to 0, which will set
    // zero lanes to 0xFF, and non-zero lanes to 0x00. If we evacuate MSBs, we
    // will get 0xFFFF if everything is zero, and something else otherwise.
    uint16_t result = _mm_movemask_epi8(_mm_cmpeq_epi8(results, 
                                                       _mm_setzero_si128()));
    if (result != 0xFFFF) {
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
  // If we get this far and don't find anything, finish the search slowly.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_first_gt_avx2 (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len,
                                            uint8_t const byte) {
  __m256i const limits = _mm256_set1_epi8(byte);
  // We process 8 SIMD registers' worth of data at once.
  // That's 32 bytes times 8 = 256.
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m256i const * big_ptr = (__m256i const *)ptr;
    __m256i const inputs[8] = {
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 1), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 2), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 3), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 4), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 5), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 6), limits),
      _mm256_cmpgt_epu8(_mm256_lddqu_si256(big_ptr + 7), limits)
    };
    // After the comparisons, we end up with 0xFF if there's a bigger byte in
    // the lane, and 0x00 otherwise. We can thus safely accumulate using bitwise
    // OR, because there's no chance of false positives.
    __m256i const results = 
      _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(inputs[0],
                                                      inputs[1]),
                                      _mm256_or_si256(inputs[2],
                                                      inputs[3])),
                      _mm256_or_si256(_mm256_or_si256(inputs[4],
                                                      inputs[5]),
                                      _mm256_or_si256(inputs[6],
                                                      inputs[7])));
    // Because we have either 0xFF or 0x00 in all lanes, the MSB alone tells us
    // if we have matches or not.
    int const result = _mm256_movemask_epi8(results);
    if (result != 0) {
      for (size_t j = 0; j < 256; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 256;
  }
  // If we get this far and don't find anything, search the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_gt_sse2 (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len,
                                            uint8_t const byte) {
  __m128i const limits = _mm_set1_epi8(byte);
  // We process 8 SIMD registers' worth of data at once.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i const *)ptr;
    __m128i const inputs[8] = {
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 1), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 2), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 3), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 4), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 5), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 6), limits),
      _mm_cmpgt_epu8(_mm_loadu_si128(big_ptr + 7), limits)
    };
    // After the comparisons, we end up with 0xFF if there's a bigger byte in
    // the lane, and 0x00 otherwise. We can thus safely accumulate using bitwise
    // OR, because there's no chance of false positives.
    __m128i const results = 
      _mm_or_si128(_mm_or_si128(_mm_or_si128(inputs[0],
                                             inputs[1]),
                                _mm_or_si128(inputs[2],
                                             inputs[3])),
                   _mm_or_si128(_mm_or_si128(inputs[4],
                                             inputs[5]),
                                _mm_or_si128(inputs[6],
                                             inputs[7])));
    // Because we have either 0xFF or 0x00 in all lanes, the MSB alone tells us
    // if we have matches or not.
    int const result = _mm_movemask_epi8(results);
    if (result != 0) {
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 128;
  }
  // If we get this far and don't find anything, search the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

ptrdiff_t find_first_gt (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  __builtin_cpu_init();
  if (byte == 0xFF) {
    return -1;
  }
  else if (byte == 0x00) {
    if (__builtin_cpu_supports("avx2")) {
      return find_first_nonzero_avx2(src, off, len);
    }
    return find_first_nonzero_sse2(src, off, len);
  }
  else if (byte == 0x7F) {
    if (__builtin_cpu_supports("avx2")) {
      return find_first_non_ascii_avx2(src, off, len);
    }
    return find_first_non_ascii_sse2(src, off, len);
  }
  else if (__builtin_cpu_supports("avx2")) {
    return find_first_gt_avx2(src, off, len, byte);
  }
  return find_first_gt_sse2(src, off, len, byte);
}
