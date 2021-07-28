#include <stddef.h>
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

static inline ptrdiff_t find_first_nonzero_sse2 (uint8_t const * const src,
                                                 size_t const off,
                                                 size_t const len) {
  // We process 8 SSE registers' worth of data at a time.
  // That's 8 times 16 bytes = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
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
    // Compare with zero will put 0xFF into matching bytes. Thus, if we evacuate
    // the MSBs, any unset ones means we found something else.
    __m128i cond = _mm_cmpeq_epi8(results, _mm_setzero_si128());
    if (_mm_movemask_epi8(cond) != 0xFFFF) {
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

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_first_nonzero_avx2 (uint8_t const * const src,
                                                 size_t const off,
                                                 size_t const len) {
  // We process 8 AVX registers' worth of data at a time.
  // That's 8 times 32 bytes = 256.
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    // We read, and OR together, all eight reads. If this is nonzero, then there
    // was a nonzero byte somewhere.
    __m256i const * big_ptr = (__m256i const *)ptr;
    __m256i const results = 
      _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(_mm256_loadu_si256(big_ptr),
                                                      _mm256_loadu_si256(big_ptr + 1)),
                                      _mm256_or_si256(_mm256_loadu_si256(big_ptr + 2),
                                                      _mm256_loadu_si256(big_ptr + 3))),
                      _mm256_or_si256(_mm256_or_si256(_mm256_loadu_si256(big_ptr + 4),
                                                      _mm256_loadu_si256(big_ptr + 5)),
                                      _mm256_or_si256(_mm256_loadu_si256(big_ptr + 6),
                                                      _mm256_loadu_si256(big_ptr + 7))));
    // Compare with zero will put 0xFF into matching bytes. Thus, if we evacuate
    // the MSBs, any unset ones means we found something else.
    __m256i cond = _mm256_cmpeq_epi8(results, _mm256_setzero_si256());
    if (((uint32_t)_mm256_movemask_epi8(cond)) != 0xFFFFFFFF) {
      // Dig manually.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 256;
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

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_first_mismatch_avx2 (uint8_t const * const src,
                                                  size_t const off,
                                                  size_t const len,
                                                  uint8_t const byte) {
  // We process 8 AVX registers' worth of data at a time.
  // That's 8 times 32 bytes = 256.
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  __m256i const matches = _mm256_set1_epi8(byte);
  for (size_t i = 0; i < big_strides; i++) {
    __m256i const * big_ptr = (__m256i const *)ptr;
    // Read and compare with target.
    __m256i const inputs[8] = {
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 1)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 2)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 3)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 4)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 5)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 6)),
      _mm256_cmpeq_epi8(matches, _mm256_loadu_si256(big_ptr + 7))
    };
    // By ANDing together, we preserve any 0x00, which indicate a mismatch.
    __m256i const result = 
      _mm256_and_si256(_mm256_and_si256(_mm256_and_si256(inputs[0],
                                                         inputs[1]),
                                        _mm256_and_si256(inputs[2],
                                                         inputs[3])),
                       _mm256_and_si256(_mm256_and_si256(inputs[4],
                                                         inputs[5]),
                                        _mm256_and_si256(inputs[6],
                                                         inputs[7])));
    // If we never saw a mismatch, all MSBs will be set. Thus, the evacuation
    // will tell us.
    if (((uint32_t)(_mm256_movemask_epi8(result))) != 0xFFFFFFFF) {
      // Dig manually.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) != byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 256;
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
  __builtin_cpu_init();
  if (byte == 0x00) {
    if (__builtin_cpu_supports("avx2")) {
      return find_first_nonzero_avx2(src, off, len);
    }
    return find_first_nonzero_sse2(src, off, len);
  }
  if (__builtin_cpu_supports("avx2")) {
    return find_first_mismatch_avx2(src, off, len, byte);
  }
  return find_first_mismatch_sse2(src, off, len, byte);
}
