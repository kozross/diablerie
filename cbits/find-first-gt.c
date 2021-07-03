#include <stddef.h>
#include <stdint.h>

#if (__x86_64__ || __i386__ && __SSE2__)
#include <emmintrin.h>
#include <immintrin.h>
#include <stdbool.h>

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_first_gt_avx2 (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len,
                                            uint8_t const byte) {
  __m256i const limits = _mm256_set1_epi8(byte);
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m256i results = _mm256_setzero_si256();
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      __m256i input = _mm256_lddqu_si256((__m256i*)ptr);
      ptr += 32;
      results = _mm256_or_si256(results, _mm256_cmpgt_epi8(input, limits));
    }
    int result = _mm256_movemask_epi8(results);
    if (result != 0) {
      ptr -= 256;
      for (size_t j = 0; j < 256; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
  }
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  return -1;
}

static inline ptrdiff_t find_first_gt_sse2 (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len,
                                            uint8_t const byte) {
  __m128i const limits = _mm_set1_epi8(byte);
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    __m128i results = _mm_setzero_si128();
    #pragma GCC unroll 8
    for (size_t j = 0; j < 8; j++) {
      __m128i input = _mm_loadu_si128((__m128i*)ptr);
      ptr += 16;
      results = _mm_or_si128(results, _mm_cmpgt_epi8(input, limits));
    }
    int result = _mm_movemask_epi8(results);
    if (result != 0) {
      ptr -= 128;
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
  }
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  return -1;
}

ptrdiff_t find_first_gt (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0xFF) {
    return -1;
  }
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return find_first_gt_avx2(src, off, len, byte);
  }
  return find_first_gt_sse2(src, off, len, byte);
}
#else
static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

static inline ptrdiff_t find_first_non_ascii (uint8_t const * const src,
                                              size_t const off,
                                              size_t const len) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // We process two 64-bit word at a time.
  // That's 8 bytes times 2 = 16.
  size_t big_strides = len / 16;
  size_t const small_strides = len % 16;
  uint64_t const mask = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const result = ((*big_ptr) & mask) | ((*(big_ptr + 1)) & mask);
    if (result != 0) {
      for (size_t j = 0; j < 16; j++) {
        if ((*ptr) > 0x7F) {
          return ptr - src;
        }
      }
    }
    ptr += 16;
  }
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > 0x7F) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_nonzero (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  size_t const big_strides = len / 16;
  size_t const small_strides = len % 16;
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    if (((*big_ptr) | (*(big_ptr + 1))) != 0) {
      for (size_t j = 0; j < 16; j++) {
        if (*(ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 16;
  }
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
      return ptr - src;
    }
    ptr++;
  }
  return -1;
}

static inline ptrdiff_t find_first_slow (uint8_t const * const src,
                                         size_t const off,
                                         size_t const len,
                                         uint8_t const byte) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < len; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  return -1;
}

ptrdiff_t find_first_gt (uint8_t const * const src,
                         size_t const off,
                         size_t const len,
                         int const byte) {
  if (byte == 0x00) {
    return find_first_nonzero(src, off, len);
  }
  else if (byte == 0xFF) {
    return -1;
  }
  else if (byte == 0x7F) {
    return find_first_non_ascii(src, off, len);
  }
  else {
    return find_first_slow(src, off, len, byte);
  }
}
#endif
