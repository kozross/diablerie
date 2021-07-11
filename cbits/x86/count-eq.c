#include <stddef.h>
#include <stdint.h>
#include <emmintrin.h>

size_t count_eq (uint8_t const * const src,
                 size_t const off,
                 size_t const len,
                 int const byte) {
  // Keep two counters at once.
  // This reduces the need to evacuate SIMD registers during bulk work.
  __m128i totals = _mm_setzero_si128();
  __m128i const matches = _mm_set1_epi8(byte);
  // Our stride is 8 SIMD registers at once.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // Big strides first, using our SIMD accumulators.
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i const *)ptr;
    // Load and compare.
    // This sets any matching lane to 0xFF (which is -1 as an unsigned integer),
    // and all others to 0x00.
    __m128i const inputs[8] = {
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 1), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 2), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 3), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 4), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 5), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 6), matches),
      _mm_cmpeq_epi8(_mm_loadu_si128(big_ptr + 7), matches)
    };
    // Since we have either -1 or 0 in all lanes, summing them produces anything
    // from 0 to -8 in all lanes. We then subtract from 0 to make them positive,
    // since x - (- y) = x + y.
    __m128i const acc = _mm_sub_epi8(
        _mm_setzero_si128(),
        _mm_add_epi8(_mm_add_epi8(_mm_add_epi8(inputs[0],
                                               inputs[1]),
                                  _mm_add_epi8(inputs[2],
                                               inputs[3])),
                     _mm_add_epi8(_mm_add_epi8(inputs[4],
                                               inputs[5]),
                                  _mm_add_epi8(inputs[6],
                                               inputs[7]))));
    // Stuff the accumulated counts into our counters.
    totals = _mm_add_epi64(
        totals,
        _mm_sad_epu8(acc,
                     _mm_setzero_si128()));
    ptr += 128;
  }
  // Evacuate our counters
  __m128i const lows = _mm_unpacklo_epi64(totals, totals);
  __m128i const highs = _mm_unpackhi_epi64(totals, totals);
  __m128i const counted = _mm_add_epi64(lows, highs);
  size_t total = (size_t)_mm_movepi64_pi64(counted);
  // Finish the job the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}
