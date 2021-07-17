#include <stddef.h>
#include <stdint.h>
#include <arm_neon.h>

size_t count_eq (uint8_t const * const src,
                 size_t const off,
                 size_t const len,
                 int const byte) {
  // Keep two counters at once.
  // This removes the need to evacuate SIMD registers during bulk work.
  uint64x2_t totals = vdupq_n_u64(0);
  uint8x16_t const matches = vdupq_n_u8(byte);
  // Our stride is 8 SIMD registers at once.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // Big strides first, using our SIMD accumulator.
  for (size_t i = 0; i < big_strides; i++) {
    // Load, then compare.
    // This fills any lane which matches our byte with 0xFF, then reinterprets
    // it as a signed byte (which makes it -1) and 0x00 otherwise.
    int8x16_t const inputs[8] = {
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 16), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 32), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 48), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 64), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 80), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 96), matches)),
      vreinterpretq_s8_u8(vceqq_u8(vld1q_u8(ptr + 112), matches))
    };
    // Since we have 0 or -1 in each lane, summing them produces 0 to -8 in each
    // lane.
    int8x16_t const summed = vaddq_s8(vaddq_s8(vaddq_s8(inputs[0],
                                                        inputs[1]),
                                               vaddq_s8(inputs[2],
                                                        inputs[3])),
                                      vaddq_s8(vaddq_s8(inputs[4],
                                                        inputs[5]),
                                               vaddq_s8(inputs[6],
                                                        inputs[7])));
    // Take absolute value, reinterpret, then stuff into our counters.
    uint8x16_t const final = vreinterpretq_u8_s8(vabsq_s8(summed));
    totals = vaddq_u64(totals, vpaddlq_u32(vpaddlq_u16(vpaddlq_u8(final))));
    ptr += 128;
  }
  // Evacuate our SIMD counters.
  size_t total = vgetq_lane_u64(totals, 0) + vgetq_lane_u64(totals, 1);
  // Finish the job one byte at a time.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) == byte) {
      total++;
    }
    ptr++;
  }
  return total;
}

