#include <stddef.h>
#include <stdint.h>
#include <arm_neon.h>

static inline ptrdiff_t find_first_gt_neon (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len,
                                            uint8_t const byte) {
  uint8x16_t const limits = vdupq_n_u8(byte);
  // Our stride is 8 SIMD registers at a time.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // Big strides first.
  for (size_t i = 0; i < big_strides; i++) {
    // Load and compare.
    // This puts 0xFF into a lane if we have a greater byte, and 0x00 otherwise.
    uint8x16_t const inputs[8] = {
      vcgtq_u8(vld1q_u8(ptr), limits),
      vcgtq_u8(vld1q_u8(ptr + 16), limits),
      vcgtq_u8(vld1q_u8(ptr + 32), limits),
      vcgtq_u8(vld1q_u8(ptr + 48), limits),
      vcgtq_u8(vld1q_u8(ptr + 64), limits),
      vcgtq_u8(vld1q_u8(ptr + 80), limits),
      vcgtq_u8(vld1q_u8(ptr + 96), limits),
      vcgtq_u8(vld1q_u8(ptr + 112), limits)
    };
    // As each lane has either 0xFF or 0x)), but nothing else, we can use a
    // lane-wise bitwise OR as an accumulator. If we end up with 0xFF in any
    // lane, we know we found a match.
    uint8x16_t const results = vorrq_u8(vorrq_u8(vorrq_u8(inputs[0],
                                                          inputs[1]),
                                                 vorrq_u8(inputs[2],
                                                          inputs[3])),
                                        vorrq_u8(vorrq_u8(inputs[4],
                                                          inputs[5]),
                                                 vorrq_u8(inputs[6],
                                                          inputs[7])));
    // Take a horizontal maximum. If this comes out to 0, it means we found
    // nothing; otherwise, there's a match somewhere.
    if (vmaxvq_u8(results)) {
      // Dig through the block by hand.
      for (size_t j = 0; j < 128; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 128;
  }
  // If we got this far, do the rest slow.
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
  if (byte == 0xFF) {
    return -1;
  }
  return find_first_gt_neon(src, off, len, byte);
}
