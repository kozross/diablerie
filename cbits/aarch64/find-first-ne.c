#include <stddef.h>
#include <stdint.h>
#include <arm_neon.h>

static inline ptrdiff_t find_first_nonzero (uint8_t const * const src,
                                            size_t const off,
                                            size_t const len) {
  // We process 8 NEON registers' worth of data at a time.
  // That's 8 times 16 bytes = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  for (size_t i = 0; i < big_strides; i++) {
    // We read, and OR together, all eight blocks. If this is nonzero, there was
    // a nonzero byte somewhere.
    uint8x16_t const results = vorrq_u8(vorrq_u8(vorrq_u8(vld1q_u8(ptr),
                                                          vld1q_u8(ptr + 16)),
                                                 vorrq_u8(vld1q_u8(ptr + 32),
                                                          vld1q_u8(ptr + 48))),
                                        vorrq_u8(vorrq_u8(vld1q_u8(ptr + 64),
                                                          vld1q_u8(ptr + 80)),
                                                 vorrq_u8(vld1q_u8(ptr + 96),
                                                          vld1q_u8(ptr + 112))));
    // Taking a horizontal maximum will give 0 if we only saw 0, and something
    // else otherwise.
    uint8_t const result = vmaxvq_u8(results);
    if (result != 0) {
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

static inline ptrdiff_t find_first_mismatch (uint8_t const * const src,
                                             size_t const off,
                                             size_t const len,
                                             uint8_t const byte) {
  // We process 8 NEON registers' worth of data at a time.
  // That's 8 times 16 bytes = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  uint8x16_t const matches = vdupq_n_u8(byte);
  for (size_t i = 0; i < big_strides; i++) {
    // Read and compare with target.
    uint8x16_t const inputs[8] = {
      vceqq_u8(matches, vld1q_u8(ptr)),
      vceqq_u8(matches, vld1q_u8(ptr + 16)),
      vceqq_u8(matches, vld1q_u8(ptr + 32)),
      vceqq_u8(matches, vld1q_u8(ptr + 48)),
      vceqq_u8(matches, vld1q_u8(ptr + 64)),
      vceqq_u8(matches, vld1q_u8(ptr + 80)),
      vceqq_u8(matches, vld1q_u8(ptr + 96)),
      vceqq_u8(matches, vld1q_u8(ptr + 112))
    };
    // By ANDing together, we preserve any 0x00, which indicate a mismatch.
    uint8x16_t const result = vandq_u8(vandq_u8(vandq_u8(inputs[0],
                                                         inputs[1]),
                                                vandq_u8(inputs[2],
                                                         inputs[3])),
                                       vandq_u8(vandq_u8(inputs[4],
                                                         inputs[5]),
                                                vandq_u8(inputs[6],
                                                         inputs[7])));
    // We take a horizontal minimum. If we had 0x00 anywhere, this is what we'll
    // get, indicating a mismatch.
    if (vminvq_u8(result) == 0) {
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
    return find_first_nonzero(src, off, len);
  }
  return find_first_mismatch(src, off, len, byte);
}
