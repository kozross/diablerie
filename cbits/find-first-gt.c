#include <stddef.h>
#include <stdint.h>

static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

static inline ptrdiff_t find_first_large (uint8_t const * const src,
                                          size_t const off,
                                          size_t const len,
                                          uint8_t const byte) {
  // We go four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // We use the method described in "Hacker's Delight", Section 6.1.
  uint64_t const match_mask = broadcast(127 - (byte - 128));
  uint64_t const low_bit_mask = broadcast(0x7F);
  uint64_t const high_bit_mask = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const inputs[4] = {
      *big_ptr,
      *(big_ptr + 1),
      *(big_ptr + 2),
      *(big_ptr + 3)
    };
    uint64_t const high_bits[4] = {
      inputs[0] & high_bit_mask,
      inputs[1] & high_bit_mask,
      inputs[2] & high_bit_mask,
      inputs[3] & high_bit_mask
    };
    uint64_t const tmps[4] = {
      (inputs[0] & low_bit_mask) + match_mask,
      (inputs[1] & low_bit_mask) + match_mask,
      (inputs[2] & low_bit_mask) + match_mask,
      (inputs[3] & low_bit_mask) + match_mask
    };
    // We match on the range 0 .. (byte - 128). If we get no match, and the sign
    // bit is set, it means that our value is higher than byte, and we should
    // report it.
    uint64_t const result = ((tmps[0] | low_bit_mask) & high_bits[0]) |
                            ((tmps[1] | low_bit_mask) & high_bits[1]) |
                            ((tmps[2] | low_bit_mask) & high_bits[2]) |
                            ((tmps[3] | low_bit_mask) & high_bits[3]);
    if (result != 0) {
      // Search manually.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 32;
  }
  // If we got this far, finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_small (uint8_t const * const src,
                                          size_t const off,
                                          size_t const len,
                                          uint8_t const byte) {
  // We go four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // We use the method described in "Hacker's Delight", Section 6.1.
  uint64_t const match_mask = broadcast(127 - byte);
  uint64_t const high_bit_mask = broadcast(0x7F);
  uint64_t const target = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    uint64_t const inputs[4] = {
      *big_ptr,
      *(big_ptr + 1),
      *(big_ptr + 2),
      *(big_ptr + 3)
    };
    uint64_t const tmps[4] = {
      (inputs[0] & high_bit_mask) + match_mask,
      (inputs[1] & high_bit_mask) + match_mask,
      (inputs[2] & high_bit_mask) + match_mask,
      (inputs[3] & high_bit_mask) + match_mask
    };
    uint64_t const result = (~(tmps[0] | inputs[0] | high_bit_mask)) &
                            (~(tmps[1] | inputs[1] | high_bit_mask)) &
                            (~(tmps[2] | inputs[2] | high_bit_mask)) &
                            (~(tmps[3] | inputs[3] | high_bit_mask));
    // This procedure detects whether we have anything in the range 0x00 to the
    // byte argument. If _everything_ is in that range, our accumulation with
    // AND will preserve it. We then check for all-matches.
    if (result != target) {
      // Search manually.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) > byte) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 32;
  }
  // If we still haven't found anything, finish the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > byte) {
      return ptr - src;
    }
    ptr++;
  }
  // We failed to find.
  return -1;
}

static inline ptrdiff_t find_first_non_ascii (uint8_t const * const src,
                                              size_t const off,
                                              size_t const len) {
  uint8_t const * ptr = (uint8_t const *)&(src[off]);
  // We step four 64-bit blocks at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  uint64_t const mask = broadcast(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    // If a byte is larger than 0x7F, it'll have a highest-order set bit. 
    // If we accumulate with bitwise OR, we preserve any MSBs anywhere.
    uint64_t const result = *big_ptr | 
                            *(big_ptr + 1) | 
                            *(big_ptr + 2) | 
                            *(big_ptr + 3);
    // If we have any set bits, we've found something.
    if ((result & mask) != 0) {
      // Dig manually.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) > 0x7F) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 32;
  }
  // If we made it this far without finding anything, finish the search the slow
  // way.
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
  // We step four 64-bit blocks at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    // If we have any non-zero bytes in any of these loads, a bitwise OR will
    // preserve them. This means we can just OR them together, then check if we
    // still have zero - if we do, they're all zeroes and can be skipped.
    if (((*big_ptr) | 
         (*(big_ptr + 1)) | 
         (*(big_ptr + 2)) | 
         (*(big_ptr + 3))) != 0) {
      // Dig through the block manually to find the first one.
      for (size_t j = 0; j < 32; j++) {
        if ((*ptr) != 0) {
          return ptr - src;
        }
        ptr++;
      }
    }
    ptr += 32;
  }
  // If we made it this far without finding anything, finish the search the slow
  // way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) != 0) {
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
  if (byte == 0x00) {
    return find_first_nonzero(src, off, len);
  }
  else if (byte == 0xFF) {
    return -1;
  }
  else if (byte < 0x7F) {
    return find_first_small(src, off, len, byte);
  }
  else if (byte == 0x7F) {
    return find_first_non_ascii(src, off, len);
  }
  return find_first_large(src, off, len, byte);
}
