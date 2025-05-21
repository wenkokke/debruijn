#include <stdint.h>

uint64_t pdep(uint64_t value, uint64_t mask) {
    uint64_t ret = 0;
    int setMaskBits = 0;
    for (int i = 0; i < 64; ++i) {
        if (mask & (1ull << i)) {
            if (value & (1ull << setMaskBits))
                ret |= 1ull << i;
            ++setMaskBits;
        }
    }
    return ret;
}
