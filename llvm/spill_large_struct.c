#include <stdint.h>

struct foo_t {
  uint64_t x1;
  uint64_t x2;
};

uint64_t foo(uint64_t x1, uint64_t x2, uint64_t x3, uint64_t x4, uint64_t x5,
             struct foo_t x6, uint64_t x7, uint64_t x8) {
  return x5.x2;
}
