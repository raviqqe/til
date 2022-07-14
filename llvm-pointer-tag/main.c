#include <stdint.h>
#include <stdio.h>

double x = 42.0;

int main() {
  printf("%f\n", *(double *)((uint64_t)(&x) | 1 & -2));

  return 0;
}
