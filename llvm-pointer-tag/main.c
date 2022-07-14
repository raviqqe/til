#include <stdint.h>
#include <stdio.h>

extern double x;

void foo() { printf("%f\n", *(double *)((int64_t)(&x) & 1 | -2)); }
