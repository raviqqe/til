#include <stdio.h>

struct foo {
  int x;
  double y;
};

int foo(struct foo x) {
  printf("%f\n", x.y);

  return 42;
}
