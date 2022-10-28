#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct stack {
  uint8_t *base;
  size_t offset;
  size_t size;
};

uint8_t *ptr(struct stack *s) { return s->base + s->offset; }

void push(struct stack *s, int x) {
  *(int *)ptr(s) = x;
  s->offset += sizeof(x);
}

int pop(struct stack *s) {
  s->offset -= sizeof(int);
  return *(int *)ptr(s);
}

void foo(struct stack *s) {
  int x = pop(s);
  int y = pop(s);
  int z = pop(s);

  push(s, z);
  push(s, y);
  push(s, x);
}
