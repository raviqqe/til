struct foo_t {
  double x1;
  double x2;
  double x3;
};

double foo(struct foo_t xs) { return xs.x2; }
