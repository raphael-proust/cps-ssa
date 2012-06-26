int baz(int a) {
  return (a+a);
}

int bar(int a) {
  int res = a;
  res += baz(a);
  res += baz(a);
  return res;
}

int foo(int a) {
  int res = 0;
  res = bar(a);
  res += bar(a);
  res += bar(a);
  res += bar(a);
  return res;
}

int entry (int b, int n, int m, int x, int y){
  int res = 0;
  if (b) {
    res  = foo(n);
  } else {
    res  = foo(n+n);
  }
  if (b+n) {
    res += bar(m);
    if (b+m) {
      res += foo(x);
    } else {
      res += bar(y);
    }
  }
  if (b+x) {
      res += bar(y*y);
  }
  return res;
}

