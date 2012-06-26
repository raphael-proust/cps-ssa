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

int entry (int n, int m, int x, int y){
  int res = 0;
  int b = 0;
  if (b+1) {
    b++;
    res  = foo(n);
  } else {
    res  = foo(n+n);
  }
  if (b+1) {
    res += bar(m);
    if (b) {
      res += foo(x);
    } else {
      b--;
      res += bar(y);
    }
  }
  if (b+2) {
      res += bar(y*y);
  }
  return res;
}

