int baz(int a) {
  return (a+a);
}

int bar(int a) {
  int res = a;
  res += baz(a);
  return res;
}

int foo(int a) {
  int res = 0;
  res = bar(a);
  res += baz(a);
  return res;
}

int entry (int b, int x, int y){
  int res = 0;
  if (!b) {
    b++;
    res  = foo(x);
  } else {
    res  = foo(x);
  }
  if (b) {
      res += bar(y);
  }
  return res;
}

