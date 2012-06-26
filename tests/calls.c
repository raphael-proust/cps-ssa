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
  res  = foo(n);
  res += bar(m);
  res += foo(x);
  res += bar(y);
  return res;
}

