int baz(int a) {
  return (a + a);
}

int foo(int a) {
  int res = baz(a + a);
  return res;
}

int entry (int b, int n){
  int res = 0;
  res += foo(n);
  res += baz(n);
  return res;
}

