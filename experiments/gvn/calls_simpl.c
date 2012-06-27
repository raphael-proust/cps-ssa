int baz(int a) {
  return (a + a);
}

int foo(int a) {
  return 0;
}

int entry (int b, int n){
  int res = 0;
  res += foo(n);
  res += baz(n);
  return res;
}

