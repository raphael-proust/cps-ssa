int baz(int a) {
  return (a + a);
}

int bar(int a) {
  return (a * a);
}

int foo(int a) {
  return 0;
}

int entry (int b, int n, int m, int x, int y){
  int res = 0;
  if(b) {
    res  = foo(n);
    res += bar(m);
    res += baz(b);
  } else {
    res  = foo(x);
    res += bar(y);
    res += baz(b);
  }
  return res;
}

