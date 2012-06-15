void foo () {
  return;
}

int bar () {
  return 42;
}

int entry () {
  int res = 0;
  foo();
  res = bar ();
  return res;
}

