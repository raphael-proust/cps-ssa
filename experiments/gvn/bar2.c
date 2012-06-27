int entry (int foo, int bar) {
  int i;
  for(i=0; i<100; i++){
    foo += bar;
  }
  return foo;
}
