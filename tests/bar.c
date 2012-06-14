int entry () {
  int i = 0;
  int foo = 0;
  int bar = 42;
  for(i=0; i<bar; i++){
    foo++;
  }
  for(i=0; i<bar; i++){
    foo += bar;
  }
  return foo;
}
