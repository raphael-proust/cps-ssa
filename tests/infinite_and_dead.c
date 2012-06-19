int entry(){
  int count = 0;

  while(1){
    count++;
  }

  /* This is dead */
  count = 0;
  const int foo = 256;
  count += foo;

  return count;

}
