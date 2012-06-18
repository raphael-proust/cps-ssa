int complicated(int input){
  int output = input;
  output *= input;

  int i;
  for(i=0; i<100; i++){
    output+=input;
  }

  return output;

}

int entry(){
  const int foo = 0;
  int blah = 0;

  if(foo){
    blah=4;
    blah=complicated(blah);
  }

  return blah;

}
