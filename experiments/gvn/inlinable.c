int foo(int input){
  int output = input;
  output *= input;
  output++;
  return output;
}

int bar(int input){
  int output = input;
  output--;
  output *= input;
  output++;
  return output;
}

int baz(int input){
  int output = input;
  output--;
  output *= input;
  return output;
}

int toto(int input){
  int output = input;
  output *= input;
  return output;
}

int entry(int b1, int b2, int seed){
  int res=0;
  if(b1){
    res+= foo(seed);
    if(b2){
      res+= bar(seed);
      if(b1-b2){
        res+= baz(seed);
        if(b1+b2){
          res+= toto(seed);
        }
      }
    }
  }
  return res;
}
