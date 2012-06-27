int entry (int b, int x, int y){
  int res = 0;
  if (!b) {
    b++;
    res  = 42;
  } else {
    res  = 43;
  }
  if (b) {
      res += b;
  }
  return res;
}

