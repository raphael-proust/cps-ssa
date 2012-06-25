int entry (int b, int n, int m, int x, int y){
  int res = 0;
  if(b) {
    res += n;
    res -= m;
    res += x*y;
    res++;
  } else {
    res += n;
    res -= m;
    res += x/y;
    res++;
  }
  return res;
}

