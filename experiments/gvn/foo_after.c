int entry (int b, int n, int m){
  int res = 0;
  if(b) {
    res -= m;
    res++;
  } else {
    res += m;
    res++;
  }
  return res;
}

