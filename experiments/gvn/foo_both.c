int entry (int b, int n, int m){
  int res = 0;
  if(b) {
    res += n;
    res -= m;
    res++;
  } else {
    res += n;
    res -= m;
    res++;
  }
  return res;
}
