int entry (int res, int b, int n, int m){
  if(b) {
    res += n;
    res -= m;
    res++;
  } else {
    res += n;
    res += m;
    res++;
  }
  return res;
}

