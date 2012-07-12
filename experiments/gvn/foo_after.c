int entry (int res, int b, int n, int m){
  if(b) {
    res -= m;
    res++;
  } else {
    res += m;
    res++;
  }
  return res;
}

