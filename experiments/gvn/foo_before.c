int entry (int res, int b, int n, int m){
  if(b) {
    res += n;
    res -= m;
  } else {
    res += n;
    res += m;
  }
  return res;
}

