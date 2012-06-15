int entry () {
  int i = 0;
  int j = 10;
  while (j>0) {
    i++;
    if (i%2 == 0) {
      j-=i;
    } else {
      j--;
    }
  }
  return j;
}
