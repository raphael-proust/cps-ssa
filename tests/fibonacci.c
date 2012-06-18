int entry (int n) {
  int i = 0;
  int num1 = 0;
  int num2 = 1;
  int temp;
  for(i = 0; i < n; i++){
    temp = num1+num2;
    num1 = num2;
    num2 = temp;
  }
  return temp;
}

