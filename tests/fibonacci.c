int entry (int n) {
     int i;
     int num1, num2, temp;
     num1 = 0;
     num2 = 1;
     for(i = 0; i < n; i++){
          temp = num1+num2;
          num1 = num2;
          num2 = temp;
     }
    return temp;
}

