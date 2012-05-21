int main (int argc, const char * argv[]) {
     int i, n;
     n = 100;
     int num1, num2, temp;
     num1 = 0;
     num2 = 1;
     for(i = 0; i < n; ++i){
          temp = num1+num2;
          num1 = num2;
          num2 = temp;
     }
    return temp;
}

