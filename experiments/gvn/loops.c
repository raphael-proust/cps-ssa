int ffor() {
  int i;
  int t = 2;
  for(i = 0; i<5; i++){
    t*=t;
  }
  return t;
}
int fiffor(int f){
  int i;
  int t = 2;
  if(f){
    for(i = 0; i<5; i++){
        t*=t;
      }
  } else {
    for(i = 5; i>0; i--){
        t/=t;
      }
  }
  return t;
}
int fforfor() {
  int i, j;
  int t = 2;
  for(i = 0; i<5; i++){
    for(j = 0; j<5; j++){
      t*=t;
    }
  }
  return t;
}
int fforif() {
  int i;
  int t = 2;
  for(i = 0; i<5; i++){
    if(i%2){
      t*=t;
    } else {
      t/=t;
    }
  }
  return t;
}
int fforifcontinue() {
  int i;
  int t = 2;
  for(i = 0; i<5; i++){
    if(i%2){
      t*=t;
    } else {
      continue;
    }
    t++;
  }
  return t;
}
int fforifbreak() {
  int i;
  int t = 2;
  for(i = 0; i<5; i++){
    if(i%2){
      t*=t;
    } else {
      break;
    }
    t++;
  }
  return t;
}
int fwhile() {
  int i = 10;
  int t = 2;
  while(t<i){
    i++;
    t*=t;
  }
  return t;
}
int fwhileif() {
  int i = 10;
  int t = 2;
  while(t<i){
    i++;
    if(i%2){
      t*=t;
    } else {
      t/=t;
    }
  }
  return t;
}
int fwhileifbreak() {
  int i = 10;
  int t = 2;
  while(1){
    i++;
    if(i%2){
      break;
    } else {
      t/=t;
    }
  }
  return t;
}
int fwhileifcontinue() {
  int i = 10;
  int t = 2;
  while(t<i){
    i++;
    if(i%2){
      continue;
    } else {
      t/=t;
    }
    t++;
  }
  return t;
}
