int entry (int n0, int n1, int n2, int n3){
  int res = 0;

  int temp0 = n0;
  int temp1 = n1;
  int temp2 = n2;
  int temp3 = n3;

  temp0 += n1;
  temp1 += n2;
  temp2 += n3;
  temp3 += n0;

  temp0 -= n2;
  temp1 -= n3;
  temp2 -= n0;
  temp3 -= n1;

  temp0 *= n3;
  temp1 *= n0;
  temp2 *= n1;
  temp3 *= n2;

  temp0 /= n0;
  temp1 /= n1;
  temp2 /= n2;
  temp3 /= n3;

  int foo0 = temp0;
  int foo1 = temp1;
  int foo2 = temp2;
  int foo3 = temp3;

  foo0 += temp1;
  foo1 += temp2;
  foo2 += temp3;
  foo3 += temp0;

  foo0 -= temp2;
  foo1 -= temp3;
  foo2 -= temp0;
  foo3 -= temp1;

  foo0 *= temp3;
  foo1 *= temp0;
  foo2 *= temp1;
  foo3 *= temp2;

  foo0 /= temp0;
  foo1 /= temp1;
  foo2 /= temp2;
  foo3 /= temp3;

  int bar0 = foo0;
  int bar1 = foo1;
  int bar2 = foo2;
  int bar3 = foo3;

  bar0 += foo1;
  bar1 += foo2;
  bar2 += foo3;
  bar3 += foo0;

  bar0 -= foo2;
  bar1 -= foo3;
  bar2 -= foo0;
  bar3 -= foo1;

  bar0 *= foo3;
  bar1 *= foo0;
  bar2 *= foo1;
  bar3 *= foo2;

  bar0 /= foo0;
  bar1 /= foo1;
  bar2 /= foo2;
  bar3 /= foo3;

  return res;
}
