int entry (int n0, int n1, int n2, int n3){

  const int temp0 = n0+n1+n2+n3;
  const int temp1 = n1+n2+n3+n0;
  const int temp2 = n2+n3+n0+n1;
  const int temp3 = n3+n0+n1+n2;

  const int foo0 = temp0-temp1-temp2-temp3;
  const int foo1 = temp1-temp2-temp3-temp0;
  const int foo2 = temp2-temp3-temp0-temp1;
  const int foo3 = temp3-temp0-temp1-temp2;

  const int bar0 = foo0*foo1*foo2*foo3;
  const int bar1 = foo1*foo2*foo3*foo0;
  const int bar2 = foo2*foo3*foo0*foo1;
  const int bar3 = foo3*foo0*foo1*foo2;

  const int res0 = bar0+bar1+bar2+bar3;
  const int res1 = temp0-foo0-bar0;

  return (res0*res1);
}
