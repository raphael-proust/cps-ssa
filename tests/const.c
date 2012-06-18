int entry (){

  const int n0 = 223;
  const int n1 = 2;
  const int n2 = 1;
  const int n3 = 34;

  const int tmp0 = n0+n1+n2+n3;
  const int tmp1 = n1+n2+n3+n0;
  const int tmp2 = n2+n3+n0+n1;
  const int tmp3 = n3+n0+n1+n2;

  const int foo0 = tmp0-tmp1-tmp2-tmp3;
  const int foo1 = tmp1-tmp2-tmp3-tmp0;
  const int foo2 = tmp2-tmp3-tmp0-tmp1;
  const int foo3 = tmp3-tmp0-tmp1-tmp2;

  const int bar0 = foo0*foo1*foo2*foo3;
  const int bar1 = foo1*foo2*foo3*foo0;
  const int bar2 = foo2*foo3*foo0*foo1;
  const int bar3 = foo3*foo0*foo1*foo2;

  const int res0 = bar0+bar1+bar2+bar3;
  const int res1 = tmp0-foo0-bar0;

  return (res0*res1);

}
