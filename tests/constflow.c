int entry (int flag0, int flag1){

  const int n0 = 223;
  const int n1 = 2;
  const int n2 = 1;
  const int n3 = 34;

  int tmp0, tmp1, tmp2, tmp3;
  int bar0, bar1, bar2, bar3;

  if (flag1) {

    tmp0 = n0+n1+n2+n3;
    tmp1 = n1+n2+n3+n0;
    tmp2 = n2+n3+n0+n1;
    tmp3 = n3+n0+n1+n2;

    const int foo0 = tmp0-tmp1-tmp2-tmp3;
    const int foo1 = tmp1-tmp2-tmp3-tmp0;
    const int foo2 = tmp2-tmp3-tmp0-tmp1;
    const int foo3 = tmp3-tmp0-tmp1-tmp2;

    bar0 = foo0*foo1*foo2*foo3;
    bar1 = foo1*foo2*foo3*foo0;
    bar2 = foo2*foo3*foo0*foo1;
    bar3 = foo3*foo0*foo1*foo2;

  }

  if (flag1) {

    tmp0 = n0+n1+n2+n3;
    tmp1 = n1+n2+n3+n0;
    tmp2 = n2+n3+n0+n1;
    tmp3 = n3+n0+n1+n2;

    const int fu0 = tmp0-tmp1-tmp2-tmp3;
    const int fu1 = tmp1-tmp2-tmp3-tmp0;
    const int fu2 = tmp2-tmp3-tmp0-tmp1;
    const int fu3 = tmp3-tmp0-tmp1-tmp2;

    bar0 = fu0*fu1*fu2*fu3;
    bar1 = fu1*fu2*fu3*fu0;
    bar2 = fu2*fu3*fu0*fu1;
    bar3 = fu3*fu0*fu1*fu2;

  }

  const int res0 = bar0+bar1+bar2+bar3;
  const int res1 = tmp0-bar0;
  const int res = res0+res1;

  return res;
}
