int entry (int flag0, int flag1){

  const int n0 = 223;
  const int n1 = 2;

  int tmp0, tmp1;
  int bar0, bar1;

  if (flag1) {

    tmp0 = n0+n1;
    tmp1 = n0-n1;

    const int foo0 = tmp0-tmp1;
    const int foo1 = tmp0+tmp1;

    bar0 = foo0*foo1;
    bar1 = foo0*foo1;

  }

  if (flag1) {

    tmp0 = n0+n1;
    tmp1 = n0+n1;

    const int fu0 = tmp0-tmp1;
    const int fu1 = tmp0*tmp1;

    bar0 = fu0*fu1;
    bar1 = fu0+fu1;

  }

  const int res0 = bar0+bar1;
  const int res1 = tmp0-bar0;
  const int res = res0+res1;

  return res;
}
