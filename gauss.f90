program gauss
  !宣言部
  real,dimension(3,3) :: a = 0;
  real,dimension(3,1) :: b = 0;
  real,dimension(3,1) :: x = 0;
  real,dimension(1,3) :: tmp = 0;
  real tmpb;
  real sum;
  integer pivot;
  real coefficient;


  !実行部
  pivot = 0;
  tmpb = 0;
  sum = 0;
  coefficient = 0;
  !この辺で適当に行列をつくってる
  a(1,1) = 1; a(1,2) = 2;  a(1,3) = -2; b(1,1) = 3;
  a(2,1) = 1; a(2,2) = -1; a(2,3) = 3; b(2,1) = 4;
  a(3,1) = 2; a(3,2) = 3; a(3,3) = -5; b(3,1) = 1;

  print *, "[A]|[B]";
  do i = 1, 3
    write(*, *)  a(i, 1:3),b(i,1)
  enddo


    !i列についてくりかえす
    do i=1,2 !3行あるので3-1ループ
    !ピボットの準備
      do j=i,3
        if(maxval(a(1:3,i)) == a(j,i)) then
          pivot = j; !j行
          exit
        end if
      end do

      !行の交換
      !i行とpivot行の交換
      tmp(1,1:3)  = a(i,1:3);
      a(i,1:3) = a(pivot,1:3);
      a(pivot,1:3) = tmp(1,1:3);
      tmpb = b(i,1);
      b(i,1) = b(pivot,1);
      b(pivot,1) = tmpb;

      !消去するゾー
      do j=i+1,3
        coefficient = a(j,i)/a(i,i)
        a(j,1:3) = a(j,1:3)-coefficient*a(i,1:3)
        b(j,1) = b(j,1)-coefficient*b(i,1)

      end do

    end do

    print *,"整形結果"
    do k = 1, 3
      write(*, *)  a(k, 1:3),b(k,1)
    enddo

    do i = 3,1,-1
      if(i == 3) then
        x(i,1) = b(i,1)/a(i,i)
      else if(i<3) then
        do j=i,3
          sum = sum + a(i,j)*x(j,1)
        end do
        x(i,1) = (b(i,1) - sum)/a(i,i)
      end if
      sum = 0
    end do

    print *,"最終結果"
    write(*, *)  x(1:3,1)


end program gauss
