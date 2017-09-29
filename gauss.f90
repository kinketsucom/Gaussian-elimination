program gauss
  !宣言部
  real,dimension(3,3) :: a = 0;
  real,dimension(1,3) :: b = 0;
  real,dimension(1,3) :: x = 0;
  real,dimension(1,3) :: tmp = 0;
  real pivot;

  !実行部
  pivot = 0;
  a(1,1) = 1; a(1,2) = 2;  a(1,3) = -2; b(1,1) = 3;
  a(2,1) = 1; a(2,2) = -1; a(2,3) = 3; b(1,2) = 4;
  a(3,1) = 2; a(3,2) = 3; a(3,3) = -5; b(1,3) = 1;

  print *, "[A]";
  do i = 1, 3
    write(*, *)  a(i, 1:3)
  enddo
  print *, "[B]";
    print *,b(1,1)
    print *,b(1,2)
    print *,b(1,3)


    !i列についてくりかえす
    do i=1,3
    !ピボットの準備
      do j=i,3
        if(maxval(a) == a(j,i)) then
          pivot = j !j行
          exit
        end if
      end do

    !行の交換
    !i行とpivot行の交換
    tmp(1,1:3)  = a(1,1:3)



    end do
    ! if (条件) then
    !   条件が真の場合の処理
    ! end if




end program gauss
