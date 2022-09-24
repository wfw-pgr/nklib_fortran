program main
  use mt19937       , only : sgrnd    , grnd
  use mt19937Mod_mod, only : sgrnd_mod, grnd_mod
  use mt19937Mod_omp, only : sgrnd_omp, grnd_omp
  implicit none
  integer, parameter :: size = 10
  integer, parameter :: seed = 20
  integer            :: i
  double precision   :: res(3,size)

  res(:,:) = 0.d0
  call sgrnd    ( seed )  !  -- change initialize routine to obtain same value.. [CAUTION] --  !
  call sgrnd_mod( seed )
  call sgrnd_omp( seed )
  do i=1, size
     res(1,i) = grnd()
     res(2,i) = grnd_mod()
     res(3,i) = grnd_omp()
  enddo
  do i=1, size
     write(6,"(i4,1x,4(f10.5,1x))") i, res(1,i), res(2,i), res(3,i), res(3,i) - res(2,i)
  enddo

end program main
