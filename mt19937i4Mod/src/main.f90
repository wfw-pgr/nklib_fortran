program main
  use mt19937i4Mod, only : sgrnd, grnd, multi_sgrnd, multi_grnd
  use mt19937i4Mod, only : mt, multi_mt, mti, multi_mti
  implicit none
  integer, parameter    :: size = 10
  integer, parameter    :: seed = 100
  integer, parameter    :: max_nOMP = 10
  integer               :: seeds(max_nOMP)
  integer               :: i, myth, nOMP
  double precision      :: res(size), res_omp(max_nOMP,size)
  character(50)         :: c_size, fmt

  nOMP = max_nOMP

  ! ------------------------------------------------------ !
  ! --- [1] initialization                             --- !
  ! ------------------------------------------------------ !
  res(:) = 0.d0
  call sgrnd( seed )
  
  do i=1, max_nOMP
     seeds(i) = seed + (i-1)
  enddo
  call multi_sgrnd( seeds, max_nOMP )

  ! ------------------------------------------------------ !
  ! --- [2] display mti & mt ( internal num. table )   --- !
  ! ------------------------------------------------------ !
  write(6,*) mti, multi_mti
  do i=0, 20
     write(6,*) mt(i), multi_mt( i, : )
  enddo

  ! ------------------------------------------------------ !
  ! --- [3] test generation                            --- !
  ! ------------------------------------------------------ !
  do i=1, size
     res(i) = grnd()
  enddo
  do myth=1, nOMP
     do i=1, size
        res_omp(myth,i) = multi_grnd( myth )
     enddo
  enddo

  ! ------------------------------------------------------ !
  ! --- [4] write result                               --- !
  ! ------------------------------------------------------ !
  do i=1, size
     write(c_size,"(i4)") nOMP+1
     fmt = "(i4,1x," // trim(c_size)  // "(f10.5,1x))"
     write(6,trim(fmt)) i, res(i), ( res_omp(myth,i), myth=1, nOMP )
  enddo
  

end program main
