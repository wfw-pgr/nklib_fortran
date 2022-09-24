program main
  use mt19937i4Mod, only : sgrnd, grnd, multi_sgrnd, multi_grnd
  use mt19937i4Mod, only : mt, multi_mt, mti, multi_mti
  implicit none
  integer, parameter :: size = 10
  integer, parameter :: seed = 20
  integer, parameter :: OMPNumThreads = 4
  integer            :: seeds(OMPNumThreads)
  integer            :: i, myth
  double precision   :: res(size), res_omp(OMPNumThreads,size)

  ! ------------------------------------------------------ !
  ! --- [1] initialization                             --- !
  ! ------------------------------------------------------ !
  res(:) = 0.d0
  call sgrnd( seed )
  
  do i=1, OMPNumThreads
     seeds(i) = seed + (i-1) * 1
  enddo
  call multi_sgrnd( seeds )

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
  do myth=1, OMPNumThreads
     do i=1, size
        res_omp(myth,i) = multi_grnd( myth )
     enddo
  enddo

  ! ------------------------------------------------------ !
  ! --- [4] write result                               --- !
  ! ------------------------------------------------------ !
  do i=1, size
     write(6,"(i4,1x,5(f10.5,1x))") i, res(i), ( res_omp(myth,i), myth=1, OMPNumThreads )
  enddo
  

end program main
