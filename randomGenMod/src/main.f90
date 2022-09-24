program main
  use mt19937i4Mod, only : grnd, multi_grnd
  use randomGenMod
  implicit none
  integer, parameter :: lun     = 50
  integer, parameter :: seed1   = 20
  integer, parameter :: seed2   = 4
  integer, parameter :: nMPI    = 1
  integer, parameter :: nData   = 1000
  integer, parameter :: nShuffle= 100
  integer            :: i, ith
  integer            :: nOMP    = 4
  integer            :: rank    = 0
  character(300)     :: outFile1= "dat/grnd.dat"
  character(300)     :: outFile2= "dat/multi_grnd.dat"
  character(300)     :: outFile3= "dat/shuffle__Knuth.dat"
  character(300)     :: outFile4= "dat/gauss__dist.dat"
  character(300)     :: outFile5= "dat/gauss__multi.dat"
  character(300)     :: fmt, c_num
  double precision, allocatable :: stack(:,:), dp_shuffle(:)

  
  ! ------------------------------------------------------ !
  ! --- [1] generate random number                     --- !
  ! ------------------------------------------------------ !
  call initialize__randomSeed( seed1, seed2, nOMP, nMPI, rank )
  allocate( stack(nData,nOMP) )

  
  ! ------------------------------------------------------ !
  ! --- [2] check for grnd                             --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile1),status="replace")
  write(c_num,"(i4)") 1
  fmt   = "(i8," // trim(c_num) // "(1x,e12.5))"
  do i=1, nData
     write(lun,trim(fmt)) i, grnd()
  enddo
  close(lun)
  write(6,*) "[main.f90]  output :: ", trim(outFile1)

  
  ! ------------------------------------------------------ !
  ! --- [3] check for multi_grnd                       --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile2),status="replace")
  write(c_num,"(i4)") nOMP
  fmt   = "(i8," // trim(c_num) // "(1x,e12.5))"
  do i=1, nData
     write(lun,trim(fmt)) i, ( multi_grnd( ith), ith=1, nOMP )
  enddo
  close(lun)
  write(6,*) "[main.f90]  output :: ", trim(outFile2)

  
  ! ------------------------------------------------------ !
  ! --- [4] test of Knuth Shuffle                      --- !
  ! ------------------------------------------------------ !
  allocate( dp_shuffle( nShuffle ), source=0.d0 )

  do i=1, nShuffle
     dp_shuffle(i) = dble(i)
  enddo
  call shuffle__Knuth_OMP( dp_shuffle, nShuffle, 1, nShuffle )
  
  open(lun,file=trim(outFile3),status="replace")
  do i=1, nShuffle
     write(lun,"(2(i8,1x))") i, int( dp_shuffle(i) )
  enddo
  close(lun)
  write(6,*) "[main.f90]  output :: ", trim(outFile3)

  
  ! ------------------------------------------------------ !
  ! --- [5] check for gauss__distribution              --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile4),status="replace")
  write(c_num,"(i4)") 1
  fmt   = "(i8," // trim(c_num) // "(1x,e12.5))"
  do i=1, nData
     write(lun,trim(fmt)) i, gauss__distribution()
  enddo
  close(lun)
  write(6,*) "[main.f90]  output :: ", trim(outFile4)

  
  ! ------------------------------------------------------ !
  ! --- [6] check for multi_grnd                       --- !
  ! ------------------------------------------------------ !
  i = gauss__multi(-1)
  write(6,*) i
  open(lun,file=trim(outFile5),status="replace")
  write(c_num,"(i4)") nOMP
  fmt   = "(i8," // trim(c_num) // "(1x,e12.5))"
  do i=1, nData
     write(lun,trim(fmt)) i, ( gauss__multi( ith ), ith=1, nOMP )
  enddo
  close(lun)
  write(6,*) "[main.f90]  output :: ", trim(outFile5)
  
  ! ------------------------------------------------------ !
  ! --- [X]  End                                       --- !
  ! ------------------------------------------------------ !
  write(6,*) "end of this program."
  
end program main
