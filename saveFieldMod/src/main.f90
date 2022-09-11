program main
  use saveFieldMod
  implicit none
  integer, parameter :: LI=101, LJ=101, nComponents=3
  double precision   :: axis1(LI), axis2(LJ), field(nComponents,LI,LJ)
  character(300)     :: outFile = "dat/out.dat"
  integer            :: i, j


  do i=1, LI
     axis1(i) = dble(i-1) / dble(LI-1) 
  enddo
  do j=1, LJ
     axis2(j) = dble(j-1) / dble(LJ-1)
  enddo
  do j=1, LJ
     do i=1, LI
        field(1,i,j) = axis1(i)**3
        field(2,i,j) = axis2(j)**3
        field(3,i,j) = sqrt( axis1(i)**2 + axis2(j)**2 )
     enddo
  enddo
  do j=1, LJ
     do i=1, LI
        write(6,*) axis1(i), axis2(j), field(1,i,j)
     enddo
  enddo
  call save__map2D_asPointFile( field, axis1, axis2, LI, LJ, nComponents, outFile )


  
end program main
