program main
  use cubSplineMod
  implicit none
  integer, parameter            :: lun  = 50
  integer, parameter            :: cLen = 300
  integer, parameter            :: LD   = 21
  integer, parameter            :: LI   = 41
  integer, parameter            :: LDx  = 41, LDy = 41
  integer, parameter            :: LIx  = 81, LIy = 81
  double precision, parameter   :: xMin_ref = -6.28d0, xMax_ref = +6.28d0
  double precision, parameter   :: xMin_itp = -6.28d0, xMax_itp = +6.28d0
  double precision, parameter   :: yMin_ref = -6.28d0, yMax_ref = +6.28d0
  double precision, parameter   :: yMin_itp = -6.28d0, yMax_itp = +6.28d0
  integer                       :: i, j
  integer                       :: iL, nLines, nCmp
  double precision, allocatable :: xD(:), yD(:), xI(:), yI(:), dydxI(:), yA(:), xI_(:)
  double precision, allocatable :: fD(:,:), fI(:,:), fA(:,:), dfdx(:,:), dfdy(:,:)
  character(cLen)               :: outFile1 = "dat/reference.dat"
  character(cLen)               :: outFile2 = "dat/interpolated.dat"
  character(cLen)               :: outFile3 = "dat/reference2D.dat"
  character(cLen)               :: outFile4 = "dat/interpolated2D.dat"

  ! ------------------------------------------------------ !
  ! --- [1] make data                                  --- !
  ! ------------------------------------------------------ !
  allocate( xD(LD), yD(LD) )
  allocate( xI(LI), yI(LI) )
  allocate( dydxI(LI), yA(LI) )
  do i=1, LD
     xD(i) = ( xMax_ref - xMin_ref ) * dble(i-1) / dble(LD-1) + xMin_ref
     yD(i) = sin( xD(i) )
  enddo
  do i=1, LI
     xI(i) = ( xMax_itp - xMin_itp ) * dble(i-1) / dble(LI-1) + xMin_itp
     yI(i) = 0.d0
     yA(i) = sin( xI(i) )
  enddo

  ! ------------------------------------------------------ !
  ! --- [2] interpolation                              --- !
  ! ------------------------------------------------------ !
  call interpolate__cubicSpline1D( xD, yD, xI, yI, dydxI )
  
  ! ------------------------------------------------------ !
  ! --- [3] save in a file                             --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile1),status="replace")
  write(lun,"(a)") "# xD_ yD_"
  write(lun,"(a,2(i10,1x))") "# ", LD, 2
  write(lun,"(a,2(i10,1x))") "# ", LD, 2
  do iL=1, LD
     write(lun,*) xD(iL), yD(iL)
  enddo
  close(lun)

  ! ------------------------------------------------------ !
  ! --- [4] save answers                               --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile2),status="replace")
  write(lun,"(a)") "# xI_ yI_ yA_"
  write(lun,"(a,2(i10,1x))") "# ", LI, 3
  write(lun,"(a,2(i10,1x))") "# ", LI, 3
  do iL=1, LI
     write(lun,*) xI(iL), yI(iL), yA(iL)
  enddo
  close(lun)


  ! ------------------------------------------------------ !
  ! --- [5]  make data for 2D                          --- !
  ! ------------------------------------------------------ !
  deallocate( xD, yD, xI, yI )
  allocate  ( xD(LDx), yD(LDy), xI(LIx), yI(LIy), xI_(LIy) )
  allocate  ( fD(LDx,LDy), fI(LIx,LIy), fA(LIx,LIy), dfdx(LIx,LIy), dfdy(LIx,LIy) )
  do i=1, LDx
     xD(i) = ( xMax_ref - xMin_ref ) * dble(i-1) / dble(LDx-1) + xMin_ref
  enddo
  do i=1, LIx
     xI(i) = ( xMax_itp - xMin_itp ) * dble(i-1) / dble(LIx-1) + xMin_itp
  enddo
  do j=1, LDy
     yD(j) = ( yMax_ref - yMin_ref ) * dble(j-1) / dble(LDy-1) + yMin_ref
  enddo
  do j=1, LIy
     yI(j) = ( yMax_itp - yMin_itp ) * dble(j-1) / dble(LIy-1) + yMin_itp
  enddo
  do j=1, LDy
     do i=1, LDx
        fD(i,j) = cos( xD(i) ) * sin( yD(j) )
     enddo
  enddo
  do j=1, LIy
     do i=1, LIx
        fA(i,j)  = cos( xI(i) ) * sin( yI(j) )
        fI (i,j) = 0.d0
     enddo
  enddo
  
  ! ------------------------------------------------------ !
  ! --- [6] interpolation                              --- !
  ! ------------------------------------------------------ !
  do i=1, LIx
     do j=1, LIy
        xI_(j) = xI(i)
     enddo
     call interpolate__cubicSpline2D( xD, yD, fD, xI_, yI, fI(i,:), dfdx(i,:), dfdy(i,:) )
  enddo
  
  ! ------------------------------------------------------ !
  ! --- [7] save reference in a file                   --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile3),status="replace")
  write(lun,"(a)") "# xD_ yD_ fD_"
  write(lun,"(a,2(i10,1x))") "# ", LDx*LDy, 3
  write(lun,"(a,3(i10,1x))") "# ", LDy,LDx, 3
  do j=1, LDy
     do i=1, LDx
        write(lun,*) xD(i), yD(j), fD(i,j)
     enddo
  enddo
  close(lun)

  ! ------------------------------------------------------ !
  ! --- [8] save interpolated in a file                --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile4),status="replace")
  write(lun,"(a)") "# xI_ yI_ fI_ fA_, dfdx_, dfdy_"
  write(lun,"(a,2(i10,1x))") "# ", LIx*LIy, 6
  write(lun,"(a,3(i10,1x))") "# ", LIy,LIx, 6
  do j=1, LIy
     do i=1, LIx
        write(lun,*) xI(i), yI(j), fI(i,j), fA(i,j), dfdx(i,j), dfdy(i,j)
     enddo
  enddo
  close(lun)

end program main
