program main
  use newtonRapMod
  implicit none
  integer, parameter            :: lun  = 50
  integer, parameter            :: cLen = 300
  integer, parameter            :: LD   = 501
  integer, parameter            :: LDx  = 101, LDy = 101
  double precision, parameter   :: xMin = -0.0d0, xMax = +6.28d0
  double precision, parameter   :: yMin = -0.0d0, yMax = +6.28d0
  double precision, parameter   :: xc   = 1.d0  , yc   = +2.d0
  integer                       :: i, j
  integer                       :: iL, nLines, nCmp, flag
  double precision              :: rix, xk, yk, fk, radii
  double precision, allocatable :: xD(:), yD(:), fD(:,:)
  character(cLen)               :: outFile1 = "dat/function.dat"
  character(cLen)               :: outFile2 = "dat/answer.dat"
  character(cLen)               :: outFile3 = "dat/function2d.dat"
  character(cLen)               :: outFile4 = "dat/answer2d.dat"
  
  ! ------------------------------------------------------ !
  ! --- [1] make data                                  --- !
  ! ------------------------------------------------------ !
  allocate( xD(LD), yD(LD) )
  do i=1, LD
     xD(i) = ( xMax - xMin ) * dble(i-1) / dble(LD-1) + xMin
     yD(i) = ( xD(i)**2 - 2.d0 * xD(i) + 1.d0 ) + 3.d0
  enddo

  ! ------------------------------------------------------ !
  ! --- [2] solve example of 1D                        --- !
  ! ------------------------------------------------------ !
  flag = 0
  rix  = LD * 0.1
  call solve__linear_NewtonRaphson1D ( yD, rix, flag, "min", 1.d-8 )
  xk   = xD( nint( 0.6*LD ) )
  call solve__cSpline_NewtonRaphson1D( xD, yD, xk, fk, flag, "min", 1.d-8 )

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
  write(lun,"(a)") "# xa_ ya_"
  write(lun,*) xD( nint(rix) ), yD( nint(rix) )
  write(lun,*) xk, fk
  close(lun)

  ! ------------------------------------------------------ !
  ! --- [5]  make data for 2D                          --- !
  ! ------------------------------------------------------ !
  deallocate( xD, yD )
  allocate  ( xD(LDx), yD(LDy) )
  allocate  ( fD(LDx,LDy) )
  do i=1, LDx
     xD(i) = ( xMax - xMin ) * dble(i-1) / dble(LDx-1) + xMin
  enddo
  do j=1, LDy
     yD(j) = ( yMax - yMin ) * dble(j-1) / dble(LDy-1) + yMin
  enddo
  do j=1, LDy
     do i=1, LDx
        radii   = sqrt( ( xD(i)-xc )**2 + ( yD(j)-yc )**2 )
        fD(i,j) = exp( - 0.5d0 * radii**2 )
     enddo
  enddo
  
  ! ------------------------------------------------------ !
  ! --- [6] call solver 2D                             --- !
  ! ------------------------------------------------------ !
  xk = 0.6 * xc         ! choose point sufficiently close points
  yk = 0.6 * yc
  call solve__cSpline_NewtonRaphson2D( xD, yD, fD, xk, yk, fk, flag, "min", 1.d-8)
  
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
  write(lun,"(a)") "# xA yA xS yS fS"
  write(lun,*) xc, yc, xk, yk, fk
  close(lun)

end program main
