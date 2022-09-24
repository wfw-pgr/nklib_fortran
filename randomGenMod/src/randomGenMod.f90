module randomGenMod
  use mt19937i4Mod, only : grnd, sgrnd, multi_grnd, multi_sgrnd
  implicit none
contains

  ! ====================================================== !
  ! === initialize seed values of the mt19937          === !
  ! ====================================================== !
  subroutine initialize__randomSeed( seed1, seed2, nOMP, nMPI, rank )
    !$ use omp_lib
    implicit none
    integer, intent(in)     :: rank, nMPI
    integer, intent(in)     :: seed1, seed2
    integer, intent(inout)  :: nOMP
    integer                 :: i
    integer                 :: numthreads = 1
    integer, parameter      :: IntMax     = 2147483647
    integer, parameter      :: intervals  = 7
    integer, allocatable    :: seeds(:)

    ! ----------------------------------------------------------------------------------- !
    ! -- caution                                                                       -- !
    ! ----------------------------------------------------------------------------------- !
    ! --  This initialization routine is an instant manner to initialize the psued     -- !
    ! --  random generator mt19937i4. This initialization permit the overlap of the    -- !
    ! --  psued random number table. ( even same seed value is also allowed. )         -- !
    ! --    !!! Use jump routines to obtain appropriate initialized tables.            -- !
    ! ----------------------------------------------------------------------------------- !
    
    ! ------------------------------------------------------ !
    ! --- [1] auto determination of nOMP                 --- !
    ! ------------------------------------------------------ !
    !$omp parallel shared(numthreads)
    !$ numthreads = omp_get_num_threads()
    !$omp end parallel
    if ( nOMP == 0 ) then
       nOMP = numthreads
       write(6,*)
       write(6,*) "[mt19937i4Mod.f90] auto determination of nOMP == OMP_NUM_THREADS"
       write(6,*) "[mt19937i4Mod.f90]            OMP_NUM_THREADS == ", nOMP
       write(6,*)
    endif
    if ( nOMP > numthreads ) then
       write(6,*)
       write(6,*) "[mt19937i4Mod.f90] nOMP ( #. of table to be used )  > OMP_NUM_THREADS. [ERROR]"
       write(6,*) "[mt19937i4Mod.f90]            OMP_NUM_THREADS == ", numthreads
       write(6,*) "[mt19937i4Mod.f90]                       nOMP == ", nOMP
       write(6,*)
       stop "[ERROR]"
    endif
    ! ------------------------------------------------------ !
    ! --- [2] initialize PE's multi seed                 --- !
    ! ------------------------------------------------------ !
    allocate( seeds(nOMP), source=0 )
    call sgrnd( int( seed2 + intervals*rank ) )
    do i=1, nOMP
       seeds(i) = int( grnd() * IntMax )
    enddo
    call multi_sgrnd( seeds, nOMP )

    ! ------------------------------------------------------ !
    ! --- [2] initialize PE's single seed again          --- !
    ! ------------------------------------------------------ !
    call sgrnd( int( seed1 + intervals*rank ) )

    ! ------------------------------------------------------ !
    ! --- [3] initialize another routine's value         --- !
    ! ------------------------------------------------------ !
    ! flipflop          = 0
    ! devStock          = 0.d0
    ! multi_flipflop(:) = 0
    ! multi_devStock(:) = 0.d0
    
    ! ------------------------------------------------------ !
    ! --- [X] Display message                            --- !
    ! ------------------------------------------------------ !
    if ( rank.eq.0 ) then
       write(6,"(2x,a)") "[initialize__randomSeed] randomGenMod           :: ( Initialized )"
       write(6,"(2x,a)") "[initialize__randomSeed] mt19937i4Mod           :: ( Initialized )"
    endif
    
    
    return
  end subroutine initialize__randomSeed


  ! ====================================================== !
  ! === gauss__distribution ( Box-Muller Method )      === !
  ! ====================================================== !
  function gauss__distribution()
    implicit none
    double precision       :: gauss__distribution
    double precision       :: v1, v2, rsq, factor
    double precision, save :: gauss__stock
    logical         , save :: flag__stock = .false.
    !  ----------------------------------------------------  !
    !  -- written by N.K.                                --  !
    !  -- to check the original code & algorithm,        --  !
    !  -- see Numerical Recipes in F77,                  --  !
    !  --               ( sec. 7.2, p.277-280 )          --  !
    !  ----------------------------------------------------  !

    if ( .not.( flag__stock ) ) then
       ! if, stock is empty....
       do 
          v1  = 2.d0 * grnd() - 1.d0
          v2  = 2.d0 * grnd() - 1.d0
          rsq = v1**2 + v2**2
          if ( ( rsq.lt.1.d0 ).and.( rsq.ne.0.d0 ) ) exit
       enddo
       factor              = sqrt( -2.d0 * log(rsq) / rsq )
       gauss__distribution = v1 * factor
       gauss__stock        = v2 * factor
       flag__stock         = .true.       
    else
       ! if, we have stock....
       gauss__distribution = gauss__stock
       flag__stock         = .false.
    endif
    
    return
  end function gauss__distribution
  
  
  ! ====================================================== !
  ! === gauss__multi ( Box-Muller Method ) OpenMP ver. === !
  ! ====================================================== !
  function gauss__multi( myth )
    !$ use omp_lib
    implicit none
    integer         , intent(in)        :: myth
    double precision                    :: gauss__multi
    double precision                    :: v1, v2, rsq, factor
    double precision, allocatable, save :: gauss__stock(:)
    logical         , allocatable, save :: flag__stock(:)
    integer                      , save :: numthreads = 1
    !  ----------------------------------------------------  !
    !  -- written by N.K.                                --  !
    !  -- to check the original code & algorithm,        --  !
    !  -- see Numerical Recipes in F77,                  --  !
    !  --               ( sec. 7.2, p.277-280 )          --  !
    !  ----------------------------------------------------  !

    ! ------------------------------------------------------ !
    ! --- [1] prepare for the multi stock space          --- !
    ! ------------------------------------------------------ !
    !  --  [CAUTION]                                     --  !
    !  --   myth == -1   for initialize                  --  !
    !  --                                                --  !
    if ( myth == -1 ) then
       gauss__multi = -1.d0
       if ( .not.( allocated( flag__stock ) ) ) then
          !$omp parallel shared(numthreads)
          !$ numthreads = omp_get_num_threads()
          !$omp end parallel
          allocate(  flag__stock( numthreads ), source=.false. )
          allocate( gauss__stock( numthreads ), source=  0.d0  )
          gauss__multi = 0.d0
       endif
       return
    endif
    if ( .not.( allocated( flag__stock ) ) ) then
       write(6,*)
       write(6,*) "[gauss__multi @ mt19937i4Mod.f90]  flag__stock is not allocated.... [ERROR]"
       write(6,*) "[gauss__multi @ mt19937i4Mod.f90]  call gauss__multi( -1 ) at first, and "
       write(6,*) "[gauss__multi @ mt19937i4Mod.f90]  initialize flag__stock & gauss__stock."
       write(6,*)
       stop "[ERROR]"
    endif

    ! ------------------------------------------------------ !
    ! --- [2] call single gauss__distribution with myth  --- !
    ! ------------------------------------------------------ !
    if ( .not.( flag__stock(myth) ) ) then
       ! if, stock is empty....
       do 
          v1  = 2.d0 * multi_grnd( myth ) - 1.d0
          v2  = 2.d0 * multi_grnd( myth ) - 1.d0
          rsq = v1**2 + v2**2
          if ( ( rsq.lt.1.d0 ).and.( rsq.ne.0.d0 ) ) exit
       enddo
       factor              = sqrt( -2.d0 * log(rsq) / rsq )
       gauss__multi        = v1 * factor
       gauss__stock(myth)  = v2 * factor
       flag__stock(myth)   = .true.       
    else
       ! if, we have stock....
       gauss__multi        = gauss__stock(myth)
       flag__stock(myth)   = .false.
    endif
    
    return
  end function gauss__multi
  
  
  ! ====================================================== !
  ! === shuffle__Knuth_OMP                             === !
  ! ====================================================== !
  subroutine shuffle__Knuth_OMP( array, nSize, first, last )
    !$ use omp_lib
    implicit none
    integer         , intent(in)    :: nSize, first, last
    double precision, intent(inout) :: array( nSize )
    integer                         :: i, randpos, myth
    double precision                :: rand, tmp

    !$ myth = omp_get_thread_num() + 1
    do i=last, first+1, -1
       randpos = int( multi_grnd( myth )*( i-first ) ) + first
       tmp              = array( randpos )
       array( randpos ) = array(       i )
       array(       i ) = tmp
    enddo


    return
  end subroutine shuffle__Knuth_OMP

  
end module randomGenMod
