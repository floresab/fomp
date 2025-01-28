! ----------------------------------------------------------------------
! Abraham R. Flores
! OMP TARGET OFFLOAD LOOP WITH DATA
! ----------------------------------------------------------------------
! https://enccs.github.io/openmp-gpu/target/
! ^ above is incomplete but still good
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMP_Day1.pdf
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMPO_Day2.pdf
! ----------------------------------------------------------------------
MODULE FOO
! ----------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: spi=>int32, dpf=>real64
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  CONTAINS
! ----------------------------------------------------------------------
  PURE REAL(dpf) FUNCTION GET_VALUE(I,J,K) RESULT(VAL)
  !$omp declare target
    INTEGER(spi), INTENT(IN) :: I,J,K
    REAL(dpf) :: VAL
    VAL=(I+J+K)**2/3.2_dpf**K
  END FUNCTION GET_VALUE
! ----------------------------------------------------------------------
  SUBROUTINE RANDINT(RNG_STATE,NMAX,N)
    REAL(dpf),    INTENT(INOUT)  :: RNG_STATE
    INTEGER(spi), INTENT(IN)     :: NMAX
    INTEGER(spi), INTENT(OUT)    :: N
    REAL(dpf), PARAMETER :: M1 = 2147483647._dpf
    REAL(dpf), PARAMETER :: M = 2147483648._dpf 
    REAL(dpf), PARAMETER :: A = 48271._dpf
    REAL(dpf) :: RNG
    RNG_STATE=MOD(A*RNG_STATE,M1)
    RNG=RNG_STATE/M*NMAX
    N=INT(RNG,KIND=spi)+1 ! randint FROM 1 -> Nmax
  END SUBROUTINE RANDINT
! ----------------------------------------------------------------------
END MODULE FOO
! ----------------------------------------------------------------------
PROGRAM OFFLOAD_LOOP3
! ----------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: spi=>int32, dpf=>real64
! ----------------------------------------------------------------------
  USE FOO
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  REAL(dpf) :: TOTAL_SUM,TOTAL_SUM2,LOCAL_SUM
  INTEGER(spi) :: I,J,K,NUM1,NUM2
  INTEGER(spi), PARAMETER :: IMAX=100
  INTEGER(spi), PARAMETER :: JMAX=100
  INTEGER(spi), PARAMETER :: KMAX=1000
  INTEGER(spi),DIMENSION(:,:),ALLOCATABLE :: K_START,K_END
  REAL(dpf) :: SEED = 1723609
! ----------------------------------------------------------------------
  ALLOCATE(K_START(IMAX,JMAX))
  ALLOCATE(K_END(IMAX,JMAX))
! ----------------------------------------------------------------------
  DO I=1,IMAX
    DO J=1,JMAX
      CALL RANDINT(SEED,KMAX,NUM1)
      CALL RANDINT(SEED,KMAX,NUM2)
      K_START(I,J)=MIN(NUM1,NUM2)
      K_END(I,J)=MAX(NUM1,NUM2)
    END DO
  END DO
! ----------------------------------------------------------------------
  TOTAL_SUM=0._dpf
  !$omp target teams distribute parallel do &
  !$omp& map(tofrom:TOTAL_SUM) &
  !$omp& map(to:K_START,K_END) &
  !$omp& shared(IMAX,JMAX,KMAX,K_START,K_END) &
  !$omp& collapse(2) reduction(+:total_sum)
  DO I=1,IMAX
    DO J=1,JMAX
! now the dimension of the k loop is based on the index i,j
! this requires us to map k_start and k_end to the device
! it is also state based so there is no way to tell the computer how to do collapse it with i,j
      LOCAL_SUM=0._dpf
      DO K=K_START(I,J),K_END(I,J)
        LOCAL_SUM=LOCAL_SUM+GET_VALUE(I,J,K)
      END DO
      TOTAL_SUM=TOTAL_SUM+LOCAL_SUM
    END DO
  END DO
  PRINT *, "RESULT (omp): ",TOTAL_SUM
  PRINT *, "EXPECTED (100,100,1000,1723609): 101285.4599454327"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM-101285.4599454327_dpf)
! ----------------------------------------------------------------------
  TOTAL_SUM2=0._dpf
  DO CONCURRENT(I=1:IMAX,J=1:JMAX) REDUCE(+: TOTAL_SUM2)
    LOCAL_SUM=0._dpf
    DO K=K_START(I,J),K_END(I,J)
      LOCAL_SUM=LOCAL_SUM+GET_VALUE(I,J,K)
    END DO
    TOTAL_SUM2=TOTAL_SUM2+LOCAL_SUM
  END DO
! ----------------------------------------------------------------------
  PRINT *, "RESULT (doconcurrent): ",TOTAL_SUM2
  PRINT *, "EXPECTED (100,100,1000,1723609): 101285.4599454327"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM2-101285.4599454327_dpf)
! ----------------------------------------------------------------------
END PROGRAM OFFLOAD_LOOP3
