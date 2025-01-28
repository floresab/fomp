! ----------------------------------------------------------------------
! Abraham R. Flores
! OMP TARGET OFFLOAD LOOP WITH DERIVIED TYPES
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
  TYPE :: BAR
    INTEGER(spi),DIMENSION(:,:),ALLOCATABLE :: K_START
    INTEGER(spi),DIMENSION(:,:),ALLOCATABLE :: K_END
    REAL(dpf) :: FACTOR
    CONTAINS
      PROCEDURE :: INIT_BAR
      !PROCEDURE :: GET_VALUE
      !* if get_value is a procedure of BAR then it fails to compile for nvfortran
      PROCEDURE :: MAP_TO_DEVICE
  END TYPE BAR 
! ----------------------------------------------------------------------
  CONTAINS
! ----------------------------------------------------------------------
  PURE REAL(dpf) FUNCTION GET_VALUE(SELF,I,J,K) RESULT(VAL)
  !$omp declare target
    TYPE(BAR),   INTENT(IN) :: SELF
    INTEGER(spi), INTENT(IN) :: I,J,K
    REAL(dpf) :: VAL
    VAL=(I+J+K)**2/SELF%FACTOR**K
  END FUNCTION GET_VALUE
! ----------------------------------------------------------------------
  SUBROUTINE  INIT_BAR(SELF,SEED,IMAX,JMAX,KMAX,FAC)
    CLASS(BAR),   INTENT(INOUT) :: SELF
    REAL(dpf),    INTENT(INOUT) :: SEED
    INTEGER(spi), INTENT(IN)    :: IMAX,JMAX,KMAX
    REAL(dpf),    INTENT(IN)    :: FAC
    INTEGER(spi)  :: I,J,NUM1,NUM2
! ----------------------------------------------------------------------
    ALLOCATE(SELF%K_START(IMAX,JMAX))
    ALLOCATE(SELF%K_END(IMAX,JMAX))
! ----------------------------------------------------------------------
    DO I=1,IMAX
      DO J=1,JMAX
        CALL RANDINT(SEED,KMAX,NUM1)
        CALL RANDINT(SEED,KMAX,NUM2)
        SELF%K_START(I,J)=MIN(NUM1,NUM2)
        SELF%K_END(I,J)=MAX(NUM1,NUM2)
      END DO
    END DO
! ----------------------------------------------------------------------
    SELF%FACTOR=FAC
! ----------------------------------------------------------------------
  END SUBROUTINE INIT_BAR
! ----------------------------------------------------------------------
  SUBROUTINE  MAP_TO_DEVICE(SELF)
    CLASS(BAR),    INTENT(INOUT) :: SELF
    !$omp target enter data map(to: SELF,SELF%K_START(:,:),SELF%K_END(:,:))
  END SUBROUTINE MAP_TO_DEVICE
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
PROGRAM OFFLOAD_LOOP4
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
  REAL(dpf),    PARAMETER :: FAC=3.2_dpf
  TYPE(BAR) :: K_DATA
  REAL(dpf) :: SEED = 1723609
! ----------------------------------------------------------------------
  CALL K_DATA%INIT_BAR(SEED,IMAX,JMAX,KMAX,FAC)
  CALL K_DATA%MAP_TO_DEVICE() !remove this line to see how it breaks if you don't map the data
! ----------------------------------------------------------------------
  TOTAL_SUM=0._dpf
  !$omp target teams distribute parallel do &
  !$omp& map(tofrom:TOTAL_SUM) &
  !$omp& map(to:K_DATA) &
  !$omp& private(k) &
  !$omp& shared(IMAX,JMAX,KMAX,K_DATA) &
  !$omp& collapse(2) reduction(+:total_sum)
  DO I=1,IMAX
    DO J=1,JMAX
! now the dimension of the k loop is stored inside derived type with allocatable components
! this requires us to map the contents of the type before using it on the arrary   
! one could use -gpu=managed but global shared memory is EXTREMLY SLOW
      LOCAL_SUM=0._dpf
      DO K=K_DATA%K_START(I,J),K_DATA%K_END(I,J)
        LOCAL_SUM=LOCAL_SUM+GET_VALUE(K_DATA,I,J,K)
      END DO
      TOTAL_SUM=TOTAL_SUM+LOCAL_SUM
    END DO
  END DO
  PRINT *, "RESULT (omp): ",TOTAL_SUM
  PRINT *, "EXPECTED (100,100,1000,3.2,1723609): 101285.4599454327"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM-101285.4599454327_dpf)
! ----------------------------------------------------------------------
  TOTAL_SUM2=0._dpf
! do concurrent is removed because -stdpar=gpu enables -gpu=managed by default
!  DO CONCURRENT(I=1:IMAX,J=1:JMAX) REDUCE(+: TOTAL_SUM2)
!    LOCAL_SUM=0._dpf
!    DO K=K_DATA%K_START(I,J),K_DATA%K_END(I,J)
!      LOCAL_SUM=LOCAL_SUM+GET_VALUE(K_DATA,I,J,K)
!    END DO
!    TOTAL_SUM2=TOTAL_SUM2+LOCAL_SUM
!  END DO
! ----------------------------------------------------------------------
!  PRINT *, "RESULT (doconcurrent): ",TOTAL_SUM2
!  PRINT *, "EXPECTED (100,100,1000,3.2,1723609): 101285.4599454327"
!  PRINT *, "ERROR: ", ABS(TOTAL_SUM2-101285.4599454327_dpf)
! ----------------------------------------------------------------------
END PROGRAM OFFLOAD_LOOP4
