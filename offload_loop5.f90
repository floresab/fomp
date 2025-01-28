! ----------------------------------------------------------------------
! Abraham R. Flores
! MPI + OMP TARGET OFFLOAD LOOP
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
  USE MPI_F08
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  TYPE :: BAR
    INTEGER(spi),DIMENSION(:,:),ALLOCATABLE :: K_START
    INTEGER(spi),DIMENSION(:,:),ALLOCATABLE :: K_END
    REAL(dpf) :: FACTOR
    CONTAINS
      PROCEDURE :: INIT_BAR
      PROCEDURE :: BROADCAST
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
  SUBROUTINE  BROADCAST(SELF,IMAX,JMAX,RANK,ROOT,COMM,IERROR)
    CLASS(BAR),    INTENT(INOUT) :: SELF
    INTEGER(spi),  INTENT(IN)    :: IMAX,JMAX
    TYPE(MPI_COMM),INTENT(IN)    :: COMM
    INTEGER(spi),  INTENT(IN)    :: RANK,ROOT
    INTEGER(spi),  INTENT(INOUT) :: IERROR
    IF (RANK.NE.0) THEN
      ALLOCATE(SELF%K_START(IMAX,JMAX))
      ALLOCATE(SELF%K_END(IMAX,JMAX))
    END IF
    CALL MPI_BCAST(SELF%K_START,IMAX*JMAX,MPI_INTEGER4,ROOT,COMM,IERROR)
    CALL MPI_BCAST(SELF%K_END,IMAX*JMAX,MPI_INTEGER4,ROOT,COMM,IERROR)
    CALL MPI_BCAST(SELF%FACTOR,1,MPI_REAL8,ROOT,COMM,IERROR)
  END SUBROUTINE BROADCAST
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
PROGRAM OFFLOAD_LOOP5
! ----------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: spi=>int32, dpf=>real64
! ----------------------------------------------------------------------
  USE FOO
  USE MPI_F08
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  TYPE(MPI_COMM) :: COMM
  INTEGER(spi) :: RANK,SIZE,IERROR
  REAL(dpf) :: TOTAL_SUM,TOTAL_SUM2,LOCAL_SUM
  INTEGER(spi) :: I,J,K,NUM1,NUM2,PARTITION,EXTRA,MAX_IJ,IDX,ISTART,IEND
  TYPE(BAR) :: K_DATA
  INTEGER(spi), PARAMETER :: IMAX=100
  INTEGER(spi), PARAMETER :: JMAX=100
  INTEGER(spi), PARAMETER :: KMAX=1000
  REAL(dpf),    PARAMETER :: FAC=3.2_dpf
  REAL(dpf) :: SEED = 1723609._dpf
! ----------------------------------------------------------------------
  CALL MPI_INIT(IERROR)
  CALL MPI_COMM_RANK(COMM, RANK, IERROR)
  CALL MPI_COMM_SIZE(COMM, SIZE, IERROR)
!-----------------------------------------------------------------------
  IF (RANK.EQ.0) CALL K_DATA%INIT_BAR(SEED,IMAX,JMAX,KMAX,FAC)
  CALL K_DATA%BROADCAST(IMAX,JMAX,RANK,0,COMM,IERROR) ! how could k_data be improved for large imax,jmax?
  CALL K_DATA%MAP_TO_DEVICE() !remove this line to see how it breaks if you don't map the data
! ----------------------------------------------------------------------
  MAX_IJ=IMAX*JMAX
  EXTRA = MOD(MAX_IJ,SIZE)
  PARTITION=MAX_IJ/SIZE
  IF (EXTRA.EQ.0) THEN
    ISTART=1_spi+RANK*PARTITION
    IEND=(1_spi+RANK)*PARTITION
  ELSE
    IF (RANK.LT.EXTRA) THEN
      ISTART = 1_spi+RANK*(PARTITION+1)
      IEND = (1_spi+RANK)*(PARTITION+1)
    ELSE 
      ISTART = 1_spi+RANK*PARTITION+EXTRA
      IEND = (1_spi+RANK)*PARTITION+EXTRA
    END IF
  END IF
! ----------------------------------------------------------------------
  TOTAL_SUM=0._dpf
  !$omp target teams distribute parallel do &
  !$omp& map(tofrom:TOTAL_SUM) &
  !$omp& map(to:K_DATA) &
  !$omp& private(k) &
  !$omp& shared(IMAX,JMAX,KMAX,K_DATA) &
  !$omp& reduction(+:total_sum)
  DO IDX=ISTART,IEND
! we split up i,j across mpi ranks which is then split up on the gpu
! the highest efficeny is usually (with a big caveat) comm_size = number of devices
! each problem and each architecture will be different. You will need to be able 
! to measure the proformence of your code in a meaningful way. I use the HPCtoolkit suite.
! lastly in order to use multi gpus you will need to bind cpu cores to specific gpus. there
! should be documentation on your local supercomputer on how to do this. Nersc refers to this 
! as affinity. 
    I=1_spi+(IDX-1_spi)/JMAX
    J=MOD((IDX-1_spi),JMAX)+1_spi
    LOCAL_SUM=0._dpf
    DO K=K_DATA%K_START(I,J),K_DATA%K_END(I,J)
      LOCAL_SUM=LOCAL_SUM+GET_VALUE(K_DATA,I,J,K)
    END DO
    TOTAL_SUM=TOTAL_SUM+LOCAL_SUM
  END DO
  CALL MPI_REDUCE(TOTAL_SUM,TOTAL_SUM,1,MPI_REAL8,MPI_SUM,0,COMM,IERROR)
  IF (RANK.EQ.0) THEN
    PRINT *, "RESULT (omp): ",TOTAL_SUM
    PRINT *, "EXPECTED (100,100,1000,3.2,1723609): 101285.4599454327"
    PRINT *, "ERROR: ", ABS(TOTAL_SUM-101285.4599454327_dpf)
  END IF
! ----------------------------------------------------------------------
  CALL MPI_FINALIZE(IERROR)
! ----------------------------------------------------------------------
END PROGRAM OFFLOAD_LOOP5
