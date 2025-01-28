! ----------------------------------------------------------------------
! Abraham R. Flores
! OMP TARGET OFFLOAD LOOP WITH FUNCTION CALL 
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
END MODULE FOO
! ----------------------------------------------------------------------
PROGRAM OFFLOAD_LOOP2
! ----------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: spi=>int32, dpf=>real64
! ----------------------------------------------------------------------
  USE FOO
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  REAL(dpf) :: TOTAL_SUM,TOTAL_SUM2
  INTEGER(spi) :: I,J,K
  INTEGER(spi), PARAMETER :: IMAX=100
  INTEGER(spi), PARAMETER :: JMAX=100
  INTEGER(spi), PARAMETER :: KMAX=1000
! ----------------------------------------------------------------------
  TOTAL_SUM=0._dpf
  !$omp target teams distribute parallel do &
  !$omp& map(tofrom:TOTAL_SUM) &
  !$omp& shared(IMAX,JMAX,KMAX) &
  !$omp& collapse(3) reduction(+:total_sum)
  DO I=1,IMAX
    DO J=1,JMAX
! you should have seen in offload1 that the k loop could be collapsed as well
      DO K=1,KMAX
        TOTAL_SUM=TOTAL_SUM+GET_VALUE(I,J,K)
      END DO
    END DO
  END DO
  PRINT *, "RESULT (omp): ",TOTAL_SUM
  PRINT *, "EXPECTED (100,100,1000): 55291341.09691928"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM-55291341.09691928_dpf)
! ----------------------------------------------------------------------
  TOTAL_SUM2=0._dpf
  DO CONCURRENT(I=1:IMAX,J=1:JMAX,K=1:KMAX) REDUCE(+: TOTAL_SUM2)
    TOTAL_SUM2=TOTAL_SUM2+GET_VALUE(I,J,K)
  END DO
! ----------------------------------------------------------------------
  PRINT *, "DO CONCURRENT SEEMS BROKEN : nvfortran 24.7-0 64-bit target on x86-64 Linux -tp haswell "
  PRINT *, "RESULT (doconcurrent): ",TOTAL_SUM2
  PRINT *, "EXPECTED (100,100,1000): 55291341.09691928"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM2-55291341.09691928_dpf)
! ----------------------------------------------------------------------
END PROGRAM OFFLOAD_LOOP2
