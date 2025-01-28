PROGRAM OFFLOAD_LOOP1
! ----------------------------------------------------------------------
! Abraham R. Flores
! SIMPLE OMP TARGET OFFLOAD LOOP EXAMPLE
! see if you can make it faster :)
! ----------------------------------------------------------------------
! https://enccs.github.io/openmp-gpu/target/
! ^ above is incomplete but still good
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMP_Day1.pdf
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMPO_Day2.pdf
! ----------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: spi=>int32, dpf=>real64
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  REAL(dpf) :: LOCAL_SUM, TOTAL_SUM,VAL
  INTEGER(spi) :: I,J,K
  INTEGER(spi), PARAMETER :: IMAX=100
  INTEGER(spi), PARAMETER :: JMAX=100
  INTEGER(spi), PARAMETER :: KMAX=1000
! ----------------------------------------------------------------------
  TOTAL_SUM=0._dpf
  !$omp target teams distribute parallel do &
  !$omp& map(tofrom:TOTAL_SUM) &
  !$omp& private(K,LOCAL_SUM,VAL) &
  !$omp& shared(IMAX,JMAX,KMAX) &
  !$omp& collapse(2) reduction(+:total_sum)
  DO I=1,IMAX
    DO J=1,JMAX
      LOCAL_SUM=0._dpf
      DO K=1,KMAX
        VAL=(I+J+K)**2/3.2_dpf**K
        LOCAL_SUM=LOCAL_SUM+VAL
      END DO
      TOTAL_SUM=TOTAL_SUM+LOCAL_SUM
    END DO
  END DO
  PRINT *, "RESULT (omp): ",TOTAL_SUM
  PRINT *, "EXPECTED (100,100,1000): 55291341.09691928"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM-55291341.09691928_dpf)
! ----------------------------------------------------------------------
  TOTAL_SUM=0._dpf
  DO CONCURRENT(I=1:IMAX,J=1:JMAX) REDUCE(+: TOTAL_SUM)
    LOCAL_SUM=0._dpf
    DO K=1,KMAX
      VAL=(I+J+K)**2/3.2_dpf**K
      LOCAL_SUM=LOCAL_SUM+VAL
    END DO
    TOTAL_SUM=TOTAL_SUM+LOCAL_SUM
  END DO
! ----------------------------------------------------------------------
  PRINT *, "RESULT (doconcurrent): ",TOTAL_SUM
  print *, "EXPECTED (100,100,1000): 55291341.09691928"
  PRINT *, "ERROR: ", ABS(TOTAL_SUM-55291341.09691928_dpf)
! ----------------------------------------------------------------------
END PROGRAM OFFLOAD_LOOP1
