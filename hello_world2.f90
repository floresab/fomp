PROGRAM HELLO_MPI_GPU_WORLD
! Abraham R. Flores
! https://enccs.github.io/openmp-gpu/target/
! ^ above is incomplete but still good
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMP_Day1.pdf
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMPO_Day2.pdf
!-----------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: spi=>int32, dpf=>real64
! ----------------------------------------------------------------------
  USE OMP_LIB ! you only need the module for omp function calls (get_num_threads)
  USE MPI_F08
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  TYPE(MPI_COMM) :: COMM
  INTEGER(spi) :: RANK,SIZE,IERROR,NUM_TEAMS,NUM_THREADS
!-----------------------------------------------------------------------
  CALL MPI_INIT(IERROR)
  CALL MPI_COMM_RANK(COMM, RANK, IERROR)
  CALL MPI_COMM_SIZE(COMM, SIZE, IERROR)
! ----------------------------------------------------------------------
  IF (RANK.EQ.0) PRINT *, "I AM THE COMMANDER"
  PRINT *, "MPI RANK: ",rank, " WITH",omp_get_num_devices() ,"AVAILABLE DEVICES"
  !$omp target map(tofrom:NUM_TEAMS,NUM_THREADS) map(to: rank)
  !$omp teams
  !$omp parallel
    PRINT *, "I AM A GPU THREAD: team_id:",omp_get_team_num()," thread_id: ", omp_get_thread_num(),"called from cpu_rank: ",rank
    NUM_TEAMS=omp_get_num_teams()
    NUM_THREADS=omp_get_num_threads()
  !$omp end parallel
  !$omp end teams
  !$omp end target
  IF (RANK.EQ.0) PRINT *, "MPI COMM SIZE=", SIZE,"DEVICE OFFLOAD: TEAMS=",NUM_TEAMS,"THREADS=",NUM_THREADS
! ----------------------------------------------------------------------
  CALL MPI_FINALIZE(IERROR)
END PROGRAM HELLO_MPI_GPU_WORLD
