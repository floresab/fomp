PROGRAM HELLO_GPU_WORLD
! Abraham R. Flores 
! https://enccs.github.io/openmp-gpu/target/
! ^ above is incomplete but still good
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMP_Day1.pdf
! https://www.olcf.ornl.gov/wp-content/uploads/2021/08/ITOpenMPO_Day2.pdf
! ----------------------------------------------------------------------
  USE OMP_LIB
! you only need the module for omp function calls (get_num_threads)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
  INTEGER :: NUM_TEAMS,NUM_THREADS
! ----------------------------------------------------------------------
    PRINT *, "I AM THE HOST"
    PRINT *, "NUMBER OF AVAILABLE DEVICES", omp_get_num_devices()
    !$omp target map(tofrom:NUM_TEAMS,NUM_THREADS)
    !$omp teams
    !$omp parallel
      PRINT *, "I AM A GPU THREAD: team_id:",omp_get_team_num()," thread_id: ", omp_get_thread_num()
      NUM_TEAMS=omp_get_num_teams()
      NUM_THREADS=omp_get_num_threads()
    !$omp end parallel
    !$omp end teams
    !$omp end target
    PRINT *, "DEVICE OFFLOAD: TEAMS=",NUM_TEAMS,"THREADS=",NUM_THREADS
! ----------------------------------------------------------------------
END PROGRAM HELLO_GPU_WORLD
