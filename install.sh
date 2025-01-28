#!/bin/bash
#simple compile script
#
FC=nvfortran
mpifc=mpif90
gpu_tag=cc86
#note you can add -stdpar=gpu
#for do concurrent offloads
${FC} hello_world.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o hw.x
${FC} offload_loop1.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o off1.x
${FC} offload_loop2.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o off2.x
${FC} offload_loop3.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o off3.x
${FC} offload_loop4.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o off4.x
${mpifc} hello_world2.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o hw2.x
${mpifc} offload_loop5.f90 -Wall -mp=gpu -gpu=${gpu_tag} -Minfo=all -o off5.x
