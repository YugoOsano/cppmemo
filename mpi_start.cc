// Open MPI Mailing list
// error while loading shared libraries:libopen-pal.so.0
// http://www.open-mpi.org/community/lists/users/2011/08/17074.php

// UBUNTU での正しい LD_LIBRARY_PATH 設定法
// http://d.hatena.ne.jp/hikaru149/20101031/1288533463

//  C++でOpenMPI入門1 ランク
// http://d.hatena.ne.jp/bettamodoki/20120626/1340694531

// 統計物理屋のための簡単MPI講座
// http://apollon.issp.u-tokyo.ac.jp/~watanabe/pdf/mpinote.pdf

// compiled by: mpic++ mpi_start.cc
// run by:      mpirun -np 4 ./a.out

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <mpi.h>

double myrand(void){
  return (double)rand()/(double)RAND_MAX;
}

double calc_pi(int seed, int trial){
  srand(seed);
  int n = 0;
  for(int i=0; i<trial; i++){
    double x = myrand();
    double y = myrand();
    if(x*x + y*y < 1.0)
      n++;
  }
  return 4.0*(double)n/(double)trial;
}

int main(int argc, char *argv[]){
  /*
  MPI::Init(argc, argv);
  int rank = MPI::COMM_WORLD.Get_rank();
  std::cout << "My rank = " << rank << std::endl;
  MPI::Finalize();
  */
  int rank, size;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  
  double pi = calc_pi(rank, 1000000);
  printf("rank=%d/%d: pi = %e \n", rank, size, pi);

  MPI_Barrier(MPI_COMM_WORLD);
  double sum = 0.0;
  //MPI_Allreduce(&pi, &sum, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
  MPI_Reduce(&pi, &sum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

  sum = sum / (double)size;

  if(0==rank){
    printf("average = %e\n", sum);
  }
  MPI_Finalize();
}
