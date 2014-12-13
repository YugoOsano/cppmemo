// Open MPI Mailing list
// error while loading shared libraries:libopen-pal.so.0
// http://www.open-mpi.org/community/lists/users/2011/08/17074.php

// UBUNTU での正しい LD_LIBRARY_PATH 設定法
// http://d.hatena.ne.jp/hikaru149/20101031/1288533463

//  C++でOpenMPI入門1 ランク
// http://d.hatena.ne.jp/bettamodoki/20120626/1340694531

// 統計物理屋のための簡単MPI講座
// http://apollon.issp.u-tokyo.ac.jp/~watanabe/pdf/mpinote.pdf

#include <iostream>
#include <mpi.h>

int main(int argc, char *argv[]){
  /*
  MPI::Init(argc, argv);
  int rank = MPI::COMM_WORLD.Get_rank();
  std::cout << "My rank = " << rank << std::endl;
  MPI::Finalize();
  */
  int rank;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  std::cout << "My rank = " << rank << std::endl;
  MPI_Finalize();
}
