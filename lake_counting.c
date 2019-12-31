// programming contest challenge p-35
// lake counting problem

#define N_MAX 10
#define M_MAX 12

char pond[N_MAX][M_MAX] = {
    "W........WW.",
    ".WWW.....WWW",
    "....WW...WW.",
    ".........WW.",
    ".........W..",
    "..W......W..",
    ".W.W.....WW.",
    "W.W.W.....W.",
    ".W.W......W.",
    "..W.......W."
};

struct Direction {
  int dx_;
  int dy_;
};
struct Direction diretions[4]={
  {1,0},
  {0,1},
  {-1,0},
  {0,-1}
};

void FillLake(int in, int im) {
}


int main() {
  for (int im = 0; im < M_MAX; im++) {
    for (int in = 0; in < N_MAX; in++) {
    }
  }
  return 0;
}
