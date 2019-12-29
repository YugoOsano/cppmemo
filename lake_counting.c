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

int main() {
  struct Direction diretions[2]={
    {0,0}
  };
  return 0;
}
