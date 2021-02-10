/* transcribed from StackOverFlow 647054 */

#include <search.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct {
  int   key;
  char* value;
} intStrMap;

int Compare(const void* l, const void* r) {
  const intStrMap *lm = l;
  const intStrMap *lr = r;
  return lm->key - lr->key;
}

int main(int argc, char** argv) {
  void* root = 0;
  intStrMap* a = malloc(sizeof(intStrMap));
  a->key = 2;
  a->value = strdup("two");
  tsearch(a, &root, Compare); /* insert */

  intStrMap* find_a = malloc(sizeof(intStrMap));
  find_a->key = 2;

  void* r = tfind(find_a, &root, Compare); /*read*/
  printf("%s\n", (*(intStrMap**)r)->value);
  return 0;
}
