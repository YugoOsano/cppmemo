// serialization library tpl was gained from:
// https://troydhanson.github.io/tpl/

// installed by autotools;
// all commands from libtoolize to automake --add-missing
// (see https://stackoverflow.com/questions/22603163/automake-error-ltmain-sh-not-found)

// compiled by:
// gcc tpl_serialization.c -ltpl
// run by:
// LD_LIBRARY_PATH=/usr/local/lib ./a.out

#include <tpl.h>

int main(int argc, char *argv[]) {
  tpl_node *tn;
  int id=0;
  char *name, *names[] = { "joe", "bob", "cary" };

  tn = tpl_map("A(is)", &id, &name);

  for(name=names[0]; id < 3; name=names[++id]) {
    tpl_pack(tn,1);
  }

  //tpl_dump(tn, TPL_FILE, "users.tpl"); //-> failed to mmap
  tpl_free(tn);
  return 0;
}
