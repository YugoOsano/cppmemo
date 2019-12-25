// intel2gas
// https://seesaawiki.jp/w/yamaneko1144/d/GAS%a4%c8NASM%a4%c8NASK%a4%cb%a4%c4%a4%a4%a4%c6
// Text User Interface mode can be used by (-q to skip initial message)
// gdb --tui -q a.out

// gdb) layout asm
// to see assembler

// gdb) layout prev
// to go back c code

void null() {
  return;
}

int return_zero() {
  return 0;
}

int return_one() {
  return 1;
}

int main () {
  null();
  return_zero();
  return_one();
  return 0;
}
