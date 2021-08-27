int ReturnInt(const int input) {
 tag_in_func:
  const int to_return = input;
  if (to_return < 0) goto tag_in_func;
  return to_return;
}
int main() {
  // tag:
  //goto tag;
  ReturnInt(1);
  return 0;
}
