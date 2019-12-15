// http://www.cs.info.mie-u.ac.jp/~toshi/lectures/compiler/clang.html

// token analysis
// clang -cc1 -dump-tokens test_clang.c

// result will be:
/*
int 'int'        [StartOfLine]  Loc=<test_clang.c:6:1>
identifier 'main'        [LeadingSpace] Loc=<test_clang.c:6:5>
l_paren '('      [LeadingSpace] Loc=<test_clang.c:6:10>
void 'void'             Loc=<test_clang.c:6:11>
r_paren ')'             Loc=<test_clang.c:6:15>
l_brace '{'      [LeadingSpace] Loc=<test_clang.c:6:17>
....
 */

// syntax analysis
// clang -cc1 -ast-dump test_clang.c
// result will be:
/*
....
`-FunctionDecl 0x27d5fd0 <test_clang.c:20:1, line:25:1> line:20:5 main 'int (void)'
  `-CompoundStmt 0x2821b60 <col:17, line:25:1>
    |-DeclStmt 0x27d61e0 <line:21:3, col:18>
    | |-VarDecl 0x27d60f8 <col:3, col:7> col:7 used left 'int'
    | `-VarDecl 0x27d6168 <col:3, col:13> col:13 used right 'int'
    |-BinaryOperator 0x27d6240 <line:22:3, col:10> 'int' '='
    | |-DeclRefExpr 0x27d61f8 <col:3> 'int' lvalue Var 0x27d60f8 'left' 'int'
    | `-IntegerLiteral 0x27d6220 <col:10> 'int' 12
    |-BinaryOperator 0x27d62b0 <line:23:3, col:11> 'int' '='
    | |-DeclRefExpr 0x27d6268 <col:3> 'int' lvalue Var 0x27d6168 'right' 'int'
    | `-IntegerLiteral 0x27d6290 <col:11> 'int' 34
    `-ReturnStmt 0x2821b48 <line:24:3, col:27>
      `-BinaryOperator 0x2821b20 <col:10, col:27> 'int' '/'
        |-ParenExpr 0x2821ae0 <col:10, col:23> 'int'
        | `-BinaryOperator 0x2821ab8 <col:11, col:18> 'int' '+'
        |   |-ImplicitCastExpr 0x27d6328 <col:11> 'int' <LValueToRValue>
        |   | `-DeclRefExpr 0x27d62d8 <col:11> 'int' lvalue Var 0x27d60f8 'left' 'int'
        |   `-ImplicitCastExpr 0x2821aa0 <col:18> 'int' <LValueToRValue>
        |     `-DeclRefExpr 0x27d6300 <col:18> 'int' lvalue Var 0x27d6168 'right' 'int'
        `-IntegerLiteral 0x2821b00 <col:27> 'int' 2
 */
// low level virtual machine intermediate representation (LLVM IR)
// clang -emit-llvm -S test_clang.c -o test_clang.ll
// result seen in test_clang.ll

int main (void) {
  int left, right;
  left = 12;
  right = 34;
  return (left + right) / 2;
}
