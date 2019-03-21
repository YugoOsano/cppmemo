        ;FILE "see_assembler_sample.c"
        SECTION .text
        GLOBAL  null
        GLOBAL null:function
null: 
.LFB0: 
MISMATCH: "        .cfi_startproc"
MISMATCH: "        pushq   %rbp"
MISMATCH: "        .cfi_def_cfa_offset 16"
MISMATCH: "        .cfi_offset 6, -16"
        movq    rbp,rsp
MISMATCH: "        .cfi_def_cfa_register 6"
        nop
MISMATCH: "        popq    %rbp"
MISMATCH: "        .cfi_def_cfa 7, 8"
        ret
MISMATCH: "        .cfi_endproc"
.LFE0: 
        GLOBAL  null:function (.-null)
        GLOBAL  return_zero
        GLOBAL return_zero:function
return_zero: 
.LFB1: 
MISMATCH: "        .cfi_startproc"
MISMATCH: "        pushq   %rbp"
MISMATCH: "        .cfi_def_cfa_offset 16"
MISMATCH: "        .cfi_offset 6, -16"
        movq    rbp,rsp
MISMATCH: "        .cfi_def_cfa_register 6"
        mov     eax,0
MISMATCH: "        popq    %rbp"
MISMATCH: "        .cfi_def_cfa 7, 8"
        ret
MISMATCH: "        .cfi_endproc"
.LFE1: 
        GLOBAL  return_zero:function (.-return_zero)
        GLOBAL  return_one
        GLOBAL return_one:function
return_one: 
.LFB2: 
MISMATCH: "        .cfi_startproc"
MISMATCH: "        pushq   %rbp"
MISMATCH: "        .cfi_def_cfa_offset 16"
MISMATCH: "        .cfi_offset 6, -16"
        movq    rbp,rsp
MISMATCH: "        .cfi_def_cfa_register 6"
        mov     eax,1
MISMATCH: "        popq    %rbp"
MISMATCH: "        .cfi_def_cfa 7, 8"
        ret
MISMATCH: "        .cfi_endproc"
.LFE2: 
        GLOBAL  return_one:function (.-return_one)
        ;IDENT "GCC: (Ubuntu 5.5.0-12ubuntu1~16.04) 5.5.0 20171010"
MISMATCH: "        .section        .note.GNU-stack,"",@progbits"

