.text
_start_MAIN:
b _global_init
_global_init_back:
str x30, [sp, #0]
str x29, [sp, #-8]
add x29, sp, #-8
add sp, sp, #-16
str x19, [x29, #-8]
str x20, [x29, #-16]
str x21, [x29, #-24]
str x22, [x29, #-32]
str x23, [x29, #-40]
str x24, [x29, #-48]
str x25, [x29, #-56]
str x26, [x29, #-64]
str x27, [x29, #-72]
str x28, [x29, #-80]
str x29, [x29, #-88]
.data
_AR_0: .word 88
.align 3
.text
ldr w19, _AR_0
sub sp, sp, w19
.data
_string_const_1: .asciz "hello"
.align 3
.text
ldr x19, =_string_const_1
mov x0, x19
bl _write_str
.data
_string_const_2: .asciz "\n"
.align 3
.text
ldr x19, =_string_const_2
mov x0, x19
bl _write_str
.data
_integer_const_3: .word 0
.align 3
.text
ldr w19, _integer_const_3
mov w0, w19
b _end_MAIN
_end_MAIN:
ldr x19, [x29, #-8]
ldr x20, [x29, #-16]
ldr x21, [x29, #-24]
ldr x22, [x29, #-32]
ldr x23, [x29, #-40]
ldr x24, [x29, #-48]
ldr x25, [x29, #-56]
ldr x26, [x29, #-64]
ldr x27, [x29, #-72]
ldr x28, [x29, #-80]
ldr x29, [x29, #-88]
ldr x30, [x29, #8]
add sp, x29, #8
ldr x29, [x29, #0]
ret x30
.text
_global_init:
b _global_init_back
