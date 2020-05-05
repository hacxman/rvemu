main:
  addi x22, x0, 16
  addi x22, x22, 16
  addi x22, x22, 16
  addi x22, x22, 16
  addi x22, x22, 16
  addi x20, x0, 0x24
  lui x10, 64
  sw x20, 0(x10)
  addi x15, x0, 67
  addi x12, x0, 1024
  sb x15, 0(x12)
  jal x1, 12
/*  addi x29, x0, 5
  addi x30, x0, 37
  add x31, x30, x29
  xor x23, x30, x29
  and x22, x30, x29
  or x21, x30, x29
  ori x24, x30, 2
  andi x25, x30, 3
  xori x26, x30, 4
  add x15, x30, x29
  sub x16, x30, x29
  sltu x18, x29, x29
  sltu x19, x29, x30
  slt x19, x29, x30
  slt x19, x29, x29
  sra x2, x29, x30
  srl x3, x29, x30
  sll x4, x29, x30
  srli x5, x29, 2
  srai x6, x29, 1
  lui x1, 0xaa
  auipc x2, 0xaa

  lb x3, 0(x0) */
int00:
  addi x13, x0, 65
  addi x11, x0, 1024
  sb x13, 0(x11)
  ret
