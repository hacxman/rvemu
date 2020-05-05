# rvemu
Yet another RISC-V emulator, this time in Haskell.

FAR FROM COMPLETE. Contains bugs! Even basic ISA isn't done properly yet.

This stuff is ugly and slow but was fun to write.

To try:

You need gdb-multiarch (on ubuntu), riscv64-unknown-elf-gcc
and riscv64-unknown-elf-objcopy.

$ stack test

Loads a add-addi.text file into mem and pauses.
You need to unpause it via GDB.

$ cd test
$ make && gdb-multiarch -ex "tar ext localhost:4243" -ex "layout regs" add-addi

Then press 'c' and Enter in GDB.
Pause via Ctrl-C (in GDB).
Step with 's' or 'si'.


Emulator will have virtio networking, serial console and a block device
all using virtio MMIO.
