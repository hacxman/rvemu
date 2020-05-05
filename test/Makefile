add-addi.text: add-addi.s
	riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -Wl,-Ttext=0x0 -nostdlib -g -o add-addi add-addi.s
	riscv64-unknown-elf-objcopy -O binary add-addi add-addi.text

clean:
	rm -f add-addi
	rm -f add-addi.text
