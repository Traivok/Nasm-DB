#switcher
switcher_file = switcher

#exercise 1
ex1_file = Exercicio1

#exercise 2
main_file = Exercicio2
main_pos = 2
main_size = 20

boot_disk = disk.img
block_size = 512
disk_size = 100

nasm_flags = -f bin
qemu_flags = -fda

ex1: basic_comp basic_qemu clean

ex2: create_disk switcher_only main_only write_switcher write_main launch_qemu clean



create_disk:
	@dd if=/dev/zero of=$(boot_disk) bs=$(block_size) count=$(disk_size) status=noxfer

switcher_only:
	@nasm $(nasm_flags) $(switcher_file).asm -o $(switcher_file).bin

main_only:
	@nasm $(nasm_flags) $(main_file).asm -o $(main_file).bin

write_switcher:
	@dd if=$(switcher_file).bin of=$(boot_disk) bs=$(block_size) count=2 conv=notrunc status=noxfer

write_main:
	@dd if=$(main_file).bin of=$(boot_disk) bs=$(block_size) seek=$(main_pos) count=$(main_size) conv=notrunc

launch_qemu:
	clear
	@qemu-system-x86_64 $(qemu_flags) $(boot_disk)

clean:
	@rm -f *.bin $(boot_disk) *~
	clear

basic_comp:
	@nasm -f bin $(ex1_file).asm -o $(ex1_file).bin

basic_qemu:
	clear
	@qemu-system-i386 $(qemu_flags) $(ex1_file).bin