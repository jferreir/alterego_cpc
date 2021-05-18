/* vlink linker script */

ENTRY(start)

stack_size = 256;

MEMORY
{
  RAM : ORIGIN = 0x4000, LENGTH = 0x7000
}

SECTIONS
{
  Stack (NOLOAD) :
  {
    __stack_start = .;
    . = . + stack_size;
    __stack_end = .;
    __initial_sp = .;
  } > RAM
  
  .text : { *(tables) *(tiles) *(sprites) *(nametable) *(fonts) *(text) } > RAM
  .data (NOLOAD) : { *(data) } > RAM
  .bss (NOLOAD) : { *(bss) } > RAM
}
