PROJECT = alterego
VASM_DIR = /mnt/ext/Homebrew/toolbox/vasm
VLINK_DIR = /mnt/ext/Homebrew/toolbox/vlink
IDISK_DIR = /mnt/ext/Homebrew/machines/cpc/toolbox/idsk/build

VASMZ80 = $(VASM_DIR)/vasmz80_oldstyle
VASMZ80FLAGS = -Fvobj -nosym

VLINK = $(VLINK_DIR)/vlink
VLINKFLAGS = -b amsdos -M$(PROJECT).map

IDSK = $(IDISK_DIR)/iDSK

linker_script := linker.ld
source_files := $(wildcard *.s)
object_files := $(patsubst %.s, %.o, $(source_files))
bin_files := tileset.bin sprites.bin fonts.bin

# Verbose flag
ifeq ($(V),1)
Q :=
else
Q := @
endif

.PHONY: all clean run

all: dsk

$(PROJECT).bin: $(bin_files) $(object_files) $(linker_script)
	$(Q) $(VLINK) $(VLINKFLAGS) -T $(linker_script) -o $@ $(object_files)

%.o: %.s
	$(Q) $(VASMZ80) $(VASMZ80FLAGS) -o $@ $<

tileset.bin: CPC_TILESET_6X8.bmp
	$(Q) cpct_img2tileset -m 0 -pf {0 1 3 5 6 7 9 10 14 15 16 17 21 23 25 26} -nt -th 8 -tw 6 -of bin $<
	$(Q) mv $(basename $<).bin $@
	$(Q) rm $(basename $<).h $(basename $<).h.s

sprites.bin: CPC_SPRITESET_6X8.bmp
	$(Q) cpct_img2tileset -m 0 -pf {0 1 3 5 6 0 9 10 14 15 16 17 21 23 25 26} -nt -th 16 -tw 6 -of bin $<
	$(Q) mv $(basename $<).bin $@
	$(Q) rm $(basename $<).h $(basename $<).h.s

fonts.bin: CPC_FONT_MODE1_8X8.bmp
	$(Q) cpct_img2tileset -m 1 -pf {0 1 10 26} -nt -tw 8 -th 8 -of bin $<
	$(Q) mv $(basename $<).bin $@
	$(Q) rm $(basename $<).h $(basename $<).h.s

dsk: $(PROJECT).bin 
	@echo Building $(PROJECT).dsk
	$(Q) rm -f $(PROJECT).dsk
	$(Q) $(IDSK) $(PROJECT).dsk -n
	$(Q) $(IDSK) $(PROJECT).dsk -i $< -f

clean:
	$(Q) rm -f $(object_files) $(bin_files) $(PROJECT).bin $(PROJECT).map


