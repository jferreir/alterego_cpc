  include "hardware.i"

SCREEN_WIDTH = 176
SCREEN_WIDTH_BYTES = (SCREEN_WIDTH / 2)
SCREEN_HEIGHT = 184
TILE_WIDTH = 6
TILE_WIDTH_BYTES = (TILE_WIDTH / 2)
TILE_HEIGHT = 8
TILE_SIZE_BYTES = (TILE_WIDTH / 2) * TILE_HEIGHT
SPRITE_WIDTH = 6
SPRITE_WIDTH_BYTES = (TILE_WIDTH / 2)
SPRITE_HEIGHT = 16
SPRITE_SIZE_BYTES = (SPRITE_WIDTH / 2) * SPRITE_HEIGHT
NUM_TILES_WIDTH = 28 ;; 4 pixels extra at each side for border
NUM_TILES_HEIGHT = 20
NUM_FONTS_8x16 = 28
FONTS_8x8_SIZE_BYTES = 16
FONTS_8x16_SIZE_BYTES = (FONTS_8x8_SIZE_BYTES * 2)

  global start

  section text

;;==============================================================================;
;; The entry point

start:
  di
  im 1 
  ld sp,__initial_sp
  ld a,$c3
  ld ($0038),a
  jp main

;;==============================================================================;
;; Main routine

main:

  call wait_vbl
  set_ga_reg RMR, RMR_RST_INT | RMR_DIS_UROM | RMR_DIS_LROM | RMR_MODE_1

  call display_off

  ;; Set screen dimmensions
  set_crtc_reg HDISP,44 ;; SCREEN_WIDTH / 2 pixels per byte
  set_crtc_reg HSYNCPOS,48
  set_crtc_reg VDISP,23

  set_border BLACK
  
  ;; blank screen
  xor a
  call clear_screen

  ;; draw title screen
  ld de,$c000 + 3*29 + 2
  ld hl,title_screen
  call draw_screen

  ;; Place main character in title screen
  ld de,$c000 + (SCREEN_WIDTH_BYTES * 2) + (TILE_WIDTH_BYTES * 4)
  ld hl,sprites
  call put_tile_6x8
  ld bc,SCREEN_WIDTH_BYTES
  ex de,hl
  add hl,bc
  ex de,hl
  call put_tile_6x8


  ;; Place alter ego character in title screen
  ld de,$c000 + (SCREEN_WIDTH_BYTES * 7) + (TILE_WIDTH_BYTES * 24)
  ld hl,sprites + (9 * SPRITE_SIZE_BYTES)
  call put_tile_6x8
  ld bc,SCREEN_WIDTH_BYTES
  ex de,hl
  add hl,bc
  ex de,hl
  call put_tile_6x8

  ;; print "PRESS START"
  ld de,$c650
  ld hl,string_0
  call put_string_8x16

  ;; Set up ISR for screen split
  ld hl,isr_title_tabint
  ld (isr_vector_table+1),hl
  call wait_vbl
  ld hl,isr
  call set_isr

.loop
  jr .loop

;;==============================================================================;
;; draw a game screen of NUM_GAME_TILES_WIDTHxNUM_GAME_TILES_HEIGHT using 
;; 6x8 tiles

draw_screen:
  ld c,NUM_TILES_HEIGHT
.loop_title0
  ld b,NUM_TILES_WIDTH
.loop_title1
  ld a,(hl)
  cp $ff              ;; skip empty tile
  jr z,.skip
  push hl
  push bc
  ld h,0
  ld l,a
  add hl,hl
  add hl,hl
  add hl,hl
  ld b,h
  ld c,l
  add hl,hl
  add hl,bc
  ld bc,tiles
  add hl,bc
  call put_tile_6x8
  pop bc
  pop hl
.skip
  inc hl
  inc de
  inc de
  inc de
  djnz .loop_title1

  inc de
  inc de
  inc de
  inc de
  dec c
  jr nz,.loop_title0

  ret

;;==============================================================================;
;; Write string of 8x16 chars to screen

put_string_8x16:
  ld a,(hl)
  or a
  ret z
  push de
  push hl
  sbc a,'A'
  ld h,0
  ld l,a
  add hl,hl
  add hl,hl
  add hl,hl
  add hl,hl
  ld bc,fonts
  add hl,bc
  push hl
  call put_tile_8x8
  ld bc,SCREEN_WIDTH_BYTES
  ex de,hl
  add hl,bc
  ex de,hl
  pop hl
  ld bc,NUM_FONTS_8x16*FONTS_8x8_SIZE_BYTES   ;; go to lower half of 8x16 character
  add hl,bc
  call put_tile_8x8
  pop hl
  inc hl
  pop de
  inc de
  inc de
  jr put_string_8x16

;;==============================================================================;
;; ISR

isr:
  push af
  push bc
  push de
  push hl

  ld hl,.counti+1
  ld b,$f5
  in a,(c)
  rra
  jp nc,.nexti
  ld (hl),$ff            ; In VBL happens the interrupt counter will contain a zero

.nexti:
  inc (hl)

.counti:
  ld a,0
  add a,a
  ld c,a
  ld b,0

isr_vector_table:
  ld hl,0
  add hl,bc

  ld a,(hl)
  inc hl
  ld h,(hl)
  ld l,a
  ld bc,.exint
  push bc
  jp (hl)

.exint
  pop hl
  pop de
  pop bc
  pop af
  ei
  ret

;;==============================================================================;
;; Interrupt table for title screen

isr_title_tabint:
  defw int0_title ;; 240 (VSYNC) + 2
  defw int1_title ;; 294
  defw int2_title ;; 34
  defw int3_title ;; 86
  defw int4_title ;; 138
  defw int5_title ;; 192

int0_title:
  ret

int1_title:
  ret

int2_title:
  ret

int3_title:
  ret

int4_title:
  set_ga_reg RMR, RMR_DIS_UROM | RMR_DIS_LROM | RMR_MODE_1

	ld hl,frameno
	ld a,(hl)
	and $10
  jr z,.skip
  call display_off
  ld bc,200
.loop:
  dec bc
  ld a,b
  or c
  jr nz,.loop

.skip:   
  ld hl,palette_1
  ld c,4
  jp set_pal

int5_title:
  set_ga_reg RMR, RMR_DIS_UROM | RMR_DIS_LROM | RMR_MODE_0

  ld hl,palette_0
  ld c,16
  call set_pal
  jp put_char_scroll

;;==============================================================================;
;; scroll message in title screen

put_char_scroll:
  ei
	ld hl,frameno
	ld a,(hl)
  inc (hl)
	and $7
	jr nz,put_char_scroll_1

put_char_scroll_0:
  call put_char_scroll_1
  ld hl,char_pointer_scroll
  ld a,(hl)
  inc (hl)

  ld l,a
  ld h,0
  ld bc,scroll_str
  add hl,bc
  ld a,(hl)
  or a
  jr z,.put_char_scroll_0
  sbc a,'A'
  ld de,$c790+88
  ld h,0
  ld l,a
  add hl,hl
  add hl,hl
  add hl,hl
  add hl,hl
  ld bc,fonts+(NUM_FONTS_8x16*FONTS_8x16_SIZE_BYTES)
  add hl,bc
  jp put_tile_8x8
.put_char_scroll_0:  
  ld hl,char_pointer_scroll
  ld (hl),a
  ret

;; de = start of line to scroll
scroll_line:
  ld b,90
  ld h,d
  ld l,e

.scroll_line_0:
  inc hl
  ld a,(hl)
  and $88
  rrca
  rrca
  rrca
  ld c,a
  ld a,(de)
  and $77
  add a
  or c
  ld (de),a
  inc de
  djnz .scroll_line_0
  ret

put_char_scroll_1:
  ld de,$cf90
  call scroll_line
  ld de,$d790
  call scroll_line
  ld de,$df90
  call scroll_line
  ld de,$e790
  call scroll_line
  ld de,$ef90
  call scroll_line
  ld de,$f790
  call scroll_line
  ld de,$ff90
  jp scroll_line

char_pointer_scroll:
  byte 0

frameno:
  byte $ff


palette_0:
  byte $4b, $43, $5b, $5a, $4f, $47, $4e, $5f, $46, $56, $45, $4c, $5d, $5c, $44, $54

palette_1:
  byte $4b, $46, $44, $54

string_0:
  string "PRESS  START"

scroll_str:
  string "ORIGINAL ZX SPECTRUM GAME DENIS GRACHEV [\_]^^   CPC CONVERSION BASED ON NES VERSION BY SHIRU   CODE BY ZOGO   GRAPHICS BY DADMAN   RETROWORKS _]_^                                                     "
