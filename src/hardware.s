  include "hardware.i"

  global wait_vbl
  global display_off
  global set_pal
  global set_isr
  global clear_screen
  global put_tile_6x8
  global put_tile_8x8
  global clear_tile_6x8
  global put_sprite
  global get_vaddr
  global scan_input
  global input_state

  section tables

  align 8
  
flip_tbl:
  byte $ff, $ee, $dd, $cc, $bb, $aa, $99, $88
  byte $77, $66, $55, $44, $33, $22, $11, $00 
  byte $ee, $ee, $cc, $cc, $aa, $aa, $88, $88 
  byte $66, $66, $44, $44, $22, $22, $00, $00 
  byte $dd, $cc, $dd, $cc, $99, $88, $99, $88 
  byte $55, $44, $55, $44, $11, $00, $11, $00 
  byte $cc, $cc, $cc, $cc, $88, $88, $88, $88 
  byte $44, $44, $44, $44, $00, $00, $00, $00 
  byte $bb, $aa, $99, $88, $bb, $aa, $99, $88 
  byte $33, $22, $11, $00, $33, $22, $11, $00 
  byte $aa, $aa, $88, $88, $aa, $aa, $88, $88 
  byte $22, $22, $00, $00, $22, $22, $00, $00 
  byte $99, $88, $99, $88, $99, $88, $99, $88 
  byte $11, $00, $11, $00, $11, $00, $11, $00 
  byte $88, $88, $88, $88, $88, $88, $88, $88 
  byte $00, $00, $00, $00, $00, $00, $00, $00 
  byte $77, $66, $55, $44, $33, $22, $11, $00 
  byte $77, $66, $55, $44, $33, $22, $11, $00 
  byte $66, $66, $44, $44, $22, $22, $00, $00 
  byte $66, $66, $44, $44, $22, $22, $00, $00 
  byte $55, $44, $55, $44, $11, $00, $11, $00 
  byte $55, $44, $55, $44, $11, $00, $11, $00 
  byte $44, $44, $44, $44, $00, $00, $00, $00 
  byte $44, $44, $44, $44, $00, $00, $00, $00 
  byte $33, $22, $11, $00, $33, $22, $11, $00 
  byte $33, $22, $11, $00, $33, $22, $11, $00 
  byte $22, $22, $00, $00, $22, $22, $00, $00 
  byte $22, $22, $00, $00, $22, $22, $00, $00 
  byte $11, $00, $11, $00, $11, $00, $11, $00 
  byte $11, $00, $11, $00, $11, $00, $11, $00 
  byte $00, $00, $00, $00, $00, $00, $00, $00 
  byte $00, $00, $00, $00, $00, $00, $00, $00 

  section text

  align 0

;==============================================================================;
; wait_vbl
; Synchronize your program to the video vertical blanking signal (VBL), which is 1/50th of a second.
; Uses: A, B

wait_vbl:
  ld b,PPI_PORTB/256
.wait_vbl_0
  in a,(c)            ;; [4] read PPI port B input
                      ;; (bit 0 = "1" if vsync is active,
                      ;;  or bit 0 = "0" if vsync is in-active)
  rra                 ;; [1] put bit 0 into carry flag
  jp c,.wait_vbl_0    ;; [3] if carry set, loop, otherwise continue

.wait_vbl_1
  in a,(c)            ;; [4] read PPI port B input
                      ;; (bit 0 = "1" if vsync is active,
                      ;;  or bit 0 = "0" if vsync is in-active)
  rra                 ;; [1] put bit 0 into carry flag
  jp nc,.wait_vbl_1   ;; [3] if carry not set, loop, otherwise continue

  ret

;==============================================================================;
; display_off
; Blank the display.

display_off:
  ld bc,$7f11             ; #7fxx GATE ARRAY IO port
  ld a,$54

.loop:
  dec c
  out (c),c               ; Select PEN
  out (c),a               ; Set color for selected pen
  jr nz,.loop
  ret

;==============================================================================;
; set_pal
; Load one or more 16-color palette at once.

; Params:
; hl = palette address
; c  = number of colors (1-16)

set_pal:
  ld b,$7f                ; #7fxx GATE ARRAY IO port

.loop:
  ld a,(hl)
  inc hl
  dec c
  out (c),c               ; Select PEN
  out (c),a               ; Set color for selected pen
  jr nz,.loop
  ret

;==============================================================================;
; clear_screen
; Fill VRAM with pattern

; Params:
; a  = pattern
; Uses HL,DE,BC

clear_screen:
  ld hl,$c000
  ld (hl),a
  ld de,$c001

  ld bc,$3fff
  ldir

  ret

;;==============================================================================;
;; put_tile_8x8
;; Places 8x8 tile

;; Params:
;; HL = tile address
;; DE = screen address
;; Uses A, B

put_tile_8x8:
  ld b,8

.loop:
  ld a,(hl)
  inc l
  ld (de),a
  inc de

  ld a,(hl)
  inc l
  ld (de),a

  dec de

  ld a,d          ; Go to next scanline
  add a,8
  ld d,a

  djnz .loop

  or $c0
  ld d,a

  ret

;;==============================================================================;
;; put_tile_6x8
;; Places 6x8 tile

;; Params:
;; hl = tile address
;; de = screen address
;; Uses A, B

put_tile_6x8:
  ld b,8

.loop:
  ld a,(hl)
  inc hl
  ld (de),a
  inc de

  ld a,(hl)
  inc hl
  ld (de),a
  inc de

  ld a,(hl)
  inc hl
  ld (de),a

  dec de
  dec de

  ld a,d          ; Go to next scanline
  add a,8
  ld d,a

  djnz .loop

  or $c0
  ld d,a

  ret

;;==============================================================================;
;; put_tile_6x8
;; Places 6x8 tile

;; Params:
;; DE = screen address
;; Uses A, B

clear_tile_6x8:
  ld b,8

.loop:
  xor a

  ld (de),a
  inc de

  ld (de),a
  inc de

  ld (de),a

  dec de
  dec de

  ld a,d          ; Go to next scanline
  add a,8
  ld d,a

  djnz .loop

  ret

;==============================================================================;
; put_sprite
; Places 16x16 sprite

; Params:
;   hl = sprite address
;   de = screen address
;   bc = sprite buffer (aligned 64)
put_sprite:

  ld iyh,16            ; sprite height

.loop:
  ld a,(de)            ; screen byte
  ld (bc),a            ; store screen byte to restore the background
  inc c
  and (hl)             ; mask byte
  inc l
  or (hl)              ; sprite byte
  inc l
  ld (de),a
  inc e

  ld a,(de)            ; screen byte
  ld (bc),a            ; store screen byte to restore the background
  inc c
  and (hl)             ; mask byte
  inc l
  or (hl)              ; sprite byte
  inc l
  ld (de),a
  inc e

  ld a,(de)            ; screen byte
  ld (bc),a            ; store screen byte to restore the background
  inc c
  and (hl)             ; mask byte
  inc l
  or (hl)              ; sprite byte
  inc l
  ld (de),a
  inc e

  ld a,(de)            ; screen byte
  ld (bc),a            ; store screen byte to restore the background
  inc c
  and (hl)             ; mask byte
  inc l
  or (hl)              ; sprite byte
  inc l
  ld (de),a

  dec iyh
  ret z

  dec e
  dec e
  dec e

  ld a,d                ; Go to next scanline
  add a,8
  ld d,a

  jr nc,.loop
    
  ld a,e                ; Go to next row
  add $40
  ld e,a
  ld a,d
  adc $c0
  ld d,a

  jr .loop

  ret

;==============================================================================    
; get_vaddr
; Calculates vram address of x [0,255] y [0,255] coordinates

; Params:
; HL = xy coord
; Returns:
; DE = vram address
; Modifies:
; AF
;==============================================================================    
get_vaddr:
  ld a,l                  ; a = y address
  and $18                 ; obtain bits 3 and 4
  add a,a                 ; shift right by 3
  add a,a
  add a,a
  srl h
  srl h                   ; mode 1 divide by 4
  or h                    ; include x address info in lower byte screen address
  ld e,a                  ; lower byte screen address

  ld a,l                  ; a = y address
  and $e7                 ; get bits 7,6,5,2,1,0
  rlca                    ; rotate 3 time left
  rlca
  rlca

  or $c0
  ld d,a                  ; higher byte screen address
  ret

;==============================================================================;
; set_isr
; Set the ISR routine to be executed 20ms

; Params:
; HL = function address
set_isr:
  di
  ld ($0039),hl
  ei
  ret

;==============================================================================;
; scan input
; scan keyboard and joystick
; input_state =  x | x | Joy 0 Fire 1 | Joy 0 Fire 2 | Joy 0 Right | Joy 0 Left | Joy 0 Down | Joy 0 Up
;
; Params:
; None

scan_input:
  ld bc,$f40e                 ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 00
  out (c),c                   ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portA
    
  ld bc,$f6c0                 ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
  out (c),c                   ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portC
                              ; portC = 0xc0 => AY BDIR,BC1 = 11 => LATCH ADDRESS, register address in portA(0x0e)
                              ; (At this stage the PSG will have selected the register specified by the data at PPI Port A). 
                              ; PSG reg 14 controls PSG IO port A
                                
  ld c,0                      ; Select PSG operation: inactive, by setting bit 7="0" and bit 6="0" of PPI Port C.
  out (c),c
  
  inc b                       ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 11                 
  ld a,$92                    ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to control
  out (c),a                   ; 0x92 => portA (input), portB (input), portC (output)
                              ; (At this stage it is possible to read from PSG register 14)   

  ld hl,input_state
  ; Check joystick
  ; ===========================================================================
  ld bc,$f649
  out (c),c                   ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
                              ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portC
                              ; portC = 0x40 => AY BDIR,BC1 = 01 => READ from PSG
                              ; portC[3:] = matrix row to activate
  
  ld b,$f4                    ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 00
  in a,(c)                    ; IN instruction, bIORD (PPI bRD) => operation = portA transfer to data bus
  
  cpl
  ld (hl),a                   ; x | x | Joy 0 Fire 1 | Joy 0 Fire 2 | Joy 0 Right | Joy 0 Left | Joy 0 Down | Joy 0 Up

  ; Check keyword
  ; ===========================================================================
  ld bc,$f645                 ; space key
  out (c),c                   ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
                              ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portC
                              ; portC = 0x40 => AY BDIR,BC1 = 01 => READ from PSG
                              ; portC[3:] = matrix row to activate
  
  ld b,$f4                    ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 00
  in a,(c)                    ; IN instruction, bIORD (PPI bRD) => operation = portA transfer to data bus
    
  cpl
  rrca
  rrca
  and $20
  or (hl)
  ld (hl),a
    
  ; ============================================================================================================================
  ld bc,$f644                 ; o key
  out (c),c                   ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
                              ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portC
                              ; portC = 0x40 => AY BDIR,BC1 = 01 => READ from PSG
                              ; portC[3:] = matrix row to activate
  
  ld b,$f4                    ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 00
  in a,(c)                    ; IN instruction, bIORD (PPI bRD) => operation = portA transfer to data bus
    
  cpl
  and $04
  or (hl)
  ld (hl),a
    
  ; ============================================================================================================================
  ld bc,$f643                 ; p key
  out (c),c                   ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
                              ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portC
                              ; portC = 0x40 => AY BDIR,BC1 = 01 => READ from PSG
                              ; portC[3:] = matrix row to activate
  
  ld b,$f4                    ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 00
  in a,(c)                    ; IN instruction, bIORD (PPI bRD) => operation = portA transfer to data bus
    
  cpl
  and $08
  or (hl)
  ld (hl),a
    
  ; ============================================================================================================================
  ld bc,$f648                 ; q,a key
  out (c),c                   ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
                              ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to portC
                              ; portC = 0x40 => AY BDIR,BC1 = 01 => READ from PSG
                              ; portC[3:] = matrix row to activate
  
  ld b,$f4                    ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 00
  in a,(c)                    ; IN instruction, bIORD (PPI bRD) => operation = portA transfer to data bus
    
  cpl
  rrca
  rrca
  rrca
  ld c,a
  and $01
  or (hl)
  ld (hl),a
  ld a,c
  rrca
  and $02
  or (hl)
  ld (hl),a
      
  ; ============================================================================================================================
  ld bc,$f700                 ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 11                 
  ld a,$82                    ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to control
  out (c),a                   ; 0x82 => portA (output), portB (input), portC (output)
  
  dec b                       ; A11 (PPI bCS) = 0, A9:8 (PPI A1:0) = 10                 
  out (c),c                   ; OUT instruction, bIOWR (PPI bWR) => operation = data bus transfer to control
                              ; Select PSG operation: inactive, by setting bit 7="0" and bit 6="0" of PPI Port C. 
                              ; (This stage is required by the CPC+. If it is missing the scanning operation will fail) 
  
  ret

  section bss

input_state:
  defs 1
