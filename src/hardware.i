GATE_ARRAY      = $7f00
PENR            = $00
INKR            = $40
RMR             = $80
MMR             = $c0

RMR_RST_INT     = $10
RMR_DIS_UROM    = $08
RMR_DIS_LROM    = $04
RMR_MODE_0      = $00
RMR_MODE_1      = $01
RMR_MODE_2      = $02

WHITE           = $40
SEE_GREEN       = $42
PASTEL_YELLOW   = $43
BLUE            = $44
PURPLE          = $45
CYAN            = $46
PINK            = $47
BRIGHT_YELLOW   = $4a
BRIGHT_WHITE    = $4b
BRIGHT_RED      = $4c
BRIGHT_MAGENTA  = $4d
ORANGE          = $4e
PASTEL_MAGENTA  = $4f
BRIGHT_GREEN    = $52
BRIGHT_CYAN     = $53
BLACK           = $54
BRIGHT_BLUE     = $55
GREEN           = $56
SKY_BLUE        = $57
MAGENTA         = $58
PASTEL_GREEN    = $59
LIME            = $5a
PASTEL_CYAN     = $5b
RED             = $5c
MAUVE           = $5d
YELLOW          = $5e
PASTEL_BLUE     = $5f

CRTC_SEL        = $bc00 ;; Select 6845 register
CRTC_WR         = $bd00 ;; Write 6845 register data
HTOT            = $00
HDISP           = $01
HSYNCPOS        = $02
HSYNCWIDTH      = $03
VTOT            = $04
VADJ            = $05
VDISP           = $06
VSYNCPOS        = $07
INTERLACE       = $08
SCANLINES       = $09
START_ADDR_HI   = $0c
START_ADDR_LO   = $0d

PPI_PORTA       = $f400 ;; Port A Data 	Read/Write 	In/Out 	PSG (Sound/Keyboard/Joystick)
PPI_PORTB       = $f500 ;; Port B Data 	Read/Write 	In 	    Vsync/Jumpers/PrinterBusy/CasIn/Exp
PPI_PORTC       = $f600 ;; Port C Data 	Read/Write 	Out 	  KeybRow/CasOut/PSG
PPI_CTRL        = $f700 ;; Control 	    Write Only 	Out 	  Control

;;==============================================================================;
;; set_vreg
;; A macro for setting one of the 6845 video registers.
;; Uses BC

;; Params:
;; reg = register number
;; val = register value

  macro set_crtc_reg, reg, val
  ld bc,CRTC_SEL | \reg
  out (c),c
  ld bc,CRTC_WR | \val
  out (c),c
  endm
 
;;==============================================================================;
;; set_ga_reg
;; A macro for setting GA register.
;; Uses BC

;; Params:
;; reg = register number
;; val = register value

  macro set_ga_reg, reg, val
  ld bc,GATE_ARRAY|\reg|\val
  out (c),c
  endm


;;==============================================================================;
;; set_border
;; A macro for selecting the border color.
;; Uses A, BC

;; Params:
;; color = border color (hardware color)

  macro set_border, color
  ld bc,$7f10             ; #7fxx GATE ARRAY IO port
  ld a,\color
  out (c),c               ; Select PEN
  out (c),a               ; Set color for selected pen
  endm
