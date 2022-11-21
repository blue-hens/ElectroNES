.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
player_x: .res 1
player_y: .res 1
player_x_dir: .res 1
player_y_dir: .res 1
buttons1: .res 1
buttons2: .res 1
right_wall_hit: .res 1
left_wall_hit: .res 1
top_wall_hit: .res 1
bottom_wall_hit: .res 1
top_left_done: .res 1
top_right_done: .res 1
bottom_left_done: .res 1
bottom_right_done: .res 1
loopcnt: .res 1
dvdcolor: .res 1
tmpvar: .res 1
.exportzp player_x, player_y

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  JSR update_player
  JSR draw_player

  LDA #$00
  STA $2005
  STA $2005

  INC loopcnt
  LDA loopcnt
  BNE finishit

  LDA #$00
  STA loopcnt
  LDA dvdcolor
  LSR
  CLC
	ADC #$01
  AND #$01
  ASL
  STA dvdcolor

  finishit:
  jsr famistudio_update

  RTI
.endproc

.import reset_handler
.import famistudio_init
.import famistudio_music_play
.import famistudio_music_pause
.import famistudio_music_stop
.import famistudio_update

.export main
.proc main
  ; write a palette
  LDX PPUSTATUS ; remote button: incoming address
  LDX #$3f
  STX PPUADDR ; remote input high byte of address
  LDX #$00
  STX PPUADDR ; remote input low byte of address
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

  LDX #$00
  STX top_left_done
  STX top_right_done
  STX bottom_left_done
  STX bottom_right_done
  STX loopcnt
  STX dvdcolor

  ; write nametables
  ; from 2000 to 201f write tile $02
  ; from 23a0 to 23bf write tile $02
  LDA PPUSTATUS ; top left
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDY #$00
row1bg:
  LDX #$02
  STX PPUDATA
  INY
  CPY #$20
  BNE row1bg

  LDA PPUSTATUS ; top left
  LDA #$23
  STA PPUADDR
  LDA #$a0
  STA PPUADDR
  LDY #$00
row2bg:
  LDX #$02
  STX PPUDATA
  INY
  CPY #$20
  BNE row2bg

  LDA PPUSTATUS ; top left
  LDA #$20
  STA PPUADDR
  LDA #$20
  STA PPUADDR
  LDX #$2f
  STX PPUDATA

  LDA PPUSTATUS ; top right
  LDA #$20
  STA PPUADDR
  LDA #$3f
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS ; bottom left
  LDA #$23
  STA PPUADDR
  LDA #$80
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS ; bottom right
  LDA #$23
  STA PPUADDR
  LDA #$9f
  STA PPUADDR
  STX PPUDATA

  ; big stars first
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$6b
  STA PPUADDR
  LDX #$2d
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$57
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$23
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$23
  STA PPUADDR
  LDA #$52
  STA PPUADDR
  STX PPUDATA

  ; next, small star 1
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$74
  STA PPUADDR
  LDX #$2d
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$43
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$5d
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$73
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$2f
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$f7
  STA PPUADDR
  STX PPUDATA

  ; finally, small star 2
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$f1
  STA PPUADDR
  LDX #$2d
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$a8
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$7a
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$23
  STA PPUADDR
  LDA #$44
  STA PPUADDR
  STX PPUDATA

  LDA PPUSTATUS
  LDA #$23
  STA PPUADDR
  LDA #$7c
  STA PPUADDR
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$23
  STA PPUADDR
  LDA #$c2
  STA PPUADDR
  LDA #%01000000
  STA PPUDATA

  LDA PPUSTATUS
  LDA #$23
  STA PPUADDR
  LDA #$e0
  STA PPUADDR
  LDA #%00001100
  STA PPUDATA

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  ldx #.lobyte(music_data_untitled)
  ldy #.hibyte(music_data_untitled)
  lda #1 ; NTSC
  jsr famistudio_init
  lda #0
  jsr famistudio_music_play

forever:
  jsr gamepad_poll
  ; respond to gamepad state
  lda gamepad
  JMP forever
.endproc

.proc draw_player
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ; write DVD tile numbers
  LDA #$10
  STA $0201
  LDA #$11
  STA $0205
  LDA #$12
  STA $0209
  LDA #$13
  STA $020d

  ; write tile attributes
  ; use palette 0
  LDA dvdcolor ; 0 or 2
  STA $0202
  STA $0206
  STA $020a
  STA $020e ;1,3,5,7

  LDA player_x_dir
  ASL
  STA tmpvar ; xdir is 0 or 1 this will put 0 or 2 into tmpvar
  LDA #$01
  CLC
  ADC tmpvar
  TAX ; puts 1 + (x << 1) into X reg

  LDA player_y_dir
  ASL
  ASL
  STA tmpvar ; ydir is 0 or 1 this will put 0 or 4 into tmpvar
  TXA ; A now has 1 + (x << 1)
  CLC
  ADC tmpvar ; A now has 1 + (x << 1) + (y << 2)
  ASL ; A now has 2,6,10, or 14
  STA tmpvar ; tmpvar has the offset to the corner we want to use the other palette

  LDA dvdcolor
  CLC
  ADC #$01 ; reg A is now 1 or 3 ; tmpvar is the offset
  LDX tmpvar
  STA $0200,X

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc update_player
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  lda gamepad
  and #PAD_A
  beq :+
    lda #$70
    sta player_x
  :

  LDX #$00
  STX right_wall_hit
  STX left_wall_hit
  stx top_wall_hit
  stx bottom_wall_hit

  LDY #$01

  LDA player_x
  CMP #$f0
  BCC not_at_right_edge
  ; if BCC is not taken, we are greater than $f0
  LDA #$00
  STY right_wall_hit
  STA player_x_dir    ; start moving left
  JMP direction_set ; we already chose a direction,
                    ; so we can skip the left side check
not_at_right_edge:
  LDA player_x
  CMP #$01
  BCS direction_set
  ; if BCS not taken, we are less than $10
  LDA #$01
  STA player_x_dir   ; start moving right
  STY left_wall_hit
direction_set:
  ; now, actually update player_x
  LDA player_x_dir
  CMP #$01
  BEQ move_right
  ; if player_dir minus $01 is not zero,
  ; that means player_dir was $00 and
  ; we need to move left
  DEC player_x
  JMP exit_subroutine
move_right:
  INC player_x
exit_subroutine:

LDA player_y
CMP #$d8
BCC not_at_bottom
; if BCC is not taken, we are greater than $d0
LDA #$00
STA player_y_dir    ; start moving up
STY bottom_wall_hit
JMP direction_y_set ; we already chose a direction,
                  ; so we can skip the left side check
not_at_bottom:
LDA player_y
CMP #$08
BCS direction_y_set
; if BCS not taken, we are less than $10
LDA #$01
STY top_wall_hit
STA player_y_dir   ; start moving down
direction_y_set:
; now, actually update player_x
LDA player_y_dir
CMP #$01
BEQ move_down
; if player_dir minus $01 is not zero,
; that means player_dir was $00 and
; we need to move up
DEC player_y
JMP exit_subroutine_y
move_down:
INC player_y
exit_subroutine_y:

  LDA top_wall_hit
  CMP #$00
  BEQ not_top
  ; yes top
  LDA left_wall_hit
  CMP #$00
  BEQ top_not_left
  ; left and top

  LDA #$01
  STA top_left_done
  LDA PPUSTATUS ; top left
  LDA #$20
  STA PPUADDR
  LDA #$20
  STA PPUADDR
  LDX #$2e
  STX PPUDATA


  top_not_left:
  LDA right_wall_hit
  CMP #$00
  BEQ not_top
  ;right and top
  LDA #$01
  STA top_right_done

  LDA PPUSTATUS ; top right
  LDA #$20
  STA PPUADDR
  LDA #$3f
  STA PPUADDR
  LDX #$2e
  STX PPUDATA

  not_top:

  LDA bottom_wall_hit
  CMP #$00
  BEQ not_bottom
  ; yes top
  LDA left_wall_hit
  CMP #$00
  BEQ bottom_not_left
  ; left and top

  LDA #$01
  STA bottom_left_done
  LDA PPUSTATUS ; bottom left
  LDA #$23
  STA PPUADDR
  LDA #$80
  STA PPUADDR
  LDX #$2e
  STX PPUDATA

  bottom_not_left:
  LDA right_wall_hit
  CMP #$00
  BEQ not_bottom
  ;right and top
  LDA #$01
  STA bottom_right_done
  LDA PPUSTATUS ; bottom right
  LDA #$23
  STA PPUADDR
  LDA #$9f
  STA PPUADDR
  LDX #$2e
  STX PPUDATA


  not_bottom:

  ; all done, clean up and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80


.segment "ZEROPAGE"
gamepad: .res 1

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda $4016
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	rts

.segment "AUDIO"
.include "SW.s"

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
.byte $0d, $1a, $14, $15, $0d, $1a, $14, $15, $0d, $1a, $14, $15, $0d, $1a, $14, $15
.byte $0d, $16, $16, $30, $0d, $32, $16, $30, $0d, $12, $12, $30, $0d, $32, $12, $30

.segment "CHR"
.incbin "tiles.chr"
