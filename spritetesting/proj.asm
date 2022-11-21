.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
; these are exported and referenced in reset.asm 
; so they can be initialized, I could take them out to make this simpler but
; they act as a demo of reset level initialization I guess
player_x: .res 1
player_y: .res 1
addrlo: .res 1
addrhi: .res 1
.exportzp player_x, player_y 

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler ; this is the frame rate based loop, it fires ~60 times per second
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

; can do logic stuff here
  JSR update_player
  JSR draw_player

  LDA #$00 ; this is for background scrolling
  STA $2005 ; x-dir set to 0
  STA $2005 ; y-dir set to 0

  jsr famistudio_update ; this keeps the music playing from famistudio

  RTI
.endproc

.import reset_handler
.import famistudio_init ; these are your remote control funcs from famistudio
.import famistudio_music_play
.import famistudio_music_pause
.import famistudio_music_stop
.import famistudio_update

.export main
.proc main
  ; write a palette
  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX #$3f
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDX #$00
  STX PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
load_palettes:
  LDA palettes,X ; with the X or Y registers you CAN use them as indexes at addresses, like addr + i
  STA PPUDATA ; Now every time we write to the PPU the target address autoincrements so the next write will be the next spot
  INX ; this is walking from X = 0 up to X = 31
  CPX #$20 ; later on you'll see 32 bytes of palette data
  BNE load_palettes ; jump back until you hit 32

  ; write background nametables
  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX #$20
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDY #$00
  STY PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
  LDX #>bgnam
  STX addrhi
  LDX #<bgnam
  STX addrlo
  LDX #$00
load_bg:
  LDA (addrlo),Y; with the X or Y registers you CAN use them as indexes at addresses, like addr + i
  STA PPUDATA ; Now every time we write to the PPU the target address autoincrements so the next write will be the next spot
  INY ; this is walking from X = 0 up to X = 255
  CPY #$00 ; later on you'll see 256 bytes of palette data
  BNE load_bg ; jump back until you hit 256
  INC addrhi
  INX
  LDY #$00
  CPX #$04 ; later on you'll see 256 bytes of palette data
  BNE load_bg

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  ldx #.lobyte(music_data_songtitle) ; FIXME this symbol is from famistudio based on the name of your track
  ldy #.hibyte(music_data_songtitle) ; the address is passed to the init in two bytes 
  lda #1 ; NTSC
  jsr famistudio_init ; prep the Audio Processing Unit
  lda #0
  jsr famistudio_music_play ; play track 0 (or whatever number is in A register)

forever: ; all games need an infinite loop
  jsr gamepad_poll ; maybe I should only do this polling once per NMI, this probably is overkill
  ; I respond to gamepad state in the update_player routine
  lda gamepad
  JMP forever
.endproc

.proc draw_player
  ; save registers ; probably too nice of a subroutine I could disregard saving registers and such
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ; I commented out everything here, but left in an example of a 32 x 32 metatile 
  ; which uses player_x, player_y to update the $SPRITES segment which draws to the PPU
  ; write the tile numbers for our sprite/metatile
  ;I commented this out rather than delete, this was writing into my sprites segment
  ; it was setting the tile from "tiles.chr" which was an export from NES Lightbox
  ; In this case I'm using 4 tiles to make a 32 x 32 meta tile (I think that's the right vocab)
  ; In my case tile $10 (16) was the bottom left, $11 the bottom right, then top left top right
  ;LDA #$10
  ;STA $0201
  ;LDA #$11
  ;STA $0205
  ;LDA #$12
  ;STA $0209
  ;LDA #$13
  ;STA $020d

  ; write tile attributes
  ; uses palette 0 for the 4 tiles
  ;LDA #$00
  ;STA $0202
  ;STA $0206
  ;STA $020a
  ;STA $020e

  ; store tile locations
  ; top left tile:
  ;LDA player_y ; sets the metatile top left corner at player_y
  ;STA $0200
  ;LDA player_x ; and player_x
  ;STA $0203

  ; top right tile (x + 8):
  ;LDA player_y
  ;STA $0204
  ;LDA player_x
  ;CLC
  ;ADC #$08
  ;STA $0207

  ; bottom left tile (y + 8):
  ;LDA player_y
  ;CLC
  ;ADC #$08
  ;STA $0208
  ;LDA player_x
  ;STA $020b

  ; bottom right tile (x + 8, y + 8)
  ;LDA player_y
  ;CLC
  ;ADC #$08
  ;STA $020c
  ;LDA player_x
  ;CLC
  ;ADC #$08
  ;STA $020f

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
  PHP ; Again this is probably overkill for our subroutine
  PHA ; It's just a rock-solid calling convention
  TXA ; it saves all of the register data on the stack for the reset after our code
  PHA
  TYA
  PHA

  lda gamepad
  and #PAD_A
  beq :+
    ;jsr a_pressed ; subrouting for the a_button being pressed?  sure
  :

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
.include "SONGTITLEHERE.s" ; export from Famistudio (click export songs, side tab: famistudio music code, then select FORMAT dropdown: ca65)

.segment "VECTORS" ; these are really important don't mess with them
.addr nmi_handler, reset_handler, irq_handler ; they handle each frame, resets, and a sound interrupt

.segment "RODATA"
palettes: ; hybrid exporting these from NES lightbox and half manual
.incbin "bg.pal"
.byte $0d, $16, $16, $30, $0d, $32, $16, $30, $0d, $12, $12, $30, $0d, $32, $12, $30
bgnam:
.incbin "globalbg.nam"

.segment "CHR"
.incbin "TILESETHERE.chr" ; NES lightbox select Tilesets "export CHR as"
