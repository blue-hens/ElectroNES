.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
; these are exported and referenced in reset.asm
; so they can be initialized, I could take them out to make this simpler but
; they act as a demo of reset level initialization I guess
tmp: .res 1
pnum: .res 1
addrlo: .res 1
addrhi: .res 1
topleftcornerlo: .res 1 ; constant don't edit please
topleftcornerhi: .res 1 ; constant don't edit please
meta_tile_0_first_tile_index: .res 1 ; constant actual starting tile for meta tile 0
meta_tile_1_first_tile_index: .res 1 ; constant actual starting tile for meta tile 0
meta_tile_2_first_tile_index: .res 1 ; constant actual starting tile for meta tile 0
levelnum: .res 1
num_moves: .res 1
num_targets: .res 1
num_done: .res 1
num_fails: .res 1
between_levels: .res 1
level_interactive: .res 1
level_animation: .res 1
level_done: .res 1
selected_x: .res 1
selected_y: .res 1
animation_increment: .res 1
animation_slow_loop: .res 1
el_x: .res 1 ; variables for the actively updating electron
el_y: .res 1
el_edge: .res 1
el_used: .res 1
el_class: .res 1
el_success: .res 1
el_dead: .res 1
el_motion: .res 1
el_path_offset: .res 1
el_path_index: .res 1
el_backward: .res 1
current_level: .res 36
current_el1: .res 3
current_el2: .res 3
current_el3: .res 3
current_el4: .res 3
current_targets: .res 8
.exportzp topleftcornerlo, topleftcornerhi, meta_tile_0_first_tile_index, meta_tile_1_first_tile_index, meta_tile_2_first_tile_index, levelnum, between_levels, level_interactive, level_animation, level_done

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler ; this is the frame rate based loop, it fires ~60 times per second
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  ; goal if between_levels and gamepad start then loadlevel
  lda between_levels
  beq not_loading
    lda gamepad
    and #PAD_START
    beq not_loading
      jsr loadlevel ; NEED LOAD LEVEL
  not_loading:

  lda level_interactive
  beq not_interactive
    lda gamepad
    and #PAD_START
    beq not_start_animation
      jsr start_animation_mode ; NEED START ANIMATION function
      jmp not_interactive
    not_start_animation:
      jsr interactive_loop ; NEED INTERACTIVE LOOP
  not_interactive:

  lda level_animation
  beq not_animation
    jsr animation_loop ; NEED ANIMATION LOOP
  not_animation:

  lda level_done
  beq leave_loop
     lda num_fails
     beq beat_level
         jsr loselevel ; need loselevel
         jmp leave_loop
     beat_level:
         jsr winlevel ; need winlevel
  leave_loop:

  jsr famistudio_update ; this keeps the music playing from famistudio

  LDA #$00 ; this is for background scrolling
  STA $2005 ; x-dir set to 0
  STA $2005 ; y-dir set to 0

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

  ldx #.lobyte(music_data_untitled) ; FIXME this symbol is from famistudio based on the name of your track
  ldy #.hibyte(music_data_untitled) ; the address is passed to the init in two bytes
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

.proc draw_meta_tile ; tile num in REG A, board X in reg X, board Y in reg Y
  LDA #$0a
  STA meta_tile_0_first_tile_index
  LDA #$0b
  STA meta_tile_1_first_tile_index
  LDA #$0c
  STA meta_tile_2_first_tile_index
  LDA #$20
  STA topleftcornerhi
  LDA #$84
  STA topleftcornerlo
  ; use X and Y register to get addrhi and addrlo to be the top left thing here
  ; get pnum to be the right starting point of the pal tiles

  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX addrhi
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDX addrlo
  STX PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
  LDX pnum
  STX PPUDATA ; that drew the top left tile

  INX
  STX PPUDATA ; that incremented the A register and stored it at the NEXT bg addr

  ;do that 16ish times

.endproc

.proc draw_bg_tiles
LDA #$0a ; metatile0
LDX #$00 ; board X is 0
LDY #$00 ; board Y is 0
JSR draw_meta_tile
.endproc

.proc loadlevel
	rts
.endproc

.proc start_animation_mode
	rts
.endproc

.proc interactive_loop
	rts
.endproc

.proc animation_loop
	rts
.endproc

.proc loselevel
	rts
.endproc

.proc winlevel
	rts
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
levels:
.incbin "levels.dat"

.segment "CHR"
.incbin "TILESETHERE.chr" ; NES lightbox select Tilesets "export CHR as"
