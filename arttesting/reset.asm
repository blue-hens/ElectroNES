.include "constants.inc"

.segment "ZEROPAGE"
.importzp topleftcornerlo, topleftcornerhi, meta_tile_0_first_tile_index, meta_tile_1_first_tile_index, meta_tile_2_first_tile_index, levelnum, between_levels, level_interactive, level_animation, level_done

.segment "CODE"
.import main
.export reset_handler
.proc reset_handler
  SEI
  CLD
  LDX #$00
  STX PPUCTRL
  STX PPUMASK

vblankwait:
  BIT PPUSTATUS
  BPL vblankwait

	LDX #$00
	LDA #$ff
clear_oam:
	STA $0200,X ; set sprite y-positions off the screen
	INX
	INX
	INX
	INX
	BNE clear_oam

vblankwait2:
	BIT PPUSTATUS
	BPL vblankwait2

  ; initialize zero-page values
  LDA #$84
  STA topleftcornerlo
  LDA #$20
  STA topleftcornerhi
  LDA #$0a
  STA meta_tile_0_first_tile_index
  LDA #$0b
  STA meta_tile_1_first_tile_index
  LDA #$0c
  STA meta_tile_2_first_tile_index
  LDA #$01
  STA between_levels
  LDA #$00
  STA levelnum
  STA level_interactive
  STA level_animation
  STA level_done

  JMP main
.endproc
