.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
; these are exported and referenced in reset.asm
; so they can be initialized, I could take them out to make this simpler but
; they act as a demo of reset level initialization I guess
tmp: .res 1
tmp2: .res 1
pnum: .res 1
addrlo: .res 1
addrhi: .res 1
idxhelper: .res 1
topleftcornerlo: .res 1 ; constant don't edit please
topleftcornerhi: .res 1 ; constant don't edit please
meta_tile_0_first_tile_index: .res 1 ; constant actual starting tile for meta tile 0
meta_tile_1_first_tile_index: .res 1 ; constant actual starting tile for meta tile 0
meta_tile_2_first_tile_index: .res 1 ; constant actual starting tile for meta tile 0
levelnum: .res 1
num_moves_max: .res 1
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
buttons_allowed: .res 1
el_x: .res 1 ; variables for the actively updating electron
el_y: .res 1
el_edge: .res 1
el_tile: .res 1 ; the tile type this electron is on
el_not_used: .res 1 ; display at all? might never use this feature, but level designers choice?
el_class: .res 1 ; target type 0,1,2,3
el_success: .res 1 ; has succeeded?
el_dead: .res 1 ; has finished traversing? (at blank or off board)
el_motion: .res 1 ; is actively animating (presumed valid tile type and path)
el_path_offset: .res 1 ; might globalize this but 0 to 15
el_path_index: .res 1 ; which path type are you? tile type 0, edge 0 is 0 type 1, edge 3 is (4+3)==7
original_level: .res 36
current_level: .res 36
current_el0: .res 3 ; storage for the electrons for pickling their state and such
current_el1: .res 3 ; the three bytes are ANIMATION BYTE, POSITION BYTE, STATE BYTE
current_el2: .res 3
current_el3: .res 3
current_targets: .res 12 ; these are const within level and are for drawing/detecting sinks
orig_el0: .res 3 ; the originals are for drawing sources
orig_el1: .res 3
orig_el2: .res 3
orig_el3: .res 3
total_levels: .res 1
.exportzp topleftcornerlo, topleftcornerhi, meta_tile_0_first_tile_index, meta_tile_1_first_tile_index, meta_tile_2_first_tile_index, levelnum, between_levels, level_interactive, level_animation, level_done, total_levels

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler ; this is the frame rate based loop, it fires ~60 times per second
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  INC animation_slow_loop
  LDA animation_slow_loop
  CMP #$08
  BMI :+
    LDA #$01
    STA buttons_allowed
    STA animation_increment
  :
  jsr gamepad_poll ; maybe I should only do this polling once per NMI, this probably is overkill
  ; I respond to gamepad state in the update_player routine
  lda gamepad

  ; goal if between_levels and gamepad start then loadlevel
  lda between_levels
  beq not_loading
    lda gamepad
    and #PAD_START
    beq not_loading
      lda levelnum
      cmp total_levels
      beq leave_loop
      lda #$00
      sta between_levels
      jsr loadlevel ; NEED LOAD LEVEL
      jmp leave_loop
  not_loading:

  lda level_interactive
  beq not_interactive
    lda gamepad
    and #PAD_START
    beq not_start_animation
      lda num_moves_max
      cmp num_moves
      bmi not_start_animation
      lda num_moves
      cmp #$00
      beq not_start_animation
        jsr start_animation_mode
        jmp not_interactive
    not_start_animation:
      jsr interactive_loop
  not_interactive:

  lda level_animation
  beq not_animation
    jsr animation_loop
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


  JSR draw_loading_screen

forever: ; all games need an infinite loop
  JMP forever
.endproc

.proc draw_loading_screen
  LDX #>bgnam
  LDY #<bgnam
  jsr draw_any_screen
  RTS
.endproc

.proc draw_good_screen
  LDX #>goodnam
  LDY #<goodnam
  jsr draw_any_screen
  RTS
.endproc

.proc draw_bad_screen
  LDX #>badnam
  LDY #<badnam
  jsr draw_any_screen
  RTS
.endproc

.proc draw_win_screen
  LDX #>winnam
  LDY #<winnam
  jsr draw_any_screen
  RTS
.endproc

.proc draw_blank_screen
  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX #$20
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDY #$00
  STY PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
  LDX #$00
  LDA #$d0
  loadingloop:
    STA PPUDATA ; Now every time we write to the PPU the target address autoincrements so the next write will be the next spot
    INY ; this is walking from X = 0 up to X = 255
    CPY #$00 ; later on you'll see 256 bytes of palette data
    BNE loadingloop ; jump back until you hit 256
    INX
    LDY #$00
    CPX #$03 ; later on you'll see 256 bytes of palette data
    BNE loadingloop

  smloop:
    STA PPUDATA ; Now every time we write to the PPU the target address autoincrements so the next write will be the next spot
    INY ; this is walking from X = 0 up to X = 255
    CPY #$c0 ; later on you'll see 256 bytes of palette data
    BNE smloop

  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX #$23
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDY #$c0
  STY PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
  LDY #$00
  LDA #%10101010 ;this sets all the BG attributes to 2 (palette 3)
  smloop2:
    STA PPUDATA ; Now every time we write to the PPU the target address autoincrements so the next write will be the next spot
    INY ; this is walking from X = 0 up to X = 255
    CPY #$40 ; later on you'll see 256 bytes of palette data
    BNE smloop2

  RTS
.endproc

.proc draw_any_screen
  ; use X as hi addr and Y as low addr
  ;LDX #>bgnam
  ;LDY #<bgnam
  ; write background nametables
  lda #$00
  sta $2000
  sta $2001 ; turn off writing for a bit

  STX addrhi
  STY addrlo
  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX #$20
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDY #$00
  STY PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data

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

    LDX #$00
  	LDA #$ff
  clear_oam:
  	STA $0200,X ; set sprite y-positions off the screen
  	INX
  	INX
  	INX
  	INX
  	BNE clear_oam

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc game_2_screen ; this thing consumes X and Y in registers and sets addrhi and addrlo to the place to draw next
  STX tmp
  LDA topleftcornerhi
  STA addrhi
  LDA #$ff
  CMP tmp
  BNE next_part
    LDA #$83
    STA addrlo
    jmp x_all_done
  next_part:
  LDA topleftcornerlo
  CLC
  ADC tmp
  ADC tmp
  ADC tmp
  ADC tmp
  STA addrlo ; should add X * 4 to addrlo and X is 0 to 5 so at most 20
  ;that should handle the right_shift
  x_all_done:

  STY tmp
  LDA tmp
  and #$1
  beq is_even ; OK so if Y is odd then subtract 80 from addrlo and add 1 to addrhi
    LDA #$80
    STA tmp2
    LDA addrlo
    CLC
    SEC
    SBC tmp2
    STA addrlo
  is_even:
  LDA tmp
  CMP #0
  beq is_zero
    INC addrhi
    DEY ; if y > 0 then add at least 1 to addrhi ((Y-1) >> 1) + 1 + addrhi
    TYA
    LSR
    TAY
    ;INY
    TYA
    CLC
    ADC addrhi
    STA addrhi
  is_zero:
  LDY tmp

  STY tmp
  LDA #$ff
  CMP tmp
  BNE next_part2
    LDA topleftcornerhi
    STA addrhi
    STX tmp
    LDA topleftcornerlo
    CLC
    ADC tmp
    ADC tmp
    ADC tmp
    ADC tmp
    CLC
    SEC
    SBC #$20
    STA addrlo
  next_part2:
  RTS
.endproc

.proc draw_meta_tile ; tile type in REG A, board X in reg X, board Y in reg Y
  ; use X and Y register to get addrhi and addrlo to be the top left thing here
  ; get pnum to be the right starting point of the pal tiles
  STA tmp
  CMP #3
  BNE not_blank_meta
      LDA #$d0
      STA pnum
      LDA tmp
      jmp done_setting_pnum
  not_blank_meta:

  CMP #2
  BNE not_meta_2
      LDA meta_tile_2_first_tile_index
      STA pnum
      LDA tmp
      jmp done_setting_pnum
  not_meta_2:

  CMP #1
  BNE not_meta_1
      LDA meta_tile_1_first_tile_index
      STA pnum
      LDA tmp
      jmp done_setting_pnum
  not_meta_1:

  CMP #0
  BNE not_meta_0
      LDA meta_tile_0_first_tile_index
      STA pnum
      LDA tmp
      jmp done_setting_pnum
  not_meta_0:

  done_setting_pnum:

  JSR game_2_screen ; sets addrhi addrlo

  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX addrhi
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDX addrlo
  STX PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data

  LDX pnum
  STX PPUDATA ; that drew the top left tile
  INX
  STX PPUDATA ; that incremented the A register and stored it at the NEXT bg addr
  INX
  STX PPUDATA
  INX
  STX PPUDATA

  INX
  LDY PPUSTATUS
  LDA addrhi
  STA PPUADDR
  LDA addrlo
  CLC
  ADC #$20
  STA tmp
  STA PPUADDR
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  STX PPUDATA

  INX
  LDY PPUSTATUS
  LDA addrhi
  STA PPUADDR
  LDA tmp
  CLC
  ADC #$20
  STA tmp
  STA PPUADDR
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  STX PPUDATA

  INX
  LDY PPUSTATUS
  LDA addrhi
  STA PPUADDR
  LDA tmp
  CLC
  ADC #$20
  STA PPUADDR
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  STX PPUDATA
  INX
  RTS
.endproc

.proc draw_bg_sprite
  ; OK, so reg X is game X, rg Y is game Y, reg A is the first of 4 possible tiles
  ; addrhi,addrlo is the 2-byte location near the entry point
  ; we have to decide if it's a horizontal edge or vertical edge
  STA pnum
  CPX #$06 ;X is 0 to 5 and we're subtracting 6 this will be negative in the normal cases
  BMI y_off_case
    x_off_case:
    LDA #$20
    STA tmp
    LDA addrlo
    CLC
    ADC tmp
    STA addrlo
    LDA pnum

    LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
    LDX addrhi
    STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
    LDX addrlo
    STX PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
    STA PPUDATA
    INC pnum

    LDA #$20
    STA tmp
    LDA addrlo
    CLC
    ADC tmp
    STA addrlo
    LDA pnum
    LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
    LDX addrhi
    STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
    LDX addrlo
    STX PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
    STA PPUDATA

    jmp leave_proc
  y_off_case:
    CPX #$FF
    BEQ x_off_case
    inc addrlo
    inc pnum
    inc pnum
    LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
    LDX addrhi
    STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
    LDX addrlo
    STX PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
    LDA pnum
    STA PPUDATA
    inc pnum
    LDA pnum
    STA PPUDATA
  leave_proc:
  RTS
.endproc

.proc load_electron_position_edge ;presume A has the position byte data
  STA tmp
  AND #$03
  STA el_edge
  LDA tmp
  LSR A
  LSR A
  STA tmp
  AND #$07
  STA el_y
  LDA tmp
  LSR A
  LSR A
  LSR A
  STA el_x
  RTS
.endproc

.proc pickle_electron_position_edge ;when we're done A should have the animation byte
  LDA el_x
  ASL A
  ASL A
  ASL A
  ORA el_y
  ASL A
  ASL A
  ORA el_edge
  RTS
.endproc

.proc load_electron_animation_data ;presume A has the animation byte data
  STA tmp
  AND #$0f
  STA el_path_index
  LDA tmp
  LSR A
  LSR A
  LSR A
  LSR A
  AND #$0f
  STA el_path_offset
  RTS
.endproc

.proc pickle_electron_animation_data ;when we're done A should have the animation byte
  LDA el_path_offset
  ASL A
  ASL A
  ASL A
  ASL A
  ORA el_path_index
  RTS
.endproc

.proc load_electron_state_data ;presume A has the state byte data
  STA tmp
  AND #$01
  STA el_motion

  LDA tmp
  LSR A
  STA tmp
  AND #$01
  STA el_dead

  LDA tmp
  LSR A
  STA tmp
  AND #$01
  STA el_success

  LDA tmp
  LSR A
  STA tmp
  AND #$03
  STA el_class

  LDA tmp
  LSR A
  LSR A
  LSR A
  LSR A
  STA tmp
  AND #$01
  STA el_not_used
  RTS
.endproc

.proc pickle_electron_state_data ;when we're done A should have the animation byte
  LDA el_not_used
  ASL A
  ASL A
  ASL A
  ASL A
  ORA el_class
  ASL A
  ORA el_success
  ASL A
  ORA el_dead
  ASL A
  ORA el_motion
  RTS
.endproc


.proc adjust_source_electron_by_edge ;presume el_edge, el_x, and el_y are set
  LDA #$00
  CMP el_edge
  BNE :+
    DEC el_y
    jmp theend
  :
  LDA #$01
  CMP el_edge
  BNE :+
    INC el_x
    jmp theend
  :
  LDA #$02
  CMP el_edge
  BNE :+
    INC el_y
    jmp theend
  :
  LDA #$03
  CMP el_edge
  BNE :+
    DEC el_x
    jmp theend
  :
  theend:
  rts
.endproc

.proc draw_targets_and_electrons
  LDX #$00
  LDA current_targets,X
  STA tmp
  INX
  LDA current_targets,X
  STA tmp2
  INX
  LDA current_targets,X
  BEQ next_target
    LDX tmp
    LDY tmp2
    JSR game_2_screen
    LDA #$b0
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo

    LDX #$01
    LDA orig_el0,X
    jsr load_electron_position_edge
    jsr adjust_source_electron_by_edge
    LDX el_x
    LDY el_y
    JSR game_2_screen
    LDA #$c0
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo

  next_target:

  LDX #$03
  LDA current_targets,X
  STA tmp
  INX
  LDA current_targets,X
  STA tmp2
  INX
  LDA current_targets,X
  BEQ next_target2
    LDX tmp
    LDY tmp2
    JSR game_2_screen
    LDA #$b4
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo

    LDX #$01
    LDA orig_el1,X
    jsr load_electron_position_edge
    jsr adjust_source_electron_by_edge
    LDX el_x
    LDY el_y
    JSR game_2_screen
    LDA #$c4
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo
  next_target2:

  LDX #$06
  LDA current_targets,X
  STA tmp
  INX
  LDA current_targets,X
  STA tmp2
  INX
  LDA current_targets,X
  BEQ next_target3
    LDX tmp
    LDY tmp2
    JSR game_2_screen
    LDA #$b8
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo

    LDX #$01
    LDA orig_el2,X
    jsr load_electron_position_edge
    jsr adjust_source_electron_by_edge
    LDX el_x
    LDY el_y
    JSR game_2_screen
    LDA #$c8
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo
  next_target3:

  LDX #$09
  LDA current_targets,X
  STA tmp
  INX
  LDA current_targets,X
  STA tmp2
  INX
  LDA current_targets,X
  BEQ next_target4
    LDX tmp
    LDY tmp2
    JSR game_2_screen
    LDA #$bc
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo

    LDX #$01
    LDA orig_el3,X
    jsr load_electron_position_edge
    jsr adjust_source_electron_by_edge
    LDX el_x
    LDY el_y
    JSR game_2_screen
    LDA #$cc
    JSR draw_bg_sprite ; REG X, REG Y, REG A, addrhi, addrlo
  next_target4:



  RTS
.endproc

.proc draw_current_board
  lda #$00
  sta $2000
  sta $2001 ; turn off writing for a bit

  jsr draw_blank_screen

  LDX #$00 ; board X is 0 ; OK I'm going to use idxhelper as the 0 to 35 counter
  LDY #$00
  STX num_moves
  STX tmp
  STX idxhelper
  STY tmp2
  meta_tile_loop:
     LDA idxhelper
     LDY #$ff ; -1
     y_sub_loop:
         INY
         CLC
         SEC
         SBC #6
         BPL y_sub_loop ; if subtracting 6 was too much then we're done here
     ADC #6
     STA tmp ; OK I THINK that tmp is now idxhelper % 6 and Y is idxhelper / 6
     LDX idxhelper
     LDA current_level,X ; this should load the tile type into A (possibly 0,1,2,3)
     STA tmp2
     LDA original_level,X
     CMP tmp2
     BEQ skip_move_inc
         INC num_moves
     skip_move_inc:
     LDA tmp2
     LDX tmp
     ; we did it (I hope), A is tile type, X is game_x and Y is game_y
     jsr draw_meta_tile

     LDX idxhelper
     INX
     STX idxhelper
     CPX #$24
     BNE meta_tile_loop

  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX #$20
  STX PPUADDR ; First byte written to PPUADDR is the "high byte" of where you'll be dropping data
  LDY #$24
  STY PPUADDR ; Next byte written to PPUADDR is the "low byte" of where you'll be dropping data
  LDA #$15
  STA PPUDATA
  LDA #$0e
  STA PPUDATA
  LDA #$1f
  STA PPUDATA
  LDA #$0e
  STA PPUDATA
  LDA #$15
  STA PPUDATA ; LEVEL
  LDA #$d0
  STA PPUDATA ; blank


  LDA levelnum
  STA tmp
  INC tmp
  jsr display_two_digits

  LDA #$d0
  STA PPUDATA ; blank
  LDA #$d0
  STA PPUDATA ; blank
  LDA #$d0
  STA PPUDATA ; blank
  LDA #$d0
  STA PPUDATA ; blank
  LDA #$d0
  STA PPUDATA ; blank

  LDA #$16
  STA PPUDATA
  LDA #$18
  STA PPUDATA
  LDA #$1f
  STA PPUDATA
  LDA #$0e
  STA PPUDATA
  LDA #$1c
  STA PPUDATA ; MOVES
  LDA #$d0
  STA PPUDATA ; blank

  LDA num_moves
  STA tmp
  jsr display_two_digits

  LDA #$a2
  STA PPUDATA

  LDA num_moves_max
  STA tmp
  jsr display_two_digits

  jsr draw_targets_and_electrons

  ; highlight the selected tile
  LDY #$23
  STY addrhi
  LDX #$c9
  STX addrlo
  TXA
  CLC
  ADC selected_x
  STA addrlo

  LDA selected_y
  ASL A
  ASL A
  ASL A
  CLC
  ADC addrlo
  STA addrlo

  LDX PPUSTATUS ; you read the PPUSTATUS to let it know you're about to send a two byte address
  LDX addrhi
  STX PPUADDR
  LDX addrlo
  STX PPUADDR
  LDA #%11111111 ; sets palette to 3
  STA PPUDATA


  vblankwait:       ; wait for another vblank before continuing
    BIT PPUSTATUS
    BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc display_two_digits
  LDA #$09
  LDX #$00
  CMP tmp
  BPL single_digit
    go_again:
    INX
    LDA tmp
    CLC
    SEC
    SBC #$0a
    STA tmp
    LDA #$09
    CMP tmp
    BMI go_again
  single_digit:
  LDA tmp
  STX PPUDATA
  STA PPUDATA
  RTS
.endproc

.proc multiplynums
  ; multiplying tmp and tmp2
  LDA #0
  LDX  #$8
  LSR  tmp
  mulloop:
  BCC  no_add
  CLC
  ADC  tmp2
  no_add:
  ROR
  ROR  tmp
  DEX
  BNE  mulloop
  STA  tmp2
  ; done, high result in tmp2, low result in tmp
  RTS
.endproc

.proc loadlevel
  LDX #>levels
  LDY #<levels
  STX addrhi
  STY addrlo
  LDA levelnum
  STA tmp
  LDA #$39 ; 57 bytes per level
  STA tmp2
  JSR multiplynums
  LDA addrhi
  CLC
  ADC tmp2
  STA addrhi
  LDA addrlo
  CLC
  ADC tmp
  STA addrlo ; ok if I'm right this should be levels addr + 57*levelnum

  ; OK now we will loop through 36 bytes and
  ; copy the level data into current_level and original_level
  LDY #$00
copyloop:
  LDA (addrlo),Y
  STA current_level,Y ; I hope this writes from static_level to current_level + Y in ZEROPAGE
  STA original_level,Y

  INY ; this is walking from Y = 0 up to Y = 36
  CPY #$24
  BNE copyloop
  ; Y is now 36
  ; next 2 bytes belong to current_el0+1, +2
  LDX #$01
  LDA (addrlo),Y
  STA current_el0,X
  STA orig_el0,X
  INY
  INX
  LDA (addrlo),Y
  STA current_el0,X
  STA orig_el0,X
  INY

  LDX #$01
  LDA (addrlo),Y
  STA current_el1,X
  STA orig_el1,X
  INY
  INX
  LDA (addrlo),Y
  STA current_el1,X
  STA orig_el1,X
  INY

  LDX #$01
  LDA (addrlo),Y
  STA current_el2,X
  STA orig_el2,X
  INY
  INX
  LDA (addrlo),Y
  STA current_el2,X
  STA orig_el2,X
  INY

  LDX #$01
  LDA (addrlo),Y
  STA current_el3,X
  STA orig_el3,X
  INY
  INX
  LDA (addrlo),Y
  STA current_el3,X
  STA orig_el3,X
  INY  ; Y should NOW be 44

  LDX #$00
  copyloop2:
    LDA (addrlo),Y
    STA current_targets,X
    INX
    INY ; this is walking from Y = 44 up to Y = 56
    CPY #$38
    BNE copyloop2
  ; y is now 56 the final byte
  LDA (addrlo),Y
  STA num_moves_max
  ;I think the level data is now loaded into zeropage and ready to be used
  ;now we want to draw the opening tiles
  LDA #$01
  STA level_interactive
  STA buttons_allowed
  LDA #$00
  ;STA level_interactive
  ;STA num_moves num_moves is set in draw_current_board
  STA num_done
  STA num_fails;
  STA between_levels
  STA level_animation
  STA level_done;
  STA selected_x
  STA selected_y

  LDX #$02
  LDA current_targets,X
  LDX #$05
  CLC
  ADC current_targets,X
  LDX #$08
  CLC
  ADC current_targets,X
  LDX #$0b
  CLC
  ADC current_targets,X
  STA num_targets

  JSR draw_current_board

  rts
.endproc

.proc select_up
  LDA selected_y
  CMP #$00
  BEQ :+
    DEC selected_y
  :
  rts
.endproc

.proc select_down
  LDA selected_y
  CMP #$05
  BEQ :+
    INC selected_y
  :
  rts
.endproc

.proc select_left
  LDA selected_x
  CMP #$00
  BEQ :+
    DEC selected_x
  :
  rts
.endproc

.proc select_right
  LDA selected_x
  CMP #$05
  BEQ :+
    INC selected_x
  :
  rts
.endproc

.proc selected_to_offset ; sets tmp to 0 to 35
  LDA selected_x
  CLC
  ADC selected_y
  ADC selected_y
  ADC selected_y
  ADC selected_y
  ADC selected_y
  ADC selected_y
  STA tmp
  RTS
.endproc

.proc cycle_selected_up
  JSR selected_to_offset
  LDX tmp
  LDA current_level,X
  STA tmp
  INC tmp
  LDA tmp
  CMP #$03
  BNE :+
      LDA #$00
      STA tmp
  :
  LDA tmp
  CMP #$04
  BNE :+
      LDA #$03
  :
  STA current_level,X
  rts
.endproc

.proc cycle_selected_down
  JSR selected_to_offset
  LDX tmp
  LDA current_level,X
  STA tmp
  DEC tmp
  LDA tmp
  CMP #$02
  BNE :+
      LDA #$03
      STA tmp
  :
  LDA tmp
  CMP #$FF
  BNE :+
      LDA #$02
  :
  STA current_level,X
  rts
.endproc

.proc interactive_loop
  lda buttons_allowed
  CMP #$01
  BNE leaveproc
  lda gamepad
  and #PAD_U
  beq :+
    jsr select_up ; NEED START ANIMATION function
    jmp button_pressed
  :
  lda gamepad
  and #PAD_D
  beq :+
    jsr select_down ; NEED START ANIMATION function
    jmp button_pressed
  :
  lda gamepad
  and #PAD_L
  beq :+
    jsr select_left ; NEED START ANIMATION function
    jmp button_pressed
  :
  lda gamepad
  and #PAD_R
  beq :+
    jsr select_right ; NEED START ANIMATION function
    jmp button_pressed
  :

  ;lda num_moves
  ;cmp num_moves_max
  ;beq leaveproc ; test for max_moves but undoing is needed

  lda gamepad
  and #PAD_A
  beq :+
    jsr cycle_selected_up ; NEED START ANIMATION function
    jmp button_pressed
  :
  lda gamepad
  and #PAD_B
  beq :+
    jsr cycle_selected_down ; NEED START ANIMATION function
    jmp button_pressed
  :
  ; IF button pressed then draw_current_board
  ; maybe also go non interactive while thinking
  ; ELSE don't do that
  ; then go interactive if no button
  leaveproc:
	rts
  button_pressed:
    LDX #$00
    STX buttons_allowed
    STX animation_slow_loop
    jsr draw_current_board
    rts
.endproc

.proc fetch_board_tile ; input is el_x, el_y and output is el_tile
  LDX el_x
  STX tmp
  LDA #$ff
  CMP tmp
  BNE :+
    LDA #$03
    STA el_tile
    jmp all_done ; if X is -1 then you're off board, call it tile 3 and be done
  :
  LDX el_y
  STX tmp
  LDA #$ff
  CMP tmp
  BNE :+
    LDA #$03
    STA el_tile
    jmp all_done ; if Y is -1 then you're off board, call it tile 3 and be done
  :

  LDX el_x
  STX tmp
  LDA tmp
  CMP #$06
  BMI :+
    LDA #$03
    STA el_tile
    jmp all_done ; if X - 6 not negative so you're off board, call it tile 3 and be done
  :

  LDX el_y
  STX tmp
  LDA tmp
  CMP #$06
  BMI :+
    LDA #$03
    STA el_tile
    jmp all_done ; if Y - 6 not negative so you're off board, call it tile 3 and be done
  :

  ;now we know el_x is 0 to 5 and el_y is 0 to 5 so let's get an offset 0 to 35
  LDA el_x
  CLC
  ADC el_y
  ADC el_y
  ADC el_y
  ADC el_y
  ADC el_y
  ADC el_y
  STA tmp
  LDX tmp
  LDA current_level,X
  STA el_tile

  all_done:
  rts
.endproc

.proc load_electron_xy
  ; now we have el_x el_y el_edge let's get the pixel X,Y of that spot then we'll get a delta to add to it
  LDA #$1b
  STA tmp2
  LDA #$1c
  STA tmp
  LDA el_x ;
  ASL A ; 2*x
  ASL A ; 4*x
  ASL A ; 8*x
  ASL A ; 16*x
  ASL A ; 32*x
  CLC
  ADC tmp
  STA tmp ; ok X set from tile offset
  LDA el_y ;
  ASL A ; 2*x
  ASL A ; 4*x
  ASL A ; 8*x
  ASL A ; 16*x
  ASL A ; 32*x
  CLC
  ADC tmp2
  STA tmp2 ; ok Y set from tile offset
  ; OK now tmp,tmp2 is the top left corner of the appropriate tile
  LDA el_edge
  CMP #$00
  BNE :+
    LDA tmp
    CLC
    ADC #$10
    STA tmp
  :

  LDA el_edge
  CMP #$01
  BNE :+
    LDA tmp
    CLC
    ADC #$20
    STA tmp
    LDA tmp2
    CLC
    ADC #$10
    STA tmp2
  :

  LDA el_edge
  CMP #$02
  BNE :+
    LDA tmp
    CLC
    ADC #$10
    STA tmp
    LDA tmp2
    CLC
    ADC #$20
    STA tmp2
  :

  LDA el_edge
  CMP #$03
  BNE :+
    LDA tmp2
    CLC
    ADC #$10
    STA tmp2
  :

  ;OK I think the edges are now honored
  ;NOW I have el_path_index and el_path_offset, tmp and tmp2 are pixelX pixelY things
  ; lookup offset is 16*el_path_index + el_path_offset
  LDA el_path_offset
  STA idxhelper
  LDA el_path_index
  ASL A
  ASL A
  ASL A
  ASL A
  CLC
  ADC el_path_offset
  STA idxhelper ; OK now idxhelper should be the offset in either path
  LDX idxhelper
  LDA epathsx,X ; this is the X delta
  STA pnum ; sorry pnum just a tmp
  LDA tmp
  CLC
  ADC pnum ; adding X delta to tmp
  STA tmp
  LDA epathsy,X ; this is the X delta
  STA pnum ; sorry pnum just a tmp
  LDA tmp2
  CLC
  ADC pnum ; adding X delta to tmp
  STA tmp2

  all_done:
  rts
.endproc

; these utilities use the el_* variables only, the caller must pickle the data
.proc electron_next_tile ; this receives an electron which was presumed on a valid tile
  ; and that has finished traversing
  ; it will update the X,Y, edge without contemplating the viability of the next tile
  ; call this at the end of the animation_loops
  ;so we have el_x, el_y, el_edge
  ;now to look up the stuff... el_tile*16 + el_edge*4 + 0 is deltaX, then deltaY, then edge
  LDA el_tile
  ASL A ; *2
  ASL A ; *4
  ASL A ; *8
  ASL A ; *16
  STA tmp

  LDA el_edge
  ASL A; *2
  ASL A; *2
  STA tmp2

  LDA #$00
  CLC
  ADC tmp
  ADC tmp2
  STA tmp
  LDX tmp
  LDA nextlookup,X ; A now has the deltaX
  CMP #$01
  BNE :+
    INC el_x
  :
  LDA nextlookup,X ; A now has the deltaX
  CMP #$FF
  BNE :+
    DEC el_x
  :
  INX
  LDA nextlookup,X ; A now has the deltaY
  CMP #$01
  BNE :+
    INC el_y
  :
  LDA nextlookup,X ; A now has the deltaY
  CMP #$FF
  BNE :+
    DEC el_y
  :

  INX
  LDA nextlookup,X ; A now has the newEdge
  STA el_edge

  rts
.endproc

.proc update_new_electron ; this receives a starting electron on it's new/first tile
  ; it should set in motion electrons that are not yet at the end of their journey
  ; it should NOT set in motion electrons that are at the end of their journey
  ; if they are at the end of their journey we should only increment success/failure ONCE  (decide_fate)
  LDA el_not_used
  CMP #$01
  BNE :+
    RTS
  :

  jsr fetch_board_tile
  LDA el_tile
  CMP #$03
  BNE :++++
    ; this is a dead electron
    ; if it was already dead don't update global counters
    LDX el_dead
    CPX #$01
    BEQ all_done
    ; ok this is the first death
    LDX #$01
    STX el_dead
    INC num_done
    LDA num_done
    CMP num_targets
    BNE :+
       LDA #$01
       STA level_done
       LDA #$00
       STA level_animation
    :
    LDX #$00
    STX el_motion
    ; detect success
    ; use el_class as target offset
    LDA el_class
    STA tmp ; I need tmp * 3 as the offset
    CLC
    ADC tmp
    ADC tmp
    STA tmp
    LDX tmp
    LDA current_targets,X ; this is the X coordinate of the correct target
    ; little nervous about the 00 - 1 is ff stuff so check carefully
    CMP el_x
    BEQ :+ ; that's good
      INC num_fails
      LDY #$00
      STY el_success
      LDA #$01
      STA level_done
      jmp all_done
    :
    INX
    LDA current_targets,X ; this is the Y coordinate of the correct target
    CMP el_y
    BEQ :+ ; that's good
      INC num_fails
      LDY #$00
      STY el_success
      LDA #$01
      STA level_done
      jmp all_done
    :
    LDY #$01
    STY el_success

    jmp all_done
  :
  LDX #$01
  STX el_motion ;let her roll
  LDX #$00
  STX el_path_offset

  ;now to look up the path... el_tile*16 + el_edge*4 + 3
  LDA el_tile
  ASL A ; *2
  ASL A ; *4
  ASL A ; *8
  ASL A ; *16
  STA tmp

  LDA el_edge
  ASL A; *2
  ASL A; *2
  STA tmp2

  LDA #$03
  CLC
  ADC tmp
  ADC tmp2
  STA tmp
  LDX tmp
  LDA nextlookup,X
  STA el_path_index

  all_done:
  rts
.endproc

;.proc load_electron_position_edge ;presume A has the position byte data
;.proc load_electron_state_data ;presume A has the state byte data
;.proc load_electron_animation_data ;presume A has the animation byte data
;.proc pickle_electron_position_edge ;outputs A with the position byte data
;.proc pickle_electron_state_data ;outputs A with the state byte data
;.proc pickle_electron_animation_data ;outputs A with the animation byte data
; STATE DATA:
;el_not_used: .res 1 ; display at all? might never use this feature, but level designers choice?
;el_class: .res 1 ; target type 0,1,2,3
;el_success: .res 1 ; has succeeded?
;el_dead: .res 1 ; has finished traversing? (at blank or off board)
;el_motion
.proc start_animation_mode
  LDX #$00
  STX level_interactive

  LDX #$01
  STX level_animation

  ;for each of the 4 electrons
  ; unpickle the electrons
  ; this is the first load, it does NOT get animation data yet...
  LDX #$01
  LDA current_el0,X ;x,y,e data here
  jsr load_electron_position_edge
  LDX #$02
  LDA current_el0,X ;state data here
  jsr load_electron_state_data ; el_not_used is the only interesting one I think
  LDX #$00
  STX el_class ; just to be sure
  STX el_success
  STX el_dead
  STX el_motion
  jsr update_new_electron; an update is what is called at the beginning of each
  jsr pickle_electron_animation_data
  LDX #$00
  STA current_el0,X

  jsr pickle_electron_position_edge
  LDX #$01
  STA current_el0,X

  jsr pickle_electron_state_data
  LDX #$02
  STA current_el0,X

  LDX #$01
  LDA current_el1,X ;x,y,e data here
  jsr load_electron_position_edge
  LDX #$02
  LDA current_el1,X ;state data here
  jsr load_electron_state_data ; el_not_used is the only interesting one I think
  LDX #$01
  STX el_class ; just to be sure
  LDX #$00
  STX el_success
  STX el_dead
  STX el_motion
  jsr update_new_electron; an update is what is called at the beginning of each
  jsr pickle_electron_animation_data
  LDX #$00
  STA current_el1,X

  jsr pickle_electron_position_edge
  LDX #$01
  STA current_el1,X

  jsr pickle_electron_state_data
  LDX #$02
  STA current_el1,X


  LDX #$01
  LDA current_el2,X ;x,y,e data here
  jsr load_electron_position_edge
  LDX #$02
  LDA current_el2,X ;state data here
  jsr load_electron_state_data ; el_not_used is the only interesting one I think
  LDX #$02
  STX el_class ; just to be sure
  LDX #$00
  STX el_success
  STX el_dead
  STX el_motion
  jsr update_new_electron; an update is what is called at the beginning of each
  jsr pickle_electron_animation_data
  LDX #$00
  STA current_el2,X

  jsr pickle_electron_position_edge
  LDX #$01
  STA current_el2,X

  jsr pickle_electron_state_data
  LDX #$02
  STA current_el2,X

  LDX #$01
  LDA current_el3,X ;x,y,e data here
  jsr load_electron_position_edge
  LDX #$02
  LDA current_el3,X ;state data here
  jsr load_electron_state_data ; el_not_used is the only interesting one I think
  LDX #$03
  STX el_class ; just to be sure
  LDX #$00
  STX el_success
  STX el_dead
  STX el_motion
  jsr update_new_electron; an update is what is called at the beginning of each
  jsr pickle_electron_animation_data
  LDX #$00
  STA current_el3,X

  jsr pickle_electron_position_edge
  LDX #$01
  STA current_el3,X

  jsr pickle_electron_state_data
  LDX #$02
  STA current_el3,X

  LDX #$00
  STX animation_increment
  STX animation_slow_loop

	rts
.endproc

.proc animation_loop

  LDA animation_increment
  CMP #$01
  BEQ :+
    RTS
  :
  LDX #$00
  STX animation_increment
  STX animation_slow_loop

  LDX #$00
  LDA current_el0,X ;x,y,e data here
  jsr load_electron_animation_data

  LDX #$01
  LDA current_el0,X ;x,y,e data here
  jsr load_electron_position_edge

  LDX #$02
  LDA current_el0,X ;x,y,e data here
  jsr load_electron_state_data

  ; if el_motion is 1 we just animate and increment the animation offset
  ; if the offset is 16 then we call electron_next_tile then update_new_electron
  LDA el_motion
  CMP #$01
  BNE :++
    jsr load_electron_xy
    LDA #$0d
    STA $0201
    LDA #$02
    STA $0202
    LDA tmp2
    STA $0200
    LDA tmp
    STA $0203

    inc el_path_offset ; TODO we'll want a delay on this
    LDA el_path_offset
    CMP #$10
    BNE:+
        jsr fetch_board_tile
        jsr electron_next_tile
        jsr update_new_electron
        jsr pickle_electron_animation_data
        LDX #$00
        STA current_el0,X
        jsr pickle_electron_position_edge
        LDX #$01
        STA current_el0,X
        jsr pickle_electron_state_data
        LDX #$02
        STA current_el0,X
        jmp start_electron1
    :

    jsr pickle_electron_animation_data
    LDX #$00
    STA current_el0,X
    jmp start_electron1
  :

  start_electron1:


  LDX #$00
  LDA current_el1,X ;x,y,e data here
  jsr load_electron_animation_data

  LDX #$01
  LDA current_el1,X ;x,y,e data here
  jsr load_electron_position_edge

  LDX #$02
  LDA current_el1,X ;x,y,e data here
  jsr load_electron_state_data

  ; if el_motion is 1 we just animate and increment the animation offset
  ; if the offset is 16 then we call electron_next_tile then update_new_electron
  LDA el_motion
  CMP #$01
  BNE :++
    jsr load_electron_xy
    LDA #$0d
    STA $0205
    LDA #$02
    STA $0206
    LDA tmp2
    STA $0204
    LDA tmp
    STA $0207

    inc el_path_offset ; TODO we'll want a delay on this
    LDA el_path_offset
    CMP #$10
    BNE:+
        jsr fetch_board_tile
        jsr electron_next_tile
        jsr update_new_electron
        jsr pickle_electron_animation_data
        LDX #$00
        STA current_el1,X
        jsr pickle_electron_position_edge
        LDX #$01
        STA current_el1,X
        jsr pickle_electron_state_data
        LDX #$02
        STA current_el1,X
        jmp start_electron2
    :

    jsr pickle_electron_animation_data
    LDX #$00
    STA current_el1,X
    jmp start_electron2
  :

  start_electron2:


  LDX #$00
  LDA current_el2,X ;x,y,e data here
  jsr load_electron_animation_data

  LDX #$01
  LDA current_el2,X ;x,y,e data here
  jsr load_electron_position_edge

  LDX #$02
  LDA current_el2,X ;x,y,e data here
  jsr load_electron_state_data

  ; if el_motion is 1 we just animate and increment the animation offset
  ; if the offset is 16 then we call electron_next_tile then update_new_electron
  LDA el_motion
  CMP #$01
  BNE :++
    jsr load_electron_xy
    LDA #$0d
    STA $0209
    LDA #$02
    STA $020a
    LDA tmp2
    STA $0208
    LDA tmp
    STA $020b

    inc el_path_offset ; TODO we'll want a delay on this
    LDA el_path_offset
    CMP #$10
    BNE:+
        jsr fetch_board_tile
        jsr electron_next_tile
        jsr update_new_electron
        jsr pickle_electron_animation_data
        LDX #$00
        STA current_el2,X
        jsr pickle_electron_position_edge
        LDX #$01
        STA current_el2,X
        jsr pickle_electron_state_data
        LDX #$02
        STA current_el2,X
        jmp start_electron3
    :

    jsr pickle_electron_animation_data
    LDX #$00
    STA current_el2,X
    jmp start_electron3
  :

  start_electron3:

  LDX #$00
  LDA current_el3,X ;x,y,e data here
  jsr load_electron_animation_data

  LDX #$01
  LDA current_el3,X ;x,y,e data here
  jsr load_electron_position_edge

  LDX #$02
  LDA current_el3,X ;x,y,e data here
  jsr load_electron_state_data

  ; if el_motion is 1 we just animate and increment the animation offset
  ; if the offset is 16 then we call electron_next_tile then update_new_electron
  LDA el_motion
  CMP #$01
  BNE :++
    jsr load_electron_xy
    LDA #$0d
    STA $020d
    LDA #$02
    STA $020e
    LDA tmp2
    STA $020c
    LDA tmp
    STA $020f

    inc el_path_offset ; TODO we'll want a delay on this
    LDA el_path_offset
    CMP #$10
    BNE:+
        jsr fetch_board_tile
        jsr electron_next_tile
        jsr update_new_electron
        jsr pickle_electron_animation_data
        LDX #$00
        STA current_el3,X
        jsr pickle_electron_position_edge
        LDX #$01
        STA current_el3,X
        jsr pickle_electron_state_data
        LDX #$02
        STA current_el3,X
        jmp start_electron4
    :

    jsr pickle_electron_animation_data
    LDX #$00
    STA current_el3,X
    jmp start_electron4
  :

  start_electron4:

	rts
.endproc

.proc loselevel
  LDA #$01
  STA between_levels
  LDA #$00
  STA level_done
  STA level_interactive
  STA level_animation
  jsr draw_bad_screen
	rts
.endproc

.proc winlevel
  inc levelnum
  LDA levelnum
  CMP total_levels
  BEQ :+
    LDA #$01
    STA between_levels
    LDA #$00
    STA level_done
    STA level_interactive
    STA level_animation
    jsr draw_good_screen
    jmp alldone
  :
  LDA #$01
  STA between_levels
  LDA #$00
  STA level_done
  STA level_interactive
  STA level_animation
  jsr draw_win_screen
  alldone:
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
bgnam:
.incbin "TitleScreen.nam"
goodnam:
.incbin "LevelClear.nam"
winnam:
.incbin "GameWin.nam"
badnam:
.incbin "TryAgain.nam"
palettes: ; hybrid exporting these from NES lightbox and half manual
.incbin "bg.pal"
.incbin "bg.pal"
nextlookup: ; tile type is row, every 4 bytes is for each edge (0, 1, 2, 3), bytes are deltaX, deltaY, newEdge, pathIndex
.byte $00, $01, $00, $00, $ff, $00, $01, $01, $00, $ff, $02, $02, $01, $00, $03, $03
.byte $01, $00, $03, $04, $00, $ff, $02, $05, $ff, $00, $01, $06, $00, $01, $00, $07
.byte $ff, $00, $01, $08, $00, $01, $00, $09, $01, $00, $03, $0a, $00, $ff, $02, $0b
epathsx:
.incbin "pathsx.dat"
epathsy:
.incbin "pathsy.dat"
levels:
.incbin "levels.dat"

.segment "CHR"
.incbin "TILESETHERE.chr" ; NES lightbox select Tilesets "export CHR as"
