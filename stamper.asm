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
  LDA #$00

  STX tmp
  STA pnum
  LDA topleftcornerhi
  STA addrhi
  LDA topleftcornerlo
  CLC
  ADC tmp
  ADC tmp
  ADC tmp
  ADC tmp      
  STA addrlo

  STY tmp
  LDA tmp
  and #$1
  beq is_even
    LDA #$80
    STA tmp2
    LDA addrlo
    SBC tmp2
    STA addrlo
  is_even:
  LDA tmp
  beq is_zero
    DEY
    TYA
    LSR
    TAY
    INY
    TYA
    ADC addrhi
    STA addrhi
  is_zero:
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
  ;do that 16ish times
