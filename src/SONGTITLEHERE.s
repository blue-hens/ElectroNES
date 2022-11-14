music_data_songtitle:
	.byte 1
	.word @instruments
	.word @samples-4
	.word @song0ch0,@song0ch1,@song0ch2,@song0ch3,@song0ch4 ; 00 : Song 1
	.byte .lobyte(@tempo_env_1_mid), .hibyte(@tempo_env_1_mid), 0, 0

.export music_data_songtitle
.global FAMISTUDIO_DPCM_PTR

@instruments:
	.word @env1,@env2,@env3,@env0 ; 00 : Instrument 1

@samples:

@env0:
	.byte $00,$c0,$7f,$00,$02
@env1:
	.byte $00,$cf,$7f,$00,$02
@env2:
	.byte $c0,$7f,$00,$01
@env3:
	.byte $7f,$00,$00

@tempo_env_1_mid:
	.byte $03,$05,$80

@song0ch0:
@song0ch0loop:
	.byte $46, .lobyte(@tempo_env_1_mid), .hibyte(@tempo_env_1_mid)
@song0ref5:
	.byte $80
@song0ref6:
	.byte $19, $a5, $00, $a7, $19, $a3, $00, $a5, $1b, $a7, $00, $a5, $19, $a3, $00, $a5, $47, $1e, $a5, $00, $a7, $1d, $ff, $eb
	.byte $47
	.byte $41, $10
	.word @song0ref6
	.byte $47, $81, $20, $a3, $00, $a7, $1e, $ff, $e9, $00, $47
@song0ref45:
	.byte $19, $a7, $00, $a3, $19, $a5, $00, $a5, $25, $a5, $00, $a5, $22, $a5, $00, $a5, $47
@song0ref62:
	.byte $1e, $a5, $00, $a5, $1d, $a5, $00, $a5, $1b, $a5, $00, $a5, $23, $a5, $00, $a5, $47
@song0ref79:
	.byte $23, $a5, $00, $a5, $22, $a9, $00, $a1, $1e, $a7, $00, $a3, $20, $a5, $00, $a5, $47, $1e, $ff, $9b, $00, $ff, $9f
@song0ref102:
	.byte $47, $ff, $ff, $bf, $47, $ff, $ff, $bf, $47, $ff, $ff, $bf, $47, $ff, $ff, $bf
	.byte $41, $0c
	.word @song0ref102
	.byte $42
	.word @song0ch0loop
@song0ch1:
@song0ch1loop:
	.byte $ff, $ff, $bf, $ff, $ff, $bf
	.byte $41, $10
	.word @song0ref5
	.byte $1e, $a5, $00, $a7, $1d, $ff, $eb
	.byte $41, $10
	.word @song0ref6
@song0ref144:
	.byte $81, $20, $a3, $00, $a7, $1e, $ff, $99, $00, $cf
	.byte $41, $10
	.word @song0ref45
	.byte $41, $10
	.word @song0ref62
	.byte $41, $10
	.word @song0ref79
@song0ref163:
	.byte $1e, $ff, $9b, $00, $ff, $9f
@song0ref169:
	.byte $ff, $ff, $bf, $ff, $ff, $bf, $ff, $ff, $bf, $ff, $ff, $bf, $ff, $ff, $bf, $ff, $ff, $bf, $42
	.word @song0ch1loop
@song0ch2:
@song0ch2loop:
	.byte $ff, $ff, $bf
	.byte $41, $10
	.word @song0ref5
	.byte $1e, $a5, $00, $a7, $1d, $ff, $eb
	.byte $41, $10
	.word @song0ref6
	.byte $41, $0a
	.word @song0ref144
	.byte $41, $10
	.word @song0ref45
	.byte $41, $10
	.word @song0ref62
	.byte $41, $10
	.word @song0ref79
	.byte $41, $18
	.word @song0ref163
	.byte $ff, $ff, $bf, $42
	.word @song0ch2loop
@song0ch3:
@song0ch3loop:
	.byte $41, $12
	.word @song0ref169
	.byte $41, $12
	.word @song0ref169
	.byte $41, $0c
	.word @song0ref169
	.byte $42
	.word @song0ch3loop
@song0ch4:
@song0ch4loop:
	.byte $41, $12
	.word @song0ref169
	.byte $41, $12
	.word @song0ref169
	.byte $41, $0c
	.word @song0ref169
	.byte $42
	.word @song0ch4loop
