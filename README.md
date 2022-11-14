# NES-starterkit

Was working through https://famicom.party/book/ and that glorious book stops before taking controls and sounds, 
so this is a continuation with NES lightbox and Famistudio integration with inspiration from https://github.com/bbbradsmith/NES-ca65-example/blob/master/example.s


## How To

So the main code is in `src/proj.asm` 

If you make PPU tiles and beautiful things using NES Lightbox then export the tilesets to `TILESETHERE.chr` or some equivalent name.
Now save your `Nametables` to `globalbg.nam` or equivalent.
Now save your palettes `Save Palettes As` to `bg.pal` or equivalent.

 * Make sure that TILESTHERE.chr name matches in the last line of `proj.asm`
 * Make sure the `globalbg.nam` matches the files imported just above that in the `RODATA` segment

Now make some music in FamiStudio and `Export Songs > FamiStudio Music Code > Format: CA65` and save that file to `SONGTITLEHERE.s` or some equivalent name.
Inside your `.s` file will be an exported symbol something like `.export music_data_untitled`.

 * Make sure your file name matches the included file in the `AUDIO` segment
 * Make sure your exported symbol matches the lines just before the `famistudio_init` command
 
 Now you can run `./runme.sh` or just the commands in it and test `works.nes` in an emulator.
 
 ## Why?
 
 This is to help start an NES game for a game-jam environment so you can split up the work between your programmer, a music person, and a graphics person.
 
 It might be nice to make a level format then you can have a fourth person design levels that "just work" but that will count on the logic of the game working.
 
 Now that there is a stub in the `update_player` subroutine that reacts to the `gamepad` variable and checks for button A being pressed.  Here's a more complete such loop:

```
	jsr gamepad_poll
	; respond to gamepad state
	lda gamepad
	and #PAD_START
	beq :+
		jsr push_start
		jmp @draw ; start trumps everything, don't check other buttons
	:
	jsr release_start ; releasing start restores scroll
	lda gamepad
	and #PAD_U
	beq :+
		jsr push_u
	:
	lda gamepad
	and #PAD_D
	beq :+
		jsr push_d
	:
	lda gamepad
	and #PAD_L
	beq :+
		jsr push_l
	:
	lda gamepad
	and #PAD_R
	beq :+
		jsr push_r
	:
	lda gamepad
	and #PAD_SELECT
	beq :+
		jsr push_select
	:
	lda gamepad
	and #PAD_B
	beq :+
		jsr push_b
	:
	lda gamepad
	and #PAD_A
	beq :+
		jsr push_a
	:
```
