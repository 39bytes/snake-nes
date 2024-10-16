.include "symbols.s"

INES_MAPPER = 0 ; Mapper 0
INES_MIRROR = 1 ; Horizontal mirroring
INES_SRAM   = 0 ; Battery backed RAM on cartridge

.segment "HEADER"
  .byte "NES", $1A ; Identifier
  .byte 2                  ; 2x 16KB PRG code
  .byte 1                  ; 1x  8KB CHR data
  .byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $0F) << 4)
  .byte (INES_MAPPER & $F0)
  .byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; Character memory
.segment "TILES"
  .incbin "../assets/tiles.chr"
  
.segment "VECTORS"
  .addr nmi
  .addr reset
  .addr irq

; Commonly used variables
.segment "ZEROPAGE"

.enum PpuSignal
  FrameReady = 1 
  DisableRendering = 2
.endenum

  ; NMI state
  nmi_lock: .res 1 ; Prevent NMI re-entry
  ; Signals for the NMI handler
  ; If set to PpuSignal::FrameReady, trigger a frame update (write nt_update)
  ; If set to PpuSignal::DisableRendering, turn off PPU rendering
  ; When the NMI triggers, the NMI handler will set this variable back to 0,
  ; which means it acknowledged the signal.
  nmi_signal: .res 1 

  ; Controller input
  buttons: .res 1

  ; Temp registers
  t1: .res 1
  t2: .res 1
  t3: .res 1
  t4: .res 1

  GRID_WIDTH = 16
  GRID_HEIGHT = 15
  SNAKE_FRAMES_PER_MOVE = 10

  ; Commonly used game state
  snake_timer: .res 1
  ; RNG seed
  seed: .res 2

.segment "BSS"
  ; Nametable/palette buffers for PPU update
  nt_update:     .res 256 
  nt_update_len: .res 1
  pal_update:    .res 32

  ; Menu state
  menu_state: .res 1

  ; Game state
  snake_head_index:     .res 1
  snake_direction:      .res 1
  snake_next_direction: .res 1
  snake_len:            .res 1
  snake_x:	        .res 256
  snake_y:	        .res 256
  ; Array of booleans for snake body presence
  snake_segments:       .res 256

  apple_x:	    .res 1
  apple_y:	    .res 1

.segment "OAM"
  oam: .res 256

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"


; Main code segment for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx PPUCTRL	; disable NMI
  stx PPUMASK	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit PPUSTATUS
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory
  
  jsr clear_background

; second wait for vblank, PPU is ready after this
vblankwait2:
  bit PPUSTATUS
  bpl vblankwait2

  ; Enable rendering
  lda #%10000000	; Enable NMI
  sta PPUCTRL
  lda #%00011110	; Enable Sprites and background
  sta PPUMASK

  jmp main


nmi:
  ; Preserve registers
  ; Push A, then X, then Y
  pha
  txa
  pha
  tya
  pha

  ; Lock the NMI, if the NMI takes too long then it will re-enter itself, 
  ; this will make it return immediately if that does happen.
  lda nmi_lock
  beq :+
    jmp @nmi_end
:
  lda #1
  sta nmi_lock

  ; Rendering logic
  ; Check what the NMI signal is
  lda nmi_signal 
  bne :+          ; If the signal is 0, that means the next frame isn't ready yet
    jmp @nmi_end
:
  cmp #PpuSignal::DisableRendering
  bne :+
    lda #%00000000
    sta PPUMASK
    lda #0
    sta nmi_signal
    jmp @nmi_end
:

  ; otherwise the signal must've been PpuSignal::FrameRead

  ; Update the nametables with the buffered tile updates
  ldx #0
  
  @nt_update_loop: 
    lda nt_update, X ; Write addr high byte
    sta PPUADDR
    inx
    lda nt_update, X ; Write addr low byte
    sta PPUADDR
    inx
    lda nt_update, X ; Write tile ID
    sta PPUDATA
    inx
    ; while (x < nt_update_len)
    cpx nt_update_len
    bcc @nt_update_loop

  ; Clear the buffer
  lda #0
  sta nt_update_len

@scroll:
  lda #0
  and #%00000011 ; keep only lowest 2 bits to prevent error
  ora #%10001000
  sta PPUCTRL
  lda #0
  sta PPUSCROLL
  lda #0
  sta PPUSCROLL
  ; enable rendering
  lda #%00011110
  sta PPUMASK

@ppu_update_done:
  ; Done rendering, unlock NMI and acknowledge frame as complete
  lda #0
  sta nmi_lock
  sta nmi_signal

@nmi_end:
  ; Restore registers
  ; Pop Y, then X, then A
  pla
  tay
  pla
  tax
  pla

  rti

irq:
  rti

; Turn off the PPU rendering for manual nametable updates
.proc ppu_disable_rendering
  lda #PpuSignal::DisableRendering
  sta nmi_signal
  :
    lda nmi_signal
    bne :-
  rts
.endproc

; Block until NMI returns
.proc ppu_update
  lda #PpuSignal::FrameReady
  sta nmi_signal
  :
    lda nmi_signal
    bne :-
  rts
.endproc

; Set tile at X/Y to A next time ppu_update is called
; Can be used with rendering on
; Preserves X, Y and A
.proc ppu_update_tile
  ; This function just stores a nametable address + a tile ID for nametable $2000
  ; into the buffer.
  ; The address is gonna have the form 0010 00YY YYYX XXXX

  ; Preserve registers
  sta t1 ; t1 = A
  stx t2 ; t2 = X
  sty t3 ; t3 = Y

  ; Computing the high byte of the address
  ; Take only the top 2 bits of Y
  tya
  lsr
  lsr
  lsr
  ora #$20 

  ldx nt_update_len ; nt_update[nt_update_len] = addr high byte
  sta nt_update, X
  inx               ; nt_update_len++;

  ; Computing the lower byte of the address
  tya ; Put the low 3 bits of Y into the top
  asl
  asl
  asl
  asl
  asl
  sta t4
  ; load X
  lda t2 
  ora t4           ; OR in X so we get YYYX XXXX
  sta nt_update, X ; nt_update[nt_update_len] = addr high byte
  inx              ; nt_update_len++; 
  ; load A
  lda t1
  sta nt_update, X
  inx
  ; Write back the new length of nt_update 
  stx nt_update_len

  ; Restore registers
  lda t1
  ldx t2
  ldy t3

  rts
.endproc

; Sets a 2x2 group of tiles at X/Y (0-15) to A next time ppu_update is called
; Can be used with rendering on
.proc ppu_update_tile_2x2
  tile_id = t1

  sta tile_id 
  clc

  txa ; x << 1
  rol
  tax
  
  tya ; y << 1
  rol
  tay 
  
  ; Top left
  lda tile_id
  jsr ppu_update_tile
  
  ; Top right
  inx
  inc tile_id
  lda tile_id
  jsr ppu_update_tile
  
  ; Bottom left
  dex
  iny
  inc tile_id 
  lda tile_id
  jsr ppu_update_tile

  ; Bottom right
  inx
  inc tile_id 
  lda tile_id
  jsr ppu_update_tile

  rts
.endproc

; Set tile at X/Y to character A next time ppu_update is called
; Can be used with rendering on
; Preserves X, Y and A
.proc ppu_put_char
  jsr ppu_update_tile
  rts
.endproc

; Update a byte in the nametable
; XY = A
.proc ppu_update_byte
  pha ; temporarily store A on stack
  tya
  pha ; temporarily store Y on stack
  ldy nt_update_len
  txa
  sta nt_update, Y
  iny
  pla ; recover Y value (but put in Y)
  sta nt_update, Y
  iny
  pla ; recover A value (byte)
  sta nt_update, Y
  iny
  sty nt_update_len

  rts
.endproc

; 32x32 -> 16x16
; Update an attribute byte to A where the top left is X/Y
; y >> 1 |
.proc ppu_update_attribute
  pha
  low_byte = t1

  lda #$C0
  sta low_byte

  ; 0xC0 | (y >> 1) << 2 | (x >> 3)
  tya
  lsr 
  lsr 
  asl ; (y >> 2) << 3
  asl
  asl

  ora low_byte
  sta low_byte
  
  txa ; (x >> 2)
  lsr
  lsr
  
  ora low_byte
  sta low_byte

  ldx #$23
  ldy low_byte
  pla
  jsr ppu_update_byte

  rts
.endproc

; Makes the background all black.
; Rendering must be turned off before this is called.
.proc clear_background
  lda PPUSTATUS ; clear write latch

  ; Set base address for the first nametable
  lda #$20
  sta PPUADDR
  lda #$00
  sta PPUADDR

  ldy #30  ; 30 rows
  :
    ldx #32
    :
      sta PPUDATA
      dex
      bne :-
    dey
    bne :--

  rts
.endproc

;
; Main section
;

.enum Direction
  Right = 0
  Left  = 1
  Up    = 2
  Down  = 3
.endenum

palettes:
  ; Background Palette
  .byte $0f, $20, $16, $00
  .byte $0f, $20, $10, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

  ; Sprite Palette
  .byte $0f, $20, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

main:
  lda PPUSTATUS   ; reset write latch
  lda #$3f        ; write palette base addr ($3F00)
  sta PPUADDR
  lda #$00
  sta PPUADDR
  ldx #$00
@load_palettes:   ; Load all 20 bytes of palettes
  lda palettes, x
  sta PPUDATA
  inx
  cpx #$20
  bne @load_palettes
setup:
  lda #$78
  sta seed
  lda #$56
  sta seed+1
  jsr init_snake
  jsr new_apple

  jsr ppu_update
@loop:
  ; Logic executed once per frame here
  jsr handle_input
  
  dec snake_timer
  bne :+
    lda #SNAKE_FRAMES_PER_MOVE
    sta snake_timer
    jsr update_snake_position
:

  ; Draw everything
  jsr ppu_update
  jmp @loop

gameover:
@loop:
  jsr handle_input
  jsr write_gameover_text
  jsr ppu_update
  jmp @loop

; ----------
; Procedures
; ----------

; --------------
; Input handling
; --------------
BUTTON_RIGHT  = 1 << 0
BUTTON_LEFT   = 1 << 1
BUTTON_DOWN   = 1 << 2
BUTTON_UP     = 1 << 3
BUTTON_START  = 1 << 4
BUTTON_SELECT = 1 << 5
BUTTON_B      = 1 << 6
BUTTON_A      = 1 << 7

; Reads the bitset of buttons from the controller.
; Preserves: X, Y
.proc poll_input
  ; Turn strobe on and off to poll input state once
  lda #1
  sta CONTROLLER1
  sta buttons     ; Insert a bit here that will be shifted out into the carry after 8 reads to end the loop
  lda #0
  sta CONTROLLER1
  
@read_button:
  lda CONTROLLER1
  lsr a        ; bit 0 -> Carry
  rol buttons  ; Carry -> bit 0; bit 7 -> Carry
  bcc @read_button

  rts
.endproc

; Preserves: X, Y
.proc handle_input
  jsr poll_input

  lda buttons
  and #BUTTON_START
  beq :+
    jmp reset             ; restart when pressing start
:
  lda buttons             ; if right is pressed
  and #BUTTON_RIGHT
  beq :+
    lda snake_direction   ; backwards (left) not allowed
    cmp #Direction::Left
    beq @end
    
    lda #Direction::Right
    sta snake_next_direction
    jmp @end
:
  lda buttons             ; if left is pressed
  and #BUTTON_LEFT
  beq :+
    lda snake_direction   ; backwards (right) not allowed
    cmp #Direction::Right
    beq @end

    lda #Direction::Left
    sta snake_next_direction
    jmp @end
:
  lda buttons             ; if up is pressed
  and #BUTTON_UP
  beq :+
    lda snake_direction   ; backwards (Down) not allowed
    cmp #Direction::Down
    beq @end

    lda #Direction::Up
    sta snake_next_direction
    jmp @end
:
  lda buttons             ; if down is pressed
  and #BUTTON_DOWN
  beq :+
    lda snake_direction   ; backwards (Up) not allowed
    cmp #Direction::Up
    beq @end

    lda #Direction::Down
    sta snake_next_direction
    jmp @end
:
  @end:
    rts
.endproc

; -----------
; Snake stuff
; -----------
; Set initial state for the snake
.proc init_snake
  lda #3
  sta snake_len
  
  lda #0
  sta snake_head_index
  
  lda #Direction::Right
  sta snake_direction

  lda #SNAKE_FRAMES_PER_MOVE
  sta snake_timer

  ; Load some initial snake segments
  ; snake[0] = (2, 8)
  ldx #0
  lda #2
  sta snake_x, X
  lda #8
  sta snake_y, X

  ldx #2 ; Write tile 1 at (2, 8)
  ldy #8
  lda #4
  jsr ppu_update_tile_2x2

  ; snake[1] = (1, 8)
  ldx #1
  lda #1
  sta snake_x, X
  lda #8
  sta snake_y, X

  ldx #1 ; Write tile 1 at (1, 8)
  ldy #8
  lda #4
  jsr ppu_update_tile_2x2

  ; snake[2] = (0, 8)
  ldx #2
  lda #0
  sta snake_x, X
  lda #8
  sta snake_y, X
  
  ldx #0 ; Write tile 1 at (0, 8)
  ldy #8
  lda #4
  jsr ppu_update_tile_2x2

  rts
.endproc

; Move the snake in the current direction
; Does not preserve registers
.proc update_snake_position
  next_x = t1
  next_y = t2

  ; Write queued direction
  lda snake_next_direction
  sta snake_direction

  ldx snake_head_index
  lda snake_x, x ; store head x
  sta next_x
  lda snake_y, x ; store head y
  sta next_y

  lda snake_direction
  cmp #Direction::Right
  beq @right
  cmp #Direction::Left
  beq @left
  cmp #Direction::Up
  beq @up
  cmp #Direction::Down
  beq @down
  
  @right:
    ldx next_x      ; next_x++
    inx
    cpx #GRID_WIDTH ; if next_x >= GRID_WIDTH
    bcc :+
      ldx #0
  :
    stx next_x
    jmp @switch_end
  @left:
    dec next_x        ; next_x--;
    bpl :+            ; if next_x < 0 
      ldx #GRID_WIDTH ; next_x = GRID_WIDTH - 1
      dex
      stx next_x
  :
    jmp @switch_end
  @up:
    dec next_y         ; next_y--;
    bpl :+             ; if next_y < 0 
      ldx #GRID_HEIGHT ; next_y = GRID_HEIGHT - 1
      dex
      stx next_y
  :
    jmp @switch_end
  @down:
    ldx next_y         ; next_y++;
    inx
    cpx #GRID_HEIGHT   ; if next_y >= GRID_HEIGHT
    bcc :+
      ldx #0           ; next_y = GRID_HEIGHT
  :
    stx next_y
@switch_end:

  dec snake_head_index ; snake_head_index --;
  ; write new head
  ; Also push x and y to use them after drawing the head tile
  ldx snake_head_index
  lda next_x
  pha         
  sta snake_x,x
  lda next_y
  pha
  sta snake_y,x

  ; Update tiles
  ; Draw the new head
  ldx next_x 
  ldy next_y
  lda #4
  jsr ppu_update_tile_2x2

  pla ; pop next_y
  tay
  pla ; pop next_x
  tax 
  ; if next_x == apple_x && next_y == apple_y
  cpx apple_x 
  bne :+
  cpy apple_y
  bne :+
  ; Mark the segment as filled
  ; Since we're on the apple, we know we can't be inside of the body so
  ; no collision check is required
  lda #1
  jsr update_segment_set
  ; Generate new apple, increase length 
  jsr new_apple 
  inc snake_len 

  ; Don't need to remove the tail if we eat an apple
  ; so just return
  rts 
:

  ; Remove old tail
  lda snake_head_index
  clc
  adc snake_len
  tax

  tail_x = t3
  tail_y = t4

  lda snake_x, x
  sta tail_x
  lda snake_y, x
  sta tail_y

  ; Remove the segment from the set
  ldx tail_x
  ldy tail_y
  lda #0
  jsr update_segment_set

  ; Clear the tail tile
  ldx tail_x
  ldy tail_y
  lda #0
  jsr ppu_update_tile_2x2

  ; Do the collision check
  ldx snake_head_index
  lda snake_x, x
  sta next_x
  lda snake_y, x
  sta next_y
  
  ldx next_x
  ldy next_y
  jsr check_segment_set
  beq :+
    jmp gameover
:

  ; Didn't hit anything, finally update the segment set to include the new head
  ldx next_x
  ldy next_y
  lda #1
  jsr update_segment_set
 
  rts
.endproc

; Write a boolean 0/1 at (x, y) to the segment set to mark it as occupied/unoccupied
; ---Parameters---
; A - 0 or 1
; X - X coordinate
; Y - Y coordinate
.proc update_segment_set
  pha
  
  ; (y << 4) | x
  tya 
  asl
  asl
  asl
  asl
  stx t1 
  ora t1
  
  tax
  pla
  sta snake_segments, x

  rts
.endproc

; Check if the tile at (x, y) is occupied
; The zero flag will be set if unoccupied, and unset if occupied.
; ---Parameters---
; X - X coordinate
; Y - Y coordinate
.proc check_segment_set
  tya 
  asl
  asl
  asl
  asl
  stx t1 
  ora t1

  tax
  lda snake_segments, x

  rts
.endproc

;
; Apple logic
;

; From https://www.nesdev.org/wiki/Random_number_generator
;
; Generates a pseudo-random byte based on the seed.
; ---Returns---
; A - The generated byte (0-255)
.proc rand_byte
  ldy #8     ; iteration count (generates 8 bits)
  lda seed+0
:
  asl        ; shift the register
  rol seed+1
  bcc :+
  eor #$39   ; apply XOR feedback whenever a 1 bit is shifted out
:
  dey
  bne :--
  sta seed+0
  cmp #0     ; reload flags
  rts
.endproc

.proc new_apple
  jsr rand_byte
  and #%00001111 ; Only need a number from 0-15 so mask off the top 4 bits
  sta apple_x
  jsr rand_byte
  and #%00001111 ; Only need a number from 0-15 so mask off the top 4 bits
  sta apple_y
  ; Apple with Y coordinate of 15 is invalid (out of bounds)
  cmp #15
  bcs new_apple

  ; Check if position is inside of body
  ldx apple_x
  ldy apple_y
  jsr check_segment_set
  bne new_apple         ; Recurse to try again

  ldx apple_x
  ldy apple_y
  lda #8
  jsr ppu_update_tile_2x2

  rts
.endproc

; 
; Menu logic
;

.include "utils.s"

.macro put_char char, x_coord, y_coord
  ldx #x_coord
  ldy #y_coord
  lda char
  jsr ppu_put_char
.endmacro

.macro put_digit digit, x_coord, y_coord
  ldx #x_coord
  ldy #y_coord

  lda #'0'
  clc
  adc digit

  jsr ppu_put_char
.endmacro

.macro update_palette_byte byte, x_coord, y_coord
  ldx #x_coord
  ldy #y_coord
  lda #byte
  jsr ppu_update_attribute
.endmacro

.proc write_gameover_text
  put_char #'G', 12, 12
  put_char #'A', 13, 12
  put_char #'M', 14, 12
  put_char #'E', 15, 12
  put_char #' ', 16, 12
  put_char #'O', 17, 12
  put_char #'V', 18, 12
  put_char #'E', 19, 12
  put_char #'R', 20, 12

  update_palette_byte %01010101, 12, 12
  update_palette_byte %01010101, 16, 12
  update_palette_byte %01010101, 20, 12

  put_char #'S', 12, 16
  put_char #'C', 13, 16
  put_char #'O', 14, 16
  put_char #'R', 15, 16
  put_char #'E', 16, 16
  put_char #':', 17, 16
  put_char #' ', 18, 16

  lda snake_len
  clc
  sbc #3

  sta Hex0
  jsr byte_to_decimal

  put_digit DecHundreds, 19, 16
  put_digit DecTens, 20, 16
  put_digit DecOnes, 21, 16

  update_palette_byte %01010101, 12, 16
  update_palette_byte %01010101, 16, 16
  update_palette_byte %01010101, 20, 16


  put_char #'P', 12, 20
  put_char #'R', 13, 20
  put_char #'E', 14, 20
  put_char #'S', 15, 20
  put_char #'S', 16, 20
  put_char #' ', 17, 20
  put_char #'S', 18, 20
  put_char #'T', 19, 20
  put_char #'A', 20, 20
  put_char #'R', 21, 20
  put_char #'T', 22, 20

  update_palette_byte %01010101, 12, 20
  update_palette_byte %01010101, 16, 20
  update_palette_byte %01010101, 20, 20
  
  rts
.endproc
