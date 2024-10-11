.include "symbols.s"

; See https://github.com/bbbradsmith/NES-ca65-example/blob/master/example.s
; Also useful: https://www.nesdev.org/wiki/The_frame_and_NMIs
INES_MAPPER = 0 ; Mapper 0
INES_MIRROR = 1 ; Horizontal mirroring
INES_SRAM   = 0 ; Battery backed RAM on cartridge

.segment "HEADER"
  .byte $4E, $45, $53, $1A ; Identifier
  .byte 2                  ; 2x 16KB PRG code
  .byte 1                  ; 1x  8KB CHR data
  .byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $0F) << 4)
  .byte (INES_MAPPER & $F0)
  .byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; Character memory
.segment "TILES"
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte %00000000	; Square TL
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte %00000000	; Square TR
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte %01111111	; Square BL
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  .byte %11111110	; Square BR
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %11111110
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; Blank
  
.segment "VECTORS"
  .addr nmi
  .addr reset
  .addr irq

; Commonly used variables
.segment "ZEROPAGE"
  ; NMI state
  nmi_lock: .res 1 ; Prevent NMI re-entry
  ; Signals for the NMI handler
  ; If set to SIGNAL_FRAME_READY, trigger a frame update (write nt_update)
  ; When the NMI triggers, the NMI handler will set this variable back to 0,
  ; which means it acknowledged the signal.
  SIGNAL_FRAME_READY = 1
  nmi_signal: .res 1 

  ; Temp registers
  t1: .res 1
  t2: .res 1
  t3: .res 1
  t4: .res 1

  GRID_WIDTH = 16
  GRID_HEIGHT = 15
  ; Commonly used game state
  SNAKE_FRAMES_PER_MOVE = 15
  snake_timer: .res 1

.segment "BSS"
  ; Nametable/palette buffers for PPU update
  nt_update:     .res 256 
  nt_update_len: .res 1
  pal_update:    .res 32

  ; Game state
  snake_head_index: .res 1
  snake_direction:  .res 1
  snake_len:        .res 1
  snake_x:	    .res 256
  snake_y:	    .res 256

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
  sta $2000
  lda #0
  sta $2005
  lda #0
  sta $2005
  ; enable rendering
  lda #%00011110
  sta $2001


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

.proc ppu_update
  lda #1
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
  sta t1 ; t1 stores tile_id
  clc

  txa ; x << 1
  rol
  tax
  
  tya ; y << 1
  rol
  tay 
  
  ; Top left
  lda t1
  jsr ppu_update_tile
  
  ; Top right
  inx
  inc t1 ; tile_id++
  lda t1
  jsr ppu_update_tile
  
  ; Bottom left
  dex
  iny
  inc t1 ; tile_id++
  lda t1
  jsr ppu_update_tile

  ; Bottom right
  inx
  inc t1 ; tile_id++
  lda t1
  jsr ppu_update_tile

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
  .byte $0f, $20, $00, $00
  .byte $0f, $00, $00, $00
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
  jsr init_snake

  jsr ppu_update
@loop:
  ; Logic executed once per frame here
  dec snake_timer
  bne :+
    lda #SNAKE_FRAMES_PER_MOVE
    sta snake_timer
    jsr update_snake_position
:

  ; Draw everything
  jsr ppu_update
  jmp @loop

; 
; Procedures
;

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
  ldx snake_head_index
  lda snake_x,x ; store head x
  sta t1        ; next_x
  lda snake_y,x ; store head y
  sta t2        ; next_y

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
    ldx t1          ; next_x++
    inx
    cpx #GRID_WIDTH ; if next_x >= GRID_WIDTH
    bcc :+
      ldx #0
  :
    stx t1
    jmp @switch_end
  @left:
    dec t1            ; next_x--;
    bpl :+            ; if next_x < 0 
      ldx #GRID_WIDTH ; next_x = GRID_WIDTH - 1
      dex
      stx t1
  :
    jmp @switch_end
  @up:
    dec t2             ; next_y--;
    bpl :+             ; if next_y < 0 
      ldx #GRID_HEIGHT ; next_y = GRID_HEIGHT - 1
      dex
      stx t2
  :
    jmp @switch_end
  @down:
    ldx t2             ; next_y++;
    inx
    cpx #GRID_HEIGHT   ; if next_y >= GRID_HEIGHT
    bcc :+
      ldx #0           ; next_y = GRID_HEIGHT
  :
    stx t2
@switch_end:

  dec snake_head_index ; snake_head_index --;
  ; write new head
  ldx snake_head_index
  lda t1
  sta snake_x,x
  lda t2
  sta snake_y,x

  ; Update tiles
  ; Draw the new head
  ldx t1 
  ldy t2
  lda #4
  jsr ppu_update_tile_2x2

  ; Remove old tail
  lda snake_head_index
  clc
  adc snake_len
  tax

  lda snake_x, x
  sta t1
  lda snake_y, x
  sta t2

  ldx t1
  ldy t2
  lda #0
  jsr ppu_update_tile_2x2
  
  rts
.endproc
