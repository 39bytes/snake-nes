;Given: Hex value in Hex0
;Returns decimal value in DecOnes, DecTens, and DecHundreds.

; https://www.nesdev.org/wiki/HexToDecimal.8

.segment "ZEROPAGE"
  Hex0: .res 1
  DecOnes: .res 1
  DecTens: .res 1
  DecHundreds: .res 1

.segment "CODE"

.proc byte_to_decimal
  ;Given: Hex value in Hex0
  ;Returns decimal value in DecOnes, DecTens, and DecHundreds.
  lda #$00
  sta DecOnes
  sta DecTens
  sta DecHundreds

  lda Hex0
  and #$0F
  tax
  lda HexDigit00Table,x
  sta DecOnes
  lda HexDigit01Table,x
  sta DecTens

  lda Hex0
  lsr a
  lsr a
  lsr a
  lsr a
  tax
  lda HexDigit10Table,x
  clc
  adc DecOnes
  sta DecOnes
  lda HexDigit11Table,x
  adc DecTens
  sta DecTens
  lda HexDigit12Table,x
  sta DecHundreds

  clc
  ldx DecOnes
  lda DecimalSumsLow,x
  sta DecOnes


  lda DecimalSumsHigh,x
  adc DecTens
  tax
  lda DecimalSumsLow,x
  sta DecTens

  lda DecimalSumsHigh,x
  adc DecHundreds
  tax
  lda DecimalSumsLow,x
  sta DecHundreds			;118

  rts
.endproc

;1 byte
HexDigit32Table:
  .byte $0

HexDigit00Table:
HexDigit56Table:
DecimalSumsLow:
    ;55 bytes
  .byte $0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$0,$1,$2,$3,$4,$5
  .byte $6,$7,$8,$9,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$0,$1
  .byte $2,$3,$4,$5,$6,$7,$8,$9,$0,$1,$2,$3,$4,$5,$6,$7
  .byte $8,$9,$0,$1,$2,$3,$4

HexDigit01Table:
HexDigit57Table:
DecimalSumsHigh:
    ;55 bytes
  .byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1
  .byte $1,$1,$1,$1,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$3,$3
  .byte $3,$3,$3,$3,$3,$3,$3,$3,$4,$4,$4,$4,$4,$4,$4,$4
  .byte $4,$4,$5,$5,$5,$5,$5

;111 bytes
;******
HexDigit50Table:
HexDigit40Table:
HexDigit30Table:
HexDigit20Table:
HexDigit10Table:
  .byte $0,$6,$2,$8,$4,$0,$6,$2,$8,$4,$0,$6,$2,$8,$4,$0

HexDigit11Table:
  .byte $0,$1,$3,$4,$6,$8,$9,$1,$2,$4,$6,$7,$9,$0,$2,$4

HexDigit12Table:
  .byte $0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$2,$2,$2
;******
HexDigit21Table:
  .byte $0,$5,$1,$6,$2,$8,$3,$9,$4,$0,$6,$1,$7,$2,$8,$4

HexDigit22Table:
  .byte $0,$2,$5,$7,$0,$2,$5,$7,$0,$3,$5,$8,$0,$3,$5,$8

HexDigit23Table:
  .byte $0,$0,$0,$0,$1,$1,$1,$1,$2,$2,$2,$2,$3,$3,$3,$3
;******
HexDigit31Table:
  .byte $0,$9,$9,$8,$8,$8,$7,$7,$6,$6,$6,$5,$5,$4,$4,$4

HexDigit33Table:
  .byte $0,$4,$8,$2,$6,$0,$4,$8,$2,$6,$0,$5,$9,$3,$7,$1

HexDigit34Table:
  .byte $0,$0,$0,$1,$1,$2,$2,$2,$3,$3,$4,$4,$4,$5,$5,$6

;******
HexDigit41Table:
  .byte $0,$3,$7,$0,$4,$8,$1,$5,$8,$2,$6,$9,$3,$6,$0,$4

HexDigit42Table:
  .byte $0,$5,$0,$6,$1,$6,$2,$7,$2,$8,$3,$8,$4,$9,$5,$0

HexDigit43Table:
  .byte $0,$5,$1,$6,$2,$7,$3,$8,$4,$9,$5,$0,$6,$1,$7,$3

HexDigit44Table:
  .byte $0,$6,$3,$9,$6,$2,$9,$5,$2,$8,$5,$2,$8,$5,$1,$8

HexDigit45Table:
  .byte $0,$0,$1,$1,$2,$3,$3,$4,$5,$5,$6,$7,$7,$8,$9,$9
;******
HexDigit51Table:
  .byte $0,$7,$5,$2,$0,$8,$5,$3,$0,$8,$6,$3,$1,$8,$6,$4

HexDigit52Table:
  .byte $0,$5,$1,$7,$3,$8,$4,$0,$6,$1,$7,$3,$9,$4,$0,$6

HexDigit53Table:
  .byte $0,$8,$7,$5,$4,$2,$1,$0,$8,$7,$5,$4,$2,$1,$0,$8

HexDigit54Table:
  .byte $0,$4,$9,$4,$9,$4,$9,$4,$8,$3,$8,$3,$8,$3,$8,$2

HexDigit55Table:
  .byte $0,$0,$0,$1,$1,$2,$2,$3,$3,$4,$4,$5,$5,$6,$6,$7
