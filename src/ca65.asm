.include   "telestrat.inc"
.include   "fcntl.inc"
.include   "include/macros_ca65.inc"
; %CC65%\ca65.exe -ttelestrat --include-dir %CC65%\asminc\ src/ca65.asm -o ca65.ld65
userzp := $80 ; FIXME

sizeof_file_max = 1000

ca65_src_code_ptr     := userzp
ca65_ptr1             := userzp+4
ca65_struct           := userzp+6
ca65_ptr2             := userzp+8
ca65_tmp1             := userzp+9

.struct ca65_infos
        length_file     .word 
        src_filename    .res 14
        ca65_tmp        .res 1
        ca65_global_labels .res 1000
        ca65_binary        .res 1000
        ca65_source_code   .res 16000
.endstruct 

.org $c000

.code
CA65_start:
        lda     #<.sizeof(ca65_infos)
        ldy     #>.sizeof(ca65_infos)

        BRK_ORIX XMALLOC
        cmp     #$00
        bne     not_oom_for_struct
        cpy     #$00
        bne     not_oom_for_struct
        PRINT   str_OOM
        PRINT   str_for_ca_struct
        jsr     return_line
        ; oom
        rts        
not_oom_for_struct:
        sta     ca65_struct
        sty     ca65_struct+1

        MALLOC  2000
        cmp #$00
        bne not_oom
        cpy #$00
        bne not_oom
        PRINT str_OOM
        ; oom
        rts

not_oom:
        sta     ca65_src_code_ptr
        sty     ca65_src_code_ptr+1


not_oom_filename:

        lda     #<filetoload
        sta     RES
        lda     #>filetoload
        sta     RES+1

        lda     ca65_struct+1
        sta     RESB+1

        lda     #<ca65_infos::src_filename
        clc 
        adc     ca65_struct
        bcc     skipme
        inc     RESB+1
skipme:    
        sta     RESB

        jsr     _strcpy

        lda     RESB
        ldx     RESB+1
        ldy     #O_RDONLY
        BRK_ORIX        XOPEN

        cpx     #$ff
        bne     load_file
        cmp     #$ff
        bne     load_file
        ; not found
        PRINT(str_notfound)
        rts

load_file:
        PRINT   str_parsing
       
        lda     ca65_struct+1
        sta     RESB+1

        lda     #ca65_infos::src_filename
        clc 
        adc     ca65_struct
        bcc     skipme2
        inc     RESB+1
skipme2:    
        sta     RESB        
       
        ldy     RESB+1
        BRK_ORIX XWSTR0
        jsr     return_line
;        Send the fp pointer
        lda     #$01 ; 1 is the fd id of the file opened
        sta     TR0   
        ; define target address
        lda     ca65_src_code_ptr

        sta     PTR_READ_DEST
        lda     ca65_src_code_ptr+1
        sta     PTR_READ_DEST+1
        ; We read 8000 bytes
        lda     #<1000
        ldy     #>1000
        ; reads byte

        BRK_TELEMON XFREAD 

        BRK_TELEMON XCLOSE
 
        jsr     parse
        FREE    ca65_struct
        rts

parse:
        lda     ca65_src_code_ptr
        sta     ca65_ptr1
        lda     ca65_src_code_ptr+1
        sta     ca65_ptr1+1

        ; Checking if it's mnemonic
        ldy     #$00
new_line:
        lda     (ca65_ptr1),y
        jsr     display_char
        cmp     #' '
        bne     check_control_command
        iny
        beq     new_line
check_control_command:        
        cmp     #'.'
        beq     start_control_command
        rts

start_control_command:
        jsr _control_command
        rts



.proc _control_command
        inc     ca65_ptr1
        bne     skip2
        inc     ca65_ptr1+1
skip2:
        ldx     #$00
skip:
        lda     controlcommand_addr_low,x
        sta     ca65_ptr2
        lda     controlcommand_addr_high,x
        sta     ca65_ptr2+1
        ldy     #$FF
loop:   
        iny
        lda     (ca65_ptr2),y
        bcs     found
continue:        
        jsr     display_char
        beq     outrout
        sty     ca65_tmp1
        cmp     (ca65_ptr1),y
        bne     out
        beq     loop
out:        
        inx
        cpx     #$05 ; FIXME number of command
        bne     skip
outrout:        
        rts
found:
        and     #%01111111
        cmp     (ca65_ptr1),y
        bne     continue
         ; at this step X contains the id of the control command
        PRINT str_control_command
        rts        
.endproc

.proc display_char
        sta    ca65_tmp1 
        pha
        txa
        pha
        tya
        pha
        lda    ca65_tmp1 
        BRK_ORIX XWR0
        pla
        tay
        pla
        tax
        pla
        rts
.endproc

.proc return_line
        BRK_ORIX XCRLF
        rts
.endproc

str_OOM:
        .ASCIIZ "Not enough memory"
str_parsing:
        .byte "Parsing : ",0
str_notfound:
        .byte "Not found",$0a,$0d,0
str_control_command:
        .byte "Control command : ",0
str_found:
        .byte "Not found",0
str_for_ca_struct:
        .ASCIIZ " for ca65 struct"

filetoload:
        .ASCIIZ "ca65.asm"
   
.proc _strcpy
        ldy     #$00

L1:     lda     (RES),y
        sta     (RESB),y
        beq     L9
        iny
        bne     L1
        inc     RES+1
        inc     RESB+1
        bne     L1

L9:     lda     RESB            ; X still contains high byte
        rts
.endproc            
    
 .data   
    
Mnemonics:
        .byte   "BR"
        .byte   $CB
        .byte   "CL"
        .byte   $C3
        .byte   "CL"
        .byte   $C4
        .byte   "CL"
        .byte   $C9
        .byte   "CL"
        .byte   $D6
        .byte   "DE"
        .byte   $D8
        .byte   "DE"
        .byte   $D9
        .byte   "IN"
        .byte   $D8
        .byte   "IN"
        .byte   $D9
        .byte   "NO"
        .byte   $D0
        .byte   "PH"
        .byte   $C1
        .byte   "PH"
        .byte   $D0
        .byte   "PL"
        .byte   $C1
        .byte   "PL"
        .byte   $D0
        .byte   "RT"
        .byte   $C9
        .byte   "RT"
        .byte   $D3
        .byte   "SE"
        .byte   $C3
        .byte   "SE"
        .byte   $C4
        .byte   "SE"
        .byte   $C9
        .byte   "TA"
        .byte   $D8
        .byte   "TA"
        .byte   $D9
        .byte   "TS"
        .byte   $D8
        .byte   "TX"
        .byte   $C1
        .byte   "TX"
        .byte   $D3
        .byte   "TY"
        .byte   $C1
        .byte   "BC"
        .byte   $C3
        .byte   "BC"
        .byte   $D3
        .byte   "BE"
        .byte   $D1
        .byte   "BN"
        .byte   $C5
        .byte   "BM"
        .byte   $C9
        .byte   "BP"
        .byte   $CC
        .byte   "BV"
        .byte   $C3
        .byte   "BV"
        .byte   $D3
        .byte   "AD"
        .byte   $C3
        .byte   "AN"
        .byte   $C4
        .byte   "AS"
        .byte   $CC
        .byte   "BI"
        .byte   $D4
        .byte   "CM"
        .byte   $D0
        .byte   "CP"
        .byte   $D8
        .byte   "CP"
        .byte   $D9
        .byte   "DE"
        .byte   $C3
        .byte   "EO"
        .byte   $D2
        .byte   "IN"
        .byte   $C3
        .byte   "JM"
        .byte   $D0
        .byte   "JS"
        .byte   $D2
        .byte   "LD"
        .byte   $C1
        .byte   "LD"
        .byte   $D8
        .byte   "LD"
        .byte   $D9
        .byte   "LS"
        .byte   $D2
        .byte   "OR"
        .byte   $C1
        .byte   "RO"
        .byte   $CC
        .byte   "RO"
        .byte   $D2
        .byte   "SB"
        .byte   $C3
        .byte   "ST"
        .byte   $C1
        .byte   "ST"
        .byte   $D8
        .byte   "ST"
        .byte   $D9
; preprocessor
controlcommand:
controlcommand_byt:
        .byte "by"
        .byte 't'+128
controlcommand_byte:
        .byte "byt"
        .byte 'e'+128
controlcommand_import:
        .byte "impor"
        .byte 't'+128        
controlcommand_importzp:
        .byte "importz"
        .byte 'p'+128        
controlcommand_incbin:
        .byte "incbi"
        .byte 'n'+128           
controlcommand_proc:
        .byte "pro"
        .byte 'c'+128        

controlcommand_addr_low:
        .byte   <controlcommand_byt
        .byte   <controlcommand_byte
        .byte   <controlcommand_import
        .byte   <controlcommand_importzp
        .byte   <controlcommand_incbin        

controlcommand_addr_high:
        .byte   >controlcommand_byt
        .byte   >controlcommand_byte
        .byte   >controlcommand_import
        .byte   >controlcommand_importzp
        .byte   >controlcommand_incbin        

; ----------------------------------------------------------------------------
; Oplen
Oplen:  .byte   $01,$55,$00,$00,$00,$01,$01,$00
        .byte   $00,$81,$00,$00,$00,$02,$02,$00
        .byte   $C1,$69,$00,$00,$00,$11,$11,$00
        .byte   $00,$0A,$00,$00,$00,$12,$12,$00
        .byte   $02,$55,$00,$00,$01,$01,$01,$00
        .byte   $00,$81,$00,$00,$02,$02,$02,$00
        .byte   $C1,$69,$00,$00,$00,$11,$11,$00
        .byte   $00,$0A,$00,$00,$00,$12,$12,$00
        .byte   $00,$55,$00,$00,$00,$01,$01,$00
        .byte   $00,$81,$00,$00,$02,$02,$02,$00
        .byte   $C1,$69,$00,$00,$00,$11,$11,$00
        .byte   $00,$0A,$00,$00,$00,$12,$12,$00
        .byte   $00,$55,$00,$00,$00,$01,$01,$00
        .byte   $00,$81,$00,$00,$62,$02,$02,$00
        .byte   $C1,$69,$00,$00,$00,$11,$11,$00
        .byte   $00,$0A,$00,$00,$00,$12,$12,$00
        .byte   $00,$55,$00,$00,$01,$01,$01,$00
        .byte   $00,$00,$00,$00,$02,$02,$02,$00
        .byte   $C1,$69,$00,$00,$11,$11,$09,$00
        .byte   $00,$0A,$00,$00,$00,$12,$00,$00
        .byte   $81,$55,$81,$00,$01,$01,$01,$00
        .byte   $00,$81,$00,$00,$02,$02,$02,$00
        .byte   $C1,$69,$00,$00,$11,$11,$09,$00
        .byte   $00,$0A,$00,$00,$12,$12,$0A,$00
        .byte   $81,$55,$00,$00,$01,$01,$01,$00
        .byte   $00,$81,$00,$00,$02,$02,$02,$00
        .byte   $C1,$69,$00,$00,$00,$11,$11,$00
        .byte   $00,$0A,$00,$00,$00,$12,$12,$00
        .byte   $81,$55,$00,$00,$01,$01,$01,$00
        .byte   $00,$81,$00,$00,$02,$02,$02,$00
        .byte   $C1,$69,$00,$00,$00,$11,$11,$00
        .byte   $00,$0A,$00,$00,$00,$12,$12,$00
; Opmode
Opmode: .byte   $80,$B1,$BF,$BF,$BF,$B1,$A3,$BF
        .byte   $8B,$B1,$A3,$BF,$BF,$B1,$A3,$BF
        .byte   $9E,$B1,$BF,$BF,$BF,$B1,$A3,$BF
        .byte   $81,$B1,$BF,$BF,$BF,$B1,$A3,$BF
        .byte   $AC,$A2,$BF,$BF,$A4,$A2,$B2,$BF
        .byte   $8D,$A2,$B2,$BF,$A4,$A2,$B2,$BF
        .byte   $9D,$A2,$BF,$BF,$BF,$A2,$B2,$BF
        .byte   $90,$A2,$BF,$BF,$BF,$A2,$B2,$BF
        .byte   $8E,$A9,$BF,$BF,$BF,$A9,$B0,$BF
        .byte   $8A,$A9,$B0,$BF,$AB,$A9,$B0,$BF
        .byte   $9F,$A9,$BF,$BF,$BF,$A9,$B0,$BF
        .byte   $83,$A9,$BF,$BF,$BF,$A9,$B0,$BF
        .byte   $8F,$A1,$BF,$BF,$BF,$A1,$B3,$BF
        .byte   $8C,$A1,$B3,$BF,$AB,$A1,$B3,$BF
        .byte   $A0,$A1,$BF,$BF,$BF,$A1,$B3,$BF
        .byte   $92,$A1,$BF,$BF,$BF,$A1,$B3,$BF
        .byte   $BF,$B5,$BF,$BF,$B7,$B5,$B6,$BF
        .byte   $86,$BF,$96,$BF,$B7,$B5,$B6,$BF
        .byte   $99,$B5,$BF,$BF,$B7,$B5,$B6,$BF
        .byte   $98,$B5,$97,$BF,$BF,$B5,$BF,$BF
        .byte   $AF,$AD,$AE,$BF,$AF,$AD,$AE,$BF
        .byte   $94,$AD,$93,$BF,$AF,$AD,$AE,$BF
        .byte   $9A,$AD,$BF,$BF,$AF,$AD,$AE,$BF
        .byte   $84,$AD,$95,$BF,$AF,$AD,$AE,$BF
        .byte   $A7,$A5,$BF,$BF,$A7,$A5,$A8,$BF
        .byte   $88,$A5,$85,$BF,$A7,$A5,$A8,$BF
        .byte   $9C,$A5,$BF,$BF,$BF,$A5,$A8,$BF
        .byte   $82,$A5,$BF,$BF,$BF,$A5,$A8,$BF
        .byte   $A6,$B4,$BF,$BF,$A6,$B4,$AA,$BF
        .byte   $87,$B4,$89,$BF,$A6,$B4,$AA,$BF
        .byte   $9B,$B4,$BF,$BF,$BF,$B4,$AA,$BF
        .byte   $91,$B4,$BF,$BF,$BF,$B4,$AA,$BF
; ----------------------------------------------------------------------------

CA65_signature:
        .ASCIIZ   "ca65 V0.1 - __DATEBUILT__"


	
; ----------------------------------------------------------------------------
; Copyrights address

        .res $FFF8-*
        .org $FFF8

signature_address:
        .word   CA65_signature

; ----------------------------------------------------------------------------
; Version + ROM Type
ROMDEF: 
        .addr CA65_start





; ----------------------------------------------------------------------------
; RESET
teleass_reset:
        .addr   CA65_start
; ----------------------------------------------------------------------------
; IRQ Vector
teleass_irq_vector:
        .addr   $02FA
end:
