.importzp  sp

.include   "telestrat.inc"
.include   "fcntl.inc"
.include   "cpu.mac"

.include   "include/macros_ca65.inc"
; %CC65%\ca65.exe -ttelestrat --include-dir %CC65%\asminc\ src/ca65.asm -o ca65.ld65
userzp := $80 ; FIXME

sizeof_file_max = 1000



ca65_src_code_ptr     := userzp
ca65_ptr1             := userzp+4
ca65_struct           := userzp+6
ca65_ptr2             := userzp+8
ca65_tmp1             := userzp+10
ca65_tmp2             := userzp+11

.struct ca65_infos
        length_file             .word 
        src_filename            .res 14
        ca65_tmp                .res 1
        ca65_global_labels      .res 1000
        ca65_binary             .res 1000
        ca65_source_code        .res 16000
.endstruct 

.org $c000

.code
CA65_start:
        lda     #<.sizeof(ca65_infos)   ; Malloc the struct
        ldy     #>.sizeof(ca65_infos)

        BRK_ORIX XMALLOC                ; launch malloc
        cmp     #$00                    ; is it null ?
        bne     not_oom_for_struct      ; no malloc works
        cpy     #$00                    ; is it null ? 16 bits test must be done, because else we could get from malloc a $XX00 adress which could announce a failed malloc
        bne     not_oom_for_struct      ; not null let's continue
        PRINT   str_OOM                 ; null reached display OOM
        PRINT   str_for_ca_struct       ; and which malloc
        BRK_ORIX XCRLF                  ; return malloc
        ; oom                           ; and exit
        rts        
not_oom_for_struct:
        sta     ca65_struct             ; store the adress
        sty     ca65_struct+1           ; in zero page

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
        BRK_ORIX XCRLF
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
        cmp     #' '
        bne     check_control_command
        inc     ca65_ptr1
        bne     skip_don_t_inc
        inc     ca65_ptr1+1
skip_don_t_inc:     
        jmp     new_line
check_control_command:        
        cmp     #'.'
        beq     start_control_command
        rts

start_control_command:
        jsr     _control_command
        rts



.proc _control_command
        line_to_parse:=ca65_ptr1                        ; if we entered here, we reached a '.'
        current_control_command_str:=ca65_ptr2

        inc     line_to_parse
        bne     skip2
        inc     line_to_parse+1
skip2:
        ldx     #$00
skip:
        lda     controlcommand_addr_low,x
        sta     current_control_command_str
        lda     controlcommand_addr_high,x
        sta     current_control_command_str+1
        ldy     #$FF
loop:   
        iny
        lda     (current_control_command_str),y
        cmp     #' '                                   ; do we reached a space ?
        beq     found
        cmp     #$80
        bcc     not_128
        
        lda     (line_to_parse),y                      ; do we reach control command ? test 
        sta     $bb80+80,y
        ora     #%10000000                             ; add line to parse
        cmp     (current_control_command_str),y
        bne     not_128
        ; checking if we have a space next the command ...
        iny
        lda     (line_to_parse),y
        cmp     #' '
        beq     found
        dey

not_128:      
        sta     $bb80,y
       
        
continue:        
      ;  jsr     display_char
      ;  beq     outrout
;        sty     ca65_tmp1
        pha
        tya
        pha
        txa
        pha
        lda    (line_to_parse),y
        BRK_ORIX        XWR0
        pla
        tax
        pla
        tay
        pla

        cmp     (line_to_parse),y
        bne     out
        beq     loop
out:
        txa
        pha
        sty     ca65_tmp1
        lda     (line_to_parse),y
        BRK_ORIX        XWR0
        BRK_ORIX        XCRLF        
        pla
        tax        
        inx
        cpx     #$06 ; FIXME number of command
        bne     skip
outrout:
        ;       save Y
        sty     ca65_tmp1
        lda     ca65_struct+1
        sta     RESB+1

        lda     #<ca65_infos::src_filename
        clc 
        adc     ca65_struct
        bcc     skipme
        inc     RESB+1
skipme:    
        ldy     RESB+1
        BRK_ORIX        XWSTR0


        lda     #'('
        BRK_ORIX        XWR0
        lda     #')'
        BRK_ORIX        XWR0

        PRINT   str_error
        lda     #'.'
        BRK_ORIX        XWR0        
        ldy     #$00
loopme:
        ;lda     (line_to_parse),y
        ;iny
        sty     ca65_tmp2
        lda     (line_to_parse),y
        cmp     #' '
        beq     outme
        BRK_ORIX        XWR0
        ldy     ca65_tmp2
        iny
        dec     ca65_tmp1
        bne     loopme
outme:



        PRINT   str_not_recognized_control_command
        rts
found:
        and     #%01111111
        dey
        sta     $bb80,y
        ; at this step x contains the id of the control command
   ;     iny
    ;    lda    (line_to_parse),y        ; control command is found, but is there a space after the control command ?
     ;   cmp    #' '
      ;  beq     control_command_is_found
        ; at this step there is a "." at the beginning of the line, but we did not found the control command !
;.imdff        
       ; PRINT str_not_recognized_control_command
;        rts
control_command_is_found:
        ;stx     ca65_tmp1
        BRK_ORIX        XCRLF
        PRINT str_control_command
        ;ldx     ca65_tmp1
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
str_error:
        .ASCIIZ "Error : '"
str_not_recognized_control_command:
        .byte "' is not a recognized control command",$0D,$0A,0


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
        .byte 't'+$80
controlcommand_byte:
        .byte "byt"
        .byte 'e'+$80
controlcommand_import:
        .byte "impor"
        .byte 't'+$80      
controlcommand_importzp:
        .byte "importz"
        .byte 'p'+$80
controlcommand_incbin:
        .byte "incbi"
        .byte 'n'+$80           
controlcommand_include:
        .byte "includ"
        .byte 'e'+$80
controlcommand_proc:
        .byte "pro"
        .byte 'c'+128        

controlcommand_addr_low:
        .byte   <controlcommand_byt
        .byte   <controlcommand_byte
        .byte   <controlcommand_import
        .byte   <controlcommand_importzp
        .byte   <controlcommand_incbin
        .byte   <controlcommand_include        

controlcommand_addr_high:
        .byte   >controlcommand_byt
        .byte   >controlcommand_byte
        .byte   >controlcommand_import
        .byte   >controlcommand_importzp
        .byte   >controlcommand_incbin
        .byte   >controlcommand_include

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
_ca65:
        rts
_cc65:
        rts
_ld65:
        rts
_cl65:
        rts


list_commands:
        .ASCIIZ "ca65"
        .ASCIIZ "cc65"
        .ASCIIZ "cl65"
        .ASCIIZ "ld65"
vector_commands:
        .word _ca65
        .word _cc65        
        .word _cl65                
        .word _ld65



signature_commands:
        .byte CPU_6502 ; ca65
        .byte CPU_6502 ; cc65
        .byte CPU_6502 ; cl65
        .byte CPU_6502 ; ld65
; ----------------------------------------------------------------------------
; Copyrights address
CA65_signature:
        .byte   "cc65 V0.3 - "
        .byte   "__DATEBUILD__"
        .byte   0

        .res $FFF1-*
        .org $FFF1
number_of_commands:
        .byte 4        
signature_command_address:
        .word   signature_commands
command_address:
        .word   list_commands
vector_address:
        .word   vector_commands
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
