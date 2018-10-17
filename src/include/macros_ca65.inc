.macro BRK_ORIX value
	.byte $00,value
.endmacro

.macro MALLOC size
    lda #<size
    ldy #>size
    BRK_ORIX XMALLOC
.endmacro

.macro PRINT str
    lda #<str
    ldy #>str
    BRK_ORIX XWSTR0
.endmacro

.macro FOPEN file,mode
  lda   #<file
  ldx   #>file
  ldy   #mode
  BRK_ORIX XOPEN
.endmacro  
  
.macro MKDIR PATH 
  lda   #<PATH
  ldx   #>PATH
  BRK_ORIX XMKDIR
.endmacro

.macro FREE ptr
  lda   ptr
  ldy   ptr+1
  BRK_ORIX XFREE
.endmacro
