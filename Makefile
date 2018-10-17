AS=ca65
CC=cl65
LD=ld65
CFLAGS=-ttelestrat
LDFILES=
CA65_ROM=ca65

all : build
.PHONY : all

HOMEDIR=/home/travis/bin/
HOMEDIR_ORIX=/home/travis/build/orix-software/ca65
ORIX_VERSION=1.0

SOURCE=src/ca65.asm

TELESTRAT_TARGET_RELEASE=release/telestrat
MYDATE = $(shell date +"%Y-%m-%d %H:%m")
 
build: $(SOURCE_BANK5)
	$(AS) $(CFLAGS) $(SOURCE) -o ca65.ld65
	$(LD) -tnone ca65.ld65 -o ca65.rom

test:
	echo hello
  

