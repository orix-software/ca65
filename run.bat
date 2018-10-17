@echo off

SET ORICUTRON="..\..\..\oricutron\"

SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%


%CC65%\ca65.exe -ttelestrat --include-dir %CC65%\asminc\ src/ca65.asm -o ca65.ld65
%CC65%\ld65.exe -tnone  ca65.ld65 -o ca65.rom



IF "%1"=="NORUN" GOTO End

copy ca65.rom %ORICUTRON%\roms\orixbank2.rom > NUL

cd %ORICUTRON%
oricutronV11 -mt  --symbols "%ORIGIN_PATH%\xa_labels_orix.txt"

:End
cd %ORIGIN_PATH%
%OSDK%\bin\MemMap "%ORIGIN_PATH%\xa_labels_orix.txt" memmap.html O docs/telemon.css

