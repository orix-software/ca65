@echo off

SET ORICUTRON="..\..\..\oricutron-iss\"

SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%


%CC65%\ca65.exe -ttelestrat --include-dir %CC65%\asminc\ src/ca65.asm -o ca65.ld65
%CC65%\ld65.exe -tnone  ca65.ld65 -o cc65.rom



IF "%1"=="NORUN" GOTO End

copy cc65.rom %ORICUTRON%\roms\ > NUL

cd %ORICUTRON%
oricutron -mt  --symbols "%ORIGIN_PATH%\xa_labels_orix.txt"

:End
cd %ORIGIN_PATH%
%OSDK%\bin\MemMap "%ORIGIN_PATH%\xa_labels_orix.txt" memmap.html O docs/telemon.css

