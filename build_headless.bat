@echo off
cd "./src/game"
fpc -dUSE_FMOD -dHEADLESS -MDELPHI -O2 -Fi../lib/vampimg -Fi../lib/vampimg/JpegLib -Fi../lib/vampimg/ZLib -Fu../lib/vampimg -Fu../lib/vampimg/JpegLib -Fu../lib/vampimg/ZLib -FE../../bin -FU../../tmp -oDoom2DF_H.exe Doom2DF.dpr
cd ".."
pause