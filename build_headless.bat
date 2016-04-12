@echo off
cd "./src/game"
fpc -dUSE_FMOD -dHEADLESS -MDELPHI -O2 -FE../../bin -FU../../tmp -oDoom2DF_H.exe Doom2DF.dpr
cd ".."
pause