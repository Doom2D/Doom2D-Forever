@echo off
cd "./src/game"
fpc -dUSE_FMOD -MDELPHI -O2 -FE../../bin -FU../../tmp Doom2DF.dpr
cd ".."
pause