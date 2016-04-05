@echo off
cd "./src/game"
fpc -MDELPHI -O2 -FE../../bin -FU../../tmp Doom2DF.dpr
cd ".."
pause