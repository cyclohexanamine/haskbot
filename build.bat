cd src
ghc -o ../haskbot Main
REM del /s /q /f *.hi
REM del /s /q /f *.ho
REM del /s /q /f *.o
haddock -h -o ../docs Main
cd ..
