cd src
ghc -o ../haskbot Main
del /s /q /f *.hi
del /s /q /f *.ho
del /s /q /f *.o
haddock -h -o ../docs Main
