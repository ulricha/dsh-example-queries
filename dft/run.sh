#!/bin/sh

rm *.vwq
ghc --make dft.hs
./dft
vldot -i dftfast_1024_vl.plan | dot -Tpdf -o vl.pdf
x100dot -i dftfast_1024_x100.plan | dot -Tpdf -o x100.pdf
x100gen -i dftfast_1024_x100.plan
