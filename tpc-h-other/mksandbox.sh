#!/bin/bash

cabal sandbox init

cabal sandbox add-source $HOME/work/dev/TableAlgebra
cabal sandbox add-source $HOME/work/dev/x100client
cabal sandbox add-source $HOME/work/dev/dsh
cabal sandbox add-source $HOME/work/dev/Alex.NestedQueries/tpc-h
cabal install --dependencies-only ^C
cabal configure --disable-library-profiling --disable-executable-profiling 

cabal install --dependencies-only --extra-lib-dirs $HOME/software/x100/lib --disable-library-profiling --disable-executable-profiling

