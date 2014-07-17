cabal sandbox delete
cabal sandbox init
cabal sandbox add-source $HOME/work/dev/TableAlgebra
cabal sandbox add-source $HOME/work/dev/x100client
cabal sandbox add-source $HOME/work/dev/dsh

cabal install --dependencies-only --extra-lib-dirs $HOME/software/x100/lib

