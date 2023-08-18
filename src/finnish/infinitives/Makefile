all: pgf hs

pgf:
	gf --make -output-format=haskell -haskell=lexical -lexical=N,A,Adv,V,V2,VS,VV,PN --haskell=gadt InfinitiveFin.gf

hs:
	ghc --make InterpretInfinitives.hs
