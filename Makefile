# A simple wrapper over the Haskell-based RGL build script

RUNMAKE=runghc Make.hs

.PHONY: build copy install doc clean

default: build copy

build: src/*/*.gf
	$(RUNMAKE) build

copy:
	$(RUNMAKE) copy

install: build copy

doc: build
	make -C doc GF_LIB_PATH=../dist

clean:
	$(RUNMAKE) clean
