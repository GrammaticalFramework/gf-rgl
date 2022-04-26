# A wrapper over the Haskell and Bash build scripts
# Will try and fallback in order:
#   - runghc Setup.hs
#   - stack runghc Setup.hs
#   - ./Setup.sh

ifneq (, $(shell which runghc))
RUNGHC=runghc Setup.hs
else ifneq (, $(shell which stack))
RUNGHC=stack runghc Setup.hs
else
RUNGHC=
endif

.PHONY: build copy install doc clean

default: build copy

build: src/*/*.gf
ifneq (, $(RUNGHC))
	$(RUNGHC) build
else
	./Setup.sh
endif

copy:
ifneq (, $(RUNGHC))
	$(RUNGHC) copy
endif

install: build copy

doc: build
	make -C doc GF_LIB_PATH=../dist

clean:
ifneq (, $(RUNGHC))
	$(RUNGHC) clean
else
	rm -r dist
endif
