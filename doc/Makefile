.PHONY: all status synopsis abstract sources

GFDOC=gfdoc
S=../src

all: synopsis

status: status.html

synopsis:
	make -C synopsis GF_LIB_PATH=../$(GF_LIB_PATH)

sources:
	make -C gfdoc sources.html

status.html:
	txt2tags -thtml status.txt

abstract:
	$(GFDOC) -txthtml $S/abstract/*.gf
	mv $S/abstract/*.html abstract
