.PHONY: all status synopsis abstract

GFDOC=gfdoc
S=../src

all: synopsis

status: status.html

synopsis:
	make -C synopsis

status.html:
	txt2tags -thtml status.txt

abstract:
	$(GFDOC) -txthtml $S/abstract/*.gf
	mv $S/abstract/*.html abstract
