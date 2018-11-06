.PHONY: all index status synopsis abstract

all: synopsis

GF_alltenses=$(GF_LIB_PATH)/alltenses
GF=gf
GFDOC=gfdoc

index:
	txt2tags -thtml index.txt
status:
	txt2tags -thtml status.txt

synopsis: synopsis.html

S=../src

# List of languages extracted from languages.csv, with 'Synopsis' column == y
LANGS=$(shell cat ../languages.csv | cut -d',' -f1,10 | grep ',y' | cut -d',' -f1)

# This list was constructed by observing what files MkSynopsis.hs reads
SRC_FILES=$(S)/abstract/Common.gf $(S)/abstract/Cat.gf $(S)/api/Constructors.gf $(S)/abstract/Structural.gf $(patsubst %,$S/*/Paradigms%.gf,$(LANGS))

EXAMPLES_OUT=$(patsubst %,api-examples-%.txt,$(LANGS))
INCLUDES=synopsis-intro.txt categories-intro.txt categories-imagemap.html synopsis-additional.txt synopsis-browse.txt synopsis-example.txt

synopsis.html: MkSynopsis.hs MkExxTable.hs $(INCLUDES) $(EXAMPLES_OUT) $(SRC_FILES)
	runghc -i.. MkSynopsis.hs

categories.png: categories.dot
	dot -Tpng $^ > $@

categories-imagemap.html: categories.dot
	dot -Tcmapx $^ > $@

abstract:
	$(GFDOC) -txthtml $S/abstract/*.gf
	mv $S/abstract/*.html abstract

api-examples.gfs: api-examples.txt MkExx.hs
	runghc MkExx.hs < $< > $@

# Since .gfo files aren't self-contained, the dependencies given here are
# incomplete. But I am thinking that the Try%.gfo file will always be newer
# than any other files it depends on, so the rule will trigger when
# needed anyway. //TH 2018-10-22
api-examples-%.txt: $(GF_alltenses)/Try%.gfo api-examples.gfs
	GF_LIB_PATH=$(GF_LIB_PATH) $(GF) -retain -s $< <api-examples.gfs >$@

clean:
	rm -rf synopsis.txt api-examples.gfs $(EXAMPLES_OUT)
