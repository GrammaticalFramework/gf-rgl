# Your GF_LIB_PATH must be set in order for this build script to work

.PHONY: all index clean

GF_alltenses=$(GF_LIB_PATH)/alltenses
GF=gf
GFDOC=gfdoc

ROOT=../..
S=$(ROOT)/src
CONFIG=$(ROOT)/languages.csv

# List of languages extracted from languages.csv, with 'Synopsis' column == y
LANGS=$(shell cat $(CONFIG) | cut -d',' -f1,11 | grep ',y' | cut -d',' -f1)

# This list was constructed by observing what files MkSynopsis.hs reads
SRC_FILES=$(S)/abstract/Common.gf $(S)/abstract/Cat.gf $(S)/api/Constructors.gf $(S)/abstract/Structural.gf $(patsubst %,$S/*/Paradigms%.gf,$(LANGS))

EXAMPLES_OUT=$(patsubst %,api-examples-%.txt,$(LANGS))
INCLUDES=intro.txt categories-intro.txt categories-imagemap.html categories.png additional.txt browse.txt example.txt

TMP=tmp.html
TEMPLATE=template.html

all: index

index: index.html

index.txt: MkSynopsis.hs MkExxTable.hs $(INCLUDES) $(EXAMPLES_OUT) $(SRC_FILES)
	runghc -i$(ROOT) MkSynopsis.hs

index-t2t.html: index.txt
	txt2tags --target=html --no-headers --quiet --toc --outfile=$@ --infile=$<
	sed -i.bak "s/<A NAME=\"\(.*\)\"><\/A>/<div id=\"\1\"><\/div>/" $@
	rm "$@.bak"

TITLE=$(shell head -n 1 index.txt)
index.html: index-t2t.html $(TEMPLATE)
	pandoc \
		--from=html \
		--to=html5 \
		--standalone \
		--template=$(TEMPLATE) \
		--metadata='title:"$(TITLE)"' \
		--variable='lang:en' \
		--variable='rel-root:$(ROOT)/..' \
		--css="synopsis.css" \
		--include-after-body="quicklinks.html" \
		--output=$(TMP) \
		index-t2t.html
	mv $(TMP) $@
	sed -i.bak "s/<table>/<table class=\"table w-auto\">/" $@
	sed -i.bak -e '/img src="categories.png"/r categories-imagemap.html' -e '/img src="categories.png"/d' $@
	rm "$@.bak"

categories.png: categories.dot
	dot -Tpng $^ > $@

categories-imagemap.html: categories.dot
	rm -f $@
	echo '<img src="categories.png" usemap="#categoriesmap" alt="RGL categories">' > $@
	dot -Tcmapx $^ >> $@

api-examples.gfs: api-examples.txt MkExx.hs
	runghc MkExx.hs < $< > $@

# Since .gfo files aren't self-contained, the dependencies given here are
# incomplete. But I am thinking that the Try%.gfo file will always be newer
# than any other files it depends on, so the rule will trigger when
# needed anyway. //TH 2018-10-22
api-examples-%.txt: $(GF_alltenses)/Try%.gfo api-examples.gfs
	GF_LIB_PATH=$(GF_LIB_PATH) $(GF) -retain -s $< <api-examples.gfs >$@

clean:
	rm -rf \
		index.txt \
		index.html \
		api-examples.gfs \
		$(EXAMPLES_OUT)
