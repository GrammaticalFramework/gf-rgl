TMP=tmp.html
TEMPLATE=../synopsis/template.html
ROOT=../..

TITLE=$(shell head -n 1 sources.txt)
sources.html: sources.txt $(TEMPLATE)
	txt2tags --target=html --no-headers --quiet --outfile=$@ --infile=$<
	pandoc \
		--from=html \
		--to=html5 \
		--standalone \
		--template=$(TEMPLATE) \
		--metadata='title:"$(TITLE)"' \
		--variable='lang:en' \
		--variable='rel-root:$(ROOT)/..' \
		--output=$(TMP) \
		$@
	mv $(TMP) $@
	sed -i.bak "s/<table>/<table class=\"table w-auto\">/" $@ && rm "$@.bak"
