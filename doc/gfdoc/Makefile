TMP=tmp.html
TEMPLATE=../synopsis/template.html
ROOT=../..

sources.html: sources.txt $(TEMPLATE)
	txt2tags --target=html --no-headers --quiet --toc --outfile=$@ --infile=$<
	pandoc \
		--from=html \
		--to=html5 \
		--standalone \
		--template=$(TEMPLATE) \
		--metadata='title:"More modules in the GF Resource Grammar Library"' \
		--variable='rel-root:$(ROOT)/..' \
		--output=$(TMP) \
		$@
	mv $(TMP) $@
	sed -i.bak "s/<table>/<table class=\"table w-auto\">/" $@ && rm "$@.bak"
