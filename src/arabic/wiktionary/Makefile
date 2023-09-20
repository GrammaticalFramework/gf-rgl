all:
	python3 read_wiktionary.py gf-abs >MorphoDictAraAbs.gf
	python3 read_wiktionary.py gf-cnc >MorphoDictAra.gf
	python3 read_wiktionary.py gf-map >source_of_MorphoDictAra.jsonl 
	gf -make MorphoDictAra.gf
	python3 read_wiktionary.py eval-funs >1-eval.txt 
	python3 to_wordnet.py >wornet-arabic.jsonl
