ita:
	cp -p MorphoDictItaAbs.header MorphoDictItaAbs.gf
	cp -p MorphoDictIta.header MorphoDictIta.gf
	python3 get_dict.py Ita tmp/IrregItaAbs.gf tmp/entries | grep -v "lin " >>MorphoDictItaAbs.gf
	python3 get_dict.py Ita tmp/IrregItaAbs.gf tmp/entries | grep -v "fun " >>MorphoDictIta.gf
