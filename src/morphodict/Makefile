ita:
	cp -p MorphoDictItaAbs.header MorphoDictItaAbs.gf
	cp -p MorphoDictIta.header MorphoDictIta.gf
	python3 get_dict.py | grep -v "lin " >>MorphoDictItaAbs.gf
	python3 get_dict.py | grep -v "fun " >>MorphoDictIta.gf
