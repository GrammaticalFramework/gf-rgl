all: lrecslides-html

lrecslides-tex:
	txt2tags -ttex gf-lrec-2010.txt
	cat prelude gf-lrec-2010.tex >tmp.tex
	mv tmp.tex gf-lrec-2010.tex
	pdflatex gf-lrec-2010.tex

lrecslides-html:
	txt2tags -thtml gf-lrec-2010.txt
