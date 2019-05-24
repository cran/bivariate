default:
	make bivariate.pdf
	make remove_other

bivariate.pdf: bivariate.tex
	pdflatex bivariate.tex

remove_other:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f temp*
	rm -f Rplots*
