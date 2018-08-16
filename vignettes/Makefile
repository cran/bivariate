default:
	make bivariate.pdf
	make removeother

bivariate.pdf: bivariate.tex
	pdflatex bivariate.tex

removeother:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f temp*
	rm -f Rplots*
