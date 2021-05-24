all: pdf_latex

pdf_latex:
	@pdflatex pres_ulysse_diapo.tex
	@pdflatex pres_ulysse_diapo.tex
	@make clean

affiche:
	@evince pres_ulysse_diapo.pdf &

clean:
	@rm -f *.aux
	@rm -f *.fdb_latexmk
	@rm -f *.fls
	@rm -f *.log
	@rm -f *.lyx
