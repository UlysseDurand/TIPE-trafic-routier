simple:
	@pdflatex pres_ulysse.tex

final:
	@pdflatex pres_ulysse.tex
	@pdflatex pres_ulysse.tex
	@make clean

affiche:
	@evince pres_ulysse.pdf &

clean:
	@rm -f *.aux
	@rm -f *.fdb_latexmk
	@rm -f *.fls
	@rm -f *.log
	@rm -f *.lyx
