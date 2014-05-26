NAME=report_sglt
all:
	latex $(NAME).tex
	bibtex $(NAME)
	latex $(NAME).tex
	latex $(NAME).tex
	dvips -o $(NAME).ps $(NAME).dvi
	ps2pdf $(NAME).ps

clean:
	rm *.aux *.dvi *.ps *.pdf *.bbl
