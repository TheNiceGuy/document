OBJECTS=document.pdf

all: $(OBJECTS)

document.pdf: document.tex
	pdflatex -shell-escape document.tex
	bibtex document.aux
	pdflatex -shell-escape document.tex
	pdflatex -shell-escape document.tex

clean:
	rm -f *.pdf
	rm -f *.log
	rm -f *.aux
	rm -f *.swp
	rm -f *.blg
	rm -f *.bbl
