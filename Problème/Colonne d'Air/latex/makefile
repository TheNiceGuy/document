OBJECTS=document.pdf

all: $(OBJECTS)

%.pdf: %.tex
	pdflatex -shell-escape $<

clean:
	rm -f *.pdf
	rm -f *.log
	rm -f *.aux
	rm -f *.swp
