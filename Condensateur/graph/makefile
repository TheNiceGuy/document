OBJECTS=graph.dep

all: $(OBJECTS)

%.dep: %.r
	Rscript $<

clean:
	rm -f *.pdf
	rm -f *.png
