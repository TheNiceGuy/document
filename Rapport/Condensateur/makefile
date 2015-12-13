DEPENDENCIES=graph latex

all: document

document: $(DEPENDENCIES)
	@for dep in $(DEPENDENCIES); do \
		cd $$dep               ;    \
		make                   ;    \
		cd ..                  ;    \
	done

	cp latex/document.pdf ./

clean:
	@for dep in $(DEPENDENCIES); do \
		cd $$dep               ;    \
		make clean             ;    \
		cd ..                  ;    \
	done
