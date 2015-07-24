DEPENDENCIES=graph figure latex

all: document

document: graph figure
	@for dep in $(DEPENDENCIES); do \
		cd $$dep          ;    \
		make              ;    \
		cd ..             ;    \
	done

	cp latex/document.pdf ./

clean:
	@for dep in $(DEPENDENCIES); do \
		cd $$dep          ;    \
		make clean        ;    \
		cd ..             ;    \
	done
