OBJECTS=champ_E_dipole.eps champ_E_plaques.eps

all: $(OBJECTS)

%.eps: %.svg
	inkscape --export-area-page -f $< -E $@

clean:
	rm -f $(OBJECTS)
	rm -f *.pdf
