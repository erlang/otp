# In order to update the figures you have to have both dia
# and imagemagick installed.

DIAGRAMS=$(wildcard *.dia)
EPS_DIAGRAMS=$(patsubst %.dia,%.eps,$(DIAGRAMS))
PNG_DIAGRAMS=$(patsubst %.dia,%.png,$(DIAGRAMS))

diagrams: $(EPS_DIAGRAMS)

png: $(PNG_DIAGRAMS)

update_png: png
	git add $(PNG_DIAGRAMS)
	git commit -m "Update internal docs figures"

%.eps: %.dia
	dia --export=$@ $<

%.png: %.eps
	convert $< -resize 65% $@
