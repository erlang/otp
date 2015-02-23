EBIN_D     = $(EBIN_DIR)/shootout
ERL_FILES  = $(wildcard *.erl)
BEAM_FILES = $(patsubst %.erl,$(EBIN_D)/%.beam,$(ERL_FILES))

.PHONY: all clean

all: $(BEAM_FILES) fasta

$(EBIN_D)/%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o $(EBIN_D) $<

$(BEAM_FILES): | $(EBIN_D)

$(EBIN_D):
	mkdir -p $(EBIN_D)

fasta: fasta.c
	gcc -O3 -o fasta fasta.c

clean:
	$(RM) -rf $(EBIN_D) fasta *-input.txt
