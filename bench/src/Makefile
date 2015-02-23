#DIRS = $(wildcard */)
DIRS = shootout

.PHONY: all clean

all:
	for d in $(DIRS); do (cd $$d && make ERLC=$(ERLC) ERL_COMPILE_FLAGS="$(ERL_COMPILE_FLAGS)" EBIN_DIR=../$(EBIN_DIR) $@); \
	done

clean:
	for d in $(DIRS); do (cd $$d && make EBIN_DIR=../$(EBIN_DIR) $@); done
