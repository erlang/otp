ERLC = erlc
EMULATOR = beam
EBIN = .
HTML = .
XML = ./xml
RM = rm -f

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include ../../vsn.mk
VSN = $(XMERL_VSN)


MODULES = \
	xmerl_test \
	test_html \
	xserl_test \
        mkdocs \
        sdocbook2xhtml



DOC_FILES = $(DOCS:%=$(HTML)/%.html) 

ERL_COMPILE_FLAGS += $(DEBUG) -I ../../include +warn_unused_wars +debug_info

SUB_DIRECTORIES =

#all:   $(MODULES:%=$(EBIN)/%.$(EMULATOR)) xsm $(DOC_FILES)
all opt:   $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(DOC_FILES)

info:
	@echo "DOC_FILES: $(DOC_FILES)"

clean:
#	@for d in $(SUB_DIRECTORIES); do \
#	  	cd $$d; $(MAKE) clean; \
#	done
	$(RM) $(HTML)/*.html
	$(RM) $(EBIN)/*.beam
	$(RM) core *~

debug:  xsmdebug $(DOC_FILES)

xsm:
	@for d in $(SUB_DIRECTORIES); do \
	  	cd $$d; $(MAKE); \
	done

xsmdebug:
	@for d in $(SUB_DIRECTORIES); do \
	  	cd $$d; $(MAKE) DEBUG=-Ddebug=1; \
	done

$(HTML)/%.html:	$(XML)/%.xml
	erl -noshell -pa ../../ebin -run mkdocs run $< $@ -s erlang halt

$(EBIN)/%.beam:	%.erl
	$(ERLC)  $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

