EBIN=../../ebin
INCLUDES=../../include
SOURCES=merl_build.erl lisp.erl lispc.erl basic.erl basicc.erl
HEADERS=$(INCLUDES)/merl.hrl
OBJECTS=$(SOURCES:%.erl=%.beam)
ERLC_FLAGS=+debug_info -I$(INCLUDES) -pa $(EBIN)

all: $(OBJECTS) test

%.beam: %.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ./ $<

# additional dependencies due to the parse transform
lispc.beam basicc.beam: $(EBIN)/merl_transform.beam $(EBIN)/merl.beam

clean:
	-rm -f $(OBJECTS)

test:
	erl -noshell -pa $(EBIN) \
	 -eval 'eunit:test([lisp, lispc, basic, basicc],[])' \
	 -s init stop
