RM=rm -f
CP=cp
EBIN=ebin
ERLC=erlc
ERLCFLAGS+= -W

MODULES=gen_tcp_dist

TARGET_FILES=$(MODULES:%=$(EBIN)/%.beam)

opt: $(TARGET_FILES)

$(EBIN)/%.beam: src/%.erl
	$(ERLC) $(ERLCFLAGS) -o$(EBIN) $<

clean:
	$(RM) $(TARGET_FILES)

