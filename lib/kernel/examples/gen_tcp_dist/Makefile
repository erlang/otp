RM=rm -f
CP=cp
EBIN=ebin
ERLC=erlc
# Works if building in open source source tree
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/include
ERLCFLAGS+= -W -I$(KERNEL_INCLUDE)

MODULES=gen_tcp_dist

TARGET_FILES=$(MODULES:%=$(EBIN)/%.beam)

opt: $(TARGET_FILES)

$(EBIN)/%.beam: src/%.erl
	$(ERLC) $(ERLCFLAGS) -o$(EBIN) $<

clean:
	$(RM) $(TARGET_FILES)

