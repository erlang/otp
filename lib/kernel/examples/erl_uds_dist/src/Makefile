RM=rm -f
CP=cp
EBIN=../ebin
EMULATOR=beam
ERLC=erlc
# Works if building in open source source tree
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/src
ERLCFLAGS+= -W -b$(EMULATOR) -I$(KERNEL_INCLUDE)

MODULES=erl_uds_dist

TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))

opt: $(TARGET_FILES)

$(EBIN)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLCFLAGS) -o$(EBIN) $<

clean:
	$(RM) $(TARGET_FILES)

