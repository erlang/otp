# Example makefile

RM=rm -f
CP=cp
EBIN=../ebin
EMULATOR=beam
ERLC=erlc
# Works if building in open source source tree
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/src
ERLCFLAGS+= -W -b$(EMULATOR) -I$(KERNEL_INCLUDE)
APP=uds_dist.app

MODULES=uds_server uds uds_dist

TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))

opt: $(TARGET_FILES) $(EBIN)/$(APP) 

$(EBIN)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLCFLAGS) -o$(EBIN) $<

$(EBIN)/$(APP): $(APP)
	$(CP) $(APP) $(EBIN)/$(APP)

clean:
	$(RM) $(TARGET_FILES) $(EBIN)/$(APP)

