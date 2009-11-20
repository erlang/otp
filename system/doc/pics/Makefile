#
# Copyright (C) 1996,1997 Ericsson Telecommunications
# Author: Lars Thorsen
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/doc

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
#
# Common macros
#

GIF_FILES= \
	min_head.gif \
	ps.gif

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

docs:

pdf:

ps:

debug opt: 



clean:
	@echo "No action" >/dev/null

# 
# Release Targets
#
include $(ERL_TOP)/make/otp_release_targets.mk

ifeq ($(DOCTYPE),pdf)
release_docs_spec: pdf
else
ifeq ($(DOCTYPE),ps)
release_docs_spec: ps
else
release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/pics
	$(INSTALL_DATA) $(GIF_FILES)  $(RELSYSDIR)/pics
endif
endif

release_spec:
