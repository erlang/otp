# ``Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(EDOC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

APPNAME=edoc
DOC_TITLE="Welcome to EDoc"

HTML_FILES = *.html
INFO_FILE = ../info
# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

# ----------------------------------------------------
# Targets
# ----------------------------------------------------


docs:
	(cd ..; \
	edoc_generate -app '$(APPNAME)' -vsn '$(VSN)')


info:
	@echo "HTML_FILES:" $(HTML_FILES)
	@echo "HTMLDIR: $(HTMLDIR)"



debug opt: 


clean:
	rm -f  $(HTML_FILES) stylesheet.css edoc-info
	rm -f errs core *~


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 


include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA)  $(HTML_FILES) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)


release_spec:
