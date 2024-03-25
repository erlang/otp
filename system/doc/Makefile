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
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
VSN=$(shell cat $(ERL_TOP)/OTP_VERSION)
OTP_REL=$(shell echo $(VSN) | awk -F. '{print $$1}')
EX_DOC_VSN=$(shell cat $(ERL_TOP)/make/ex_doc_vsn)
APPLICATION=Erlang System Documentation
APP_DIR=../../lib/erl_interface
INDEX_DIR=../../../doc
RELSYS_HTMLDIR= $(RELEASE_PATH)/doc/system

# ----------------------------------------------------
# Correctly copy assets
# ----------------------------------------------------
GUIDES=$(shell awk -F: '{print $$1}' guides)
ASSETS_SRC=$(foreach guide, $(GUIDES), $(wildcard $(guide)/assets/*.gif) $(wildcard $(guide)/assets/*.svg))
ASSETS=$(foreach asset, $(ASSETS_SRC), assets/$(notdir $(asset)))

HTML_DEPS=$(foreach guide, $(GUIDES), $(wildcard $(guide)/*.md))

include $(ERL_TOP)/make/doc.mk

## If any asset is changed, we copy all of them
assets.copy: $(ASSETS_SRC)
	$(V_at)cp -f $(ASSETS_SRC) assets/
	$(V_at)touch $@

# ----------------------------------------------------
# Copy and update HOWTOs
# ----------------------------------------------------
installation_guide/%.md: ../../HOWTO/%.md ../../OTP_VERSION ../../make/ex_doc_vsn
	$(gen_verbose)sed \
          -e 's:%EX_DOC_VSN%:$(EX_DOC_VSN):g' \
          -e 's:%OTP-VSN%:$(VSN):g' \
          -e 's:%OTP-REL%:$(OTP_REL):g' \
          $< > $@

# ----------------------------------------------------
# Generate deprecations etc
# ----------------------------------------------------
DEPRECATIONS_SCRIPT = $(ERL_TOP)/lib/stdlib/scripts/update_deprecations
GENERATED_FILES = general_info/deprecations.md general_info/scheduled_for_removal.md general_info/removed.md

general_info/deprecations.md: general_info/DEPRECATIONS $(wildcard general_info/deprecations_*.md) $(DEPRECATIONS_SCRIPT)
	$(gen_verbose)escript $(DEPRECATIONS_SCRIPT) make_markdown deprecations $(ERL_TOP) $@

general_info/scheduled_for_removal.md: general_info/DEPRECATIONS $(wildcard general_info/scheduled_for_removal_*.md) $(DEPRECATIONS_SCRIPT)
	$(gen_verbose)escript $(DEPRECATIONS_SCRIPT) make_markdown scheduled_for_removal $(ERL_TOP) $@

general_info/removed.md: general_info/DEPRECATIONS $(wildcard general_info/removed_*.md) $(DEPRECATIONS_SCRIPT)
	$(gen_verbose)escript $(DEPRECATIONS_SCRIPT) make_markdown removed $(ERL_TOP) $@

# ----------------------------------------------------
# Add some extra html dependencies
# ----------------------------------------------------
$(HTMLDIR)/index.html: $(GENERATED_FILES) guides \
	installation_guide/OTP-PATCH-APPLY.md \
	installation_guide/INSTALL.md \
	installation_guide/INSTALL-CROSS.md \
	installation_guide/INSTALL-WIN32.md \
	assets.copy
