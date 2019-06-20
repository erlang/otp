# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2019. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
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
# %CopyrightEnd%
#

APPLICATION ?= $(basename $(notdir $(PWD)))

.PHONY: test info gclean dialyzer dialyzer_plt dclean

test:
	$(ERL_TOP)/make/test_target_script.sh $(ERL_TOP)

info:
	@echo "$(APPLICATION)_VSN:   $(VSN)"
	@echo "APP_VSN:   $(APP_VSN)"
	@echo ""
	@echo "DIA_PLT:      $(DIA_PLT)"
	@echo "DIA_ANALYSIS: $(DIA_ANALYSIS)"
	@echo ""

gclean: 
	git clean -fXd


DIA_DEFAULT_PLT_APPS = erts kernel stdlib $(APPLICATION)
DIA_PLT_DIR  = ./priv/plt
DIA_PLT      = $(DIA_PLT_DIR)/$(APPLICATION).plt
DIA_ANALYSIS = $(basename $(DIA_PLT)).dialyzer_analysis

dialyzer_plt: $(DIA_PLT)

$(DIA_PLT_DIR):
	@mkdir -p $@

$(DIA_PLT): $(DIA_PLT_DIR)
	@echo "Building $(APPLICATION) plt file"
	@$(ERL_TOP)/bin/dialyzer --build_plt \
                  --output_plt $@ \
		  --apps $(sort $(DIA_PLT_APPS) $(DIA_DEFAULT_PLT_APPS)) \
		  --output $(DIA_ANALYSIS) \
                  --verbose

dialyzer: $(DIA_PLT)
	@echo "Running dialyzer on $(APPLICATION)"
	@dialyzer --plt $< \
                  ../$(APPLICATION)/ebin \
                  --verbose

dclean:
	rm -f $(DIA_PLT)
	rm -f $(DIA_ANALYSIS)
