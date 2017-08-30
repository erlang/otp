#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#
OTP_MAKE_ROOT=/home/super/otp/otp_make
include $(OTP_MAKE_ROOT)/otp.mk

#
# Release Macros
#

#
# Common macros
#


SGML_FILES= release_notes.sgml

FIG_FILES = 
HTML_FILES= $(SGML_FILES:.sgml=.html)
TEX_FILES= $(SGML_FILES:.sgml=.tex)
DVI_FILES= $(SGML_FILES:.sgml=.dvi)
PSFIG_FILES= $(FIG_FILES:.fig=.ps) 
PS_FILES= $(SGML_FILES:.sgml=.ps)
GIF_FILES= min_head.gif
ERL_FILES=
HRL_FILES=
DATA_FILES=


#
# Make Rules
#

all $(DEFAULT_OPT_TARGETS) $(DEFAULT_DEBUG_TARGETS): $(HTML_FILES) $(TEX_FILES) $(PSFIG_FILES) $(DVI_FILES) $(PS_FILES)

clean:
	@rm -f *.toc *.log *.aux *.tex sgmls_output sgmls_errs $(HTML_FILES) $(TEX_FILES) $(DVI_FILES) $(PSFIG_FILES) $(PS_FILES)

# 
# Release Targets
#
include $(OTP_MAKE_ROOT)/otp_release_targets.mk

release_variant: opt.$(TARGET)
	$(MKPATH.$(TARGET)) $(RELEASE_PATH)/$(TARGET)/lib/mnesia-$(VSN)/doc/misc
	$(INSTALLFILES.$(TARGET)) $(HTML_FILES) $(GIF_FILES) $(ERL_FILES) $(HRL_FILES) $(DATA_FILES) $(PS_FILES) $(RELEASE_PATH)/$(TARGET)/lib/mnesia-$(VSN)/doc/misc

