#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

SGML_FILES= asn1.sgml

FIG_FILES =   

GIF_FILES= 

PSFIG_FILES= 

USERS_GUIDE  = users_guide.sgml

EXTRA_GEN_FILES= $(SGML_FILES:.sgml=.html) \
	users_guide_frame.html users_guide_first.html \
	  min_head.gif

HTML_FILES= $(USERS_GUIDE:.sgml=.html)

TEX_FILES= $(SGML_FILES:.sgml=.tex) $(USERS_GUIDE:.sgml=.tex)
DVI_FILES= $(USERS_GUIDE:.sgml=.dvi)
PS_FILES= $(USERS_GUIDE:.sgml=.ps)


#
# Make Rules
#
all $(DEFAULT_OPT_TARGETS) $(DEFAULT_DEBUG_TARGETS): $(HTML_FILES) $(TEX_FILES)  $(DVI_FILES) $(PS_FILES) 

clean:
	@rm -f *.toc *.log *.aux *.tex sgmls_output sgmls_errs $(HTML_FILES)  $(EXTRA_GEN_FILES) $(TEX_FILES) $(DVI_FILES) $(PS_FILES) 

# 
# Release Targets
#
include $(OTP_MAKE_ROOT)/otp_release_targets.mk

release_variant: opt.$(TARGET)
	$(MKPATH.$(TARGET)) $(RELEASE_PATH)/$(TARGET)/lib/asn1/doc/users_guide
	$(INSTALLFILES.$(TARGET)) $(HTML_FILES) $(EXTRA_GEN_FILES) $(GIF_FILES) $(PS_FILES) $(RELEASE_PATH)/$(TARGET)/lib/asn1/doc/users_guide


