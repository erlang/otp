# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2014. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
include $(APP_PWD)/vsn.mk
include $(ERL_TOP)/make/otp_default_release_path.mk

RELEASED_APP_VSN=$(APP)-$($(APP_VSN))
ifeq ($(TESTROOT),)
REL_DIR=$(OTP_DEFAULT_RELEASE_PATH)/releases/$(SYSTEM_VSN)
else
REL_DIR=$(TESTROOT)/releases/$(SYSTEM_VSN)
endif
INST_APP_VSNS=$(REL_DIR)/installed_application_versions

.PHONY: update

update:
	test -d "$(REL_DIR)" || mkdir -p "$(REL_DIR)" ;			\
	if test ! -f "$(INST_APP_VSNS)" ; then				\
	 echo "$(RELEASED_APP_VSN)" > "$(INST_APP_VSNS)" || exit 1;	\
	else								\
	 if test x = x`grep $(RELEASED_APP_VSN) "$(INST_APP_VSNS)"` ; then \
	  echo $(RELEASED_APP_VSN) >> "$(INST_APP_VSNS)" || exit 1;	\
	 fi ;								\
	fi

