# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2014. All Rights Reserved.
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

