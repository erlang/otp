#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(JINTERFACE_VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

SUB_DIRECTORIES = com/ericsson/otp/erlang

SPECIAL_TARGETS = 

TARGET_FILES= $(POM_TARGET)

SPECIAL_TARGETS =

POM_FILE= pom.xml

POM_TARGET= ../$(POM_FILE)
POM_SRC= $(POM_FILE).src

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(POM_TARGET): $(POM_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


include $(ERL_TOP)/make/otp_subdir.mk
