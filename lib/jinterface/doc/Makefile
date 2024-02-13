#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
APPLICATION=jinterface

JAVADOC = javadoc
JAVADOC_PKGS = com.ericsson.otp.erlang
JAVA_PKG_PATH = com/ericsson/otp/erlang
JAVADOC_TITLE = 'Java-Erlang Interface Library'
JAVADOC_DEST = ../doc/assets/java
JAVA_SRC_PATH = $(ERL_TOP)/lib/$(APPLICATION)/java_src/$(JAVA_PKG_PATH)

# don't add filenames to the Makefile!
# all java sourcefiles listed in common include file
include $(JAVA_SRC_PATH)/java_files

JAVA_SRC_FILES = $(JAVA_FILES:%=$(JAVA_SRC_PATH)/%.java)

assets/java/index.html: $(JAVA_SRC_FILES)
	(cd ../java_src;$(JAVADOC) -encoding UTF-8 -sourcepath . -d $(JAVADOC_DEST) \
	   -windowtitle $(JAVADOC_TITLE) $(JAVADOC_PKGS))

$(HTMLDIR)/index.html: assets/java/index.html

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk
