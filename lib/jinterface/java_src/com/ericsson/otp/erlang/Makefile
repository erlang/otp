# -*-Makefile-*-

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

JAVA_DEST_ROOT = $(ERL_TOP)/lib/jinterface/priv/
JAVA_SRC_ROOT = $(ERL_TOP)/lib/jinterface/java_src/
JAVA_CLASS_SUBDIR = com/ericsson/otp/erlang/

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include $(ERL_TOP)/lib/jinterface/vsn.mk
VSN=$(JINTERFACE_VSN)

#

EBINDIR=$(ERL_TOP)/lib/jinterface/ebin

APP_FILE= jinterface.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBINDIR)/$(APP_FILE)

APPUP_FILE= jinterface.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBINDIR)/$(APPUP_FILE)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/jinterface-$(VSN)

#
# JAVA macros
#

# don't add filenames to the Makefile!
# all java sourcefiles listed in common include file
include $(ERL_TOP)/lib/jinterface/java_src/$(JAVA_CLASS_SUBDIR)/java_files

TARGET_FILES= $(JAVA_FILES:%=$(JAVA_DEST_ROOT)$(JAVA_CLASS_SUBDIR)%.class) $(APP_TARGET) $(APPUP_TARGET)
JAVA_SRC= $(JAVA_FILES:%=%.java)

JARFILE= OtpErlang.jar


# ----------------------------------------------------
# Programs and Flags
# ----------------------------------------------------

JAR= jar

CLASSPATH = $(JAVA_SRC_ROOT)

JAVADOCFLAGS=-d $(DOCDIR)
JAVAFLAGS=-d $(JAVA_DEST_ROOT) 
JARFLAGS=-cf
ifneq ($(V),0)
JARFLAGS=-cfv
endif

JAVA_OPTIONS = -Xlint 

ifeq ($(TESTROOT),)
RELEASE_PATH="$(ERL_TOP)/release/$(TARGET)"
else
RELEASE_PATH=$(TESTROOT)
endif


# ----------------------------------------------------
# Make Rules
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) $(ERL_TOP)/lib/jinterface/vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(JINTERFACE_VSN);' $< > $@
$(APPUP_TARGET): $(APPUP_SRC) $(ERL_TOP)/lib/jinterface/vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(JINTERFACE_VSN);' $< > $@

debug opt: make_dirs $(JAVA_DEST_ROOT)$(JARFILE)

make_dirs:
	$(V_at)if [ ! -d "$(JAVA_DEST_ROOT)" ];then mkdir "$(JAVA_DEST_ROOT)"; fi

$(JAVA_DEST_ROOT)$(JARFILE): $(TARGET_FILES)
	@(cd $(JAVA_DEST_ROOT) ; $(JAR) $(JARFLAGS) $(JARFILE) $(JAVA_CLASS_SUBDIR))

clean:
	$(V_at)rm -f $(TARGET_FILES) *~

docs:

# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------

# include $(ERL_TOP)/make/otp_release_targets.mk

release release_docs release_tests release_html:
	$(V_at)$(MAKE) $(MFLAGS) RELEASE_PATH="$(RELEASE_PATH)" $(TARGET_MAKEFILE)  $@_spec

release_spec: opt
	$(V_at)$(INSTALL_DIR) "$(RELSYSDIR)/java_src/com/ericsson/otp/erlang"
	$(V_at)$(INSTALL_DATA) $(JAVA_SRC) "$(RELSYSDIR)/java_src/com/ericsson/otp/erlang"
	$(V_at)$(INSTALL_DIR) "$(RELSYSDIR)/priv"
	$(V_at)$(INSTALL_DATA) $(JAVA_DEST_ROOT)$(JARFILE) "$(RELSYSDIR)/priv"
	$(V_at)$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(V_at)$(INSTALL_DATA) $(APP_TARGET) "$(RELSYSDIR)/ebin/$(APP_FILE)"
	$(V_at)$(INSTALL_DATA) $(APPUP_TARGET) "$(RELSYSDIR)/ebin/$(APPUP_FILE)"

release_docs_spec:




# ----------------------------------------------------
