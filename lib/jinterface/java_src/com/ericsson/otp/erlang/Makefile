# -*-Makefile-*-

#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

TARGET_FILES= $(JAVA_FILES:%=$(JAVA_DEST_ROOT)$(JAVA_CLASS_SUBDIR)%.class)
JAVA_SRC= $(JAVA_FILES:%=%.java)

JARFILE= OtpErlang.jar


# ----------------------------------------------------
# Programs and Flags
# ----------------------------------------------------

JAR= jar

CLASSPATH = $(JAVA_SRC_ROOT)

JAVADOCFLAGS=-d $(DOCDIR)
JAVAFLAGS=-d $(JAVA_DEST_ROOT) 
JARFLAGS=-cvf

JAVA_OPTIONS = 

ifeq ($(TESTROOT),)
RELEASE_PATH=$(ERL_TOP)/release/$(TARGET)
else
RELEASE_PATH=$(TESTROOT)
endif


# ----------------------------------------------------
# Make Rules
# ----------------------------------------------------

debug opt: make_dirs $(JAVA_DEST_ROOT)$(JARFILE)

make_dirs:
	if [ ! -d "$(JAVA_DEST_ROOT)" ];then mkdir "$(JAVA_DEST_ROOT)"; fi

$(JAVA_DEST_ROOT)$(JARFILE): $(TARGET_FILES)
	@(cd $(JAVA_DEST_ROOT) ; $(JAR) $(JARFLAGS) $(JARFILE) $(JAVA_CLASS_SUBDIR))

clean:
	rm -f $(TARGET_FILES) *~

docs:

# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------

# include $(ERL_TOP)/make/otp_release_targets.mk

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(RELEASE_PATH) $(TARGET_MAKEFILE)  $@_spec

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/java_src/com/ericsson/otp/erlang
	$(INSTALL_DATA) $(JAVA_SRC) $(RELSYSDIR)/java_src/com/ericsson/otp/erlang
	$(INSTALL_DIR) $(RELSYSDIR)/priv
	$(INSTALL_DATA) $(JAVA_DEST_ROOT)$(JARFILE) $(RELSYSDIR)/priv

release_docs_spec:




# ----------------------------------------------------

