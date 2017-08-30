#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk

EBIN= ../ebin

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSTRANSACTIONS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/cosTransactions-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

INETRC_EXAMPLE = \
#        inetrc

IDL_FILES = \
#        hotel.idl \
#        travelAgency.idl

GEN_ERL_MODULES =  \
#        oe_travelAgency \
#	travelAgency_book \

MODULES= \
#        travelAgency_book_impl \

GEN_HRL_FILES = \
#        oe_travelAgency.hrl \
#	travelAgency_book.hrl

HRL_FILES= 

ERL_FILES= $(MODULES:%=%.erl)

JAVA_CLASSES = \
#	HotelResource

JAVA_FILES= $(JAVA_CLASSES:%=%.java)
CLASS_FILES= $(JAVA_CLASSES:%=%.class)

TARGET_FILES = \
        $(GEN_ERL_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
        $(MODULES:%=$(EBIN)/%.$(EMULATOR))


ifeq ($(findstring sparc-sun-solaris2, $(TARGET)),sparc-sun-solaris2)
    JAVA_TARGET=java
endif

# ----------------------------------------------------
# PROGRAMS
# ----------------------------------------------------
JAVA_IDL = idl
LOCAL_CLASSPATH = $(ERL_TOP)/lib/cosTransactions/priv:$(ERL_TOP)/lib/cosTransactions/examples/java_output:$(ERL_TOP)/lib/cosTransactions/src:$(ERL_TOP)/lib/cosTransactions/examples:$(ERL_TOP)/lib/cosTransactions/examples/java_output/hotel

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa /clearcase/otp/libraries/cosTransactions/ebin -pa /clearcase/otp/libraries/ic/ebin
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-pa /clearcase/otp/libraries/cosTransactions -I/clearcase/otp/libraries/cosTransactions
YRL_FLAGS = 

JAVA_OPTIONS = -classpath ../priv:/opt/local/pgm/OrbixWeb2.0.1

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
tests debug opt: $(TARGET_FILES) $(JAVA_TARGET)

java: 	java_costransactions_idl java_objects 
#	java_hotel_idl

clean:	
	rm -f $(TARGET_FILES) $(GEN_ERL_MODULES:%=%.erl) $(GEN_HRL_FILES) $(CLASS_FILES)
	rm -rf java_costransactions_idl
	rm -rf java_output/*
	rm -f errs core *~
#	rm -rf java_hotel_idl

docs:

java_costransactions_idl:
	$(JAVA_IDL) ../src/CosTransactions.idl
	@if [ -d java_output ]; then \
		echo "compiling java classes for CosTransactions ... This will take a while!!"; \
		CLASSPATH="${CLASSPATH}:${LOCAL_CLASSPATH}"; \
		export CLASSPATH;\
		(cd java_output/CosTransactions; $(JAVA) *.java;); \
	fi
	@touch java_costransactions_idl

#java_hotel_idl:
#	$(JAVA_IDL) hotel.idl
#	@if [ -d java_output ]; then \
#		echo "compiling java classes for hotel ..."; \
#		CLASSPATH="${CLASSPATH}:${LOCAL_CLASSPATH}"; \
#		export CLASSPATH;\
#		(cd java_output/hotel; $(JAVA) *.java;); \
#	fi
#	@touch java_hotel_idl

#java_objects:
#	@if [ -d java_output ]; then \
#		echo "compiling java example files ..."; \
#		CLASSPATH="${CLASSPATH}:${LOCAL_CLASSPATH}"; \
#		export CLASSPATH;\
#		$(JAVA) *.java; \
#	fi
#	@touch java_hotel_resource

#oe_travelAgency.erl: travelAgency.idl
#	erlc $(ERL_IDL_FLAGS) travelAgency.idl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(ERL_FILES) $(JAVA_FILES) $(IDL_FILES) "$(RELSYSDIR)/examples"
	$(INSTALL_DATA) $(INETRC_EXAMPLE) "$(RELSYSDIR)/examples"
	@tar cf - java_output | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:
