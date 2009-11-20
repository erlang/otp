#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

include ./vsn.mk
include ./config.mk
SUBDIRS = src 
ifeq ($(CAN_BUILD_DRIVER), true) 
SUBDIRS += c_src 
endif 
SUBDIRS += examples demos doc/src
CLEANDIRS = $(SUBDIRS) api_gen

ifeq ($(INSIDE_ERLSRC),true)
# we are inside erl src
# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
SUB_DIRECTORIES=$(SUBDIRS)
include $(ERL_TOP)/make/otp_subdir.mk
else 
# we are building standalone wxErlang
all: opt 

opt: 
	@mkdir -p ebin
	@mkdir -p priv
	@mkdir -p c_src/$(SYS_TYPE)
	@for d in $(SUBDIRS); do         \
	   if test ! -d $$d ; then        \
	       echo "=== Skipping subdir $$d" ; \
	   else                   \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ;                        \
	done

# clean, removes beam, object and target files 

clean: 
	rm -f *~
	@for d in $(CLEANDIRS); do        \
	   if test ! -d $$d ; then        \
	       echo "=== Skipping subdir $$d" ; \
	   else                        \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ;                        \
	done

docs: 
	(cd doc/src/ && $(MAKE) $@)

prefix      = @prefix@
exec_prefix = @exec_prefix@
libdir      = @libdir@

install: 
	escript ./install.es $(INSTALLDIR)

release: 
	escript ./install.es --create_release

endif
