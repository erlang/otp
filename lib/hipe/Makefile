#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
SHELL=/bin/sh

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

ifdef HIPE_ENABLED
HIPE_SUBDIRS = regalloc sparc ppc x86 amd64 arm opt tools llvm
else
HIPE_SUBDIRS =
endif

ALWAYS_SUBDIRS = misc main cerl icode flow util doc/src

ifdef HIPE_ENABLED
# "rtl" below must be the first directory so that file rtl/hipe_literals.hrl
# which is needed by many other HiPE files is built first
SUB_DIRECTORIES = rtl $(ALWAYS_SUBDIRS) $(HIPE_SUBDIRS)
else
SUB_DIRECTORIES = $(ALWAYS_SUBDIRS)
endif


include native.mk

ifndef EBIN
EBIN = ../ebin
endif

#
# Default Subdir Targets
#
include $(ERL_TOP)/make/otp_subdir.mk

# The overriding docs target have been removed so the default make rules work properly.

edocs:
	@if [ -d $(ERL_TOP)/lib/edoc/ebin ]; then \
	  erl -noshell -pa $(ERL_TOP)/lib/edoc/ebin $(ERL_TOP)/lib/syntax_tools/ebin $(ERL_TOP)/lib/xmerl/ebin -run edoc_run application 'hipe' '"."' '[new,no_packages]' -s init stop ; \
	fi

all-subdirs:
	$(V_at)for dir in $(SUB_DIRECTORIES); do \
		(cd $$dir; $(MAKE) $(MAKETARGET) EBIN=$(EBIN); cd ..); \
	done

# distclean and realclean should clean the bootstrap files
all-subdirs-x:
	$(V_at)for dir in $(SUB_DIRECTORIES); do \
		(cd $$dir; $(MAKE) $(MAKETARGET) EBIN=../boot_ebin; cd ..); \
	done

clean:
	$(V_at)$(MAKE) MAKETARGET="clean" all-subdirs all-subdirs-x
distclean:
	$(V_at)$(MAKE) MAKETARGET="distclean" all-subdirs all-subdirs-x
realclean:
	$(V_at)$(MAKE) MAKETARGET="realclean" all-subdirs all-subdirs-x

