# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
# Invoke with GNU make or clearmake -C gnu.
#

include $(ERL_TOP)/make/run_make.mk
include modules.mk

conf: 
	cd ../..; $(MAKE) conf

dconf: 
	$(MAKE) FLEX_SCANNER_REENTRANT=disable conf

econf: 
	$(MAKE) FLEX_SCANNER_REENTRANT=enable conf

setup:
	(cd ../../priv/lib && \
		rm -f $(STD_DRV).so $(MT_DRV).so && \
		ln -s $(TARGET)/$(STD_DRV).so && \
		ln -s $(TARGET)/$(MT_DRV).so )

info:
	$(MAKE) -f $(TARGET)/Makefile $@

