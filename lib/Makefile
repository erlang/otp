#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2012. All Rights Reserved.
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

ERTS_SUB_DIRECTORIES = stdlib sasl kernel compiler
OTHER_SUB_DIRECTORIES = tools test_server common_test runtime_tools
ifdef BUILD_ALL
  OTHER_SUB_DIRECTORIES += \
          snmp otp_mibs appmon erl_interface asn1 jinterface \
          wx debugger reltool gs inets \
          ic mnesia crypto orber os_mon parsetools syntax_tools \
          pman public_key ssl toolbar tv observer odbc \
          diameter \
          cosTransactions cosEvent cosTime cosNotification \
          cosProperty cosFileTransfer cosEventDomain et megaco webtool \
	  xmerl edoc eunit ssh inviso typer erl_docgen \
	  percept eldap dialyzer hipe
  EXTRA_FILE := $(wildcard EXTRA-APPLICATIONS)
  EXTRA_APPLICATIONS := $(if $(EXTRA_FILE),$(shell cat $(EXTRA_FILE)))
endif

ifdef BOOTSTRAP
  SUB_DIRECTORIES = \
    kernel stdlib compiler
else
  ifdef SECONDARY_BOOTSTRAP
    SUB_DIRECTORIES = hipe parsetools asn1/src
  else
    ifdef TERTIARY_BOOTSTRAP
      SUB_DIRECTORIES = snmp sasl jinterface ic syntax_tools wx
    else # Not bootstrap build
      SUB_DIRECTORIES = $(ERTS_SUB_DIRECTORIES) \
			$(OTHER_SUB_DIRECTORIES) \
			$(EXTRA_APPLICATIONS)
    endif
  endif
endif


# ----------------------------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk
