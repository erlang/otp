#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#
# Macros
#
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
  ERTS_SUB_DIRECTORIES = stdlib sasl kernel compiler
  OTHER_SUB_DIRECTORIES =	\
     snmp otp_mibs appmon erl_interface os_mon tools runtime_tools
  ifdef BUILD_ALL
    OTHER_SUB_DIRECTORIES += mnesia jinterface ic asn1 debugger \
	inets mnesia_session orber pman tv observer cosTransactions cosEvent \
      cosTime cosNotification cosProperty cosFileTransfer cosEventDomain
  endif
else
  ifeq ($(findstring ose,$(TARGET)),ose)
    ERTS_SUB_DIRECTORIES = stdlib sasl kernel compiler erl_interface
    OTHER_SUB_DIRECTORIES =	\
      snmp otp_mibs appmon tools
#    OTHER_SUB_DIRECTORIES =	\
#      appmon os_mon tools runtime_tools
    ifdef BUILD_ALL
      OTHER_SUB_DIRECTORIES += mnesia  \
        inets pman tv observer
#      OTHER_SUB_DIRECTORIES += mnesia ic asn1 debugger \
#        inets orber pman tv observer cosTransactions cosEvent \
#        cosTime cosNotification cosProperty cosFileTransfer cosEventDomain
    endif
  else
#
# unix and win32
# --------------
#
    ERTS_SUB_DIRECTORIES = stdlib sasl kernel compiler
    OTHER_SUB_DIRECTORIES = tools test_server
    ifdef BUILD_ALL
      ifeq ($(findstring win32,$(TARGET)),win32) # BUILD_ALL on win32
        OTHER_SUB_DIRECTORIES += 			\
          snmp otp_mibs appmon erl_interface asn1 jinterface gs wx inets ic \
          mnesia crypto orber os_mon parsetools syntax_tools pman \
          public_key ssl toolbar tv observer debugger reltool odbc runtime_tools \
          cosTransactions cosEvent cosTime cosNotification cosProperty \
          cosFileTransfer cosEventDomain et megaco webtool \
	  xmerl edoc eunit ssh inviso typer docbuilder erl_docgen common_test percept
# dialyzer
        OTHER_SUB_DIRECTORIES += hipe
      else # BUILD_ALL on unix
        OTHER_SUB_DIRECTORIES += \
          snmp otp_mibs appmon erl_interface asn1 jinterface wx debugger reltool gs inets \
          ic mnesia crypto orber os_mon parsetools syntax_tools \
          pman public_key ssl toolbar tv observer odbc \
          runtime_tools cosTransactions cosEvent cosTime cosNotification \
          cosProperty cosFileTransfer cosEventDomain et megaco webtool \
	  xmerl edoc eunit ssh inviso typer docbuilder erl_docgen common_test percept
# dialyzer
        OTHER_SUB_DIRECTORIES += hipe $(TSP_APP)
      endif
    endif
  endif
endif

ifdef BOOTSTRAP
  SUB_DIRECTORIES = \
    kernel stdlib compiler orber/include
else
  ifdef SECONDARY_BOOTSTRAP
    SUB_DIRECTORIES = hipe parsetools asn1/src
  else
    ifdef TERTIARY_BOOTSTRAP
      SUB_DIRECTORIES = snmp
    else
      ifdef FOURTH_BOOTSTRAP
        SUB_DIRECTORIES = sasl jinterface ic syntax_tools
      else # Not bootstrap build
        SUB_DIRECTORIES = $(ERTS_SUB_DIRECTORIES) $(OTHER_SUB_DIRECTORIES)
      endif
    endif
  endif
endif


# ----------------------------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk
