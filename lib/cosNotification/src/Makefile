#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
#
include $(ERL_TOP)/make/target.mk

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSNOTIFICATION_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/cosNotification-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	CosNotification_Common \
	CosNotifyChannelAdmin_ConsumerAdmin_impl \
	CosNotifyChannelAdmin_EventChannelFactory_impl \
	CosNotifyChannelAdmin_EventChannel_impl \
	CosNotifyChannelAdmin_SupplierAdmin_impl \
	CosNotifyFilter_Filter_impl \
	CosNotifyFilter_MappingFilter_impl \
	CosNotifyFilter_FilterFactory_impl \
	PullerConsumer_impl \
	PullerSupplier_impl \
	PusherConsumer_impl \
	PusherSupplier_impl \
	cosNotificationApp \
	cosNotification_Scanner \
	cosNotification_Filter \
	cosNotification_eventDB

ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = \
	CosNotification_Definitions.hrl

YECC_FILES = cosNotification_Grammar.yrl

GEN_YECC_ERL_FILES = cosNotification_Grammar.erl

GEN_YECC_HRL_FILES = 

GEN_NOTIFICATION_ERL_FILES =  \
	oe_CosNotification.erl \
	CosNotification.erl \
	CosNotification_AdminPropertiesAdmin.erl \
	CosNotification_EventHeader.erl \
	CosNotification_EventType.erl \
	CosNotification_FixedEventHeader.erl \
	CosNotification_NamedPropertyRange.erl \
	CosNotification_Property.erl \
	CosNotification_PropertyError.erl \
	CosNotification_PropertyRange.erl \
	CosNotification_QoSAdmin.erl \
	CosNotification_StructuredEvent.erl \
	CosNotification_UnsupportedAdmin.erl \
	CosNotification_UnsupportedQoS.erl \
	CosNotification_EventBatch.erl \
	CosNotification_EventTypeSeq.erl \
	CosNotification_NamedPropertyRangeSeq.erl \
	CosNotification_PropertyErrorSeq.erl \
	CosNotification_PropertySeq.erl

GEN_CHANNELADMIN_ERL_FILES =  \
	oe_CosNotifyChannelAdmin.erl \
	CosNotifyChannelAdmin_AdminLimit.erl \
	CosNotifyChannelAdmin_AdminLimitExceeded.erl \
	CosNotifyChannelAdmin_AdminNotFound.erl \
	CosNotifyChannelAdmin_ChannelNotFound.erl \
	CosNotifyChannelAdmin_ConnectionAlreadyActive.erl \
	CosNotifyChannelAdmin_ConnectionAlreadyInactive.erl \
	CosNotifyChannelAdmin_ConsumerAdmin.erl \
	CosNotifyChannelAdmin_EventChannel.erl \
	CosNotifyChannelAdmin_EventChannelFactory.erl \
	CosNotifyChannelAdmin_NotConnected.erl \
	CosNotifyChannelAdmin_ProxyConsumer.erl \
	CosNotifyChannelAdmin_ProxyNotFound.erl \
	CosNotifyChannelAdmin_ProxyPullConsumer.erl \
	CosNotifyChannelAdmin_ProxyPullSupplier.erl \
	CosNotifyChannelAdmin_ProxyPushConsumer.erl \
	CosNotifyChannelAdmin_ProxyPushSupplier.erl \
	CosNotifyChannelAdmin_ProxySupplier.erl \
	CosNotifyChannelAdmin_SequenceProxyPullConsumer.erl \
	CosNotifyChannelAdmin_SequenceProxyPullSupplier.erl \
	CosNotifyChannelAdmin_SequenceProxyPushConsumer.erl \
	CosNotifyChannelAdmin_SequenceProxyPushSupplier.erl \
	CosNotifyChannelAdmin_StructuredProxyPullConsumer.erl \
	CosNotifyChannelAdmin_StructuredProxyPullSupplier.erl \
	CosNotifyChannelAdmin_StructuredProxyPushConsumer.erl \
	CosNotifyChannelAdmin_StructuredProxyPushSupplier.erl \
	CosNotifyChannelAdmin_SupplierAdmin.erl \
	CosNotifyChannelAdmin_AdminIDSeq.erl \
	CosNotifyChannelAdmin_ChannelIDSeq.erl \
	CosNotifyChannelAdmin_ProxyIDSeq.erl

GEN_NOTIFYFILTER_ERL_FILES =  \
	oe_CosNotifyFilter.erl \
	CosNotifyFilter_CallbackNotFound.erl \
	CosNotifyFilter_ConstraintExp.erl \
	CosNotifyFilter_ConstraintInfo.erl \
	CosNotifyFilter_ConstraintNotFound.erl \
	CosNotifyFilter_DuplicateConstraintID.erl \
	CosNotifyFilter_Filter.erl \
	CosNotifyFilter_FilterAdmin.erl \
	CosNotifyFilter_FilterFactory.erl \
	CosNotifyFilter_FilterNotFound.erl \
	CosNotifyFilter_InvalidConstraint.erl \
	CosNotifyFilter_InvalidGrammar.erl \
	CosNotifyFilter_InvalidValue.erl \
	CosNotifyFilter_MappingConstraintInfo.erl \
	CosNotifyFilter_MappingConstraintPair.erl \
	CosNotifyFilter_MappingFilter.erl \
	CosNotifyFilter_UnsupportedFilterableData.erl \
	CosNotifyFilter_CallbackIDSeq.erl \
	CosNotifyFilter_ConstraintExpSeq.erl \
	CosNotifyFilter_ConstraintIDSeq.erl \
	CosNotifyFilter_ConstraintInfoSeq.erl \
	CosNotifyFilter_FilterIDSeq.erl \
	CosNotifyFilter_MappingConstraintInfoSeq.erl \
	CosNotifyFilter_MappingConstraintPairSeq.erl

GEN_NOTIFYCOMM_ERL_FILES =  \
	oe_CosNotifyComm.erl \
	CosNotifyComm_InvalidEventType.erl \
	CosNotifyComm_NotifyPublish.erl \
	CosNotifyComm_NotifySubscribe.erl \
	CosNotifyComm_PullConsumer.erl \
	CosNotifyComm_PullSupplier.erl \
	CosNotifyComm_PushConsumer.erl \
	CosNotifyComm_PushSupplier.erl \
	CosNotifyComm_SequencePullConsumer.erl \
	CosNotifyComm_SequencePullSupplier.erl \
	CosNotifyComm_SequencePushConsumer.erl \
	CosNotifyComm_SequencePushSupplier.erl \
	CosNotifyComm_StructuredPullConsumer.erl \
	CosNotifyComm_StructuredPullSupplier.erl \
	CosNotifyComm_StructuredPushConsumer.erl \
	CosNotifyComm_StructuredPushSupplier.erl \

GEN_OE_EVENTCOMM_ERL_FILES =  \
	oe_cosNotificationAppComm.erl \
	oe_CosNotificationComm_Event.erl

EXTERNAL_INC_PATH = ../include

GEN_NOTIFICATION_HRL_FILES = \
	oe_CosNotification.hrl \
	CosNotification.hrl \
	CosNotification_AdminPropertiesAdmin.hrl \
	CosNotification_QoSAdmin.hrl \

EXTERNAL_GEN_NOTIFICATION_HRL_FILES = \
	 $(GEN_NOTIFICATION_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

GEN_CHANNELADMIN_HRL_FILES =  \
	oe_CosNotifyChannelAdmin.hrl \
	CosNotifyChannelAdmin.hrl \
	CosNotifyChannelAdmin_ConsumerAdmin.hrl \
	CosNotifyChannelAdmin_EventChannel.hrl \
	CosNotifyChannelAdmin_EventChannelFactory.hrl \
	CosNotifyChannelAdmin_ProxyConsumer.hrl \
	CosNotifyChannelAdmin_ProxyPullConsumer.hrl \
	CosNotifyChannelAdmin_ProxyPullSupplier.hrl \
	CosNotifyChannelAdmin_ProxyPushConsumer.hrl \
	CosNotifyChannelAdmin_ProxyPushSupplier.hrl \
	CosNotifyChannelAdmin_ProxySupplier.hrl \
	CosNotifyChannelAdmin_SequenceProxyPullConsumer.hrl \
	CosNotifyChannelAdmin_SequenceProxyPullSupplier.hrl \
	CosNotifyChannelAdmin_SequenceProxyPushConsumer.hrl \
	CosNotifyChannelAdmin_SequenceProxyPushSupplier.hrl \
	CosNotifyChannelAdmin_StructuredProxyPullConsumer.hrl \
	CosNotifyChannelAdmin_StructuredProxyPullSupplier.hrl \
	CosNotifyChannelAdmin_StructuredProxyPushConsumer.hrl \
	CosNotifyChannelAdmin_StructuredProxyPushSupplier.hrl \
	CosNotifyChannelAdmin_SupplierAdmin.hrl \

EXTERNAL_GEN_CHANNELADMIN_HRL_FILES = \
	$(GEN_CHANNELADMIN_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

GEN_NOTIFYFILTER_HRL_FILES =  \
	oe_CosNotifyFilter.hrl \
	CosNotifyFilter.hrl \
	CosNotifyFilter_Filter.hrl \
	CosNotifyFilter_FilterAdmin.hrl \
	CosNotifyFilter_FilterFactory.hrl \
	CosNotifyFilter_MappingFilter.hrl

EXTERNAL_GEN_NOTIFYFILTER_HRL_FILES = \
	$(GEN_NOTIFYFILTER_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

GEN_NOTIFYCOMM_HRL_FILES =  \
	oe_CosNotifyComm.hrl \
	CosNotifyComm.hrl \
	CosNotifyComm_NotifyPublish.hrl \
	CosNotifyComm_NotifySubscribe.hrl \
	CosNotifyComm_PullConsumer.hrl \
	CosNotifyComm_PullSupplier.hrl \
	CosNotifyComm_PushConsumer.hrl \
	CosNotifyComm_PushSupplier.hrl \
	CosNotifyComm_SequencePullConsumer.hrl \
	CosNotifyComm_SequencePullSupplier.hrl \
	CosNotifyComm_SequencePushConsumer.hrl \
	CosNotifyComm_SequencePushSupplier.hrl \
	CosNotifyComm_StructuredPullConsumer.hrl \
	CosNotifyComm_StructuredPullSupplier.hrl \
	CosNotifyComm_StructuredPushConsumer.hrl \
	CosNotifyComm_StructuredPushSupplier.hrl \

EXTERNAL_GEN_NOTIFYCOMM_HRL_FILES = \
	$(GEN_NOTIFYCOMM_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

GEN_OE_EVENTCOMM_HRL_FILES =  \
	oe_cosNotificationAppComm.hrl \
	oe_CosNotificationComm.hrl \
	oe_CosNotificationComm_Event.hrl

IDL_GEN_ERL_FILES = \
	$(GEN_NOTIFICATION_ERL_FILES) \
	$(GEN_OE_EVENTCOMM_ERL_FILES) \
	$(GEN_NOTIFYCOMM_ERL_FILES) \
	$(GEN_NOTIFYFILTER_ERL_FILES) \
	$(GEN_CHANNELADMIN_ERL_FILES)

IDL_GEN_HRL_FILES = \
	$(EXTERNAL_GEN_NOTIFICATION_HRL_FILES) \
	$(GEN_OE_EVENTCOMM_HRL_FILES) \
	$(EXTERNAL_GEN_NOTIFYCOMM_HRL_FILES) \
	$(EXTERNAL_GEN_NOTIFYFILTER_HRL_FILES) \
	$(EXTERNAL_GEN_CHANNELADMIN_HRL_FILES)

GEN_ERL_FILES = \
	$(IDL_GEN_ERL_FILES) \
	$(GEN_YECC_ERL_FILES)

GEN_HRL_FILES = \
	$(IDL_GEN_HRL_FILES) \
	$(GEN_YECC_HRL_FILES)


GEN_FILES =  \
	$(GEN_HRL_FILES) \
	$(GEN_ERL_FILES)

TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

IDL_FILES = \
	CosNotification.idl \
	CosNotifyChannelAdmin.idl \
	CosNotifyFilter.idl \
	CosNotifyComm.idl \
	cosNotificationAppComm.idl

APPUP_FILE = cosNotification.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = cosNotification.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/cosNotification/ebin \
	-pa $(ERL_TOP)/lib/ic/ebin\
	-pa $(ERL_TOP)/lib/orber/ebin \
	-pa $(ERL_TOP)/lib/cosEvent/ebin \
	-pa $(ERL_TOP)/lib/cosTime/ebin \
	-I$(ERL_TOP)/lib/cosEvent/src

# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber/include \
	-pa $(ERL_TOP)/lib/cosNotification/include \
	-pa $(ERL_TOP)/lib/cosEvent/include \
	-pa $(ERL_TOP)/lib/cosTime/include \
	-I$(ERL_TOP)/lib/orber/include \
	-I$(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosEvent/include \
	-I$(ERL_TOP)/lib/cosTime/include \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"cosNotification_$(COSNOTIFICATION_VSN)"}' 

YECC_COMPILE_FLAGS =

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

debug:
	@${MAKE} TYPE=debug

cleanb:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET) $(APPUP_TARGET) IDL-GENERATED
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
IDL-GENERATED: CosNotification.idl CosNotifyChannelAdmin.idl \
  CosNotifyFilter.idl cosNotificationAppComm.idl CosNotifyComm.idl
	$(gen_verbose)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosNotification.cfg"}' CosNotification.idl
	$(V_at)mv $(GEN_NOTIFICATION_HRL_FILES) $(EXTERNAL_INC_PATH)
	$(V_at)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosNotifyChannelAdmin.cfg"}' CosNotifyChannelAdmin.idl
	$(V_at)mv $(GEN_CHANNELADMIN_HRL_FILES) $(EXTERNAL_INC_PATH)
	$(V_at)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosNotifyFilter.cfg"}' CosNotifyFilter.idl
	$(V_at)mv $(GEN_NOTIFYFILTER_HRL_FILES) $(EXTERNAL_INC_PATH)
	$(V_at)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"cosNotificationComm.cfg"}' cosNotificationAppComm.idl
	$(V_at)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosNotifyComm.cfg"}' CosNotifyComm.idl
	$(V_at)mv $(GEN_NOTIFYCOMM_HRL_FILES) $(EXTERNAL_INC_PATH)
	$(V_at)>IDL-GENERATED

$(IDL_GEN_ERL_FILES) $(IDL_GEN_HRL_FILES): IDL-GENERATED

$(TARGET_FILES): IDL-GENERATED

$(GEN_YECC_ERL_FILES) $(GEN_YECC_HRL_FILES): cosNotification_Grammar.yrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(GEN_FILES) $(IDL_FILES) $(YECC_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(IDL_FILES) $(YECC_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(GEN_HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:
