#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

include ./vsn.mk

ifdef TERTIARY_BOOTSTRAP
 INSIDE_ERLSRC = true
 SUBDIRS = src
else # Normal build
 include ./config.mk
 SUBDIRS = src
 ifeq ($(CAN_BUILD_DRIVER), true)
 SUBDIRS += c_src
 endif
 SUBDIRS += examples doc/src
endif #TERTIARY_BOOTSTRAP

CLEANDIRS = $(SUBDIRS) api_gen

# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
SUB_DIRECTORIES=$(SUBDIRS)

include $(ERL_TOP)/make/otp_subdir.mk
