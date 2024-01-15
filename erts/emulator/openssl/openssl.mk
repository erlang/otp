#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2023. All Rights Reserved.
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

OPENSSL_O = md5_dgst.o

OPENSSL_LIB_NAME = micro-openssl
OPENSSL_DIR =  $(ERL_TOP)/erts/emulator/openssl
OPENSSL_INCLUDE_DIR = $(OPENSSL_DIR)/include
OPENSSL_OBJDIR = $(OPENSSL_DIR)/obj/$(TARGET)/$(TYPE)
OPENSSL_OBJS = $(OPENSSL_O:%=$(OPENSSL_OBJDIR)/%)
OPENSSL_CFLAGS = $(filter-out -DDEBUG,$(CFLAGS)) -I$(OPENSSL_INCLUDE_DIR) \
	-DERLANG_OPENSSL_INTEGRATION

ifeq ($(TARGET), win32)
OPENSSL_LIB = $(OPENSSL_OBJDIR)/$(OPENSSL_LIB_NAME).lib

$(OPENSSL_LIB): $(OPENSSL_OBJS)
	$(V_AR) -out:$@ $(OPENSSL_OBJS)
else

OPENSSL_LIB = $(OPENSSL_OBJDIR)/$(OPENSSL_LIB_NAME).a

$(OPENSSL_LIB): $(OPENSSL_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(OPENSSL_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

$(OPENSSL_OBJDIR)/%.o: $(OPENSSL_DIR)/crypto/md5/%.c
	$(V_CC) -c $(OPENSSL_CFLAGS) -o $@ $<

# Dependencies.

$(OPENSSL_OBJDIR)/md5_dgst.o: $(OPENSSL_DIR)/crypto/md5/md5_dgst.c \
	$(OPENSSL_DIR)/crypto/md5/md5_local.h \
	$(OPENSSL_DIR)/include/crypto/md32_common.h \
	$(OPENSSL_DIR)/include/openssl_local/md5.h
