# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
include $(ERL_TOP)/erts/vsn.mk

VERSION_HEADER = erlang.nsh
MAKENSIS = makensis
MAKENSISFLAGS = /V2
CUSTOM_MODERN=custom_modern.exe

# This is not the way we usually do in our makefiles, 
# but making release is the ONLY thing we do with this one,
# Its not called during ordinary recursive make.
all: release

opt debug depend:
	@echo Nothing to do for "'"$@"'" on $(TARGET)

clean:
	rm -f $(VERSION_HEADER)

include $(ERL_TOP)/make/otp_release_targets.mk

TARGET_DIR = $(RELEASE_PATH)

ifdef MSYSTEM
 ifeq ($(MSYSTEM),$(filter $(MSYSTEM),MSYS MINGW32 MINGW64))
  USEMSYS := true
 endif
endif

ifeq ($(USEMSYS),true)

  MAKENSISFLAGS = //V2
  WTESTROOT=$(shell (msys2win_path.sh "$(RELEASE_PATH)"))
  WTARGET_DIR=$(shell (msys2win_path.sh "$(TARGET_DIR)"))

else

  MAKENSISFLAGS = /V2
  WTESTROOT=$(shell (cygpath -d "$(RELEASE_PATH)" 2>/dev/null || cygpath -w "$(RELEASE_PATH)"))
  WTARGET_DIR=$(shell (cygpath -d "$(TARGET_DIR)" 2>/dev/null || cygpath -d "$(TARGET_DIR)"))

endif

ifeq ($(CONFIG_SUBTYPE),win64)
  WINTYPE=win64
else
  WINTYPE=win32
endif

REDIST_FILE=$(shell (sh ./find_redist.sh || echo ""))
ifeq ($(USEMSYS),true)
	NICEREDISTFILE=$(shell (msys2win_path.sh -m "$(REDIST_FILE)" 2>/dev/null || echo ""))
else
	NICEREDISTFILE=$(shell (cygpath -d -m "$(REDIST_FILE)" 2>/dev/null || echo ""))
endif

REDIST_TARGET=$(shell (sh ./find_redist.sh -n || echo ""))
REDIST_DLL_VERSION=$(shell (sh ./dll_version_helper.sh $(NICEREDISTFILE) || echo ""))
REDIST_DLL_NAME=$(shell (sh ./dll_version_helper.sh -n $(NICEREDISTFILE) || echo ""))

release_spec:
	@NSIS_VER=`makensis /hdrinfo | head -1 | awk '{print $$2}'`; \
	case $$NSIS_VER in \
	v2.0b*) \
	  echo '!define MUI_VERSION "$(SYSTEM_VSN)"' > $(VERSION_HEADER);\
	  echo '!define ERTS_VERSION "$(VSN)"' >> $(VERSION_HEADER);\
	  echo '!define TESTROOT "$(WTESTROOT)"' >> $(VERSION_HEADER);\
	  echo '!define OUTFILEDIR "$(WTARGET_DIR)"' >> $(VERSION_HEADER);\
	  if [ -f "$(RELEASE_PATH)/docs/doc/index.html" ];\
	  then\
		echo '!define HAVE_DOCS 1' >> $(VERSION_HEADER); \
	  fi;\
	  $(MAKENSIS) erlang.nsi;;\
	v2.*) \
	  echo '!define OTP_VERSION "$(SYSTEM_VSN)"' > $(VERSION_HEADER);\
	  echo '!define ERTS_VERSION "$(VSN)"' >> $(VERSION_HEADER);\
	  echo '!define TESTROOT "$(WTESTROOT)"' >> $(VERSION_HEADER);\
	  echo '!define OUTFILEDIR "$(WTARGET_DIR)"' >> $(VERSION_HEADER);\
	  echo '!define WINTYPE "$(WINTYPE)"' >> $(VERSION_HEADER);\
	  if [ -f $(CUSTOM_MODERN) ];\
	  then \
		echo '!define HAVE_CUSTOM_MODERN 1' >> $(VERSION_HEADER); \
	  fi;\
	  if [ '!' -z "$(REDIST_FILE)" -a '!' -z "$(REDIST_DLL_VERSION)" ];\
	  then \
		cp $(REDIST_FILE) "$(RELEASE_PATH)/$(REDIST_TARGET)";\
		echo '!define HAVE_REDIST_FILE 1'  >> $(VERSION_HEADER); \
		echo '!define REDIST_DLL_VERSION "$(REDIST_DLL_VERSION)"' >> $(VERSION_HEADER);\
		echo '!define REDIST_DLL_NAME "$(REDIST_DLL_NAME)"' >> $(VERSION_HEADER);\
		echo '!define REDIST_EXECUTABLE "$(REDIST_TARGET)"' >> $(VERSION_HEADER);\
	  fi;\
	  if [ -f "$(RELEASE_PATH)/docs/doc/index.html" ];\
	  then \
		echo '!define HAVE_DOCS 1' >> $(VERSION_HEADER); \
	  fi;\
	  echo "Running $(MAKENSIS) $(MAKENSISFLAGS) erlang20.nsi";\
	  $(MAKENSIS) $(MAKENSISFLAGS) erlang20.nsi;;\
	*) \
	  echo 'Unsupported NSIS version';;\
	esac	

release_docs release_docs_spec docs:

