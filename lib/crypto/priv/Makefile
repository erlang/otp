# ``Licensed under the Apache License, Version 2.0 (the "License");
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
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#

# ----------------------------------------------------
#        THIS MAKEFILE SERVES AS AN EXAMPLE OF
#        HOW TO RELINK THE CRYPTO NIF LIBRARY
# ----------------------------------------------------

# ----------------------------------------------------
# Variables for linking a .so library on unix.
# Note: These may differ between systems.
# ----------------------------------------------------

SO_LD = gcc
SO_LDFLAGS = -G 
SO_SSL_LIBDIR = /usr/local/lib
SO_NIFLIB = $(LIBDIR)/$(LIB_NAME).so

# ----------------------------------------------------
# Variables for linking a win32 .dll library.
# Note: These may differ between systems.
# ----------------------------------------------------

DLL_LD = link.exe
DLL_LDFLAGS = -release -nologo -incremental:no -dll
DLL_LIBDIR = "c:\\OpenSSL\\lib\\VC"
DLL_LIBS = libeay32.lib MSVCRT.LIB kernel32.lib \
	   advapi32.lib gdi32.lib user32.lib \
	   comctl32.lib comdlg32.lib shell32.lib
DLL_NIFLIB = $(LIBDIR)/$(LIB_NAME).dll
DLL_EXP = $(LIBDIR)/$(LIB_NAME).exp
DLL_LIB = $(LIBDIR)/$(LIB_NAME).lib

# ----------------------------------------------------
# Common variables
# ----------------------------------------------------

OBJDIR = ./
LIBDIR = ../lib
LIB_NAME = crypto
OBJS = $(OBJDIR)/crypto.o

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(SO_NIFLIB): $(OBJS)
	$(SO_LD) $(SO_LDFLAGS) -L$(SO_SSL_LIBDIR) -Wl,-R$(SO_SSL_LIBDIR) \
	-o $@ $^ -lcrypto

$(DLL_NIFLIB): $(OBJS)
	$(DLL_LD) $(DLL_LDFLAGS) -out:$@ -libpath:$(DLL_LIBDIR) $(OBJS) \
	$(DLL_LIBS)

so:	$(SO_NIFLIB)

dll:	$(DLL_NIFLIB)

clean:
	rm -f $(SO_NIFLIB) $(DLL_NIFLIB) 
	rm -f $(DLL_EXP) $(DLL_LIB)
	rm -f core *~

