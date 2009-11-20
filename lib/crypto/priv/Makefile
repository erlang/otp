# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#

# ----------------------------------------------------
#        THIS MAKEFILE SERVES AS AN EXAMPLE OF
#        HOW TO RELINK THE CRYPTO DRIVER
# ----------------------------------------------------

# ----------------------------------------------------
# Variables for linking a .so driver on unix.
# Note: These may differ between systems.
# ----------------------------------------------------

SO_LD = gcc
SO_LDFLAGS = -G 
SO_SSL_LIBDIR = /usr/local/lib
SO_DRIVER = $(LIBDIR)/$(DRIVER_NAME).so

# ----------------------------------------------------
# Variables for linking a win32 .dll driver.
# Note: These may differ between systems.
# ----------------------------------------------------

DLL_LD = link.exe
DLL_LDFLAGS = -release -nologo -incremental:no -dll
DLL_LIBDIR = "c:\\OpenSSL\\lib\\VC"
DLL_LIBS = libeay32.lib MSVCRT.LIB kernel32.lib \
	   advapi32.lib gdi32.lib user32.lib \
	   comctl32.lib comdlg32.lib shell32.lib
DLL_DRIVER = $(LIBDIR)/$(DRIVER_NAME).dll
DLL_EXP = $(LIBDIR)/$(DRIVER_NAME).exp
DLL_LIB = $(LIBDIR)/$(DRIVER_NAME).lib

# ----------------------------------------------------
# Common variables
# ----------------------------------------------------

OBJDIR = ./
LIBDIR = ../lib
DRIVER_NAME = crypto_drv
OBJS = $(OBJDIR)/crypto_drv.o

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(SO_DRIVER): $(OBJS)
	$(SO_LD) $(SO_LDFLAGS) -L$(SO_SSL_LIBDIR) -Wl,-R$(SO_SSL_LIBDIR) \
	-o $@ $^ -lcrypto

$(DLL_DRIVER): $(OBJS)
	$(DLL_LD) $(DLL_LDFLAGS) -out:$@ -libpath:$(DLL_LIBDIR) $(OBJS) \
	$(DLL_LIBS)

so:	$(SO_DRIVER)

dll:	$(DLL_DRIVER)

clean:
	rm -f $(SO_DRIVER) $(DLL_DRIVER) 
	rm -f $(DLL_EXP) $(DLL_LIB)
	rm -f core *~

