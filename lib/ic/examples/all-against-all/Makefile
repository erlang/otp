#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
#
# Point this at your version of OTP
OTPROOT=/usr/local/otp/releases/otp_beam_sunos5_r7a

# Type actual IC Version
ICVSN=4.0.4

# Type actual Erl Interface Vesrion
EIVSN=3.2.2

# Type actual Erl Interface Vesrion
JIVSN=1.2

# IDL file(s)
IDLS=random.idl

# Own C-server files
CSRV=server callbacks

# Own C-client files
CCL=client

# Generated C-server files
GCSRVS=rmod_random__s 

# Generated C-server files
GCCLS=rmod_random

# Includes
IFLAGS=-I$(OTPROOT)/lib/ic-$(ICVSN)/include \
       -I$(OTPROOT)/lib/erl_interface-$(EIVSN)/include

LDFLAGS=-L$(OTPROOT)/lib/ic-$(ICVSN)/priv/lib \
	-L$(OTPROOT)/lib/erl_interface-$(EIVSN)/lib

LDLIBS=-lic -lerl_interface -lei -lnsl -lsocket


# Erlang compiler
ERLC=$(OTPROOT)/bin/erlc 

# Erlang compiler flags. 
EFLAGS='+{scoped_op_calls,true}'

# C compiler
CC=gcc

# C compiler flags
CFLAGS=-ggdb -O2 -Wall $(IFLAGS)

# Java compiler
JAVAC=javac

CLASSPATH= "./:$(OTPROOT)/lib/ic-$(ICVSN)/priv/ic.jar:$(OTPROOT)/lib/jinterface-$(JIVSN)/priv/OtpErlang.jar"
JFLAGS=-classpath $(CLASSPATH) -O

JGENJFILES = \
	./rmod/_randomImplBase.java \
	./rmod/random.java \
	./rmod/randomHolder.java \
	./rmod/_randomStub.java \
	./rmod/randomHelper.java


all: server client eall jall


server: 
	$(ERLC) $(EFLAGS) '+{be,c_server}' $(IDLS)
	$(CC) $(IFLAGS) -c $(CSRV:=.c) $(GCSRVS:=.c)
	$(CC) $(CSRV:=.o) $(GCSRVS:=.o) -o $@ $(LDFLAGS) $(LDLIBS)

client: 
	$(ERLC) $(EFLAGS) '+{be,c_client}' $(IDLS)
	$(CC) $(IFLAGS) -c $(CCL:=.c) $(GCCLS:=.c)
	$(CC) $(CCL:=.o) $(GCCLS:=.o) -o $@ $(LDFLAGS) $(LDLIBS)

eall:
	$(ERLC) $(EFLAGS) '+{be,erl_genserv}' $(IDLS)
	$(ERLC) *.erl

jall:
	$(ERLC) $(EFLAGS) '+{be,java}' $(IDLS)
	$(JAVAC) $(JFLAGS) */*.java *.java


clean: 
	/bin/rm -rf $(GCCLS:=.o) $(GCCLS:=.c) $(GCSRVS:=.o) $(GCSRVS:=.c) $(CCL:=.o) $(CSRV:=.o) rmod.erl rmod_random.erl *.jam *.beam oe* *.h *.hrl *~ core server client *.class











