#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

SNMP_VSN = 4.17.3
PRE_VSN  =
APP_VSN  = "snmp-$(SNMP_VSN)$(PRE_VSN)"

TICKETS = OTP-9700 OTP-9868

TICKETS_4_17_2 = OTP-9236

TICKETS_4_17_1 = OTP-8761

TICKETS_4_17 = OTP-8478

TICKETS_4_16_2 = \
	OTP-8563 \
	OTP-8574 \
	OTP-8594 \
	OTP-8595 \
	OTP-8646 \
	OTP-8648

TICKETS_4_16_1 = \
	OTP-8480 \
	OTP-8481

TICKETS_4_16 = \
	OTP-8395 \
	OTP-8433 \
	OTP-8442

TICKETS_4_15 = \
	OTP-8229 \
	OTP-8249

TICKETS_4_14 = \
	OTP-8223 \
	OTP-8228 \
	OTP-8237

TICKETS_4_13_5 = \
	OTP-8116 \
	OTP-8120 \
	OTP-8181 \
	OTP-8182

