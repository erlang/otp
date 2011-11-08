#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2002-2011. All Rights Reserved.
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

SCRIPT_SKELETON_SRC = \
	meas.sh.skel.src \
	mstone1.sh.skel.src

MESSAGE_PACKAGES = \
	time_test.msgs

MODULES = \
        megaco_codec_transform  \
        megaco_codec_mstone_lib \
        megaco_codec_mstone1    \
        megaco_codec_mstone2    \
        megaco_codec_meas 

README = MEAS_README

