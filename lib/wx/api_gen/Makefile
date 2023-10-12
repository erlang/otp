#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2008-2021. All Rights Reserved.
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
ERLC = erlc
ERL  = erl
EBIN = .

# make GL_DIR=/usr/include/GL WX_DIR=/src/wxWidgets
GL_DIR = /usr/include/GL
WX_DIR = $(ERL_TOP)/../wxWidgets

ERL_COMPILE_FLAGS=+debug_info +warn_unused_vars

COMPILER = gen_util wx_gen wx_gen_erl wx_gen_nif wx_gen_doc
COMPILER_T = $(COMPILER:%=$(EBIN)/%.beam)

GL_COMP = gl_gen gl_gen_erl gl_gen_nif gl_gen_doc gl_scan_doc
GL_COMP_T = $(GL_COMP:%=$(EBIN)/%.beam)

TARGET_EDIR = ../src/gen
TARGET_CDIR = ../c_src/gen

C_TARGETS   = wxe_funcs.cpp
GL_C_TARGETS = gl_nif.c

WX = wx_code_generated

GL = gl_code_generated

opt:  $(WX) $(GL)

$(WX): wxxml_generated $(COMPILER_T) wxapi.conf $(wildcard wx_extra/wx*.c_src) $(wildcard wx_extra/wx*.erl)
	erl -noshell -run wx_gen code && touch wx_code_generated

wxxml_generated:  wx_doxygen.conf wx_extra/wxe_evth.h wx_extra/added_func.h
	WX_DIR=$(WX_DIR) doxygen wx_doxygen.conf > wx_doxygen.log && touch wxxml_generated

glxml_generated: gl_doxygen.conf
	sed -e 's|@GL_DIR@|$(GL_DIR)|g' gl_doxygen.conf > gl_doxygen
	doxygen gl_doxygen && touch glxml_generated

$(GL): glxml_generated $(GL_COMP_T) glapi.conf
	erl -noshell -run gl_gen code && touch gl_code_generated

%.beam: %.erl wx_gen.hrl gl_gen.hrl
	$(ERLC) -W $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

# TODO split cleans into separate targets?
complete_clean:
	rm -f gl_doxygen wx_doxygen wx_xml/*.x* gl_xml/*.x*
	rm -f *_generated
	$(MAKE) clean
clean:
	rm -f *~
	rm -f $(COMPILER_T) $(GL_COMP_T)
	rm -f wx_extra/*~
