#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2019. All Rights Reserved.
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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(KERNEL_VSN)
APPLICATION=kernel

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml

XML_REF3_FILES = application.xml \
	auth.xml \
	code.xml \
	disk_log.xml \
	erl_boot_server.xml \
	erl_ddll.xml \
	erl_epmd.xml \
	erl_prim_loader_stub.xml \
	erlang_stub.xml \
	erpc.xml \
	error_handler.xml \
	error_logger.xml \
	file.xml \
	gen_tcp.xml \
	gen_udp.xml \
	gen_sctp.xml \
	global.xml \
	global_group.xml \
	heart.xml \
	inet.xml \
	inet_res.xml \
	init_stub.xml \
        logger.xml \
        logger_std_h.xml \
        logger_disk_log_h.xml \
        logger_filters.xml \
        logger_formatter.xml \
	net.xml \
	net_adm.xml \
	net_kernel.xml \
	os.xml \
	pg.xml \
	pg2.xml \
	rpc.xml \
	seq_trace.xml \
	wrap_log_reader.xml \
	user.xml \
	zlib_stub.xml

XML_REF4_FILES = app.xml config.xml

XML_REF6_FILES = kernel_app.xml

XML_PART_FILES = part.xml
XML_CHAPTER_FILES = \
        notes.xml \
        introduction_chapter.xml \
	logger_chapter.xml \
	logger_cookbook.xml \
	eep48_chapter.xml

BOOK_FILES = book.xml

# The .png file is generated from a .dia file with target 'update_png'
IMAGE_FILES = \
	logger_arch.png

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_REF4_FILES)\
	$(XML_REF6_FILES) $(XML_APPLICATION_FILES)

TOP_SPECS_FILE = specs.xml

$(SPECDIR)/specs_erl_prim_loader_stub.xml:
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module erl_prim_loader_stub
$(SPECDIR)/specs_erlang_stub.xml:
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module erlang_stub
$(SPECDIR)/specs_init_stub.xml:
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module init_stub
$(SPECDIR)/specs_zlib_stub.xml:
	$(gen_verbose)escript $(SPECS_EXTRACTOR) $(SPECS_FLAGS) \
                -o$(dir $@) -module zlib_stub

NO_CHUNKS = erl_prim_loader_stub.xml erlang_stub.xml init_stub.xml zlib_stub.xml

# ----------------------------------------------------
# FIGURES
# ----------------------------------------------------
# In order to update the figures you have to have both dia
# and imagemagick installed.
# The generated .png file must be committed.

update_png:
	dia --export=logger_arch.eps logger_arch.dia
	convert logger_arch.eps -resize 65% logger_arch.png

include $(ERL_TOP)/make/doc.mk
