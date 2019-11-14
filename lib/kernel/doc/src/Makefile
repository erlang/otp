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
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)

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
	logger_cookbook.xml

BOOK_FILES = book.xml

# The .png file is generated from a .dia file with target 'update_png'
IMAGE_FILES = \
	logger_arch.png

XML_FILES = \
	$(BOOK_FILES) $(XML_CHAPTER_FILES) \
	$(XML_PART_FILES) $(XML_REF3_FILES) $(XML_REF4_FILES)\
	$(XML_REF6_FILES) $(XML_APPLICATION_FILES)

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN4_FILES = $(XML_REF4_FILES:%.xml=$(MAN4DIR)/%.4)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

HTML_REF_MAN_FILE = $(HTMLDIR)/index.html

TOP_PDF_FILE = $(PDFDIR)/$(APPLICATION)-$(VSN).pdf

SPECS_FILES = $(XML_REF3_FILES:%.xml=$(SPECDIR)/specs_%.xml)

TOP_SPECS_FILE = specs.xml


# ----------------------------------------------------
# FIGURES
# ----------------------------------------------------
# In order to update the figures you have to have both dia
# and imagemagick installed.
# The generated .png file must be committed.

update_png:
	dia --export=logger_arch.eps logger_arch.dia
	convert logger_arch.eps -resize 65% logger_arch.png

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
XML_FLAGS +=

SPECS_ESRC = ../../src

SPECS_FLAGS = -I../../include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%: %
	$(INSTALL_DATA) $< $@

docs: man pdf html

$(TOP_PDF_FILE): $(XML_FILES)

pdf: $(TOP_PDF_FILE)

html: images $(HTML_REF_MAN_FILE)

man: $(MAN3_FILES) $(MAN4_FILES) $(MAN6_FILES)

images: $(IMAGE_FILES:%=$(HTMLDIR)/%)

info:
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "XML_REF3_ESOCK_FILES:  $(XML_REF3_ESOCK_FILES)"
	@echo "XML_REF3_FILES:        $(XML_REF3_FILES)"
	@echo "XML_REF4_FILES:        $(XML_REF4_FILES)"
	@echo "XML_REF6_FILES:        $(XML_REF6_FILES)"
	@echo "XML_PART_FILES:        $(XML_PART_FILES)"
	@echo "XML_CHAPTER_FILES:     $(XML_CHAPTER_FILES)"
	@echo "BOOK_FILES:            $(BOOK_FILES)"

debug opt:

clean clean_docs:
	rm -rf $(HTMLDIR)/*
	rm -rf $(XMLDIR)
	rm -f  $(MAN3DIR)/*
	rm -f  $(MAN4DIR)/*
	rm -f  $(MAN6DIR)/*
	rm -f  $(TOP_PDF_FILE) $(TOP_PDF_FILE:%.pdf=%.fo)
	rm -f  $(SPECDIR)/*
	rm -f  errs core *~ *.eps

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


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_docs_spec: docs
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DATA) $(TOP_PDF_FILE) "$(RELSYSDIR)/doc/pdf"
	$(INSTALL_DIR) "$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(HTMLDIR)/* \
		"$(RELSYSDIR)/doc/html"
	$(INSTALL_DATA) $(INFO_FILE) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DATA) $(MAN3DIR)/* "$(RELEASE_PATH)/man/man3"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man4"
	$(INSTALL_DATA) $(MAN4_FILES) "$(RELEASE_PATH)/man/man4"
	$(INSTALL_DIR) "$(RELEASE_PATH)/man/man6"
	$(INSTALL_DATA) $(MAN6_FILES) "$(RELEASE_PATH)/man/man6"

release_spec:
