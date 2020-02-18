#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2003-2018. All Rights Reserved.
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
VSN=$(COMMON_TEST_VSN)
APPLICATION=common_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

CT_XML_FILES = $(CT_MODULES:=.xml)

XML_APPLICATION_FILES = ref_man.xml
XML_REF1_FILES = ct_run_cmd.xml
# REMEMBER: links to HTML files for these modules in ref_man.xml
XML_REF3_FILES = ct.xml \
	ct_master.xml \
	ct_cover.xml \
	ct_telnet.xml \
	ct_ftp.xml \
	ct_ssh.xml \
	ct_rpc.xml \
	ct_snmp.xml \
	unix_telnet.xml \
	ct_slave.xml \
	ct_property_test.xml \
	ct_netconfc.xml \
	ct_hooks.xml \
        ct_testspec.xml
XML_REF6_FILES = common_test_app.xml

XML_PART_FILES = part.xml

XML_CHAPTER_FILES = \
	introduction.xml \
	basics_chapter.xml \
	getting_started_chapter.xml \
	install_chapter.xml \
	write_test_chapter.xml \
	test_structure_chapter.xml \
	run_test_chapter.xml \
	example_chapter.xml \
	why_test_chapter.xml \
	config_file_chapter.xml \
	cover_chapter.xml \
	ct_master_chapter.xml \
	event_handler_chapter.xml \
	ct_hooks_chapter.xml \
	ct_property_test_chapter.xml \
	dependencies_chapter.xml \
	notes.xml

BOOK_FILES = book.xml

IMAGE_FILES = \
	tc_execution.gif \
	config.gif \
	html_logs.gif

XML_FILES=$(XML_APPLICATION_FILES) $(XML_REF1_FILES)  $(XML_REF3_FILES) $(XML_REF6_FILES) \
	$(XML_PART_FILES) $(XML_CHAPTER_FILES) $(BOOK_FILES)

TOP_SPECS_FILE = specs.xml

NO_CHUNKS = ct_hooks.xml

# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk
