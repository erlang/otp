#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2000-2018. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)
APPLICATION=megaco


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include files.mk

# ----------------------------------------------------

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

STANDARD_DIR = ../standard
STANDARDS = $(STANDARD_DIR)/rfc3525.txt \
            $(STANDARD_DIR)/rfc4234.txt \
            $(STANDARD_DIR)/rfc4566.txt \
            $(STANDARD_DIR)/implementors_guide_v10-13.pdf

NO_CHUNKS = megaco_edist_compress.xml megaco_user.xml megaco_encoder.xml \
	megaco_transport.xml megaco_tcp.xml megaco_udp.xml \
	megaco_codec_meas.xml megaco_codec_mstone1.xml \
	megaco_codec_mstone2.xml megaco_codec_transform.xml

include $(ERL_TOP)/make/doc.mk

$(HTMLDIR)/megaco_architecture.html: megaco_architecture.xml
$(HTMLDIR)/megaco_codec_meas.html: megaco_codec_meas.xml
$(HTMLDIR)/megaco_codec_transform.html: megaco_codec_transform.xml
$(HTMLDIR)/megaco_debug.html: megaco_debug.xml
$(HTMLDIR)/megaco_encoder.html: megaco_encoder.xml
$(HTMLDIR)/megaco_encode.html: megaco_encode.xml
$(HTMLDIR)/megaco_examples.html: megaco_examples.xml
$(HTMLDIR)/megaco_flex_scanner.html: megaco_flex_scanner.xml
$(HTMLDIR)/megaco_intro.html: megaco_intro.xml
$(HTMLDIR)/megaco_mib.html: megaco_mib.xml
$(HTMLDIR)/megaco_performance.html: megaco_performance.xml
$(HTMLDIR)/megaco_run.html: megaco_run.xml
$(HTMLDIR)/megaco.html: megaco.xml
$(HTMLDIR)/megaco_tcp.html: megaco_tcp.xml
$(HTMLDIR)/megaco_transport_mechanisms.html: megaco_transport_mechanisms.xml
$(HTMLDIR)/megaco_transport.html: megaco_transport.xml
$(HTMLDIR)/megaco_udp.html: megaco_udp.xml
$(HTMLDIR)/megaco_user.html: megaco_user.xml
