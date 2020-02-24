#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(SSL_VSN)
APPLICATION=ssl

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = refman.xml
XML_REF3_FILES = ssl.xml ssl_crl_cache.xml ssl_crl_cache_api.xml ssl_session_cache_api.xml
XML_REF6_FILES = ssl_app.xml

XML_PART_FILES = usersguide.xml
XML_CHAPTER_FILES = \
	ssl_introduction.xml \
	ssl_protocol.xml \
	using_ssl.xml \
	ssl_distribution.xml \
	standards_compliance.xml \
	notes.xml

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES) $(XML_REF6_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

TOP_SPECS_FILE = specs.xml

NO_CHUNKS = ssl_crl_cache_api.xml ssl_session_cache_api.xml

# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk
