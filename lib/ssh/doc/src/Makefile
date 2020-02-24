#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2004-2018. All Rights Reserved.
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
VSN=$(SSH_VSN)
APPLICATION=ssh

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = \
	ssh.xml \
	ssh_client_channel.xml \
	ssh_client_key_api.xml \
	ssh_connection.xml \
	ssh_server_channel.xml \
	ssh_server_key_api.xml \
	ssh_file.xml \
	ssh_sftp.xml \
	ssh_sftpd.xml \

XML_REF6_FILES = ssh_app.xml

XML_PART_FILES = usersguide.xml

XML_CHAPTER_FILES = \
	notes.xml \
	introduction.xml \
	using_ssh.xml \
	terminology.xml \
	configure_algos.xml

BOOK_FILES = book.xml

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) $(XML_REF3_FILES)  $(XML_REF6_FILES)\
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

IMAGE_FILES = SSH_protocols.png

TOP_SPECS_FILE = specs.xml

NO_CHUNKS = ssh_client_key_api.xml ssh_server_key_api.xml ssh_server_channel.xml

# ----------------------------------------------------
include $(ERL_TOP)/make/doc.mk
