# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2024. All Rights Reserved.
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

include $(ERL_TOP)/make/otp_default_release_path.mk

# ----------------------------------------------------
# Local documentation target for testing 
# ----------------------------------------------------
local_docs:

# ----------------------------------------------------
# Standard release target
# ----------------------------------------------------

ifeq ($(TESTROOT),)

release release_docs release_tests:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(OTP_DEFAULT_RELEASE_PATH) \
		$(TARGET_MAKEFILE)  $@_spec

else

release release_docs release_tests:
	$(MAKE) $(MFLAGS) RELEASE_PATH="$(TESTROOT)" $(TARGET_MAKEFILE)  $@_spec 

endif
