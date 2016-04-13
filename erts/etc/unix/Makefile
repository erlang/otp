#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

include $(ERL_TOP)/make/output.mk
include $(ERL_TOP)/make/target.mk

include $(ERL_TOP)/make/$(TARGET)/otp.mk
include ../../vsn.mk

opt debug: etc

.PHONY: etc
etc: etp-commands

etp-commands: etp-commands.in
	$(gen_verbose)sed 's:@ERL_TOP@:${ERL_TOP}:g' etp-commands.in > etp-commands

.PHONY: docs
docs:

.PHONY: clean
clean:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

.PHONY: release_spec
release_spec: etc