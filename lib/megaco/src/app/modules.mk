#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

MODULES = \
	megaco

EXTERNAL_HRL_FILES = \
        ../../include/megaco.hrl \
        ../../include/megaco_message_v1.hrl \
        ../../include/megaco_message_v2.hrl \
        ../../include/megaco_message_prev3a.hrl \
        ../../include/megaco_message_prev3b.hrl \
        ../../include/megaco_message_prev3c.hrl \
        ../../include/megaco_message_v3.hrl \
        ../../include/megaco_sdp.hrl

INTERNAL_HRL_FILES = \
	megaco_internal.hrl


