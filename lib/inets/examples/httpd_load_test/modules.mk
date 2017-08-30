#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

SCRIPT_SKELETONS = \
	hdlt.sh.skel

CONF_SKELETONS = \
	hdlt.config.skel

CERT_FILES = \
	hdlt_ssl_client_cert.pem \
	hdlt_ssl_server_cert.pem

README = HDLT_README

MODULES = \
        hdlt \
        hdlt_ctrl \
        hdlt_client \
	hdlt_logger \
	hdlt_random_html \
	hdlt_server \
	hdlt_slave

INTERNAL_HRL_FILES = \
        hdlt_logger.hrl


