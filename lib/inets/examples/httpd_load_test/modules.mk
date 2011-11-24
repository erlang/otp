#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2010. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
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


