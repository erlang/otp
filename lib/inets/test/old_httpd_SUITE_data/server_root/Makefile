#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
VSN=$(INETS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/inets-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULE= 

AUTH_FILES = auth/group \
	auth/passwd 
CGI_FILES = cgi-bin/printenv.sh 
CONF_FILES = conf/8080.conf \
	conf/8888.conf \
	conf/httpd.conf \
	conf/ssl.conf \
	conf/mime.types 
OPEN_FILES = htdocs/open/dummy.html 
MNESIA_OPEN_FILES = htdocs/mnesia_open/dummy.html 
MISC_FILES = htdocs/misc/friedrich.html \
	htdocs/misc/oech.html
SECRET_FILES = htdocs/secret/dummy.html 
MNESIA_SECRET_FILES = htdocs/mnesia_secret/dummy.html 
HTDOCS_FILES = htdocs/index.html \
	htdocs/config.shtml \
	htdocs/echo.shtml \
	htdocs/exec.shtml \
	htdocs/flastmod.shtml \
	htdocs/fsize.shtml \
	htdocs/include.shtml
ICON_FILES = icons/README \
	icons/a.gif \
	icons/alert.black.gif \
	icons/alert.red.gif \
	icons/apache_pb.gif \
	icons/back.gif \
	icons/ball.gray.gif \
	icons/ball.red.gif \
	icons/binary.gif \
	icons/binhex.gif \
	icons/blank.gif \
	icons/bomb.gif \
	icons/box1.gif \
	icons/box2.gif \
	icons/broken.gif \
	icons/burst.gif \
	icons/button1.gif \
	icons/button10.gif \
	icons/button2.gif \
	icons/button3.gif \
	icons/button4.gif \
	icons/button5.gif \
	icons/button6.gif \
	icons/button7.gif \
	icons/button8.gif \
	icons/button9.gif \
	icons/buttonl.gif \
	icons/buttonr.gif \
	icons/c.gif \
	icons/comp.blue.gif \
	icons/comp.gray.gif \
	icons/compressed.gif \
	icons/continued.gif \
	icons/dir.gif \
	icons/down.gif \
	icons/dvi.gif \
	icons/f.gif \
	icons/folder.gif \
	icons/folder.open.gif \
	icons/folder.sec.gif \
	icons/forward.gif \
	icons/generic.gif \
	icons/generic.red.gif \
	icons/generic.sec.gif \
	icons/hand.right.gif \
	icons/hand.up.gif \
	icons/htdig.gif \
	icons/icon.sheet.gif \
	icons/image1.gif \
	icons/image2.gif \
	icons/image3.gif \
	icons/index.gif \
	icons/layout.gif \
	icons/left.gif \
	icons/link.gif \
	icons/movie.gif \
	icons/p.gif \
	icons/patch.gif \
	icons/pdf.gif \
	icons/pie0.gif \
	icons/pie1.gif \
	icons/pie2.gif \
	icons/pie3.gif \
	icons/pie4.gif \
	icons/pie5.gif \
	icons/pie6.gif \
	icons/pie7.gif \
	icons/pie8.gif \
	icons/portal.gif \
	icons/poweredby.gif \
	icons/ps.gif \
	icons/quill.gif \
	icons/right.gif \
	icons/screw1.gif \
	icons/screw2.gif \
	icons/script.gif \
	icons/sound1.gif \
	icons/sound2.gif \
	icons/sphere1.gif \
	icons/sphere2.gif \
	icons/star.gif \
	icons/star_blank.gif \
	icons/tar.gif \
	icons/tex.gif \
	icons/text.gif \
	icons/transfer.gif \
	icons/unknown.gif \
	icons/up.gif \
	icons/uu.gif \
	icons/uuencoded.gif \
	icons/world1.gif \
	icons/world2.gif

SSL_FILES = ssl/ssl_client.pem \
	ssl/ssl_server.pem

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: 

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/auth
	$(INSTALL_DATA) $(AUTH_FILES) $(RELSYSDIR)/examples/server_root/auth
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/cgi-bin
	$(INSTALL_SCRIPT) $(CGI_FILES) $(RELSYSDIR)/examples/server_root/cgi-bin
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/conf
	$(INSTALL_DATA) $(CONF_FILES) $(RELSYSDIR)/examples/server_root/conf
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/htdocs/open
	$(INSTALL_DATA) $(OPEN_FILES) \
		$(RELSYSDIR)/examples/server_root/htdocs/open
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/htdocs/mnesia_open
	$(INSTALL_DATA) $(MNESIA_OPEN_FILES) \
		$(RELSYSDIR)/examples/server_root/htdocs/mnesia_open
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/htdocs/misc
	$(INSTALL_DATA) $(MISC_FILES) \
		$(RELSYSDIR)/examples/server_root/htdocs/misc
	$(INSTALL_DIR)  \
		$(RELSYSDIR)/examples/server_root/htdocs/secret/top_secret
	$(INSTALL_DIR)  \
		$(RELSYSDIR)/examples/server_root/htdocs/mnesia_secret/top_secret
	$(INSTALL_DATA) $(SECRET_FILES)  \
		$(RELSYSDIR)/examples/server_root/htdocs/secret
	$(INSTALL_DATA) $(MNESIA_SECRET_FILES)  \
		$(RELSYSDIR)/examples/server_root/htdocs/mnesia_secret
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/htdocs
	$(INSTALL_DATA) $(HTDOCS_FILES) $(RELSYSDIR)/examples/server_root/htdocs
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/icons
	$(INSTALL_DATA) $(ICON_FILES) $(RELSYSDIR)/examples/server_root/icons
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/ssl
	$(INSTALL_DATA) $(SSL_FILES) $(RELSYSDIR)/examples/server_root/ssl
	$(INSTALL_DIR) $(RELSYSDIR)/examples/server_root/logs

release_docs_spec:

