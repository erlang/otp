#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
#
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(INETS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/inets-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULE= 

AUTH_FILES = server_root/auth/group \
	server_root/auth/passwd 
CGI_FILES = server_root/cgi-bin/printenv.sh 
CONF_FILES = server_root/conf/8080.conf \
	server_root/conf/8888.conf \
	server_root/conf/httpd.conf \
	server_root/conf/ssl.conf \
	server_root/conf/mime.types 
OPEN_FILES = server_root/htdocs/open/dummy.html 
MNESIA_OPEN_FILES = server_root/htdocs/mnesia_open/dummy.html 
MISC_FILES = server_root/htdocs/misc/friedrich.html \
	server_root/htdocs/misc/oech.html
SECRET_FILES = server_root/htdocs/secret/dummy.html 
MNESIA_SECRET_FILES = server_root/htdocs/mnesia_secret/dummy.html 
HTDOCS_FILES = server_root/htdocs/index.html \
	server_root/htdocs/config.shtml \
	server_root/htdocs/echo.shtml \
	server_root/htdocs/exec.shtml \
	server_root/htdocs/flastmod.shtml \
	server_root/htdocs/fsize.shtml \
	server_root/htdocs/include.shtml
ICON_FILES = server_root/icons/README \
	server_root/icons/a.gif \
	server_root/icons/alert.black.gif \
	server_root/icons/alert.red.gif \
	server_root/icons/apache_pb.gif \
	server_root/icons/back.gif \
	server_root/icons/ball.gray.gif \
	server_root/icons/ball.red.gif \
	server_root/icons/binary.gif \
	server_root/icons/binhex.gif \
	server_root/icons/blank.gif \
	server_root/icons/bomb.gif \
	server_root/icons/box1.gif \
	server_root/icons/box2.gif \
	server_root/icons/broken.gif \
	server_root/icons/burst.gif \
	server_root/icons/button1.gif \
	server_root/icons/button10.gif \
	server_root/icons/button2.gif \
	server_root/icons/button3.gif \
	server_root/icons/button4.gif \
	server_root/icons/button5.gif \
	server_root/icons/button6.gif \
	server_root/icons/button7.gif \
	server_root/icons/button8.gif \
	server_root/icons/button9.gif \
	server_root/icons/buttonl.gif \
	server_root/icons/buttonr.gif \
	server_root/icons/c.gif \
	server_root/icons/comp.blue.gif \
	server_root/icons/comp.gray.gif \
	server_root/icons/compressed.gif \
	server_root/icons/continued.gif \
	server_root/icons/dir.gif \
	server_root/icons/down.gif \
	server_root/icons/dvi.gif \
	server_root/icons/f.gif \
	server_root/icons/folder.gif \
	server_root/icons/folder.open.gif \
	server_root/icons/folder.sec.gif \
	server_root/icons/forward.gif \
	server_root/icons/generic.gif \
	server_root/icons/generic.red.gif \
	server_root/icons/generic.sec.gif \
	server_root/icons/hand.right.gif \
	server_root/icons/hand.up.gif \
	server_root/icons/htdig.gif \
	server_root/icons/icon.sheet.gif \
	server_root/icons/image1.gif \
	server_root/icons/image2.gif \
	server_root/icons/image3.gif \
	server_root/icons/index.gif \
	server_root/icons/layout.gif \
	server_root/icons/left.gif \
	server_root/icons/link.gif \
	server_root/icons/movie.gif \
	server_root/icons/p.gif \
	server_root/icons/patch.gif \
	server_root/icons/pdf.gif \
	server_root/icons/pie0.gif \
	server_root/icons/pie1.gif \
	server_root/icons/pie2.gif \
	server_root/icons/pie3.gif \
	server_root/icons/pie4.gif \
	server_root/icons/pie5.gif \
	server_root/icons/pie6.gif \
	server_root/icons/pie7.gif \
	server_root/icons/pie8.gif \
	server_root/icons/portal.gif \
	server_root/icons/poweredby.gif \
	server_root/icons/ps.gif \
	server_root/icons/quill.gif \
	server_root/icons/right.gif \
	server_root/icons/screw1.gif \
	server_root/icons/screw2.gif \
	server_root/icons/script.gif \
	server_root/icons/sound1.gif \
	server_root/icons/sound2.gif \
	server_root/icons/sphere1.gif \
	server_root/icons/sphere2.gif \
	server_root/icons/star.gif \
	server_root/icons/star_blank.gif \
	server_root/icons/tar.gif \
	server_root/icons/tex.gif \
	server_root/icons/text.gif \
	server_root/icons/transfer.gif \
	server_root/icons/unknown.gif \
	server_root/icons/up.gif \
	server_root/icons/uu.gif \
	server_root/icons/uuencoded.gif \
	server_root/icons/world1.gif \
	server_root/icons/world2.gif

SSL_FILES = server_root/ssl/ssl_client.pem \
	server_root/ssl/ssl_server.pem

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

