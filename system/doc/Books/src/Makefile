# ----------------------------------------------------
# Copyright (C) 1997, Ericsson Telecommunications
# Author: Lars Thorsen, Hans Nilsson
# ----------------------------------------------------
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

ifeq (ug.dep,$(wildcard ug.dep))
include ug.dep
include database_management.dep
include orber_ic.dep
include basic_application.dep
include tools.dep
include operation_maintenance.dep
include interfaces.dep
include stdlib.dep
include corba_service.dep
endif

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
BOOKS = \
	ug \
	database_management \
	orber_ic \
	corba_service \
	basic_application \
	tools \
	operation_maintenance \
	interfaces \
	stdlib

TEX_FILES = $(shell for i in $(BOOKS) ; do (echo $$i.tex); done)

FRAME_CROP_PDF_FILES = $(BOOKS:%=%.frame.pdf) $(BOOKS:%=%.crop.pdf)

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAG_booksty = -booksty otpBOOK
DVIPS_FLAGS += -O '19mm,32.5mm'

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all:	$(FRAME_CROP_PDF_FILES)

books:	all

clean:
	rm -f $(TEX_FILES)
	rm -f *.psframe *.pscrop *.ps
	rm -f $(TOP_PS_FILES)
	rm -f errs core *~ $(LATEX_CLEAN)
	rm -f *.dep *.pdf

dep depend:
	@for i in $(BOOKS); do \
		echo "Running docdepend for $$i ..." ; \
		docdepend $$i.xml | \
		sed s/insidecover.tex/insidecover.xml/ > $$i.dep ; \
	done

# ----------------------------------------------------
# Rules
# ----------------------------------------------------
.SUFFIXES: .psframe .pscrop

# The following rules are for multiple suffixes, e.g. kalle.pdf, 
# kalle.frame.pdf. The order of the rules is important. Default rules
# from otp.mk are disabled in order to get it right.

%.pdf:	%.ps

%.ps:	%.dvi

%.pdf:	%.dvi

%.frame.ps:	%.dvi
	BOOK=$@ ./make_headers
	$(DVI2PS) -frame $(DVIPS_FLAGS) -f $< > $@

%.frame.pdf:	%.dvi
	BOOK=$@ ./make_headers
	$(DVI2PS) -frame $(DVIPS_FLAGS) -f $< | \
		$(DISTILL) $(DISTILL_FLAGS) > $@

%.crop.ps:	%.dvi
	BOOK=$@ ./make_headers
	$(DVI2PS) -crop $(DVIPS_FLAGS) -f $< > $@

%.crop.pdf:	%.dvi
	BOOK=$@ ./make_headers
	$(DVI2PS) -crop $(DVIPS_FLAGS) -f $< | \
		$(DISTILL) $(DISTILL_FLAGS) > $@

%.pdf:	%.dvi
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | \
		$(DISTILL) $(DISTILL_FLAGS) > $@

%.ps:  %.dvi
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@

%.pdf: %.dvi
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | \
		$(DISTILL) $(DISTILL_FLAGS) > $@

# ----------------------------------------------------
# Release targets
# ----------------------------------------------------
#

ifeq ($(TESTROOT),)
release_books:	all

else
release_books:	all
	$(INSTALL_DIR) $(TESTROOT)/books
	$(INSTALL_DATA) $(FRAME_CROP_PDF_FILES) $(TESTROOT)/books
endif

