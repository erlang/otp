#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

include ../../vsn.mk

VSN = $(DIAMETER_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
include files.mk

XML_REF_FILES = $(XML_REF1_FILES) $(XML_REF3_FILES) $(XML_REF4_FILES)

XML_FILES = $(BOOK_FILES) $(XML_APPLICATION_FILES) \
            $(XML_REF_FILES) \
            $(XML_PART_FILES) $(XML_CHAPTER_FILES)

XML_GEN_FILES = $(XMLDIR)/seehere.ent $(patsubst %.ent,$(XMLDIR)/%.ent,$(XML_EXTRA_FILES))

EXTRA_FILES=depend.mk $(XMLDIR)/seehere.ent

NO_CHUNKS = diameter_app.xml diameter_transport.xml

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

include $(ERL_TOP)/make/doc.mk

$(XMLDIR)/seehere.ent: Makefile seealso.ent
	$(gen_verbose) sed -f seehere.sed seealso.ent > $@
$(XMLDIR)/%.ent: %.ent
	$(gen_verbose) cp $< $@

ldocs: local_docs $(INDEX_TARGET)

depend.mk: depend.sed Makefile $(XMLDIR)/seehere.ent $(XMLDIR)/seealso.ent $(XML_REF_FILES) $(XML_CHAPTER_FILES)
	$(gen_verbose) \
	(for f in $(XML_REF_FILES) $(XML_CHAPTER_FILES); do \
	     sed -f $< $$f | sed "s@%FILE%@`basename $$f .xml`@g"; \
	 done) \
	> $@

-include depend.mk

.PHONY: depend ldocs
