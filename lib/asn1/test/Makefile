#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
# Target Specs
# ----------------------------------------------------

MODULES= \
	h323test \
	choice_extension \
	ber_decode_error \
	testExternal \
	testPrim \
        testPrimStrings \
	testCompactBitString \
        testPrimExternal \
	testChoPrim \
	testChoExtension \
	testChoExternal \
	testChoOptional \
	testChoOptionalImplicitTag \
	testChoRecursive \
	testChoTypeRefCho \
	testChoTypeRefPrim \
	testChoTypeRefSeq \
	testChoTypeRefSet \
	testConstraints \
	testDef \
	testOpt \
	testSeqDefault \
	testSeqExtension \
	testSeqExternal \
	testSeqOptional \
	testSeq2738 \
	testSeqPrim \
	testSeqTag \
	testSeqTypeRefCho \
	testSeqTypeRefPrim \
	testSeqTypeRefSeq \
	testSeqTypeRefSet \
	testSeqIndefinite \
	testSeqOf \
	testSeqOfIndefinite \
	testSeqOfCho \
	testSeqOfExternal \
	testSeqOfTag \
	testSetDefault \
	testSetExtension \
	testSetExternal \
	testSetOptional \
	testSetPrim \
	testSetTag \
	testSetTypeRefCho \
	testSetTypeRefPrim \
	testSetTypeRefSeq \
	testSetTypeRefSet \
	testSetIndefinite \
	testChoiceIndefinite \
	testSetOf \
	testSetOfCho \
	testSetOfExternal \
	testSetOfTag \
	testEnumExt \
	testInfObjectClass \
	testInfObj \
	testParameterizedInfObj \
	testMergeCompile \
	testDeepTConstr \
	testTimer \
	testRANAP \
	testMegaco \
	testMvrasn6 \
	testSeqSetDefaultVal \
	testParamBasic \
	testContextSwitchingTypes \
	testTypeValueNotation \
	testOpenTypeImplicitTag \
	testROSE \
	testINSTANCE_OF \
	test_partial_incomplete_decode \
	testDER \
	test_selective_decode \
	test_special_decode_performance \
	testTCAP \
	testSSLspecs \
	test_driver_load \
	testSelectionTypes \
	test_undecoded_rest \
	test_inline \
	testTcapsystem \
	testNBAPsystem \
	test_bad_values \
	test_compile_options \
	testDoubleEllipses \
	test_modified_x420 \
	testX420 \
	test_x691 \
	asn1_test_lib \
	asn1_app_test \
	asn1_appup_test \
	asn1_wrapper \
	asn1_SUITE \
	asn1_bin_SUITE \
	asn1_bin_v2_SUITE

SUITE= asn1_SUITE.erl
SUITE_BIN= asn1_bin_SUITE.erl
SUITE_BIN_V2= asn1_bin_v2_SUITE.erl
SUITE_SRC= asn1_SUITE.erl.src
SUITE_BIN_SRC= asn1_bin_SUITE.erl.src
SUITE_BIN_V2_SRC= asn1_bin_SUITE.erl.src

ERL_FILES= $(MODULES:%=%.erl)

HRL_FILES= External.hrl

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/asn1_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warnings_as_errors
EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(EMAKEFILE): $(ERL_FILES) $(HRL_FILES)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) $(ERL_FILES) >$(EMAKEFILE)

tests debug opt: $(SUITE) $(SUITE_BIN) $(SUITE_BIN_V2) $(EMAKEFILE)

clean:
	rm -f $(SUITE) $(SUITE_BIN) $(SUITE_BIN_V2)
	rm -f core

docs:

#-----------------------------------------------------
# Special Targets
#-----------------------------------------------------
$(SUITE): $(SUITE_SRC)
	sed -e 's;%BIN%;;' -e 's;%PER%;per;' -e 's;%BER%;ber;'  $< > $@
	cat asn1_common_SUITE.erl.src >> $@
	cat asn1_particular_SUITE.erl.src >> $@


$(SUITE_BIN): $(SUITE_SRC)
	sed -e 's;%BIN%;bin_;' -e 's;%PER%;per_bin;' -e 's;%BER%;ber_bin;' $< > $@
	echo "common() -> []." >> $@
	cat asn1_bin_particular_SUITE.erl.src >> $@

$(SUITE_BIN_V2): $(SUITE_SRC)
	sed -e 's;%BIN%;bin_v2_;' -e 's;%PER%;per_bin;' -e 's;%BER%;ber_bin_v2;' $< > $@
	echo "common() -> []." >> $@
	cat asn1_bin_v2_particular_SUITE.erl.src >> $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELSYSDIR)/asn1_SUITE_data
	$(INSTALL_DIR) $(RELSYSDIR)/asn1_bin_SUITE_data
	$(INSTALL_DIR) $(RELSYSDIR)/asn1_bin_v2_SUITE_data
	$(INSTALL_DATA)  $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)
	$(INSTALL_DATA) asn1.spec $(INSTALL_PROGS) $(RELSYSDIR)
	chmod -R u+w $(RELSYSDIR)
	cd asn1_SUITE_data; tar cfh $(RELSYSDIR)/asn1_SUITE_data.tar *
	cd $(RELSYSDIR)/asn1_SUITE_data; tar xf $(RELSYSDIR)/asn1_SUITE_data.tar
	cd $(RELSYSDIR)/asn1_bin_SUITE_data; tar xf $(RELSYSDIR)/asn1_SUITE_data.tar
	cd $(RELSYSDIR)/asn1_bin_v2_SUITE_data; tar xf $(RELSYSDIR)/asn1_SUITE_data.tar
	rm $(RELSYSDIR)/asn1_SUITE_data.tar

release_docs_spec:
