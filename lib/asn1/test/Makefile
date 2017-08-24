#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
# Target Specs
# ----------------------------------------------------

MODULES= \
	h323test \
	ber_decode_error \
	testPrim \
	testPrimStrings \
	testCompactBitString \
	testPrimExternal \
	testChoPrim \
	testChoExtension \
	testChoExternal \
	testChoOptional \
	testChoRecursive \
	testChoTypeRefCho \
	testChoTypeRefPrim \
	testChoTypeRefSeq \
	testChoTypeRefSet \
	testConstraints \
	testDef \
	testExtensionDefault \
	testOpt \
	testSeqDefault \
	testSeqExtension \
	testSeqExternal \
	testSeqOptional \
	testSeq2738 \
	testSeqPrim \
	testSeqSetIndefinite \
	testSeqTag \
	testSeqTypeRefCho \
	testSeqTypeRefPrim \
	testSeqTypeRefSeq \
	testSeqTypeRefSet \
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
	testChoiceIndefinite \
	testSetOf \
	testSetOfCho \
	testSetOfExternal \
	testSetOfTag \
	testEnumExt \
	testInfObjectClass \
	testInfObj \
	testInfObjExtract \
	testParameterizedInfObj \
	testFragmented \
	testMaps \
	testMergeCompile \
	testMultipleLevels \
	testDeepTConstr \
	testTimer \
	testMegaco \
	testSeqSetDefaultVal \
	testParamBasic \
	testContextSwitchingTypes \
	testTypeValueNotation \
	testOpenTypeImplicitTag \
	testINSTANCE_OF \
	test_partial_incomplete_decode \
	testDER \
	test_selective_decode \
	test_special_decode_performance \
	testTCAP \
	testSSLspecs \
	testSelectionTypes \
	test_undecoded_rest \
	testTcapsystem \
	testNBAPsystem \
	test_compile_options \
	testDoubleEllipses \
	test_modified_x420 \
	test_x691 \
	testWSParamClass \
	testValueTest \
	testUniqueObjectSets \
	testRfcs \
	testImporting \
	testExtensibilityImplied \
	asn1_test_lib \
	asn1_app_SUITE \
	asn1_SUITE \
	error_SUITE \
	syntax_SUITE

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
ERL_COMPILE_FLAGS += +warnings_as_errors +nowarn_export_all
EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(EMAKEFILE): $(ERL_FILES) $(HRL_FILES)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) $(ERL_FILES) >$(EMAKEFILE)

tests debug opt: $(EMAKEFILE)

clean:
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/asn1_SUITE_data"
	$(INSTALL_DATA) $(EMAKEFILE) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) asn1.spec asn1.cover $(INSTALL_PROGS) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	cd asn1_SUITE_data; tar cfh "$(RELSYSDIR)/asn1_SUITE_data.tar" *
	cd "$(RELSYSDIR)/asn1_SUITE_data"; tar xf "$(RELSYSDIR)/asn1_SUITE_data.tar"
	rm "$(RELSYSDIR)/asn1_SUITE_data.tar"

release_docs_spec:
