# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2019. All Rights Reserved.
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


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(MEGACO_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/megaco_test

ifeq ($(findstring win32,$(TARGET)),win32)

MAKEFILE_SRC = Makefile.win32.src

else

MAKEFILE_SRC = Makefile.src

endif

ifeq ($(TT_DIR),)
TT_DIR = /tmp
endif

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

EBIN = .

HRL_FILES = megaco_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(HRL_FILES) $(ERL_FILES) 

TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

CODEC_CASES = codec3c

ALL_CASES   = $(CODEC_CASES)

EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../src/app/megaco.mk 

ifeq ($(USE_MEGACO_TEST_CODE),true)
ERL_COMPILE_FLAGS += -DMEGACO_TEST_CODE=mona_lisa_spelar_doom
endif

ifeq ($(USE_MEGACO_HIPE),true)
ERL_COMPILE_FLAGS += +native -Dmegaco_hipe_special=true
endif

ERL_COMPILE_FLAGS += $(MEGACO_ERL_COMPILE_FLAGS)

# We have a behaviour in the test catalog (megaco_test_generator)
ERL_COMPILE_FLAGS += -pa ../../megaco/test

ERL_PATH = -pa ../../megaco/examples/simple \
           -pa ../../megaco/ebin \
           -pa ../../et/ebin

ifndef SUITE
SUITE = megaco_SUITE
endif

ESTOP = -s init stop

ifeq ($(DONT_STOP),true)
MAYBE_ESTOP =
else
MAYBE_ESTOP = $(ESTOP)
endif

ETVIEW = -s et_viewer
ifeq ($(USE_ET_VIEWER),true)
MAYBE_ETVIEW =
else
MAYBE_ETVIEW = $(ETVIEW)
endif

ifeq ($(MERL),)
MERL = erl
endif

ARGS += -noshell

ifeq ($(DISABLE_TC_TIMEOUT),true)
ARGS += -megaco_test_timeout
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(BUILDTARGET)

targets: $(TARGET_FILES)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)

$(EMAKEFILE): 
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' | grep -v Warning > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) | grep -v Warning >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "MAKE_EMAKE        = $(MAKE_EMAKE)"
	@echo "EMAKEFILE         = $(EMAKEFILE)"
	@echo "BUILDTARGET       = $(BUILDTARGET)"
	@echo ""
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo "ERL  = $(ERL)"
	@echo "MERL = $(MERL)"
	@echo ""
	@echo "ARGS = $(ARGS)"
	@echo ""
	@echo "HRL_FILES    = $(HRL_FILES)"
	@echo "ERL_FILES    = $(ERL_FILES)"
	@echo "TARGET_FILES = $(TARGET_FILES)"
	@echo ""

help:
	@echo ""
	@echo "This Makefile controls the test of the $(APPLICATION) application. "
	@echo ""
	@echo "There are two separate ways to perform the test of $(APPLICATION)."
	@echo ""
	@echo "  a) Run the official OTP test-server (which we do not describe here)"
	@echo ""
	@echo "  b) Run the test-server provided with this application. "
	@echo "     There are a number of targets to run the entire or parts"
	@echo "     of this applications ($(APPLICATION)) test-suite"
	@echo ""
	@echo "Targets:"
	@echo ""
	@echo "    help"
	@echo "      Print this info"
	@echo ""
	@echo "    info"
	@echo "      Prints various environment variables. "
	@echo "      May be useful when debugging the Makefile. "
	@echo ""
	@echo "    tests | debug | opt "
	@echo "      Compile all test-code. "
	@echo ""
	@echo "    clean "
	@echo "      Remove all targets. "
	@echo ""
	@echo "    codec"
	@echo "      Run the $(APPLICATION) codec sub-test-suite(s). "
	@echo ""
	@echo ""


# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

call: make
	@echo "make sure epmd is new"
	@epmd -kill > /dev/null
	@echo "Running all codec sub-suites separatelly"
	@for i in $(CODEC_CASES); do \
            echo "SUITE: $$i"; \
            clearmake -V $$i > $$i.log; \
        done

make: targets

utest: make
	$(MERL) $(ARGS) -sname megaco_utest $(ERL_PATH) \
            $(MAYBE_ETVIEW) \
            -s megaco_test_lib t $(SUITE) \
            $(ESTOP)

ftest: make
	$(MERL) $(ARGS) -sname megaco_ftest $(ERL_PATH) \
            -s megaco_filter \
            -s megaco_test_lib t $(SUITE) \
            $(ESTOP)

decode_compact_prof1: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_compact $(ERL_PATH) \
            -s megaco_codec_v1_SUITE profile_decode_compact_text_messages \
            $(ESTOP)

decode_compact_flex_prof1: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_compact_flex $(ERL_PATH) \
            -s megaco_codec_v1_SUITE profile_decode_compact_flex_text_messages \
            $(ESTOP)

decode_compact_prof2: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_compact $(ERL_PATH) \
            -s megaco_codec_v2_SUITE profile_decode_compact_text_messages \
            $(ESTOP)

decode_compact_flex_prof2: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_compact_flex $(ERL_PATH) \
            -s megaco_codec_v2_SUITE profile_decode_compact_flex_text_messages \
            $(ESTOP)

decode_pretty_prof1: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_pretty $(ERL_PATH) \
            -s megaco_codec_v1_SUITE profile_decode_pretty_text_messages \
            $(ESTOP)

decode_pretty_flex_prof1: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_pretty_flex $(ERL_PATH) \
            -s megaco_codec_v1_SUITE profile_decode_pretty_flex_text_messages \
            $(ESTOP)

decode_pretty_prof2: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_pretty $(ERL_PATH) \
            -s megaco_codec_v2_SUITE profile_decode_pretty_text_messages \
            $(ESTOP)

decode_pretty_flex_prof2: make
	$(MERL) $(ARGS) -sname megaco_profile_decode_pretty_flex $(ERL_PATH) \
            -s megaco_codec_v2_SUITE profile_decode_pretty_flex_text_messages \
            $(ESTOP)

encode_compact_prof1: make
	$(MERL) $(ARGS) -sname megaco_profile_encode_compact $(ERL_PATH) \
            -s megaco_codec_v1_SUITE profile_encode_compact_text_messages \
            $(ESTOP)

encode_compact_prof2: make
	$(MERL) $(ARGS) -sname megaco_profile_encode_compact $(ERL_PATH) \
            -s megaco_codec_v2_SUITE profile_encode_compact_text_messages \
            $(ESTOP)

encode_pretty_prof1: make
	$(MERL) $(ARGS) -sname megaco_profile_encode_pretty $(ERL_PATH) \
            -s megaco_codec_v1_SUITE profile_encode_pretty_text_messages \
            $(ESTOP)

encode_pretty_prof2: make
	$(MERL) $(ARGS) -sname megaco_profile_encode_pretty $(ERL_PATH) \
            -s megaco_codec_v2_SUITE profile_encode_pretty_text_messages \
            $(ESTOP)


##########################

tickets: make
	$(MERL) $(ARGS) -sname megaco_tickets $(ERL_PATH) \
            -s megaco_test_lib tickets $(SUITE) \
            $(ESTOP)


##########################

codec: make
	$(MERL) $(ARGS) -sname megaco_codec $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_test \
            $(MAYBE_ESTOP)

codec3c: make
	$(MERL) $(ARGS) -sname megaco_codec3c $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_prev3c_test \
            $(MAYBE_ESTOP)

codec3c_tickets: make
	$(MERL) $(ARGS) -sname megaco_codec3c_tickets $(ERL_PATH) \
            -s megaco_codec_prev3c_test tickets \
            $(ESTOP)

codec3: make
	$(MERL) $(ARGS) -sname megaco_codec3 $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_v3_test \
            $(MAYBE_ESTOP)

codec3_tickets: make
	$(MERL) $(ARGS) -sname megaco_codec3_tickets $(ERL_PATH) \
            -s megaco_codec_v3_test tickets \
            $(ESTOP)


##########################

time1: make
	$(MERL) $(ARGS) -sname megaco_time1 $(ERL_PATH) \
            -run megaco_codec_v1_SUITE tt $(TT_DIR) \
            $(ESTOP)

time2: make
	$(MERL) $(ARGS) -sname megaco_time2 $(ERL_PATH) \
            -run megaco_codec_v2_SUITE tt $(TT_DIR) \
            $(ESTOP)

timeo1: make
	$(MERL) $(ARGS) -sname megaco_timeo1 $(ERL_PATH) \
            -run megaco_codec_v1_SUITE tt_official $(TT_DIR) \
            $(ESTOP)

timeo2: make
	$(MERL) $(ARGS) -sname megaco_timeo2 $(ERL_PATH) \
            -run megaco_codec_v2_SUITE tt_official $(TT_DIR) \
            $(ESTOP)

timeo3: make
	$(MERL) $(ARGS) -sname megaco_timeo3 $(ERL_PATH) \
            -run megaco_codec_v3_test tt_official $(TT_DIR) \
            $(ESTOP)

timet1: make
	$(MERL) $(ARGS) -sname megaco_timet1 $(ERL_PATH) \
            -run megaco_codec_v1_SUITE tt_texts $(TT_DIR) \
            $(ESTOP)

timet2: make
	$(MERL) $(ARGS) -sname megaco_timet2 $(ERL_PATH) \
            -run megaco_codec_v2_SUITE tt_texts $(TT_DIR) \
            $(ESTOP)

timet3: make
	$(MERL) $(ARGS) -sname megaco_timet3 $(ERL_PATH) \
            -run megaco_codec_v3_test tt_texts $(TT_DIR) \
            $(ESTOP)

timeb1: make
	$(MERL) $(ARGS) -sname megaco_timeb1 $(ERL_PATH) \
            -run megaco_codec_v1_SUITE tt_bins $(TT_DIR) \
            $(ESTOP)

timeb2: make
	$(MERL) $(ARGS) -sname megaco_timeb2 $(ERL_PATH) \
            -run megaco_codec_v2_SUITE tt_bins $(TT_DIR) \
            $(ESTOP)

timeb3: make
	$(MERL) $(ARGS) -sname megaco_timeb3 $(ERL_PATH) \
            -run megaco_codec_v3_test tt_bins $(TT_DIR) \
            $(ESTOP)


###################

gnuplot_gif: make
	$(MERL) $(ARGS) -sname megaco_gnuplot_gif $(ERL_PATH) \
            -s megaco_call_flow_test gnuplot_gif \
            $(ESTOP)

display_v1: make
	$(MERL) $(ARGS) -sname megaco_display_text_msgs_v1 $(ERL_PATH) \
            -s megaco_codec_v1_SUITE display_text_messages \
            $(ESTOP)

generate_v1: make
	$(MERL) $(ARGS) -sname megaco_generate_text_msgs_v1 $(ERL_PATH) \
            -s megaco_codec_v1_SUITE generate_text_messages \
            $(ESTOP)

display_v2: make
	$(MERL) $(ARGS) -sname megaco_display_text_msgs_v2 $(ERL_PATH) \
            -s megaco_codec_v2_SUITE display_text_messages \
            $(ESTOP)

generate_v2: make
	$(MERL) $(ARGS) -sname megaco_generate_text_msgs_v2 $(ERL_PATH) \
            -s megaco_codec_v2_SUITE generate_text_messages \
            $(ESTOP)

display_prev3a: make
	$(MERL) $(ARGS) -sname megaco_display_text_msgs_prev3a $(ERL_PATH) \
            -s megaco_codec_prev3a_SUITE display_text_messages \
            $(ESTOP)

display_prev3b: make
	$(MERL) $(ARGS) -sname megaco_display_text_msgs_prev3b $(ERL_PATH) \
            -s megaco_codec_prev3b_SUITE display_text_messages \
            $(ESTOP)

generate_prev3b: make
	$(MERL) $(ARGS) -sname megaco_generate_text_msgs_prev3b $(ERL_PATH) \
            -s megaco_codec_prev3b_SUITE generate_text_messages \
            $(ESTOP)

display_prev3c: make
	$(MERL) $(ARGS) -sname megaco_display_text_msgs_prev3c $(ERL_PATH) \
            -s megaco_codec_prev3c_test display_text_messages \
            $(ESTOP)

generate_prev3c: make
	$(MERL) $(ARGS) -sname megaco_generate_text_msgs_prev3c $(ERL_PATH) \
            -s megaco_codec_prev3c_test generate_text_messages \
            $(ESTOP)

display_v3: make
	$(MERL) $(ARGS) -sname megaco_display_text_msgs_v3 $(ERL_PATH) \
            -s megaco_codec_v3_test display_text_messages \
            $(ESTOP)

generate_v3: make
	$(MERL) $(ARGS) -sname megaco_generate_text_msgs_v3 $(ERL_PATH) \
            -s megaco_codec_v3_test generate_text_messages \
            $(ESTOP)

generate: make
	$(MERL) $(ARGS) -sname megaco_generate_text_msgs $(ERL_PATH) \
            -s megaco_codec_v1_SUITE generate_text_messages \
            -s megaco_codec_v2_SUITE generate_text_messages \
            -s megaco_codec_v3_test generate_text_messages \
            $(ESTOP)

node:
	$(MERL) -sname megaco $(ERL_PATH)


# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  "$(RELSYSDIR)"
	$(INSTALL_DATA) $(RELTEST_FILES) "$(RELSYSDIR)"
#	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) \
#                        $(HRL_FILES) $(ERL_FILES) \
#                        "$(RELSYSDIR)"
#
	chmod -R u+w "$(RELSYSDIR)"

