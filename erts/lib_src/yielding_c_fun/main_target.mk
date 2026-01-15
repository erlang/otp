## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2020-2025. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## %CopyrightEnd%

ifndef TARGET
YCF_OBJ_DIR=$(YCF_SOURCE_DIR)
YCF_BIN_DIR=$(YCF_SOURCE_DIR)/bin
YCF_EXECUTABLE=$(YCF_BIN_DIR)/yielding_c_fun.bin$(EXE_SUFFIX)
else
YCF_OBJ_DIR=$(YCF_SOURCE_DIR)/$(TARGET)
YCF_BIN_DIR=$(YCF_SOURCE_DIR)/bin/$(TARGET)
YCF_EXECUTABLE=$(YCF_BIN_DIR)/yielding_c_fun$(EXE_SUFFIX)
_create_dirs := $(shell mkdir -p $(YCF_OBJ_DIR) $(YCF_BIN_DIR))
endif

YCF_INCLUDE_DIRS = \
	-I$(YCF_SOURCE_DIR)

YCF_HEADERS = $(sort $(shell find $(YCF_SOURCE_DIR) -name '*.h'))

YCF_SOURCES = $(sort $(wildcard $(YCF_SOURCE_DIR)/*.c))

YCF_OBJECTS = $(addprefix $(YCF_OBJ_DIR)/,$(notdir $(YCF_SOURCES:.c=.o)))

# YCF is a short lived tool leaking memory deliberately. Disable all sanitizers.
YCF_CFLAGS = $(filter-out -Wstrict-prototypes -Wdeclaration-after-statement -Wmissing-prototypes -fsanitize%,$(CFLAGS))
YCF_LDFLAGS = $(filter-out -fsanitize%,$(LDFLAGS))

$(YCF_EXECUTABLE): $(YCF_OBJECTS)
	$(V_LD) $(YCF_CFLAGS) $(YCF_LDFLAGS) $(YCF_OBJECTS) -o $@

$(YCF_OBJ_DIR)/%.o: $(YCF_SOURCE_DIR)/%.c $(YCF_HEADERS)
	$(V_CC) $(YCF_CFLAGS) $(YCF_INCLUDE_DIRS) -c $< -o $@
