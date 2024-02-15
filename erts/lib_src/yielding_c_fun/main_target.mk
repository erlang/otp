# This file is included by GNUmakefile

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
	-I$(YCF_SOURCE_DIR) \
	-I$(YCF_SOURCE_DIR)/lib/simple_c_gc

YCF_HEADERS = $(sort $(shell find $(YCF_SOURCE_DIR) -name '*.h'))

YCF_EXTRA_SOURCES = \
	$(YCF_SOURCE_DIR)/lib/simple_c_gc/simple_c_gc.c

YCF_SOURCES = $(sort $(wildcard $(YCF_SOURCE_DIR)/*.c) $(YCF_EXTRA_SOURCES))

YCF_OBJECTS = $(addprefix $(YCF_OBJ_DIR)/,$(notdir $(YCF_SOURCES:.c=.o)))

# YCF is a short lived tool leaking memory deliberately. Disable all sanitizers.
YCF_CFLAGS = $(filter-out -Wstrict-prototypes -Wdeclaration-after-statement -Wmissing-prototypes -fsanitize%,$(CFLAGS))
YCF_LDFLAGS = $(filter-out -fsanitize%,$(LDFLAGS))

$(YCF_EXECUTABLE): $(YCF_OBJECTS)
	$(V_LD) $(YCF_CFLAGS) $(YCF_LDFLAGS) $(YCF_OBJECTS) -o $@

$(YCF_OBJ_DIR)/%.o: $(YCF_SOURCE_DIR)/lib/simple_c_gc/%.c $(YCF_HEADERS)
	$(V_CC) $(YCF_CFLAGS) $(YCF_INCLUDE_DIRS) -c $< -o $@

$(YCF_OBJ_DIR)/%.o: $(YCF_SOURCE_DIR)/%.c $(YCF_HEADERS)
	$(V_CC) $(YCF_CFLAGS) $(YCF_INCLUDE_DIRS) -c $< -o $@
