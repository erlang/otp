# This file is included by GNUmakefile

YCF_INCLUDE_DIRS = \
	-I$(YCF_SOURCE_DIR) \
	-I$(YCF_SOURCE_DIR)/lib/tiny_regex_c \
	-I$(YCF_SOURCE_DIR)/lib/simple_c_gc

YCF_HEADERS = $(sort $(shell find $(YCF_SOURCE_DIR) -name '*.h'))

YCF_EXTRA_SOURCES = \
	$(YCF_SOURCE_DIR)/lib/tiny_regex_c/re.c \
	$(YCF_SOURCE_DIR)/lib/simple_c_gc/simple_c_gc.c

YCF_SOURCES = $(sort $(wildcard $(YCF_SOURCE_DIR)/*.c) $(YCF_EXTRA_SOURCES))

YCF_OBJECTS = $(patsubst $(YCF_SOURCE_DIR)/%.c, $(YCF_SOURCE_DIR)/%.o, $(YCF_SOURCES))

YCF_CFLAGS = $(filter-out -Wstrict-prototypes -Wdeclaration-after-statement -Wmissing-prototypes,$(CFLAGS))

YCF_EXECUTABLE = $(YCF_SOURCE_DIR)/bin/yielding_c_fun.bin

$(YCF_EXECUTABLE): $(YCF_OBJECTS)
	$(LD) $(YCF_CFLAGS) $(LDFLAGS) $(YCF_OBJECTS) -o $@

$(YCF_SOURCE_DIR)/%.o: $(YCF_SOURCE_DIR)/%.c $(YCF_HEADERS)
	$(CC) $(YCF_CFLAGS) $(LDFLAGS) $(YCF_INCLUDE_DIRS) -c $< -o $@
