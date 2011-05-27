ifeq ($(OVERRIDE_TARGET),)

ifeq ($(TARGET),)

TARGET := $(shell $(DIAMETER_TOP)/autoconf/config.guess)

else

endif

else

ifneq ($(TARGET),)

ifneq ($(TARGET), $(OVERRIDE_TARGET))
$(warning overriding $$(TARGET) = \
	"$(TARGET)" \
	with \
	$$(OVERRIDE_TARGET) = \
	"$(OVERRIDE_TARGET)")
else
endif

override TARGET := $(OVERRIDE_TARGET)

else

TARGET := $(OVERRIDE_TARGET)

endif

endif

