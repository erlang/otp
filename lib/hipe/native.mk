ifeq ($(NATIVE_LIBS_ENABLED),yes)
ifndef SECONDARY_BOOTSTRAP
ERL_COMPILE_FLAGS += +native
else
EBIN = ../boot_ebin
endif
endif
