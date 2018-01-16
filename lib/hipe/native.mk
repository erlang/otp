ifeq ($(NATIVE_LIBS_ENABLED),yes)
ifndef SECONDARY_BOOTSTRAP
ERL_COMPILE_FLAGS += +native "+{hipe,[verify_gcsafe]}"
else
EBIN = ../boot_ebin
endif
endif
