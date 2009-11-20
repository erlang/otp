ifndef SECONDARY_BOOTSTRAP
ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
endif
endif
