# Generated dependency rules.
# Do *not* edit this file; instead, run 'make depend'.
# 
# emem objects...
$(EMEM_OBJ_DIR)/erl_memory.o: erl_memory.c \
  $(ERL_TOP)/erts/include/erl_fixed_size_int_types.h \
  $(ERL_TOP)/erts/include/$(TARGET)/erl_int_sizes_config.h \
  $(ERL_TOP)/erts/include/erl_memory_trace_parser.h \
  erl_memory_trace_block_table.h \
  $(ERL_TOP)/erts/include/internal/ethread.h \
  $(ERL_TOP)/erts/include/internal/$(TARGET)/ethread_header_config.h
$(EMEM_OBJ_DIR)/erl_memory_trace_block_table.o: erl_memory_trace_block_table.c \
  erl_memory_trace_block_table.h \
  $(ERL_TOP)/erts/include/erl_fixed_size_int_types.h \
  $(ERL_TOP)/erts/include/$(TARGET)/erl_int_sizes_config.h \
  $(ERL_TOP)/erts/include/erl_memory_trace_parser.h
# EOF
