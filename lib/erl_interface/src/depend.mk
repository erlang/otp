# Generated dependency rules
$(ST_OBJDIR)/ei_connect.o: connect/ei_connect.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h misc/eiext.h misc/ei_portio.h \
  misc/ei_internal.h connect/ei_connect_int.h misc/ei_locking.h \
  connect/eisend.h connect/eirecv.h misc/eimd5.h misc/putget.h \
  connect/ei_resolve.h epmd/ei_epmd.h
$(ST_OBJDIR)/ei_resolve.o: connect/ei_resolve.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h connect/ei_resolve.h misc/ei_locking.h
$(ST_OBJDIR)/eirecv.o: connect/eirecv.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eirecv.h misc/ei_portio.h \
  misc/ei_internal.h misc/putget.h misc/ei_trace.h misc/show_msg.h
$(ST_OBJDIR)/send.o: connect/send.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(ST_OBJDIR)/send_exit.o: connect/send_exit.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/ei_connect_int.h misc/ei_trace.h \
  misc/ei_internal.h misc/putget.h misc/show_msg.h
$(ST_OBJDIR)/send_reg.o: connect/send_reg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(ST_OBJDIR)/decode_atom.o: decode/decode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_big.o: decode/decode_big.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_bignum.o: decode/decode_bignum.c $(TARGET)/config.h
$(ST_OBJDIR)/decode_binary.o: decode/decode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_boolean.o: decode/decode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_char.o: decode/decode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_double.o: decode/decode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_fun.o: decode/decode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_malloc.h decode/decode_skip.h misc/putget.h
$(ST_OBJDIR)/decode_intlist.o: decode/decode_intlist.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_list_header.o: decode/decode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_long.o: decode/decode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_pid.o: decode/decode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_port.o: decode/decode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_ref.o: decode/decode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_skip.o: decode/decode_skip.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  decode/decode_skip.h
$(ST_OBJDIR)/decode_string.o: decode/decode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_trace.o: decode/decode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(ST_OBJDIR)/decode_tuple_header.o: decode/decode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_ulong.o: decode/decode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_version.o: decode/decode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_longlong.o: decode/decode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/decode_ulonglong.o: decode/decode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_atom.o: encode/encode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_bignum.o: encode/encode_bignum.c $(TARGET)/config.h
$(ST_OBJDIR)/encode_binary.o: encode/encode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_boolean.o: encode/encode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_char.o: encode/encode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_double.o: encode/encode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_fun.o: encode/encode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_list_header.o: encode/encode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_long.o: encode/encode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_pid.o: encode/encode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_port.o: encode/encode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_ref.o: encode/encode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_string.o: encode/encode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_trace.o: encode/encode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(ST_OBJDIR)/encode_tuple_header.o: encode/encode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_ulong.o: encode/encode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_version.o: encode/encode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(ST_OBJDIR)/encode_longlong.o: encode/encode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(ST_OBJDIR)/encode_ulonglong.o: encode/encode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(ST_OBJDIR)/epmd_port.o: epmd/epmd_port.c misc/ei_internal.h epmd/ei_epmd.h \
  misc/putget.h
$(ST_OBJDIR)/epmd_publish.o: epmd/epmd_publish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(ST_OBJDIR)/epmd_unpublish.o: epmd/epmd_unpublish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(ST_OBJDIR)/ei_decode_term.o: misc/ei_decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_decode_term.h misc/putget.h
$(ST_OBJDIR)/ei_format.o: misc/ei_format.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_malloc.h misc/ei_format.h
$(ST_OBJDIR)/ei_locking.o: misc/ei_locking.c $(TARGET)/config.h \
  misc/ei_malloc.h misc/ei_locking.h
$(ST_OBJDIR)/ei_malloc.o: misc/ei_malloc.c misc/ei_malloc.h
$(ST_OBJDIR)/ei_portio.o: misc/ei_portio.c misc/ei_portio.h misc/ei_internal.h
$(ST_OBJDIR)/ei_printterm.o: misc/ei_printterm.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_printterm.h misc/ei_malloc.h
$(ST_OBJDIR)/ei_pthreads.o: misc/ei_pthreads.c $(TARGET)/config.h \
  ../include/ei.h misc/ei_locking.h
$(ST_OBJDIR)/ei_trace.o: misc/ei_trace.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_trace.h
$(ST_OBJDIR)/ei_x_encode.o: misc/ei_x_encode.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_x_encode.h \
  misc/ei_malloc.h
$(ST_OBJDIR)/eimd5.o: misc/eimd5.c misc/eimd5.h
$(ST_OBJDIR)/get_type.o: misc/get_type.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h
$(ST_OBJDIR)/show_msg.o: misc/show_msg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h misc/ei_printterm.h \
  misc/ei_internal.h misc/show_msg.h
$(ST_OBJDIR)/ei_compat.o: misc/ei_compat.c ../include/ei.h misc/ei_internal.h
$(ST_OBJDIR)/hash_dohash.o: registry/hash_dohash.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_foreach.o: registry/hash_foreach.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_freetab.o: registry/hash_freetab.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_insert.o: registry/hash_insert.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_isprime.o: registry/hash_isprime.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_lookup.o: registry/hash_lookup.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_newtab.o: registry/hash_newtab.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_remove.o: registry/hash_remove.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_resize.o: registry/hash_resize.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/hash_rlookup.o: registry/hash_rlookup.c registry/hash.h ../include/ei.h
$(ST_OBJDIR)/reg_close.o: registry/reg_close.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_delete.o: registry/reg_delete.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_dirty.o: registry/reg_dirty.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_dump.o: registry/reg_dump.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(ST_OBJDIR)/reg_free.o: registry/reg_free.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_get.o: registry/reg_get.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_getf.o: registry/reg_getf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_geti.o: registry/reg_geti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_getp.o: registry/reg_getp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_gets.o: registry/reg_gets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_make.o: registry/reg_make.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_open.o: registry/reg_open.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_purge.o: registry/reg_purge.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_resize.o: registry/reg_resize.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_restore.o: registry/reg_restore.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(ST_OBJDIR)/reg_set.o: registry/reg_set.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_setf.o: registry/reg_setf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_seti.o: registry/reg_seti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_setp.o: registry/reg_setp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_sets.o: registry/reg_sets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_stat.o: registry/reg_stat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/reg_tabstat.o: registry/reg_tabstat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(ST_OBJDIR)/decode_term.o: legacy/decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h ../include/erl_interface.h
$(ST_OBJDIR)/encode_term.o: legacy/encode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h ../include/erl_interface.h \
  legacy/erl_marshal.h legacy/erl_eterm.h legacy/portability.h
$(ST_OBJDIR)/erl_connect.o: legacy/erl_connect.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_config.h \
  legacy/erl_connect.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h misc/putget.h connect/ei_connect_int.h \
  misc/ei_locking.h epmd/ei_epmd.h misc/ei_internal.h
$(ST_OBJDIR)/erl_error.o: legacy/erl_error.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_error.h
$(ST_OBJDIR)/erl_eterm.o: legacy/erl_eterm.c misc/ei_locking.h \
  $(TARGET)/config.h connect/ei_resolve.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_eterm.h \
  legacy/portability.h legacy/erl_malloc.h legacy/erl_marshal.h \
  legacy/erl_error.h legacy/erl_internal.h misc/ei_internal.h
$(ST_OBJDIR)/erl_fix_alloc.o: legacy/erl_fix_alloc.c $(TARGET)/config.h \
  misc/ei_locking.h ../include/erl_interface.h ../include/ei.h \
  legacy/erl_error.h legacy/erl_malloc.h legacy/erl_fix_alloc.h \
  legacy/erl_eterm.h legacy/portability.h
$(ST_OBJDIR)/erl_format.o: legacy/erl_format.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h legacy/erl_error.h legacy/erl_internal.h
$(ST_OBJDIR)/erl_malloc.o: legacy/erl_malloc.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_fix_alloc.h legacy/erl_malloc.h \
  legacy/erl_internal.h legacy/erl_eterm.h legacy/portability.h \
  misc/ei_malloc.h
$(ST_OBJDIR)/erl_marshal.o: legacy/erl_marshal.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_marshal.h \
  legacy/erl_eterm.h legacy/portability.h legacy/erl_malloc.h \
  legacy/erl_error.h legacy/erl_internal.h misc/eiext.h misc/putget.h
$(ST_OBJDIR)/erl_timeout.o: legacy/erl_timeout.c $(TARGET)/config.h \
  legacy/erl_timeout.h
$(ST_OBJDIR)/global_names.o: legacy/global_names.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(ST_OBJDIR)/global_register.o: legacy/global_register.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h ../include/erl_interface.h
$(ST_OBJDIR)/global_unregister.o: legacy/global_unregister.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(ST_OBJDIR)/global_whereis.o: legacy/global_whereis.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h

$(MT_OBJDIR)/ei_connect.o: connect/ei_connect.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h misc/eiext.h misc/ei_portio.h \
  misc/ei_internal.h connect/ei_connect_int.h misc/ei_locking.h \
  connect/eisend.h connect/eirecv.h misc/eimd5.h misc/putget.h \
  connect/ei_resolve.h epmd/ei_epmd.h
$(MT_OBJDIR)/ei_resolve.o: connect/ei_resolve.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h connect/ei_resolve.h misc/ei_locking.h
$(MT_OBJDIR)/eirecv.o: connect/eirecv.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eirecv.h misc/ei_portio.h \
  misc/ei_internal.h misc/putget.h misc/ei_trace.h misc/show_msg.h
$(MT_OBJDIR)/send.o: connect/send.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(MT_OBJDIR)/send_exit.o: connect/send_exit.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/ei_connect_int.h misc/ei_trace.h \
  misc/ei_internal.h misc/putget.h misc/show_msg.h
$(MT_OBJDIR)/send_reg.o: connect/send_reg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(MT_OBJDIR)/decode_atom.o: decode/decode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_big.o: decode/decode_big.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_bignum.o: decode/decode_bignum.c $(TARGET)/config.h
$(MT_OBJDIR)/decode_binary.o: decode/decode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_boolean.o: decode/decode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_char.o: decode/decode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_double.o: decode/decode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_fun.o: decode/decode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_malloc.h decode/decode_skip.h misc/putget.h
$(MT_OBJDIR)/decode_intlist.o: decode/decode_intlist.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_list_header.o: decode/decode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_long.o: decode/decode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_pid.o: decode/decode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_port.o: decode/decode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_ref.o: decode/decode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_skip.o: decode/decode_skip.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  decode/decode_skip.h
$(MT_OBJDIR)/decode_string.o: decode/decode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_trace.o: decode/decode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(MT_OBJDIR)/decode_tuple_header.o: decode/decode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_ulong.o: decode/decode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_version.o: decode/decode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_longlong.o: decode/decode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/decode_ulonglong.o: decode/decode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_atom.o: encode/encode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_bignum.o: encode/encode_bignum.c $(TARGET)/config.h
$(MT_OBJDIR)/encode_binary.o: encode/encode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_boolean.o: encode/encode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_char.o: encode/encode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_double.o: encode/encode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_fun.o: encode/encode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_list_header.o: encode/encode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_long.o: encode/encode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_pid.o: encode/encode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_port.o: encode/encode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_ref.o: encode/encode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_string.o: encode/encode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_trace.o: encode/encode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(MT_OBJDIR)/encode_tuple_header.o: encode/encode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_ulong.o: encode/encode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_version.o: encode/encode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MT_OBJDIR)/encode_longlong.o: encode/encode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(MT_OBJDIR)/encode_ulonglong.o: encode/encode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(MT_OBJDIR)/epmd_port.o: epmd/epmd_port.c misc/ei_internal.h epmd/ei_epmd.h \
  misc/putget.h
$(MT_OBJDIR)/epmd_publish.o: epmd/epmd_publish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(MT_OBJDIR)/epmd_unpublish.o: epmd/epmd_unpublish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(MT_OBJDIR)/ei_decode_term.o: misc/ei_decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_decode_term.h misc/putget.h
$(MT_OBJDIR)/ei_format.o: misc/ei_format.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_malloc.h misc/ei_format.h
$(MT_OBJDIR)/ei_locking.o: misc/ei_locking.c $(TARGET)/config.h \
  misc/ei_malloc.h misc/ei_locking.h
$(MT_OBJDIR)/ei_malloc.o: misc/ei_malloc.c misc/ei_malloc.h
$(MT_OBJDIR)/ei_portio.o: misc/ei_portio.c misc/ei_portio.h misc/ei_internal.h
$(MT_OBJDIR)/ei_printterm.o: misc/ei_printterm.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_printterm.h misc/ei_malloc.h
$(MT_OBJDIR)/ei_pthreads.o: misc/ei_pthreads.c $(TARGET)/config.h \
  ../include/ei.h misc/ei_locking.h
$(MT_OBJDIR)/ei_trace.o: misc/ei_trace.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_trace.h
$(MT_OBJDIR)/ei_x_encode.o: misc/ei_x_encode.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_x_encode.h \
  misc/ei_malloc.h
$(MT_OBJDIR)/eimd5.o: misc/eimd5.c misc/eimd5.h
$(MT_OBJDIR)/get_type.o: misc/get_type.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h
$(MT_OBJDIR)/show_msg.o: misc/show_msg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h misc/ei_printterm.h \
  misc/ei_internal.h misc/show_msg.h
$(MT_OBJDIR)/ei_compat.o: misc/ei_compat.c ../include/ei.h misc/ei_internal.h
$(MT_OBJDIR)/hash_dohash.o: registry/hash_dohash.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_foreach.o: registry/hash_foreach.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_freetab.o: registry/hash_freetab.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_insert.o: registry/hash_insert.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_isprime.o: registry/hash_isprime.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_lookup.o: registry/hash_lookup.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_newtab.o: registry/hash_newtab.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_remove.o: registry/hash_remove.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_resize.o: registry/hash_resize.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/hash_rlookup.o: registry/hash_rlookup.c registry/hash.h ../include/ei.h
$(MT_OBJDIR)/reg_close.o: registry/reg_close.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_delete.o: registry/reg_delete.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_dirty.o: registry/reg_dirty.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_dump.o: registry/reg_dump.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(MT_OBJDIR)/reg_free.o: registry/reg_free.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_get.o: registry/reg_get.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_getf.o: registry/reg_getf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_geti.o: registry/reg_geti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_getp.o: registry/reg_getp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_gets.o: registry/reg_gets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_make.o: registry/reg_make.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_open.o: registry/reg_open.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_purge.o: registry/reg_purge.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_resize.o: registry/reg_resize.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_restore.o: registry/reg_restore.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(MT_OBJDIR)/reg_set.o: registry/reg_set.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_setf.o: registry/reg_setf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_seti.o: registry/reg_seti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_setp.o: registry/reg_setp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_sets.o: registry/reg_sets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_stat.o: registry/reg_stat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/reg_tabstat.o: registry/reg_tabstat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MT_OBJDIR)/decode_term.o: legacy/decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h ../include/erl_interface.h
$(MT_OBJDIR)/encode_term.o: legacy/encode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h ../include/erl_interface.h \
  legacy/erl_marshal.h legacy/erl_eterm.h legacy/portability.h
$(MT_OBJDIR)/erl_connect.o: legacy/erl_connect.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_config.h \
  legacy/erl_connect.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h misc/putget.h connect/ei_connect_int.h \
  misc/ei_locking.h epmd/ei_epmd.h misc/ei_internal.h
$(MT_OBJDIR)/erl_error.o: legacy/erl_error.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_error.h
$(MT_OBJDIR)/erl_eterm.o: legacy/erl_eterm.c misc/ei_locking.h \
  $(TARGET)/config.h connect/ei_resolve.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_eterm.h \
  legacy/portability.h legacy/erl_malloc.h legacy/erl_marshal.h \
  legacy/erl_error.h legacy/erl_internal.h misc/ei_internal.h
$(MT_OBJDIR)/erl_fix_alloc.o: legacy/erl_fix_alloc.c $(TARGET)/config.h \
  misc/ei_locking.h ../include/erl_interface.h ../include/ei.h \
  legacy/erl_error.h legacy/erl_malloc.h legacy/erl_fix_alloc.h \
  legacy/erl_eterm.h legacy/portability.h
$(MT_OBJDIR)/erl_format.o: legacy/erl_format.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h legacy/erl_error.h legacy/erl_internal.h
$(MT_OBJDIR)/erl_malloc.o: legacy/erl_malloc.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_fix_alloc.h legacy/erl_malloc.h \
  legacy/erl_internal.h legacy/erl_eterm.h legacy/portability.h \
  misc/ei_malloc.h
$(MT_OBJDIR)/erl_marshal.o: legacy/erl_marshal.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_marshal.h \
  legacy/erl_eterm.h legacy/portability.h legacy/erl_malloc.h \
  legacy/erl_error.h legacy/erl_internal.h misc/eiext.h misc/putget.h
$(MT_OBJDIR)/erl_timeout.o: legacy/erl_timeout.c $(TARGET)/config.h \
  legacy/erl_timeout.h
$(MT_OBJDIR)/global_names.o: legacy/global_names.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(MT_OBJDIR)/global_register.o: legacy/global_register.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h ../include/erl_interface.h
$(MT_OBJDIR)/global_unregister.o: legacy/global_unregister.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(MT_OBJDIR)/global_whereis.o: legacy/global_whereis.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h

$(MD_OBJDIR)/ei_connect.o: connect/ei_connect.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h misc/eiext.h misc/ei_portio.h \
  misc/ei_internal.h connect/ei_connect_int.h misc/ei_locking.h \
  connect/eisend.h connect/eirecv.h misc/eimd5.h misc/putget.h \
  connect/ei_resolve.h epmd/ei_epmd.h
$(MD_OBJDIR)/ei_resolve.o: connect/ei_resolve.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h connect/ei_resolve.h misc/ei_locking.h
$(MD_OBJDIR)/eirecv.o: connect/eirecv.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eirecv.h misc/ei_portio.h \
  misc/ei_internal.h misc/putget.h misc/ei_trace.h misc/show_msg.h
$(MD_OBJDIR)/send.o: connect/send.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(MD_OBJDIR)/send_exit.o: connect/send_exit.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/ei_connect_int.h misc/ei_trace.h \
  misc/ei_internal.h misc/putget.h misc/show_msg.h
$(MD_OBJDIR)/send_reg.o: connect/send_reg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(MD_OBJDIR)/decode_atom.o: decode/decode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_big.o: decode/decode_big.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_bignum.o: decode/decode_bignum.c $(TARGET)/config.h
$(MD_OBJDIR)/decode_binary.o: decode/decode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_boolean.o: decode/decode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_char.o: decode/decode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_double.o: decode/decode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_fun.o: decode/decode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_malloc.h decode/decode_skip.h misc/putget.h
$(MD_OBJDIR)/decode_intlist.o: decode/decode_intlist.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_list_header.o: decode/decode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_long.o: decode/decode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_pid.o: decode/decode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_port.o: decode/decode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_ref.o: decode/decode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_skip.o: decode/decode_skip.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  decode/decode_skip.h
$(MD_OBJDIR)/decode_string.o: decode/decode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_trace.o: decode/decode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(MD_OBJDIR)/decode_tuple_header.o: decode/decode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_ulong.o: decode/decode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_version.o: decode/decode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_longlong.o: decode/decode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/decode_ulonglong.o: decode/decode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_atom.o: encode/encode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_bignum.o: encode/encode_bignum.c $(TARGET)/config.h
$(MD_OBJDIR)/encode_binary.o: encode/encode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_boolean.o: encode/encode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_char.o: encode/encode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_double.o: encode/encode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_fun.o: encode/encode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_list_header.o: encode/encode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_long.o: encode/encode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_pid.o: encode/encode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_port.o: encode/encode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_ref.o: encode/encode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_string.o: encode/encode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_trace.o: encode/encode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(MD_OBJDIR)/encode_tuple_header.o: encode/encode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_ulong.o: encode/encode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_version.o: encode/encode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MD_OBJDIR)/encode_longlong.o: encode/encode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(MD_OBJDIR)/encode_ulonglong.o: encode/encode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(MD_OBJDIR)/epmd_port.o: epmd/epmd_port.c misc/ei_internal.h epmd/ei_epmd.h \
  misc/putget.h
$(MD_OBJDIR)/epmd_publish.o: epmd/epmd_publish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(MD_OBJDIR)/epmd_unpublish.o: epmd/epmd_unpublish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(MD_OBJDIR)/ei_decode_term.o: misc/ei_decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_decode_term.h misc/putget.h
$(MD_OBJDIR)/ei_format.o: misc/ei_format.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_malloc.h misc/ei_format.h
$(MD_OBJDIR)/ei_locking.o: misc/ei_locking.c $(TARGET)/config.h \
  misc/ei_malloc.h misc/ei_locking.h
$(MD_OBJDIR)/ei_malloc.o: misc/ei_malloc.c misc/ei_malloc.h
$(MD_OBJDIR)/ei_portio.o: misc/ei_portio.c misc/ei_portio.h misc/ei_internal.h
$(MD_OBJDIR)/ei_printterm.o: misc/ei_printterm.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_printterm.h misc/ei_malloc.h
$(MD_OBJDIR)/ei_pthreads.o: misc/ei_pthreads.c $(TARGET)/config.h \
  ../include/ei.h misc/ei_locking.h
$(MD_OBJDIR)/ei_trace.o: misc/ei_trace.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_trace.h
$(MD_OBJDIR)/ei_x_encode.o: misc/ei_x_encode.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_x_encode.h \
  misc/ei_malloc.h
$(MD_OBJDIR)/eimd5.o: misc/eimd5.c misc/eimd5.h
$(MD_OBJDIR)/get_type.o: misc/get_type.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h
$(MD_OBJDIR)/show_msg.o: misc/show_msg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h misc/ei_printterm.h \
  misc/ei_internal.h misc/show_msg.h
$(MD_OBJDIR)/ei_compat.o: misc/ei_compat.c ../include/ei.h misc/ei_internal.h
$(MD_OBJDIR)/hash_dohash.o: registry/hash_dohash.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_foreach.o: registry/hash_foreach.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_freetab.o: registry/hash_freetab.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_insert.o: registry/hash_insert.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_isprime.o: registry/hash_isprime.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_lookup.o: registry/hash_lookup.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_newtab.o: registry/hash_newtab.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_remove.o: registry/hash_remove.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_resize.o: registry/hash_resize.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/hash_rlookup.o: registry/hash_rlookup.c registry/hash.h ../include/ei.h
$(MD_OBJDIR)/reg_close.o: registry/reg_close.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_delete.o: registry/reg_delete.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_dirty.o: registry/reg_dirty.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_dump.o: registry/reg_dump.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(MD_OBJDIR)/reg_free.o: registry/reg_free.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_get.o: registry/reg_get.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_getf.o: registry/reg_getf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_geti.o: registry/reg_geti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_getp.o: registry/reg_getp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_gets.o: registry/reg_gets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_make.o: registry/reg_make.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_open.o: registry/reg_open.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_purge.o: registry/reg_purge.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_resize.o: registry/reg_resize.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_restore.o: registry/reg_restore.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(MD_OBJDIR)/reg_set.o: registry/reg_set.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_setf.o: registry/reg_setf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_seti.o: registry/reg_seti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_setp.o: registry/reg_setp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_sets.o: registry/reg_sets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_stat.o: registry/reg_stat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/reg_tabstat.o: registry/reg_tabstat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MD_OBJDIR)/decode_term.o: legacy/decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h ../include/erl_interface.h
$(MD_OBJDIR)/encode_term.o: legacy/encode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h ../include/erl_interface.h \
  legacy/erl_marshal.h legacy/erl_eterm.h legacy/portability.h
$(MD_OBJDIR)/erl_connect.o: legacy/erl_connect.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_config.h \
  legacy/erl_connect.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h misc/putget.h connect/ei_connect_int.h \
  misc/ei_locking.h epmd/ei_epmd.h misc/ei_internal.h
$(MD_OBJDIR)/erl_error.o: legacy/erl_error.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_error.h
$(MD_OBJDIR)/erl_eterm.o: legacy/erl_eterm.c misc/ei_locking.h \
  $(TARGET)/config.h connect/ei_resolve.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_eterm.h \
  legacy/portability.h legacy/erl_malloc.h legacy/erl_marshal.h \
  legacy/erl_error.h legacy/erl_internal.h misc/ei_internal.h
$(MD_OBJDIR)/erl_fix_alloc.o: legacy/erl_fix_alloc.c $(TARGET)/config.h \
  misc/ei_locking.h ../include/erl_interface.h ../include/ei.h \
  legacy/erl_error.h legacy/erl_malloc.h legacy/erl_fix_alloc.h \
  legacy/erl_eterm.h legacy/portability.h
$(MD_OBJDIR)/erl_format.o: legacy/erl_format.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h legacy/erl_error.h legacy/erl_internal.h
$(MD_OBJDIR)/erl_malloc.o: legacy/erl_malloc.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_fix_alloc.h legacy/erl_malloc.h \
  legacy/erl_internal.h legacy/erl_eterm.h legacy/portability.h \
  misc/ei_malloc.h
$(MD_OBJDIR)/erl_marshal.o: legacy/erl_marshal.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_marshal.h \
  legacy/erl_eterm.h legacy/portability.h legacy/erl_malloc.h \
  legacy/erl_error.h legacy/erl_internal.h misc/eiext.h misc/putget.h
$(MD_OBJDIR)/erl_timeout.o: legacy/erl_timeout.c $(TARGET)/config.h \
  legacy/erl_timeout.h
$(MD_OBJDIR)/global_names.o: legacy/global_names.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(MD_OBJDIR)/global_register.o: legacy/global_register.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h ../include/erl_interface.h
$(MD_OBJDIR)/global_unregister.o: legacy/global_unregister.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(MD_OBJDIR)/global_whereis.o: legacy/global_whereis.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h

$(MDD_OBJDIR)/ei_connect.o: connect/ei_connect.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h misc/eiext.h misc/ei_portio.h \
  misc/ei_internal.h connect/ei_connect_int.h misc/ei_locking.h \
  connect/eisend.h connect/eirecv.h misc/eimd5.h misc/putget.h \
  connect/ei_resolve.h epmd/ei_epmd.h
$(MDD_OBJDIR)/ei_resolve.o: connect/ei_resolve.c $(TARGET)/config.h \
  misc/eidef.h ../include/ei.h connect/ei_resolve.h misc/ei_locking.h
$(MDD_OBJDIR)/eirecv.o: connect/eirecv.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eirecv.h misc/ei_portio.h \
  misc/ei_internal.h misc/putget.h misc/ei_trace.h misc/show_msg.h
$(MDD_OBJDIR)/send.o: connect/send.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(MDD_OBJDIR)/send_exit.o: connect/send_exit.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/ei_connect_int.h misc/ei_trace.h \
  misc/ei_internal.h misc/putget.h misc/show_msg.h
$(MDD_OBJDIR)/send_reg.o: connect/send_reg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h connect/eisend.h misc/putget.h \
  connect/ei_connect_int.h misc/ei_internal.h misc/ei_trace.h \
  misc/show_msg.h
$(MDD_OBJDIR)/decode_atom.o: decode/decode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_big.o: decode/decode_big.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_bignum.o: decode/decode_bignum.c $(TARGET)/config.h
$(MDD_OBJDIR)/decode_binary.o: decode/decode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_boolean.o: decode/decode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_char.o: decode/decode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_double.o: decode/decode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_fun.o: decode/decode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_malloc.h decode/decode_skip.h misc/putget.h
$(MDD_OBJDIR)/decode_intlist.o: decode/decode_intlist.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_list_header.o: decode/decode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_long.o: decode/decode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_pid.o: decode/decode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_port.o: decode/decode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_ref.o: decode/decode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_skip.o: decode/decode_skip.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  decode/decode_skip.h
$(MDD_OBJDIR)/decode_string.o: decode/decode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_trace.o: decode/decode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(MDD_OBJDIR)/decode_tuple_header.o: decode/decode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_ulong.o: decode/decode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_version.o: decode/decode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_longlong.o: decode/decode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/decode_ulonglong.o: decode/decode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_atom.o: encode/encode_atom.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_bignum.o: encode/encode_bignum.c $(TARGET)/config.h
$(MDD_OBJDIR)/encode_binary.o: encode/encode_binary.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_boolean.o: encode/encode_boolean.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_char.o: encode/encode_char.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_double.o: encode/encode_double.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_fun.o: encode/encode_fun.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_list_header.o: encode/encode_list_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_long.o: encode/encode_long.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_pid.o: encode/encode_pid.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_port.o: encode/encode_port.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_ref.o: encode/encode_ref.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_string.o: encode/encode_string.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_trace.o: encode/encode_trace.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/putget.h
$(MDD_OBJDIR)/encode_tuple_header.o: encode/encode_tuple_header.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_ulong.o: encode/encode_ulong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_version.o: encode/encode_version.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h
$(MDD_OBJDIR)/encode_longlong.o: encode/encode_longlong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(MDD_OBJDIR)/encode_ulonglong.o: encode/encode_ulonglong.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h
$(MDD_OBJDIR)/epmd_port.o: epmd/epmd_port.c misc/ei_internal.h epmd/ei_epmd.h \
  misc/putget.h
$(MDD_OBJDIR)/epmd_publish.o: epmd/epmd_publish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(MDD_OBJDIR)/epmd_unpublish.o: epmd/epmd_unpublish.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_internal.h \
  misc/putget.h ../include/erl_interface.h epmd/ei_epmd.h
$(MDD_OBJDIR)/ei_decode_term.o: misc/ei_decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_decode_term.h misc/putget.h
$(MDD_OBJDIR)/ei_format.o: misc/ei_format.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_malloc.h misc/ei_format.h
$(MDD_OBJDIR)/ei_locking.o: misc/ei_locking.c $(TARGET)/config.h \
  misc/ei_malloc.h misc/ei_locking.h
$(MDD_OBJDIR)/ei_malloc.o: misc/ei_malloc.c misc/ei_malloc.h
$(MDD_OBJDIR)/ei_portio.o: misc/ei_portio.c misc/ei_portio.h misc/ei_internal.h
$(MDD_OBJDIR)/ei_printterm.o: misc/ei_printterm.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/ei_printterm.h misc/ei_malloc.h
$(MDD_OBJDIR)/ei_pthreads.o: misc/ei_pthreads.c $(TARGET)/config.h \
  ../include/ei.h misc/ei_locking.h
$(MDD_OBJDIR)/ei_trace.o: misc/ei_trace.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/ei_trace.h
$(MDD_OBJDIR)/ei_x_encode.o: misc/ei_x_encode.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/ei_x_encode.h \
  misc/ei_malloc.h
$(MDD_OBJDIR)/eimd5.o: misc/eimd5.c misc/eimd5.h
$(MDD_OBJDIR)/get_type.o: misc/get_type.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h
$(MDD_OBJDIR)/show_msg.o: misc/show_msg.c misc/eidef.h $(TARGET)/config.h \
  ../include/ei.h misc/eiext.h misc/putget.h misc/ei_printterm.h \
  misc/ei_internal.h misc/show_msg.h
$(MDD_OBJDIR)/ei_compat.o: misc/ei_compat.c ../include/ei.h misc/ei_internal.h
$(MDD_OBJDIR)/hash_dohash.o: registry/hash_dohash.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_foreach.o: registry/hash_foreach.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_freetab.o: registry/hash_freetab.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_insert.o: registry/hash_insert.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_isprime.o: registry/hash_isprime.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_lookup.o: registry/hash_lookup.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_newtab.o: registry/hash_newtab.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_remove.o: registry/hash_remove.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_resize.o: registry/hash_resize.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/hash_rlookup.o: registry/hash_rlookup.c registry/hash.h ../include/ei.h
$(MDD_OBJDIR)/reg_close.o: registry/reg_close.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_delete.o: registry/reg_delete.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_dirty.o: registry/reg_dirty.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_dump.o: registry/reg_dump.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(MDD_OBJDIR)/reg_free.o: registry/reg_free.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_get.o: registry/reg_get.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_getf.o: registry/reg_getf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_geti.o: registry/reg_geti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_getp.o: registry/reg_getp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_gets.o: registry/reg_gets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_make.o: registry/reg_make.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_open.o: registry/reg_open.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_purge.o: registry/reg_purge.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_resize.o: registry/reg_resize.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_restore.o: registry/reg_restore.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  registry/reg.h registry/hash.h connect/eisend.h connect/eirecv.h \
  connect/ei_connect_int.h
$(MDD_OBJDIR)/reg_set.o: registry/reg_set.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_setf.o: registry/reg_setf.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_seti.o: registry/reg_seti.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_setp.o: registry/reg_setp.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_sets.o: registry/reg_sets.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_stat.o: registry/reg_stat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/reg_tabstat.o: registry/reg_tabstat.c registry/reg.h ../include/ei.h \
  registry/hash.h
$(MDD_OBJDIR)/decode_term.o: legacy/decode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h ../include/erl_interface.h
$(MDD_OBJDIR)/encode_term.o: legacy/encode_term.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  misc/putget.h misc/ei_x_encode.h ../include/erl_interface.h \
  legacy/erl_marshal.h legacy/erl_eterm.h legacy/portability.h
$(MDD_OBJDIR)/erl_connect.o: legacy/erl_connect.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_config.h \
  legacy/erl_connect.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h misc/putget.h connect/ei_connect_int.h \
  misc/ei_locking.h epmd/ei_epmd.h misc/ei_internal.h
$(MDD_OBJDIR)/erl_error.o: legacy/erl_error.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_error.h
$(MDD_OBJDIR)/erl_eterm.o: legacy/erl_eterm.c misc/ei_locking.h \
  $(TARGET)/config.h connect/ei_resolve.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_eterm.h \
  legacy/portability.h legacy/erl_malloc.h legacy/erl_marshal.h \
  legacy/erl_error.h legacy/erl_internal.h misc/ei_internal.h
$(MDD_OBJDIR)/erl_fix_alloc.o: legacy/erl_fix_alloc.c $(TARGET)/config.h \
  misc/ei_locking.h ../include/erl_interface.h ../include/ei.h \
  legacy/erl_error.h legacy/erl_malloc.h legacy/erl_fix_alloc.h \
  legacy/erl_eterm.h legacy/portability.h
$(MDD_OBJDIR)/erl_format.o: legacy/erl_format.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_eterm.h legacy/portability.h \
  legacy/erl_malloc.h legacy/erl_error.h legacy/erl_internal.h
$(MDD_OBJDIR)/erl_malloc.o: legacy/erl_malloc.c ../include/erl_interface.h \
  ../include/ei.h legacy/erl_fix_alloc.h legacy/erl_malloc.h \
  legacy/erl_internal.h legacy/erl_eterm.h legacy/portability.h \
  misc/ei_malloc.h
$(MDD_OBJDIR)/erl_marshal.o: legacy/erl_marshal.c $(TARGET)/config.h \
  ../include/erl_interface.h ../include/ei.h legacy/erl_marshal.h \
  legacy/erl_eterm.h legacy/portability.h legacy/erl_malloc.h \
  legacy/erl_error.h legacy/erl_internal.h misc/eiext.h misc/putget.h
$(MDD_OBJDIR)/erl_timeout.o: legacy/erl_timeout.c $(TARGET)/config.h \
  legacy/erl_timeout.h
$(MDD_OBJDIR)/global_names.o: legacy/global_names.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(MDD_OBJDIR)/global_register.o: legacy/global_register.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h ../include/erl_interface.h
$(MDD_OBJDIR)/global_unregister.o: legacy/global_unregister.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h
$(MDD_OBJDIR)/global_whereis.o: legacy/global_whereis.c misc/eidef.h \
  $(TARGET)/config.h ../include/ei.h misc/eiext.h \
  connect/eisend.h connect/eirecv.h connect/ei_connect_int.h \
  ../include/erl_interface.h legacy/erl_connect.h

