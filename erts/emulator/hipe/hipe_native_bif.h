/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */
/*
 * hipe_native_bif.h
 */
 
#ifndef HIPE_NATIVE_BIF_H
#define HIPE_NATIVE_BIF_H

#include "bif.h"
#include "hipe_arch.h"

/*
 * Prototypes for entry points used by native code.
 */
AEXTERN(Eterm,nbif_callemu,(void));
AEXTERN(int,nbif_suspend_0,(void));	/* caller ignores retval */
AEXTERN(int,nbif_suspend_msg,(void));
AEXTERN(int,nbif_suspend_msg_timeout,(void));

AEXTERN(Eterm,nbif_rethrow,(Process*, Eterm, Eterm));
AEXTERN(Eterm,nbif_raw_raise,(Process*, Eterm, Eterm, Eterm));
AEXTERN(Eterm,nbif_set_timeout,(Process*, Eterm));

AEXTERN(Eterm,nbif_gc_1,(void));

AEXTERN(Eterm,nbif_apply,(void));
AEXTERN(Eterm,nbif_find_na_or_make_stub,(void));
AEXTERN(Eterm,nbif_nonclosure_address,(void));

AEXTERN(Eterm,nbif_add_2,(void));
AEXTERN(Eterm,nbif_sub_2,(void));
AEXTERN(Eterm,nbif_mul_2,(void));

AEXTERN(Eterm,nbif_conv_big_to_float,(void));
AEXTERN(void,nbif_fclearerror_error,(Process*));

AEXTERN(int,nbif_bs_put_big_integer,(void));
AEXTERN(int,nbif_bs_put_small_float,(void));
AEXTERN(void,nbif_bs_put_bits,(void));
AEXTERN(Eterm,nbif_bs_get_integer_2,(void));
AEXTERN(Eterm,nbif_bs_get_float_2,(void));
AEXTERN(Eterm,nbif_bs_get_binary_2,(void));
AEXTERN(char*,nbif_bs_allocate,(void));
AEXTERN(Binary*,nbif_bs_reallocate,(void));
AEXTERN(Eterm,nbif_bs_utf8_size,(Eterm));
AEXTERN(Eterm,nbif_bs_put_utf8,(Process*,Eterm,byte*,unsigned int));
AEXTERN(Eterm,nbif_bs_get_utf8,(void));
AEXTERN(Eterm,nbif_bs_utf16_size,(Eterm));
AEXTERN(Eterm,nbif_bs_put_utf16be,(Process*,Eterm,byte*,unsigned int));
AEXTERN(Eterm,nbif_bs_put_utf16le,(Process*,Eterm,byte*,unsigned int));
AEXTERN(Eterm,nbif_bs_get_utf16,(void));
AEXTERN(Uint,nbif_is_unicode,(Eterm));
AEXTERN(Eterm,nbif_bs_validate_unicode_retract,(void));
AEXTERN(Uint,nbif_is_divisible,(Uint,Uint));

AEXTERN(void,nbif_select_msg,(Process*));
AEXTERN(Eterm,nbif_cmp_2,(void));
AEXTERN(Eterm,nbif_eq_2,(void));

BIF_RETTYPE nbif_impl_hipe_nonclosure_address(NBIF_ALIST_2);
BIF_RETTYPE nbif_impl_hipe_conv_big_to_float(NBIF_ALIST_1);
void hipe_fclearerror_error(Process*);
void hipe_select_msg(Process*);
void hipe_gc(Process*, Eterm);
BIF_RETTYPE nbif_impl_hipe_set_timeout(NBIF_ALIST_1);
void hipe_handle_exception(Process*);
BIF_RETTYPE nbif_impl_hipe_rethrow(NBIF_ALIST_2);
BIF_RETTYPE nbif_impl_hipe_raw_raise(NBIF_ALIST_3);
char *hipe_bs_allocate(int);
Binary *hipe_bs_reallocate(Binary*, int);
int hipe_bs_put_small_float(Process*, Eterm, Uint, byte*, unsigned, unsigned);
void hipe_bs_put_bits(Eterm, Uint, byte*, unsigned, unsigned);
Eterm hipe_bs_utf8_size(Eterm);
Eterm hipe_bs_put_utf8(Process*, Eterm arg, byte* base, Uint offset);
Eterm hipe_bs_utf16_size(Eterm);
BIF_RETTYPE nbif_impl_hipe_bs_put_utf16be(NBIF_ALIST_3);
BIF_RETTYPE nbif_impl_hipe_bs_put_utf16le(NBIF_ALIST_3);
Uint hipe_is_unicode(Eterm);
struct erl_bin_match_buffer;
int hipe_bs_validate_unicode_retract(struct erl_bin_match_buffer*, Eterm);
Uint hipe_is_divisible(Uint, Uint);

#ifdef NO_FPE_SIGNALS
AEXTERN(void,nbif_emulate_fpe,(Process*));
void hipe_emulate_fpe(Process*);
#endif

AEXTERN(void,nbif_emasculate_binary,(Eterm));
void hipe_emasculate_binary(Eterm);

AEXTERN(BIF_RETTYPE,nbif_hipe_bifs_build_stacktrace,(Process*,Eterm));
BIF_RETTYPE hipe_bifs_build_stacktrace_1(BIF_ALIST_1);

/*
 * Stuff that is different in SMP and non-SMP.
 */
int hipe_bs_put_big_integer(Process*, Eterm, Uint, byte*, unsigned, unsigned);

AEXTERN(Eterm,nbif_check_get_msg,(Process*));
Eterm hipe_check_get_msg(Process*);

AEXTERN(BIF_RETTYPE,nbif_hipe_bifs_debug_native_called,(Process*,Eterm,Eterm));
BIF_RETTYPE hipe_bifs_debug_native_called_2(BIF_ALIST_2);

/*
 * SMP-specific stuff
 */
AEXTERN(void,nbif_atomic_inc,(void));
AEXTERN(void,nbif_clear_timeout,(Process*));
void hipe_atomic_inc(int*);
void hipe_clear_timeout(Process*);

#define BIF_LIST(M,F,A,B,C,I)	AEXTERN(Eterm,nbif_##C,(void));
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */
