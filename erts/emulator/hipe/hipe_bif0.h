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
 * hipe_bif0.h
 *
 * Compiler and linker support.
 */
#ifndef HIPE_BIF0_H
#define HIPE_BIF0_H

extern Uint *hipe_bifs_find_pc_from_mfa(Eterm mfa);

extern void hipe_mfa_info_table_init(void);
extern void *hipe_get_remote_na(Eterm m, Eterm f, unsigned int a);
extern BIF_RETTYPE hipe_find_na_or_make_stub(BIF_ALIST_3);
extern int hipe_find_mfa_from_ra(const void *ra, Eterm *m, Eterm *f, unsigned int *a);
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
extern void *hipe_mfa_get_trampoline(Eterm m, Eterm f, unsigned int a);
extern void hipe_mfa_set_trampoline(Eterm m, Eterm f, unsigned int a, void *trampoline);
#endif
#if defined(__arm__)
extern void *hipe_primop_get_trampoline(Eterm name);
extern void hipe_primop_set_trampoline(Eterm name, void *trampoline);
#endif

/* needed in beam_load.c */
void hipe_mfa_save_orig_beam_op(Eterm m, Eterm f, unsigned int a, Eterm *pc);

/* these are also needed in hipe_amd64.c */
extern void *term_to_address(Eterm);
extern int term_to_Sint32(Eterm, Sint *);

#endif /* HIPE_BIF0_H */
