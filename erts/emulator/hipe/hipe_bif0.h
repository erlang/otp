/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2001-2017. All Rights Reserved.
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

extern ErtsCodeInfo *hipe_bifs_find_pc_from_mfa(Eterm mfa);

extern void hipe_mfa_info_table_init(void);
extern void *hipe_get_remote_na(Eterm m, Eterm f, unsigned int a);
extern BIF_RETTYPE nbif_impl_hipe_find_na_or_make_stub(NBIF_ALIST_3);
extern int hipe_find_mfa_from_ra(const void *ra, Eterm *m, Eterm *f, unsigned int *a);

/* needed in beam_load.c */
int hipe_need_blocking(Module*);
int hipe_purge_need_blocking(Module*);
void hipe_purge_refs(struct hipe_ref*, Eterm, int is_blocking);
void hipe_purge_sdescs(struct hipe_sdesc*, Eterm, int is_blocking);
void hipe_purge_module(Module*, int is_blocking);
void hipe_redirect_to_module(Module* modp);

/* these are also needed in hipe_amd64.c */
extern void *term_to_address(Eterm);
extern int term_to_Sint32(Eterm, Sint *);

#endif /* HIPE_BIF0_H */
