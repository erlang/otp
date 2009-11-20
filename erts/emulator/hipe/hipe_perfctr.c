/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */
/* $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "error.h"
#include "global.h"
#include "bif.h"
#include "big.h"
#include "erl_binary.h"
#include "hipe_perfctr.h"
#include "libperfctr.h"

static struct vperfctr *vperfctr;
static unsigned int have_rdtsc;
static double tsc_to_ms;
static unsigned int tsc_on; /* control calls must set tsc_on if have_rdtsc is true */
static unsigned int nractrs;
static unsigned int users;
#define USER_BIFS	(1<<0)
#define USER_HRVTIME	(1<<1)

static int hipe_perfctr_open(unsigned int user)
{
    struct perfctr_info info;

    if (!vperfctr) {
	vperfctr = vperfctr_open();
	if (!vperfctr)
	    return -1;
	if (vperfctr_info(vperfctr, &info) >= 0) {
	    tsc_to_ms = (double)(info.tsc_to_cpu_mult ? : 1) / (double)info.cpu_khz;
	    have_rdtsc = (info.cpu_features & PERFCTR_FEATURE_RDTSC) ? 1 : 0;
	}
	tsc_on = 0;
	nractrs = 0;
    }
    users |= user;
    return 0;
}

static void hipe_perfctr_reset(void)
{
    struct vperfctr_control control;

    memset(&control, 0, sizeof control);
    if (have_rdtsc)
	control.cpu_control.tsc_on = 1;
    nractrs = 0;
    if (vperfctr_control(vperfctr, &control) >= 0)
	tsc_on = 1;
}

static void hipe_perfctr_close(unsigned int user)
{
    if (!vperfctr)
	return;
    users &= ~user;
    switch (users) {
      case 0:
	vperfctr_unlink(vperfctr);
	vperfctr_close(vperfctr);
	vperfctr = NULL;
	tsc_on = 0;
	nractrs = 0;
	break;
      case USER_HRVTIME:
	hipe_perfctr_reset();
    }
}

/*
 * Interface for HiPE's hrvtime code.
 */

int hipe_perfctr_hrvtime_open(void)
{
    if (hipe_perfctr_open(USER_HRVTIME) < 0)
	return -1;
    if (have_rdtsc) {
	if (!tsc_on)
	    hipe_perfctr_reset(); /* note: updates tsc_on */
	if (tsc_on)
	    return 0;
    }
    hipe_perfctr_hrvtime_close();
    return -1;
}

void hipe_perfctr_hrvtime_close(void)
{
    hipe_perfctr_close(USER_HRVTIME);
}

double hipe_perfctr_hrvtime_get(void)
{
    return (double)vperfctr_read_tsc(vperfctr) * tsc_to_ms;
}

/*
 * BIF interface for user-programmable performance counters.
 */

BIF_RETTYPE hipe_bifs_vperfctr_open_0(BIF_ALIST_0)
{
    if (hipe_perfctr_open(USER_BIFS) < 0)
	BIF_RET(am_false); /* arity 0 BIFs can't fail :-( */
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_vperfctr_close_0(BIF_ALIST_0)
{
    hipe_perfctr_close(USER_BIFS);
    BIF_RET(NIL);
}

static Eterm ull_to_integer(unsigned long long x, Process *p)
{
    unsigned long long tmpx;
    unsigned int ds, i;
    size_t sz;
    Eterm *hp;
    ErtsDigit *xp;

    if (x <= (unsigned long long)MAX_SMALL)
	return make_small(x);

    /* Calculate number of digits. */
    ds = 0;
    tmpx = x;
    do {
	++ds;
	tmpx = (tmpx >> (D_EXP / 2)) >> (D_EXP / 2);
    } while (tmpx != 0);

    sz = BIG_NEED_SIZE(ds);	/* number of words including arity */
    hp = HAlloc(p, sz);
    *hp = make_pos_bignum_header(sz-1);

    xp = (ErtsDigit*)(hp+1);
    i = 0;
    do {
	xp[i++] = (ErtsDigit)x;
	x = (x >> (D_EXP / 2)) >> (D_EXP / 2);
    } while (i < ds);
    while (i & (BIG_DIGITS_PER_WORD-1))
	xp[i++] = 0;

    return make_big(hp);
}

BIF_RETTYPE hipe_bifs_vperfctr_info_0(BIF_ALIST_0)
{
    struct perfctr_info info;

    if (!vperfctr || vperfctr_info(vperfctr, &info) < 0)
	BIF_RET(am_false); /* arity 0 BIFs can't fail :-( */
    BIF_RET(new_binary(BIF_P, (void*)&info, sizeof info));
}

BIF_RETTYPE hipe_bifs_vperfctr_read_tsc_0(BIF_ALIST_0)
{
    unsigned long long val;

    if (!vperfctr || !tsc_on)
	BIF_RET(am_false); /* arity 0 BIFs can't fail :-( */
    val = vperfctr_read_tsc(vperfctr);
    BIF_RET(ull_to_integer(val, BIF_P));
}

BIF_RETTYPE hipe_bifs_vperfctr_read_pmc_1(BIF_ALIST_1)
{
    Uint pmc;
    unsigned long long val;

    if (!vperfctr ||
	is_not_small(BIF_ARG_1) ||
	(pmc = unsigned_val(BIF_ARG_1), pmc >= nractrs))
	BIF_RET(am_false); /* for consistency with the arity 0 BIFs */
    val = vperfctr_read_pmc(vperfctr, pmc);
    BIF_RET(ull_to_integer(val, BIF_P));
}

BIF_RETTYPE hipe_bifs_vperfctr_control_1(BIF_ALIST_1)
{
    void *bytes;
    struct vperfctr_control control;
    Uint bitoffs;
    Uint bitsize;

    if (!vperfctr)
	BIF_ERROR(BIF_P, BADARG);
    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (binary_size(BIF_ARG_1) != sizeof control)
	BIF_ERROR(BIF_P, BADARG);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    memcpy(&control, bytes, sizeof control);
    if (have_rdtsc)
	control.cpu_control.tsc_on = 1;
    if (vperfctr_control(vperfctr, &control) < 0) {
	hipe_perfctr_reset();
	BIF_ERROR(BIF_P, BADARG);
    }
    tsc_on = control.cpu_control.tsc_on;
    nractrs = control.cpu_control.nractrs;
    BIF_RET(NIL);
}
