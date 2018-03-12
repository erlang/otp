/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "big.h"
#include "erl_instrument.h"
#include "erl_threads.h"

typedef union { long l; double d; } Align_t;

typedef struct {
    Uint size;
#ifdef VALGRIND
    void* valgrind_leak_suppressor;
#endif
    Align_t mem[1];
} StatBlock_t;

#define STAT_BLOCK_HEADER_SIZE (sizeof(StatBlock_t) - sizeof(Align_t))

typedef struct MapStatBlock_t_ MapStatBlock_t;
struct MapStatBlock_t_ {
    Uint size;
    ErtsAlcType_t type_no;
    Eterm pid;
    MapStatBlock_t *prev;
    MapStatBlock_t *next;
    Align_t mem[1];
};

#define MAP_STAT_BLOCK_HEADER_SIZE (sizeof(MapStatBlock_t) - sizeof(Align_t))

typedef struct {
    Uint size;
    Uint max_size;
    Uint max_size_ever;

    Uint blocks;
    Uint max_blocks;
    Uint max_blocks_ever;
} Stat_t;

static erts_mtx_t instr_mutex;
static erts_mtx_t instr_x_mutex;

int erts_instr_memory_map;
int erts_instr_stat;

static ErtsAllocatorFunctions_t real_allctrs[ERTS_ALC_A_MAX+1];

struct stats_ {
    Stat_t tot;
    Stat_t a[ERTS_ALC_A_MAX+1];
    Stat_t *ap[ERTS_ALC_A_MAX+1];
    Stat_t c[ERTS_ALC_C_MAX+1];
    Stat_t n[ERTS_ALC_N_MAX+1];
};

static struct stats_ *stats;

static MapStatBlock_t *mem_anchor;

static Eterm *am_tot;
static Eterm *am_n;
static Eterm *am_a;
static Eterm *am_c;

static int atoms_initialized;

static struct {
    Eterm total;
    Eterm allocators;
    Eterm classes;
    Eterm types;
    Eterm sizes;
    Eterm blocks;
    Eterm instr_hdr;
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, const char *name)
{
    *atom = am_atom_put((char *) name, sys_strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(void)
{
#ifdef DEBUG
    Eterm *atom;
    for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	*atom = THE_NON_VALUE;
    }
#endif

    AM_INIT(total);
    AM_INIT(allocators);
    AM_INIT(classes);
    AM_INIT(types);
    AM_INIT(sizes);
    AM_INIT(blocks);
    AM_INIT(instr_hdr);

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	ASSERT(*atom != THE_NON_VALUE);
    }
#endif

    atoms_initialized = 1;
}

#undef AM_INIT

static void
init_am_tot(void)
{
    am_tot = (Eterm *) erts_alloc(ERTS_ALC_T_INSTR_INFO,
				  sizeof(Eterm));
    atom_init(am_tot, "total");
}


static void
init_am_n(void)
{
    int i;
    am_n = (Eterm *) erts_alloc(ERTS_ALC_T_INSTR_INFO,
				(ERTS_ALC_N_MAX+1)*sizeof(Eterm));

    for (i = ERTS_ALC_N_MIN; i <= ERTS_ALC_N_MAX; i++) {
	atom_init(&am_n[i], ERTS_ALC_N2TD(i));
    }

}

static void
init_am_c(void)
{
    int i;
    am_c = (Eterm *) erts_alloc(ERTS_ALC_T_INSTR_INFO,
				(ERTS_ALC_C_MAX+1)*sizeof(Eterm));

    for (i = ERTS_ALC_C_MIN; i <= ERTS_ALC_C_MAX; i++) {
	atom_init(&am_c[i], ERTS_ALC_C2CD(i));
    }

}

static void
init_am_a(void)
{
    int i;
    am_a = (Eterm *) erts_alloc(ERTS_ALC_T_INSTR_INFO,
				(ERTS_ALC_A_MAX+1)*sizeof(Eterm));

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	atom_init(&am_a[i], ERTS_ALC_A2AD(i));
    }

}

static ERTS_INLINE void
stat_upd_alloc(ErtsAlcType_t n, Uint size)
{
    ErtsAlcType_t t = ERTS_ALC_N2T(n);
    ErtsAlcType_t a = ERTS_ALC_T2A(t);
    ErtsAlcType_t c = ERTS_ALC_T2C(t);

    stats->ap[a]->size += size;
    if (stats->ap[a]->max_size < stats->ap[a]->size)
	stats->ap[a]->max_size = stats->ap[a]->size;

    stats->c[c].size += size;
    if (stats->c[c].max_size < stats->c[c].size)
	stats->c[c].max_size = stats->c[c].size;

    stats->n[n].size += size;
    if (stats->n[n].max_size < stats->n[n].size)
	stats->n[n].max_size = stats->n[n].size;

    stats->tot.size += size;
    if (stats->tot.max_size < stats->tot.size)
	stats->tot.max_size = stats->tot.size;

    stats->ap[a]->blocks++;
    if (stats->ap[a]->max_blocks < stats->ap[a]->blocks)
	stats->ap[a]->max_blocks = stats->ap[a]->blocks;

    stats->c[c].blocks++;
    if (stats->c[c].max_blocks < stats->c[c].blocks)
	stats->c[c].max_blocks = stats->c[c].blocks;

    stats->n[n].blocks++;
    if (stats->n[n].max_blocks < stats->n[n].blocks)
	stats->n[n].max_blocks = stats->n[n].blocks;

    stats->tot.blocks++;
    if (stats->tot.max_blocks < stats->tot.blocks)
	stats->tot.max_blocks = stats->tot.blocks;

}


static ERTS_INLINE void
stat_upd_free(ErtsAlcType_t n, Uint size)
{
    ErtsAlcType_t t = ERTS_ALC_N2T(n);
    ErtsAlcType_t a = ERTS_ALC_T2A(t);
    ErtsAlcType_t c = ERTS_ALC_T2C(t);

    ASSERT(stats->ap[a]->size >= size);
    stats->ap[a]->size -= size;

    ASSERT(stats->c[c].size >= size);
    stats->c[c].size -= size;

    ASSERT(stats->n[n].size >= size);
    stats->n[n].size -= size;

    ASSERT(stats->tot.size >= size);
    stats->tot.size -= size;

    ASSERT(stats->ap[a]->blocks > 0);
    stats->ap[a]->blocks--;

    ASSERT(stats->c[c].blocks > 0);
    stats->c[c].blocks--;

    ASSERT(stats->n[n].blocks > 0);
    stats->n[n].blocks--;

    ASSERT(stats->tot.blocks > 0);
    stats->tot.blocks--;

}


static ERTS_INLINE void
stat_upd_realloc(ErtsAlcType_t n, Uint size, Uint old_size)
{
    if (old_size)
	stat_upd_free(n, old_size);
    stat_upd_alloc(n, size);
}

/*
 * stat instrumentation callback functions
 */

static void stat_pre_lock(void)
{
    erts_mtx_lock(&instr_mutex);
}

static void stat_pre_unlock(void)
{
    erts_mtx_unlock(&instr_mutex);
}

static ErtsAllocatorWrapper_t instr_wrapper;

static void *
stat_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint ssize;
    void *res;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&instr_mutex);
    }

    ssize = size + STAT_BLOCK_HEADER_SIZE;
    res = (*real_af->alloc)(n, real_af->extra, ssize);
    if (res) {
	stat_upd_alloc(n, size);
	((StatBlock_t *) res)->size = size;
#ifdef VALGRIND
	/* Suppress "possibly leaks" by storing an actual dummy pointer
	   to the _start_ of the allocated block.*/
	((StatBlock_t *) res)->valgrind_leak_suppressor = res;
#endif
	res = (void *) ((StatBlock_t *) res)->mem;
    }

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&instr_mutex);
    }

    return res;
}

static void *
stat_realloc(ErtsAlcType_t n, void *extra, void *ptr, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint old_size;
    Uint ssize;
    void *sptr;
    void *res;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&instr_mutex);
    }

    if (ptr) {
	sptr = (void *) (((char *) ptr) - STAT_BLOCK_HEADER_SIZE);
	old_size = ((StatBlock_t *) sptr)->size;
    }
    else {
	sptr = NULL;
	old_size = 0;
    }

    ssize = size + STAT_BLOCK_HEADER_SIZE;
    res = (*real_af->realloc)(n, real_af->extra, sptr, ssize);
    if (res) {
	stat_upd_realloc(n, size, old_size);
	((StatBlock_t *) res)->size = size;
#ifdef VALGRIND
	((StatBlock_t *) res)->valgrind_leak_suppressor = res;
#endif
	res = (void *) ((StatBlock_t *) res)->mem;
    }

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&instr_mutex);
    }

    return res;
}

static void
stat_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *sptr;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&instr_mutex);
    }

    if (ptr) {
	sptr = (void *) (((char *) ptr) - STAT_BLOCK_HEADER_SIZE);
	stat_upd_free(n, ((StatBlock_t *) sptr)->size);
    }
    else {
	sptr = NULL;
    }

    (*real_af->free)(n, real_af->extra, sptr);

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&instr_mutex);
    }

}

/*
 * map stat instrumentation callback functions
 */

static void map_stat_pre_lock(void)
{
    erts_mtx_lock(&instr_x_mutex);
    erts_mtx_lock(&instr_mutex);
}

static void map_stat_pre_unlock(void)
{
    erts_mtx_unlock(&instr_mutex);
    erts_mtx_unlock(&instr_x_mutex);
}

static void *
map_stat_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint msize;
    void *res;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&instr_mutex);
    }

    msize = size + MAP_STAT_BLOCK_HEADER_SIZE;
    res = (*real_af->alloc)(n, real_af->extra, msize);
    if (res) {
	MapStatBlock_t *mb = (MapStatBlock_t *) res;
	stat_upd_alloc(n, size);

	mb->size = size;
	mb->type_no = n;
	mb->pid = erts_get_current_pid();

	mb->prev = NULL;
	mb->next = mem_anchor;
	if (mem_anchor)
	    mem_anchor->prev = mb;
	mem_anchor = mb;

	res = (void *) mb->mem;
    }

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&instr_mutex);
    }

    return res;
}

static void *
map_stat_realloc(ErtsAlcType_t n, void *extra, void *ptr, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint old_size;
    Uint msize;
    void *mptr;
    void *res;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&instr_x_mutex);
	erts_mtx_lock(&instr_mutex);
    }

    if (ptr) {
	mptr = (void *) (((char *) ptr) - MAP_STAT_BLOCK_HEADER_SIZE);
	old_size = ((MapStatBlock_t *) mptr)->size;
    }
    else {
	mptr = NULL;
	old_size = 0;
    }

    msize = size + MAP_STAT_BLOCK_HEADER_SIZE;
    res = (*real_af->realloc)(n, real_af->extra, mptr, msize);
    if (res) {
	MapStatBlock_t *mb = (MapStatBlock_t *) res;

	mb->size = size;
	mb->type_no = n;
	mb->pid = erts_get_current_pid();

	stat_upd_realloc(n, size, old_size);

	if (mptr != res) {

	    if (mptr) {
		if (mb->prev)
		    mb->prev->next = mb;
		else {
		    ASSERT(mem_anchor == (MapStatBlock_t *) mptr);
		    mem_anchor = mb;
		}
		if (mb->next)
		    mb->next->prev = mb;
	    }
	    else {
		mb->prev = NULL;
		mb->next = mem_anchor;
		if (mem_anchor)
		    mem_anchor->prev = mb;
		mem_anchor = mb;
	    }

	}

	res = (void *) mb->mem;
    }
    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&instr_mutex);
	erts_mtx_unlock(&instr_x_mutex);
    }

    return res;
}

static void
map_stat_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *mptr;

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_lock(&instr_x_mutex);
	erts_mtx_lock(&instr_mutex);
    }

    if (ptr) {
	MapStatBlock_t *mb;

	mptr = (void *) (((char *) ptr) - MAP_STAT_BLOCK_HEADER_SIZE);
	mb = (MapStatBlock_t *) mptr;

	stat_upd_free(n, mb->size);

	if (mb->prev)
	    mb->prev->next = mb->next;
	else
	    mem_anchor = mb->next;
	if (mb->next)
	    mb->next->prev = mb->prev;
    }
    else {
	mptr = NULL;
    }

    (*real_af->free)(n, real_af->extra, mptr);

    if (!erts_is_allctr_wrapper_prelocked()) {
	erts_mtx_unlock(&instr_mutex);
	erts_mtx_unlock(&instr_x_mutex);
    }

}

static void dump_memory_map_to_stream(fmtfn_t to, void* to_arg)
{
    ErtsAlcType_t n;
    MapStatBlock_t *bp;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock) {
	ASSERT(!erts_is_allctr_wrapper_prelocked());
	erts_mtx_lock(&instr_mutex);
    }

    /* Write header */

    erts_cbprintf(to, to_arg,
	    "{instr_hdr,\n"
	    " %lu,\n"
	    " %lu,\n"
	    " {",
	    (unsigned long) ERTS_INSTR_VSN,
	    (unsigned long) MAP_STAT_BLOCK_HEADER_SIZE);

#if ERTS_ALC_N_MIN != 1
#error ERTS_ALC_N_MIN is not 1
#endif

    for (n = ERTS_ALC_N_MIN; n <= ERTS_ALC_N_MAX; n++) {
	ErtsAlcType_t t = ERTS_ALC_N2T(n);
	ErtsAlcType_t a = ERTS_ALC_T2A(t);
	ErtsAlcType_t c = ERTS_ALC_T2C(t);
	const char *astr;

	if (erts_allctrs_info[a].enabled)
	    astr = ERTS_ALC_A2AD(a);
	else
	    astr = ERTS_ALC_A2AD(ERTS_ALC_A_SYSTEM);

	erts_cbprintf(to, to_arg,
		"%s{%s,%s,%s}%s",
		(n == ERTS_ALC_N_MIN) ? "" : "  ",
		ERTS_ALC_N2TD(n),
		astr,
		ERTS_ALC_C2CD(c),
		(n == ERTS_ALC_N_MAX) ? "" : ",\n");
    }

    erts_cbprintf(to, to_arg, "}}.\n");

    /* Write memory data */
    for (bp = mem_anchor; bp; bp = bp->next) {
	if (is_internal_pid(bp->pid))
	    erts_cbprintf(to, to_arg,
		    "{%lu, %lu, %lu, {%lu,%lu,%lu}}.\n",
		    (UWord) bp->type_no,
		    (UWord) bp->mem,
		    (UWord) bp->size,
		    (UWord) pid_channel_no(bp->pid),
		    (UWord) pid_number(bp->pid),
		    (UWord) pid_serial(bp->pid));
	else
	    erts_cbprintf(to, to_arg,
		    "{%lu, %lu, %lu, undefined}.\n",
		    (UWord) bp->type_no,
		    (UWord) bp->mem,
		    (UWord) bp->size);
    }

    if (lock)
	erts_mtx_unlock(&instr_mutex);
}

int erts_instr_dump_memory_map_to(fmtfn_t to, void* to_arg)
{
    if (!erts_instr_memory_map)
	return 0;

    dump_memory_map_to_stream(to, to_arg);
    return 1;
}

int erts_instr_dump_memory_map(const char *name)
{
    int fd;

    if (!erts_instr_memory_map)
	return 0;

    fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0640);
    if (fd < 0)
	return 0;

    dump_memory_map_to_stream(erts_write_fd, (void*)&fd);

    close(fd);
    return 1;
}

Eterm erts_instr_get_memory_map(Process *proc)
{
    MapStatBlock_t *org_mem_anchor;
    Eterm hdr_tuple, md_list, res;
    Eterm *hp;
    Uint hsz;
    MapStatBlock_t *bp;
#ifdef DEBUG
    Eterm *end_hp;
#endif
    
    if (!erts_instr_memory_map)
	return am_false;

    if (!atoms_initialized)
	init_atoms();
    if (!am_n)
	init_am_n();
    if (!am_c)
	init_am_c();
    if (!am_a)
	init_am_a();

    erts_mtx_lock(&instr_x_mutex);
    erts_mtx_lock(&instr_mutex);

    /* Header size */
    hsz = 5 + 1 + (ERTS_ALC_N_MAX+1-ERTS_ALC_N_MIN)*(1 + 4);

    /* Memory data list */
    for (bp = mem_anchor; bp; bp = bp->next) {
	if (is_internal_pid(bp->pid)) {
#if (_PID_NUM_SIZE - 1 > MAX_SMALL)
	    if (internal_pid_number(bp->pid) > MAX_SMALL)
		hsz += BIG_UINT_HEAP_SIZE;
#endif
#if (_PID_SER_SIZE - 1 > MAX_SMALL)
	    if (internal_pid_serial(bp->pid) > MAX_SMALL)
		hsz += BIG_UINT_HEAP_SIZE;
#endif
	    hsz += 4;
	}

	if ((UWord) bp->mem > MAX_SMALL)
	    hsz += BIG_UINT_HEAP_SIZE;
	if (bp->size > MAX_SMALL)
	    hsz += BIG_UINT_HEAP_SIZE;

	hsz += 5 + 2;
    }

    hsz += 3; /* Root tuple */

    org_mem_anchor = mem_anchor;
    mem_anchor = NULL;

    erts_mtx_unlock(&instr_mutex);

    hp = HAlloc(proc, hsz); /* May end up calling map_stat_alloc() */

    erts_mtx_lock(&instr_mutex);

#ifdef DEBUG
    end_hp = hp + hsz;
#endif

    {	/* Build header */
	ErtsAlcType_t n;
	Eterm type_map;
	Uint *hp2 = hp;
#ifdef DEBUG
	Uint *hp2_end;
#endif

	hp += (ERTS_ALC_N_MAX + 1 - ERTS_ALC_N_MIN)*4;

#ifdef DEBUG
	hp2_end = hp;
#endif

	type_map = make_tuple(hp);
	*(hp++) = make_arityval(ERTS_ALC_N_MAX + 1 - ERTS_ALC_N_MIN);

	for (n = ERTS_ALC_N_MIN; n <= ERTS_ALC_N_MAX; n++) {
	    ErtsAlcType_t t = ERTS_ALC_N2T(n);
	    ErtsAlcType_t a = ERTS_ALC_T2A(t);
	    ErtsAlcType_t c = ERTS_ALC_T2C(t);

	    if (!erts_allctrs_info[a].enabled)
		a = ERTS_ALC_A_SYSTEM;

	    *(hp++) = TUPLE3(hp2, am_n[n], am_a[a], am_c[c]);
	    hp2 += 4;
	}

	ASSERT(hp2 == hp2_end);

	hdr_tuple = TUPLE4(hp,
			   am.instr_hdr,
			   make_small(ERTS_INSTR_VSN),
			   make_small(MAP_STAT_BLOCK_HEADER_SIZE),
			   type_map);

	hp += 5;
    }

    /* Build memory data list */

    for (md_list = NIL, bp = org_mem_anchor; bp; bp = bp->next) {
	Eterm tuple;
	Eterm type;
	Eterm ptr;
	Eterm size;
	Eterm pid;

	if (is_not_internal_pid(bp->pid))
	    pid = am_undefined;
	else {
	    Eterm c;
	    Eterm n;
	    Eterm s;

#if (ERST_INTERNAL_CHANNEL_NO > MAX_SMALL)
#error Oversized internal channel number
#endif
	    c = make_small(ERST_INTERNAL_CHANNEL_NO);

#if (_PID_NUM_SIZE - 1 > MAX_SMALL)
	    if (internal_pid_number(bp->pid) > MAX_SMALL) {
		n = uint_to_big(internal_pid_number(bp->pid), hp);
		hp += BIG_UINT_HEAP_SIZE;
	    }
	    else
#endif
		n = make_small(internal_pid_number(bp->pid));

#if (_PID_SER_SIZE - 1 > MAX_SMALL)
	    if (internal_pid_serial(bp->pid) > MAX_SMALL) {
		s = uint_to_big(internal_pid_serial(bp->pid), hp);
		hp += BIG_UINT_HEAP_SIZE;
	    }
	    else
#endif
		s = make_small(internal_pid_serial(bp->pid));
	    pid = TUPLE3(hp, c, n, s);
	    hp += 4;
	}


#if ERTS_ALC_N_MAX > MAX_SMALL
#error Oversized memory type number
#endif
	type = make_small(bp->type_no);

	if ((UWord) bp->mem > MAX_SMALL) {
	    ptr = uint_to_big((UWord) bp->mem, hp);
	    hp += BIG_UINT_HEAP_SIZE;
	}
	else
	    ptr = make_small((UWord) bp->mem);

	if (bp->size > MAX_SMALL) {
	    size = uint_to_big(bp->size, hp);
	    hp += BIG_UINT_HEAP_SIZE;
	}
	else
	    size = make_small(bp->size);

	tuple = TUPLE4(hp, type, ptr, size, pid);
	hp += 5;

	md_list = CONS(hp, tuple, md_list);
	hp += 2;
    }

    res = TUPLE2(hp, hdr_tuple, md_list);
    
    ASSERT(hp + 3 == end_hp);

    if (mem_anchor) {
	for (bp = mem_anchor; bp->next; bp = bp->next)
	    ;
	ASSERT(org_mem_anchor);
	org_mem_anchor->prev = bp; 
	bp->next = org_mem_anchor;
    }
    else {
	mem_anchor = org_mem_anchor;
    }

    erts_mtx_unlock(&instr_mutex);
    erts_mtx_unlock(&instr_x_mutex);

    return res;
}

static ERTS_INLINE void
begin_new_max_period(Stat_t *stat, int min, int max)
{
    int i;
    for (i = min; i <= max; i++) {
	stat[i].max_size = stat[i].size;
	stat[i].max_blocks = stat[i].blocks;
    }
}

static ERTS_INLINE void
update_max_ever_values(Stat_t *stat, int min, int max)
{
    int i;
    for (i = min; i <= max; i++) {
	if (stat[i].max_size_ever < stat[i].max_size)
	    stat[i].max_size_ever = stat[i].max_size;
	if (stat[i].max_blocks_ever < stat[i].max_blocks)
	    stat[i].max_blocks_ever = stat[i].max_blocks;
    }
}

#define bld_string	erts_bld_string
#define bld_tuple	erts_bld_tuple
#define bld_tuplev	erts_bld_tuplev
#define bld_list	erts_bld_list
#define bld_2tup_list	erts_bld_2tup_list
#define bld_uint	erts_bld_uint

Eterm
erts_instr_get_stat(Process *proc, Eterm what, int begin_max_period)
{
    int i, len, max, min, allctr;
    Eterm *names, *values, res;
    Uint arr_size, stat_size, hsz, *hszp, *hp, **hpp;
    Stat_t *stat_src, *stat;

    if (!erts_instr_stat)
	return am_false;

    if (!atoms_initialized)
	init_atoms();

    if (what == am.total) {
	min		= 0;
	max		= 0;
	allctr		= 0;
	stat_size	= sizeof(Stat_t);
	stat_src	= &stats->tot;
	if (!am_tot)
	    init_am_tot();
	names		= am_tot;
    }
    else if (what == am.allocators) {
	min		= ERTS_ALC_A_MIN;
	max		= ERTS_ALC_A_MAX;
	allctr		= 1;
	stat_size	= sizeof(Stat_t)*(ERTS_ALC_A_MAX+1);
	stat_src	= stats->a;
	if (!am_a)
	    init_am_a();
	names		= am_a;
    }
    else if (what == am.classes) {
	min		= ERTS_ALC_C_MIN;
	max		= ERTS_ALC_C_MAX;
	allctr		= 0;
	stat_size	= sizeof(Stat_t)*(ERTS_ALC_C_MAX+1);
	stat_src	= stats->c;
	if (!am_c)
	    init_am_c();
	names		= &am_c[ERTS_ALC_C_MIN];
    }
    else if (what == am.types) {
	min		= ERTS_ALC_N_MIN;
	max		= ERTS_ALC_N_MAX;
	allctr		= 0;
	stat_size	= sizeof(Stat_t)*(ERTS_ALC_N_MAX+1);
	stat_src	= stats->n;
	if (!am_n)
	    init_am_n();
	names		= &am_n[ERTS_ALC_N_MIN];
    }
    else {
	return THE_NON_VALUE;
    }

    stat = (Stat_t *) erts_alloc(ERTS_ALC_T_TMP, stat_size);

    arr_size = (max - min + 1)*sizeof(Eterm);

    if (allctr)
	names = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, arr_size);

    values = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, arr_size);

    erts_mtx_lock(&instr_mutex);

    update_max_ever_values(stat_src, min, max);

    sys_memcpy((void *) stat, (void *) stat_src, stat_size);

    if (begin_max_period)
	begin_new_max_period(stat_src, min, max);

    erts_mtx_unlock(&instr_mutex);

    hsz = 0;
    hszp = &hsz;
    hpp = NULL;

 restart_bld:

    len = 0;
    for (i = min; i <= max; i++) {
	if (!allctr || erts_allctrs_info[i].enabled) {
	    Eterm s[2];

	    if (allctr)
		names[len] = am_a[i];
	    
	    s[0] = bld_tuple(hpp, hszp, 4,
			     am.sizes,
			     bld_uint(hpp, hszp, stat[i].size),
			     bld_uint(hpp, hszp, stat[i].max_size),
			     bld_uint(hpp, hszp, stat[i].max_size_ever));

	    s[1] = bld_tuple(hpp, hszp, 4,
			     am.blocks,
			     bld_uint(hpp, hszp, stat[i].blocks),
			     bld_uint(hpp, hszp, stat[i].max_blocks),
			     bld_uint(hpp, hszp, stat[i].max_blocks_ever));

	    values[len] = bld_list(hpp, hszp, 2, s);
	    
	    len++;
	}
    }

    res = bld_2tup_list(hpp, hszp, len, names, values);

    if (!hpp) {
	hp = HAlloc(proc, hsz);
	hszp = NULL;
	hpp = &hp;
	goto restart_bld;
    }

    erts_free(ERTS_ALC_T_TMP, (void *) stat);
    erts_free(ERTS_ALC_T_TMP, (void *) values);
    if (allctr)
	erts_free(ERTS_ALC_T_TMP, (void *) names);

    return res;
}

static void
dump_stat_to_stream(fmtfn_t to, void* to_arg, int begin_max_period)
{
    ErtsAlcType_t i, a_max, a_min;

    erts_mtx_lock(&instr_mutex);

    erts_cbprintf(to, to_arg,
	    "{instr_vsn,%lu}.\n",
	    (unsigned long) ERTS_INSTR_VSN);
    
    update_max_ever_values(&stats->tot, 0, 0);

    erts_cbprintf(to, to_arg,
	    "{total,[{total,[{sizes,%lu,%lu,%lu},{blocks,%lu,%lu,%lu}]}]}.\n",
	    (UWord) stats->tot.size,
	    (UWord) stats->tot.max_size,
	    (UWord) stats->tot.max_size_ever,
	    (UWord) stats->tot.blocks,
	    (UWord) stats->tot.max_blocks,
	    (UWord) stats->tot.max_blocks_ever);

    a_max = 0;
    a_min = ~0;
    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (erts_allctrs_info[i].enabled) {
	    if (a_min > i)
		a_min = i;
	    if (a_max < i)
		a_max = i;
	}
    }

    ASSERT(ERTS_ALC_A_MIN <= a_min && a_min <= ERTS_ALC_A_MAX);
    ASSERT(ERTS_ALC_A_MIN <= a_max && a_max <= ERTS_ALC_A_MAX);
    ASSERT(a_min <= a_max);

    update_max_ever_values(stats->a, a_min, a_max);

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (erts_allctrs_info[i].enabled) {
	    erts_cbprintf(to, to_arg,
		    "%s{%s,[{sizes,%lu,%lu,%lu},{blocks,%lu,%lu,%lu}]}%s",
		    i == a_min ? "{allocators,\n [" : "  ",
		    ERTS_ALC_A2AD(i),
		    (UWord) stats->a[i].size,
		    (UWord) stats->a[i].max_size,
		    (UWord) stats->a[i].max_size_ever,
		    (UWord) stats->a[i].blocks,
		    (UWord) stats->a[i].max_blocks,
		    (UWord) stats->a[i].max_blocks_ever,
		    i == a_max ? "]}.\n" : ",\n");
	}
    }

    update_max_ever_values(stats->c, ERTS_ALC_C_MIN, ERTS_ALC_C_MAX);

    for (i = ERTS_ALC_C_MIN; i <= ERTS_ALC_C_MAX; i++) {
	erts_cbprintf(to, to_arg,
		"%s{%s,[{sizes,%lu,%lu,%lu},{blocks,%lu,%lu,%lu}]}%s",
		i == ERTS_ALC_C_MIN ? "{classes,\n [" : "  ",
		ERTS_ALC_C2CD(i),
		(UWord) stats->c[i].size,
		(UWord) stats->c[i].max_size,
		(UWord) stats->c[i].max_size_ever,
		(UWord) stats->c[i].blocks,
		(UWord) stats->c[i].max_blocks,
		(UWord) stats->c[i].max_blocks_ever,
		i == ERTS_ALC_C_MAX ? "]}.\n" :  ",\n" );
    }

    update_max_ever_values(stats->n, ERTS_ALC_N_MIN, ERTS_ALC_N_MAX);

    for (i = ERTS_ALC_N_MIN; i <= ERTS_ALC_N_MAX; i++) {
	erts_cbprintf(to, to_arg,
		"%s{%s,[{sizes,%lu,%lu,%lu},{blocks,%lu,%lu,%lu}]}%s",
		i == ERTS_ALC_N_MIN ? "{types,\n [" : "  ",
		ERTS_ALC_N2TD(i),
		(UWord) stats->n[i].size,
		(UWord) stats->n[i].max_size,
		(UWord) stats->n[i].max_size_ever,
		(UWord) stats->n[i].blocks,
		(UWord) stats->n[i].max_blocks,
		(UWord) stats->n[i].max_blocks_ever,
		i == ERTS_ALC_N_MAX ? "]}.\n" :  ",\n" );
    }

    if (begin_max_period) {
	begin_new_max_period(&stats->tot, 0, 0);
	begin_new_max_period(stats->a, a_min, a_max);
	begin_new_max_period(stats->c, ERTS_ALC_C_MIN, ERTS_ALC_C_MAX);
	begin_new_max_period(stats->n, ERTS_ALC_N_MIN, ERTS_ALC_N_MAX);
    }

    erts_mtx_unlock(&instr_mutex);

}

int erts_instr_dump_stat_to(fmtfn_t to, void* to_arg, int begin_max_period)
{
    if (!erts_instr_stat)
	return 0;

    dump_stat_to_stream(to, to_arg, begin_max_period);
    return 1;
}

int erts_instr_dump_stat(const char *name, int begin_max_period)
{
    int fd;

    if (!erts_instr_stat)
	return 0;

    fd = open(name, O_WRONLY | O_CREAT | O_TRUNC,0640);
    if (fd < 0)
	return 0;

    dump_stat_to_stream(erts_write_fd, (void*)&fd, begin_max_period);

    close(fd);
    return 1;
}


Uint
erts_instr_get_total(void)
{
    return erts_instr_stat ? stats->tot.size : 0;
}

Uint
erts_instr_get_max_total(void)
{
    if (erts_instr_stat) {
	update_max_ever_values(&stats->tot, 0, 0);
	return stats->tot.max_size_ever;
    }
    return 0;
}

Eterm
erts_instr_get_type_info(Process *proc)
{
    Eterm res, *tpls;
    Uint hsz, *hszp, *hp, **hpp;
    ErtsAlcType_t n;

    if (!am_n)
	init_am_n();
    if (!am_a)
	init_am_a();
    if (!am_c)
	init_am_c();

    tpls = (Eterm *) erts_alloc(ERTS_ALC_T_TMP,
				(ERTS_ALC_N_MAX-ERTS_ALC_N_MIN+1)
				* sizeof(Eterm));
    hsz = 0;
    hszp = &hsz;
    hpp = NULL;

 restart_bld:

#if ERTS_ALC_N_MIN != 1
#error ERTS_ALC_N_MIN is not 1
#endif

    for (n = ERTS_ALC_N_MIN; n <= ERTS_ALC_N_MAX; n++) {
	ErtsAlcType_t t = ERTS_ALC_N2T(n);
	ErtsAlcType_t a = ERTS_ALC_T2A(t);
	ErtsAlcType_t c = ERTS_ALC_T2C(t);

	if (!erts_allctrs_info[a].enabled)
	    a = ERTS_ALC_A_SYSTEM;

	tpls[n - ERTS_ALC_N_MIN]
	    = bld_tuple(hpp, hszp, 3, am_n[n], am_a[a], am_c[c]);
    }

    res = bld_tuplev(hpp, hszp, ERTS_ALC_N_MAX-ERTS_ALC_N_MIN+1, tpls);
    
    if (!hpp) {
	hp = HAlloc(proc, hsz);
	hszp = NULL;
	hpp = &hp;
	goto restart_bld;
    }

    erts_free(ERTS_ALC_T_TMP, tpls);

    return res;
}

Uint
erts_instr_init(int stat, int map_stat)
{
    Uint extra_sz;
    int i;

    am_tot = NULL;
    am_n = NULL;
    am_c = NULL;
    am_a = NULL;

    erts_instr_memory_map = 0;
    erts_instr_stat = 0;
    atoms_initialized = 0;

    if (!stat && !map_stat)
	return 0;

    stats = erts_alloc(ERTS_ALC_T_INSTR_INFO, sizeof(struct stats_));

    erts_mtx_init(&instr_mutex, "instr", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DEBUG);

    mem_anchor = NULL;

    /* Install instrumentation functions */
    ERTS_CT_ASSERT(sizeof(erts_allctrs) == sizeof(real_allctrs));

    sys_memcpy((void *)real_allctrs,(void *)erts_allctrs,sizeof(erts_allctrs));

    sys_memzero((void *) &stats->tot, sizeof(Stat_t));
    sys_memzero((void *) stats->a, sizeof(Stat_t)*(ERTS_ALC_A_MAX+1));
    sys_memzero((void *) stats->c, sizeof(Stat_t)*(ERTS_ALC_C_MAX+1));
    sys_memzero((void *) stats->n, sizeof(Stat_t)*(ERTS_ALC_N_MAX+1));

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (erts_allctrs_info[i].enabled)
	    stats->ap[i] = &stats->a[i];
	else
	    stats->ap[i] = &stats->a[ERTS_ALC_A_SYSTEM];
    }

    if (map_stat) {

	erts_mtx_init(&instr_x_mutex, "instr_x", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DEBUG);

	erts_instr_memory_map = 1;
	erts_instr_stat = 1;
	for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	    erts_allctrs[i].alloc	= map_stat_alloc;
	    erts_allctrs[i].realloc	= map_stat_realloc;
	    erts_allctrs[i].free	= map_stat_free;
	    erts_allctrs[i].extra	= (void *) &real_allctrs[i];
	}
	instr_wrapper.lock = map_stat_pre_lock;
	instr_wrapper.unlock = map_stat_pre_unlock;
	extra_sz = MAP_STAT_BLOCK_HEADER_SIZE;
    }
    else {
	erts_instr_stat = 1;
	for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	    erts_allctrs[i].alloc	= stat_alloc;
	    erts_allctrs[i].realloc	= stat_realloc;
	    erts_allctrs[i].free	= stat_free;
	    erts_allctrs[i].extra	= (void *) &real_allctrs[i];
	}
	instr_wrapper.lock = stat_pre_lock;
	instr_wrapper.unlock = stat_pre_unlock;
	extra_sz = STAT_BLOCK_HEADER_SIZE;
    }
    erts_allctr_wrapper_prelock_init(&instr_wrapper);
    return extra_sz;
}

