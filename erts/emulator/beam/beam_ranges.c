/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "beam_load.h"

typedef struct {
    BeamInstr* start;		/* Pointer to start of module. */
    erts_smp_atomic_t end; /* (BeamInstr*) Points one word beyond last function in module. */
} Range;

/* Range 'end' needs to be atomic as we purge module
    by setting end=start in active code_ix */
#define RANGE_END(R) ((BeamInstr*)erts_smp_atomic_read_nob(&(R)->end))

static Range* find_range(BeamInstr* pc);
static void lookup_loc(FunctionInfo* fi, BeamInstr* pc,
		       BeamInstr* modp, int idx);

/*
 * The following variables keep a sorted list of address ranges for
 * each module.  It allows us to quickly find a function given an
 * instruction pointer.
 */
struct ranges {
    Range* modules;	       /* Sorted lists of module addresses. */
    Sint n;		       /* Number of range entries. */
    Sint allocated;	       /* Number of allocated entries. */
    erts_smp_atomic_t mid;     /* Cached search start point */
};
static struct ranges r[ERTS_NUM_CODE_IX];
static erts_smp_atomic_t mem_used;

#ifdef HARD_DEBUG
static void check_consistency(struct ranges* p)
{
    int i;

    ASSERT(p->n <= p->allocated);
    ASSERT((Uint)(p->mid - p->modules) < p->n ||
	   (p->mid == p->modules && p->n == 0));
    for (i = 0; i < p->n; i++) {
	ASSERT(p->modules[i].start <= RANGE_END(&p->modules[i]));
	ASSERT(!i || RANGE_END(&p->modules[i-1]) < p->modules[i].start);
    }
}
#  define CHECK(r) check_consistency(r)
#else
#  define CHECK(r)
#endif /* HARD_DEBUG */


void
erts_init_ranges(void)
{
    Sint i;

    erts_smp_atomic_init_nob(&mem_used, 0);
    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
	r[i].modules = 0;
	r[i].n = 0;
	r[i].allocated = 0;
	erts_smp_atomic_init_nob(&r[i].mid, 0);
    }
}

void
erts_start_staging_ranges(void)
{
    ErtsCodeIndex dst = erts_staging_code_ix();

    if (r[dst].modules) {
	erts_smp_atomic_add_nob(&mem_used, -r[dst].allocated);
	erts_free(ERTS_ALC_T_MODULE_REFS, r[dst].modules);
	r[dst].modules = NULL;
    }
}

void
erts_end_staging_ranges(int commit)
{
    ErtsCodeIndex dst = erts_staging_code_ix();

    if (commit && r[dst].modules == NULL) {
	Sint i;
	Sint n;

	/* No modules added, just clone src and remove purged code. */
	ErtsCodeIndex src = erts_active_code_ix();

	erts_smp_atomic_add_nob(&mem_used, r[src].n);
	r[dst].modules = erts_alloc(ERTS_ALC_T_MODULE_REFS,
				    r[src].n * sizeof(Range));
	r[dst].allocated = r[src].n;
	n = 0;
	for (i = 0; i < r[src].n; i++) {
	    Range* rp = r[src].modules+i;
	    if (rp->start < RANGE_END(rp)) {
		/* Only insert a module that has not been purged. */
		r[dst].modules[n] = *rp;
		n++;
	    }
	}
	r[dst].n = n;
	erts_smp_atomic_set_nob(&r[dst].mid,
				(erts_aint_t) (r[dst].modules + n / 2));
    }
}

void
erts_update_ranges(BeamInstr* code, Uint size)
{
    ErtsCodeIndex dst = erts_staging_code_ix();
    ErtsCodeIndex src = erts_active_code_ix();
    Sint i;
    Sint n;
    Sint need;

    if (src == dst) {
	ASSERT(!erts_initialized);

	/*
	 * During start-up of system, the indices are the same.
	 * Handle this by faking a source area.
	 */
	src = (src+1) % ERTS_NUM_CODE_IX;
	if (r[src].modules) {
	    erts_smp_atomic_add_nob(&mem_used, -r[src].allocated);
	    erts_free(ERTS_ALC_T_MODULE_REFS, r[src].modules);
	}
	r[src] = r[dst];
	r[dst].modules = 0;
    }

    CHECK(&r[src]);

    ASSERT(r[dst].modules == NULL);
    need = r[dst].allocated = r[src].n + 1;
    erts_smp_atomic_add_nob(&mem_used, need);
    r[dst].modules = (Range *) erts_alloc(ERTS_ALC_T_MODULE_REFS,
					  need * sizeof(Range));
    n = 0;
    for (i = 0; i < r[src].n; i++) {
	Range* rp = r[src].modules+i;
	if (code < rp->start) {
	    r[dst].modules[n].start = code;
	    erts_smp_atomic_init_nob(&r[dst].modules[n].end,
				     (erts_aint_t)(((byte *)code) + size));
	    ASSERT(!n || RANGE_END(&r[dst].modules[n-1]) < code);
	    n++;
	    break;
	}
	if (rp->start < RANGE_END(rp)) {
	    /* Only insert a module that has not been purged. */
	    r[dst].modules[n].start = rp->start;
	    erts_smp_atomic_init_nob(&r[dst].modules[n].end,
				     (erts_aint_t)(RANGE_END(rp)));
	    ASSERT(!n || RANGE_END(&r[dst].modules[n-1]) < rp->start);
	    n++;
	}
    }

    while (i < r[src].n) {
	Range* rp = r[src].modules+i;
	if (rp->start < RANGE_END(rp)) {
	    /* Only insert a module that has not been purged. */
	    r[dst].modules[n].start = rp->start;
	    erts_smp_atomic_init_nob(&r[dst].modules[n].end,
				     (erts_aint_t)(RANGE_END(rp)));
	    ASSERT(!n || RANGE_END(&r[dst].modules[n-1]) < rp->start);
	    n++;
	}
	i++;
    }

    if (n == 0 || code > r[dst].modules[n-1].start) {
	r[dst].modules[n].start = code;
	erts_smp_atomic_init_nob(&r[dst].modules[n].end,
				 (erts_aint_t)(((byte *)code) + size));
	ASSERT(!n || RANGE_END(&r[dst].modules[n-1]) < code);
	n++;
    }

    ASSERT(n <= r[src].n+1);
    r[dst].n = n;
    erts_smp_atomic_set_nob(&r[dst].mid,
			    (erts_aint_t) (r[dst].modules + n / 2));

    CHECK(&r[dst]);
    CHECK(&r[src]);
}

void
erts_remove_from_ranges(BeamInstr* code)
{
    Range* rp = find_range(code);
    erts_smp_atomic_set_nob(&rp->end, (erts_aint_t)rp->start);
}

UWord
erts_ranges_sz(void)
{
    return erts_smp_atomic_read_nob(&mem_used) * sizeof(Range);
}

/*
 * Find a function from the given pc and fill information in
 * the FunctionInfo struct. If the full_info is non-zero, fill
 * in all available information (including location in the
 * source code). If no function is found, the 'current' field
 * will be set to NULL.
 */

void
erts_lookup_function_info(FunctionInfo* fi, BeamInstr* pc, int full_info)
{
    BeamInstr** low;
    BeamInstr** high;
    BeamInstr** mid;
    Range* rp;

    fi->current = NULL;
    fi->needed = 5;
    fi->loc = LINE_INVALID_LOCATION;
    rp = find_range(pc);
    if (rp == 0) {
	return;
    }

    low = (BeamInstr **) (rp->start + MI_FUNCTIONS);
    high = low + rp->start[MI_NUM_FUNCTIONS];
    while (low < high) {
	mid = low + (high-low) / 2;
	if (pc < mid[0]) {
	    high = mid;
	} else if (pc < mid[1]) {
	    fi->current = mid[0]+2;
	    if (full_info) {
		BeamInstr** fp = (BeamInstr **) (rp->start +
						 MI_FUNCTIONS);
		int idx = mid - fp;
		lookup_loc(fi, pc, rp->start, idx);
	    }
	    return;
	} else {
	    low = mid + 1;
	}
    }
}

static Range*
find_range(BeamInstr* pc)
{
    ErtsCodeIndex active = erts_active_code_ix();
    Range* low = r[active].modules;
    Range* high = low + r[active].n;
    Range* mid = (Range *) erts_smp_atomic_read_nob(&r[active].mid);

    CHECK(&r[active]);
    while (low < high) {
	if (pc < mid->start) {
	    high = mid;
	} else if (pc > RANGE_END(mid)) {
	    low = mid + 1;
	} else {
	    erts_smp_atomic_set_nob(&r[active].mid, (erts_aint_t) mid);
	    return mid;
	}
	mid = low + (high-low) / 2;
    }
    return 0;
}

static void
lookup_loc(FunctionInfo* fi, BeamInstr* orig_pc, BeamInstr* modp, int idx)
{
    Eterm* line = (Eterm *) modp[MI_LINE_TABLE];
    Eterm* low;
    Eterm* high;
    Eterm* mid;
    Eterm pc;

    if (line == 0) {
	return;
    }

    pc = (Eterm) (BeamInstr) orig_pc;
    fi->fname_ptr = (Eterm *) (BeamInstr) line[MI_LINE_FNAME_PTR];
    low = (Eterm *) (BeamInstr) line[MI_LINE_FUNC_TAB+idx];
    high = (Eterm *) (BeamInstr) line[MI_LINE_FUNC_TAB+idx+1];
    while (high > low) {
	mid = low + (high-low) / 2;
	if (pc < mid[0]) {
	    high = mid;
	} else if (pc < mid[1]) {
	    int file;
	    int index = mid - (Eterm *) (BeamInstr) line[MI_LINE_FUNC_TAB];

	    if (line[MI_LINE_LOC_SIZE] == 2) {
		Uint16* loc_table =
		    (Uint16 *) (BeamInstr) line[MI_LINE_LOC_TAB];
		fi->loc = loc_table[index];
	    } else {
		Uint32* loc_table =
		    (Uint32 *) (BeamInstr) line[MI_LINE_LOC_TAB];
		ASSERT(line[MI_LINE_LOC_SIZE] == 4);
		fi->loc = loc_table[index];
	    }
	    if (fi->loc == LINE_INVALID_LOCATION) {
		return;
	    }
	    fi->needed += 3+2+3+2;
	    file = LOC_FILE(fi->loc);
	    if (file == 0) {
		/* Special case: Module name with ".erl" appended */
		Atom* mod_atom = atom_tab(atom_val(fi->current[0]));
		fi->needed += 2*(mod_atom->len+4);
	    } else {
		Atom* ap = atom_tab(atom_val((fi->fname_ptr)[file-1]));
		fi->needed += 2*ap->len;
	    }
	    return;
	} else {
	    low = mid + 1;
	}
    }
}
