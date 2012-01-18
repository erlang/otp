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

static void lookup_loc(FunctionInfo* fi, BeamInstr* pc,
		       BeamInstr* modp, int idx);

/*
 * The following variables keep a sorted list of address ranges for
 * each module.  It allows us to quickly find a function given an
 * instruction pointer.
 */
static Range* modules = NULL;  /* Sorted lists of module addresses. */
static int num_loaded_modules; /* Number of loaded modules. */
static int allocated_modules;  /* Number of slots allocated. */
static Range* mid_module = NULL; /* Cached search start point */

void
erts_init_ranges(void)
{
    allocated_modules = 128;
    modules = (Range *) erts_alloc(ERTS_ALC_T_MODULE_REFS,
				   allocated_modules*sizeof(Range));
    mid_module = modules;
    num_loaded_modules = 0;
}

void
erts_update_ranges(BeamInstr* code, Uint size)
{
    int i;

    if (num_loaded_modules == allocated_modules) {
	allocated_modules *= 2;
	modules = (Range *) erts_realloc(ERTS_ALC_T_MODULE_REFS,
					 (void *) modules,
					 allocated_modules * sizeof(Range));
    }
    for (i = num_loaded_modules; i > 0; i--) {
	if (code > modules[i-1].start) {
	    break;
	}
	modules[i] = modules[i-1];
    }
    modules[i].start = code;
    modules[i].end = (BeamInstr *) (((byte *)code) + size);
    num_loaded_modules++;
    mid_module = &modules[num_loaded_modules/2];
}

void
erts_remove_from_ranges(BeamInstr* code)
{
    int i;

    for (i = 0; i < num_loaded_modules; i++) {
	if (modules[i].start == code) {
	    num_loaded_modules--;
	    while (i < num_loaded_modules) {
		modules[i] = modules[i+1];
		i++;
	    }
	    mid_module = &modules[num_loaded_modules/2];
	    return;
	}
    }
    ASSERT(0);			/* Not found? */
}

Uint
erts_ranges_sz(void)
{
    return allocated_modules*sizeof(Range);
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
    Range* low = modules;
    Range* high = low + num_loaded_modules;
    Range* mid = mid_module;

    fi->current = NULL;
    fi->needed = 5;
    fi->loc = LINE_INVALID_LOCATION;
    while (low < high) {
	if (pc < mid->start) {
	    high = mid;
	} else if (pc > mid->end) {
	    low = mid + 1;
	} else {
	    BeamInstr** low1 = (BeamInstr **) (mid->start + MI_FUNCTIONS);
	    BeamInstr** high1 = low1 + mid->start[MI_NUM_FUNCTIONS];
	    BeamInstr** mid1;

	    while (low1 < high1) {
		mid1 = low1 + (high1-low1) / 2;
		if (pc < mid1[0]) {
		    high1 = mid1;
		} else if (pc < mid1[1]) {
		    mid_module = mid;
		    fi->current = mid1[0]+2;
		    if (full_info) {
			BeamInstr** fp = (BeamInstr **) (mid->start +
							 MI_FUNCTIONS);
			int idx = mid1 - fp;
			lookup_loc(fi, pc, mid->start, idx);
		    }
		    return;
		} else {
		    low1 = mid1 + 1;
		}
	    }
	    return;
	}
	mid = low + (high-low) / 2;
    }
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
