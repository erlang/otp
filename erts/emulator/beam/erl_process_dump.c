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

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_db.h"
#include "dist.h"
#include "beam_catches.h"
#include "erl_binary.h"
#define ERTS_WANT_EXTERNAL_TAGS
#include "external.h"

#define PTR_FMT "%bpX"
#define ETERM_FMT "%beX"

#define OUR_NIL	_make_header(0,_TAG_HEADER_FLOAT)

static void dump_process_info(fmtfn_t to, void *to_arg, Process *p);
static void dump_element(fmtfn_t to, void *to_arg, Eterm x);
static void dump_dist_ext(fmtfn_t to, void *to_arg, ErtsDistExternal *edep);
static void dump_element_nl(fmtfn_t to, void *to_arg, Eterm x);
static int stack_element_dump(fmtfn_t to, void *to_arg, Eterm* sp,
			      int yreg);
static void stack_trace_dump(fmtfn_t to, void *to_arg, Eterm* sp);
static void print_function_from_pc(fmtfn_t to, void *to_arg, BeamInstr* x);
static void heap_dump(fmtfn_t to, void *to_arg, Eterm x);
static void dump_binaries(fmtfn_t to, void *to_arg, Binary* root);
static void dump_externally(fmtfn_t to, void *to_arg, Eterm term);

static Binary* all_binaries;

extern BeamInstr beam_apply[];
extern BeamInstr beam_exit[];
extern BeamInstr beam_continue_exit[];


void
erts_deep_process_dump(fmtfn_t to, void *to_arg)
{
    int i, max = erts_ptab_max(&erts_proc);

    all_binaries = NULL;
    
    for (i = 0; i < max; i++) {
	Process *p = erts_pix2proc(i);
	if (p && p->i != ENULL) {
	    erts_aint32_t state = erts_smp_atomic32_read_acqb(&p->state);
	    if (!(state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_GC)))
		dump_process_info(to, to_arg, p);
       }
    }

    dump_binaries(to, to_arg, all_binaries);
}

Uint erts_process_memory(Process *p, int incl_msg_inq) {
  ErtsMessage *mp;
  Uint size = 0;
  struct saved_calls *scb;
  size += sizeof(Process);

  if (incl_msg_inq)
      ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);

  erts_doforall_links(ERTS_P_LINKS(p), &erts_one_link_size, &size);
  erts_doforall_monitors(ERTS_P_MONITORS(p), &erts_one_mon_size, &size);
  size += (p->heap_sz + p->mbuf_sz) * sizeof(Eterm);
  if (p->abandoned_heap)
      size += (p->hend - p->heap) * sizeof(Eterm);
  if (p->old_hend && p->old_heap)
    size += (p->old_hend - p->old_heap) * sizeof(Eterm);


  size += p->msg.len * sizeof(ErtsMessage);

  for (mp = p->msg.first; mp; mp = mp->next)
    if (mp->data.attached)
      size += erts_msg_attached_data_size(mp)*sizeof(Eterm);

  if (p->arg_reg != p->def_arg_reg) {
    size += p->arity * sizeof(p->arg_reg[0]);
  }

  if (erts_smp_atomic_read_nob(&p->psd) != (erts_aint_t) NULL)
    size += sizeof(ErtsPSD);

  scb = ERTS_PROC_GET_SAVED_CALLS_BUF(p);
  if (scb) {
    size += (sizeof(struct saved_calls)
	     + (scb->len-1) * sizeof(scb->ct[0]));
  }

  size += erts_dicts_mem_size(p);
  return size;
}

static void
dump_process_info(fmtfn_t to, void *to_arg, Process *p)
{
    Eterm* sp;
    ErtsMessage* mp;
    int yreg = -1;

    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);

    if ((ERTS_TRACE_FLAGS(p) & F_SENSITIVE) == 0 && p->msg.first) {
	erts_print(to, to_arg, "=proc_messages:%T\n", p->common.id);
	for (mp = p->msg.first; mp != NULL; mp = mp->next) {
	    Eterm mesg = ERL_MESSAGE_TERM(mp);
	    if (is_value(mesg))
		dump_element(to, to_arg, mesg);
	    else
		dump_dist_ext(to, to_arg, mp->data.dist_ext);
	    mesg = ERL_MESSAGE_TOKEN(mp);
	    erts_print(to, to_arg, ":");
	    dump_element(to, to_arg, mesg);
	    erts_print(to, to_arg, "\n");
	}
    }

    if ((ERTS_TRACE_FLAGS(p) & F_SENSITIVE) == 0) {
	if (p->dictionary) {
	    erts_print(to, to_arg, "=proc_dictionary:%T\n", p->common.id);
	    erts_deep_dictionary_dump(to, to_arg,
				      p->dictionary, dump_element_nl);
	}
    }

    if ((ERTS_TRACE_FLAGS(p) & F_SENSITIVE) == 0) {
	erts_print(to, to_arg, "=proc_stack:%T\n", p->common.id);
	for (sp = p->stop; sp < STACK_START(p); sp++) {
	    yreg = stack_element_dump(to, to_arg, sp, yreg);
	}

	erts_print(to, to_arg, "=proc_heap:%T\n", p->common.id);
	for (sp = p->stop; sp < STACK_START(p); sp++) {
	    Eterm term = *sp;
	    
	    if (!is_catch(term) && !is_CP(term)) {
		heap_dump(to, to_arg, term);
	    }
	}
	for (mp = p->msg.first; mp != NULL; mp = mp->next) {
	    Eterm mesg = ERL_MESSAGE_TERM(mp);
	    if (is_value(mesg))
		heap_dump(to, to_arg, mesg);
	    mesg = ERL_MESSAGE_TOKEN(mp);
	    heap_dump(to, to_arg, mesg);
	}
	if (p->dictionary) {
	    erts_deep_dictionary_dump(to, to_arg, p->dictionary, heap_dump);
	}
    }
}

static void
dump_dist_ext(fmtfn_t to, void *to_arg, ErtsDistExternal *edep)
{
    if (!edep)
	erts_print(to, to_arg, "D0:E0:");
    else {
	byte *e;
	size_t sz;
	if (!(edep->flags & ERTS_DIST_EXT_ATOM_TRANS_TAB))
	    erts_print(to, to_arg, "D0:");
	else {
	    int i;
	    erts_print(to, to_arg, "D%X:", edep->attab.size);
	    for (i = 0; i < edep->attab.size; i++)
		dump_element(to, to_arg, edep->attab.atom[i]);
	}
	sz = edep->ext_endp - edep->extp;
	e = edep->extp;
	if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR) {
	    ASSERT(*e != VERSION_MAGIC);
	    sz++;
	}
	else {
	    ASSERT(*e == VERSION_MAGIC);
	}

	erts_print(to, to_arg, "E%X:", sz);
	if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR)
	    erts_print(to, to_arg, "%02X", VERSION_MAGIC);
	while (e < edep->ext_endp)
	    erts_print(to, to_arg, "%02X", *e++);
    }
}

static void
dump_element(fmtfn_t to, void *to_arg, Eterm x)
{
    if (is_list(x)) {
	erts_print(to, to_arg, "H" PTR_FMT, list_val(x));
    } else if (is_boxed(x)) {
	erts_print(to, to_arg, "H" PTR_FMT, boxed_val(x));
    } else if (is_immed(x)) {
	if (is_atom(x)) {
	    unsigned char* s = atom_tab(atom_val(x))->name;
	    int len = atom_tab(atom_val(x))->len;
	    int i;

	    erts_print(to, to_arg, "A%X:", atom_tab(atom_val(x))->len);
	    for (i = 0; i < len; i++) {
		erts_putc(to, to_arg, *s++);
	    }
	} else if (is_small(x)) {
	    erts_print(to, to_arg, "I%T", x);
	} else if (is_pid(x)) {
	    erts_print(to, to_arg, "P%T", x);
	} else if (is_port(x)) {
	    erts_print(to, to_arg, "p<%beu.%beu>",
		       port_channel_no(x), port_number(x));
	} else if (is_nil(x)) {
	    erts_putc(to, to_arg, 'N');
	}
    }
}

static void
dump_element_nl(fmtfn_t to, void *to_arg, Eterm x)
{
    dump_element(to, to_arg, x);
    erts_putc(to, to_arg, '\n');
}

static void
stack_trace_dump(fmtfn_t to, void *to_arg, Eterm *sp) {
    Eterm x = *sp;
    if (is_CP(x)) {
        erts_print(to, to_arg, "%p:", sp);
        erts_print(to, to_arg, "SReturn addr 0x%X (", cp_val(x));
        print_function_from_pc(to, to_arg, cp_val(x));
        erts_print(to, to_arg, ")\n");
    }
}

void
erts_limited_stack_trace(fmtfn_t to, void *to_arg, Process *p)
{
    Eterm* sp;


    if (ERTS_TRACE_FLAGS(p) & F_SENSITIVE) {
	return;
    }

    if (STACK_START(p) < STACK_TOP(p)) {
        return;
    }

    if ((STACK_START(p) - STACK_TOP(p)) < 512) {
        if (erts_sys_is_area_readable((char*)STACK_TOP(p),
                                      (char*)STACK_START(p)))
            for (sp = STACK_TOP(p); sp < STACK_START(p); sp++)
                stack_trace_dump(to, to_arg, sp);
        else
            erts_print(to, to_arg, "Could not read from stack memory: %p - %p\n",
                       STACK_TOP(p), STACK_START(p));
    } else {
        sp = STACK_TOP(p);
        if (erts_sys_is_area_readable((char*)STACK_TOP(p),
                                      (char*)(STACK_TOP(p) + 25)))
            for (; sp < (STACK_TOP(p) + 256); sp++)
                stack_trace_dump(to, to_arg, sp);
        else
            erts_print(to, to_arg, "Could not read from stack memory: %p - %p\n",
                       STACK_TOP(p), STACK_TOP(p) + 256);

        erts_print(to, to_arg, "%p: skipping %d frames\n",
                   sp, STACK_START(p) - STACK_TOP(p) - 512);

        if (erts_sys_is_area_readable((char*)(STACK_START(p) - 256),
                                      (char*)STACK_START(p)))
            for (sp = STACK_START(p) - 256; sp < STACK_START(p); sp++)
                stack_trace_dump(to, to_arg, sp);
        else
            erts_print(to, to_arg, "Could not read from stack memory: %p - %p\n",
                       STACK_START(p) - 256, STACK_START(p));
    }

}

static int
stack_element_dump(fmtfn_t to, void *to_arg, Eterm* sp, int yreg)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erts_print(to, to_arg, "%p:", sp);
    } else {
        erts_print(to, to_arg, "y%d:", yreg);
        yreg++;
    }

    if (is_CP(x)) {
        erts_print(to, to_arg, "SReturn addr 0x%X (", cp_val(x));
        print_function_from_pc(to, to_arg, cp_val(x));
        erts_print(to, to_arg, ")\n");
        yreg = 0;
    } else if is_catch(x) {
        erts_print(to, to_arg, "SCatch 0x%X (", catch_pc(x));
        print_function_from_pc(to, to_arg, catch_pc(x));
        erts_print(to, to_arg, ")\n");
    } else {
	dump_element(to, to_arg, x);
	erts_putc(to, to_arg, '\n');
    }
    return yreg;
}

static void
print_function_from_pc(fmtfn_t to, void *to_arg, BeamInstr* x)
{
    ErtsCodeMFA* cmfa = find_function_from_pc(x);
    if (cmfa == NULL) {
        if (x == beam_exit) {
            erts_print(to, to_arg, "<terminate process>");
        } else if (x == beam_continue_exit) {
            erts_print(to, to_arg, "<continue terminate process>");
        } else if (x == beam_apply+1) {
            erts_print(to, to_arg, "<terminate process normally>");
        } else {
            erts_print(to, to_arg, "unknown function");
        }
    } else {
	erts_print(to, to_arg, "%T:%T/%bpu + %bpu",
		   cmfa->module, cmfa->function, cmfa->arity,
                   (x-(BeamInstr*)cmfa) * sizeof(Eterm));
    }
}

static void
heap_dump(fmtfn_t to, void *to_arg, Eterm x)
{
    DeclareTmpHeapNoproc(last,1);
    Eterm* next = last;
    Eterm* ptr;

    if (is_immed(x) || is_CP(x)) {
	return;
    }
    UseTmpHeapNoproc(1);
    *last = OUR_NIL;

    while (x != OUR_NIL) {
	if (is_CP(x)) {
	    next = (Eterm *) x;
	} else if (is_list(x)) {
	    ptr = list_val(x);
	    if (ptr[0] != OUR_NIL) {
		erts_print(to, to_arg, PTR_FMT ":l", ptr);
		dump_element(to, to_arg, ptr[0]);
		erts_putc(to, to_arg, '|');
		dump_element(to, to_arg, ptr[1]);
		erts_putc(to, to_arg, '\n');
		if (is_immed(ptr[1])) {
		    ptr[1] = make_small(0);
		}
		x = ptr[0];
		ptr[0] = (Eterm) next;
		next = ptr + 1;
		continue;
	    }
	} else if (is_boxed(x)) {
	    Eterm hdr;

	    ptr = boxed_val(x);
	    hdr = *ptr;
	    if (hdr != OUR_NIL) {	/* If not visited */
		erts_print(to, to_arg, PTR_FMT ":", ptr);
	        if (is_arity_value(hdr)) {
		    Uint i;
		    Uint arity = arityval(hdr);

		    erts_print(to, to_arg, "t" ETERM_FMT ":", arity);
		    for (i = 1; i <= arity; i++) {
			dump_element(to, to_arg, ptr[i]);
			if (is_immed(ptr[i])) {
			    ptr[i] = make_small(0);
			}
			if (i < arity) {
			    erts_putc(to, to_arg, ',');
			}
		    }
		    erts_putc(to, to_arg, '\n');
		    if (arity == 0) {
			ptr[0] = OUR_NIL;
		    } else {
			x = ptr[arity];
			ptr[0] = (Eterm) next;
			next = ptr + arity - 1;
			continue;
		    }
		} else if (hdr == HEADER_FLONUM) {
		    FloatDef f;
		    char sbuf[31];
		    int i;

		    GET_DOUBLE_DATA((ptr+1), f);
		    i = sys_double_to_chars(f.fd, (char*) sbuf, sizeof(sbuf));
		    sys_memset(sbuf+i, 0, 31-i);
		    erts_print(to, to_arg, "F%X:%s\n", i, sbuf);
		    *ptr = OUR_NIL;
		} else if (_is_bignum_header(hdr)) {
		    erts_print(to, to_arg, "B%T\n", x);
		    *ptr = OUR_NIL;
		} else if (is_binary_header(hdr)) {
		    Uint tag = thing_subtag(hdr);
		    Uint size = binary_size(x);
		    Uint i;

		    if (tag == HEAP_BINARY_SUBTAG) {
			byte* p;

			erts_print(to, to_arg, "Yh%X:", size);
			p = binary_bytes(x);
			for (i = 0; i < size; i++) {
			    erts_print(to, to_arg, "%02X", p[i]);
			}
		    } else if (tag == REFC_BINARY_SUBTAG) {
			ProcBin* pb = (ProcBin *) binary_val(x);
			Binary* val = pb->val;

			if (erts_atomic_xchg_nob(&val->intern.refc, 0) != 0) {
			    val->intern.flags = (UWord) all_binaries;
			    all_binaries = val;
			}
			erts_print(to, to_arg,
				   "Yc" PTR_FMT ":" PTR_FMT ":" PTR_FMT,
				   val,
				   pb->bytes - (byte *)val->orig_bytes,
				   size);
		    } else if (tag == SUB_BINARY_SUBTAG) {
			ErlSubBin* Sb = (ErlSubBin *) binary_val(x);
			Eterm* real_bin;
			void* val;

			/*
			 * Must use boxed_val() here, because the original
			 * binary may have been visited and have had its
			 * header word changed to OUR_NIL (in which case
			 * binary_val() will cause an assertion failure in
			 * the DEBUG emulator).
			 */

			real_bin = boxed_val(Sb->orig);

			if (thing_subtag(*real_bin) == REFC_BINARY_SUBTAG) {
			    /*
			     * Unvisited REFC_BINARY: Point directly to
			     * the binary.
			     */
			    ProcBin* pb = (ProcBin *) real_bin;
			    val = pb->val;
			} else {
			    /*
			     * Heap binary or visited REFC binary: Point
			     * to heap binary or ProcBin on the heap.
			     */
			    val = real_bin;
			}
			erts_print(to, to_arg,
				   "Ys" PTR_FMT ":" PTR_FMT ":" PTR_FMT,
				   val, Sb->offs, size);
		    }
		    erts_putc(to, to_arg, '\n');
		    *ptr = OUR_NIL;
		} else if (is_external_pid_header(hdr)) {
		    erts_print(to, to_arg, "P%T\n", x);
		    *ptr = OUR_NIL;
		} else if (is_external_port_header(hdr)) {
		    erts_print(to, to_arg, "p<%beu.%beu>\n",
			       port_channel_no(x), port_number(x));
		    *ptr = OUR_NIL;
		} else {
		    /*
		     * All other we dump in the external term format.
		     */
			dump_externally(to, to_arg, x);
		    erts_putc(to, to_arg, '\n');
		    *ptr = OUR_NIL;
		}
	    }
	}
	x = *next;
	*next = OUR_NIL;
	next--;
    }
    UnUseTmpHeapNoproc(1);
}

static void
dump_binaries(fmtfn_t to, void *to_arg, Binary* current)
{
    while (current) {
	long i;
	long size = current->orig_size;
	byte* bytes = (byte*) current->orig_bytes;

	erts_print(to, to_arg, "=binary:" PTR_FMT "\n", current);
	erts_print(to, to_arg, "%X:", size);
	for (i = 0; i < size; i++) {
	    erts_print(to, to_arg, "%02X", bytes[i]);
	}
	erts_putc(to, to_arg, '\n');
	current = (Binary *) current->intern.flags;
    }
}

static void
dump_externally(fmtfn_t to, void *to_arg, Eterm term)
{
    byte sbuf[1024]; /* encode and hope for the best ... */
    byte* s; 
    byte* p;

    if (is_fun(term)) {
	/*
	 * The fun's environment used to cause trouble. There were
	 * two kind of problems:
	 *
	 * 1. A term used in the environment could already have been
	 *    dumped and thus destroyed (since dumping is destructive).
	 *
	 * 2. A term in the environment could be too big, so that
	 *    the buffer for external format overflowed (allocating
	 *    memory is not really a solution, as it could be exhausted).
	 *
	 * Simple solution: Set all variables in the environment to NIL.
	 * The crashdump_viewer does not allow inspection of them anyway.
	 */
	ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	Uint num_free = funp->num_free;
	Uint i;

	for (i = 0; i < num_free; i++) {
	    funp->env[i] = NIL;
	}
    }

    /* Do not handle maps */
    if (is_map(term)) {
        term = am_undefined;
    }

    s = p = sbuf;
    erts_encode_ext(term, &p);
    erts_print(to, to_arg, "E%X:", p-s);
    while (s < p) {
	erts_print(to, to_arg, "%02X", *s++);
    }
}

void erts_dump_process_state(fmtfn_t to, void *to_arg, erts_aint32_t psflg)
{
    char *s;
    switch (erts_process_state2status(psflg)) {
    case am_free: s = "Non Existing"; break; /* Should never happen */
    case am_exiting: s = "Exiting"; break;
    case am_garbage_collecting: s = "Garbing"; break;
    case am_suspended: s = "Suspended"; break;
    case am_running: s = "Running"; break;
    case am_runnable: s = "Scheduled"; break;
    case am_waiting: s = "Waiting"; break;
    default: s = "Undefined"; break; /* Should never happen */
    }

    erts_print(to, to_arg, "%s\n", s);
}

void
erts_dump_extended_process_state(fmtfn_t to, void *to_arg, erts_aint32_t psflg) {

    int i;

    switch (ERTS_PSFLGS_GET_ACT_PRIO(psflg)) {
    case PRIORITY_MAX: erts_print(to, to_arg, "ACT_PRIO_MAX | "); break;
    case PRIORITY_HIGH: erts_print(to, to_arg, "ACT_PRIO_HIGH | "); break;
    case PRIORITY_NORMAL: erts_print(to, to_arg, "ACT_PRIO_NORMAL | "); break;
    case PRIORITY_LOW: erts_print(to, to_arg, "ACT_PRIO_LOW | "); break;
    }
    switch (ERTS_PSFLGS_GET_USR_PRIO(psflg)) {
    case PRIORITY_MAX: erts_print(to, to_arg, "USR_PRIO_MAX | "); break;
    case PRIORITY_HIGH: erts_print(to, to_arg, "USR_PRIO_HIGH | "); break;
    case PRIORITY_NORMAL: erts_print(to, to_arg, "USR_PRIO_NORMAL | "); break;
    case PRIORITY_LOW: erts_print(to, to_arg, "USR_PRIO_LOW | "); break;
    }
    switch (ERTS_PSFLGS_GET_PRQ_PRIO(psflg)) {
    case PRIORITY_MAX: erts_print(to, to_arg, "PRQ_PRIO_MAX"); break;
    case PRIORITY_HIGH: erts_print(to, to_arg, "PRQ_PRIO_HIGH"); break;
    case PRIORITY_NORMAL: erts_print(to, to_arg, "PRQ_PRIO_NORMAL"); break;
    case PRIORITY_LOW: erts_print(to, to_arg, "PRQ_PRIO_LOW"); break;
    }

    psflg &= ~(ERTS_PSFLGS_ACT_PRIO_MASK |
               ERTS_PSFLGS_USR_PRIO_MASK |
               ERTS_PSFLGS_PRQ_PRIO_MASK);

    if (psflg)
        erts_print(to, to_arg, " | ");

    for (i = 0; i <= ERTS_PSFLG_MAX && psflg; i++) {
        erts_aint32_t chk = (1 << i);
        if (psflg & chk) {
            switch (chk) {
            case ERTS_PSFLG_IN_PRQ_MAX:
                erts_print(to, to_arg, "IN_PRQ_MAX"); break;
            case ERTS_PSFLG_IN_PRQ_HIGH:
                erts_print(to, to_arg, "IN_PRQ_HIGH"); break;
            case ERTS_PSFLG_IN_PRQ_NORMAL:
                erts_print(to, to_arg, "IN_PRQ_NORMAL"); break;
            case ERTS_PSFLG_IN_PRQ_LOW:
                erts_print(to, to_arg, "IN_PRQ_LOW"); break;
            case ERTS_PSFLG_FREE:
                erts_print(to, to_arg, "FREE"); break;
            case ERTS_PSFLG_EXITING:
                erts_print(to, to_arg, "EXITING"); break;
            case ERTS_PSFLG_PENDING_EXIT:
                erts_print(to, to_arg, "PENDING_EXIT"); break;
            case ERTS_PSFLG_ACTIVE:
                erts_print(to, to_arg, "ACTIVE"); break;
            case ERTS_PSFLG_IN_RUNQ:
                erts_print(to, to_arg, "IN_RUNQ"); break;
            case ERTS_PSFLG_RUNNING:
                erts_print(to, to_arg, "RUNNING"); break;
            case ERTS_PSFLG_SUSPENDED:
                erts_print(to, to_arg, "SUSPENDED"); break;
            case ERTS_PSFLG_GC:
                erts_print(to, to_arg, "GC"); break;
            case ERTS_PSFLG_BOUND:
                erts_print(to, to_arg, "BOUND"); break;
            case ERTS_PSFLG_TRAP_EXIT:
                erts_print(to, to_arg, "TRAP_EXIT"); break;
            case ERTS_PSFLG_ACTIVE_SYS:
                erts_print(to, to_arg, "ACTIVE_SYS"); break;
            case ERTS_PSFLG_RUNNING_SYS:
                erts_print(to, to_arg, "RUNNING_SYS"); break;
            case ERTS_PSFLG_PROXY:
                erts_print(to, to_arg, "PROXY"); break;
            case ERTS_PSFLG_DELAYED_SYS:
                erts_print(to, to_arg, "DELAYED_SYS"); break;
            case ERTS_PSFLG_OFF_HEAP_MSGQ:
                erts_print(to, to_arg, "OFF_HEAP_MSGQ"); break;
            case ERTS_PSFLG_ON_HEAP_MSGQ:
                erts_print(to, to_arg, "ON_HEAP_MSGQ"); break;
            case ERTS_PSFLG_DIRTY_CPU_PROC:
                erts_print(to, to_arg, "DIRTY_CPU_PROC"); break;
            case ERTS_PSFLG_DIRTY_IO_PROC:
                erts_print(to, to_arg, "DIRTY_IO_PROC"); break;
            case ERTS_PSFLG_DIRTY_ACTIVE_SYS:
                erts_print(to, to_arg, "DIRTY_ACTIVE_SYS"); break;
            case ERTS_PSFLG_DIRTY_RUNNING:
                erts_print(to, to_arg, "DIRTY_RUNNING"); break;
            case ERTS_PSFLG_DIRTY_RUNNING_SYS:
                erts_print(to, to_arg, "DIRTY_RUNNING_SYS"); break;
            default:
                erts_print(to, to_arg, "UNKNOWN(%d)", chk); break;
            }
            if (psflg > chk)
                erts_print(to, to_arg, " | ");
            psflg -= chk;
        }
    }
    erts_print(to, to_arg, "\n");
}
