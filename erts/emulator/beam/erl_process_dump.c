/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2018. All Rights Reserved.
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
#include "erl_map.h"
#define ERTS_WANT_EXTERNAL_TAGS
#include "external.h"
#include "erl_proc_sig_queue.h"

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
void erts_print_base64(fmtfn_t to, void *to_arg,
                       byte* src, Uint size);
static void dump_externally(fmtfn_t to, void *to_arg, Eterm term);
static void mark_literal(Eterm* ptr);
static void init_literal_areas(void);
static void dump_literals(fmtfn_t to, void *to_arg);
static void dump_module_literals(fmtfn_t to, void *to_arg,
                                 ErtsLiteralArea* lit_area);

static Binary* all_binaries;

extern BeamInstr beam_apply[];
extern BeamInstr beam_exit[];
extern BeamInstr beam_continue_exit[];

void
erts_deep_process_dump(fmtfn_t to, void *to_arg)
{
    int i, max = erts_ptab_max(&erts_proc);

    all_binaries = NULL;
    init_literal_areas();

    for (i = 0; i < max; i++) {
	Process *p = erts_pix2proc(i);
	if (p && p->i != ENULL) {
	    erts_aint32_t state = erts_atomic32_read_acqb(&p->state);
	    if (state & ERTS_PSFLG_EXITING)
                continue;
            if (state & ERTS_PSFLG_GC) {
                ErtsSchedulerData *sdp = erts_get_scheduler_data();
                if (!sdp || p != sdp->current_process)
                    continue;

                /* We want to dump the garbing process that caused the dump */
            }

            dump_process_info(to, to_arg, p);
       }
    }

    dump_literals(to, to_arg);
    dump_binaries(to, to_arg, all_binaries);
}

static void
monitor_size(ErtsMonitor *mon, void *vsize)
{
    *((Uint *) vsize) += erts_monitor_size(mon);
}

static void
link_size(ErtsMonitor *lnk, void *vsize)
{
    *((Uint *) vsize) += erts_link_size(lnk);
}

Uint erts_process_memory(Process *p, int incl_msg_inq) {
  Uint size = 0;
  struct saved_calls *scb;
  size += sizeof(Process);

  if (incl_msg_inq) {
      erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
      erts_proc_sig_fetch(p);
      erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
  }

  erts_link_tree_foreach(ERTS_P_LINKS(p),
                         link_size, (void *) &size);
  erts_monitor_tree_foreach(ERTS_P_MONITORS(p),
                            monitor_size, (void *) &size);
  erts_monitor_list_foreach(ERTS_P_LT_MONITORS(p),
                            monitor_size, (void *) &size);
  size += (p->heap_sz + p->mbuf_sz) * sizeof(Eterm);
  if (p->abandoned_heap)
      size += (p->hend - p->heap) * sizeof(Eterm);
  if (p->old_hend && p->old_heap)
    size += (p->old_hend - p->old_heap) * sizeof(Eterm);


  size += p->sig_qs.len * sizeof(ErtsMessage);

  ERTS_FOREACH_SIG_PRIVQS(
      p, mp,
      {
          if (ERTS_SIG_IS_NON_MSG((ErtsSignal *) mp))
              size += erts_proc_sig_signal_size((ErtsSignal *) mp);
          else if (mp->data.attached)
              size += erts_msg_attached_data_size(mp) * sizeof(Eterm);
      });

  if (p->arg_reg != p->def_arg_reg) {
    size += p->arity * sizeof(p->arg_reg[0]);
  }

  if (erts_atomic_read_nob(&p->psd) != (erts_aint_t) NULL)
    size += sizeof(ErtsPSD);

  scb = ERTS_PROC_GET_SAVED_CALLS_BUF(p);
  if (scb) {
    size += (sizeof(struct saved_calls)
	     + (scb->len-1) * sizeof(scb->ct[0]));
  }

  size += erts_dicts_mem_size(p);
  return size;
}

static ERTS_INLINE void
dump_msg(fmtfn_t to, void *to_arg, ErtsMessage *mp)
{
    if (ERTS_SIG_IS_MSG((ErtsSignal *) mp)) {
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

static ERTS_INLINE void
heap_dump_msg(fmtfn_t to, void *to_arg, ErtsMessage *mp)
{
    if (ERTS_SIG_IS_MSG((ErtsSignal *) mp)) {
        Eterm mesg = ERL_MESSAGE_TERM(mp);
        if (is_value(mesg))
            heap_dump(to, to_arg, mesg);
        mesg = ERL_MESSAGE_TOKEN(mp);
        heap_dump(to, to_arg, mesg);
    }
}

static void
dump_process_info(fmtfn_t to, void *to_arg, Process *p)
{
    Eterm* sp;
    int yreg = -1;

    if (ERTS_TRACE_FLAGS(p) & F_SENSITIVE)
        return;

    erts_proc_sig_fetch(p);

    if (p->sig_qs.first || p->sig_qs.cont) {
	erts_print(to, to_arg, "=proc_messages:%T\n", p->common.id);
        ERTS_FOREACH_SIG_PRIVQS(p, mp, dump_msg(to, to_arg, mp));
    }

    if (p->dictionary) {
        erts_print(to, to_arg, "=proc_dictionary:%T\n", p->common.id);
        erts_deep_dictionary_dump(to, to_arg,
                                  p->dictionary, dump_element_nl);
    }

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

    if (p->sig_qs.first || p->sig_qs.cont)
        ERTS_FOREACH_SIG_PRIVQS(p, mp, heap_dump_msg(to, to_arg, mp));

    if (p->dictionary) {
        erts_deep_dictionary_dump(to, to_arg, p->dictionary, heap_dump);
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
        if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR) {
            byte sbuf[3];
            int i = 0;

            sbuf[i++] = VERSION_MAGIC;
            while (i < sizeof(sbuf) && e < edep->ext_endp) {
                sbuf[i++] = *e++;
            }
            erts_print_base64(to, to_arg, sbuf, i);
        }
        erts_print_base64(to, to_arg, e, edep->ext_endp - e);
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
            if (erts_is_literal(x, ptr)) {
                mark_literal(ptr);
            } else if (ptr[0] != OUR_NIL) {
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
            if (erts_is_literal(x, ptr)) {
                mark_literal(ptr);
            } else if (hdr != OUR_NIL) {
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

		    if (tag == HEAP_BINARY_SUBTAG) {
			byte* p;

			erts_print(to, to_arg, "Yh%X:", size);
			p = binary_bytes(x);
                        erts_print_base64(to, to_arg, p, size);
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
		} else if (is_map_header(hdr)) {
                    if (is_flatmap_header(hdr)) {
                        flatmap_t* fmp = (flatmap_t *) flatmap_val(x);
                        Eterm* values = ptr + sizeof(flatmap_t) / sizeof(Eterm);
                        Uint map_size = fmp->size;
                        int i;

                        erts_print(to, to_arg, "Mf" ETERM_FMT ":", map_size);
                        dump_element(to, to_arg, fmp->keys);
                        erts_putc(to, to_arg, ':');
                        for (i = 0; i < map_size; i++) {
                            dump_element(to, to_arg, values[i]);
                            if (is_immed(values[i])) {
                                values[i] = make_small(0);
                            }
                            if (i < map_size-1) {
                                erts_putc(to, to_arg, ',');
                            }
                        }
                        erts_putc(to, to_arg, '\n');
                        *ptr = OUR_NIL;
                        x = fmp->keys;
                        if (map_size) {
                            fmp->keys = (Eterm) next;
                            next = &values[map_size-1];
                        }
                        continue;
                    } else {
                        Uint i;
                        Uint sz = 0;
                        Eterm* nodes = ptr + 1;

                        switch (MAP_HEADER_TYPE(hdr)) {
                        case MAP_HEADER_TAG_HAMT_HEAD_ARRAY:
                            nodes++;
                            sz = 16;
                            erts_print(to, to_arg, "Mh" ETERM_FMT ":" ETERM_FMT ":",
                                       hashmap_size(x), sz);
                            break;
                        case MAP_HEADER_TAG_HAMT_HEAD_BITMAP:
                            nodes++;
                            sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                            erts_print(to, to_arg, "Mh" ETERM_FMT ":" ETERM_FMT ":",
                                       hashmap_size(x), sz);
                            break;
                        case MAP_HEADER_TAG_HAMT_NODE_BITMAP:
                            sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                            erts_print(to, to_arg, "Mn" ETERM_FMT ":", sz);
                            break;
                        }
                        *ptr = OUR_NIL;
                        for (i = 0; i < sz; i++) {
                            dump_element(to, to_arg, nodes[i]);
                            if (is_immed(nodes[i])) {
                                nodes[i] = make_small(0);
                            }
                            if (i < sz-1) {
                                erts_putc(to, to_arg, ',');
                            }
                        }
                        erts_putc(to, to_arg, '\n');
                        x = nodes[0];
                        nodes[0] = (Eterm) next;
                        next = &nodes[sz-1];
                        continue;
                    }
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
	SWord size = current->orig_size;
	byte* bytes = (byte*) current->orig_bytes;

	erts_print(to, to_arg, "=binary:" PTR_FMT "\n", current);
	erts_print(to, to_arg, "%X:", size);
        erts_print_base64(to, to_arg, bytes, size);
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

    s = p = sbuf;
    erts_encode_ext(term, &p);
    erts_print(to, to_arg, "E%X:", p-s);
    erts_print_base64(to, to_arg, sbuf, p-s);
}

/*
 * Handle dumping of literal areas.
 */

static ErtsLiteralArea** lit_areas;
static Uint num_lit_areas;

static int compare_areas(const void * a, const void * b)
{
    ErtsLiteralArea** a_p = (ErtsLiteralArea **) a;
    ErtsLiteralArea** b_p = (ErtsLiteralArea **) b;

    if (*a_p < *b_p) {
        return -1;
    } else if (*b_p < *a_p) {
        return 1;
    } else {
        return 0;
    }
}


static void
init_literal_areas(void)
{
    int i;
    Module* modp;
    ErtsCodeIndex code_ix;
    ErtsLiteralArea** area_p;

    code_ix = erts_active_code_ix();
    erts_rlock_old_code(code_ix);

    lit_areas = area_p = erts_dump_lit_areas;
    num_lit_areas = 0;
    for (i = 0; i < module_code_size(code_ix); i++) {
	modp = module_code(i, code_ix);
        if (modp == NULL) {
            continue;
        }
        if (modp->curr.code_length > 0 &&
            modp->curr.code_hdr->literal_area) {
            *area_p++ = modp->curr.code_hdr->literal_area;
        }
        if (modp->old.code_length > 0 && modp->old.code_hdr->literal_area) {
            *area_p++ = modp->old.code_hdr->literal_area;
        }
    }

    num_lit_areas = area_p - lit_areas;
    ASSERT(num_lit_areas <= erts_dump_num_lit_areas);
    for (i = 0; i < num_lit_areas; i++) {
        lit_areas[i]->off_heap = 0;
    }

    qsort(lit_areas, num_lit_areas, sizeof(ErtsLiteralArea *),
          compare_areas);

    erts_runlock_old_code(code_ix);
}

static int search_areas(const void * a, const void * b) {
    Eterm* key = (Eterm *) a;
    ErtsLiteralArea** b_p = (ErtsLiteralArea **) b;
    if (key < b_p[0]->start) {
        return -1;
    } else if (b_p[0]->end <= key) {
        return 1;
    } else {
        return 0;
    }
}

static void mark_literal(Eterm* ptr)
{
    ErtsLiteralArea** ap;

    ap = bsearch(ptr, lit_areas, num_lit_areas, sizeof(ErtsLiteralArea*),
                 search_areas);

    /*
     * If the literal was created by native code, this search will not
     * find it and ap will be NULL.
     */

    if (ap) {
        ap[0]->off_heap = (struct erl_off_heap_header *) 1;
    }
}


static void
dump_literals(fmtfn_t to, void *to_arg)
{
    ErtsCodeIndex code_ix;
    int i;

    code_ix = erts_active_code_ix();
    erts_rlock_old_code(code_ix);

    erts_print(to, to_arg, "=literals\n");
    for (i = 0; i < num_lit_areas; i++) {
        if (lit_areas[i]->off_heap) {
            dump_module_literals(to, to_arg, lit_areas[i]);
        }
    }

    erts_runlock_old_code(code_ix);
}

static void
dump_module_literals(fmtfn_t to, void *to_arg, ErtsLiteralArea* lit_area)
{
    Eterm* htop;
    Eterm* hend;

    htop = lit_area->start;
    hend = lit_area->end;
    while (htop < hend) {
        Eterm w = *htop;
        Eterm term;
        Uint size;

        switch (primary_tag(w)) {
        case TAG_PRIMARY_HEADER:
            term = make_boxed(htop);
            erts_print(to, to_arg, PTR_FMT ":", htop);
            if (is_arity_value(w)) {
                Uint i;
                Uint arity = arityval(w);

                erts_print(to, to_arg, "t" ETERM_FMT ":", arity);
                for (i = 1; i <= arity; i++) {
                    dump_element(to, to_arg, htop[i]);
                    if (i < arity) {
                        erts_putc(to, to_arg, ',');
                    }
                }
                erts_putc(to, to_arg, '\n');
            } else if (w == HEADER_FLONUM) {
                FloatDef f;
                char sbuf[31];
                int i;

                GET_DOUBLE_DATA((htop+1), f);
                i = sys_double_to_chars(f.fd, sbuf, sizeof(sbuf));
                sys_memset(sbuf+i, 0, 31-i);
                erts_print(to, to_arg, "F%X:%s\n", i, sbuf);
            } else if (_is_bignum_header(w)) {
                erts_print(to, to_arg, "B%T\n", term);
            } else if (is_binary_header(w)) {
                Uint tag = thing_subtag(w);
                Uint size = binary_size(term);

                if (tag == HEAP_BINARY_SUBTAG) {
                    byte* p;

                    erts_print(to, to_arg, "Yh%X:", size);
                    p = binary_bytes(term);
                    erts_print_base64(to, to_arg, p, size);
                } else if (tag == REFC_BINARY_SUBTAG) {
                    ProcBin* pb = (ProcBin *) binary_val(term);
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
                    ErlSubBin* Sb = (ErlSubBin *) binary_val(term);
                    Eterm* real_bin;
                    void* val;

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
            } else if (is_map_header(w)) {
                if (is_flatmap_header(w)) {
                    flatmap_t* fmp = (flatmap_t *) flatmap_val(term);
                    Eterm* values = htop + sizeof(flatmap_t) / sizeof(Eterm);
                    Uint map_size = fmp->size;
                    int i;

                    erts_print(to, to_arg, "Mf" ETERM_FMT ":", map_size);
                    dump_element(to, to_arg, fmp->keys);
                    erts_putc(to, to_arg, ':');
                    for (i = 0; i < map_size; i++) {
                        dump_element(to, to_arg, values[i]);
                        if (i < map_size-1) {
                            erts_putc(to, to_arg, ',');
                        }
                    }
                    erts_putc(to, to_arg, '\n');
                } else {
                    Uint i;
                    Uint sz = 0;
                    Eterm* nodes = htop + 1;

                    switch (MAP_HEADER_TYPE(w)) {
                    case MAP_HEADER_TAG_HAMT_HEAD_ARRAY:
                        nodes++;
                        sz = 16;
                        erts_print(to, to_arg, "Mh" ETERM_FMT ":" ETERM_FMT ":",
                                   hashmap_size(term), sz);
                        break;
                    case MAP_HEADER_TAG_HAMT_HEAD_BITMAP:
                        nodes++;
                        sz = hashmap_bitcount(MAP_HEADER_VAL(w));
                        erts_print(to, to_arg, "Mh" ETERM_FMT ":" ETERM_FMT ":",
                                   hashmap_size(term), sz);
                        break;
                    case MAP_HEADER_TAG_HAMT_NODE_BITMAP:
                        sz = hashmap_bitcount(MAP_HEADER_VAL(w));
                        erts_print(to, to_arg, "Mn" ETERM_FMT ":", sz);
                        break;
                    }
                    for (i = 0; i < sz; i++) {
                        dump_element(to, to_arg, nodes[i]);
                        if (i < sz-1) {
                            erts_putc(to, to_arg, ',');
                        }
                    }
                    erts_putc(to, to_arg, '\n');
                }
            } else if (is_export_header(w)) {
                dump_externally(to, to_arg, term);
                erts_putc(to, to_arg, '\n');
            }
            size = 1 + header_arity(w);
            switch (w & _HEADER_SUBTAG_MASK) {
            case MAP_SUBTAG:
                if (is_flatmap_header(w)) {
                    size += 1 + flatmap_get_size(htop);
                } else {
                    size += hashmap_bitcount(MAP_HEADER_VAL(w));
                }
                break;
            case SUB_BINARY_SUBTAG:
                size += 1;
                break;
            }
            break;
        default:
            ASSERT(!is_header(htop[1]));
            erts_print(to, to_arg, PTR_FMT ":l", htop);
            dump_element(to, to_arg, htop[0]);
            erts_putc(to, to_arg, '|');
            dump_element(to, to_arg, htop[1]);
            erts_putc(to, to_arg, '\n');
            size = 2;
            break;
        }
        htop += size;
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
            case ERTS_PSFLG_UNUSED:
                erts_print(to, to_arg, "UNUSED"); break;
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
            case ERTS_PSFLG_SYS_TASKS:
                erts_print(to, to_arg, "SYS_TASKS"); break;
            case ERTS_PSFLG_SIG_IN_Q:
                erts_print(to, to_arg, "SIG_IN_Q"); break;
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
            case ERTS_PSFLG_SIG_Q:
                erts_print(to, to_arg, "SIG_Q"); break;
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
