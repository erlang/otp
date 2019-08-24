/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
/* This File contains functions which are called if a user hits ^C */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "version.h"
#include "error.h"
#include "version.h"
#include "erl_db.h"
#include "bif.h"
#include "erl_version.h"
#include "hash.h"
#include "atom.h"
#include "beam_load.h"
#include "erl_hl_timer.h"
#include "erl_thr_progress.h"
#include "erl_proc_sig_queue.h"
#include "dist.h"

/* Forward declarations -- should really appear somewhere else */
static void process_killer(void);
void do_break(void);
void erl_crash_dump_v(char *file, int line, char* fmt, va_list args);

#ifdef DEBUG
static void bin_check(void);
#endif

static void print_garb_info(fmtfn_t to, void *to_arg, Process* p);
#ifdef OPPROF
static void dump_frequencies(void);
#endif

static void dump_attributes(fmtfn_t to, void *to_arg, byte* ptr, int size);

extern char* erts_system_version[];

#define WRITE_BUFFER_SIZE (64*1024)

static void
port_info(fmtfn_t to, void *to_arg)
{
    int i, max = erts_ptab_max(&erts_port);
    for (i = 0; i < max; i++) {
	Port *p = erts_pix2port(i);
	if (p)
	    print_port_info(p, to, to_arg);
    }
}

void
process_info(fmtfn_t to, void *to_arg)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int i, max = erts_ptab_max(&erts_proc);
    for (i = 0; i < max; i++) {
	Process *p = erts_pix2proc(i);
	if (p && p->i != ENULL) {
	    /* Do not include processes with no heap,
	     * they are most likely just created and has invalid data
	     */
	    if (p->heap != NULL) {
                ErtsProcLocks locks = ((esdp && (p == esdp->current_process ||
                                                 p == esdp->free_process))
                                       ? ERTS_PROC_LOCK_MAIN : 0);
		print_process_info(to, to_arg, p, locks);
            }
	}
    }

    /* Look for FREE processes in the run-queues and dist entries.
       These have been removed from the ptab but we still want them
       in the crash dump for debugging. */

    /* First loop through all run-queues */
    for (i = 0; i < erts_no_schedulers + ERTS_NUM_DIRTY_RUNQS; i++) {
        ErtsRunQueue *rq = ERTS_RUNQ_IX(i);
        int j;
        for (j = 0; j < ERTS_NO_PROC_PRIO_QUEUES; j++) {
            Process *p = rq->procs.prio[j].first;
            while (p) {
                if (ERTS_PSFLG_FREE & erts_atomic32_read_acqb(&p->state))
                    print_process_info(to, to_arg, p, 0);
                p = p->next;
            }
        }
    }

    /* Then check all dist entries */
    erts_dist_print_procs_suspended_on_de(to, to_arg);

    port_info(to, to_arg);
}

static void
process_killer(void)
{
    int i, j, max = erts_ptab_max(&erts_proc);
    Process* rp;

    erts_printf("\n\nProcess Information\n\n");
    erts_printf("--------------------------------------------------\n");
    for (i = max-1; i >= 0; i--) {
	rp = erts_pix2proc(i);
	if (rp && rp->i != ENULL) {
	    int br;
	    print_process_info(ERTS_PRINT_STDOUT, NULL, rp, 0);
	    erts_printf("(k)ill (n)ext (r)eturn:\n");
	    while(1) {
		if ((j = sys_get_key(0)) <= 0)
		    erts_exit(0, "");
		switch(j) {
		case 'k':
                    ASSERT(erts_init_process_id != ERTS_INVALID_PID);
                    /* Send a 'kill' exit signal from init process */
                    erts_proc_sig_send_exit(NULL, erts_init_process_id,
                                            rp->common.id, am_kill, NIL,
                                            0);
		case 'n': br = 1; break;
		case 'r': return;
		default: return;
		}
		if (br == 1) break;
	    }
	}
    }
}

typedef struct {
    int is_first;
    fmtfn_t to;
    void *to_arg;
} PrintMonitorContext;

static int doit_print_link(ErtsLink *lnk, void *vpcontext, Sint reds)
{
    PrintMonitorContext *pcontext = vpcontext;
    fmtfn_t to = pcontext->to;
    void *to_arg = pcontext->to_arg;

    if (pcontext->is_first) {
	pcontext->is_first = 0;
	erts_print(to, to_arg, "%T", lnk->other.item);
    } else {
	erts_print(to, to_arg, ", %T", lnk->other.item);
    }
    return 1;
}
    

static int doit_print_monitor(ErtsMonitor *mon, void *vpcontext, Sint reds)
{
    ErtsMonitorData *mdp;
    PrintMonitorContext *pcontext = vpcontext;
    fmtfn_t to = pcontext->to;
    void *to_arg = pcontext->to_arg;
    char *prefix = ", ";
 
    mdp = erts_monitor_to_data(mon);
    switch (mon->type) {
    case ERTS_MON_TYPE_PROC:
    case ERTS_MON_TYPE_PORT:
    case ERTS_MON_TYPE_TIME_OFFSET:
    case ERTS_MON_TYPE_DIST_PROC:
    case ERTS_MON_TYPE_RESOURCE:
    case ERTS_MON_TYPE_NODE:

        if (pcontext->is_first) {
            pcontext->is_first = 0;
            prefix = "";
        }

        if (erts_monitor_is_target(mon)) {
            if (mon->type != ERTS_MON_TYPE_RESOURCE)
                erts_print(to, to_arg, "%s{from,%T,%T}", prefix, mon->other.item, mdp->ref);
            else {
                ErtsResource* rsrc = mon->other.ptr;
                erts_print(to, to_arg, "%s{from,{%T,%T},%T}", prefix, rsrc->type->module,
                           rsrc->type->name, mdp->ref);
            }
        }
        else {
            if (!(mon->flags & ERTS_ML_FLG_NAME))
                erts_print(to, to_arg, "%s{to,%T,%T}", prefix, mon->other.item, mdp->ref);
            else {
                ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;
                Eterm node;
                if (mdep->dist)
                    node = mdep->dist->nodename;
                else
                    node = erts_this_dist_entry->sysname;
                erts_print(to, to_arg, "%s{to,{%T,%T},%T}", prefix, mdep->u.name,
                           node, mdp->ref);
            }
        }

        break;

    default:
        /* ignore other monitors... */
        break;
    }
    return 1;
}

/* Display info about an individual Erlang process */
void
print_process_info(fmtfn_t to, void *to_arg, Process *p, ErtsProcLocks orig_locks)
{
    int garbing = 0;
    int running = 0;
    int exiting = 0;
    Sint len;
    struct saved_calls *scb;
    erts_aint32_t state;
    ErtsProcLocks locks = orig_locks;

    /* display the PID */
    erts_print(to, to_arg, "=proc:%T\n", p->common.id);

    /* Display the state */
    erts_print(to, to_arg, "State: ");

    state = erts_atomic32_read_acqb(&p->state);
    erts_dump_process_state(to, to_arg, state);
    if (state & ERTS_PSFLG_GC) {
        garbing = 1;
        running = 1;
    } else if (state & (ERTS_PSFLG_RUNNING
			| ERTS_PSFLG_DIRTY_RUNNING))
        running = 1;

    if (state & ERTS_PSFLG_EXITING)
        exiting = 1;

    if (!(locks & ERTS_PROC_LOCK_MAIN)) {
        locks |= ERTS_PROC_LOCK_MAIN;
        if (ERTS_IS_CRASH_DUMPING) {
            if (erts_proc_trylock(p, locks)) {
                /* crash dumping and main lock taken, this probably means that
                   the process is doing a GC on a dirty-scheduler... so we cannot
                   do erts_proc_sig_fetch as that would potentially cause a segfault */
                locks = 0;
            }
        } else {
            erts_proc_lock(p, locks);
        }
    } else {
        ERTS_ASSERT(locks == ERTS_PROC_LOCK_MAIN && "Only main lock should be held");
    }

    /*
     * If the process is registered as a global process, display the
     * registered name
     */
    if (!ERTS_PROC_IS_EXITING(p) && p->common.u.alive.reg)
	erts_print(to, to_arg, "Name: %T\n", p->common.u.alive.reg->name);

    /*
     * Display the initial function name
     */
    erts_print(to, to_arg, "Spawned as: %T:%T/%bpu\n",
	       p->u.initial.module,
	       p->u.initial.function,
	       p->u.initial.arity);
    
    if (p->current != NULL) {
	if (running) {
	    erts_print(to, to_arg, "Last scheduled in for: ");
	} else {
	    erts_print(to, to_arg, "Current call: ");
	}
	erts_print(to, to_arg, "%T:%T/%bpu\n",
		   p->current->module,
		   p->current->function,
		   p->current->arity);
    }

    erts_print(to, to_arg, "Spawned by: %T\n", p->parent);

    if (locks & ERTS_PROC_LOCK_MAIN) {
        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
        len = erts_proc_sig_fetch(p);
        erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    } else {
        len = p->sig_qs.len;
    }
    erts_print(to, to_arg, "Message queue length: %d\n", len);

    /* display the message queue only if there is anything in it
       and we can do it safely */
    if (!ERTS_IS_CRASH_DUMPING && p->sig_qs.first != NULL && !garbing
        && (locks & ERTS_PROC_LOCK_MAIN)) {
	erts_print(to, to_arg, "Message queue: [");
        ERTS_FOREACH_SIG_PRIVQS(
            p, mp,
            {
                if (ERTS_SIG_IS_NON_MSG((ErtsSignal *) mp))
                    erts_print(to, to_arg, mp->next ? "%T," : "%T",
                               ERL_MESSAGE_TERM(mp));
            });
	erts_print(to, to_arg, "]\n");
    }

    {
       int frags = 0;
       ErlHeapFragment *m = p->mbuf;
       while (m != NULL) {
	   frags++;
	   m = m->next;
       }
       erts_print(to, to_arg, "Number of heap fragments: %d\n", frags);
    }
    erts_print(to, to_arg, "Heap fragment data: %beu\n", MBUF_SIZE(p));

    scb = ERTS_PROC_GET_SAVED_CALLS_BUF(p);
    if (scb) {
       int i, j;

       erts_print(to, to_arg, "Last calls:");
       for (i = 0; i < scb->n; i++) {
	     erts_print(to, to_arg, " ");
	     j = scb->cur - i - 1;
	     if (j < 0)
		j += scb->len;
	     if (scb->ct[j] == &exp_send)
		erts_print(to, to_arg, "send");
	     else if (scb->ct[j] == &exp_receive)
		erts_print(to, to_arg, "'receive'");
	     else if (scb->ct[j] == &exp_timeout)
		   erts_print(to, to_arg, "timeout");
	     else
		 erts_print(to, to_arg, "%T:%T/%bpu\n",
			    scb->ct[j]->info.mfa.module,
			    scb->ct[j]->info.mfa.function,
			    scb->ct[j]->info.mfa.arity);
       }
       erts_print(to, to_arg, "\n");
    }

    /* display the links only if there are any*/
    if (!exiting && (ERTS_P_LINKS(p) || ERTS_P_MONITORS(p) || ERTS_P_LT_MONITORS(p))) {
	PrintMonitorContext context = {1, to, to_arg};
	erts_print(to, to_arg,"Link list: [");
	erts_link_tree_foreach(ERTS_P_LINKS(p), doit_print_link, &context);	
	erts_monitor_tree_foreach(ERTS_P_MONITORS(p), doit_print_monitor, &context);
	erts_monitor_list_foreach(ERTS_P_LT_MONITORS(p), doit_print_monitor, &context);
	erts_print(to, to_arg,"]\n");
    }

    if (!ERTS_IS_CRASH_DUMPING) {

	/* and the dictionary */
	if (p->dictionary != NULL && !garbing) {
	    erts_print(to, to_arg, "Dictionary: ");
	    erts_dictionary_dump(to, to_arg, p->dictionary);
	    erts_print(to, to_arg, "\n");
	}
    }
    
    /* print the number of reductions etc */
    erts_print(to, to_arg, "Reductions: %beu\n", p->reds);

    erts_print(to, to_arg, "Stack+heap: %beu\n", p->heap_sz);
    erts_print(to, to_arg, "OldHeap: %bpu\n",
               (OLD_HEAP(p) == NULL) ? 0 : (OLD_HEND(p) - OLD_HEAP(p)) );
    erts_print(to, to_arg, "Heap unused: %bpu\n", (p->hend - p->htop));
    erts_print(to, to_arg, "OldHeap unused: %bpu\n",
	       (OLD_HEAP(p) == NULL) ? 0 : (OLD_HEND(p) - OLD_HTOP(p)) );
    erts_print(to, to_arg, "BinVHeap: %b64u\n", p->off_heap.overhead);
    erts_print(to, to_arg, "OldBinVHeap: %b64u\n", BIN_OLD_VHEAP(p));
    erts_print(to, to_arg, "BinVHeap unused: %b64u\n",
               BIN_VHEAP_SZ(p) - p->off_heap.overhead);
    if (BIN_OLD_VHEAP_SZ(p) >= BIN_OLD_VHEAP(p)) {
        erts_print(to, to_arg, "OldBinVHeap unused: %b64u\n",
                   BIN_OLD_VHEAP_SZ(p) - BIN_OLD_VHEAP(p));
    } else {
        erts_print(to, to_arg, "OldBinVHeap unused: overflow\n");
    }
    erts_print(to, to_arg, "Memory: %beu\n", erts_process_memory(p, !0));

    if (garbing) {
	print_garb_info(to, to_arg, p);
    }
    
    if (ERTS_IS_CRASH_DUMPING) {
	erts_program_counter_info(to, to_arg, p);
    } else {
	erts_print(to, to_arg, "Stack dump:\n");
	if (!garbing)
	    erts_stack_dump(to, to_arg, p);
    }

    /* Display all states */
    erts_print(to, to_arg, "Internal State: ");
    erts_dump_extended_process_state(to, to_arg, state);

    erts_proc_unlock(p, locks & ~orig_locks);
}

static void
print_garb_info(fmtfn_t to, void *to_arg, Process* p)
{
    /* A scheduler is probably concurrently doing gc... */
    if (!ERTS_IS_CRASH_DUMPING)
      return;
    erts_print(to, to_arg, "New heap start: %bpX\n", p->heap);
    erts_print(to, to_arg, "New heap top: %bpX\n", p->htop);
    erts_print(to, to_arg, "Stack top: %bpX\n", p->stop);
    erts_print(to, to_arg, "Stack end: %bpX\n", p->hend);
    erts_print(to, to_arg, "Old heap start: %bpX\n", OLD_HEAP(p));
    erts_print(to, to_arg, "Old heap top: %bpX\n", OLD_HTOP(p));
    erts_print(to, to_arg, "Old heap end: %bpX\n", OLD_HEND(p));
}

void
info(fmtfn_t to, void *to_arg)
{
    erts_memory(&to, to_arg, NULL, THE_NON_VALUE);
    atom_info(to, to_arg);
    module_info(to, to_arg);
    export_info(to, to_arg);
    register_info(to, to_arg);
    erts_fun_info(to, to_arg);
    erts_node_table_info(to, to_arg);
    erts_dist_table_info(to, to_arg);
    erts_allocated_areas(&to, to_arg, NULL);
    erts_allocator_info(to, to_arg);

}

static int code_size(struct erl_module_instance* modi)
{
    int size = modi->code_length;

    if (modi->code_hdr) {
        ErtsLiteralArea* lit = modi->code_hdr->literal_area;
        if (lit)
            size += (lit->end - lit->start) * sizeof(Eterm);
    }
    return size;
}

void
loaded(fmtfn_t to, void *to_arg)
{
    int i;
    int old = 0;
    int cur = 0;
    BeamCodeHeader* code;
    Module* modp;
    ErtsCodeIndex code_ix;

    code_ix = erts_active_code_ix();
    erts_rlock_old_code(code_ix);

    /*
     * Calculate and print totals.
     */
    for (i = 0; i < module_code_size(code_ix); i++) {
	if ((modp = module_code(i, code_ix)) != NULL) {
	    cur += code_size(&modp->curr);
            old += code_size(&modp->old);
	}
    }
    erts_print(to, to_arg, "Current code: %d\n", cur);
    erts_print(to, to_arg, "Old code: %d\n", old);
    
    /*
     * Print one line per module.
     */

    for (i = 0; i < module_code_size(code_ix); i++) {
	modp = module_code(i, code_ix);
	if (!ERTS_IS_CRASH_DUMPING) {
	    /*
	     * Interactive dump; keep it brief.
	     */
	    if (modp != NULL && ((modp->curr.code_length != 0) ||
                                 (modp->old.code_length != 0))) {
		erts_print(to, to_arg, "%T %d", make_atom(modp->module),
                           code_size(&modp->curr));
		if (modp->old.code_length != 0)
		    erts_print(to, to_arg, " (%d old)", code_size(&modp->old));
		erts_print(to, to_arg, "\n");
	    }
	} else {
	    /*
	     * To crash dump; make it parseable.
	     */
	    if (modp != NULL && ((modp->curr.code_length != 0) ||
                                 (modp->old.code_length != 0))) {
		erts_print(to, to_arg, "=mod:");
		erts_print(to, to_arg, "%T", make_atom(modp->module));
		erts_print(to, to_arg, "\n");
		erts_print(to, to_arg, "Current size: %d\n",
			   code_size(&modp->curr));
		code = modp->curr.code_hdr;
		if (code != NULL && code->attr_ptr) {
		    erts_print(to, to_arg, "Current attributes: ");
		    dump_attributes(to, to_arg, code->attr_ptr,
				    code->attr_size);
		}
		if (code != NULL && code->compile_ptr) {
		    erts_print(to, to_arg, "Current compilation info: ");
		    dump_attributes(to, to_arg, code->compile_ptr,
				    code->compile_size);
		}

		if (modp->old.code_length != 0) {
		    erts_print(to, to_arg, "Old size: %d\n", code_size(&modp->old));
		    code = modp->old.code_hdr;
		    if (code->attr_ptr) {
			erts_print(to, to_arg, "Old attributes: ");
			dump_attributes(to, to_arg, code->attr_ptr,
					code->attr_size);
		    }
		    if (code->compile_ptr) {
			erts_print(to, to_arg, "Old compilation info: ");
			dump_attributes(to, to_arg, code->compile_ptr,
					code->compile_size);
		    }
		}
	    }
	}
    }
    erts_runlock_old_code(code_ix);
}


static void
dump_attributes(fmtfn_t to, void *to_arg, byte* ptr, int size)
{
    erts_print_base64(to, to_arg, ptr, size);
    erts_print(to, to_arg, "\n");
}


void
do_break(void)
{
    int i;
#ifdef __WIN32__
    char *mode; /* enough for storing "window" */

    /* check if we're in console mode and, if so,
       halt immediately if break is called */
    mode = erts_read_env("ERL_CONSOLE_MODE");
    if (mode && sys_strcmp(mode, "window") != 0)
	erts_exit(0, "");
    erts_free_read_env(mode);
#endif /* __WIN32__ */

    ASSERT(erts_thr_progress_is_blocking());

    erts_printf("\n"
		"BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded\n"
		"       (v)ersion (k)ill (D)b-tables (d)istribution\n");

    while (1) {
	if ((i = sys_get_key(0)) <= 0)
	    erts_exit(0, "");
	switch (i) {
	case 'q':
	case 'a': 
	case '*': /* 
		   * The asterisk is an read error on windows, 
		   * where sys_get_key isn't that great in console mode.
		   * The usual reason for a read error is Ctrl-C. Treat this as
		   * 'a' to avoid infinite loop.
		   */
	    erts_exit(0, "");
	case 'A':		/* Halt generating crash dump */
	    erts_exit(ERTS_ERROR_EXIT, "Crash dump requested by user");
	case 'c':
	    return;
	case 'p':
	    process_info(ERTS_PRINT_STDOUT, NULL);
	    return;
	case 'm':
	    return;
	case 'o':
	    port_info(ERTS_PRINT_STDOUT, NULL);
	    return;
	case 'i':
	    info(ERTS_PRINT_STDOUT, NULL);
	    return;
	case 'l':
	    loaded(ERTS_PRINT_STDOUT, NULL);
	    return;
	case 'v':
	    erts_printf("Erlang (%s) emulator version "
		       ERLANG_VERSION "\n",
		       EMULATOR);
#if ERTS_SAVED_COMPILE_TIME
	    erts_printf("Compiled on " ERLANG_COMPILE_DATE "\n");
#endif
	    return;
	case 'd':
	    distribution_info(ERTS_PRINT_STDOUT, NULL);
	    return;
	case 'D':
	    db_info(ERTS_PRINT_STDOUT, NULL, 1);
	    return; 
	case 'k':
	    process_killer();
	    return;
#ifdef OPPROF
	case 'X':
	    dump_frequencies();
	    return;
	case 'x':
	    {
		int i;
		for (i = 0; i <= HIGHEST_OP; i++) {
		    if (opc[i].name != NULL) {
			erts_printf("%-16s %8d\n", opc[i].name, opc[i].count);
		    }
		}
	    }
	    return;
	case 'z':
	    {
		int i;
		for (i = 0; i <= HIGHEST_OP; i++)
		    opc[i].count = 0;
	    }
	    return;
#endif
#ifdef DEBUG
	case 't':
	    /* erts_p_slpq(); */
	    return;
	case 'b':
	    bin_check();
	    return;
	case 'C':
	    abort();
#endif
	case '\n':
	    continue;
	default:
	    erts_printf("Eh?\n\n");
	}
    }

}


#ifdef OPPROF
static void
dump_frequencies(void)
{
    int i;
    FILE* fp;
    time_t now;
    static char name[] = "op_freq.dump";

    fp = fopen(name, "w");
    if (fp == NULL) {
	fprintf(stderr, "Failed to open %s for writing\n", name);
	return;
    }

    time(&now);
    fprintf(fp, "# Generated %s\n", ctime(&now));

    for (i = 0; i <= HIGHEST_OP; i++) {
	if (opc[i].name != NULL) {
	    fprintf(fp, "%s %d\n", opc[i].name, opc[i].count);
	}
    }
    fclose(fp);
    erts_printf("Frequencies dumped to %s\n", name);
}
#endif


#ifdef DEBUG

static void 
bin_check(void)
{
    Process  *rp;
    struct erl_off_heap_header* hdr;
    int i, printed = 0, max = erts_ptab_max(&erts_proc);

    for (i=0; i < max; i++) {
	rp = erts_pix2proc(i);
	if (!rp)
	    continue;
	for (hdr = rp->off_heap.first; hdr; hdr = hdr->next) {
	    if (hdr->thing_word == HEADER_PROC_BIN) {
		ProcBin *bp = (ProcBin*) hdr;
		if (!printed) {
		    erts_printf("Process %T holding binary data \n", rp->common.id);
		    printed = 1;
		}
		erts_printf("%p orig_size: %bpd, norefs = %bpd\n",
			    bp->val, 
			    bp->val->orig_size, 
			    erts_refc_read(&bp->val->intern.refc, 1));
	    }
	}
	if (printed) {
	    erts_printf("--------------------------------------\n");
	    printed = 0;
	}
    }
    /* db_bin_check() has to be rewritten for the AVL trees... */
    /*db_bin_check();*/ 
}

#endif

static Sint64 crash_dump_limit = ERTS_SINT64_MAX;
static Sint64 crash_dump_written = 0;

typedef struct LimitedWriterInfo_ {
    fmtfn_t to;
    void* to_arg;
} LimitedWriterInfo;

static int
crash_dump_limited_writer(void* vfdp, char* buf, size_t len)
{
    const char stop_msg[] = "\n=abort:CRASH DUMP SIZE LIMIT REACHED\n";
    LimitedWriterInfo* lwi = (LimitedWriterInfo *) vfdp;

    crash_dump_written += len;
    if (crash_dump_written <= crash_dump_limit) {
        return lwi->to(lwi->to_arg, buf, len);
    }

    len -= (crash_dump_written - crash_dump_limit);
    lwi->to(lwi->to_arg, buf, len);
    lwi->to(lwi->to_arg, (char*)stop_msg, sizeof(stop_msg)-1);
    if (lwi->to == &erts_write_fp) {
        fclose((FILE *) lwi->to_arg);
    }

    /* We assume that crash dump was called from erts_exit_vv() */
    erts_exit_epilogue();
}

/* XXX THIS SHOULD BE IN SYSTEM !!!! */
void
erl_crash_dump_v(char *file, int line, char* fmt, va_list args)
{
    ErtsThrPrgrData tpd_buf; /* in case we aren't a managed thread... */
    int fd;
    size_t envsz;
    time_t now;
    char env[21]; /* enough to hold any 64-bit integer */
    size_t dumpnamebufsize = MAXPATHLEN;
    char dumpnamebuf[MAXPATHLEN];
    char* dumpname;
    int secs;
    int env_erl_crash_dump_seconds_set = 1;
    int i;
    fmtfn_t to = &erts_write_fd;
    void*   to_arg;
    FILE* fp = 0;
    LimitedWriterInfo lwi;
    static char* write_buffer;  /* 'static' to avoid a leak warning in valgrind */

    if (ERTS_SOMEONE_IS_CRASH_DUMPING)
	return;

    /* Order all managed threads to block, this has to be done
       first to guarantee that this is the only thread to generate
       crash dump. */
    erts_thr_progress_fatal_error_block(&tpd_buf);

#ifdef ERTS_SYS_SUSPEND_SIGNAL
    /*
     * We suspend all scheduler threads so that we can dump some
     * data about the currently running processes and scheduler data.
     * We have to be very very careful when doing this as the schedulers
     * could be anywhere.
     */
    sys_init_suspend_handler();

    for (i = 0; i < erts_no_schedulers; i++) {
        erts_tid_t tid = ERTS_SCHEDULER_IX(i)->tid;
        if (!erts_equal_tids(tid,erts_thr_self()))
            sys_thr_suspend(tid);
    }

#endif

    /* Allow us to pass certain places without locking... */
    erts_atomic32_set_mb(&erts_writing_erl_crash_dump, 1);
    erts_tsd_set(erts_is_crash_dumping_key, (void *) 1);


    envsz = sizeof(env);
    /* ERL_CRASH_DUMP_SECONDS not set
     * if we have a heart port, break immediately
     * otherwise dump crash indefinitely (until crash is complete)
     * same as ERL_CRASH_DUMP_SECONDS = 0
     * - do not write dump
     * - do not set an alarm
     * - break immediately
     *
     * ERL_CRASH_DUMP_SECONDS = 0
     * - do not write dump
     * - do not set an alarm
     * - break immediately
     *
     * ERL_CRASH_DUMP_SECONDS < 0
     * - do not set alarm
     * - write dump until done
     *
     * ERL_CRASH_DUMP_SECONDS = S (and S positive)
     * - Don't dump file forever
     * - set alarm (set in sys)
     * - write dump until alarm or file is written completely
     */
	
    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP_SECONDS", env, &envsz) == 1) {
        env_erl_crash_dump_seconds_set = 1;
        secs = atoi(env);
    } else {
        env_erl_crash_dump_seconds_set = 0;
        secs = -1;
    }

    if (secs == 0) {
        return;
    }

    /* erts_sys_prepare_crash_dump returns 1 if heart port is found, otherwise 0
     * If we don't find heart (0) and we don't have ERL_CRASH_DUMP_SECONDS set
     * we should continue writing a dump
     *
     * beware: secs -1 means no alarm
     */

    if (erts_sys_prepare_crash_dump(secs) && !env_erl_crash_dump_seconds_set ) {
	return;
    }

    crash_dump_limit = ERTS_SINT64_MAX;
    envsz = sizeof(env);
    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP_BYTES", env, &envsz) == 1) {
        Sint64 limit;
        char* endptr;
        errno = 0;
        limit = ErtsStrToSint64(env, &endptr, 10);
        if (errno == 0 && limit >= 0 && endptr != env && *endptr == 0) {
            if (limit == 0)
                return;
            crash_dump_limit = limit;
            to = &crash_dump_limited_writer;
        }
    }

    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP",&dumpnamebuf[0],&dumpnamebufsize) != 1)
	dumpname = "erl_crash.dump";
    else
	dumpname = &dumpnamebuf[0];
    
    erts_fprintf(stderr,"\nCrash dump is being written to: %s...", dumpname);

    fd = open(dumpname,O_WRONLY | O_CREAT | O_TRUNC,0640);
    if (fd < 0)
	return; /* Can't create the crash dump, skip it */

    /*
     * Wrap into a FILE* so that we can use buffered output. Set an
     * explicit buffer to make sure the first write does not fail because
     * of a failure to allocate a buffer.
     */
    write_buffer = (char *) erts_alloc_fnf(ERTS_ALC_T_TMP, WRITE_BUFFER_SIZE);
    if (write_buffer && (fp = fdopen(fd, "w")) != NULL) {
        setvbuf(fp, write_buffer, _IOFBF, WRITE_BUFFER_SIZE);
        lwi.to = &erts_write_fp;
        lwi.to_arg = (void*)fp;
    } else {
        lwi.to = &erts_write_fd;
        lwi.to_arg = (void*)&fd;
    }
    if (to == &crash_dump_limited_writer) {
        to_arg = (void *) &lwi;
    } else {
        to = lwi.to;
        to_arg = lwi.to_arg;
    }

    time(&now);
    erts_cbprintf(to, to_arg, "=erl_crash_dump:0.5\n%s", ctime(&now));

    if (file != NULL)
       erts_cbprintf(to, to_arg, "The error occurred in file %s, line %d\n", file, line);

    if (fmt != NULL && *fmt != '\0') {
	erts_cbprintf(to, to_arg, "Slogan: ");
	erts_vcbprintf(to, to_arg, fmt, args);
    }
    erts_cbprintf(to, to_arg, "System version: ");
    erts_print_system_version(to, to_arg, NULL);
#if ERTS_SAVED_COMPILE_TIME
    erts_cbprintf(to, to_arg, "%s\n", "Compiled: " ERLANG_COMPILE_DATE);
#endif

    erts_cbprintf(to, to_arg, "Taints: ");
    erts_print_nif_taints(to, to_arg);
    erts_cbprintf(to, to_arg, "Atoms: %d\n", atom_table_size());

    /* We want to note which thread it was that called erts_exit */
    if (erts_get_scheduler_data()) {
        erts_cbprintf(to, to_arg, "Calling Thread: scheduler:%d\n",
                      erts_get_scheduler_data()->no);
    } else {
        if (!erts_thr_getname(erts_thr_self(), dumpnamebuf, MAXPATHLEN))
            erts_cbprintf(to, to_arg, "Calling Thread: %s\n", dumpnamebuf);
        else
            erts_cbprintf(to, to_arg, "Calling Thread: %p\n", erts_thr_self());
    }

#if defined(ERTS_HAVE_TRY_CATCH)

    /*
     * erts_print_scheduler_info is not guaranteed to be safe to call
     * here for all schedulers as we may have suspended a scheduler
     * in the middle of updating the STACK_TOP and STACK_START
     * variables and thus when scanning the stack we could get
     * segmentation faults. We protect against this very unlikely
     * scenario by using the ERTS_SYS_TRY_CATCH.
     */
    for (i = 0; i < erts_no_schedulers; i++) {
        ERTS_SYS_TRY_CATCH(
            erts_print_scheduler_info(to, to_arg, ERTS_SCHEDULER_IX(i)),
            erts_cbprintf(to, to_arg, "** crashed **\n"));
    }
    for (i = 0; i < erts_no_dirty_cpu_schedulers; i++) {
        ERTS_SYS_TRY_CATCH(
            erts_print_scheduler_info(to, to_arg, ERTS_DIRTY_CPU_SCHEDULER_IX(i)),
            erts_cbprintf(to, to_arg, "** crashed **\n"));
    }
    erts_cbprintf(to, to_arg, "=dirty_cpu_run_queue\n");
    erts_print_run_queue_info(to, to_arg, ERTS_DIRTY_CPU_RUNQ);

    for (i = 0; i < erts_no_dirty_io_schedulers; i++) {
        ERTS_SYS_TRY_CATCH(
            erts_print_scheduler_info(to, to_arg, ERTS_DIRTY_IO_SCHEDULER_IX(i)),
            erts_cbprintf(to, to_arg, "** crashed **\n"));
    }
    erts_cbprintf(to, to_arg, "=dirty_io_run_queue\n");
    erts_print_run_queue_info(to, to_arg, ERTS_DIRTY_IO_RUNQ);
#endif


#ifdef ERTS_SYS_SUSPEND_SIGNAL

    /* We resume all schedulers so that we are in a known safe state
       when we write the rest of the crash dump */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_tid_t tid = ERTS_SCHEDULER_IX(i)->tid;
        if (!erts_equal_tids(tid,erts_thr_self()))
	    sys_thr_resume(tid);
    }
#endif

    /*
     * Wait for all managed threads to block. If all threads haven't blocked
     * after a minute, we go anyway and hope for the best...
     *
     * We do not release system again. We expect an exit() or abort() after
     * dump has been written.
     */
    erts_thr_progress_fatal_error_wait(60000);
    /* Either worked or not... */

#ifndef ERTS_HAVE_TRY_CATCH
    /* This is safe to call here, as all schedulers are blocked */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_print_scheduler_info(to, to_arg, ERTS_SCHEDULER_IX(i));
    }
#endif
    
    info(to, to_arg); /* General system info */
    if (erts_ptab_initialized(&erts_proc))
	process_info(to, to_arg); /* Info about each process and port */
    db_info(to, to_arg, 0);
    erts_print_bif_timer_info(to, to_arg);
    distribution_info(to, to_arg);
    erts_cbprintf(to, to_arg, "=loaded_modules\n");
    loaded(to, to_arg);
    erts_dump_fun_entries(to, to_arg);
    erts_deep_process_dump(to, to_arg);
    erts_cbprintf(to, to_arg, "=atoms\n");
    dump_atoms(to, to_arg);

    erts_cbprintf(to, to_arg, "=end\n");
    if (fp) {
        fclose(fp);
    }
    close(fd);
    erts_fprintf(stderr,"done\n");
}

void
erts_print_base64(fmtfn_t to, void *to_arg, byte* src, Uint size)
{
    static const byte base64_chars[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    while (size >= 3) {
        erts_putc(to, to_arg, base64_chars[src[0] >> 2]);
        erts_putc(to, to_arg, base64_chars[((src[0] & 0x03) << 4) | (src[1] >> 4)]);
        erts_putc(to, to_arg, base64_chars[((src[1] & 0x0f) << 2) | (src[2] >> 6)]);
        erts_putc(to, to_arg, base64_chars[src[2] & 0x3f]);
        size -= 3;
        src += 3;
    }
    if (size == 1) {
        erts_putc(to, to_arg, base64_chars[src[0] >> 2]);
        erts_putc(to, to_arg, base64_chars[(src[0] & 0x03) << 4]);
        erts_print(to, to_arg, "==");
    } else if (size == 2) {
        erts_putc(to, to_arg, base64_chars[src[0] >> 2]);
        erts_putc(to, to_arg, base64_chars[((src[0] & 0x03) << 4) | (src[1] >> 4)]);
        erts_putc(to, to_arg, base64_chars[(src[1] & 0x0f) << 2]);
        erts_putc(to, to_arg, '=');
    }
}
