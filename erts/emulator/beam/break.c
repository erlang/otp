/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
#include "erl_instrument.h"
#include "erl_hl_timer.h"
#include "erl_thr_progress.h"

/* Forward declarations -- should really appear somewhere else */
static void process_killer(void);
void do_break(void);
void erl_crash_dump_v(char *file, int line, char* fmt, va_list args);
void erl_crash_dump(char* file, int line, char* fmt, ...);

#ifdef DEBUG
static void bin_check(void);
#endif

static void print_garb_info(int to, void *to_arg, Process* p);
#ifdef OPPROF
static void dump_frequencies(void);
#endif

static void dump_attributes(int to, void *to_arg, byte* ptr, int size);

extern char* erts_system_version[];

static void
port_info(int to, void *to_arg)
{
    int i, max = erts_ptab_max(&erts_port);
    for (i = 0; i < max; i++) {
	Port *p = erts_pix2port(i);
	if (p)
	    print_port_info(p, to, to_arg);
    }
}

void
process_info(int to, void *to_arg)
{
    int i, max = erts_ptab_max(&erts_proc);
    for (i = 0; i < max; i++) {
	Process *p = erts_pix2proc(i);
	if (p && p->i != ENULL) {
	    /* Do not include processes with no heap,
	     * they are most likely just created and has invalid data
	     */
	    if (!ERTS_PROC_IS_EXITING(p) && p->heap != NULL)
		print_process_info(to, to_arg, p);
	}
    }

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
	    print_process_info(ERTS_PRINT_STDOUT, NULL, rp);
	    erts_printf("(k)ill (n)ext (r)eturn:\n");
	    while(1) {
		if ((j = sys_get_key(0)) <= 0)
		    erts_exit(0, "");
		switch(j) {
		case 'k': {
		    ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
		    erts_aint32_t state;
		    erts_proc_inc_refc(rp);
		    erts_smp_proc_lock(rp, rp_locks);
		    state = erts_smp_atomic32_read_acqb(&rp->state);
		    if (state & (ERTS_PSFLG_FREE
				 | ERTS_PSFLG_EXITING
				 | ERTS_PSFLG_ACTIVE
				 | ERTS_PSFLG_ACTIVE_SYS
				 | ERTS_PSFLG_IN_RUNQ
				 | ERTS_PSFLG_RUNNING
				 | ERTS_PSFLG_RUNNING_SYS)) {
			erts_printf("Can only kill WAITING processes this way\n");
		    }
		    else {
			(void) erts_send_exit_signal(NULL,
						     NIL,
						     rp,
						     &rp_locks,
						     am_kill,
						     NIL,
						     NULL,
						     0);
		    }
		    erts_smp_proc_unlock(rp, rp_locks);
		    erts_proc_dec_refc(rp);
		}
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
    int to;
    void *to_arg;
} PrintMonitorContext;

static void doit_print_link(ErtsLink *lnk, void *vpcontext)
{
    PrintMonitorContext *pcontext = vpcontext;
    int to = pcontext->to;
    void *to_arg = pcontext->to_arg;

    if (pcontext->is_first) {
	pcontext->is_first = 0;
	erts_print(to, to_arg, "%T", lnk->pid);
    } else {
	erts_print(to, to_arg, ", %T", lnk->pid);
    }
}
    

static void doit_print_monitor(ErtsMonitor *mon, void *vpcontext)
{
    PrintMonitorContext *pcontext = vpcontext;
    int to = pcontext->to;
    void *to_arg = pcontext->to_arg;
    char *prefix = ", ";
 
    if (pcontext->is_first) {
	pcontext->is_first = 0;
	prefix = "";
    }

    if (mon->type == MON_ORIGIN) {
	if (is_atom(mon->pid)) { /* dist by name */
	    ASSERT(is_node_name_atom(mon->pid));
	    erts_print(to, to_arg, "%s{to,{%T,%T},%T}", prefix, mon->name,
		       mon->pid, mon->ref);
	} else if (is_atom(mon->name)){ /* local by name */
	    erts_print(to, to_arg, "%s{to,{%T,%T},%T}", prefix, mon->name,
		       erts_this_dist_entry->sysname, mon->ref);
	} else { /* local and distributed by pid */
	    erts_print(to, to_arg, "%s{to,%T,%T}", prefix, mon->pid, mon->ref);
	}
    } else { /* MON_TARGET */
	erts_print(to, to_arg, "%s{from,%T,%T}", prefix, mon->pid, mon->ref);
    }
}
			       
/* Display info about an individual Erlang process */
void
print_process_info(int to, void *to_arg, Process *p)
{
    time_t approx_started;
    int garbing = 0;
    int running = 0;
    struct saved_calls *scb;
    erts_aint32_t state;

    /* display the PID */
    erts_print(to, to_arg, "=proc:%T\n", p->common.id);

    /* Display the state */
    erts_print(to, to_arg, "State: ");

    state = erts_smp_atomic32_read_acqb(&p->state);
    erts_dump_process_state(to, to_arg, state);
    if (state & ERTS_PSFLG_GC) {
        garbing = 1;
        running = 1;
    } else if (state & ERTS_PSFLG_RUNNING)
        running = 1;

    /*
     * If the process is registered as a global process, display the
     * registered name
     */
    if (p->common.u.alive.reg)
	erts_print(to, to_arg, "Name: %T\n", p->common.u.alive.reg->name);

    /*
     * Display the initial function name
     */
    erts_print(to, to_arg, "Spawned as: %T:%T/%bpu\n",
	       p->u.initial[INITIAL_MOD],
	       p->u.initial[INITIAL_FUN],
	       p->u.initial[INITIAL_ARI]);
    
    if (p->current != NULL) {
	if (running) {
	    erts_print(to, to_arg, "Last scheduled in for: ");
	} else {
	    erts_print(to, to_arg, "Current call: ");
	}
	erts_print(to, to_arg, "%T:%T/%bpu\n",
		   p->current[0],
		   p->current[1],
		   p->current[2]);
    }

    erts_print(to, to_arg, "Spawned by: %T\n", p->parent);
    approx_started = (time_t) p->approx_started;
    erts_print(to, to_arg, "Started: %s", ctime(&approx_started));
    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);
    erts_print(to, to_arg, "Message queue length: %d\n", p->msg.len);

    /* display the message queue only if there is anything in it */
    if (!ERTS_IS_CRASH_DUMPING && p->msg.first != NULL && !garbing) {
	ErlMessage* mp;
	erts_print(to, to_arg, "Message queue: [");
	for (mp = p->msg.first; mp; mp = mp->next)
	    erts_print(to, to_arg, mp->next ? "%T," : "%T", ERL_MESSAGE_TERM(mp));
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
			    scb->ct[j]->code[0],
			    scb->ct[j]->code[1],
			    scb->ct[j]->code[2]);
       }
       erts_print(to, to_arg, "\n");
    }

    /* display the links only if there are any*/
    if (ERTS_P_LINKS(p) || ERTS_P_MONITORS(p)) {
	PrintMonitorContext context = {1,to}; 
	erts_print(to, to_arg,"Link list: [");
	erts_doforall_links(ERTS_P_LINKS(p), &doit_print_link, &context);	
	erts_doforall_monitors(ERTS_P_MONITORS(p), &doit_print_monitor, &context);
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
    erts_print(to, to_arg, "Memory: %beu\n", erts_process_memory(p));

    if (garbing) {
	print_garb_info(to, to_arg, p);
    }
    
    if (ERTS_IS_CRASH_DUMPING) {
	erts_program_counter_info(to, to_arg, p);
    } else {
	erts_print(to, to_arg, "Stack dump:\n");
#ifdef ERTS_SMP
	if (!garbing)
#endif
	    erts_stack_dump(to, to_arg, p);
    }

    /* Display all states */
    erts_print(to, to_arg, "Internal State: ");
    erts_dump_extended_process_state(to, to_arg, state);
}

static void
print_garb_info(int to, void *to_arg, Process* p)
{
    /* ERTS_SMP: A scheduler is probably concurrently doing gc... */
#ifndef ERTS_SMP
    erts_print(to, to_arg, "New heap start: %bpX\n", p->heap);
    erts_print(to, to_arg, "New heap top: %bpX\n", p->htop);
    erts_print(to, to_arg, "Stack top: %bpX\n", p->stop);
    erts_print(to, to_arg, "Stack end: %bpX\n", p->hend);
    erts_print(to, to_arg, "Old heap start: %bpX\n", OLD_HEAP(p));
    erts_print(to, to_arg, "Old heap top: %bpX\n", OLD_HTOP(p));
    erts_print(to, to_arg, "Old heap end: %bpX\n", OLD_HEND(p));
#endif
}

void
info(int to, void *to_arg)
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

void
loaded(int to, void *to_arg)
{
    int i;
    int old = 0;
    int cur = 0;
    BeamInstr* code;
    Module* modp;
    ErtsCodeIndex code_ix;

    code_ix = erts_active_code_ix();
    erts_rlock_old_code(code_ix);

    /*
     * Calculate and print totals.
     */
    for (i = 0; i < module_code_size(code_ix); i++) {
	if ((modp = module_code(i, code_ix)) != NULL &&
	    ((modp->curr.code_length != 0) ||
	     (modp->old.code_length != 0))) {
	    cur += modp->curr.code_length;
	    if (modp->old.code_length != 0) {
		old += modp->old.code_length;
	    }
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
	    if (modp != NULL &&
	    ((modp->curr.code_length != 0) ||
	     (modp->old.code_length != 0))) {
		erts_print(to, to_arg, "%T", make_atom(modp->module));
		cur += modp->curr.code_length;
		erts_print(to, to_arg, " %d", modp->curr.code_length );
		if (modp->old.code_length != 0) {
		    erts_print(to, to_arg, " (%d old)",
			       modp->old.code_length );
		    old += modp->old.code_length;
		}
		erts_print(to, to_arg, "\n");
	    }
	} else {
	    /*
	     * To crash dump; make it parseable.
	     */
	    if (modp != NULL &&
		((modp->curr.code_length != 0) ||
		 (modp->old.code_length != 0))) {
		erts_print(to, to_arg, "=mod:");
		erts_print(to, to_arg, "%T", make_atom(modp->module));
		erts_print(to, to_arg, "\n");
		erts_print(to, to_arg, "Current size: %d\n",
			   modp->curr.code_length);
		code = modp->curr.code;
		if (code != NULL && code[MI_ATTR_PTR]) {
		    erts_print(to, to_arg, "Current attributes: ");
		    dump_attributes(to, to_arg, (byte *) code[MI_ATTR_PTR],
				    code[MI_ATTR_SIZE]);
		}
		if (code != NULL && code[MI_COMPILE_PTR]) {
		    erts_print(to, to_arg, "Current compilation info: ");
		    dump_attributes(to, to_arg, (byte *) code[MI_COMPILE_PTR],
				    code[MI_COMPILE_SIZE]);
		}

		if (modp->old.code_length != 0) {
		    erts_print(to, to_arg, "Old size: %d\n", modp->old.code_length);
		    code = modp->old.code;
		    if (code[MI_ATTR_PTR]) {
			erts_print(to, to_arg, "Old attributes: ");
			dump_attributes(to, to_arg, (byte *) code[MI_ATTR_PTR],
					code[MI_ATTR_SIZE]);
		    }
		    if (code[MI_COMPILE_PTR]) {
			erts_print(to, to_arg, "Old compilation info: ");
			dump_attributes(to, to_arg, (byte *) code[MI_COMPILE_PTR],
					code[MI_COMPILE_SIZE]);
		    }
		}
	    }
	}
    }
    erts_runlock_old_code(code_ix);
}


static void
dump_attributes(int to, void *to_arg, byte* ptr, int size)
{
    while (size-- > 0) {
	erts_print(to, to_arg, "%02X", *ptr++);
    }
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
    if (mode && strcmp(mode, "window") != 0)
	erts_exit(0, "");
    erts_free_read_env(mode);
#endif /* __WIN32__ */

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
	    erts_p_slpq();
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
			    erts_smp_atomic_read_nob(&bp->val->refc));
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

/* XXX THIS SHOULD BE IN SYSTEM !!!! */
void
erl_crash_dump_v(char *file, int line, char* fmt, va_list args)
{
#ifdef ERTS_SMP
    ErtsThrPrgrData tpd_buf; /* in case we aren't a managed thread... */
#endif
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

    if (ERTS_SOMEONE_IS_CRASH_DUMPING)
	return;

#ifdef ERTS_SMP
    /* Order all managed threads to block, this has to be done
       first to guarantee that this is the only thread to generate
       crash dump. */
    erts_thr_progress_fatal_error_block(&tpd_buf);

#ifdef ERTS_THR_HAVE_SIG_FUNCS
    /*
     * We suspend all scheduler threads so that we can dump some
     * data about the currently running processes and scheduler data.
     * We have to be very very careful when doing this as the schedulers
     * could be anywhere.
     */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_tid_t tid = ERTS_SCHEDULER_IX(i)->tid;
        if (!erts_equal_tids(tid,erts_thr_self()))
            sys_thr_suspend(tid);
    }

#endif

    /* Allow us to pass certain places without locking... */
    erts_smp_atomic32_set_mb(&erts_writing_erl_crash_dump, 1);
    erts_smp_tsd_set(erts_is_crash_dumping_key, (void *) 1);

#else /* !ERTS_SMP */
    erts_writing_erl_crash_dump = 1;
#endif /* ERTS_SMP */

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
	
    if (erts_sys_getenv__("ERL_CRASH_DUMP_SECONDS", env, &envsz) != 0) {
	env_erl_crash_dump_seconds_set = 0;
	secs = -1;
    } else {
	env_erl_crash_dump_seconds_set = 1;
	secs = atoi(env);
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

    if (erts_sys_getenv__("ERL_CRASH_DUMP",&dumpnamebuf[0],&dumpnamebufsize) != 0)
	dumpname = "erl_crash.dump";
    else
	dumpname = &dumpnamebuf[0];
    
    erts_fprintf(stderr,"\nCrash dump is being written to: %s...", dumpname);

    fd = open(dumpname,O_WRONLY | O_CREAT | O_TRUNC,0640);
    if (fd < 0)
	return; /* Can't create the crash dump, skip it */
    time(&now);
    erts_fdprintf(fd, "=erl_crash_dump:0.3\n%s", ctime(&now));

    if (file != NULL)
       erts_fdprintf(fd, "The error occurred in file %s, line %d\n", file, line);

    if (fmt != NULL && *fmt != '\0') {
	erts_fdprintf(fd, "Slogan: ");
	erts_vfdprintf(fd, fmt, args);
    }
    erts_fdprintf(fd, "System version: ");
    erts_print_system_version(fd, NULL, NULL);
#if ERTS_SAVED_COMPILE_TIME
    erts_fdprintf(fd, "%s\n", "Compiled: " ERLANG_COMPILE_DATE);
#endif

    erts_fdprintf(fd, "Taints: ");
    erts_print_nif_taints(fd, NULL);
    erts_fdprintf(fd, "Atoms: %d\n", atom_table_size());

#ifdef USE_THREADS
    /* We want to note which thread it was that called erts_exit */
    if (erts_get_scheduler_data()) {
        erts_fdprintf(fd, "Calling Thread: scheduler:%d\n",
                      erts_get_scheduler_data()->no);
    } else {
        if (!erts_thr_getname(erts_thr_self(), dumpnamebuf, MAXPATHLEN))
            erts_fdprintf(fd, "Calling Thread: %s\n", dumpnamebuf);
        else
            erts_fdprintf(fd, "Calling Thread: %p\n", erts_thr_self());
    }
#else
    erts_fdprintf(fd, "Calling Thread: scheduler:1\n");
#endif

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
            erts_print_scheduler_info(fd, NULL, ERTS_SCHEDULER_IX(i)),
            erts_fdprintf(fd, "** crashed **\n"));
    }
#endif

#ifdef ERTS_SMP

#if defined(ERTS_THR_HAVE_SIG_FUNCS)

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
#endif

#ifndef ERTS_HAVE_TRY_CATCH
    /* This is safe to call here, as all schedulers are blocked */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_print_scheduler_info(fd, NULL, ERTS_SCHEDULER_IX(i));
    }
#endif
    
    info(fd, NULL); /* General system info */
    if (erts_ptab_initialized(&erts_proc))
	process_info(fd, NULL); /* Info about each process and port */
    db_info(fd, NULL, 0);
    erts_print_bif_timer_info(fd, NULL);
    distribution_info(fd, NULL);
    erts_fdprintf(fd, "=loaded_modules\n");
    loaded(fd, NULL);
    erts_dump_fun_entries(fd, NULL);
    erts_deep_process_dump(fd, NULL);
    erts_fdprintf(fd, "=atoms\n");
    dump_atoms(fd, NULL);

    /* Keep the instrumentation data at the end of the dump */
    if (erts_instr_memory_map || erts_instr_stat) {
	erts_fdprintf(fd, "=instr_data\n");

	if (erts_instr_stat) {
	    erts_fdprintf(fd, "=memory_status\n");
	    erts_instr_dump_stat_to_fd(fd, 0);
	}
	if (erts_instr_memory_map) {
	    erts_fdprintf(fd, "=memory_map\n");
	    erts_instr_dump_memory_map_to_fd(fd);
	}
    }

    erts_fdprintf(fd, "=end\n");
    close(fd);
    erts_fprintf(stderr,"done\n");
}

void
erl_crash_dump(char* file, int line, char* fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  erl_crash_dump_v(file, line, fmt, args);
  va_end(args);
}
