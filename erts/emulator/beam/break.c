/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
#include "erl_bif_timer.h"

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
    int i;
    for (i = 0; i < erts_max_ports; i++)
	print_port_info(to, to_arg, i);
}

void
process_info(int to, void *to_arg)
{
    int i;
    for (i = 0; i < erts_max_processes; i++) {
	if ((process_tab[i] != NULL) && (process_tab[i]->i != ENULL)) {
	   if (process_tab[i]->status != P_EXITING)
	       print_process_info(to, to_arg, process_tab[i]);
	}
    }

    port_info(to, to_arg);
}

static void
process_killer(void)
{
    int i, j;
    Process* rp;

    erts_printf("\n\nProcess Information\n\n");
    erts_printf("--------------------------------------------------\n");
    for (i = erts_max_processes-1; i >= 0; i--) {
	if (((rp = process_tab[i]) != NULL) && rp->i != ENULL) {
	    int br;
	    print_process_info(ERTS_PRINT_STDOUT, NULL, rp);
	    erts_printf("(k)ill (n)ext (r)eturn:\n");
	    while(1) {
		if ((j = sys_get_key(0)) <= 0)
		    halt_0(0);
		switch(j) {
		case 'k':
		    if (rp->status == P_WAITING) {
			Uint32 rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
			erts_smp_proc_inc_refc(rp);
			erts_smp_proc_lock(rp, rp_locks);
			(void) erts_send_exit_signal(NULL,
						     NIL,
						     rp,
						     &rp_locks,
						     am_kill,
						     NIL,
						     NULL,
						     0);
			erts_smp_proc_unlock(rp, rp_locks);
			erts_smp_proc_dec_refc(rp);
		    }
		    else
			erts_printf("Can only kill WAITING processes this way\n");

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
	    erts_print(to, to_arg,"}");
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
    int garbing = 0;
    int running = 0;
    struct saved_calls *scb;

    /* display the PID */
    erts_print(to, to_arg, "=proc:%T\n", p->id);

    /* Display the state */
    erts_print(to, to_arg, "State: ");
    switch (p->status) {
    case P_FREE:
	erts_print(to, to_arg, "Non Existing\n"); /* Should never happen */
	break;
    case P_RUNABLE:
	erts_print(to, to_arg, "Scheduled\n");
	break;
    case P_WAITING:
	erts_print(to, to_arg, "Waiting\n");
	break;
    case P_SUSPENDED:
	erts_print(to, to_arg, "Suspended\n");
	break;
    case P_RUNNING:
	erts_print(to, to_arg, "Running\n");
	running = 1;
	break;
    case P_EXITING:
	erts_print(to, to_arg, "Exiting\n");
	break;
    case P_GARBING:
	erts_print(to, to_arg, "Garbing\n");
	garbing = 1;
	running = 1;
	break;
    }

    /*
     * If the process is registered as a global process, display the
     * registered name
     */
    if (p->reg != NULL)
	erts_print(to, to_arg, "Name: %T\n", p->reg->name);

    /*
     * Display the initial function name
     */
    erts_print(to, to_arg, "Spawned as: %T:%T/%bpu\n",
	       p->initial[INITIAL_MOD],
	       p->initial[INITIAL_FUN],
	       p->initial[INITIAL_ARI]);
    
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

    erts_print(to, to_arg, "Started: %s", ctime((time_t*)&p->started.tv_sec));
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
    erts_print(to, to_arg, "Heap fragment data: %bpu\n", MBUF_SIZE(p));

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
    if (p->nlinks != NULL || p->monitors != NULL) {
	PrintMonitorContext context = {1,to}; 
	erts_print(to, to_arg,"Link list: [");
	erts_doforall_links(p->nlinks, &doit_print_link, &context);	
	erts_doforall_monitors(p->monitors, &doit_print_monitor, &context);
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
    erts_print(to, to_arg, "Reductions: %bpu\n", p->reds);

    erts_print(to, to_arg, "Stack+heap: %bpu\n", p->heap_sz);
    erts_print(to, to_arg, "OldHeap: %bpu\n",
               (OLD_HEAP(p) == NULL) ? 0 :
               (unsigned)(OLD_HEND(p) - OLD_HEAP(p)) );
    erts_print(to, to_arg, "Heap unused: %bpu\n", (p->hend - p->htop));
    erts_print(to, to_arg, "OldHeap unused: %bpu\n",
	       (OLD_HEAP(p) == NULL) ? 0 : (OLD_HEND(p) - OLD_HTOP(p)) );

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

    /*
     * Calculate and print totals.
     */
    for (i = 0; i < module_code_size(); i++) {
	if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
	    cur += module_code(i)->code_length;
	    if (module_code(i)->old_code_length != 0) {
		old += module_code(i)->old_code_length;
	    }
	}
    }
    erts_print(to, to_arg, "Current code: %d\n", cur);
    erts_print(to, to_arg, "Old code: %d\n", old);
    
    /*
     * Print one line per module.
     */

    for (i = 0; i < module_code_size(); i++) {
	if (!ERTS_IS_CRASH_DUMPING) {
	    /*
	     * Interactive dump; keep it brief.
	     */
	    if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
		erts_print(to, to_arg, "%T", make_atom(module_code(i)->module));
		cur += module_code(i)->code_length;
		erts_print(to, to_arg, " %d", module_code(i)->code_length );
		if (module_code(i)->old_code_length != 0) {
		    erts_print(to, to_arg, " (%d old)",
			       module_code(i)->old_code_length );
		    old += module_code(i)->old_code_length;
		}
		erts_print(to, to_arg, "\n");
	    }
	} else {
	    /*
	     * To crash dump; make it parseable.
	     */
	    if (module_code(i) != NULL &&
		((module_code(i)->code_length != 0) ||
		 (module_code(i)->old_code_length != 0))) {
		erts_print(to, to_arg, "=mod:");
		erts_print(to, to_arg, "%T", make_atom(module_code(i)->module));
		erts_print(to, to_arg, "\n");
		erts_print(to, to_arg, "Current size: %d\n",
			   module_code(i)->code_length);
		code = module_code(i)->code;
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

		if (module_code(i)->old_code_length != 0) {
		    erts_print(to, to_arg, "Old size: %d\n", module_code(i)->old_code_length);
		    code = module_code(i)->old_code;
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
	erl_exit(0, "");
    erts_free_read_env(mode);
#endif /* __WIN32__ */

    erts_printf("\n"
		"BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded\n"
		"       (v)ersion (k)ill (D)b-tables (d)istribution\n");

    while (1) {
	if ((i = sys_get_key(0)) <= 0)
	    erl_exit(0, "");
	switch (i) {
	case 'q':
	case 'a': 
	case '*': /* 
		   * The asterisk is an read error on windows, 
		   * where sys_get_key isn't that great in console mode.
		   * The usual reason for a read error is Ctrl-C. Treat this as
		   * 'a' to avoid infinite loop.
		   */
	    erl_exit(0, "");
	case 'A':		/* Halt generating crash dump */
	    erl_exit(1, "Crash dump requested by user");
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
	    erts_printf("Compiled on " ERLANG_COMPILE_DATE "\n");
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
	    p_slpq();
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
    ProcBin *bp;
    int i, printed;

    for (i=0; i < erts_max_processes; i++) {
	if ((rp = process_tab[i]) == NULL)
	    continue;
	if (!(bp = rp->off_heap.mso))
	    continue;
	printed = 0;
	while (bp) {
	    if (printed == 0) {
		erts_printf("Process %T holding binary data \n", rp->id);
		printed = 1;
	    }
	    erts_printf("0x%08lx orig_size: %ld, norefs = %ld\n",
		       (unsigned long)bp->val, 
		       (long)bp->val->orig_size, 
		       erts_smp_atomic_read(&bp->val->refc));

	    bp = bp->next;
	}
	if (printed == 1)
	    erts_printf("--------------------------------------\n");
    }
    /* db_bin_check() has to be rewritten for the AVL trees... */
    /*db_bin_check();*/ 
}

#endif

/* XXX THIS SHOULD BE IN SYSTEM !!!! */
void
erl_crash_dump_v(char *file, int line, char* fmt, va_list args)
{
    int fd;
    time_t now;
    size_t dumpnamebufsize = MAXPATHLEN;
    char dumpnamebuf[MAXPATHLEN];
    char* dumpname;

    if (ERTS_IS_CRASH_DUMPING)
	return;

    /* Wait for all threads to block. If all threads haven't blocked
     * after a minute, we go anyway and hope for the best...
     *
     * We do not release system again. We expect an exit() or abort() after
     * dump has been written.
     *
     * NOTE: We allow gc therefore it is important not to lock *any*
     *       process locks.
     */
    erts_smp_emergency_block_system(60000, ERTS_BS_FLG_ALLOW_GC);
    /* Either worked or not... */

    /* Allow us to pass certain places without locking... */
#ifdef ERTS_SMP
    erts_smp_atomic_inc(&erts_writing_erl_crash_dump);
#else
    erts_writing_erl_crash_dump = 1;
#endif

    erts_sys_prepare_crash_dump();

    if (erts_sys_getenv("ERL_CRASH_DUMP",&dumpnamebuf[0],&dumpnamebufsize) != 0)
	dumpname = "erl_crash.dump";
    else
	dumpname = &dumpnamebuf[0];

    fd = open(dumpname,O_WRONLY | O_CREAT | O_TRUNC,0640);
    if (fd < 0) 
	return; /* Can't create the crash dump, skip it */
    
    time(&now);
    erts_fdprintf(fd, "=erl_crash_dump:0.1\n%s", ctime(&now));

    if (file != NULL)
       erts_fdprintf(fd, "The error occurred in file %s, line %d\n", file, line);

    if (fmt != NULL && *fmt != '\0') {
	erts_fdprintf(fd, "Slogan: ");
	erts_vfdprintf(fd, fmt, args);
    }
    erts_fdprintf(fd, "System version: ");
    erts_print_system_version(fd, NULL, NULL);
    erts_fdprintf(fd, "%s\n", "Compiled: " ERLANG_COMPILE_DATE);
    erts_fdprintf(fd, "Taints: ");
    erts_print_nif_taints(fd, NULL);
    erts_fdprintf(fd, "Atoms: %d\n", atom_table_size());
    info(fd, NULL); /* General system info */
    if (process_tab != NULL)  /* XXX true at init */
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
    erts_fprintf(stderr,"\nCrash dump was written to: %s\n", dumpname);
}

void
erl_crash_dump(char* file, int line, char* fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  erl_crash_dump_v(file, line, fmt, args);
  va_end(args);
}
