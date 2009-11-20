/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
#include <ctype.h>
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_version.h"
#include "erl_db.h"
#include "beam_bp.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "dist.h"
#include "erl_mseg.h"
#include "erl_nmgc.h"
#include "erl_threads.h"
#include "erl_bif_timer.h"
#include "erl_instrument.h"
#include "erl_printf_term.h"
#include "erl_misc_utils.h"
#include "packet_parser.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_mode_switch_init() */
#include "hipe_signal.h"	/* for hipe_signal_init() */
#endif

#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif

/*
 * Note about VxWorks: All variables must be initialized by executable code,
 * not by an initializer. Otherwise a new instance of the emulator will
 * inherit previous values.
 */

extern void erl_crash_dump_v(char *, int, char *, va_list);
#ifdef __WIN32__
extern void ConNormalExit(void);
extern void ConWaitForExit(void);
#endif

#define ERTS_MIN_COMPAT_REL 7

#ifdef ERTS_SMP
erts_smp_atomic_t erts_writing_erl_crash_dump;
#else
volatile int erts_writing_erl_crash_dump = 0;
#endif
int erts_initialized = 0;

#if defined(USE_THREADS) && !defined(ERTS_SMP)
static erts_tid_t main_thread;
#endif

erts_cpu_info_t *erts_cpuinfo;

int erts_use_sender_punish;

/*
 * Configurable parameters.
 */

Uint display_items;	    /* no of items to display in traces etc */
Uint display_loads;		/* print info about loaded modules */
int H_MIN_SIZE;			/* The minimum heap grain */

Uint32 erts_debug_flags;	/* Debug flags. */
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
int count_instructions;
#endif
int erts_backtrace_depth;	/* How many functions to show in a backtrace
				 * in error codes.
				 */

int erts_async_max_threads;  /* number of threads for async support */
int erts_async_thread_suggested_stack_size;
erts_smp_atomic_t erts_max_gen_gcs;

Eterm erts_error_logger_warnings; /* What to map warning logs to, am_error, 
				     am_info or am_warning, am_error is 
				     the default for BC */

int erts_compat_rel;

static int use_multi_run_queue;
static int no_schedulers;
static int no_schedulers_online;

#ifdef DEBUG
Uint32 verbose;             /* See erl_debug.h for information about verbose */
#endif

int erts_disable_tolerant_timeofday; /* Time correction can be disabled it is
				      * not and/or it is too slow.
				      */

int erts_modified_timing_level;

int erts_no_crash_dump = 0;	/* Use -d to suppress crash dump. */

/*
 * Other global variables.
 */

ErtsModifiedTimings erts_modified_timings[] = {
    /* 0 */	{make_small(0), CONTEXT_REDS, INPUT_REDUCTIONS},
    /* 1 */	{make_small(0), 2*CONTEXT_REDS, 2*INPUT_REDUCTIONS},
    /* 2 */	{make_small(0), CONTEXT_REDS/2, INPUT_REDUCTIONS/2},
    /* 3 */	{make_small(0), 3*CONTEXT_REDS, 3*INPUT_REDUCTIONS},
    /* 4 */	{make_small(0), CONTEXT_REDS/3, 3*INPUT_REDUCTIONS},
    /* 5 */	{make_small(0), 4*CONTEXT_REDS, INPUT_REDUCTIONS/2},
    /* 6 */	{make_small(1), CONTEXT_REDS/4, 2*INPUT_REDUCTIONS},
    /* 7 */	{make_small(1), 5*CONTEXT_REDS, INPUT_REDUCTIONS/3},
    /* 8 */	{make_small(10), CONTEXT_REDS/5, 3*INPUT_REDUCTIONS},
    /* 9 */	{make_small(10), 6*CONTEXT_REDS, INPUT_REDUCTIONS/4}
};

#define ERTS_MODIFIED_TIMING_LEVELS \
  (sizeof(erts_modified_timings)/sizeof(ErtsModifiedTimings))

Export *erts_delay_trap = NULL;

int erts_use_r9_pids_ports;

#ifdef HYBRID
Eterm *global_heap;
Eterm *global_hend;
Eterm *global_htop;
Eterm *global_saved_htop;
Eterm *global_old_heap;
Eterm *global_old_hend;
ErlOffHeap erts_global_offheap;
Uint   global_heap_sz = SH_DEFAULT_SIZE;

#ifndef INCREMENTAL
Eterm *global_high_water;
Eterm *global_old_htop;
#endif

Uint16 global_gen_gcs;
Uint16 global_max_gen_gcs;
Uint   global_gc_flags;

Uint   global_heap_min_sz = SH_DEFAULT_SIZE;
#endif

int ignore_break;
int replace_intr;

static ERTS_INLINE int
has_prefix(const char *prefix, const char *string)
{
    int i;
    for (i = 0; prefix[i]; i++)
	if (prefix[i] != string[i])
	    return 0;
    return 1;
}

static char*
progname(char *fullname) 
{
    int i;
    
    i = strlen(fullname);
    while (i >= 0) {
	if ((fullname[i] != '/') && (fullname[i] != '\\')) 
	    i--;
	else 
	    break;
    }
    return fullname+i+1;
}

static int
this_rel_num(void)
{
    static int this_rel = -1;

    if (this_rel < 1) {
	int i;
	char this_rel_str[] = ERLANG_OTP_RELEASE;
	    
	i = 0;
	while (this_rel_str[i] && !isdigit((int) this_rel_str[i]))
	    i++;
	this_rel = atoi(&this_rel_str[i]); 
	if (this_rel < 1)
	    erl_exit(-1, "Unexpected ERLANG_OTP_RELEASE format\n");
    }
    return this_rel;
}

/*
 * Common error printout function, all error messages
 * that don't go to the error logger go through here.
 */

void erl_error(char *fmt, va_list args)
{
    erts_vfprintf(stderr, fmt, args);
}

static void early_init(int *argc, char **argv);

void
erts_short_init(void)
{
    early_init(NULL, NULL);
    erl_init();
    erts_initialized = 1;
}

void
erl_init(void)
{
    init_benchmarking();

#ifdef ERTS_SMP
    erts_system_block_init();
#endif

    erts_init_monitors();
    erts_init_gc();
    init_time();
    erts_init_process();
    erts_init_scheduling(use_multi_run_queue,
			 no_schedulers,
			 no_schedulers_online);

    H_MIN_SIZE = erts_next_heap_size(H_MIN_SIZE, 0);

    erts_init_trace();
    erts_init_binary();
    erts_init_bits();
    erts_init_fun_table();
    init_atom_table();
    init_export_table();
    init_module_table();
    init_register_table();
    init_message();
    erts_bif_info_init();
    erts_ddll_init();
    init_emulator();
    erts_bp_init();
    init_db(); /* Must be after init_emulator */
    erts_bif_timer_init();
    erts_init_node_tables();
    init_dist();
    erl_drv_thr_init();
    init_io();
    init_copy();
    init_load();
    erts_init_bif();
    erts_init_bif_chksum();
    erts_init_bif_re();
    erts_init_unicode(); /* after RE to get access to PCRE unicode */
    erts_delay_trap = erts_export_put(am_erlang, am_delay_trap, 2);
    erts_late_init_process();
#if HAVE_ERTS_MSEG
    erts_mseg_late_init(); /* Must be after timer (init_time()) and thread
			      initializations */
#endif
#ifdef HIPE
    hipe_mode_switch_init(); /* Must be after init_load/beam_catches/init */
#endif
#ifdef _OSE_
    erl_sys_init_final();
#endif
    packet_parser_init();
}

static void
init_shared_memory(int argc, char **argv)
{
#ifdef HYBRID
    int arg_size = 0;

    global_heap_sz = erts_next_heap_size(global_heap_sz,0);

    /* Make sure arguments will fit on the heap, no one else will check! */
    while (argc--)
        arg_size += 2 + strlen(argv[argc]);
    if (global_heap_sz < arg_size)
        global_heap_sz = erts_next_heap_size(arg_size,1);

#ifndef INCREMENTAL
    global_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
					    sizeof(Eterm) * global_heap_sz);
    global_hend = global_heap + global_heap_sz;
    global_htop = global_heap;
    global_high_water = global_heap;
    global_old_hend = global_old_htop = global_old_heap = NULL;
#endif

    global_gen_gcs = 0;
    global_max_gen_gcs = erts_smp_atomic_read(&erts_max_gen_gcs);
    global_gc_flags = erts_default_process_flags;

    erts_global_offheap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    erts_global_offheap.funs = NULL;
#endif
    erts_global_offheap.overhead = 0;
#endif

#ifdef INCREMENTAL
    erts_init_incgc();
#endif
}


/*
 * Create the very first process.
 */

void
erts_first_process(Eterm modname, void* code, unsigned size, int argc, char** argv)
{
    int i;
    Eterm args;
    Eterm pid;
    Eterm* hp;
    Process parent;
    Process* p;
    ErlSpawnOpts so;
    
    if (erts_find_function(modname, am_start, 1) == NULL) {
	char sbuf[256];
	Atom* ap;

	ap = atom_tab(atom_val(modname));
	memcpy(sbuf, ap->name, ap->len);
	sbuf[ap->len] = '\0';
	erl_exit(5, "No function %s:start/1\n", sbuf);
    }

    /*
     * We need a dummy parent process to be able to call erl_create_process().
     */
    erts_init_empty_process(&parent);
    hp = HAlloc(&parent, argc*2 + 4);
    args = NIL;
    for (i = argc-1; i >= 0; i--) {
	int len = sys_strlen(argv[i]);
	args = CONS(hp, new_binary(&parent, (byte*)argv[i], len), args);
	hp += 2;
    }
    args = CONS(hp, new_binary(&parent, code, size), args);
    hp += 2;
    args = CONS(hp, args, NIL);

    so.flags = 0;
    pid = erl_create_process(&parent, modname, am_start, args, &so);
    p = process_tab[internal_pid_index(pid)];
    p->group_leader = pid;

    erts_cleanup_empty_process(&parent);
}

/*
 * XXX Old way of starting. Hopefully soon obsolete.
 */

static void
erl_first_process_otp(char* modname, void* code, unsigned size, int argc, char** argv)
{
    int i;
    Eterm start_mod;
    Eterm args;
    Eterm* hp;
    Process parent;
    ErlSpawnOpts so;
    Eterm env;
    
    start_mod = am_atom_put(modname, sys_strlen(modname));
    if (erts_find_function(start_mod, am_start, 2) == NULL) {
	erl_exit(5, "No function %s:start/2\n", modname);
    }

    /*
     * We need a dummy parent process to be able to call erl_create_process().
     */

    erts_init_empty_process(&parent);
    erts_smp_proc_lock(&parent, ERTS_PROC_LOCK_MAIN);
    hp = HAlloc(&parent, argc*2 + 4);
    args = NIL;
    for (i = argc-1; i >= 0; i--) {
	int len = sys_strlen(argv[i]);
	args = CONS(hp, new_binary(&parent, (byte*)argv[i], len), args);
	hp += 2;
    }
    env = new_binary(&parent, code, size);
    args = CONS(hp, args, NIL);
    hp += 2;
    args = CONS(hp, env, args);

    so.flags = 0;
    (void) erl_create_process(&parent, start_mod, am_start, args, &so);
    erts_smp_proc_unlock(&parent, ERTS_PROC_LOCK_MAIN);
    erts_cleanup_empty_process(&parent);
}

Eterm
erts_preloaded(Process* p)
{
    Eterm previous;
    int j;
    int need;
    Eterm mod;
    Eterm* hp;
    char* name;
    const Preload *preload = sys_preloaded();

    j = 0;
    while (preload[j].name != NULL) {
	j++;
    }
    previous = NIL;
    need = 2*j;
    hp = HAlloc(p, need);
    j = 0;
    while ((name = preload[j].name) != NULL)  {
	mod = am_atom_put(name, sys_strlen(name));
	previous = CONS(hp, mod, previous);
	hp += 2;
	j++;
    }
    return previous;
}


/* static variables that must not change (use same values at restart) */
static char* program;
static char* init = "init";
static char* boot = "boot";
static int    boot_argc;
static char** boot_argv;

static char *
get_arg(char* rest, char* next, int* ip)
{
    if (*rest == '\0') {
	if (next == NULL) {
	    erts_fprintf(stderr, "too few arguments\n");
	    erts_usage();
	}
	(*ip)++;
	return next;
    }
    return rest;
}

static void 
load_preloaded(void)
{
    int i;
    int res;
    Preload* preload_p;
    Eterm module_name;
    byte* code;
    char* name;
    int length;

    if ((preload_p = sys_preloaded()) == NULL) {
	return;
    }
    i = 0;
    while ((name = preload_p[i].name) != NULL) {
	length = preload_p[i].size;
	module_name = am_atom_put(name, sys_strlen(name));
	if ((code = sys_preload_begin(&preload_p[i])) == 0)
	    erl_exit(1, "Failed to find preloaded code for module %s\n", 
		     name);
	res = erts_load_module(NULL, 0, NIL, &module_name, code, length);
	sys_preload_end(&preload_p[i]);
	if (res < 0)
	    erl_exit(1,"Failed loading preloaded module %s\n", name);
	i++;
    }
}

/* be helpful (or maybe downright rude:-) */
void erts_usage(void)
{
    erts_fprintf(stderr, "Usage: %s [flags] [ -- [init_args] ]\n", progname(program));
    erts_fprintf(stderr, "The flags are:\n\n");

    /*    erts_fprintf(stderr, "-# number  set the number of items to be used in traces etc\n"); */

    erts_fprintf(stderr, "-a size    suggested stack size in kilo words for threads\n");
    erts_fprintf(stderr, "           in the async-thread pool, valid range is [%d-%d]\n",
		 ERTS_ASYNC_THREAD_MIN_STACK_SIZE,
		 ERTS_ASYNC_THREAD_MAX_STACK_SIZE);
    erts_fprintf(stderr, "-A number  set number of threads in async thread pool,\n");
    erts_fprintf(stderr, "           valid range is [0-%d]\n",
		 ERTS_MAX_NO_OF_ASYNC_THREADS);

    erts_fprintf(stderr, "-B[c|d|i]  c to have Ctrl-c interrupt the Erlang shell,\n");
    erts_fprintf(stderr, "           d (or no extra option) to disable the break\n");
    erts_fprintf(stderr, "           handler, i to ignore break signals\n");

    /*    erts_fprintf(stderr, "-b func    set the boot function (default boot)\n"); */

    erts_fprintf(stderr, "-c         disable continuous date/time correction with\n");
    erts_fprintf(stderr, "           respect to uptime\n");

    erts_fprintf(stderr, "-d         don't write a crash dump for internally detected errors\n");
    erts_fprintf(stderr, "           (halt(String) will still produce a crash dump)\n");

    erts_fprintf(stderr, "-h number  set minimum heap size in words (default %d)\n",
	       H_DEFAULT_SIZE);

    /*    erts_fprintf(stderr, "-i module  set the boot module (default init)\n"); */

    erts_fprintf(stderr, "-K boolean enable or disable kernel poll\n");

    erts_fprintf(stderr, "-l         turn on auto load tracing\n");

    erts_fprintf(stderr, "-M<X> <Y>  memory allocator switches,\n");
    erts_fprintf(stderr, "           see the erts_alloc(3) documentation for more info.\n");

    erts_fprintf(stderr, "-P number  set maximum number of processes on this node,\n");
    erts_fprintf(stderr, "           valid range is [%d-%d]\n",
	       ERTS_MIN_PROCESSES, ERTS_MAX_PROCESSES);
    erts_fprintf(stderr, "-R number  set compatibility release number,\n");
    erts_fprintf(stderr, "           valid range [%d-%d]\n",
	       ERTS_MIN_COMPAT_REL, this_rel_num());

    erts_fprintf(stderr, "-r         force ets memory block to be moved on realloc\n");
    erts_fprintf(stderr, "-sbt type  set scheduler bind type, valid types are:\n");
    erts_fprintf(stderr, "           u|ns|ts|ps|s|nnts|nnps|tnnps|db\n");
    erts_fprintf(stderr, "-sct cput  set cpu topology,\n");
    erts_fprintf(stderr, "           see the erl(1) documentation for more info.\n");
    erts_fprintf(stderr, "-sss size  suggested stack size in kilo words for scheduler threads,\n");
    erts_fprintf(stderr, "           valid range is [%d-%d]\n",
		 ERTS_SCHED_THREAD_MIN_STACK_SIZE,
		 ERTS_SCHED_THREAD_MAX_STACK_SIZE);
    erts_fprintf(stderr, "-S n1:n2   set number of schedulers (n1), and number of\n");
    erts_fprintf(stderr, "           schedulers online (n2), valid range for both\n");
    erts_fprintf(stderr, "           numbers are [1-%d]\n",
		 ERTS_MAX_NO_OF_SCHEDULERS);
    erts_fprintf(stderr, "-T number  set modified timing level,\n");
    erts_fprintf(stderr, "           valid range is [0-%d]\n",
		 ERTS_MODIFIED_TIMING_LEVELS-1);
    erts_fprintf(stderr, "-V         print Erlang version\n");

    erts_fprintf(stderr, "-v         turn on chatty mode (GCs will be reported etc)\n");

    erts_fprintf(stderr, "-W<i|w>    set error logger warnings mapping,\n");
    erts_fprintf(stderr, "           see error_logger documentation for details\n");

    erts_fprintf(stderr, "\n");
    erts_fprintf(stderr, "Note that if the emulator is started with erlexec (typically\n");
    erts_fprintf(stderr, "from the erl script), these flags should be specified with +.\n");
    erts_fprintf(stderr, "\n\n");
    erl_exit(-1, "");
}

static void
early_init(int *argc, char **argv) /*
				   * Only put things here which are
				   * really important initialize
				   * early!
				   */
{
    ErtsAllocInitOpts alloc_opts = ERTS_ALLOC_INIT_DEF_OPTS_INITER;
    int ncpu;
    int ncpuonln;
    int ncpuavail;
    int schdlrs;
    int schdlrs_onln;
    use_multi_run_queue = 1;
    erts_printf_eterm_func = erts_printf_term;
    erts_disable_tolerant_timeofday = 0;
    display_items = 200;
    display_loads = 0;
    erts_backtrace_depth = DEFAULT_BACKTRACE_SIZE;
    erts_async_max_threads = 0;
    erts_async_thread_suggested_stack_size = ERTS_ASYNC_THREAD_MIN_STACK_SIZE;
    H_MIN_SIZE = H_DEFAULT_SIZE;

    erts_initialized = 0;

    erts_use_sender_punish = 1;

    erts_cpuinfo = erts_cpu_info_create();

#ifdef ERTS_SMP
    ncpu = erts_get_cpu_configured(erts_cpuinfo);
    ncpuonln = erts_get_cpu_online(erts_cpuinfo);
    ncpuavail = erts_get_cpu_available(erts_cpuinfo);
#else
    ncpu = 1;
    ncpuonln = 1;
    ncpuavail = 1;
#endif

    ignore_break = 0;
    replace_intr = 0;
    program = argv[0];

    erts_modified_timing_level = -1;

    erts_compat_rel = this_rel_num();

    erts_use_r9_pids_ports = 0;

    erts_sys_pre_init();

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init();
#endif
#ifdef ERTS_SMP
    erts_smp_atomic_init(&erts_writing_erl_crash_dump, 0L);
#else
    erts_writing_erl_crash_dump = 0;
#endif

    erts_smp_atomic_init(&erts_max_gen_gcs, (long)((Uint16) -1));

    erts_pre_init_process();
#if defined(USE_THREADS) && !defined(ERTS_SMP)
    main_thread = erts_thr_self();
#endif

    /*
     * We need to know the number of schedulers to use before we
     * can initialize the allocators.
     */
    no_schedulers = (Uint) (ncpu > 0 ? ncpu : 1);
    no_schedulers_online = (ncpuavail > 0
			    ? ncpuavail
			    : (ncpuonln > 0 ? ncpuonln : no_schedulers));

    schdlrs = no_schedulers;
    schdlrs_onln = no_schedulers_online;

    if (argc && argv) {
	int i = 1;
	while (i < *argc) {
	    if (strcmp(argv[i], "--") == 0) { /* end of emulator options */
		i++;
		break;
	    }
	    if (argv[i][0] == '-') {
		switch (argv[i][1]) {
		case 'S' : {
		    int tot, onln;
		    char *arg = get_arg(argv[i]+2, argv[i+1], &i);
		    switch (sscanf(arg, "%d:%d", &tot, &onln)) {
		    case 0:
			switch (sscanf(arg, ":%d", &onln)) {
			case 1:
			    tot = no_schedulers;
			    goto chk_S;
			default:
			    goto bad_S;
			}
		    case 1:
			onln = tot < schdlrs_onln ? tot : schdlrs_onln;
		    case 2:
		    chk_S:
			if (tot > 0)
			    schdlrs = tot;
			else
			    schdlrs = no_schedulers + tot;
			if (onln > 0)
			    schdlrs_onln = onln;
			else
			    schdlrs_onln = no_schedulers_online + onln;
			if (schdlrs < 1 || ERTS_MAX_NO_OF_SCHEDULERS < schdlrs) {
			    erts_fprintf(stderr,
					 "bad amount of schedulers %d\n",
					 tot);
			    erts_usage();
			}
			if (schdlrs_onln < 1 || schdlrs < schdlrs_onln) {
			    erts_fprintf(stderr,
					 "bad amount of schedulers online %d "
					 "(total amount of schedulers %d)\n",
					 schdlrs_onln, schdlrs);
			    erts_usage();
			}
			break;
		    default:
		    bad_S:
			erts_fprintf(stderr,
				     "bad amount of schedulers %s\n",
				     arg);
			erts_usage();
			break;
		    }

		    VERBOSE(DEBUG_SYSTEM,
			    ("using %d:%d scheduler(s)\n", tot, onln));
		    break;
		}
		default:
		    break;
		}
	    }
	    i++;
	}
    }

#ifdef ERTS_SMP
    no_schedulers = schdlrs;
    no_schedulers_online = schdlrs_onln;

    erts_no_schedulers = (Uint) no_schedulers;
#endif

    erts_alloc_init(argc, argv, &alloc_opts); /* Handles (and removes)
						 -M flags. */

    erts_early_init_scheduling(); /* Require allocators */
    erts_init_utils(); /* Require allocators */

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_late_init();
#endif

#if defined(HIPE)
    hipe_signal_init();	/* must be done very early */
#endif
    erl_sys_init();

    erl_sys_args(argc, argv);

    erts_ets_realloc_always_moves = 0;

}

#ifndef ERTS_SMP
static void set_main_stack_size(void)
{
    if (erts_sched_thread_suggested_stack_size > 0) {
# if HAVE_DECL_GETRLIMIT && HAVE_DECL_SETRLIMIT && HAVE_DECL_RLIMIT_STACK
	struct rlimit rl;
	int bytes = erts_sched_thread_suggested_stack_size * sizeof(Uint) * 1024;
	if (getrlimit(RLIMIT_STACK, &rl) != 0 ||
	    (rl.rlim_cur = bytes, setrlimit(RLIMIT_STACK, &rl) != 0)) {
	    erts_fprintf(stderr, "failed to set stack size for scheduler "
				 "thread to %d bytes\n", bytes);
	    erts_usage();
	}	    
# else
	erts_fprintf(stderr, "no OS support for dynamic stack size limit\n");
	erts_usage();    
# endif
    }
}
#endif

void
erl_start(int argc, char **argv)
{
    int i = 1;
    char* arg=NULL;
    char* Parg = NULL;
    int have_break_handler = 1;
    char envbuf[21]; /* enough for any 64-bit integer */
    size_t envbufsz;
    int async_max_threads = erts_async_max_threads;

    early_init(&argc, argv);

    envbufsz = sizeof(envbuf);
    if (erts_sys_getenv(ERL_MAX_ETS_TABLES_ENV, envbuf, &envbufsz) == 0)
	user_requested_db_max_tabs = atoi(envbuf);
    else
	user_requested_db_max_tabs = 0;

    envbufsz = sizeof(envbuf);
    if (erts_sys_getenv("ERL_FULLSWEEP_AFTER", envbuf, &envbufsz) == 0) {
	Uint16 max_gen_gcs = atoi(envbuf);
	erts_smp_atomic_set(&erts_max_gen_gcs, (long) max_gen_gcs);
    }

    envbufsz = sizeof(envbuf);
    if (erts_sys_getenv("ERL_THREAD_POOL_SIZE", envbuf, &envbufsz) == 0) {
	async_max_threads = atoi(envbuf);
    }
    

#ifdef DEBUG
    verbose = DEBUG_DEFAULT;
#endif

    erts_error_logger_warnings = am_error;

    while (i < argc) {
	if (argv[i][0] != '-') {
	    erts_usage();
	}
	if (strcmp(argv[i], "--") == 0) { /* end of emulator options */
	    i++;
	    break;
	}
	switch (argv[i][1]) {

	    /*
	     * NOTE: -M flags are handled (and removed from argv) by
	     * erts_alloc_init(). 
	     *
	     * The -d, -m, -S, -t, and -T flags was removed in
	     * Erlang 5.3/OTP R9C.
	     *
	     * -S, and -T has been reused in Erlang 5.5/OTP R11B.
	     *
	     * -d has been reused in a patch R12B-4.
	     */

	case '#' :
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if ((display_items = atoi(arg)) == 0) {
		erts_fprintf(stderr, "bad display items%s\n", arg);
		erts_usage();
	    }
	    VERBOSE(DEBUG_SYSTEM,
                    ("using display items %d\n",display_items));
	    break;

	case 'l':
	    display_loads++;
	    break;

	case 'v':
#ifdef DEBUG
	    if (argv[i][2] == '\0') {
		verbose |= DEBUG_SYSTEM;
	    } else {
		char *ch;
		for (ch = argv[i]+2; *ch != '\0'; ch++) {
		    switch (*ch) {
		    case 's': verbose |= DEBUG_SYSTEM; break;
		    case 'g': verbose |= DEBUG_PRIVATE_GC; break;
		    case 'h': verbose |= DEBUG_HYBRID_GC; break;
		    case 'M': verbose |= DEBUG_MEMORY; break;
		    case 'a': verbose |= DEBUG_ALLOCATION; break;
		    case 't': verbose |= DEBUG_THREADS; break;
		    case 'p': verbose |= DEBUG_PROCESSES; break;
		    case 'm': verbose |= DEBUG_MESSAGES; break;
		    default : erts_fprintf(stderr,"Unknown verbose option: %c\n",*ch);
		    }
		}
	    }
            erts_printf("Verbose level: ");
            if (verbose & DEBUG_SYSTEM) erts_printf("SYSTEM ");
            if (verbose & DEBUG_PRIVATE_GC) erts_printf("PRIVATE_GC ");
            if (verbose & DEBUG_HYBRID_GC) erts_printf("HYBRID_GC ");
            if (verbose & DEBUG_MEMORY) erts_printf("PARANOID_MEMORY ");
	    if (verbose & DEBUG_ALLOCATION) erts_printf("ALLOCATION ");
	    if (verbose & DEBUG_THREADS) erts_printf("THREADS ");
	    if (verbose & DEBUG_PROCESSES) erts_printf("PROCESSES ");
	    if (verbose & DEBUG_MESSAGES) erts_printf("MESSAGES ");
            erts_printf("\n");
#else
	    erts_fprintf(stderr, "warning: -v (only in debug compiled code)\n");
#endif
	    break;
	case 'V' :
	    {
		char tmp[256];

		tmp[0] = tmp[1] = '\0';
#ifdef DEBUG
		strcat(tmp, ",DEBUG");
#endif
#ifdef ERTS_SMP
		strcat(tmp, ",SMP");
#endif
#ifdef USE_THREADS
		strcat(tmp, ",ASYNC_THREADS");
#endif
#ifdef HIPE
		strcat(tmp, ",HIPE");
#endif
#ifdef INCREMENTAL
		strcat(tmp, ",INCREMENTAL_GC");
#endif
#ifdef HYBRID
                strcat(tmp, ",HYBRID");
#endif
		erts_fprintf(stderr, "Erlang ");
		if (tmp[1]) {
		    erts_fprintf(stderr, "(%s) ", tmp+1);
		}
		erts_fprintf(stderr, "(" EMULATOR ") emulator version "
			   ERLANG_VERSION "\n");
		erl_exit(0, "");
	    }
	    break;

	case 'H':		/* undocumented */
	    fprintf(stderr, "The undocumented +H option has been removed (R10B-6).\n\n");
	    break;

	case 'h':
	    /* set default heap size */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if ((H_MIN_SIZE = atoi(arg)) <= 0) {
		erts_fprintf(stderr, "bad heap size %s\n", arg);
		erts_usage();
	    }
	    VERBOSE(DEBUG_SYSTEM,
                    ("using minimum heap size %d\n",H_MIN_SIZE));
	    break;

	case 'd':
	    /*
	     * Never produce crash dumps for internally detected
	     * errors; only produce a core dump. (Generation of
	     * crash dumps is destructive and makes it impossible
	     * to inspect the contents of process heaps in the
	     * core dump.)
	     */
	    erts_no_crash_dump = 1;
	    break;

	case 'e':
	    /* set maximum number of ets tables */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (( user_requested_db_max_tabs = atoi(arg) ) < 0) {
		erts_fprintf(stderr, "bad maximum number of ets tables %s\n", arg);
		erts_usage();
	    }
	    VERBOSE(DEBUG_SYSTEM,
                    ("using maximum number of ets tables %d\n",
                     user_requested_db_max_tabs));
	    break;

	case 'i':
	    /* define name of module for initial function */
	    init = get_arg(argv[i]+2, argv[i+1], &i);
	    break;

	case 'b':
	    /* define name of initial function */
	    boot = get_arg(argv[i]+2, argv[i+1], &i);
	    break;

	case 'B':
	  if (argv[i][2] == 'i')          /* +Bi */
	    ignore_break = 1;
	  else if (argv[i][2] == 'c')     /* +Bc */
	    replace_intr = 1;
	  else if (argv[i][2] == 'd')     /* +Bd */
	    have_break_handler = 0;
	  else if (argv[i+1][0] == 'i') { /* +B i */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    ignore_break = 1;
	  }
	  else if (argv[i+1][0] == 'c') { /* +B c */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    replace_intr = 1;
	  }
	  else if (argv[i+1][0] == 'd') { /* +B d */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    have_break_handler = 0;
	  }
	  else			          /* +B */
	    have_break_handler = 0;
	  break;

	case 'K':
	    /* If kernel poll support is present,
	       erl_sys_args() will remove the K parameter
	       and value */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    erts_fprintf(stderr,
		       "kernel-poll not supported; \"K\" parameter ignored\n",
		       arg);
	    break;

	case 'P':
	    /* set maximum number of processes */
	    Parg = get_arg(argv[i]+2, argv[i+1], &i);
	    erts_max_processes = atoi(Parg);
	    /* Check of result is delayed until later. This is because +R
	       may be given after +P. */
	    break;

	case 'S' : /* Was handled in early_init() just read past it */
	    (void) get_arg(argv[i]+2, argv[i+1], &i);
	    break;

	case 's' : {
	    char *estr;
	    int res;
	    char *sub_param = argv[i]+2;
	    if (has_prefix("bt", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		res = erts_init_scheduler_bind_type(arg);
		if (res != ERTS_INIT_SCHED_BIND_TYPE_SUCCESS) {
		    switch (res) {
		    case ERTS_INIT_SCHED_BIND_TYPE_NOT_SUPPORTED:
			estr = "not supported";
			break;
		    case ERTS_INIT_SCHED_BIND_TYPE_ERROR_NO_CPU_TOPOLOGY:
			estr = "no cpu topology available";
			break;
		    case ERTS_INIT_SCHED_BIND_TYPE_ERROR_NO_BAD_TYPE:
			estr = "invalid type";
			break;
		    default:
			estr = "undefined error";
			break;
		    }
		    erts_fprintf(stderr,
				 "setting scheduler bind type '%s' failed: %s\n",
				 arg,
				 estr);
		    erts_usage();
		}
	    }
	    else if (has_prefix("ct", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		res = erts_init_cpu_topology(arg);
		if (res != ERTS_INIT_CPU_TOPOLOGY_OK) {
		    switch (res) {
		    case ERTS_INIT_CPU_TOPOLOGY_INVALID_ID:
			estr = "invalid identifier";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE:
			estr = "invalid identifier range";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY:
			estr = "invalid hierarchy";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_TYPE:
			estr = "invalid identifier type";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_INVALID_NODES:
			estr = "invalid nodes declaration";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_MISSING_LID:
			estr = "missing logical identifier";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_NOT_UNIQUE_LIDS:
			estr = "not unique logical identifiers";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_NOT_UNIQUE_ENTITIES:
			estr = "not unique entities";
			break;
		    case ERTS_INIT_CPU_TOPOLOGY_MISSING:
			estr = "missing cpu topology";
			break;
		    default:
			estr = "undefined error";
			break;
		    }
		    erts_fprintf(stderr,
				 "bad cpu topology '%s': %s\n",
				 arg,
				 estr);
		    erts_usage();
		}
	    }
	    else if (sys_strcmp("mrq", sub_param) == 0)
		use_multi_run_queue = 1;
	    else if (sys_strcmp("srq", sub_param) == 0)
		use_multi_run_queue = 0;
	    else if (sys_strcmp("nsp", sub_param) == 0)
		erts_use_sender_punish = 0;
	    else if (has_prefix("ss", sub_param)) {
		/* suggested stack size (Kilo Words) for scheduler threads */
		arg = get_arg(sub_param+2, argv[i+1], &i);
		erts_sched_thread_suggested_stack_size = atoi(arg);

		if ((erts_sched_thread_suggested_stack_size
		     < ERTS_SCHED_THREAD_MIN_STACK_SIZE)
		    || (erts_sched_thread_suggested_stack_size >
			ERTS_SCHED_THREAD_MAX_STACK_SIZE)) {
		    erts_fprintf(stderr, "bad stack size for scheduler threads %s\n",
				 arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM,
			("suggested scheduler thread stack size %d kilo words\n",
			 erts_sched_thread_suggested_stack_size));
	    }
	    else {
		erts_fprintf(stderr, "bad scheduling option %s\n", argv[i]);
		erts_usage();
	    }
	    break;
	}
	case 'T' :
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    errno = 0;
	    erts_modified_timing_level = atoi(arg);
	    if ((erts_modified_timing_level == 0 && errno != 0)
		|| erts_modified_timing_level < 0
		|| erts_modified_timing_level >= ERTS_MODIFIED_TIMING_LEVELS) {
		erts_fprintf(stderr, "bad modified timing level %s\n", arg);
		erts_usage();
	    }
	    else {
		VERBOSE(DEBUG_SYSTEM,
			("using modified timing level %d\n",
			 erts_modified_timing_level));
	    }

	    break;

	case 'R': {
	    /* set compatibility release */

	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    erts_compat_rel = atoi(arg);

	    if (erts_compat_rel < ERTS_MIN_COMPAT_REL
		|| erts_compat_rel > this_rel_num()) {
		erts_fprintf(stderr, "bad compatibility release number %s\n", arg);
		erts_usage();
	    }

	    ASSERT(ERTS_MIN_COMPAT_REL >= 7);
	    switch (erts_compat_rel) {
	    case 7:
	    case 8:
	    case 9:
		erts_use_r9_pids_ports = 1;
	    default:
		break;
	    }

	    break;
	}

	case 'A':
	    /* set number of threads in thread pool */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (((async_max_threads = atoi(arg)) < 0) ||
		(async_max_threads > ERTS_MAX_NO_OF_ASYNC_THREADS)) {
		erts_fprintf(stderr, "bad number of async threads %s\n", arg);
		erts_usage();
	    }

	    VERBOSE(DEBUG_SYSTEM, ("using %d async-threads\n",
				   async_max_threads));
	    break;

	case 'a':
	    /* suggested stack size (Kilo Words) for threads in thread pool */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    erts_async_thread_suggested_stack_size = atoi(arg);
	    
	    if ((erts_async_thread_suggested_stack_size
		 < ERTS_ASYNC_THREAD_MIN_STACK_SIZE)
		|| (erts_async_thread_suggested_stack_size >
		    ERTS_ASYNC_THREAD_MAX_STACK_SIZE)) {
		erts_fprintf(stderr, "bad stack size for async threads %s\n",
			     arg);
		erts_usage();
	    }

	    VERBOSE(DEBUG_SYSTEM,
		    ("suggested async-thread stack size %d kilo words\n",
		     erts_async_thread_suggested_stack_size));
	    break;

 	case 'r':
	    erts_ets_realloc_always_moves = 1;
	    break;
	case 'n':   /* XXX obsolete */
	    break;
	case 'c':
	    if (argv[i][2] == 0) { /* -c: documented option */
		erts_disable_tolerant_timeofday = 1;
	    }
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
	    else if (argv[i][2] == 'i') { /* -ci: undcoumented option*/
		count_instructions = 1;
	    }
#endif
	    break;
	case 'W':
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    switch (arg[0]) {
	    case 'i':
		erts_error_logger_warnings = am_info;
		break;
	    case 'w':
		erts_error_logger_warnings = am_warning;
		break;
	    case 'e': /* The default */
		erts_error_logger_warnings = am_error;
	    default:
		erts_fprintf(stderr, "unrecognized warning_map option %s\n", arg);
		erts_usage();
	    }
	    break;

	default:
	    erts_fprintf(stderr, "%s unknown flag %s\n", argv[0], argv[i]);
	    erts_usage();
	}
	i++;
    }

#ifdef USE_THREADS
    erts_async_max_threads = async_max_threads;
#endif

    /* Delayed check of +P flag */
    if (erts_max_processes < ERTS_MIN_PROCESSES
	|| erts_max_processes > ERTS_MAX_PROCESSES
	|| (erts_use_r9_pids_ports
	    && erts_max_processes > ERTS_MAX_R9_PROCESSES)) {
	erts_fprintf(stderr, "bad number of processes %s\n", Parg);
	erts_usage();
    }

   /* Restart will not reinstall the break handler */
#ifdef __WIN32__
    if (ignore_break)
	erts_set_ignore_break();
    else if (replace_intr)
	erts_replace_intr();
    else
	init_break_handler();
#else
    if (ignore_break)
	erts_set_ignore_break();
    else if (have_break_handler)
	init_break_handler();
    if (replace_intr)
	erts_replace_intr();
#endif

    boot_argc = argc - i;  /* Number of arguments to init */
    boot_argv = &argv[i];

    erl_init();

    init_shared_memory(boot_argc, boot_argv);
    load_preloaded();

    erts_initialized = 1;

    erl_first_process_otp("otp_ring0", NULL, 0, boot_argc, boot_argv);

#ifdef ERTS_SMP
    erts_start_schedulers();
    /* Let system specific code decide what to do with the main thread... */

    erts_sys_main_thread(); /* May or may not return! */
#else
    set_main_stack_size();
    process_main();
#endif
}


#ifdef USE_THREADS

__decl_noreturn void erts_thr_fatal_error(int err, char *what)
{
    char *errstr = err ? strerror(err) : NULL;
    erts_fprintf(stderr,
		 "Failed to %s: %s%s(%d)\n",
		 what,
		 errstr ? errstr : "",
		 errstr ? " " : "",
		 err);
    abort();
}

#endif

static void
system_cleanup(int exit_code)
{
    /* No cleanup wanted if ...
     * 1. we are about to do an abnormal exit
     * 2. we haven't finished initializing, or
     * 3. another thread than the main thread is performing the exit
     *    (in threaded non smp case).
     */

    if (exit_code != 0
	|| !erts_initialized
#if defined(USE_THREADS) && !defined(ERTS_SMP)
	|| !erts_equal_tids(main_thread, erts_thr_self())
#endif
	)
	return;

#ifdef ERTS_SMP
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0);
#endif
    erts_smp_block_system(ERTS_BS_FLG_ALLOW_GC); /* We never release it... */
#endif

#ifdef HYBRID
    if (ma_src_stack) erts_free(ERTS_ALC_T_OBJECT_STACK,
                                (void *)ma_src_stack);
    if (ma_dst_stack) erts_free(ERTS_ALC_T_OBJECT_STACK,
                                (void *)ma_dst_stack);
    if (ma_offset_stack) erts_free(ERTS_ALC_T_OBJECT_STACK,
                                   (void *)ma_offset_stack);
    ma_src_stack = NULL;
    ma_dst_stack = NULL;
    ma_offset_stack = NULL;
    erts_cleanup_offheap(&erts_global_offheap);
#endif

#if defined(HYBRID) && !defined(INCREMENTAL)
    if (global_heap) {
	ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		       (void*) global_heap,
		       sizeof(Eterm) * global_heap_sz);
    }
    global_heap = NULL;
#endif

#ifdef INCREMENTAL
    erts_cleanup_incgc();
#endif

#if defined(USE_THREADS) && !defined(ERTS_SMP)
    exit_async();
#endif
#if HAVE_ERTS_MSEG
    erts_mseg_exit();
#endif

    /*
     * A lot more cleaning could/should have been done...
     */

}

/*
 * Common exit function, all exits from the system go through here.
 * n <= 0 -> normal exit with status n;
 * n = 127 -> Erlang crash dump produced, exit with status 1;
 * other positive n -> Erlang crash dump and core dump produced.
 */

__decl_noreturn void erl_exit0(char *file, int line, int n, char *fmt,...)
{
    unsigned int an;
    va_list args;

    va_start(args, fmt);

    save_statistics();

    system_cleanup(n);

    an = abs(n);

    if (erts_mtrace_enabled)
	erts_mtrace_exit((Uint32) an);

    /* Produce an Erlang core dump if error */
    if (n > 0 && erts_initialized &&
	(erts_no_crash_dump == 0 || n == ERTS_DUMP_EXIT)) {
	erl_crash_dump_v(file, line, fmt, args); 
    }

    /* need to reinitialize va_args thing */
    va_end(args);
    va_start(args, fmt);

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
    else ConNormalExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS) && !defined(_OSE_)
    sys_tty_reset();
#endif

    if (n == ERTS_INTR_EXIT)
	exit(0);
    else if (n == 127)
	ERTS_EXIT_AFTER_DUMP(1);
    else if (n > 0 || n == ERTS_ABORT_EXIT)
        abort();
    exit(an);
}

__decl_noreturn void erl_exit(int n, char *fmt,...)
{
    unsigned int an;
    va_list args;

    va_start(args, fmt);

    save_statistics();

    system_cleanup(n);

    an = abs(n);

    if (erts_mtrace_enabled)
	erts_mtrace_exit((Uint32) an);

    /* Produce an Erlang core dump if error */
    if (n > 0 && erts_initialized &&
	(erts_no_crash_dump == 0 || n == ERTS_DUMP_EXIT)) {
	erl_crash_dump_v((char*) NULL, 0, fmt, args);
    }

    /* need to reinitialize va_args thing */
    va_end(args);
    va_start(args, fmt);

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
    else ConNormalExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS) && !defined(_OSE_)
    sys_tty_reset();
#endif

    if (n == ERTS_INTR_EXIT)
	exit(0);
    else if (n == ERTS_DUMP_EXIT)
	ERTS_EXIT_AFTER_DUMP(1);
    else if (n > 0 || n == ERTS_ABORT_EXIT)
        abort();
    exit(an);
}

