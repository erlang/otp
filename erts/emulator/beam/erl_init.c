/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
#include "erl_threads.h"
#include "erl_hl_timer.h"
#include "erl_instrument.h"
#include "erl_printf_term.h"
#include "erl_misc_utils.h"
#include "packet_parser.h"
#include "erl_cpu_topology.h"
#include "erl_thr_progress.h"
#include "erl_thr_queue.h"
#include "erl_async.h"
#include "erl_ptab.h"
#include "erl_bif_unique.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_mode_switch_init() */
#include "hipe_signal.h"	/* for hipe_signal_init() */
#endif

#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif

#define ERTS_DEFAULT_NO_ASYNC_THREADS	10

/*
 * The variables below (prefixed with etp_) are for erts/etc/unix/etp-commands
 * only. Do not remove even though they aren't used elsewhere in the emulator!
 */
#ifdef ERTS_SMP
const int etp_smp_compiled = 1;
#else
const int etp_smp_compiled = 0;
#endif
#ifdef USE_THREADS
const int etp_thread_compiled = 1;
#else
const int etp_thread_compiled = 0;
#endif
const char etp_erts_version[] = ERLANG_VERSION;
const char etp_otp_release[] = ERLANG_OTP_RELEASE;
const char etp_compile_date[] = ERLANG_COMPILE_DATE;
const char etp_arch[] = ERLANG_ARCHITECTURE;
#ifdef ERTS_ENABLE_KERNEL_POLL
const int etp_kernel_poll_support = 1;
#else
const int etp_kernel_poll_support = 0;
#endif
#if defined(ARCH_64)
const int etp_arch_bits = 64;
#elif defined(ARCH_32)
const int etp_arch_bits = 32;
#else
# error "Not 64-bit, nor 32-bit arch"
#endif
#ifdef HIPE
const int etp_hipe = 1;
#else
const int etp_hipe = 0;
#endif
#ifdef DEBUG
const int etp_debug_compiled = 1;
#else
const int etp_debug_compiled = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
const int etp_lock_count = 1;
#else
const int etp_lock_count = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
const int etp_lock_check = 1;
#else
const int etp_lock_check = 0;
#endif
#ifdef WORDS_BIGENDIAN
const int etp_big_endian = 1;
#else
const int etp_big_endian = 0;
#endif
const Eterm etp_the_non_value = THE_NON_VALUE;

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

static void erl_init(int ncpu,
		     int proc_tab_sz,
		     int legacy_proc_tab,
		     int port_tab_sz,
		     int port_tab_sz_ignore_files,
		     int legacy_port_tab,
		     int time_correction,
		     ErtsTimeWarpMode time_warp_mode,
		     int node_tab_delete_delay,
		     ErtsDbSpinCount db_spin_count);

static erts_atomic_t exiting;

#ifdef ERTS_SMP
erts_smp_atomic32_t erts_writing_erl_crash_dump;
erts_tsd_key_t erts_is_crash_dumping_key;
#else
volatile int erts_writing_erl_crash_dump = 0;
#endif
int erts_initialized = 0;

#if defined(USE_THREADS) && !defined(ERTS_SMP)
static erts_tid_t main_thread;
#endif

int erts_use_sender_punish;

/*
 * Configurable parameters.
 */

Uint display_items;	    	/* no of items to display in traces etc */
int H_MIN_SIZE;			/* The minimum heap grain */
int BIN_VH_MIN_SIZE;		/* The minimum binary virtual*/

Uint32 erts_debug_flags;	/* Debug flags. */
int erts_backtrace_depth;	/* How many functions to show in a backtrace
				 * in error codes.
				 */

erts_smp_atomic32_t erts_max_gen_gcs;

Eterm erts_error_logger_warnings; /* What to map warning logs to, am_error, 
				     am_info or am_warning, am_error is 
				     the default for BC */

int erts_compat_rel;

static int no_schedulers;
static int no_schedulers_online;
#ifdef ERTS_DIRTY_SCHEDULERS
static int no_dirty_cpu_schedulers;
static int no_dirty_cpu_schedulers_online;
static int no_dirty_io_schedulers;
#endif

#ifdef DEBUG
Uint32 verbose;             /* See erl_debug.h for information about verbose */
#endif

int erts_atom_table_size = ATOM_LIMIT;	/* Maximum number of atoms */

int erts_pd_initial_size = 10;

int erts_modified_timing_level;

int erts_no_crash_dump = 0;	/* Use -d to suppress crash dump. */

int erts_no_line_info = 0;	/* -L: Don't load line information */

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

static ERTS_INLINE void
set_default_time_adj(int *time_correction_p, ErtsTimeWarpMode *time_warp_mode_p)
{
    *time_correction_p = 1;
    *time_warp_mode_p = ERTS_NO_TIME_WARP_MODE;
    if (!erts_check_time_adj_support(*time_correction_p,
				     *time_warp_mode_p)) {
	*time_correction_p = 0;
	ASSERT(erts_check_time_adj_support(*time_correction_p,
					   *time_warp_mode_p));
    }
}

/*
 * Common error printout function, all error messages
 * that don't go to the error logger go through here.
 */

void erl_error(char *fmt, va_list args)
{
    erts_vfprintf(stderr, fmt, args);
}

static int early_init(int *argc, char **argv);

void
erts_short_init(void)
{
    
    int ncpu;
    int time_correction;
    ErtsTimeWarpMode time_warp_mode;

    set_default_time_adj(&time_correction,
			 &time_warp_mode);
    ncpu = early_init(NULL, NULL);
    erl_init(ncpu,
	     ERTS_DEFAULT_MAX_PROCESSES,
	     0,
	     ERTS_DEFAULT_MAX_PORTS,
	     0,
	     0,
	     time_correction,
	     time_warp_mode,
	     ERTS_NODE_TAB_DELAY_GC_DEFAULT,
	     ERTS_DB_SPNCNT_NORMAL);
    erts_initialized = 1;
}

static void
erl_init(int ncpu,
	 int proc_tab_sz,
	 int legacy_proc_tab,
	 int port_tab_sz,
	 int port_tab_sz_ignore_files,
	 int legacy_port_tab,
	 int time_correction,
	 ErtsTimeWarpMode time_warp_mode,
	 int node_tab_delete_delay,
	 ErtsDbSpinCount db_spin_count)
{
    init_benchmarking();

    erts_bif_unique_init();
    erts_init_monitors();
    erts_init_time(time_correction, time_warp_mode);
    erts_init_sys_common_misc();
    erts_init_process(ncpu, proc_tab_sz, legacy_proc_tab);
    erts_init_scheduling(no_schedulers,
			 no_schedulers_online
#ifdef ERTS_DIRTY_SCHEDULERS
			 , no_dirty_cpu_schedulers,
			 no_dirty_cpu_schedulers_online,
			 no_dirty_io_schedulers
#endif
			 );
    erts_late_init_time_sup();
    erts_init_cpu_topology(); /* Must be after init_scheduling */
    erts_init_gc(); /* Must be after init_scheduling */
    erts_alloc_late_init();

    H_MIN_SIZE      = erts_next_heap_size(H_MIN_SIZE, 0);
    BIN_VH_MIN_SIZE = erts_next_heap_size(BIN_VH_MIN_SIZE, 0);

    erts_init_trace();
    erts_init_bits();
    erts_code_ix_init();
    erts_init_fun_table();
    init_atom_table();
    init_export_table();
    init_module_table();
    init_register_table();
    init_message();
    erts_bif_info_init();
    erts_ddll_init();
    init_emulator();
    erts_ptab_init(); /* Must be after init_emulator() */
    erts_init_binary(); /* Must be after init_emulator() */
    erts_bp_init();
    init_db(db_spin_count); /* Must be after init_emulator */
    erts_init_node_tables(node_tab_delete_delay);
    init_dist();
    erl_drv_thr_init();
    erts_init_async();
    erts_init_io(port_tab_sz, port_tab_sz_ignore_files, legacy_port_tab);
    init_load();
    erts_init_bif();
    erts_init_bif_chksum();
    erts_init_bif_binary();
    erts_init_bif_re();
    erts_init_unicode(); /* after RE to get access to PCRE unicode */
    erts_init_external();
    erts_init_map();
    erts_delay_trap = erts_export_put(am_erlang, am_delay_trap, 2);
    erts_late_init_process();
#if HAVE_ERTS_MSEG
    erts_mseg_late_init(); /* Must be after timer (erts_init_time()) and thread
			      initializations */
#endif
#ifdef HIPE
    hipe_mode_switch_init(); /* Must be after init_load/beam_catches/init */
#endif
    packet_parser_init();
    erl_nif_init();
}

static Eterm
erl_first_process_otp(char* modname, void* code, unsigned size, int argc, char** argv)
{
    int i;
    Eterm start_mod;
    Eterm args;
    Eterm res;
    Eterm* hp;
    Process parent;
    ErlSpawnOpts so;
    Eterm env;
    
    start_mod = erts_atom_put((byte *) modname, sys_strlen(modname), ERTS_ATOM_ENC_LATIN1, 1);
    if (erts_find_function(start_mod, am_start, 2,
			   erts_active_code_ix()) == NULL) {
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

    so.flags = SPO_SYSTEM_PROC;
    res = erl_create_process(&parent, start_mod, am_start, args, &so);
    erts_smp_proc_unlock(&parent, ERTS_PROC_LOCK_MAIN);
    erts_cleanup_empty_process(&parent);
    return res;
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
    Eterm res;
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
	module_name = erts_atom_put((byte *) name, sys_strlen(name), ERTS_ATOM_ENC_LATIN1, 1);
	if ((code = sys_preload_begin(&preload_p[i])) == 0)
	    erl_exit(1, "Failed to find preloaded code for module %s\n", 
		     name);
	res = erts_preload_module(NULL, 0, NIL, &module_name, code, length);
	sys_preload_end(&preload_p[i]);
	if (res != NIL)
	    erl_exit(1,"Failed loading preloaded module %s (%T)\n",
		     name, res);
	i++;
    }
}

/* be helpful (or maybe downright rude:-) */
void erts_usage(void)
{
    int this_rel = this_rel_num();
    erts_fprintf(stderr, "Usage: %s [flags] [ -- [init_args] ]\n", progname(program));
    erts_fprintf(stderr, "The flags are:\n\n");

    /*    erts_fprintf(stderr, "-# number  set the number of items to be used in traces etc\n"); */

    erts_fprintf(stderr, "-a size        suggested stack size in kilo words for threads\n");
    erts_fprintf(stderr, "               in the async-thread pool, valid range is [%d-%d]\n",
		 ERTS_ASYNC_THREAD_MIN_STACK_SIZE,
		 ERTS_ASYNC_THREAD_MAX_STACK_SIZE);
    erts_fprintf(stderr, "-A number      set number of threads in async thread pool,\n");
    erts_fprintf(stderr, "               valid range is [0-%d]\n",
		 ERTS_MAX_NO_OF_ASYNC_THREADS);

    erts_fprintf(stderr, "-B[c|d|i]      c to have Ctrl-c interrupt the Erlang shell,\n");
    erts_fprintf(stderr, "               d (or no extra option) to disable the break\n");
    erts_fprintf(stderr, "               handler, i to ignore break signals\n");

    /*    erts_fprintf(stderr, "-b func    set the boot function (default boot)\n"); */

    erts_fprintf(stderr, "-c bool        enable or disable time correction\n");
    erts_fprintf(stderr, "-C mode        set time warp mode; valid modes are:\n");
    erts_fprintf(stderr, "               no_time_warp|single_time_warp|multi_time_warp\n");
    erts_fprintf(stderr, "-d             don't write a crash dump for internally detected errors\n");
    erts_fprintf(stderr, "               (halt(String) will still produce a crash dump)\n");
    erts_fprintf(stderr, "-fn[u|a|l]     Control how filenames are interpreted\n");
    erts_fprintf(stderr, "-hms size      set minimum heap size in words (default %d)\n",
	       H_DEFAULT_SIZE);
    erts_fprintf(stderr, "-hmbs size     set minimum binary virtual heap size in words (default %d)\n",
	       VH_DEFAULT_SIZE);
    erts_fprintf(stderr, "-hpds size     initial process dictionary size (default %d)\n",
	       erts_pd_initial_size);

    /*    erts_fprintf(stderr, "-i module  set the boot module (default init)\n"); */

    erts_fprintf(stderr, "-K boolean     enable or disable kernel poll\n");
    erts_fprintf(stderr, "-n[s|a|d]      Control behavior of signals to ports\n");
    erts_fprintf(stderr, "               Note that this flag is deprecated!\n");
    erts_fprintf(stderr, "-M<X> <Y>      memory allocator switches,\n");
    erts_fprintf(stderr, "               see the erts_alloc(3) documentation for more info.\n");
    erts_fprintf(stderr, "-pc <set>      Control what characters are considered printable (default latin1)\n");
    erts_fprintf(stderr, "-P number      set maximum number of processes on this node,\n");
    erts_fprintf(stderr, "               valid range is [%d-%d]\n",
		 ERTS_MIN_PROCESSES, ERTS_MAX_PROCESSES);
    erts_fprintf(stderr, "-Q number      set maximum number of ports on this node,\n");
    erts_fprintf(stderr, "               valid range is [%d-%d]\n",
		 ERTS_MIN_PORTS, ERTS_MAX_PORTS);
    erts_fprintf(stderr, "-R number      set compatibility release number,\n");
    erts_fprintf(stderr, "               valid range [%d-%d]\n",
		 this_rel-2, this_rel);

    erts_fprintf(stderr, "-r             force ets memory block to be moved on realloc\n");
    erts_fprintf(stderr, "-rg amount     set reader groups limit\n");
    erts_fprintf(stderr, "-sbt type      set scheduler bind type, valid types are:\n");
    erts_fprintf(stderr, "-stbt type     u|ns|ts|ps|s|nnts|nnps|tnnps|db\n");
    erts_fprintf(stderr, "-sbwt val      set scheduler busy wait threshold, valid values are:\n");
    erts_fprintf(stderr, "               none|very_short|short|medium|long|very_long.\n");
    erts_fprintf(stderr, "-scl bool      enable/disable compaction of scheduler load,\n");
    erts_fprintf(stderr, "               see the erl(1) documentation for more info.\n");
    erts_fprintf(stderr, "-sct cput      set cpu topology,\n");
    erts_fprintf(stderr, "               see the erl(1) documentation for more info.\n");
    erts_fprintf(stderr, "-secio bool    enable/disable eager check I/O scheduling,\n");
    erts_fprintf(stderr, "               see the erl(1) documentation for more info.\n");
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT
    erts_fprintf(stderr, "-sub bool      enable/disable scheduler utilization balancing,\n");
#else
    erts_fprintf(stderr, "-sub false     disable scheduler utilization balancing,\n");
#endif
    erts_fprintf(stderr, "               see the erl(1) documentation for more info.\n");
    erts_fprintf(stderr, "-sws val       set scheduler wakeup strategy, valid values are:\n");
    erts_fprintf(stderr, "               default|legacy.\n");
    erts_fprintf(stderr, "-swct val      set scheduler wake cleanup threshold, valid values are:\n");
    erts_fprintf(stderr, "               very_lazy|lazy|medium|eager|very_eager.\n");
    erts_fprintf(stderr, "-swt val       set scheduler wakeup threshold, valid values are:\n");
    erts_fprintf(stderr, "               very_low|low|medium|high|very_high.\n");
    erts_fprintf(stderr, "-sss size      suggested stack size in kilo words for scheduler threads,\n");
    erts_fprintf(stderr, "               valid range is [%d-%d]\n",
		 ERTS_SCHED_THREAD_MIN_STACK_SIZE,
		 ERTS_SCHED_THREAD_MAX_STACK_SIZE);
    erts_fprintf(stderr, "-spp Bool      set port parallelism scheduling hint\n");
    erts_fprintf(stderr, "-S n1:n2       set number of schedulers (n1), and number of\n");
    erts_fprintf(stderr, "               schedulers online (n2), maximum for both\n");
    erts_fprintf(stderr, "               numbers is %d\n",
		 ERTS_MAX_NO_OF_SCHEDULERS);
    erts_fprintf(stderr, "-SP p1:p2      specify schedulers (p1) and schedulers online (p2)\n");
    erts_fprintf(stderr, "               as percentages of logical processors configured and logical\n");
    erts_fprintf(stderr, "               processors available, respectively\n");
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_fprintf(stderr, "-SDcpu n1:n2   set number of dirty CPU schedulers (n1), and number of\n");
    erts_fprintf(stderr, "               dirty CPU schedulers online (n2), valid range for both\n");
    erts_fprintf(stderr, "               numbers is [1-%d], and n2 must be less than or equal to n1\n",
		 ERTS_MAX_NO_OF_DIRTY_CPU_SCHEDULERS);
    erts_fprintf(stderr, "-SDPcpu p1:p2  specify dirty CPU schedulers (p1) and dirty CPU schedulers\n");
    erts_fprintf(stderr, "               online (p2) as percentages of logical processors configured\n");
    erts_fprintf(stderr, "               and logical processors available, respectively\n");
    erts_fprintf(stderr, "-SDio n        set number of dirty I/O schedulers, valid range is [0-%d]\n",
		 ERTS_MAX_NO_OF_DIRTY_IO_SCHEDULERS);
#endif
    erts_fprintf(stderr, "-t size        set the maximum number of atoms the emulator can handle\n");
    erts_fprintf(stderr, "               valid range is [%d-%d]\n",
		 MIN_ATOM_TABLE_SIZE, MAX_ATOM_TABLE_SIZE);
    erts_fprintf(stderr, "-T number      set modified timing level, valid range is [0-%d]\n",
		 ERTS_MODIFIED_TIMING_LEVELS-1);
    erts_fprintf(stderr, "-V             print Erlang version\n");

    erts_fprintf(stderr, "-v             turn on chatty mode (GCs will be reported etc)\n");

    erts_fprintf(stderr, "-W<i|w|e>      set error logger warnings mapping,\n");
    erts_fprintf(stderr, "               see error_logger documentation for details\n");
    erts_fprintf(stderr, "-zdbbl size    set the distribution buffer busy limit in kilobytes\n");
    erts_fprintf(stderr, "               valid range is [1-%d]\n", INT_MAX/1024);
    erts_fprintf(stderr, "-zdntgc time   set delayed node table gc in seconds\n");
    erts_fprintf(stderr, "               valid values are infinity or intergers in the range [0-%d]\n",
		 ERTS_NODE_TAB_DELAY_GC_MAX);
#if 0
    erts_fprintf(stderr, "-zebwt  val    set ets busy wait threshold, valid values are:\n");
    erts_fprintf(stderr, "               none|very_short|short|medium|long|very_long|extremely_long\n");
#endif
    erts_fprintf(stderr, "\n");
    erts_fprintf(stderr, "Note that if the emulator is started with erlexec (typically\n");
    erts_fprintf(stderr, "from the erl script), these flags should be specified with +.\n");
    erts_fprintf(stderr, "\n\n");
    erl_exit(-1, "");
}

#ifdef USE_THREADS
/*
 * allocators for thread lib
 */

static void *ethr_std_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_STD, (Uint) size);
}
static void *ethr_std_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_STD, ptr, (Uint) size);
}
static void ethr_std_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_STD, ptr);
}
static void *ethr_sl_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_SL, (Uint) size);
}
static void *ethr_sl_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_SL, ptr, (Uint) size);
}
static void ethr_sl_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_SL, ptr);
}
static void *ethr_ll_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_LL, (Uint) size);
}
static void *ethr_ll_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_LL, ptr, (Uint) size);
}
static void ethr_ll_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_LL, ptr);
}

#endif

static int
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
    int schdlrs_percentage = 100;
    int schdlrs_onln_percentage = 100;
    int max_main_threads;
#ifdef ERTS_DIRTY_SCHEDULERS
    int dirty_cpu_scheds;
    int dirty_cpu_scheds_online;
    int dirty_cpu_scheds_pctg = 100;
    int dirty_cpu_scheds_onln_pctg = 100;
    int dirty_io_scheds;
#endif
    int max_reader_groups;
    int reader_groups;
    char envbuf[21]; /* enough for any 64-bit integer */
    size_t envbufsz;

    erts_save_emu_args(*argc, argv);

    erts_sched_compact_load = 1;
    erts_printf_eterm_func = erts_printf_term;
    display_items = 200;
    erts_backtrace_depth = DEFAULT_BACKTRACE_SIZE;
    erts_async_max_threads = ERTS_DEFAULT_NO_ASYNC_THREADS;
    erts_async_thread_suggested_stack_size = ERTS_ASYNC_THREAD_MIN_STACK_SIZE;
    H_MIN_SIZE = H_DEFAULT_SIZE;
    BIN_VH_MIN_SIZE = VH_DEFAULT_SIZE;

    erts_initialized = 0;

    erts_use_sender_punish = 1;

    erts_pre_early_init_cpu_topology(&max_reader_groups,
				     &ncpu,
				     &ncpuonln,
				     &ncpuavail);
#ifndef ERTS_SMP
    ncpu = 1;
    ncpuonln = 1;
    ncpuavail = 1;
#endif

    ignore_break = 0;
    replace_intr = 0;
    program = argv[0];

    erts_modified_timing_level = -1;

    erts_compat_rel = this_rel_num();

    erts_sys_pre_init();
    erts_atomic_init_nob(&exiting, 0);
#ifdef ERTS_SMP
    erts_thr_progress_pre_init();
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init();
#endif
#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&erts_writing_erl_crash_dump, 0L);
    erts_tsd_key_create(&erts_is_crash_dumping_key,"erts_is_crash_dumping_key");
#else
    erts_writing_erl_crash_dump = 0;
#endif

    erts_smp_atomic32_init_nob(&erts_max_gen_gcs,
			       (erts_aint32_t) ((Uint16) -1));

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

#ifdef ERTS_DIRTY_SCHEDULERS
    dirty_cpu_scheds = no_schedulers;
    dirty_cpu_scheds_online = no_schedulers_online;
    dirty_io_scheds = 10;
#endif

    envbufsz = sizeof(envbuf);

    /* erts_sys_getenv(_raw)() not initialized yet; need erts_sys_getenv__() */
    if (erts_sys_getenv__("ERL_THREAD_POOL_SIZE", envbuf, &envbufsz) == 0)
	erts_async_max_threads = atoi(envbuf);
    else
	erts_async_max_threads = ERTS_DEFAULT_NO_ASYNC_THREADS;
    if (erts_async_max_threads > ERTS_MAX_NO_OF_ASYNC_THREADS)
	erts_async_max_threads = ERTS_MAX_NO_OF_ASYNC_THREADS;

    if (argc && argv) {
	int i = 1;
	while (i < *argc) {
	    if (strcmp(argv[i], "--") == 0) { /* end of emulator options */
		i++;
		break;
	    }
	    if (argv[i][0] == '-') {
		switch (argv[i][1]) {
		case 'r': {
		    char *sub_param = argv[i]+2;
		    if (has_prefix("g", sub_param)) {
			char *arg = get_arg(sub_param+1, argv[i+1], &i);
			if (sscanf(arg, "%d", &max_reader_groups) != 1) {
			    erts_fprintf(stderr,
					 "bad reader groups limit: %s\n", arg);
			    erts_usage();
			}
			if (max_reader_groups < 0) {
			    erts_fprintf(stderr,
					 "bad reader groups limit: %d\n",
					 max_reader_groups);
			    erts_usage();
			}
		    }
		    break;
		}
		case 'A': {
		    /* set number of threads in thread pool */
		    char *arg = get_arg(argv[i]+2, argv[i+1], &i);
		    if (((erts_async_max_threads = atoi(arg)) < ERTS_MIN_NO_OF_ASYNC_THREADS) ||
			(erts_async_max_threads > ERTS_MAX_NO_OF_ASYNC_THREADS)) {
			erts_fprintf(stderr,
				     "bad number of async threads %s\n",
				     arg);
			erts_usage();
			VERBOSE(DEBUG_SYSTEM, ("using %d async-threads\n",
					       erts_async_max_threads));
		    }
		    break;
		}
		case 'S' :
		    if (argv[i][2] == 'P') {
			int ptot, ponln;
			char *arg = get_arg(argv[i]+3, argv[i+1], &i);
			switch (sscanf(arg, "%d:%d", &ptot, &ponln)) {
			case 0:
			    switch (sscanf(arg, ":%d", &ponln)) {
			    case 1:
				if (ponln < 0)
				    goto bad_SP;
				ptot = 100;
				goto chk_SP;
			    default:
				goto bad_SP;
			    }
			case 1:
			    if (ptot < 0)
				goto bad_SP;
			    ponln = ptot < 100 ? ptot : 100;
			    goto chk_SP;
			case 2:
			    if (ptot < 0 || ponln < 0)
				goto bad_SP;
			chk_SP:
			    schdlrs_percentage = ptot;
			    schdlrs_onln_percentage = ponln;
			    break;
			default:
                        bad_SP:
                            erts_fprintf(stderr,
                                         "bad schedulers percentage specifier %s\n",
                                         arg);
                            erts_usage();
                            break;
                        }

                        VERBOSE(DEBUG_SYSTEM,
                                ("using %d:%d scheduler percentages\n",
                                 schdlrs_percentage, schdlrs_onln_percentage));
                    }
#ifdef ERTS_DIRTY_SCHEDULERS
		    else if (argv[i][2] == 'D') {
			char *arg;
			char *type = argv[i]+3;
			if (strncmp(type, "Pcpu", 4) == 0) {
			    int ptot, ponln;
			    arg = get_arg(argv[i]+7, argv[i+1], &i);
			    switch (sscanf(arg, "%d:%d", &ptot, &ponln)) {
			    case 0:
				switch (sscanf(arg, ":%d", &ponln)) {
				case 1:
				    if (ponln < 0)
					goto bad_SDPcpu;
				    ptot = 100;
				    goto chk_SDPcpu;
				default:
				    goto bad_SDPcpu;
				}
			    case 1:
				if (ptot < 0)
				    goto bad_SDPcpu;
				ponln = ptot < 100 ? ptot : 100;
				goto chk_SDPcpu;
			    case 2:
				if (ptot < 0 || ponln < 0)
				    goto bad_SDPcpu;
			    chk_SDPcpu:
				dirty_cpu_scheds_pctg = ptot;
				dirty_cpu_scheds_onln_pctg = ponln;
				break;
			    default:
			    bad_SDPcpu:
				erts_fprintf(stderr,
					     "bad dirty CPU schedulers percentage specifier %s\n",
					     arg);
				erts_usage();
				break;
			    }
			    VERBOSE(DEBUG_SYSTEM,
				    ("using %d:%d dirty CPU scheduler percentages\n",
				     dirty_cpu_scheds_pctg, dirty_cpu_scheds_onln_pctg));
			} else if (strncmp(type, "cpu", 3) == 0) {
			    int tot, onln;
			    arg = get_arg(argv[i]+6, argv[i+1], &i);
			    switch (sscanf(arg, "%d:%d", &tot, &onln)) {
			    case 0:
				switch (sscanf(arg, ":%d", &onln)) {
				case 1:
				    tot = no_schedulers;
				    goto chk_SDcpu;
				default:
				    goto bad_SDcpu;
				}
			    case 1:
				onln = tot < dirty_cpu_scheds_online ?
				    tot : dirty_cpu_scheds_online;
			    case 2:
			    chk_SDcpu:
				if (tot > 0)
				    dirty_cpu_scheds = tot;
				else
				    dirty_cpu_scheds = no_schedulers + tot;
				if (onln > 0)
				    dirty_cpu_scheds_online = onln;
				else
				    dirty_cpu_scheds_online = no_schedulers_online + onln;
				if (dirty_cpu_scheds < 1 ||
				    ERTS_MAX_NO_OF_DIRTY_CPU_SCHEDULERS < dirty_cpu_scheds) {
				    erts_fprintf(stderr,
						 "bad amount of dirty CPU schedulers %d\n",
						 tot);
				    erts_usage();
				}
				if (dirty_cpu_scheds_online < 1 ||
				    dirty_cpu_scheds < dirty_cpu_scheds_online) {
				    erts_fprintf(stderr,
						 "bad amount of dirty CPU schedulers online %d "
						 "(total amount of dirty CPU schedulers %d)\n",
						 dirty_cpu_scheds_online, dirty_cpu_scheds);
				    erts_usage();
				}
				break;
			    default:
			    bad_SDcpu:
				erts_fprintf(stderr,
					     "bad amount of dirty CPU schedulers %s\n",
					     arg);
				erts_usage();
				break;
			    }
			    VERBOSE(DEBUG_SYSTEM,
				    ("using %d:%d dirty CPU scheduler(s)\n", tot, onln));
			} else if (strncmp(type, "io", 2) == 0) {
			    arg = get_arg(argv[i]+5, argv[i+1], &i);
			    dirty_io_scheds = atoi(arg);
			    if (dirty_io_scheds < 0 ||
				dirty_io_scheds > ERTS_MAX_NO_OF_DIRTY_IO_SCHEDULERS) {
				erts_fprintf(stderr,
					     "bad number of dirty I/O schedulers %s\n",
					     arg);
				erts_usage();
			    }
			    VERBOSE(DEBUG_SYSTEM,
				    ("using %d dirty I/O scheduler(s)\n", dirty_io_scheds));
			} else {
			    erts_fprintf(stderr,
					 "bad or missing dirty scheduler specifier: %s\n",
					 argv[i]);
			    erts_usage();
			    break;
			}
		    }
#endif
		    else {
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
		    }
                    break;
		default:
		    break;
		}
	    }
	    i++;
	}

#ifdef ERTS_SMP
	/* apply any scheduler percentages */
	if (schdlrs_percentage != 100 || schdlrs_onln_percentage != 100) {
	    schdlrs = schdlrs * schdlrs_percentage / 100;
	    schdlrs_onln = schdlrs_onln * schdlrs_onln_percentage / 100;
	    if (schdlrs < 1)
                schdlrs = 1;
            if (ERTS_MAX_NO_OF_SCHEDULERS < schdlrs) {
		erts_fprintf(stderr,
			     "bad schedulers percentage %d "
			     "(total amount of schedulers %d)\n",
			     schdlrs_percentage, schdlrs);
		erts_usage();
	    }
	    if (schdlrs_onln < 1)
                schdlrs_onln = 1;
            if (schdlrs < schdlrs_onln) {
		erts_fprintf(stderr,
			     "bad schedulers online percentage %d "
			     "(total amount of schedulers %d, online %d)\n",
			     schdlrs_onln_percentage, schdlrs, schdlrs_onln);
		erts_usage();
	    }
	}
#else
	/* Silence gcc warnings */
	(void)schdlrs_percentage;
	(void)schdlrs_onln_percentage;
#endif
#ifdef ERTS_DIRTY_SCHEDULERS
	/* apply any dirty scheduler precentages */
	if (dirty_cpu_scheds_pctg != 100 || dirty_cpu_scheds_onln_pctg != 100) {
	    dirty_cpu_scheds = dirty_cpu_scheds * dirty_cpu_scheds_pctg / 100;
	    dirty_cpu_scheds_online = dirty_cpu_scheds_online * dirty_cpu_scheds_onln_pctg / 100;
	}
	if (dirty_cpu_scheds > schdlrs)
	    dirty_cpu_scheds = schdlrs;
	if (dirty_cpu_scheds_online > schdlrs_onln)
	    dirty_cpu_scheds_online = schdlrs_onln;
#endif
    }

#ifndef USE_THREADS
    erts_async_max_threads = 0;
#endif

#ifdef ERTS_SMP
    no_schedulers = schdlrs;
    no_schedulers_online = schdlrs_onln;

    erts_no_schedulers = (Uint) no_schedulers;
#endif
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_no_dirty_cpu_schedulers = no_dirty_cpu_schedulers = dirty_cpu_scheds;
    no_dirty_cpu_schedulers_online = dirty_cpu_scheds_online;
    erts_no_dirty_io_schedulers = no_dirty_io_schedulers = dirty_io_scheds;
#endif
    erts_early_init_scheduling(no_schedulers);

    alloc_opts.ncpu = ncpu;
    erts_alloc_init(argc, argv, &alloc_opts); /* Handles (and removes)
						 -M flags. */
    /* Require allocators */
#ifdef ERTS_SMP
    /*
     * Thread progress management:
     *
     * * Managed threads:
     * ** Scheduler threads (see erl_process.c)
     * ** Aux thread (see erl_process.c)
     * ** Sys message dispatcher thread (see erl_trace.c)
     *
     * * Unmanaged threads that need to register:
     * ** Async threads (see erl_async.c)
     * ** Dirty scheduler threads
     */
    erts_thr_progress_init(no_schedulers,
			   no_schedulers+2,
#ifndef ERTS_DIRTY_SCHEDULERS
			   erts_async_max_threads
#else
			   erts_async_max_threads +
			   erts_no_dirty_cpu_schedulers +
			   erts_no_dirty_io_schedulers
#endif
			   );
#endif
    erts_thr_q_init();
    erts_init_utils();
    erts_early_init_cpu_topology(no_schedulers,
				 &max_main_threads,
				 max_reader_groups,
				 &reader_groups);

#ifdef USE_THREADS
    {
	erts_thr_late_init_data_t elid = ERTS_THR_LATE_INIT_DATA_DEF_INITER;
	elid.mem.std.alloc = ethr_std_alloc;
	elid.mem.std.realloc = ethr_std_realloc;
	elid.mem.std.free = ethr_std_free;
	elid.mem.sl.alloc = ethr_sl_alloc;
	elid.mem.sl.realloc = ethr_sl_realloc;
	elid.mem.sl.free = ethr_sl_free;
	elid.mem.ll.alloc = ethr_ll_alloc;
	elid.mem.ll.realloc = ethr_ll_realloc;
	elid.mem.ll.free = ethr_ll_free;
	elid.main_threads = max_main_threads;
	elid.reader_groups = reader_groups;

	erts_thr_late_init(&elid);
    }
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_late_init();
#endif
    
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_late_init();
#endif

#if defined(HIPE)
    hipe_signal_init();	/* must be done very early */
#endif

    erl_sys_args(argc, argv);

    /* Creates threads on Windows that depend on the arguments, so has to be after erl_sys_args */
    erl_sys_init();

    erts_ets_realloc_always_moves = 0;
    erts_ets_always_compress = 0;
    erts_dist_buf_busy_limit = ERTS_DE_BUSY_LIMIT;

    return ncpu;
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
    int have_break_handler = 1;
    char envbuf[21]; /* enough for any 64-bit integer */
    size_t envbufsz;
    int ncpu = early_init(&argc, argv);
    int proc_tab_sz = ERTS_DEFAULT_MAX_PROCESSES;
    int port_tab_sz = ERTS_DEFAULT_MAX_PORTS;
    int port_tab_sz_ignore_files = 0;
    int legacy_proc_tab = 0;
    int legacy_port_tab = 0;
    int time_correction;
    ErtsTimeWarpMode time_warp_mode;
    int node_tab_delete_delay = ERTS_NODE_TAB_DELAY_GC_DEFAULT;
    ErtsDbSpinCount db_spin_count = ERTS_DB_SPNCNT_NORMAL;

    set_default_time_adj(&time_correction,
			 &time_warp_mode);

    envbufsz = sizeof(envbuf);
    if (erts_sys_getenv_raw(ERL_MAX_ETS_TABLES_ENV, envbuf, &envbufsz) == 0)
	user_requested_db_max_tabs = atoi(envbuf);
    else
	user_requested_db_max_tabs = 0;

    envbufsz = sizeof(envbuf);
    if (erts_sys_getenv_raw("ERL_FULLSWEEP_AFTER", envbuf, &envbufsz) == 0) {
	Uint16 max_gen_gcs = atoi(envbuf);
	erts_smp_atomic32_set_nob(&erts_max_gen_gcs,
				  (erts_aint32_t) max_gen_gcs);
    }

    envbufsz = sizeof(envbuf);
    if (erts_sys_getenv_raw("ERL_MAX_PORTS", envbuf, &envbufsz) == 0) {
	port_tab_sz = atoi(envbuf);
	port_tab_sz_ignore_files = 1;
    }

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__DARWIN__)
    /*
     * The default stack size on MacOS X is too small for pcre.
     */
    erts_sched_thread_suggested_stack_size = 256;
#endif

#ifdef DEBUG
    verbose = DEBUG_DEFAULT;
#endif

    erts_error_logger_warnings = am_warning;

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
	case 'p':
	    if (!strncmp(argv[i],"-pc",3)) {
		int printable_chars = ERL_PRINTABLE_CHARACTERS_LATIN1;
		arg = get_arg(argv[i]+3, argv[i+1], &i);
		if (!strcmp(arg,"unicode")) {
		    printable_chars = ERL_PRINTABLE_CHARACTERS_UNICODE;
		} else if (strcmp(arg,"latin1")) {
		    erts_fprintf(stderr, "bad range of printable "
				 "characters: %s\n", arg);
		    erts_usage();
		}
		erts_set_printable_characters(printable_chars);
		break;
	    } else {
		erts_fprintf(stderr, "%s unknown flag %s\n", argv[0], argv[i]);
		erts_usage();
	    }
	case 'f':
	    if (!strncmp(argv[i],"-fn",3)) {
		int warning_type =  ERL_FILENAME_WARNING_WARNING;
		arg = get_arg(argv[i]+3, argv[i+1], &i);
		switch (*arg) {
		case 'u':
		    switch (*(arg+1)) {
		    case 'w':
		    case 0:
			break;
		    case 'i':
			warning_type =  ERL_FILENAME_WARNING_IGNORE;
			break;
		    case 'e':
			warning_type =  ERL_FILENAME_WARNING_ERROR;
			break;
		    default:
			erts_fprintf(stderr, "bad type of warnings for "
				     "wrongly coded filename: %s\n", arg+1);
			erts_usage();
		    }
		    erts_set_user_requested_filename_encoding
			(
			 ERL_FILENAME_UTF8,
			 warning_type
			 );
		    break;
		case 'l':
		    erts_set_user_requested_filename_encoding
			(
			 ERL_FILENAME_LATIN1,
			 warning_type
			 );
		    break;
		case 'a':
		    switch (*(arg+1)) {
		    case 'w':
		    case 0:
			break;
		    case 'i':
			warning_type =  ERL_FILENAME_WARNING_IGNORE;
			break;
		    case 'e':
			warning_type =  ERL_FILENAME_WARNING_ERROR;
			break;
		    default:
			erts_fprintf(stderr, "bad type of warnings for "
				     "wrongly coded filename: %s\n", arg+1);
			erts_usage();
		    }
		    erts_set_user_requested_filename_encoding
			(
			 ERL_FILENAME_UNKNOWN,
			 warning_type
			 );
		    break;
		default:
		    erts_fprintf(stderr, "bad filename encoding %s, can be "
				 "(l,u or a, optionally followed by w, "
				 "i or e)\n", arg);
		    erts_usage();
		}
		break;
	    } else {
		erts_fprintf(stderr, "%s unknown flag %s\n", argv[0], argv[i]);
		erts_usage();
	    }
	case 'L':
	    erts_no_line_info = 1;
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

	case 'h': {
	    char *sub_param = argv[i]+2;
	    /* set default heap size
	     *
	     * h|ms  - min_heap_size
	     * h|mbs - min_bin_vheap_size
	     * h|pds - erts_pd_initial_size
	     *
	     */
	    if (has_prefix("mbs", sub_param)) {
		arg = get_arg(sub_param+3, argv[i+1], &i);
		if ((BIN_VH_MIN_SIZE = atoi(arg)) <= 0) {
		    erts_fprintf(stderr, "bad heap size %s\n", arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM, ("using minimum binary virtual heap size %d\n", BIN_VH_MIN_SIZE));

	    } else if (has_prefix("ms", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		if ((H_MIN_SIZE = atoi(arg)) <= 0) {
		    erts_fprintf(stderr, "bad heap size %s\n", arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM, ("using minimum heap size %d\n", H_MIN_SIZE));
	    } else if (has_prefix("pds", sub_param)) {
		arg = get_arg(sub_param+3, argv[i+1], &i);
		if ((erts_pd_initial_size = atoi(arg)) <= 0) {
		    erts_fprintf(stderr, "bad initial process dictionary size %s\n", arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM, ("using initial process dictionary size %d\n",
			    erts_pd_initial_size));
	    } else {
	        /* backward compatibility */
		arg = get_arg(argv[i]+2, argv[i+1], &i);
		if ((H_MIN_SIZE = atoi(arg)) <= 0) {
		    erts_fprintf(stderr, "bad heap size %s\n", arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM, ("using minimum heap size %d\n", H_MIN_SIZE));
	    }
	    break;
	}
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
	    if (sys_strcmp("c", argv[i]+2) == 0) {
		erts_ets_always_compress = 1;
	    }
	    else {
		/* set maximum number of ets tables */
		arg = get_arg(argv[i]+2, argv[i+1], &i);
		if (( user_requested_db_max_tabs = atoi(arg) ) < 0) {
		    erts_fprintf(stderr, "bad maximum number of ets tables %s\n", arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM,
			("using maximum number of ets tables %d\n",
			 user_requested_db_max_tabs));
	    }
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

	case 'n':
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    switch (arg[0]) {
	    case 's': /* synchronous */
		erts_port_synchronous_ops = 1;
		erts_port_schedule_all_ops = 0;
		break;
	    case 'a': /* asynchronous */
		erts_port_synchronous_ops = 0;
		erts_port_schedule_all_ops = 1;
		break;
	    case 'd': /* Default - schedule on conflict (asynchronous) */
		erts_port_synchronous_ops = 0;
		erts_port_schedule_all_ops = 0;
		break;
	    default:
	    bad_n_option:
		erts_fprintf(stderr, "bad -n option %s\n", arg);
		erts_usage();
	    }
	    if (arg[1] != '\0')
		goto bad_n_option;
	    break;

	case 'P': /* set maximum number of processes */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (strcmp(arg, "legacy") == 0)
		legacy_proc_tab = 1;
	    else {
		errno = 0;
		proc_tab_sz = strtol(arg, NULL, 10);
		if (errno != 0
		    || proc_tab_sz < ERTS_MIN_PROCESSES
		    || ERTS_MAX_PROCESSES < proc_tab_sz) {
		    erts_fprintf(stderr, "bad number of processes %s\n", arg);
		    erts_usage();
		}
	    }
	    break;

	case 'Q': /* set maximum number of ports */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (strcmp(arg, "legacy") == 0)
		legacy_port_tab = 1;
	    else {
		errno = 0;
		port_tab_sz = strtol(arg, NULL, 10);
		if (errno != 0
		    || port_tab_sz < ERTS_MIN_PROCESSES
		    || ERTS_MAX_PROCESSES < port_tab_sz) {
		    erts_fprintf(stderr, "bad number of ports %s\n", arg);
		    erts_usage();
		}
		port_tab_sz_ignore_files = 1;
	    }
	    break;

	case 'S' : /* Was handled in early_init() just read past it */
	    if (argv[i][2] == 'D') {
		char* type = argv[i]+3;
		if (strcmp(type, "Pcpu") == 0)
		    (void) get_arg(argv[i]+7, argv[i+1], &i);
		if (strcmp(type, "cpu") == 0)
		    (void) get_arg(argv[i]+6, argv[i+1], &i);
		else if (strcmp(type, "io") == 0)
		    (void) get_arg(argv[i]+5, argv[i+1], &i);
	    } else if (argv[i][2] == 'P')
		(void) get_arg(argv[i]+3, argv[i+1], &i);
	    else
		(void) get_arg(argv[i]+2, argv[i+1], &i);
	    break;

	case 's' : {
	    char *estr;
	    int res;
	    char *sub_param = argv[i]+2;
	    if (has_prefix("bt", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		res = erts_init_scheduler_bind_type_string(arg);
		if (res != ERTS_INIT_SCHED_BIND_TYPE_SUCCESS) {
		    switch (res) {
		    case ERTS_INIT_SCHED_BIND_TYPE_NOT_SUPPORTED:
			estr = "not supported";
			break;
		    case ERTS_INIT_SCHED_BIND_TYPE_ERROR_NO_CPU_TOPOLOGY:
			estr = "no cpu topology available";
			break;
		    case ERTS_INIT_SCHED_BIND_TYPE_ERROR_BAD_TYPE:
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
	    else if (has_prefix("bwt", sub_param)) {
		arg = get_arg(sub_param+3, argv[i+1], &i);
		if (erts_sched_set_busy_wait_threshold(arg) != 0) {
		    erts_fprintf(stderr, "bad scheduler busy wait threshold: %s\n",
				 arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM,
			("scheduler wakup threshold: %s\n", arg));
	    }
	    else if (has_prefix("cl", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		if (sys_strcmp("true", arg) == 0) {
		    erts_sched_compact_load = 1;
		    erts_sched_balance_util = 0;
		}
		else if (sys_strcmp("false", arg) == 0)
		    erts_sched_compact_load = 0;
		else {
		    erts_fprintf(stderr,
				 "bad scheduler compact load value '%s'\n",
				 arg);
		    erts_usage();
		}
	    }
	    else if (has_prefix("ct", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		res = erts_init_cpu_topology_string(arg);
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
	    else if (has_prefix("ecio", sub_param)) {
		arg = get_arg(sub_param+4, argv[i+1], &i);
#ifndef __OSE__
		if (sys_strcmp("true", arg) == 0)
		    erts_eager_check_io = 1;
		else
#endif
		if (sys_strcmp("false", arg) == 0)
		    erts_eager_check_io = 0;
		else {
		    erts_fprintf(stderr,
				 "bad schedule eager check I/O value '%s'\n",
				 arg);
		    erts_usage();
		}
	    }
	    else if (has_prefix("pp", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		if (sys_strcmp(arg, "true") == 0)
		    erts_port_parallelism = 1;
		else if (sys_strcmp(arg, "false") == 0)
		    erts_port_parallelism = 0;
		else {
		    erts_fprintf(stderr,
				 "bad port parallelism scheduling hint %s\n",
				 arg);
		    erts_usage();
		}
	    }
	    else if (sys_strcmp("nsp", sub_param) == 0)
		erts_use_sender_punish = 0;
	    else if (has_prefix("tbt", sub_param)) {
		arg = get_arg(sub_param+3, argv[i+1], &i);
		res = erts_init_scheduler_bind_type_string(arg);
		if (res == ERTS_INIT_SCHED_BIND_TYPE_ERROR_BAD_TYPE) {
		    erts_fprintf(stderr,
				 "setting scheduler bind type '%s' failed: invalid type\n",
				 arg);
		    erts_usage();
		}
	    }
	    else if (has_prefix("ub", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		if (sys_strcmp("true", arg) == 0) {
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT
		    erts_sched_balance_util = 1;
#else
		    erts_fprintf(stderr,
				 "scheduler utilization balancing not "
				 "supported on this system\n");
		    erts_usage();
#endif
		}
		else if (sys_strcmp("false", arg) == 0)
		    erts_sched_balance_util = 0;
		else {
		    erts_fprintf(stderr, "bad scheduler utilization balancing "
				 " value '%s'\n", arg);
		    erts_usage();
		}
	    }
	    else if (has_prefix("wct", sub_param)) {
		arg = get_arg(sub_param+3, argv[i+1], &i);
		if (erts_sched_set_wake_cleanup_threshold(arg) != 0) {
		    erts_fprintf(stderr, "scheduler wake cleanup threshold: %s\n",
				 arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM,
			("scheduler wake cleanup threshold: %s\n", arg));
	    }
	    else if (has_prefix("wt", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		if (erts_sched_set_wakeup_other_thresold(arg) != 0) {
		    erts_fprintf(stderr, "scheduler wakeup threshold: %s\n",
				 arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM,
			("scheduler wakeup threshold: %s\n", arg));
	    }
	    else if (has_prefix("ws", sub_param)) {
		arg = get_arg(sub_param+2, argv[i+1], &i);
		if (erts_sched_set_wakeup_other_type(arg) != 0) {
		    erts_fprintf(stderr, "scheduler wakeup strategy: %s\n",
				 arg);
		    erts_usage();
		}
		VERBOSE(DEBUG_SYSTEM,
			("scheduler wakeup threshold: %s\n", arg));
	    }
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
	    else if (has_prefix("fwi", sub_param)) {
		long val;
		arg = get_arg(sub_param+3, argv[i+1], &i);
		errno = 0;
		val = strtol(arg, NULL, 10);
		if (errno != 0 || val < 0) {
		    erts_fprintf(stderr,
				 "bad scheduler forced wakeup "
				 "interval %s\n",
				 arg);
		    erts_usage();
		}
#ifdef ERTS_SMP
		erts_runq_supervision_interval = val;
#endif
	    }
	    else {
		erts_fprintf(stderr, "bad scheduling option %s\n", argv[i]);
		erts_usage();
	    }
	    break;
	}
	case 't':
	    /* set atom table size */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    errno = 0;
	    erts_atom_table_size = strtol(arg, NULL, 10);
	    if (errno != 0 ||
		erts_atom_table_size < MIN_ATOM_TABLE_SIZE ||
		erts_atom_table_size > MAX_ATOM_TABLE_SIZE) {
		erts_fprintf(stderr, "bad atom table size %s\n", arg);
		erts_usage();
	    }
	    VERBOSE(DEBUG_SYSTEM,
                    ("setting maximum number of atoms to %d\n",
		     erts_atom_table_size));
	    break;

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
	    int this_rel;

	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    erts_compat_rel = atoi(arg);

	    this_rel = this_rel_num();
	    if (erts_compat_rel < this_rel - 2 || this_rel < erts_compat_rel) {
		erts_fprintf(stderr, "bad compatibility release number %s\n", arg);
		erts_usage();
	    }

	    switch (erts_compat_rel) {
		/* Currently no compat features... */
	    default:
		break;
	    }

	    break;
	}

	case 'A': /* Was handled in early init just read past it */
	    (void) get_arg(argv[i]+2, argv[i+1], &i);
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

	case 'r': {
	    char *sub_param = argv[i]+2;
	    if (has_prefix("g", sub_param)) {
		get_arg(sub_param+1, argv[i+1], &i);
		/* already handled */
	    }
	    else {
		erts_ets_realloc_always_moves = 1;
	    }
	    break;
	}
	case 'C':
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (sys_strcmp(arg, "no_time_warp") == 0)
		time_warp_mode = ERTS_NO_TIME_WARP_MODE;
	    else if (sys_strcmp(arg, "single_time_warp") == 0)
		time_warp_mode = ERTS_SINGLE_TIME_WARP_MODE;
	    else if (sys_strcmp(arg, "multi_time_warp") == 0)
		time_warp_mode = ERTS_MULTI_TIME_WARP_MODE;
	    else {
		erts_fprintf(stderr,
			     "Invalid time warp mode: %s\n", arg);
		erts_usage();
	    }
	    break;
	case 'c':
	    if (sys_strcmp(argv[i]+2, "false") == 0)
		goto time_correction_false;
	    else if (sys_strcmp(argv[i]+2, "true") == 0)
		goto time_correction_true;
	    else if (argv[i][2] == '\0') {
		if (i + 1 >= argc)
		    goto time_correction_false;
		else {
		    if (sys_strcmp(argv[i+1], "false") == 0) {
			(void) get_arg(argv[i]+2, argv[i+1], &i);
			goto time_correction_false;
		    }
		    else if (sys_strcmp(argv[i+1], "true") == 0) {
			(void) get_arg(argv[i]+2, argv[i+1], &i);
		    time_correction_true:
			time_correction = 1;
			break;
		    }
		    else {
		    time_correction_false:
			time_correction = 0;
			break;
		    }
		}
	    }
	    else {
		arg = get_arg(argv[i]+2, argv[i+1], &i);
		erts_fprintf(stderr, "Invalid time correnction value: %s\n", arg);
		erts_usage();
	    }
	    break;
	case 'W':
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    switch (arg[0]) {
	    case 'i':
		erts_error_logger_warnings = am_info;
		break;
	    case 'e':
		erts_error_logger_warnings = am_error;
		break;
	    case 'w':
		erts_error_logger_warnings = am_warning;
		break;
	    default:
		erts_fprintf(stderr, "unrecognized warning_map option %s\n", arg);
		erts_usage();
	    }
	    break;

	case 'z': {
	    char *sub_param = argv[i]+2;

	    if (has_prefix("dbbl", sub_param)) {
		int new_limit;
		arg = get_arg(sub_param+4, argv[i+1], &i);
		new_limit = atoi(arg);
		if (new_limit < 1 || INT_MAX/1024 < new_limit) {
		    erts_fprintf(stderr, "Invalid dbbl limit: %d\n", new_limit);
		    erts_usage();
		} else {
		    erts_dist_buf_busy_limit = new_limit*1024;
		}
	    }
	    else if (has_prefix("dntgc", sub_param)) {
		long secs;

		arg = get_arg(sub_param+5, argv[i+1], &i);
		if (sys_strcmp(arg, "infinity") == 0)
		    secs = ERTS_NODE_TAB_DELAY_GC_INFINITY;
		else {
		    char *endptr;
		    errno = 0;
		    secs = strtol(arg, &endptr, 10);
		    if (errno != 0 || *arg == '\0' || *endptr != '\0'
			|| secs < 0 || ERTS_NODE_TAB_DELAY_GC_MAX < secs) {
			erts_fprintf(stderr, "Invalid delayed node table gc: %s\n", arg);
			erts_usage();
		    }
		}
		node_tab_delete_delay = (int) secs;
	    }
	    else if (has_prefix("ebwt", sub_param)) {
		arg = get_arg(sub_param+4, argv[i+1], &i);
		if (sys_strcmp(arg, "none") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_NONE;
		else if (sys_strcmp(arg, "very_short") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_VERY_LOW;
		else if (sys_strcmp(arg, "short") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_LOW;
		else if (sys_strcmp(arg, "medium") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_NORMAL;
		else if (sys_strcmp(arg, "long") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_HIGH;
		else if (sys_strcmp(arg, "very_long") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_VERY_HIGH;
		else if (sys_strcmp(arg, "extremely_long") == 0)
		    db_spin_count = ERTS_DB_SPNCNT_EXTREMELY_HIGH;
		else {
		    erts_fprintf(stderr,
				 "Invalid ets busy wait threshold: %s\n", arg);
		    erts_usage();
		}
	    } else {
		erts_fprintf(stderr, "bad -z option %s\n", argv[i]);
		erts_usage();
	    }
	    break;
        }

	default:
	    erts_fprintf(stderr, "%s unknown flag %s\n", argv[0], argv[i]);
	    erts_usage();
	}
	i++;
    }

    if (!erts_check_time_adj_support(time_correction, time_warp_mode)) {
	char *time_correction_str = time_correction ? "Enabled" : "Disabled";
	char *time_warp_str = "undefined";
	switch (time_warp_mode) {
	case ERTS_NO_TIME_WARP_MODE:
	    time_warp_str = "no";
	    break;
	case ERTS_SINGLE_TIME_WARP_MODE:
	    time_warp_str = "single";
	    break;
	case ERTS_MULTI_TIME_WARP_MODE:
	    time_warp_str = "multi";
	    break;
	default:
	    time_warp_str = "undefined";
	    break;
	}
	erts_fprintf(stderr, "%s time correction with %s time warp mode "
		     "is not supported on this platform\n",
		     time_correction_str,
		     time_warp_str);
	erts_usage();
    }

/* Output format on windows for sprintf defaults to three exponents.
 * We use two-exponent to mimic normal sprintf behaviour.
 */

#if defined(__WIN32__) && defined(_TWO_DIGIT_EXPONENT)
    _set_output_format(_TWO_DIGIT_EXPONENT);
#endif

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

    erl_init(ncpu,
	     proc_tab_sz,
	     legacy_proc_tab,
	     port_tab_sz,
	     port_tab_sz_ignore_files,
	     legacy_port_tab,
	     time_correction,
	     time_warp_mode,
	     node_tab_delete_delay,
	     db_spin_count);

    load_preloaded();
    erts_end_staging_code_ix();
    erts_commit_staging_code_ix();

    erts_initialized = 1;

    (void) erl_first_process_otp("otp_ring0", NULL, 0,
				 boot_argc, boot_argv);

#ifdef ERTS_SMP
    erts_start_schedulers();
    /* Let system specific code decide what to do with the main thread... */

    erts_sys_main_thread(); /* May or may not return! */
#else
    {
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
	erts_thr_set_main_status(1, 1);
#if ERTS_USE_ASYNC_READY_Q
	esdp->aux_work_data.async_ready.queue
	    = erts_get_async_ready_queue(1);
#endif
	set_main_stack_size();
	erts_sched_init_time_sup(esdp);
	process_main();
    }
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
system_cleanup(int flush_async)
{
    /*
     * Make sure only one thread exits the runtime system.
     */
    if (erts_atomic_inc_read_nob(&exiting) != 1) {
	/*
	 * Another thread is currently exiting the system;
	 * wait for it to do its job.
	 */
#ifdef ERTS_SMP
	if (erts_thr_progress_is_managed_thread()) {
	    /*
	     * The exiting thread might be waiting for
	     * us to block; need to update status...
	     */
	    erts_thr_progress_active(NULL, 0);
	    erts_thr_progress_prepare_wait(NULL);
	}
#endif
	/* Wait forever... */
	while (1)
	    erts_milli_sleep(10000000);
    }

    /* No cleanup wanted if ...
     * 1. we are about to do an abnormal exit
     * 2. we haven't finished initializing, or
     * 3. another thread than the main thread is performing the exit
     *    (in threaded non smp case).
     */

    if (!flush_async
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
#endif

    erts_exit_flush_async();
}

static __decl_noreturn void __noreturn
erl_exit_vv(int n, int flush_async, char *fmt, va_list args1, va_list args2)
{
    unsigned int an;

    system_cleanup(flush_async);

    save_statistics();
    if (n < 0)
        an = -(unsigned int)n;
    else
        an = n;

    if (erts_mtrace_enabled)
	erts_mtrace_exit((Uint32) an);

    /* Produce an Erlang core dump if error */
    if (((n > 0 && erts_no_crash_dump == 0) || n == ERTS_DUMP_EXIT)
	&& erts_initialized) {
	erl_crash_dump_v((char*) NULL, 0, fmt, args1);
    }

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args2);	/* Print error message. */
    sys_tty_reset(n);

    if (n == ERTS_INTR_EXIT)
	exit(0);
    else if (n == ERTS_DUMP_EXIT)
	ERTS_EXIT_AFTER_DUMP(1);
    else if (n > 0 || n == ERTS_ABORT_EXIT)
        abort();
    exit(an);
}

/* Exit without flushing async threads */
__decl_noreturn void __noreturn erl_exit(int n, char *fmt, ...)
{
    va_list args1, args2;
    va_start(args1, fmt);
    va_start(args2, fmt);
    erl_exit_vv(n, 0, fmt, args1, args2);
    va_end(args2);
    va_end(args1);
}

/* Exit after flushing async threads */
__decl_noreturn void __noreturn erl_exit_flush_async(int n, char *fmt, ...)
{
    va_list args1, args2;
    va_start(args1, fmt);
    va_start(args2, fmt);
    erl_exit_vv(n, 1, fmt, args1, args2);
    va_end(args2);
    va_end(args1);
}
