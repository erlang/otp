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

/*
 * This is a C version of the erl.exec Bourne shell script, including
 * additions required for Windows NT.
 */

#include "etc_common.h"

#include "erl_driver.h"
#include "erl_misc_utils.h"

#ifdef __WIN32__
#  include "erl_version.h"
#  include "init_file.h"
#  include <Shlobj.h>
#endif

#define NO 0
#define YES 1
#define DEFAULT_PROGNAME "erl"

#ifdef __WIN32__
#define INI_FILENAME L"erl.ini"
#define INI_SECTION "erlang"
#define DIRSEP "\\"
#define PATHSEP ";"
#define NULL_DEVICE "nul"
#define BINARY_EXT ""
#define DLL_EXT ".dll"
#define EMULATOR_EXECUTABLE "beam.dll"
#else
#define PATHSEP ":"
#define DIRSEP "/"
#define NULL_DEVICE "/dev/null"
#define BINARY_EXT ""
#define EMULATOR_EXECUTABLE "beam"

#endif
#define QUOTE(s) s

/* +M alloc_util allocators */
static const char plusM_au_allocs[]= {
    'u',	/* all alloc_util allocators */
    'B',	/* binary_alloc		*/
    'I',	/* literal_alloc	*/
    'D',	/* std_alloc		*/
    'E',	/* ets_alloc		*/
    'F',	/* fix_alloc		*/
    'H',	/* eheap_alloc		*/
    'L',	/* ll_alloc		*/
    'R',	/* driver_alloc		*/
    'S',	/* sl_alloc		*/
    'T',	/* temp_alloc		*/
    'X',	/* exec_alloc		*/
    'Z',        /* test_alloc           */
    '\0'
};

/* +M alloc_util allocator specific arguments */
static char *plusM_au_alloc_switches[] = {
    "as",
    "asbcst",
    "atags",
    "acul",
    "acnl",
    "acfml",
    "e",
    "t",
    "lmbcs",
    "mbcgs",
    "mbsd",
    "mmbcs",
    "mmmbc",
    "mmsbc",
    "msbclt",
    "ramv",
    "rmbcmt",
    "rsbcmt",
    "rsbcst",
    "sbct",
    "smbcs",
    NULL
};

/* +M other arguments */
static char *plusM_other_switches[] = {
    "ea",
    "ummc",
    "uycs",
    "usac",
    "im",
    "is",
    "it",
    "lpm",
    "Mamcbf",
    "Mrmcbf",
    "Mmcs",
    "Mscs",
    "Mscrfsd",
    "Msco",
    "Mscrpm",
    "Ye",
    "Ym",
    "Ytp",
    "Ytt",
    "Iscs",
    "Xscs",
    NULL
};

/* +s arguments with values */
static char *pluss_val_switches[] = {
    "bt",
    "bwtdcpu",
    "bwtdio",
    "bwt",
    "cl",
    "ct",
    "ecio",
    "fwi",
    "tbt",
    "wct",
    "wtdcpu",
    "wtdio",
    "wt",
    "ws",
    "ss",
    "ssdcpu",
    "ssdio",
    "pp",
    "ub",
    NULL
};
/* +h arguments with values */
static char *plush_val_switches[] = {
    "ms",
    "mbs",
    "pds",
    "max",
    "maxk",
    "maxel",
    "mqd",
    "",
    NULL
};

/* +r arguments with values */
static char *plusr_val_switches[] = {
    "g",
    NULL
};

/* +z arguments with values */
static char *plusz_val_switches[] = {
    "dbbl",
    "dntgc",
    "ebwt",
    NULL
};


/*
 * Define sleep(seconds) in terms of Sleep() on Windows.
 */

#ifdef __WIN32__
#define sleep(seconds) Sleep(seconds*1000)
#endif

#define SMP_SUFFIX	  ".smp"

void usage(const char *switchname);
static void usage_format(char *format, ...);
void start_epmd(char *epmd);
void error(char* format, ...);

/*
 * Local functions.
 */

static void usage_notsup(const char *switchname, const char *alt);
static char **build_args_from_env(char *env_var);
static char **build_args_from_string(char *env_var);
static void initial_argv_massage(int *argc, char ***argv);
static void get_parameters(int argc, char** argv);
static void add_arg(char *new_arg);
static void add_args(char *first_arg, ...);
static void ensure_EargsSz(int sz);
static void add_Eargs(char *new_arg);
static void *emalloc(size_t size);
static void *erealloc(void *p, size_t size);
static void efree(void *p);
static char* strsave(char* string);
static int is_one_of_strings(char *str, char *strs[]);
static char *write_str(char *to, const char *from);
static void get_home(void);
static void add_epmd_port(void);
#ifdef __WIN32__
static void get_start_erl_data(char *);
static char* get_value(HKEY key, char* value_name, BOOL mustExit);
static char* possibly_quote(char* arg);

/* 
 * Functions from win_erlexec.c
 */
int start_win_emulator(char* emu, char *startprog,char** argv, int start_detached);
int start_emulator(char* emu, char*start_prog, char** argv, int start_detached);
#endif



/*
 * Variables.
 */
int nohup = 0;
int keep_window = 0;

static char **Eargsp = NULL;	/* Emulator arguments (to appear first). */
static int EargsSz = 0;		/* Size of Eargsp */
static int EargsCnt = 0;	/* Number of emulator arguments. */
static char **argsp = NULL;	/* Common arguments. */
static int argsCnt = 0;		/* Number of common arguments */
static int argsSz = 0;		/* Size of argsp */
static char tmpStr[10240];	/* Temporary string buffer. */
static int verbose = 0;		/* If non-zero, print some extra information. */
static int start_detached = 0;	/* If non-zero, the emulator should be
				 * started detached (in the background).
				 */
static int start_smp_emu = 1;   /* Start the smp emulator. */
static const char* emu_type = 0; /* Type of emulator (lcnt, valgrind, etc) */

#ifdef __WIN32__
static char *start_emulator_program = NULL; /* For detachec mode - 
					       erl.exe/werl.exe */
static char* key_val_name = ERLANG_VERSION; /* Used by the registry
					   * access functions.
					   */
static char* boot_script = NULL; /* used by option -start_erl and -boot */
static char** config_scripts = NULL; /* used by option -start_erl and -config */
static int config_script_cnt = 0;
static int got_start_erl = 0;

static HANDLE this_module_handle;
static int run_werl;
static WCHAR *utf8_to_utf16(unsigned char *bytes);
static char *utf16_to_utf8(WCHAR *wstr);
static WCHAR *latin1_to_utf16(char *str);
#endif

/*
 * Needed parameters to be fetched from the environment (Unix)
 * or the ini file (Win32).
 */

static char* bindir;		/* Location of executables. */
static char* rootdir;		/* Root location of Erlang installation. */
static char* emu;		/* Emulator to run. */
static char* progname;		/* Name of this program. */
static char* home;		/* Path of user's home directory. */

static void
set_env(char *key, char *value)
{
#ifdef __WIN32__
    WCHAR *wkey = latin1_to_utf16(key);
    WCHAR *wvalue = utf8_to_utf16(value);
    if (!SetEnvironmentVariableW(wkey, wvalue))
	error("SetEnvironmentVariable(\"%s\", \"%s\") failed!", key, value);
    efree(wkey);
    efree(wvalue);
#else
    size_t size = strlen(key) + 1 + strlen(value) + 1;
    char *str = emalloc(size);
    sprintf(str, "%s=%s", key, value);
    if (putenv(str) != 0)
	error("putenv(\"%s\") failed!", str);
#ifdef HAVE_COPYING_PUTENV
    efree(str);
#endif
#endif
}


static char *
get_env(char *key)
{
#ifdef __WIN32__
    DWORD size = 32;
    WCHAR *value = NULL;
    WCHAR *wkey = latin1_to_utf16(key);
    char *res;
    while (1) {
	DWORD nsz;
	if (value)
	    efree(value);
	value = emalloc(size*sizeof(WCHAR));
	SetLastError(0);
	nsz = GetEnvironmentVariableW(wkey, value, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    efree(value);
	    efree(wkey);
	    return NULL;
	}
	if (nsz <= size) {
	    efree(wkey);
	    res = utf16_to_utf8(value);
	    efree(value);
	    return res;
	}
	size = nsz;
    }
#else
    return getenv(key);
#endif
}

static void
free_env_val(char *value)
{
#ifdef __WIN32__
    if (value)
	free(value);
#endif
}

/*
 * Add the type and architecture suffix to the program name if needed.
 * On Windows, we insert it just before ".DLL".
 */
static char*
add_extra_suffixes(char *prog)
{
   char *res;
   char *p;
   int len;
#ifdef __WIN32__
   char *dll_p;
   int dll = 0;
#endif

   len = strlen(prog);

   /* Allocate enough extra space for suffixes */
   p = emalloc(len + 100);
   res = p;
   p = write_str(p, prog);

#ifdef __WIN32__
   dll_p = res + len - 4;
   if (dll_p >= res) {
      if (dll_p[0] == '.' &&
	  (dll_p[1] == 'd' || dll_p[1] == 'D') &&
	  (dll_p[2] == 'l' || dll_p[2] == 'L') &&
	  (dll_p[3] == 'l' || dll_p[3] == 'L')) {
	  p = dll_p;
	  dll = 1;
      }
   }
#endif

   if (emu_type && strcmp("opt",emu_type) != 0) {
       p = write_str(p, ".");
       p = write_str(p, emu_type);
   }
   if (start_smp_emu) {
       p = write_str(p, SMP_SUFFIX);
   }
#ifdef __WIN32__
   if (dll) {
       p = write_str(p, DLL_EXT);
   }
#endif

   return res;
}

#ifdef __WIN32__
static void add_boot_config(void)
{
    int i;
    if (boot_script)
	add_args("-boot", boot_script, NULL);
    for (i = 0; i < config_script_cnt; i++) {
        add_args("-config", config_scripts[i], NULL);
    }
}
# define ADD_BOOT_CONFIG add_boot_config()
#else
# define ADD_BOOT_CONFIG
#endif


#ifdef __WIN32__
__declspec(dllexport) int win_erlexec(int argc, char **argv, HANDLE module, int windowed)
#else
int main(int argc, char **argv)
#endif
{
    int haltAfterwards = 0;	/* If true, put 's erlang halt' at the end
				 * of the arguments. */
    int isdistributed = 0;
    int no_epmd = 0;
    int i;
    char* s;
    char *epmd_prog = NULL;
    char *malloc_lib;
    int process_args = 1;
    int print_args_exit = 0;
    int print_qouted_cmd_exit = 0;
    char* emu_name;

#ifdef __WIN32__
    this_module_handle = module;
    run_werl = windowed;
    /* if we started this erl just to get a detached emulator, 
     * the arguments are already prepared for beam, so we skip
     * directly to start_emulator */
    s = get_env("ERL_CONSOLE_MODE");
    if (s != NULL && strcmp(s, "detached")==0) {
	free_env_val(s);
	s = get_env("ERL_EMULATOR_DLL");
	if (s != NULL) {
	    argv[0] = strsave(s);
	} else {
	    argv[0] = strsave(EMULATOR_EXECUTABLE);
	}
	ensure_EargsSz(argc + 1);
	memcpy((void *) Eargsp, (void *) argv, argc * sizeof(char *));
	Eargsp[argc] = NULL;
	emu = argv[0];
	start_emulator_program = strsave(argv[0]);
	goto skip_arg_massage;
    }   
    free_env_val(s);
#else
    int reset_cerl_detached = 0;

    s = get_env("CERL_DETACHED_PROG");
    if (s && strcmp(s, "") != 0) {
	emu = s;
	start_detached = 1;
	reset_cerl_detached = 1;
	ensure_EargsSz(argc + 1);
	memcpy((void *) Eargsp, (void *) argv, argc * sizeof(char *));
	Eargsp[argc] = emu;
	Eargsp[argc] = NULL;
	goto skip_arg_massage;
    }
    free_env_val(s);
#endif

    initial_argv_massage(&argc, &argv); /* Merge with env; expand -args_file */

    i = 1;
#ifdef __WIN32__
    /* Not used? /rickard */
    if ((argc > 2) && (strcmp(argv[i], "-regkey") == 0)) {
	key_val_name = strsave(argv[i+1]);
	i = 3;
    }
#endif		

    get_parameters(argc, argv);
    
    /*
     * Construct the path of the executable.
     */
#if defined(__WIN32__) && defined(WIN32_ALWAYS_DEBUG)
    emu_type = "debug";
#endif

    /* We need to do this before the ordinary processing. */
    malloc_lib = get_env("ERL_MALLOC_LIB");
    while (i < argc) {
	if (argv[i][0] == '+') {
	    if (argv[i][1] == 'M' && argv[i][2] == 'Y' && argv[i][3] == 'm') {
		if (argv[i][4] == '\0') {
		    if (++i < argc)
			malloc_lib = argv[i];
		    else
			usage("+MYm");
		}
		else
		    malloc_lib = &argv[i][4];
	    }
	}
	else if (argv[i][0] == '-') {
	    if (strcmp(argv[i], "-smp") == 0) {
		if (i + 1 >= argc)
		    goto smp;

		if (strcmp(argv[i+1], "auto") == 0) {
		    i++;
		} else if (strcmp(argv[i+1], "enable") == 0) {
		    i++;
		smp_enable:
                    ;
		} else if (strcmp(argv[i+1], "disable") == 0) {
		    i++;
		smp_disable:
                    usage_notsup("-smp disable", " Use \"+S 1\" instead.");
		} else {
		smp:
                    ;
		}
	    } else if (strcmp(argv[i], "-smpenable") == 0) {
		goto smp_enable;
	    } else if (strcmp(argv[i], "-smpauto") == 0) {
                ;
	    } else if (strcmp(argv[i], "-smpdisable") == 0) {
		goto smp_disable;
	    } else if (strcmp(argv[i], "-extra") == 0) {
		break;
	    } else if (strcmp(argv[i], "-emu_type") == 0) {
		if (i + 1 >= argc) {
                    usage(argv[i]);
                }
                emu_type = argv[i+1];
                i++;
	    }
	}
	i++;
    }

    if (malloc_lib) {
	if (strcmp(malloc_lib, "libc") != 0)
	    usage("+MYm");
    }
    emu = add_extra_suffixes(emu);
    emu_name = strsave(emu);
    erts_snprintf(tmpStr, sizeof(tmpStr), "%s" DIRSEP "%s" BINARY_EXT, bindir, emu);
    emu = strsave(tmpStr);

    s = get_env("ESCRIPT_NAME");
    if(s) {
        add_Eargs(s);         /* argv[0] = scriptname*/
    } else {
        add_Eargs(emu);       /* argv[0] = erl or cerl */
    }

    /* Add the bindir to the front of the PATH, and remove all subsequent
     * occurrences to avoid ballooning it on repeated up/downgrades. */

    s = get_env("PATH");

    if (s == NULL) {
        erts_snprintf(tmpStr, sizeof(tmpStr),
            "%s" PATHSEP "%s" DIRSEP "bin" PATHSEP, bindir, rootdir);
    } else if (strstr(s, rootdir) == NULL) {
        erts_snprintf(tmpStr, sizeof(tmpStr),
            "%s" PATHSEP "%s" DIRSEP "bin" PATHSEP "%s", bindir, rootdir, s);
    } else {
        const char *bindir_slug, *bindir_slug_index;
        int bindir_slug_length;
        const char *in_index;
        char *out_index;

        erts_snprintf(tmpStr, sizeof(tmpStr), "%s" PATHSEP, bindir);

        bindir_slug = strsave(tmpStr);
        bindir_slug_length = strlen(bindir_slug);

        out_index = &tmpStr[bindir_slug_length];
        in_index = s;

        while ((bindir_slug_index = strstr(in_index, bindir_slug))) {
            int block_length = (bindir_slug_index - in_index);

            memcpy(out_index, in_index, block_length);

            in_index = bindir_slug_index + bindir_slug_length;
            out_index += block_length;
        }

        strcpy(out_index, in_index);
    }

    free_env_val(s);
    set_env("PATH", tmpStr);
    
    i = 1;

    get_home();
    add_args("-home", home, NULL);

    add_epmd_port();

    add_arg("--");

    while (i < argc) {
	if (!process_args) {	/* Copy arguments after '-extra' */
	    add_arg(argv[i]);
	    i++;
	} else {
	    switch (argv[i][0]) {
	      case '-':
		switch (argv[i][1]) {
#ifdef __WIN32__
		case 'b':
		    if (strcmp(argv[i], "-boot") == 0) {
			if (boot_script)
			    error("Conflicting -boot options");
                        if (got_start_erl)
                            error("Conflicting -start_erl and -boot options");
			if (i+1 >= argc)
			    usage("-boot");
			boot_script = strsave(argv[i+1]);
			i++;
		    }
		    else {
			add_arg(argv[i]);
		    }
		    break;
#endif
		case 'c':
		    if (strcmp(argv[i], "-compile") == 0) {
			/*
			 * Note that the shell script erl.exec does an recursive call
			 * on itself here.  We'll avoid doing that.
			 */
			add_args("-noshell", "-noinput", "-s", "c", "lc_batch",
				 NULL);
			add_Eargs("-B");
			haltAfterwards = 0;
		    }
#ifdef __WIN32__
		    else if (strcmp(argv[i], "-config") == 0){
			if (got_start_erl)
			    error("Conflicting -start_erl and -config options");
			if (i+1 >= argc)
			    usage("-config");
                        config_script_cnt++;
                        config_scripts = erealloc(config_scripts,
                                                  config_script_cnt * sizeof(char*));
			config_scripts[config_script_cnt-1] = strsave(argv[i+1]);
			i++;
		    }
#endif
		    else {
			add_arg(argv[i]);
		    }
		    break;

		  case 'd':
		    if (strcmp(argv[i], "-detached") != 0) {
			add_arg(argv[i]);
		    } else {
			start_detached = 1;
			add_args("-noshell", "-noinput", NULL);
		    }
		    break;

		  case 'e':
		    if (strcmp(argv[i], "-extra") == 0) {
			process_args = 0;
			ADD_BOOT_CONFIG;
			add_arg(argv[i]);
		    } else if (strcmp(argv[i], "-emu_args") == 0) { /* -emu_args */
			verbose = 1;
		    } else if (strcmp(argv[i], "-emu_args_exit") == 0) {
			print_args_exit = 1;
		    } else if (strcmp(argv[i], "-emu_name_exit") == 0) {
			printf("%s\n", emu_name);
			exit(0);
		    } else if (strcmp(argv[i], "-emu_qouted_cmd_exit") == 0) {
			print_qouted_cmd_exit = 1;
		    } else if (strcmp(argv[i], "-env") == 0) { /* -env VARNAME VARVALUE */
			if (i+2 >= argc)
			    usage("-env");
			set_env(argv[i+1], argv[i+2]);
			i += 2;
		    } else if (strcmp(argv[i], "-epmd") == 0) { 
			if (i+1 >= argc)
			    usage("-epmd");
			epmd_prog = argv[i+1];
			++i;
		    } else {
			add_arg(argv[i]);
		    }
		    break;
		  case 'k':
		    if (strcmp(argv[i], "-keep_window") == 0) {
			keep_window = 1;
		    } else
			add_arg(argv[i]);
		    break;

		  case 'm':
		    /*
		     * Note that the shell script erl.exec does an recursive call
		     * on itself here.  We'll avoid doing that.
		     */
		    if (strcmp(argv[i], "-make") == 0) {
			add_args("-noshell", "-noinput", "-s", "make", "all_or_nothing", NULL);
			add_Eargs("-B");
			haltAfterwards = 1;
			i = argc; /* Skip rest of command line */
		    } else if (strcmp(argv[i], "-man") == 0) {
#if defined(__WIN32__)
			error("-man not supported on Windows");
#else
			argv[i] = "man";
			erts_snprintf(tmpStr, sizeof(tmpStr), "%s/man", rootdir);
			set_env("MANPATH", tmpStr);
			execvp("man", argv+i);
			error("Could not execute the 'man' command.");
#endif
		    } else
			add_arg(argv[i]);
		    break;

		  case 'n':
		    if (strcmp(argv[i], "-name") == 0) { /* -name NAME */
			if (i+1 >= argc)
			    usage("-name");
		    
			/*
			 * Note: Cannot use add_args() here, due to non-defined
			 * evaluation order.
			 */

			add_arg(argv[i]);
			add_arg(argv[i+1]);
			isdistributed = 1;
			i++;
		    } else if (strcmp(argv[i], "-noinput") == 0) {
			add_args("-noshell", "-noinput", NULL);
		    } else if (strcmp(argv[i], "-nohup") == 0) {
			add_arg("-nohup");
			nohup = 1;
		    } else if (strcmp(argv[i], "-no_epmd") == 0) {
			add_arg("-no_epmd");
			no_epmd = 1;
		    } else {
			add_arg(argv[i]);
		    }
		    break;

		  case 's':	/* -sname NAME */
		    if (strcmp(argv[i], "-sname") == 0) {
			if (i+1 >= argc)
			    usage("-sname");
			add_arg(argv[i]);
			add_arg(argv[i+1]);
			isdistributed = 1;
			i++;
		    }
#ifdef __WIN32__
		    else if (strcmp(argv[i], "-service_event") == 0) {
			add_arg(argv[i]);
			add_arg(argv[i+1]);
			i++;
		    }		    
		    else if (strcmp(argv[i], "-start_erl") == 0) {
			if (i+1 < argc && argv[i+1][0] != '-') {
			    get_start_erl_data(argv[i+1]);
			    i++;
			} else
			    get_start_erl_data((char *) NULL);
		    }
#endif
		    else if (strcmp(argv[i], "-start_epmd") == 0) {
			if (i+1 >= argc)
			    usage("-start_epmd");

			if (strcmp(argv[i+1], "true") == 0) {
			    /* The default */
			    no_epmd = 0;
			}
			else if (strcmp(argv[i+1], "false") == 0) {
			    no_epmd = 1;
			}
			else
			    usage_format("Expected boolean argument for \'-start_epmd\'.\n");

			add_arg(argv[i]);
			add_arg(argv[i+1]);
			i++;
		    }
		    else
			add_arg(argv[i]);
		
		    break;
	
		  case 'v':	/* -version */
		    if (strcmp(argv[i], "-version") == 0) {
			add_Eargs("-V");
		    } else {
			add_arg(argv[i]);
		    }
		    break;

		  default:
		    add_arg(argv[i]);
		    break;
		} /* switch(argv[i][1] */
		break;

	      case '+':
		switch (argv[i][1]) {
		  case 'a':
		  case 'A':
		  case 'C':
		  case 'e':
		  case 'i':
		  case 'n':
		  case 'P':
		  case 'Q':
		  case 't':
		  case 'T':
		  case 'R':
		  case 'W':
		  case 'K':
		      if (argv[i][2] != '\0')
			  goto the_default;
		      if (i+1 >= argc)
			  usage(argv[i]);
		      argv[i][0] = '-';
		      add_Eargs(argv[i]);
		      add_Eargs(argv[i+1]);
		      i++;
		      break;
		  case 'I':
                      if (argv[i][2] == 'O' && (argv[i][3] == 't' || argv[i][3] == 'p')) {
                          if (argv[i][4] != '\0')
                              goto the_default;
                          argv[i][0] = '-';
                          add_Eargs(argv[i]);
                          add_Eargs(argv[i+1]);
                          i++;
                          break;
                      }
                      if (argv[i][2] == 'O' && argv[i][3] == 'P' &&
                          (argv[i][4] == 't' || argv[i][4] == 'p')) {
                          if (argv[i][5] != '\0')
                              goto the_default;
                          argv[i][0] = '-';
                          add_Eargs(argv[i]);
                          add_Eargs(argv[i+1]);
                          i++;
                          break;
                      }
                      usage(argv[i]);
                      break;
		  case 'S':
		      if (argv[i][2] == 'P') {
			  if (argv[i][3] != '\0')
			      goto the_default;
		      }
		      else if (argv[i][2] == 'D') {
			  char* type = argv[i]+3;
			  if (strncmp(type, "cpu", 3) != 0 &&
			      strncmp(type, "Pcpu", 4) != 0 &&
			      strncmp(type, "io", 2) != 0)
			      usage(argv[i]);
			  if ((argv[i][3] == 'c' && argv[i][6] != '\0') ||
			      (argv[i][3] == 'P' && argv[i][7] != '\0') ||
			      (argv[i][3] == 'i' && argv[i][5] != '\0'))
			      goto the_default;
		      }
		      else if (argv[i][2] != '\0')
			  goto the_default;
		      if (i+1 >= argc)
			  usage(argv[i]);
		      argv[i][0] = '-';
		      add_Eargs(argv[i]);
		      add_Eargs(argv[i+1]);
		      i++;
		      break;
		  case 'B':
		      argv[i][0] = '-';
		      if (argv[i][2] != '\0') {
			  if ((argv[i][2] != 'i') &&
			      (argv[i][2] != 'c') &&
			      (argv[i][2] != 'd')) { 
			  usage(argv[i]);
			} else {
			  add_Eargs(argv[i]);
			  break;
			}
		      }
		      if (i+1 < argc) {
			if ((argv[i+1][0] != '-') &&
			    (argv[i+1][0] != '+')) {
			  if (argv[i+1][0] == 'i') {
			    add_Eargs(argv[i]);
			    add_Eargs(argv[i+1]);
			    i++;
			    break;
			  } else {
			    usage(argv[i]);
			  }
			}
		      }
		      add_Eargs(argv[i]);
		      break;
		  case 'c':
		      argv[i][0] = '-';
		      if (argv[i][2] == '\0' && i+1 < argc) {
			  if (strcmp(argv[i+1], "true") == 0
			      || strcmp(argv[i+1], "false") == 0) {
			      add_Eargs(argv[i]);
			      add_Eargs(argv[i+1]);
			      i++;
			      break;
			  }
		      }
		      add_Eargs(argv[i]);
		      break;
		  case 'M': {
		      int x;
		      for (x = 0; plusM_au_allocs[x]; x++)
			  if (plusM_au_allocs[x] == argv[i][2])
			      break;
		      if ((plusM_au_allocs[x]
			   && is_one_of_strings(&argv[i][3],
						plusM_au_alloc_switches))
			  || is_one_of_strings(&argv[i][2],
					       plusM_other_switches)) {
			  if (i+1 >= argc
			      || argv[i+1][0] == '-'
			      || argv[i+1][0] == '+')
			      usage(argv[i]);
			  argv[i][0] = '-';
			  add_Eargs(argv[i]);
			  add_Eargs(argv[i+1]);
			  i++;
		      }
		      else
			  goto the_default;
		      break;
		  }
		  case 'h':
		      if (!is_one_of_strings(&argv[i][2], plush_val_switches)) {
			  goto the_default;
		      } else {
			  if (i+1 >= argc
			      || argv[i+1][0] == '-'
			      || argv[i+1][0] == '+')
			      usage(argv[i]);
			  argv[i][0] = '-';
			  add_Eargs(argv[i]);
			  add_Eargs(argv[i+1]);
			  i++;
		      }
		      break;
		  case 'r':
		      if (!is_one_of_strings(&argv[i][2],
					     plusr_val_switches))
			  goto the_default;
		      else {
			  if (i+1 >= argc
			      || argv[i+1][0] == '-'
			      || argv[i+1][0] == '+')
			      usage(argv[i]);
			  argv[i][0] = '-';
			  add_Eargs(argv[i]);
			  add_Eargs(argv[i+1]);
			  i++;
		      }
		      break;
		  case 's':
		      if (!is_one_of_strings(&argv[i][2],
					     pluss_val_switches))
			  goto the_default;
		      else {
			  if (i+1 >= argc
			      || argv[i+1][0] == '-'
			      || argv[i+1][0] == '+')
			      usage(argv[i]);
			  argv[i][0] = '-';
			  add_Eargs(argv[i]);
			  add_Eargs(argv[i+1]);
			  i++;
		      }
		      break;
		  case 'p':
		      if (argv[i][2] != 'c' || argv[i][3] != '\0')
			  goto the_default;
		      if (i+1 >= argc)
			  usage(argv[i]);
		      argv[i][0] = '-';
		      add_Eargs(argv[i]);
		      add_Eargs(argv[i+1]);
		      i++;
		      break;
		  case 'z':
		      if (!is_one_of_strings(&argv[i][2], plusz_val_switches)) {
			  goto the_default;
		      } else {
			  if (i+1 >= argc
			      || argv[i+1][0] == '-'
			      || argv[i+1][0] == '+')
			      usage(argv[i]);
			  argv[i][0] = '-';
			  add_Eargs(argv[i]);
			  add_Eargs(argv[i+1]);
			  i++;
		      }
		      break;
		  default:
		  the_default:
		    argv[i][0] = '-'; /* Change +option to -option. */
		    add_Eargs(argv[i]);
		}
		break;
      
	      default:
		add_arg(argv[i]);
	    } /* switch(argv[i][0] */
	    i++;
	}
    }

    if (process_args) {
	ADD_BOOT_CONFIG;
    }
#undef ADD_BOOT_CONFIG

    /* Doesn't conflict with -extra, since -make skips all the rest of
       the arguments. */
    if (haltAfterwards) {
	add_args("-s", "erlang", "halt", NULL);
    }
    
    if (isdistributed && !no_epmd)
	start_epmd(epmd_prog);

#if (! defined(__WIN32__)) && defined(DEBUG)
    if (start_detached && get_env("ERL_CONSOLE_MODE")) {
	/* Start the emulator within an xterm.
	 * Move up all arguments and insert
	 * "xterm -e " first.
	 * The path must be searched for this 
	 * to work, i.e execvp() must be used. 
	 */
	ensure_EargsSz(EargsCnt+2);
	for (i = EargsCnt; i > 0; i--)
	    Eargsp[i+1] = Eargsp[i-1]; /* Two args to insert */
	EargsCnt += 2; /* Two args to insert */
	Eargsp[0] = emu = "xterm";
	Eargsp[1] = "-e";
    }    
#endif
    
    add_Eargs("--");
    add_Eargs("-root");
    add_Eargs(rootdir);
    add_Eargs("-progname");
    add_Eargs(progname);
    add_Eargs("--");
    ensure_EargsSz(EargsCnt + argsCnt + 1);
    for (i = 0; i < argsCnt; i++)
	Eargsp[EargsCnt++] = argsp[i];
    Eargsp[EargsCnt] = NULL;
    
    if (print_qouted_cmd_exit) {
	printf("\"%s\" ", emu);
	for (i = 1; i < EargsCnt; i++)
	    printf("\"%s\" ", Eargsp[i]);
	printf("\n");
	exit(0);
    }

    if (print_args_exit) {
	for (i = 1; i < EargsCnt; i++)
	    printf("%s\n", Eargsp[i]);
	exit(0);
    }

    if (verbose) {
	printf("Executing: %s", emu);
	for (i = 0; i < EargsCnt; i++)
	    printf(" %s", Eargsp[i]);
	printf("\n\n");
    }

#ifdef __WIN32__

    if (EargsSz != EargsCnt + 1)
	Eargsp = (char **) erealloc((void *) Eargsp, (EargsCnt + 1) * 
				    sizeof(char *));
    efree((void *) argsp);

 skip_arg_massage:
    /*DebugBreak();*/

    if (run_werl) {
	if (start_detached) {
	    char *p;
	    /* transform werl to erl */
	    p = start_emulator_program+strlen(start_emulator_program);
	    while (--p >= start_emulator_program && *p != '/' && *p != '\\' &&
		   *p != 'W' && *p != 'w')
		;
	    if (p >= start_emulator_program && (*p == 'W' || *p == 'w') &&
		(p[1] == 'E' || p[1] == 'e') && (p[2] == 'R' || p[2] == 'r') &&
		(p[3] == 'L' || p[3] == 'l')) {
		memmove(p,p+1,strlen(p));
	    }
	}
      return start_win_emulator(emu, start_emulator_program, Eargsp, start_detached);
    } else {
      return start_emulator(emu, start_emulator_program, Eargsp, start_detached);
    }

#else

 skip_arg_massage:
    if (start_detached) {
	int status = fork();
	if (status != 0)	/* Parent */
	    return 0;

	if (reset_cerl_detached)
	    putenv("CERL_DETACHED_PROG=");

	/* Detach from controlling terminal */
#ifdef HAVE_SETSID
	setsid();
#elif defined(TIOCNOTTY)
	{
	  int fd = open("/dev/tty", O_RDWR);
	  if (fd >= 0) {
	    ioctl(fd, TIOCNOTTY, NULL);
	    close(fd);
	  }
	}
#endif

	status = fork();
	if (status != 0)	/* Parent */
	    return 0;

	/*
	 * Grandchild.
	 */
	close(0);
	open("/dev/null", O_RDONLY);
	close(1);
	open("/dev/null", O_WRONLY);
	close(2);
	open("/dev/null", O_WRONLY);
#ifdef DEBUG
	execvp(emu, Eargsp); /* "xterm ..." needs to search the path */
#endif
    } 
#ifdef DEBUG
    else
#endif
    {
	execv(emu, Eargsp);
    }
    if (errno == ENOENT) {
        error("The emulator \'%s\' does not exist.", emu);
    } else {
        error("Error %d executing \'%s\'.", errno, emu);
    }
    return 1;
#endif
}


static void
usage_aux(void)
{
  fprintf(stderr,
	  "Usage: erl [-version] [-sname NAME | -name NAME] "
	  "[-noshell] [-noinput] [-env VAR VALUE] [-compile file ...] "
#ifdef __WIN32__
	  "[-start_erl [datafile]] "
#endif
	  "[-make] [-man [manopts] MANPAGE] [-x] [-emu_args] [-start_epmd BOOLEAN] "
	  "[-args_file FILENAME] [+A THREADS] [+a SIZE] [+B[c|d|i]] [+c [BOOLEAN]] "
	  "[+C MODE] [+h HEAP_SIZE_OPTION] [+K BOOLEAN] "
	  "[+l] [+M<SUBSWITCH> <ARGUMENT>] [+P MAX_PROCS] [+Q MAX_PORTS] "
	  "[+R COMPAT_REL] "
	  "[+r] [+rg READER_GROUPS_LIMIT] [+s SCHEDULER_OPTION] "
	  "[+S NO_SCHEDULERS:NO_SCHEDULERS_ONLINE] "
	  "[+SP PERCENTAGE_SCHEDULERS:PERCENTAGE_SCHEDULERS_ONLINE] "
	  "[+T LEVEL] [+V] [+v] "
	  "[+W<i|w|e>] [+z MISC_OPTION] [args ...]\n");
  exit(1);
}

void
usage(const char *switchname)
{
    fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
    usage_aux();
}

static void
usage_notsup(const char *switchname, const char *alt)
{
    fprintf(stderr, "Argument \'%s\' not supported.%s\n", switchname, alt);
    usage_aux();
}

static void
usage_format(char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    usage_aux();
}

void
start_epmd(char *epmd)
{
    char  epmd_cmd[MAXPATHLEN+100];
#ifdef __WIN32__
    char* arg1 = NULL;
#endif
    int   result;

    if (!epmd) {
	epmd = epmd_cmd;
#ifdef __WIN32__
	erts_snprintf(epmd_cmd, sizeof(epmd_cmd), "%s" DIRSEP "epmd", bindir);
	arg1 = "-daemon";
#else
	erts_snprintf(epmd_cmd, sizeof(epmd_cmd), "\"%s" DIRSEP "epmd\" -daemon", bindir);
#endif
    } 
#ifdef __WIN32__
    if (arg1 != NULL) {
	strcat(epmd, " ");
	strcat(epmd, arg1);
    }
    {
	wchar_t wcepmd[MAXPATHLEN+100];
	STARTUPINFOW start;
	PROCESS_INFORMATION pi;
	memset(&start, 0, sizeof (start));
	start.cb = sizeof (start);
	MultiByteToWideChar(CP_UTF8, 0, epmd, -1, wcepmd, MAXPATHLEN+100);

	if (!CreateProcessW(NULL, wcepmd, NULL, NULL, FALSE, 
			       CREATE_DEFAULT_ERROR_MODE | DETACHED_PROCESS,
			       NULL, NULL, &start, &pi))
	    result = -1;
	else
	    result = 0;
    }
#else
    result = system(epmd);
#endif
    if (result == -1) {
      fprintf(stderr, "Error spawning %s (error %d)\n", epmd_cmd,errno);
      exit(1);
    }
}

static void
add_arg(char *new_arg)
{
    if (argsCnt >= argsSz)
	argsp = (char **) erealloc((void *) argsp,
				   sizeof(char *) * (argsSz += 20));
    argsp[argsCnt++] = QUOTE(new_arg);
}

static void
add_args(char *first_arg, ...)
{
    va_list ap;
    char* arg;
    
    add_arg(first_arg);
    va_start(ap, first_arg);
    while ((arg = va_arg(ap, char *)) != NULL) {
	add_arg(arg);
    }
    va_end(ap);
}

static void
ensure_EargsSz(int sz)
{
    if (EargsSz < sz)
	Eargsp = (char **) erealloc((void *) Eargsp,
				    sizeof(char *) * (EargsSz = sz));
}

static void
add_Eargs(char *new_arg)
{
    if (EargsCnt >= EargsSz)
	Eargsp = (char **) erealloc((void *) Eargsp,
				    sizeof(char *) * (EargsSz += 20));
    Eargsp[EargsCnt++] = QUOTE(new_arg);
}

#if !defined(__WIN32__)
void error(char* format, ...)
{
    char sbuf[1024];
    va_list ap;

    va_start(ap, format);
    erts_vsnprintf(sbuf, sizeof(sbuf), format, ap);
    va_end(ap);
    fprintf(stderr, "erlexec: %s\n", sbuf);
    exit(1);
}
#endif

static void *
emalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
	error("Insufficient memory");
    return p;
}

static void *
erealloc(void *p, size_t size)
{
    void *res = realloc(p, size);
    if (res == NULL)
	error("Insufficient memory");
    return res;
}

static void
efree(void *p) 
{
    free(p);
}

static int
is_one_of_strings(char *str, char *strs[])
{
    int i, j;
    for (i = 0; strs[i]; i++) {
	for (j = 0; str[j] && strs[i][j] && str[j] == strs[i][j]; j++);
	if (!str[j] && !strs[i][j])
	    return 1;
    }
    return 0;
}

static char *write_str(char *to, const char *from)
{
    while (*from)
	*(to++) = *(from++);
    *to = '\0';
    return to;
}

char*
strsave(char* string)
{
    char* p = emalloc(strlen(string)+1);
    strcpy(p, string);
    return p;
}


#if defined(__WIN32__)

static void get_start_erl_data(char *file)
{
    static char* a_config_script;
    int fp;
    char tmpbuffer[512];
    char start_erl_data[512];
    int bytesread;
    char* env;
    char* reldir;
    char* otpstring;
    char* tprogname;
    if (boot_script) 
	error("Conflicting -start_erl and -boot options");
    if (config_scripts)
	error("Conflicting -start_erl and -config options");
    env = get_env("RELDIR");
    if (env)
	reldir = strsave(env);
    else {
	erts_snprintf(tmpbuffer, sizeof(tmpbuffer), "%s/releases", rootdir);
	reldir = strsave(tmpbuffer);
    }
    free_env_val(env);
    if (file == NULL)
       erts_snprintf(start_erl_data, sizeof(start_erl_data), "%s/start_erl.data", reldir);
    else
       erts_snprintf(start_erl_data, sizeof(start_erl_data), "%s", file);
    fp = _open(start_erl_data, _O_RDONLY );
    if( fp == -1 )
	error( "open failed on %s",start_erl_data );
    else {
	if( ( bytesread = _read( fp, tmpbuffer, 512 ) ) <= 0 )
	    error( "Problem reading file %s", start_erl_data );
	else {
	    tmpbuffer[bytesread]='\0';
	    if ((otpstring = strchr(tmpbuffer,' ')) != NULL) {
		*otpstring = '\0';
		otpstring++;
		
/*
 *   otpstring is the otpversion
 *   tmpbuffer is the emuversion
*/
	    }
	}
    }
    tprogname = otpstring;
    while (*tprogname) {
	if (*tprogname <= ' ') {
	    *tprogname='\0';
	    break;
	}
	tprogname++;
    }
	
    bindir = emalloc(512);
    erts_snprintf(bindir,512,"%s/erts-%s/bin",rootdir,tmpbuffer);
    /* BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin */
    tprogname = progname;
    progname = emalloc(strlen(tprogname) + 20);
    erts_snprintf(progname,strlen(tprogname) + 20,"%s -start_erl",tprogname);

    boot_script = emalloc(512);
    a_config_script = emalloc(512);
    erts_snprintf(boot_script, 512, "%s/%s/start", reldir, otpstring);
    erts_snprintf(a_config_script, 512, "%s/%s/sys", reldir, otpstring);
    config_scripts = &a_config_script;
    config_script_cnt = 1;
       
    got_start_erl = 1;
}


static wchar_t *replace_filename(wchar_t *path, wchar_t *new_base) 
{
    int plen = wcslen(path);
    wchar_t *res = (wchar_t *) emalloc((plen+wcslen(new_base)+1)*sizeof(wchar_t));
    wchar_t *p;

    wcscpy(res,path);
    for (p = res+plen-1 ;p >= res && *p != L'\\'; --p)
        ;
    *(p+1) =L'\0';
    wcscat(res,new_base);
    return res;
}

static char *path_massage(wchar_t *long_path)
{
     char *p;
     int len;
     len = WideCharToMultiByte(CP_UTF8, 0, long_path, -1, NULL, 0, NULL, NULL);
     p = emalloc(len*sizeof(char));
     WideCharToMultiByte(CP_UTF8, 0, long_path, -1, p, len, NULL, NULL);
     return p;
}
    
static char *do_lookup_in_section(InitSection *inis, char *name, 
				  char *section, wchar_t *filename, int is_path)
{
    char *p = lookup_init_entry(inis, name);

    if (p == NULL) {
	error("Could not find key %s in section %s of file %S",
	      name,section,filename);
    }

    return strsave(p);
}

// Setup bindir, rootdir and progname as utf8 buffers
static void get_parameters(int argc, char** argv)
{
    wchar_t *p;
    wchar_t buffer[MAX_PATH];
    wchar_t *ini_filename;
    HANDLE module = GetModuleHandle(NULL); /* This might look strange, but we want the erl.ini 
					      that resides in the same dir as erl.exe, not 
					      an erl.ini in our directory */
    InitFile *inif;
    InitSection *inis;

    if (module == NULL) {
        error("Cannot GetModuleHandle()");
    }

    if (GetModuleFileNameW(module,buffer,MAX_PATH) == 0) {
        error("Could not GetModuleFileName");
    }

    ini_filename = replace_filename(buffer,INI_FILENAME);

    if ((inif = load_init_file(ini_filename)) == NULL) {
	wchar_t wbindir[MAX_PATH];
	wchar_t wrootdir[MAX_PATH];

	/* Assume that the path is absolute and that
	   it does not contain any symbolic link */

	/* Determine bindir */
	if (GetEnvironmentVariableW(L"ERLEXEC_DIR", buffer, MAX_PATH) == 0) {
	    wcscpy(buffer, ini_filename);
	    for (p = buffer+wcslen(buffer)-1; p >= buffer && *p != L'\\'; --p)
		;
	    *p = L'\0';
	}
	bindir = path_massage(buffer);

	/* Determine rootdir */
	for (p = buffer+wcslen(buffer)-1; p >= buffer && *p != L'\\'; --p)
	    ;
	p--;
	for (;p >= buffer && *p != L'\\'; --p)
	    ;
	*p =L'\0';
	rootdir = path_massage(buffer);

	/* Hardcoded progname */
	progname = strsave(DEFAULT_PROGNAME);
    } else {
	if ((inis = lookup_init_section(inif,INI_SECTION)) == NULL) {
	    error("Could not find section %s in init file %s",
		  INI_SECTION, ini_filename);
	}

	bindir = do_lookup_in_section(inis, "Bindir", INI_SECTION, ini_filename,1);
	rootdir = do_lookup_in_section(inis, "Rootdir", INI_SECTION, 
				       ini_filename,1);
	progname = do_lookup_in_section(inis, "Progname", INI_SECTION, 
					ini_filename,0);
	free_init_file(inif);
    }

    emu = EMULATOR_EXECUTABLE;
    start_emulator_program = strsave(argv[0]);

    free(ini_filename);
}

static void
get_home(void)
{
    wchar_t *profile;
    char* homedrive;
    char* homepath;

    homedrive = get_env("HOMEDRIVE");
    homepath = get_env("HOMEPATH");
    if (!homedrive || !homepath) {
        if (SHGetKnownFolderPath(&FOLDERID_Profile, 0, NULL, &profile) == S_OK) {
            home = utf16_to_utf8(profile);
            /* CoTaskMemFree(profile); */
	} else
	    error("HOMEDRIVE or HOMEPATH is not set and GetWindowsDir failed");
    } else {
	home = emalloc(strlen(homedrive)+strlen(homepath)+1);
	strcpy(home, homedrive);
	strcat(home, homepath);
    }
    free_env_val(homedrive);
    free_env_val(homepath);
}

#else

static void
get_parameters(int argc, char** argv)
{
    progname = get_env("PROGNAME");
    if (!progname) {
	progname = strsave(DEFAULT_PROGNAME);
    }	

    emu = get_env("EMU");
    if (!emu) {
	emu = strsave(EMULATOR_EXECUTABLE);
    }	

    bindir = get_env("BINDIR");
    if (!bindir) {
	/* Determine bindir from absolute path to executable */
	char *p;
	char buffer[PATH_MAX];
	strncpy(buffer, argv[0], sizeof(buffer));
	buffer[sizeof(buffer)-1] = '\0';
	
	for (p = buffer+strlen(buffer)-1 ; p >= buffer && *p != '/'; --p)
	    ;
	*p ='\0';
	bindir = strsave(buffer);
    }

    rootdir = get_env("ROOTDIR");
    if (!rootdir) {
	/* Determine rootdir from absolute path to bindir */
	char *p;
	char buffer[PATH_MAX];
	strncpy(buffer, bindir, sizeof(buffer));
	buffer[sizeof(buffer)-1] = '\0';
	
	for (p = buffer+strlen(buffer)-1; p >= buffer && *p != '/'; --p)
	    ;
	p--;
	for (; p >= buffer && *p != '/'; --p)
	    ;
	*p ='\0';
	rootdir = strsave(buffer);
    }

    if (!progname || !emu || !rootdir || !bindir) {
	error("PROGNAME, EMU, ROOTDIR and BINDIR  must be set");
    }
}

static void
get_home(void)
{
    home = get_env("HOME");
    if (home == NULL)
	error("HOME must be set");
}

#endif

static void add_epmd_port(void)
{
    char* port = get_env("ERL_EPMD_PORT");
    if (port != NULL) {
	add_args("-epmd_port", port, NULL);	
    }
}

static char **build_args_from_env(char *env_var)
{
    char *value = get_env(env_var);
    char **res = build_args_from_string(value);
    free_env_val(value);
    return res;
}

static char **build_args_from_string(char *string)
{
    int argc = 0;
    char **argv = NULL;
    int alloced = 0;
    char **cur_s = NULL;	/* Initialized to avoid warning. */
    int s_alloced = 0;
    int s_pos = 0;
    char *p = string;
    enum {Start, Build, Build0, BuildSQuoted, BuildDQuoted, AcceptNext} state;

#define ENSURE()					\
    if (s_pos >= s_alloced) {			        \
	if (!*cur_s) {					\
	    *cur_s = emalloc(s_alloced = 20);		\
	} else {					\
	    *cur_s = erealloc(*cur_s, s_alloced += 20);	\
	}						\
    }


    if (!p)
	return NULL;
    argv = emalloc(sizeof(char *) * (alloced = 10));
    state = Start;
    for(;;) {
	switch (state) {
	case Start:
	    if (!*p)
		goto done;
	    if (argc >= alloced - 2) { /* Make room for extra NULL and "--" */
		argv = erealloc(argv, (alloced += 10) * sizeof(char *));
	    }
	    cur_s = argc + argv;
	    *cur_s = NULL;
	    s_pos = 0;
	    s_alloced = 0;
	    state = Build0;
	    break;
	case Build0:
	    switch (*p) {
	    case ' ':
		++p;
		break;
	    case '\0':
		state = Start;
		break;
	    default:
		state = Build;
		break;
	    }
	    break;
	case Build:
	    switch (*p) {
	    case ' ':
	    case '\0':
		ENSURE();
		(*cur_s)[s_pos] = '\0';
		++argc;
		state = Start;
		break;
	    case '"':
		++p;
		state = BuildDQuoted;
		break;
	    case '\'':
		++p;
		state = BuildSQuoted;
		break;
	    case '\\':
		++p;
		state = AcceptNext;
		break;
	    default:
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
		break;
	    }
	    break;
	case BuildDQuoted:
	    switch (*p) {
	    case '"':
		++p;
		/* fall through */
	    case '\0':
		state = Build;
		break;
	    default:
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
		break;
	    }
	    break;
	case BuildSQuoted:
	    switch (*p) {
	    case '\'':
		++p;
		/* fall through */
	    case '\0':
		state = Build;
		break;
	    default:
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
		break;
	    }
	    break;
	case AcceptNext:
	    if (!*p) {
		state = Build;
	    } else {
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
	    }
	    state = Build;
	    break;
	}
    }
done:
    if (!argc) {
	efree(argv);
	return NULL;
    }
    argv[argc++] = "--"; /* Add a -- separator in order
                            for different from different environments
                            to effect each other */
    argv[argc++] = NULL; /* Sure to be large enough */
    return argv;
#undef ENSURE
}

static char *
errno_string(void)
{
    char *str = strerror(errno);
    if (!str)
	return "unknown error";
    return str;
}

static char **
read_args_file(char *filename)
{
    int c, aix = 0, quote = 0, cmnt = 0, asize = 0;
    char **res, *astr = NULL;
    FILE *file;

#undef EAF_CMNT
#undef EAF_QUOTE
#undef SAVE_CHAR

#define EAF_CMNT	(1 << 8)
#define EAF_QUOTE	(1 << 9)
#define SAVE_CHAR(C)						\
    do {							\
	if (!astr)						\
	    astr = emalloc(sizeof(char)*(asize = 20));		\
	if (aix == asize)					\
	    astr = erealloc(astr, sizeof(char)*(asize += 20));	\
	if (' ' != (char) (C))					\
	    astr[aix++] = (char) (C);				\
	else if (aix > 0 && astr[aix-1] != ' ')			\
	    astr[aix++] = ' ';					\
   } while (0)

    do {
	errno = 0;
	file = fopen(filename, "r");
    } while (!file && errno == EINTR);
    if (!file) {
	usage_format("Failed to open arguments file \"%s\": %s\n",
		     filename,
		     errno_string());
    }

    while (1) {
	c = getc(file);
	if (c == EOF) {
	    if (ferror(file)) {
		if (errno == EINTR) {
		    clearerr(file);
		    continue;
		}
		usage_format("Failed to read arguments file \"%s\": %s\n",
			     filename,
			     errno_string());
	    }
	    break;
	}

	switch (quote | cmnt | c) {
	case '\\':
	    quote = EAF_QUOTE;
	    break;
	case '#':
	    cmnt = EAF_CMNT;
	    break;
	case EAF_CMNT|'\n':
	    cmnt = 0;
	    /* Fall through... */
	case '\n':
	case '\f':
	case '\r':
	case '\t':
	case '\v':
	    if (!quote)
		c = ' ';
	    /* Fall through... */
	default:
	    if (!cmnt)
		SAVE_CHAR(c);
	    quote = 0;
	    break;
	}
    }

    SAVE_CHAR('\0');

    fclose(file);

    if (astr[0] == '\0')
	res = NULL;
    else
	res = build_args_from_string(astr);

    efree(astr);

    return res;

#undef EAF_CMNT
#undef EAF_QUOTE
#undef SAVE_CHAR
}


typedef struct {
    char **argv;
    int argc;
    int size;
} argv_buf;

static void
trim_argv_buf(argv_buf *abp)
{
    abp->argv = erealloc(abp->argv, sizeof(char *)*(abp->size = abp->argc));
}

static void
save_arg(argv_buf *abp, char *arg)
{
    if (abp->size <= abp->argc) {
	if (!abp->argv)
	    abp->argv = emalloc(sizeof(char *)*(abp->size = 100));
	else
	    abp->argv = erealloc(abp->argv, sizeof(char *)*(abp->size += 100));
    }
    abp->argv[abp->argc++] = arg;
}

#define DEF_ARGV_STACK_SIZE 10
#define ARGV_STACK_SIZE_INCR 50

typedef struct {
    char **argv;
    int ix;
} argv_stack_element;

typedef struct {
    int top_ix;
    int size;
    argv_stack_element *base;
    argv_stack_element def_buf[DEF_ARGV_STACK_SIZE];
} argv_stack;

#define ARGV_STACK_INIT(S)		\
do {					\
    (S)->top_ix = 0;			\
    (S)->size = DEF_ARGV_STACK_SIZE;	\
    (S)->base = &(S)->def_buf[0];	\
} while (0)

static void
push_argv(argv_stack *stck, char **argv, int ix)
{
    if (stck->top_ix == stck->size) {
	if (stck->base != &stck->def_buf[0]) {
	    stck->size += ARGV_STACK_SIZE_INCR;
	    stck->base = erealloc(stck->base,
				  sizeof(argv_stack_element)*stck->size);
	}
	else {
	    argv_stack_element *base;
	    base = emalloc(sizeof(argv_stack_element)
			   *(stck->size + ARGV_STACK_SIZE_INCR));
	    memcpy((void *) base,
		   (void *) stck->base,
		   sizeof(argv_stack_element)*stck->size);
	    stck->base = base;
	    stck->size += ARGV_STACK_SIZE_INCR;
	}
    }
    stck->base[stck->top_ix].argv = argv;
    stck->base[stck->top_ix++].ix = ix;
}

static void
pop_argv(argv_stack *stck, char ***argvp, int *ixp)
{
    if (stck->top_ix == 0) {
	*argvp = NULL;
	*ixp = 0;
    }
    else {
	*argvp = stck->base[--stck->top_ix].argv;
	*ixp = stck->base[stck->top_ix].ix;
	if (stck->top_ix == 0 && stck->base != &stck->def_buf[0]) {
	    efree(stck->base);
	    stck->base = &stck->def_buf[0];
	    stck->size = DEF_ARGV_STACK_SIZE;
	}
    }
}

static void
get_file_args(char *filename, argv_buf *abp, argv_buf *xabp)
{
    argv_stack stck;
    int i;
    char **argv;

    ARGV_STACK_INIT(&stck);

    i = 0;
    argv = read_args_file(filename);
    
    while (argv) {
	
	while (argv[i]) {
	    if (strcmp(argv[i], "-args_file") == 0) {
		char **new_argv;
		char *fname;
		if (!argv[++i])
		    usage("-args_file");
		fname = argv[i++];
		new_argv = read_args_file(fname);
		if (new_argv) {
		    if (argv[i])
			push_argv(&stck, argv, i);
		    else
			efree(argv);
		    i = 0;
		    argv = new_argv;
		}
	    }
	    else {
		if (strcmp(argv[i], "-extra") == 0) {
		    i++;
		    while (argv[i])
			save_arg(xabp, argv[i++]);
		    break;
		}
		save_arg(abp, argv[i++]);
	    }
	}

	efree(argv);

	pop_argv(&stck, &argv, &i);
    }
}

static void
initial_argv_massage(int *argc, char ***argv)
{
    argv_buf ab = {0}, xab = {0};
    int ix, vix, ac;
    char **av;
    char *sep = "--";
    struct {
	int argc;
	char **argv;
    } avv[] = {{INT_MAX, NULL}, {INT_MAX, NULL}, {INT_MAX, NULL},
	       {INT_MAX, NULL}, {INT_MAX, NULL},
               {INT_MAX, NULL}, {INT_MAX, NULL}};
    /*
     * The environment flag containing OTP release is intentionally
     * undocumented and intended for OTP internal use only.
     */

    vix = 0;

    av = build_args_from_env("ERL_OTP" OTP_SYSTEM_VERSION "_FLAGS");
    if (av)
	avv[vix++].argv = av;

    av = build_args_from_env("ERL_AFLAGS");
    if (av)
	avv[vix++].argv = av;

    /* command line */
    if (*argc > 1) {
	avv[vix].argc = *argc - 1;
	avv[vix++].argv = &(*argv)[1];
        avv[vix].argc = 1;
        avv[vix++].argv = &sep;
    }

    av = build_args_from_env("ERL_FLAGS");
    if (av)
	avv[vix++].argv = av;

    av = build_args_from_env("ERL_ZFLAGS");
    if (av)
	avv[vix++].argv = av;

    if (vix == (*argc > 1 ? 1 : 0)) {
	/* Only command line argv; check if we can use argv as it is... */
	ac = *argc;
	av = *argv;
	for (ix = 1; ix < ac; ix++) {
	    if (strcmp(av[ix], "-args_file") == 0) {
		/* ... no; we need to expand arguments from
		   file into argument list */
		goto build_new_argv;
	    }
	    if (strcmp(av[ix], "-extra") == 0) {
		break;
	    }
	}

	/* ... yes; we can use argv as it is. */
	return;
    }

 build_new_argv:

    save_arg(&ab, (*argv)[0]);
    
    vix = 0;
    while (avv[vix].argv) {
	ac = avv[vix].argc;
	av = avv[vix].argv;

	ix = 0;
	while (ix < ac && av[ix]) {
	    if (strcmp(av[ix], "-args_file") == 0) {
		if (++ix == ac)
		    usage("-args_file");
		get_file_args(av[ix++], &ab, &xab);
	    }
	    else {
		if (strcmp(av[ix], "-extra") == 0) {
		    ix++;
		    while (ix < ac && av[ix])
			save_arg(&xab, av[ix++]);
		    break;
		}
		save_arg(&ab, av[ix++]);
	    }
	}

	vix++;
    }

    vix = 0;
    while (avv[vix].argv) {
	if (avv[vix].argc == INT_MAX) /* not command line */
	    efree(avv[vix].argv);
	vix++;
    }

    if (xab.argc) {
	save_arg(&ab, "-extra");
	for (ix = 0; ix < xab.argc; ix++)
	    save_arg(&ab, xab.argv[ix]);
	efree(xab.argv);
    }

    save_arg(&ab, NULL);
    trim_argv_buf(&ab);
    *argv = ab.argv;
    *argc = ab.argc - 1;
}

#ifdef __WIN32__
static char*
possibly_quote(char* arg)
{
    int mustQuote = NO;
    int n = 0;
    char* s;
    char* narg;

    /*
     * Scan the string to find out if it needs quoting and return
     * the original argument if not.
     */

    for (s = arg; *s; s++, n++) {
	if (*s == ' ' || *s == '"') {
	    mustQuote = YES;
	    n++;
	}
    }
    if (!mustQuote) {
	return arg;
    }

    /*
     * Insert the quotes and put a backslash in front of every quote
     * inside the string.
     */

    s = narg = emalloc(n+2+1);
    for (*s++ = '"'; *arg; arg++, s++) {
	if (*s == '"') {
	    *s++ = '\\';
	}
	*s = *arg;
    }
    *s++ = '"';
    *s = '\0';
    return narg;
}

/*
 * Unicode helpers to handle environment and command line parameters on
 * Windows. We internally handle all environment variables in UTF8,
 * but put and get the environment using the WCHAR (limited UTF16) interface
 * 
 * These are simplified to only handle Unicode characters that can fit in 
 * Windows simplified UTF16, i.e. characters that fit in 16 bits.
 */

static int utf8_len(unsigned char first) 
{
    if ((first & ((unsigned char) 0x80)) == 0) {
	return 1;
    } else if ((first & ((unsigned char) 0xE0)) == 0xC0) {
	return 2;
    } else if ((first & ((unsigned char) 0xF0)) == 0xE0) {
	return 3;
    } else if ((first & ((unsigned char) 0xF8)) == 0xF0) {
	return 4;
    } 
    return 1; /* will be a '?' */
}

static WCHAR *utf8_to_utf16(unsigned char *bytes)
{
    unsigned int unipoint;
    unsigned char *tmp = bytes;
    WCHAR *target, *res;
    int num = 0;
    
    while (*tmp) {
	num++;
	tmp += utf8_len(*tmp);
    }
    res = target = emalloc((num + 1) * sizeof(WCHAR));
    while (*bytes) {
	if (((*bytes) & ((unsigned char) 0x80)) == 0) {
	    unipoint = (unsigned int) *bytes;
	    ++bytes;
	} else if (((*bytes) & ((unsigned char) 0xE0)) == 0xC0) {
	    unipoint = 
		(((unsigned int) ((*bytes) & ((unsigned char) 0x1F))) << 6) |
		((unsigned int) (bytes[1] & ((unsigned char) 0x3F)));
	    bytes += 2;
	} else if (((*bytes) & ((unsigned char) 0xF0)) == 0xE0) {
	    unipoint = 
		(((unsigned int) ((*bytes) & ((unsigned char) 0xF))) << 12) |
		(((unsigned int) (bytes[1] & ((unsigned char) 0x3F))) << 6) |
		((unsigned int) (bytes[2] & ((unsigned char) 0x3F)));
	    if (unipoint > 0xFFFF) {
		 unipoint = (unsigned int) '?';
	    }
	    bytes +=3;
	} else if (((*bytes) & ((unsigned char) 0xF8)) == 0xF0) {
	    unipoint = (unsigned int) '?'; /* Cannot put in a wchar */
	    bytes += 4;
	} else {
	    unipoint = (unsigned int) '?';
	}
	*target++ = (WCHAR) unipoint;
    }
    *target = L'\0';
    return res;
}

static int put_utf8(WCHAR ch, unsigned char *target, int sz, int *pos)
{
    unsigned int x = (unsigned int) ch;
    if (x < 0x80) {
    if (*pos >= sz) {
	return -1;
    }
	target[(*pos)++] = (unsigned char) x;
    }
    else if (x < 0x800) {
	if (((*pos) + 1) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((unsigned char) (x >> 6)) | 
			    ((unsigned char) 0xC0));
	target[(*pos)++] = (((unsigned char) (x & 0x3F)) | 
			    ((unsigned char) 0x80));
    } else {
	if ((x >= 0xD800 && x <= 0xDFFF) ||
	    (x == 0xFFFE) ||
	    (x == 0xFFFF)) { /* Invalid unicode range */
	    return -1;
	}
	if (((*pos) + 2) >= sz) {
	    return -1;
	}

	target[(*pos)++] = (((unsigned char) (x >> 12)) | 
			    ((unsigned char) 0xE0));
	target[(*pos)++] = ((((unsigned char) (x >> 6)) & 0x3F)  | 
			    ((unsigned char) 0x80));
	target[(*pos)++] = (((unsigned char) (x & 0x3F)) | 
			    ((unsigned char) 0x80));
    }
    return 0;
}

static int need_bytes_for_utf8(WCHAR x)
{
    if (x < 0x80)
	return 1;
    else if (x < 0x800)
	return 2;
    else 
	return 3;
}

static WCHAR *latin1_to_utf16(char *str)
{
    int len = strlen(str);
    int i;
    WCHAR *wstr = emalloc((len+1) * sizeof(WCHAR));
    for(i=0;i<len;++i)
	wstr[i] = (WCHAR) str[i];
    wstr[len] = L'\0';
    return wstr;
}

static char *utf16_to_utf8(WCHAR *wstr) 
{
    int len = wcslen(wstr);
    char *result;
    int i,pos;
    int reslen = 0;
    for(i=0;i<len;++i) {
	reslen += need_bytes_for_utf8(wstr[i]);
    }
    result = emalloc(reslen+1);
    pos = 0;
    for(i=0;i<len;++i) {
	if (put_utf8((int) wstr[i], result, reslen, &pos) < 0) {
	    break;
	}
    }
    result[pos] = '\0';
    return result;
}
    
#endif
