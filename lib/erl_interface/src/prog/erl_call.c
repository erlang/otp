/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 * 
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
 *

 */
/*
 * Description: This file implements the erl_call command line
 * utility. The erl_call command can be used to:
 *
 * * Execute code on an Erlang node and get the result back
 * * Start and stop Erlang nodes
 * * Upload and compile a module on an Erlang node
 *
 * See the erl_call man page or HTML documentation for additional
 * information.
 *
 */

/* An exception from using eidef.h, use config.h directly */
#include "config.h"

#ifdef __WIN32__
#include <winsock2.h>
#include <direct.h>
#include <windows.h>
#include <winbase.h>

#else /* unix */

#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include <time.h>
#if HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif

#endif

#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>

/* In VC++, noreturn is a declspec that has to be before the types,
 * but in GNUC it is an attribute to be placed between return type
 * and function name, hence __decl_noreturn <types> __noreturn <function name>
 *
 * at some platforms (e.g. Android) __noreturn is defined at sys/cdef.h
 */
#if __GNUC__
#  define __decl_noreturn
#  ifndef __noreturn
#     define __noreturn __attribute__((noreturn))
#  endif
#else
#  if defined(__WIN32__) && defined(_MSC_VER)
#    define __noreturn
#    define __decl_noreturn __declspec(noreturn)
#  else
#    define __noreturn
#    define __decl_noreturn
#  endif
#endif

#include "ei.h"
#include "ei_resolve.h"

#define ERL_START_MSG "gurka" /* make something up */
#define ERL_START_TIME 10000   /* wait this long (ms) */
#define ERL_START_LOGFILE ".erl_start.out" /* basename of logfile */

/* flags used by erl_connect and erl_xconnect */
#define ERL_START_ENODE   0x0001
#define ERL_START_EPMD    0x0002
#define ERL_START_LONG    0x0004
#define ERL_START_COOKIE  0x0008
#define ERL_START_DEBUG   0x0010
#define ERL_START_VERBOSE 0x0020
#define ERL_START_REMOTE  0x0040

/* error return values */
#define ERL_S_TIMEOUT    -51  /* a timeout occurred */
#define ERL_BADARG     -52  /* an argument contained an incorrect value */
#define ERL_SYS_ERROR  -99  /* a system error occurred (check errno) */

struct call_flags {
    int startp;
    int cookiep;
    int modp;
    int evalp;
    int randomp;
    int dynamic_name;
    int use_long_name;	/* indicates if -name was used, else -sname or -n */
    int use_localhost_fallback;
    int debugp;
    int verbosep;
    int haltp;
    int fetch_stdout;
    int print_result_term;
    long port;
    char *hostname;
    char *cookie;
    char *node;
    char *hidden;
    char *apply;
    char *script;
};

/* start an erlang system */
int erl_start_sys(ei_cnode *ec, char *alive, Erl_IpAddr addr, int flags,
		  char *erl, char *add_args[]);
__decl_noreturn static void __noreturn usage_arg(const char *progname, const char *switchname);
__decl_noreturn static void __noreturn usage_error(const char *progname, const char *switchname);
__decl_noreturn static void __noreturn usage(const char *progname);
static int get_module(char **mbuf, char **mname);
static int do_connect(ei_cnode *ec, char *nodename, struct call_flags *flags);
static int read_stdin(char **buf);
static void split_apply_string(char *str, char **mod, 
			       char **fun, char **args);
static void* ei_chk_malloc(size_t size);
static void* ei_chk_calloc(size_t nmemb, size_t size);
static void* ei_chk_realloc(void *old, size_t size);
static char* ei_chk_strdup(char *s);
static int rpc_print_node_stdout(ei_cnode* ec, int fd, char *mod,
                                 char *fun, const char* inbuf,
                                 int inbuflen, ei_x_buff* x);
__decl_noreturn static void __noreturn exit_free_flags_fields(
                int exit_status,
                struct call_flags* flags);

/* Converts the given hostname to a shortname, if required. */
static void format_node_hostname(const struct call_flags *flags,
                                 const char *hostname,
                                 char dst[EI_MAXHOSTNAMELEN + 1])
{
    char *ct;

    strncpy(dst, hostname, EI_MAXHOSTNAMELEN);
    dst[EI_MAXHOSTNAMELEN] = '\0';

    /* If shortnames, cut off the name at first '.' */
    if (flags->use_long_name == 0 && (ct = strchr(dst, '.'))) {
        *ct = '\0';
    }
}

static void start_timeout(int timeout);

/***************************************************************************
 *
 *  XXXXX
 *
 ***************************************************************************/

int main(int argc, char *argv[])
{
    int i = 1,fd,creation;
    struct hostent *hp;
    char host_name[EI_MAXHOSTNAMELEN+1];
    char nodename[MAXNODELEN+1];
    char *p = NULL;
    int modsize = 0;
    char *host = NULL;
    char *module = NULL;
    char *modname = NULL;
    struct call_flags flags = {0}; /* Default 0 and NULL in all fields */
    char* progname = argv[0];
    ei_cnode ec;
    flags.port = -1;
    flags.hostname = NULL;
    flags.fetch_stdout = 0;
    flags.print_result_term = 1;
    flags.script = NULL;
    flags.hidden = NULL;
    flags.apply = NULL;
    flags.cookie = NULL;
    flags.node = NULL;

    ei_init();

    /* Get the command line options */
    while (i < argc) {
	if (argv[i][0] != '-') {
	    usage_error(progname, argv[i]);
	}

	if (strcmp(argv[i], "-sname") == 0) { /* -sname NAME */
	    if (i+1 >= argc) {
		usage_arg(progname, "-sname ");
	    }
            if (flags.node != NULL) {
                free(flags.node);
            }
	    flags.node = ei_chk_strdup(argv[i+1]);
	    i++;
	    flags.use_long_name = 0;
	} else if (strcmp(argv[i], "-name") == 0) {  /* -name NAME */
	    if (i+1 >= argc) {
		usage_arg(progname, "-name ");
	    }
            if (flags.node != NULL) {
                free(flags.node);
            }
	    flags.node = ei_chk_strdup(argv[i+1]);
	    i++;
	    flags.use_long_name = 1;
	} else if (strcmp(argv[i], "-address") == 0) {  /* -address [HOST:]PORT */
	    if (i+1 >= argc) {
		usage_arg(progname, "-address ");
	    }
            {
                char* hostname_port_arg = ei_chk_strdup(argv[i+1]);
                char* address_string_end = strchr(hostname_port_arg, ':');
                if (address_string_end == NULL) {
                    flags.port = strtol(hostname_port_arg, NULL, 10);
                    free(hostname_port_arg);
                    hostname_port_arg = NULL;
                } else {
                    flags.port = strtol(address_string_end + 1, NULL, 10);
                    /* Remove port part from hostname_port_arg*/
                    *address_string_end = '\0';
                    if (strlen(hostname_port_arg) > 0) {
                        flags.hostname = hostname_port_arg;
                    } else {
                        free(hostname_port_arg);
                        hostname_port_arg = NULL;
                    }
                }

                if (flags.port < 1 || flags.port > 65535) {
                    if (hostname_port_arg != NULL) {
                        free(hostname_port_arg);
                    }
                    usage_error(progname, "-address");
                }
                i++;
            }
        } else if (strcmp(argv[i], "-timeout") == 0) {
            long timeout;

            if (i+1 >= argc) {
                usage_arg(progname, "-timeout ");
            }

            timeout = strtol(argv[i+1], NULL, 10);
            if (timeout <= 0 || timeout >= (1 << 20)) {
                usage_error(progname, "-timeout");
            }

            start_timeout(timeout);
            i++;
        } else if (strcmp(argv[i], "-fetch_stdout") == 0) {
            flags.fetch_stdout = 1;
        } else if (strcmp(argv[i], "-no_result_term") == 0) {
            flags.print_result_term = 0;
        } else if (strcmp(argv[i], "-__uh_test__") == 0) {
            /* Fakes a failure in the call to ei_gethostbyname(h_hostname) so
             * we can test the localhost fallback. */
            flags.use_localhost_fallback = 1;
	} else {
	    if (strlen(argv[i]) != 2) {
		usage_error(progname, argv[i]);
	    }
		    
	    switch (argv[i][1]) {
	    case 's':
		flags.startp = 1;
		break;
	    case 'q':
		flags.haltp = 1;
		break;
	    case 'v':
		flags.verbosep = 1;
		break;
	    case 'd':
		flags.debugp = 1;
		break;
	    case 'r':
		flags.randomp = 1;
		break;
            case 'R':
                flags.dynamic_name = 1;
                break;
	    case 'e':
		flags.evalp = 1;
		break;
	    case 'm':
		flags.modp = 1;
		break;
	    case 'c':
		if (i+1 >= argc) {
		    usage_arg(progname, "-c ");
		}
		flags.cookiep = 1;
                if (flags.cookie != NULL) {
                    free(flags.cookie);
                }
		flags.cookie = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case 'n':
		if (i+1 >= argc) {
		    usage_arg(progname, "-n ");
		}
                if (flags.node != NULL) {
                    free(flags.node);
                }
		flags.node = ei_chk_strdup(argv[i+1]);
		flags.use_long_name = 1;
		i++;
		break;
	    case 'h':
		if (i+1 >= argc) {
		    usage_arg(progname, "-h ");
		}
                if (flags.hidden != NULL) {
                    free(flags.hidden);
                }
		flags.hidden = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case 'x':
		if (i+1 >= argc) {
		    usage_arg(progname, "-x ");
		}
                if (flags.script != NULL) {
                    free(flags.script);
                }
		flags.script = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case 'a':
		if (i+1 >= argc) {
		    usage_arg(progname, "-a ");
		}
                if (flags.apply != NULL) {
                    free(flags.apply);
                }
		flags.apply = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case '?':
		usage(progname);
	    default:
		usage_error(progname, argv[i]);
	    }
	}
	i++;

    } /* while */

    /*
     * Can't have them both !
     */
    if ((flags.modp && flags.evalp) ||
        (flags.port != -1 && flags.startp) ||
        (flags.port != -1 && flags.node)) {
      usage(progname);
    }

    /*
     * Read an Erlang module from stdin.
     */
    if (flags.modp) {
      modsize = get_module(&module, &modname);
    }

    if (flags.verbosep || flags.debugp) {
	fprintf(stderr,"erl_call: "
		"node = %s\nCookie = %s\n"
		"flags = %s %s %s\n"
		"module: name = %s , size = %d\n"
		"apply = %s\n",
		(flags.node ? flags.node : ""),
		(flags.cookie ? flags.cookie : ""),
		(flags.startp ? "startp" : ""),
		(flags.verbosep ? "verbosep" : ""),
		(flags.debugp ? "debugp" : ""),
		(modname ? modname : ""), modsize,
		(flags.apply ? flags.apply : "" ));
    }

    /* 
     * What we, at least, requires !
     */
    if (flags.node == NULL && flags.port == -1) {
	usage(progname);
    }

    if (!flags.cookiep) {
	flags.cookie = NULL;
    }

    creation = time(NULL) + 1; /* "random" */

    if (flags.hidden == NULL && !flags.dynamic_name) {
      /* As default we are c17@gethostname */
      i = flags.randomp ? (time(NULL) % 997) : 17;
      flags.hidden = (char *) ei_chk_malloc(10 + 2 ); /* c17 or cXYZ */
      sprintf(flags.hidden, "c%d",
	  i < 0 ?  (int) getpid() : i);
    }
    {
      /* A name for our hidden node was specified */
      char h_hostname[EI_MAXHOSTNAMELEN+1];
      char h_nodename_buf[MAXNODELEN+1];
      char *h_nodename = h_nodename_buf;
      char *h_alivename = flags.hidden;
      struct in_addr h_ipadr;

      /* gethostname requires len to be max(hostname) + 1 */
      if (gethostname(h_hostname, EI_MAXHOSTNAMELEN+1) < 0) {
          fprintf(stderr,"erl_call: failed to get host name: %d\n", errno);
          exit_free_flags_fields(1, &flags);
      }

      if (flags.use_localhost_fallback || (hp = ei_gethostbyname(h_hostname)) == 0) {
          /* Failed to resolve our own hostname; try binding to loopback and
           * hope for the best. */
          hp = ei_gethostbyname("localhost");
          flags.use_localhost_fallback = 1;

          format_node_hostname(&flags, h_hostname, h_hostname);
      } else {
          format_node_hostname(&flags, hp->h_name, h_hostname);
      }

      memcpy(&h_ipadr.s_addr, *hp->h_addr_list, sizeof(struct in_addr));
      if (h_alivename) {
          if (snprintf(h_nodename_buf, sizeof(h_nodename_buf), "%s@%s",
                       h_alivename, h_hostname) > sizeof(h_nodename_buf)) {;
              fprintf(stderr,"erl_call: hostname too long: %s\n", h_hostname);
              exit_free_flags_fields(1, &flags);
          }
      }
      else {
          /* dynamic node name */
          h_nodename = NULL;
      }
      
      if (ei_connect_xinit(&ec, h_hostname, h_alivename, h_nodename,
			   (Erl_IpAddr)&h_ipadr, flags.cookie, 
			   (short) creation) < 0) {
	  fprintf(stderr,"erl_call: can't create C node %s; %d\n",
		  h_nodename, erl_errno);
          exit_free_flags_fields(1, &flags);
      }

    }
    if (flags.port != -1 && flags.hostname != NULL) {
        host = flags.hostname;
        strcpy(host_name, flags.hostname);
    } else if ((flags.port != -1 && flags.hostname == NULL) ||
        (strchr((const char *)flags.node, (int) '@') == 0)) {
	strcpy(host_name, ei_thishostname(&ec));
	host = host_name;
    } else {
        p = strchr((const char *)flags.node, (int) '@');
	*p = 0;
	host = p+1;
    }

    if (flags.use_localhost_fallback && strcmp(host, ei_thishostname(&ec)) == 0) {
        /* We're on the same host *and* have used the localhost fallback, so we
         * skip canonical name resolution since it's bound to fail.
         *
         * `ei_connect` will do the right thing later on. */
        strcpy(host_name, ei_thishostname(&ec));
    } else {
        if ((hp = ei_gethostbyname(host)) == 0) {
            fprintf(stderr,"erl_call: can't ei_gethostbyname(%s)\n", host);
            exit_free_flags_fields(1, &flags);
        }

        format_node_hostname(&flags, hp->h_name, host_name);
    }

    if (flags.port == -1) {
        if (snprintf(nodename, sizeof(nodename),
                     "%s@%s", flags.node, host_name) > sizeof(nodename)) {
            fprintf(stderr,"erl_call: nodename too long: %s\n", flags.node);
            exit_free_flags_fields(1, &flags);
        }
    }
    /* 
     * Try to connect. Start an Erlang system if the
     * start option is on and no system is running.
     */
    if (flags.startp && !flags.haltp) {
	fd = do_connect(&ec, nodename, &flags);
    } else if (flags.port == -1) {
        if ((fd = ei_connect(&ec, nodename)) < 0) {
            /* We failed to connect ourself */
            /* FIXME do we really know we failed because of node not up? */
            if (flags.haltp) {
                exit_free_flags_fields(0, &flags);
            } else {
                fprintf(stderr,"erl_call: failed to connect to node %s\n",
                        nodename);
                exit_free_flags_fields(1, &flags);
            }
        }
    } else {
        /* Connect using address:port */
        if ((fd = ei_connect_host_port(&ec, host, (int)flags.port)) < 0) {
            /* We failed to connect ourself */
            /* FIXME do we really know we failed because of node not up? */
            if (flags.haltp) {
                exit_free_flags_fields(0, &flags);
            } else {
                fprintf(stderr,"erl_call: failed to connect to node with address \"%s:%ld\"\n",
                        flags.hostname == NULL ? "" : flags.hostname,
                        flags.port);
                exit_free_flags_fields(1, &flags);
            }
        }
    }

    /* If we are connected and the halt switch is set */
    if (fd && flags.haltp) {
	int i = 0;
	char *p;
	ei_x_buff reply;

	ei_encode_empty_list(NULL, &i);

	p = (char *)ei_chk_malloc(i);
	i = 0;		/* Reset */
	  
	ei_encode_empty_list(p, &i);

	ei_x_new_with_version(&reply);

	/* FIXME if fails we want to exit != 0 ? */
	ei_rpc(&ec, fd, "erlang", "halt", p, i, &reply);
	free(p);
	ei_x_free(&reply);
        exit_free_flags_fields(0, &flags);
    }

    if (flags.verbosep) {
        if (flags.port == -1) {
            fprintf(stderr,"erl_call: we are now connected to node \"%s\"\n",
                    nodename);
        } else {
            fprintf(stderr,"erl_call: we are now connected to node with address \"%s:%ld\"\n",
                    flags.hostname == NULL ? "": flags.hostname,
                    flags.port);
        }
    }

    /*
     * Compile the module read from stdin.
     */
    if (flags.modp && (modname != NULL)) {
      char fname[256];

      if (strlen(modname) + 4 + 1 > sizeof(fname)) {
      fprintf(stderr,"erl_call: module name too long: %s\n", modname);
      exit_free_flags_fields(1, &flags);
      }
      strcpy(fname, modname);
      strcat(fname, ".erl");

      /*
       * ei_format("[~s,~w]", fname, erl_mk_binary(module, modsize));
       */

      {
	  int i = 0;
	  char *p;
	  ei_x_buff reply;

	  ei_encode_list_header(NULL, &i, 2);
	  ei_encode_string(NULL, &i, fname);
	  ei_encode_binary(NULL, &i, module, modsize);
	  ei_encode_empty_list(NULL, &i);

	  p = (char *)ei_chk_malloc(i);
	  i = 0;		/* Reset */
	  
	  ei_encode_list_header(p, &i, 2);
	  ei_encode_string(p, &i, fname);
	  ei_encode_binary(p, &i, module, modsize);
	  ei_encode_empty_list(p, &i);

	  ei_x_new_with_version(&reply);

	  if (ei_rpc(&ec, fd, "file", "write_file", p, i, &reply) < 0) {
	      free(p);
	      ei_x_free(&reply);
	      fprintf(stderr,"erl_call: can't write to source file %s\n",
		      fname);
              exit_free_flags_fields(1, &flags);
	  }
	  free(p);
	  ei_x_free(&reply);
      }
     
      /* Compile AND load file on other node */

      {
	  int i = 0;
	  char *p;
	  ei_x_buff reply;

	  ei_encode_list_header(NULL, &i, 2);
	  ei_encode_atom(NULL, &i, fname);
	  ei_encode_empty_list(NULL, &i);
	  ei_encode_empty_list(NULL, &i);

	  p = (char *)ei_chk_malloc(i);
	  i = 0;		/* Reset */
	  
	  ei_encode_list_header(p, &i, 2);
	  ei_encode_atom(p, &i, fname);
	  ei_encode_empty_list(p, &i);
	  ei_encode_empty_list(p, &i);

	  ei_x_new_with_version(&reply);

	  /* erl_format("[~a,[]]", modname) */

	  if (ei_rpc(&ec, fd, "c", "c", p, i, &reply) < 0) {
	      free(p);
	      ei_x_free(&reply);
	      fprintf(stderr,"erl_call: can't compile file %s\n", fname);
	  }
	  free(p);
	  /* FIXME complete this code
	     FIXME print out error message as term
	  if (!erl_match(erl_format("{ok,_}"), reply)) {
	      fprintf(stderr,"erl_call: compiler errors\n");
	  }
	  */
	  ei_x_free(&reply);
      }
     
    }

    /*
     * If we loaded any module source code, we can free the buffer
     * now. This buffer was allocated in read_stdin().
     */
    if (module != NULL) {
        free(module);
    }

    /*
     * Eval the Erlang functions read from stdin/
     */
    if (flags.evalp) {
      char *evalbuf;
      int len;

      len = read_stdin(&evalbuf);
      {
	  int i = 0;
          int rpc_res;
          char *p;
	  ei_x_buff reply;

	  ei_encode_list_header(NULL, &i, 1);
	  ei_encode_binary(NULL, &i, evalbuf, len);
	  ei_encode_empty_list(NULL, &i);

	  p = (char *)ei_chk_malloc(i);
	  i = 0;		/* Reset */
	  
	  ei_encode_list_header(p, &i, 1);
	  ei_encode_binary(p, &i, evalbuf, len);
	  ei_encode_empty_list(p, &i);

	  ei_x_new_with_version(&reply);
	  /* erl_format("[~w]", erl_mk_binary(evalbuf,len))) */

          if (flags.fetch_stdout) {
              rpc_res = rpc_print_node_stdout(&ec, fd, "erl_eval", "eval_str", p, i, &reply);
          } else {
              rpc_res = ei_rpc(&ec, fd, "erl_eval", "eval_str", p, i, &reply);
          }

	  if (rpc_res < 0) {
	      fprintf(stderr,"erl_call: evaluating input failed: %s\n",
		      evalbuf);
	      free(p);
	      free(evalbuf);	/* Allocated in read_stdin() */
	      ei_x_free(&reply);
              exit_free_flags_fields(1, &flags);
	  }
          if (flags.print_result_term) {
              i = 0;
              ei_print_term(stdout,reply.buff,&i);
          }
	  free(p);
	  free(evalbuf);	/* Allocated in read_stdin() */
	  ei_x_free(&reply);
      }
    }
    /*
     * Any Erlang call to be made ?
     */
    if (flags.apply != NULL) {
      char *mod,*fun,*args;
      ei_x_buff e, reply;
      int rpc_res;
      split_apply_string(flags.apply, &mod, &fun, &args);
      if (flags.verbosep) {
	  fprintf(stderr,"erl_call: module = %s, function = %s, args = %s\n",
		  mod, fun, args);
      }

      ei_x_new(&e);		/* No version to ei_rpc() */
      
      if (ei_x_format_wo_ver(&e, args) < 0) {
          fprintf(stderr, "erl_call: Failed to parse arguments,\n"
                  "see the documentation for allowed term types.\n"
                  "Arguments: %s\n", args);
          free(mod);
          free(fun);
          free(args);
          exit_free_flags_fields(-1, &flags);
      }
      free(args);
      ei_x_new_with_version(&reply);

      if (flags.fetch_stdout) {
          rpc_res = rpc_print_node_stdout(&ec, fd, mod, fun, e.buff, e.index, &reply);
      } else {
          rpc_res = ei_rpc(&ec, fd, mod, fun, e.buff, e.index, &reply);
      }
      if (rpc_res < 0) {
	  /* FIXME no error message and why -1 ? */
	  ei_x_free(&e);
	  ei_x_free(&reply);
          free(mod);
          free(fun);
          exit_free_flags_fields(-1, &flags);
      } else {
          if (flags.print_result_term) {
              int i = 0;
              ei_print_term(stdout,reply.buff,&i);
          }
	  ei_x_free(&e);
	  ei_x_free(&reply);
      }
      free(mod);
      free(fun);
    }
    exit_free_flags_fields(0, &flags);
    return(0);
}


/***************************************************************************
 *
 *  XXXXX
 *
 ***************************************************************************/


/* 
 * This function does only return on success.
 */
static int do_connect(ei_cnode *ec, char *nodename, struct call_flags *flags)
{
    int fd;
    int start_flags;
    int r;

    start_flags = ERL_START_ENODE |
	(flags->use_long_name? ERL_START_LONG : 0) |
	(flags->verbosep? ERL_START_VERBOSE : 0) |
	(flags->debugp? ERL_START_DEBUG : 0);

    if ((fd = ei_connect(ec, nodename)) >= 0) {
	/* success */
	if (flags->verbosep) {
	    fprintf(stderr,"erl_call: now connected to node %s\n", nodename);
	}
    } else {
	char alive[EI_MAXALIVELEN+1];
	char *hostname;
	struct hostent *h;
	char *cookieargs[3];
	char **args;

	cookieargs[0] = "-setcookie";
	cookieargs[1] = flags->cookie;
	cookieargs[2] = NULL;

	args = (flags->cookie) ? cookieargs : NULL;

	if (!(hostname = strrchr(nodename,'@'))) {
	    return ERL_BADARG;
	}
	strncpy(alive,nodename,hostname-nodename);
	alive[hostname-nodename] = 0x0;
	hostname++;

	h = ei_gethostbyname(hostname);


	if ((r=erl_start_sys(ec,alive,(Erl_IpAddr)(h->h_addr_list[0]),
			     start_flags,flags->script,args)) < 0) {
	    fprintf(stderr,"erl_call: unable to start node, error = %d\n", r);
            exit_free_flags_fields(1, flags);
	}

	if ((fd=ei_connect(ec, nodename)) >= 0) {
	    /* success */
	    if (flags->verbosep) {
		fprintf(stderr,"erl_call: now connected to node \"%s\"\n",
			nodename);
	    }
	} else {
	    /* (failure) */
	    switch (fd) {
	    case ERL_NO_DAEMON:
		fprintf(stderr,"erl_call: no epmd running\n");
                exit_free_flags_fields(1, flags);
	    case ERL_CONNECT_FAIL:
		fprintf(stderr,"erl_call: connect failed\n");
                exit_free_flags_fields(1, flags);
	    case ERL_NO_PORT:
		fprintf(stderr,"erl_call: node is not running\n");
                exit_free_flags_fields(1, flags);
	    case ERL_TIMEOUT:
		fprintf(stderr,"erl_call: connect timed out\n");
                exit_free_flags_fields(1, flags);
	    default:
		fprintf(stderr,"erl_call: error during connect\n");
                exit_free_flags_fields(1, flags);
	    }
	}
    }

    return fd;
} /* do_connect */

#define SKIP_SPACE(s) while(isspace((int)*(s))) (s)++
#define EAT(s) while (!isspace((int)*(s)) && (*(s) != '\0')) (s)++

static void split_apply_string(char *str, 
			       char **mod, 
			       char **fun, 
			       char **args)
{
  char *begin=str;
  char *start="start";
  char *empty_list="[]";
  int len;

  SKIP_SPACE(str);
  if (*str == '\0') {
      fprintf(stderr,"erl_call: wrong format of apply string (1)\n");
      exit(1);
  }

  EAT(str);
  len = str-begin;
  *mod = (char *) ei_chk_calloc(len + 1, sizeof(char));
  memcpy(*mod, begin, len);

  SKIP_SPACE(str);
  if (*str == '\0') {
    *fun = ei_chk_strdup(start);
    *args = ei_chk_strdup(empty_list);
    return;
  }
  begin = str;
  EAT(str);
  len = str-begin;
  *fun = (char *) ei_chk_calloc(len + 1, sizeof(char));
  memcpy(*fun, begin, len);

  SKIP_SPACE(str);
  if (*str == '\0') {
    *args = ei_chk_strdup(empty_list);
    return;
  }

  *args = ei_chk_strdup(str);
  
  return;

} /* split_apply_string */


/* 
 * Read from stdin until EOF is reached.
 * Allocate the buffer needed.
 */
static int read_stdin(char **buf)
{
    int bsize = BUFSIZ;
    int len = 0;
    int i;
    char *tmp = (char *) ei_chk_malloc(bsize);

    while (1) {
	if ((i = read(0, &tmp[len], bsize-len)) < 0) {
	    fprintf(stderr,"erl_call: can't read stdin, errno = %d", errno);
	    exit(1);
	} else if (i == 0) {
	    break;
	} else {
	    len += i;
	    if ((len+50) > bsize) {
		bsize = len * 2;
		tmp = (char *) ei_chk_realloc(tmp, bsize);
	    } else {
		continue;
	    }
	}
    } /* while */
    *buf = tmp;
    return len;

} /* read_stdin */

/*
 * Get the module from stdin.
 */
static int get_module(char **mbuf, char **mname)
{
  char *tmp;
  int len,i;

  len = read_stdin(mbuf);
  /*
   * Now, get the module name.
   */
  if ((tmp = strstr(*mbuf, "-module(")) != NULL) {
    char *start;
    tmp += strlen("-module(");
    while ((*tmp) == ' ') tmp++; /* eat space */
    start = tmp;
    while (1) {
      if (isalnum((int)*tmp) || (*tmp == '_')) {
	tmp++;
	continue;
      } else {
	  break;
      }
    } /* while */
    i = tmp - start;
    *mname = (char *) ei_chk_calloc(i+1, sizeof(char));
    memcpy(*mname, start, i);
  }

  return len;

} /* get_module */

#ifdef __WIN32__
static DWORD WINAPI timer_thread(void *data) {
    DWORD_PTR timeout = (DWORD_PTR)data * 1000;

    Sleep(timeout);
    exit(1);
}

static void start_timeout(int timeout) {
    if (CreateThread(NULL, 0, timer_thread, (void*)(DWORD_PTR)timeout, 0, NULL) == NULL) {
        fprintf(stderr,"erl_call: Failed to start timer thread\n");
        exit(1);
    }
}
#else
static void start_timeout(int timeout) {
    alarm(timeout);
}
#endif

/***************************************************************************
 *
 *  Different error reporting functions that output usage 
 *
 ***************************************************************************/

static void usage_noexit(const char *progname) {
  fprintf(stderr,"\nUsage: %s [-[demqrsv]] [-c Cookie] [-h HiddenName] \n", progname);
  fprintf(stderr,"            [-x ErlScript] [-a [Mod [Fun [Args]]]] [-timeout Secs]\n");
  fprintf(stderr,"            (-n Node | -sname Node | -name Node | -address [HOSTNAME:]PORT)\n\n");
#ifdef __WIN32__
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a \"erlang length [[a,b,c]]\"\n");
#else
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a 'erlang length [[a,b,c]]'\n");
#endif
  fprintf(stderr,"         -c  cookie string; by default read from ~/.erlang.cookie\n");
  fprintf(stderr,"         -d  direct Erlang output to ~/.erl_call.out.<Nodename>\n");
  fprintf(stderr,"         -e  evaluate contents of standard input (e.g., echo \"X=1,Y=2,{X,Y}.\"|%s -e ...)\n",
          progname);
  fprintf(stderr,
          "         -fetch_stdout\n"
          "           execute the code, specified with the -a or -e option, in a new\n"
          "           process that has a group leader that forwards all stdout (standard\n"
          "           output) data so that it is printed to stdout of the\n"
          "           %s process. See the %s man page for additional information.\n",
          progname, progname);
  fprintf(stderr,"         -h  specify a name for the erl_call client node\n");
  fprintf(stderr,"         -m  read and compile Erlang module from stdin\n");
  fprintf(stderr,"         -n  name of Erlang node, same as -name\n");
  fprintf(stderr,"         -name  name of Erlang node, expanded to a fully qualified\n");
  fprintf(stderr,"         -sname name of Erlang node, short form will be used\n");
  fprintf(stderr,"         -address [HOSTNAME:]PORT of Erlang node\n"
          "                  (the default hostname is the hostname of the local manchine)\n"
          "                  (e.g., %s -address my_host:36303 ...)\n"
          "                  (cannot be combinated with -s, -n, -name and -sname)\n",
          progname);
  fprintf(stderr,"         -no_result_term  do not print the result term\n");
  fprintf(stderr,"         -timeout  command timeout, in seconds\n");
  fprintf(stderr,"         -q  halt the Erlang node (overrides the -s switch)\n");
  fprintf(stderr,"         -r  use a random name for the erl_call client node\n");
  fprintf(stderr,"         -s  start a new Erlang node if necessary\n");
  fprintf(stderr,"         -v  verbose mode, i.e print some information on stderr\n");
  fprintf(stderr,"         -x  use specified erl start script, default is erl\n");
}

__decl_noreturn static void __noreturn usage_arg(const char *progname, const char *switchname) {
  fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
  usage_noexit(progname);
  exit(1);
}

__decl_noreturn static void __noreturn usage_error(const char *progname, const char *switchname) {
  fprintf(stderr, "Illegal argument \'%s\'.\n", switchname);
  usage_noexit(progname);
  exit(1);
}

void __noreturn usage(const char *progname) {
  usage_noexit(progname);
  exit(0);
}

/***************************************************************************
 *
 *  Utility functions
 *
 ***************************************************************************/

static void* ei_chk_malloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL) {
        fprintf(stderr,"erl_call: insufficient memory\n");
        exit(1);
    }
    return p;
}

static void* ei_chk_calloc(size_t nmemb, size_t size)
{
    void *p = calloc(nmemb, size);
    if (p == NULL) {
        fprintf(stderr,"erl_call: insufficient memory\n");
        exit(1);
    }
    return p;
}

static void* ei_chk_realloc(void *old, size_t size)
{
    void *p = realloc(old, size);
    if (!p) {
        fprintf(stderr, "erl_call: cannot reallocate %u bytes of memory from %p\n",
                (unsigned) size, old);
        exit (1);
    }
    return p;
}

static char* ei_chk_strdup(char *s)
{
    char *p = strdup(s);
    if (p == NULL) {
        fprintf(stderr,"erl_call: insufficient memory\n");
        exit(1);
    }
    return p;
}

/*
 * Helper function that:
 *
 * 1. Executes a function on a remote node
 *
 * 2. Forwards what the executed function and its subprocesses prints
 *    to stdout of this process
 *
 * 3. Returns the result of the executed function (the result term is
 *    written to the buffer pointed to by x)
 *
 * This function is similar to (and is based on) the function ei_rpc
 */
static int rpc_print_node_stdout(ei_cnode* ec, int fd, char *mod,
                                 char *fun, const char* inbuf,
                                 int inbuflen, ei_x_buff* x)
{
    int i, index;
    int got_rex_response = 0;
    int initial_buff_index = x->index;
    ei_term t;
    erlang_msg msg;
    char rex[MAXATOMLEN];
 
    if (ei_xrpc_to(ec, fd, mod, fun, inbuf, inbuflen, EI_RPC_FETCH_STDOUT) < 0) {
	return ERL_ERROR;
    }

    while (!got_rex_response) {
        /* ei_rpc_from() responds with a tick if it gets one... */
        while ((i = ei_rpc_from(ec, fd, ERL_NO_TIMEOUT, &msg, x)) == ERL_TICK)
            ;

        if (i == ERL_ERROR) return i;

        index = 0;
        if (ei_decode_version(x->buff, &index, &i) < 0)
            goto ebadmsg;

        if (ei_decode_ei_term(x->buff, &index, &t) < 0)
            goto ebadmsg;

        if (t.ei_type != ERL_SMALL_TUPLE_EXT && t.ei_type != ERL_LARGE_TUPLE_EXT)
            goto ebadmsg;

        if (t.arity != 2)
            goto ebadmsg;

        if (ei_decode_atom(x->buff, &index, rex) < 0)
            goto ebadmsg;

        if (strcmp("rex_stdout", rex) == 0) {
            int type;
            int size;
            char* binary_buff;
            long actual_size;
            ei_get_type(x->buff, &index, &type, &size);
            if(type != ERL_BINARY_EXT) {
                goto ebadmsg;
            }
            binary_buff = ei_chk_malloc(size + 1);
            ei_decode_binary(x->buff, &index, binary_buff, &actual_size);
            binary_buff[size] = '\0';
            printf("%s", binary_buff);
            free(binary_buff);
            /* Reset the buffer as we need to read more and we have no
               use for what we have already read */
            x->index = initial_buff_index;
        } else {
            if(strcmp("rex", rex) != 0)
                goto ebadmsg;
            got_rex_response = 1;
        }
    }
    /* remove header */
    x->index -= index;
    memmove(x->buff, &x->buff[index], x->index);
    return 0;

ebadmsg:

    return ERL_ERROR;
}


__decl_noreturn static void __noreturn
exit_free_flags_fields(int exit_status, struct call_flags* flags) {
    if (flags->script != NULL) {
        free(flags->script);
    }
    if (flags->hidden != NULL) {
        free(flags->hidden);
    }
    if (flags->cookie != NULL) {
        free(flags->cookie);
    }
    if (flags->apply != NULL) {
        free(flags->apply);
    }
    if (flags->hostname != NULL) {
        free(flags->hostname);
    }
    exit(exit_status);
}


/* Constants and helper functions used by erl_start_sys */

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifndef RSH
#define RSH "/usr/bin/ssh"
#endif

#ifndef HAVE_SOCKLEN_T
typedef int SocklenType;
#else
typedef socklen_t SocklenType;
#endif

/* FIXME check errors from malloc */

static struct in_addr *get_addr(const char *hostname, struct in_addr *oaddr);

static int wait_for_erlang(int sockd, int magic, struct timeval *timeout);
#if defined(__WIN32__)
static int unique_id(void);
static HANDLE spawn_erlang_epmd(ei_cnode *ec,
				       char *alive,
				       Erl_IpAddr adr,
				       int flags,
				       char *erl_or_epmd,
				       char *args[],
				       int port,
				       int is_erlang);
#else
static int exec_erlang(ei_cnode *ec, char *alive, Erl_IpAddr adr, int flags,
		       char *erl, char *args[],int port);
#endif

#if defined(__WIN32__)
#define DEF_ERL_COMMAND "erl"
#define DEF_EPMD_COMMAND "epmd"
#define ERL_REPLY_FMT   "-s erl_reply reply \"%s\" \"%d\" \"%d\""
#define ERL_NAME_FMT    "-noinput -name %s"
#define ERL_SNAME_FMT   "-noinput -sname %s"

#define IP_ADDR_CHARS 15
#define FORMATTED_INT_LEN 10

static int unique_id(void){
    return (int) GetCurrentThreadId();
}

static int enquote_args(char **oargs, char ***qargs){
    char **args;
    int len;
    int i;
    int qwhole;
    int extra;
    char *ptr;
    char *ptr2;

    if(oargs == NULL){
	*qargs = malloc(sizeof(char *));
	**qargs = NULL;
	return 0;
    };

    for(len=0;oargs[len] != NULL; ++len)
	;
    args = malloc(sizeof(char *) * (len + 1));

    for(i = 0; i < len; ++i){
	qwhole = strchr(oargs[i],' ') != NULL;
	extra = qwhole * 2;
	for(ptr = oargs[i]; *ptr != '\0'; ++ptr)
	    extra += (*ptr == '"');
	args[i] = malloc(strlen(oargs[i]) +
			     extra +
			     1);
	ptr2 = args[i];
	if(qwhole)
	    *(ptr2++) = '"';
	for(ptr = oargs[i]; *ptr != '\0'; ++ptr){
	    if(*ptr == '"')
		*(ptr2++) = '\\';
	    *(ptr2++) = *ptr;
	}
	if(qwhole)
	    *(ptr2++) = '"';
	*ptr2 = '\0';
    }
    args[len] = NULL;
    *qargs = args;
    return len;
}

static void free_args(char **args){
    char **ptr = args;
    while(*ptr != NULL)
	free(*(ptr++));
    free(args);
}

/* In NT we cannot fork(), Erlang and Epmd gets 
   spawned by this function instead. */

static HANDLE spawn_erlang_epmd(ei_cnode *ec,
				       char *alive,
				       Erl_IpAddr adr,
				       int flags,
				       char *erl_or_epmd,
				       char *args[],
				       int port,
				       int is_erlang)
{
    STARTUPINFO sinfo;
    SECURITY_ATTRIBUTES sa;
    PROCESS_INFORMATION pinfo;
    char *cmdbuf;
    int cmdlen;
    char *ptr;
    int i;
    int num_args;
    char *name_format;
    struct in_addr myaddr;
    struct in_addr *hisaddr = (struct in_addr *)adr;
    char iaddrbuf[IP_ADDR_CHARS + 1];
    HANDLE ret;

    if(is_erlang){
	get_addr(ei_thishostname(ec), &myaddr);
	if((ptr = inet_ntoa(myaddr)) == NULL)
	    return INVALID_HANDLE_VALUE;
	else
	    strcpy(iaddrbuf,ptr);
    }
    if ((flags & ERL_START_REMOTE) ||
	(is_erlang && (hisaddr->s_addr != myaddr.s_addr))) {
	return INVALID_HANDLE_VALUE;
    } else {
	num_args = enquote_args(args, &args);
	for(cmdlen = i = 0; args[i] != NULL; ++i)
	    cmdlen += strlen(args[i]) + 1;
	if(!erl_or_epmd)
	    erl_or_epmd = (is_erlang) ? DEF_ERL_COMMAND :
	    DEF_EPMD_COMMAND;
	if(is_erlang){
	    name_format = (flags & ERL_START_LONG) ? ERL_NAME_FMT :
		ERL_SNAME_FMT;
	    cmdlen +=
		strlen(erl_or_epmd) + (*erl_or_epmd != '\0') +
		strlen(name_format) + 1 + strlen(alive) +
		strlen(ERL_REPLY_FMT) + 1 + strlen(iaddrbuf) + 2 * FORMATTED_INT_LEN + 1;
	    ptr = cmdbuf = malloc(cmdlen);
	    if(*erl_or_epmd != '\0')
		ptr += sprintf(ptr,"%s ",erl_or_epmd);
	    ptr += sprintf(ptr, name_format,
			   alive);
	    ptr += sprintf(ptr, " " ERL_REPLY_FMT,
		       iaddrbuf, port, unique_id());
	} else { /* epmd */
	    cmdlen += strlen(erl_or_epmd) + (*erl_or_epmd != '\0') + 1;
	    ptr = cmdbuf = malloc(cmdlen);
	    if(*erl_or_epmd != '\0')
		ptr += sprintf(ptr,"%s ",erl_or_epmd);
	    else
		*(ptr++) = '\0';
	}
	for(i= 0; args[i] != NULL; ++i){
	    *(ptr++) = ' ';
	    strcpy(ptr,args[i]);
	    ptr += strlen(args[i]);
	}
	free_args(args);
	if (flags & ERL_START_VERBOSE) {
	    fprintf(stderr,"erl_call: commands are %s\n",cmdbuf);
	}
	/* OK, one single command line... */
	/* Hmmm, hidden or unhidden window??? */
	memset(&sinfo,0,sizeof(sinfo));
	sinfo.cb = sizeof(STARTUPINFO); 
	sinfo.dwFlags = STARTF_USESHOWWINDOW /*| 
	    STARTF_USESTDHANDLES*/;
	sinfo.wShowWindow = SW_HIDE; /* Hidden! */
	sinfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	sinfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	sinfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = /*TRUE*/ FALSE;
	if(!CreateProcess(
			  NULL,
			  cmdbuf,
			  &sa,
			  NULL,
			  /*TRUE*/ FALSE,
			  0 | CREATE_NEW_CONSOLE,
			  NULL,
			  NULL,
			  &sinfo,
			  &pinfo))
	    ret = INVALID_HANDLE_VALUE;
	else
	    ret = pinfo.hProcess;
	free(cmdbuf);
	return ret;
    }
    /* NOTREACHED */
}
#else /* Unix */

/* call this from the child process to start an erlang system. This
 * function just builds the erlang command line and then calls it.
 *
 * node - the nodename for the new node
 * flags - various options that can be set (see erl_start.h)
 * erl - name of the erlang executable, or NULL for default ("erl")
 * args - additional arguments to pass to erlang executable
 * port - the port number where we wait for acknowledgment from the enode
 *
 * we have a potential problem if args conflicts with any of the 
 * arguments we use here.
 */
static int exec_erlang(ei_cnode *ec,
		       char *alive,
		       Erl_IpAddr adr,
		       int flags,
		       char *erl,
		       char *args[],
		       int port)
{
#if !defined(__WIN32__) 
  int fd,len,l,i;
  char **s;
  char *argv[4];
  char argbuf[BUFSIZ];
  struct in_addr myaddr;
  struct in_addr *hisaddr = (struct in_addr *)adr;

  if (!get_addr(ei_thishostname(ec), &myaddr)) {
      fprintf(stderr,"erl_call: failed to find hostname\r\n");
      return ERL_SYS_ERROR;
  }

  /* on this host? */
  /* compare ip addresses, unless forced by flag setting to use rsh */
  if ((flags & ERL_START_REMOTE) || (hisaddr->s_addr != myaddr.s_addr)) {
    argv[0] = RSH;
    len = strlen(inet_ntoa(*hisaddr));
    argv[1] = malloc(len+1);
    strcpy(argv[1],inet_ntoa(*hisaddr));
  }
  else {
  /* Yes - use sh to start local Erlang */
    argv[0] = "sh";
    argv[1] = "-c";
  }
  argv[2] = argbuf;
  argv[3] = NULL;

  len = 0;
  *argbuf=(char)0;

  sprintf(argbuf,"exec %s ", (erl? erl: "erl"));
  len = strlen(argbuf);

  /* *must* be noinput or node (seems to) hang... */
  /* long or short names? */
  sprintf(&argbuf[len], "-noinput %s %s ",
	  ((flags & ERL_START_LONG) ? "-name" : "-sname"),
	  alive);
  len = strlen(argbuf);

  /* now make the new node report back when it's ready */
  /* add: myip, myport and replymsg */
  sprintf(&argbuf[len],
	  "-s erl_reply reply %s %d %d ",
	  inet_ntoa(myaddr),port,(int)getpid());
#ifdef HARD_DEBUG
  fprintf(stderr,"erl_call: debug %s\n",&argbuf[len]);
#endif
  len = strlen(argbuf);

  /* additional arguments to be passed to the other system */
  /* make sure that they will fit first */
  for (l=0, s = args; s && *s; s++) l+= strlen(*s) + 1;

  if (len + l + 1 > BUFSIZ) return ERL_BADARG;
  else {
    for (s = args; s && *s; s++) {
      strcat(argbuf," ");
      strcat(argbuf,*s);
    }
    len += l + 1;
  }

  if (flags & ERL_START_VERBOSE) {
    fprintf(stderr,"erl_call: %s %s %s\n",argv[0],argv[1],argv[2]);
  }

  /* close all descriptors in child */
  for (i=0; i<64; i++) close(i);

  /* debug output to file? */
  if (flags & ERL_START_DEBUG) {
    char debugfile[MAXPATHLEN+1];
    char *home=getenv("HOME");
    sprintf(debugfile,"%s/%s.%s",home,ERL_START_LOGFILE,alive);
    if ((fd=open(debugfile, O_WRONLY | O_CREAT | O_APPEND, 0644)) >= 0) {
      time_t t = time(NULL);
      dup2(fd,1);
      dup2(fd,2);
      fprintf(stderr,"\n\n===== Log started ======\n%s \n",ctime(&t));
      fprintf(stderr,"erl_call: %s %s %s\n",argv[0],argv[1],argv[2]);
    }
  }

  /* start the system */
  execvp(argv[0], argv);

  if (flags & ERL_START_DEBUG) {
    fprintf(stderr,"erl_call: exec failed: (%d) %s %s %s\n",
	    errno,argv[0],argv[1],argv[2]);
  }

#endif
  /* (hopefully) NOT REACHED */
  return ERL_SYS_ERROR;
} /* exec_erlang() */

#endif /* defined(WINDOWS) */

#if defined(__WIN32__)
static void gettimeofday(struct timeval *now,void *dummy){
    SYSTEMTIME systime;
	FILETIME ft;
    DWORD x;
    GetSystemTime(&systime);
    SystemTimeToFileTime(&systime,&ft);
    x = ft.dwLowDateTime / 10;
    now->tv_sec = x / 1000000;
    now->tv_usec = x % 1000000;
}

#endif


/* wait for the remote system to reply */
/*
 * sockd - an open socket where we expect a connection from the e-node
 * magic - sign on message the e-node must provide for verification
 * timeout - how long to wait before returning failure
 *
 * OBS: the socket is blocking, and there is a potential deadlock if we
 * get an accept but the peer sends no data (and does not close).
 * in normal cases the timeout will work ok however, i.e. either we
 * never get any connection, or we get connection then close().
 */
static int wait_for_erlang(int sockd, int magic, struct timeval *timeout)
{
  struct timeval to;
  struct timeval stop_time;
  struct timeval now;
  fd_set rdset;
  int fd;
  int n,i;
  char buf[16];
  struct sockaddr_in peer;
  SocklenType len = (SocklenType) sizeof(peer);

  /* determine when we should exit this function */
  gettimeofday(&now,NULL);
  stop_time.tv_sec = now.tv_sec + timeout->tv_sec;
  stop_time.tv_usec = now.tv_usec + timeout->tv_usec;
  while (stop_time.tv_usec > 1000000) {
    stop_time.tv_sec++;
    stop_time.tv_usec -= 1000000;
  }

#ifdef HARD_DEBUG
  fprintf(stderr,"erl_call: debug time is %ld.%06ld, "
	  "will timeout at %ld.%06ld\n",
	  now.tv_sec,now.tv_usec,stop_time.tv_sec,stop_time.tv_usec);
#endif

  while (1) {
    FD_ZERO(&rdset);
    FD_SET(sockd,&rdset);

    /* adjust the timeout to (stoptime - now) */
    gettimeofday(&now,NULL);
    to.tv_sec = stop_time.tv_sec - now.tv_sec;
    to.tv_usec = stop_time.tv_usec - now.tv_usec;
    while ((to.tv_usec < 0) && (to.tv_sec > 0)) {
      to.tv_usec += 1000000;
      to.tv_sec--;
    }
    if (to.tv_sec < 0) return ERL_TIMEOUT;

#ifdef HARD_DEBUG
    fprintf(stderr,"erl_call: debug remaining to timeout: %ld.%06ld\n",
	    to.tv_sec,to.tv_usec);
#endif
    switch ((i = select(sockd+1,&rdset,NULL,NULL,&to))) {
    case -1:
      return ERL_SYS_ERROR;
      break;

    case 0: /* timeout */
#ifdef HARD_DEBUG
      gettimeofday(&now,NULL);
      fprintf(stderr,"erl_call: debug timed out at %ld.%06ld\n",
	      now.tv_sec,now.tv_usec);
#endif
      return ERL_TIMEOUT;
      break;

    default: /* ready descriptors */
#ifdef HARD_DEBUG
      gettimeofday(&now,NULL);
      fprintf(stderr,"erl_call: debug got select at %ld.%06ld\n",
	      now.tv_sec,now.tv_usec);
#endif
      if (FD_ISSET(sockd,&rdset)) {
	if ((fd = accept(sockd,(struct sockaddr *)&peer,&len)) < 0)
	  return ERL_SYS_ERROR;

	/* now get sign-on message and terminate it */
#if defined(__WIN32__)
	if ((n=recv(fd,buf,16,0)) >= 0) buf[n]=0x0;
	closesocket(fd);
#else
	if ((n=read(fd,buf,16)) >= 0) buf[n]=0x0;
	close(fd);
#endif
#ifdef HARD_DEBUG
	fprintf(stderr,"erl_call: debug got %d, expected %d\n",
		atoi(buf),magic);
#endif
	if (atoi(buf) == magic) return 0; /* success */
      } /* if FD_SET */
    } /* switch */
  } /* while */

  /* unreached? */
  return ERL_SYS_ERROR;
} /* wait_for_erlang() */


static struct in_addr *get_addr(const char *hostname, struct in_addr *oaddr)
{
  struct hostent *hp;

#if !defined (__WIN32__)
  char buf[1024];
  struct hostent host;
  int herror;

  hp = ei_gethostbyname_r(hostname,&host,buf,1024,&herror);
#else
  hp = ei_gethostbyname(hostname);
#endif

  if (hp) {
    memmove(oaddr,hp->h_addr_list[0],sizeof(*oaddr));
    return oaddr;
  }
  return NULL;
}

/* Start an Erlang node. return value 0 indicates that node was
 * started successfully, negative values indicate error. 
 * 
 * node -  the name of the remote node to start (alivename@hostname).
 * flags - turn on or off certain options. See erl_start.h for a list.
 * erl -  is the name of the erl script to call. If NULL, the default
 * name "erl" will be used.
 * args - a NULL-terminated list of strings containing
 * additional arguments to be sent to the remote Erlang node. These
 * strings are simply appended to the end of the command line, so any
 * quoting of special characters, etc must be done by the caller.
 * There may be some conflicts between some of these arguments and the
 * default arguments hard-coded into this function, so be careful. 
 */
int erl_start_sys(ei_cnode *ec, char *alive, Erl_IpAddr adr, int flags,
		  char *erl, char *args[])
{
  struct timeval timeout;
  struct sockaddr_in addr;
  SocklenType namelen;
  int port;
  int sockd = 0;
  int one = 1;
#if defined(__WIN32__)
  HANDLE pid;
#else
  int pid;
#endif
  int r = 0;

  if (((sockd = socket(AF_INET, SOCK_STREAM, 0)) < 0) ||
      (setsockopt(sockd, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one)) < 0)) {
    r = ERL_SYS_ERROR;
    goto done;
  }

  memset(&addr,0,sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = 0;

  if (bind(sockd,(struct sockaddr *)&addr,sizeof(addr))<0) {
      return ERL_SYS_ERROR;
  }
  namelen = sizeof(addr);
  if (getsockname(sockd,(struct sockaddr *)&addr,&namelen)<0) {
      return ERL_SYS_ERROR;
  }
  port = ntohs(addr.sin_port);

  listen(sockd,5);

#if defined(__WIN32__)
  pid = spawn_erlang_epmd(ec,alive,adr,flags,erl,args,port,1);
  if (pid == INVALID_HANDLE_VALUE)
     return ERL_SYS_ERROR;
  timeout.tv_usec = 0;
  timeout.tv_sec = 10; /* ignoring ERL_START_TIME */
  if((r = wait_for_erlang(sockd,unique_id(),&timeout))
     == ERL_TIMEOUT) {
      /* Well, this is not a nice way to do it, and it does not 
	 always kill the emulator, but the alternatives are few.*/
      TerminateProcess(pid,1);
  }
#else /* Unix */
  switch ((pid = fork())) {
  case -1:
    r = ERL_SYS_ERROR;
    break;

  case 0:
    /* child - start the erlang node */
    exec_erlang(ec, alive, adr, flags, erl, args, port);

    /* error if reached - parent reports back to caller after timeout
       so we just exit here */
    exit(1);
    break;

  default:

    /* parent - waits for response from Erlang node */
    /* child pid used here as magic number */
    timeout.tv_usec = 0;
    timeout.tv_sec = 10; /* ignoring ERL_START_TIME */
    if ((r = wait_for_erlang(sockd,pid,&timeout)) == ERL_TIMEOUT) {
      /* kill child if no response */
      kill(pid,SIGINT);
      sleep(1);
      if (waitpid(pid,NULL,WNOHANG) != pid) {
	/* no luck - try harder */
	kill(pid,SIGKILL);
	sleep(1);
	waitpid(pid,NULL,WNOHANG);
      }
    }

  }
#endif /* defined(__WIN32__) */

done:
#if defined(__WIN32__)
  if (sockd) closesocket(sockd);
#else
  if (sockd) close(sockd);
#endif
  return r;
} /* erl_start_sys() */
