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
 *

 */
/*
 * Function: Makes it possible to send and receive Erlang
 *    messages from the (Unix) command line. 
 *    Note: We don't free any memory at all since we only
 *    live for a short while.   
 *
 */

#ifdef __WIN32__
#include <winsock2.h>
#include <direct.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS

#include <stdio.h>
#include <string.h>
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <time.h>

#else /* unix */

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/param.h> 
#include <netdb.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#endif

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>

#include "ei.h"
#include "ei_resolve.h"
#include "erl_start.h"		/* FIXME remove dependency */

/*
 * Some nice global variables
 * (I don't think "nice" is the right word actually... -gordon)
 */
/* FIXME problem for threaded ? */

struct call_flags {
    int startp;
    int cookiep;
    int modp;
    int evalp;
    int randomp;
    int use_long_name;	/* indicates if -name was used, else -sname or -n */
    int debugp;
    int verbosep;
    int haltp;
    char *cookie;
    char *node;
    char *hidden;
    char *apply;
    char *script;
};

static void usage_arg(const char *progname, const char *switchname);
static void usage_error(const char *progname, const char *switchname);
static void usage(const char *progname);
static int get_module(char **mbuf, char **mname);
static int do_connect(ei_cnode *ec, char *nodename, struct call_flags *flags);
static int read_stdin(char **buf);
static void split_apply_string(char *str, char **mod, 
			       char **fun, char **args);
static void* ei_chk_malloc(size_t size);
static void* ei_chk_calloc(size_t nmemb, size_t size);
static void* ei_chk_realloc(void *old, size_t size);
static char* ei_chk_strdup(char *s);


/***************************************************************************
 *
 *  XXXXX
 *
 ***************************************************************************/

/* FIXME isn't VxWorks to handle arguments differently? */

#if !defined(VXWORKS)
int main(int argc, char *argv[])
#else
int erl_call(int argc, char **argv)
#endif
{
    int i = 1,fd,creation;
    struct hostent *hp;
    char host_name[EI_MAXHOSTNAMELEN+1];
    char nodename[MAXNODELEN+1];
    char *p = NULL;
    char *ct = NULL; /* temporary used when truncating nodename */
    int modsize = 0;
    char *host = NULL;
    char *module = NULL;
    char *modname = NULL;
    struct call_flags flags = {0}; /* Default 0 and NULL in all fields */
    char* progname = argv[0];
    ei_cnode ec;

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

	    flags.node = ei_chk_strdup(argv[i+1]);
	    i++;
	    flags.use_long_name = 0;
	} else if (strcmp(argv[i], "-name") == 0) {  /* -name NAME */
	    if (i+1 >= argc) {
		usage_arg(progname, "-name ");
	    }

	    flags.node = ei_chk_strdup(argv[i+1]);
	    i++;
	    flags.use_long_name = 1;
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
		flags.cookie = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case 'n':
		if (i+1 >= argc) {
		    usage_arg(progname, "-n ");
		}
		flags.node = ei_chk_strdup(argv[i+1]);
		flags.use_long_name = 1;
		i++;
		break;
	    case 'h':
		if (i+1 >= argc) {
		    usage_arg(progname, "-h ");
		}
		flags.hidden = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case 'x':
		if (i+1 >= argc) {
		    usage_arg(progname, "-x ");
		}
		flags.script = ei_chk_strdup(argv[i+1]);
		i++;
		break;
	    case 'a':
		if (i+1 >= argc) {
		    usage_arg(progname, "-a ");
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
    if (flags.modp && flags.evalp) {
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
    if (flags.node == NULL) {
	usage(progname);
    }

    if (!flags.cookiep) {
	flags.cookie = NULL;
    }

    /* FIXME decide how many bits etc or leave to connect_xinit? */
    creation = (time(NULL) % 3) + 1; /* "random" */

    if (flags.hidden == NULL) {
      /* As default we are c17@gethostname */
      i = flags.randomp ? (time(NULL) % 997) : 17;
      flags.hidden = (char *) ei_chk_malloc(10 + 2 ); /* c17 or cXYZ */
#if defined(VXWORKS)
      sprintf(flags.hidden, "c%d",
	  i < 0 ?  (int) taskIdSelf() : i);
#else
      sprintf(flags.hidden, "c%d",
	  i < 0 ?  (int) getpid() : i);
#endif
    }
    {
      /* A name for our hidden node was specified */
      char h_hostname[EI_MAXHOSTNAMELEN+1];
      char h_nodename[MAXNODELEN+1];
      char *h_alivename=flags.hidden;
      struct in_addr h_ipadr;
      char* ct;

      /* gethostname requires len to be max(hostname) + 1 */
      if (gethostname(h_hostname, EI_MAXHOSTNAMELEN+1) < 0) {
	  fprintf(stderr,"erl_call: failed to get host name: %d\n", errno);
	  exit(1);
      }
      if ((hp = ei_gethostbyname(h_hostname)) == 0) {
	  fprintf(stderr,"erl_call: can't resolve hostname %s\n", h_hostname);
	  exit(1);
      }
      /* If shortnames, cut off the name at first '.' */
      if (flags.use_long_name == 0 && (ct = strchr(hp->h_name, '.')) != NULL) {
	  *ct = '\0';
      }
      strncpy(h_hostname, hp->h_name, EI_MAXHOSTNAMELEN);
      h_hostname[EI_MAXHOSTNAMELEN] = '\0';
      memcpy(&h_ipadr.s_addr, *hp->h_addr_list, sizeof(struct in_addr));
      if (strlen(h_alivename) + strlen(h_hostname) + 2 > sizeof(h_nodename)) {
	  fprintf(stderr,"erl_call: hostname too long: %s\n", h_hostname);
	  exit(1);
      }
      sprintf(h_nodename, "%s@%s", h_alivename, h_hostname);
      
      if (ei_connect_xinit(&ec, h_hostname, h_alivename, h_nodename,
			   (Erl_IpAddr)&h_ipadr, flags.cookie, 
			   (short) creation) < 0) {
	  fprintf(stderr,"erl_call: can't create C node %s; %d\n",
		  h_nodename, erl_errno);
      	  exit(1);
      }

    }
    if ((p = strchr((const char *)flags.node, (int) '@')) == 0) {
	strcpy(host_name, ei_thishostname(&ec));
	host = host_name;
    } else {
	*p = 0;
	host = p+1;
    }

    /* 
     * Expand name to a real name (may be ip-address) 
     */
    /* FIXME better error string */
    if ((hp = ei_gethostbyname(host)) == 0) {
	fprintf(stderr,"erl_call: can't ei_gethostbyname(%s)\n", host);
	exit(1);
    }
    /* If shortnames, cut off the name at first '.' */
    if (flags.use_long_name == 0 && (ct = strchr(hp->h_name, '.')) != NULL) {
	*ct = '\0';
    }
    strncpy(host_name, hp->h_name, EI_MAXHOSTNAMELEN);
    host_name[EI_MAXHOSTNAMELEN] = '\0';
    if (strlen(flags.node) + strlen(host_name) + 2 > sizeof(nodename)) {
	fprintf(stderr,"erl_call: nodename too long: %s\n", flags.node);
	exit(1);
    }
    sprintf(nodename, "%s@%s", flags.node, host_name);

    /* 
     * Try to connect. Start an Erlang system if the
     * start option is on and no system is running.
     */
    if (flags.startp && !flags.haltp) {
	fd = do_connect(&ec, nodename, &flags);
    } else if ((fd = ei_connect(&ec, nodename)) < 0) {
	/* We failed to connect ourself */
	/* FIXME do we really know we failed because of node not up? */
	if (flags.haltp) {
	    exit(0);
	} else {
	    fprintf(stderr,"erl_call: failed to connect to node %s\n",
		    nodename);
	    exit(1);
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
	exit(0);
    }

    if (flags.verbosep) {
	fprintf(stderr,"erl_call: we are now connected to node \"%s\"\n",
		nodename);
    }

    /*
     * Compile the module read from stdin.
     */
    if (flags.modp && (modname != NULL)) {
      char fname[256];

      if (strlen(modname) + 4 + 1 > sizeof(fname)) {
      fprintf(stderr,"erl_call: module name too long: %s\n", modname);
      exit(1);
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
	      exit(1);
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

	  if (ei_rpc(&ec, fd, "erl_eval", "eval_str", p, i, &reply) < 0) {
	      fprintf(stderr,"erl_call: evaluating input failed: %s\n",
		      evalbuf);
	      free(p);
	      free(evalbuf);	/* Allocated in read_stdin() */
	      ei_x_free(&reply);
	      exit(1);
	  }
	  i = 0;
	  ei_print_term(stdout,reply.buff,&i);
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

      split_apply_string(flags.apply, &mod, &fun, &args);
      if (flags.verbosep) {
	  fprintf(stderr,"erl_call: module = %s, function = %s, args = %s\n",
		  mod, fun, args);
      }

      ei_x_new(&e);		/* No version to ei_rpc() */
      
      if (ei_x_format_wo_ver(&e, args) < 0) {
	  /* FIXME no error message and why -1 ? */
	  exit(-1);
      }

      ei_x_new_with_version(&reply);

      if (ei_rpc(&ec, fd, mod, fun, e.buff, e.index, &reply) < 0) {
	  /* FIXME no error message and why -1 ? */
	  ei_x_free(&e);
	  ei_x_free(&reply);
	  exit(-1);
      } else {
	  int i = 0;
	  ei_print_term(stdout,reply.buff,&i);
	  ei_x_free(&e);
	  ei_x_free(&reply);
      }
    }

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
	    exit(1);
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
		exit(1);
	    case ERL_CONNECT_FAIL:
		fprintf(stderr,"erl_call: connect failed\n");
		exit(1);
	    case ERL_NO_PORT:
		fprintf(stderr,"erl_call: node is not running\n");
		exit(1);
	    case ERL_TIMEOUT:
		fprintf(stderr,"erl_call: connect timed out\n");
		exit(1);
	    default:
		fprintf(stderr,"erl_call: error during connect\n");
		exit(1);
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


/***************************************************************************
 *
 *  Different error reporting functions that output usage 
 *
 ***************************************************************************/

static void usage_noexit(const char *progname) {
  fprintf(stderr,"\nUsage: %s [-[demqrsv]] [-c Cookie] [-h HiddenName] \n", progname);
  fprintf(stderr,"            [-x ErlScript] [-a [Mod [Fun [Args]]]]\n");
  fprintf(stderr,"            (-n Node | -sname Node | -name Node)\n\n");
#ifdef __WIN32__
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a \"erlang length [[a,b,c]]\"\n");
#else
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a 'erlang length [[a,b,c]]'\n");
#endif
  fprintf(stderr,"         -c  cookie string; by default read from ~/.erlang.cookie\n");
  fprintf(stderr,"         -d  direct Erlang output to ~/.erl_call.out.<Nodename>\n");
  fprintf(stderr,"         -e  evaluate contents of standard input (e.g echo \"X=1,Y=2,{X,Y}.\"|erl_call -e ...)\n");
  fprintf(stderr,"         -h  specify a name for the erl_call client node\n");
  fprintf(stderr,"         -m  read and compile Erlang module from stdin\n");
  fprintf(stderr,"         -n  name of Erlang node, same as -name\n");
  fprintf(stderr,"         -name  name of Erlang node, expanded to a fully qualified\n");
  fprintf(stderr,"         -sname name of Erlang node, short form will be used\n");
  fprintf(stderr,"         -q  halt the Erlang node (overrides the -s switch)\n");
  fprintf(stderr,"         -r  use a random name for the erl_call client node\n");
  fprintf(stderr,"         -s  start a new Erlang node if necessary\n");
  fprintf(stderr,"         -v  verbose mode, i.e print some information on stderr\n");
  fprintf(stderr,"         -x  use specified erl start script, default is erl\n");
}

static void usage_arg(const char *progname, const char *switchname) {
  fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
  usage_noexit(progname);
  exit(1);
}

static void usage_error(const char *progname, const char *switchname) {
  fprintf(stderr, "Illegal argument \'%s\'.\n", switchname);
  usage_noexit(progname);
  exit(1);
}

static void usage(const char *progname) {
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
