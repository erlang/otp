/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
 * A compiler wrapper that translate (some) gcc command line arguments
 * to the Visual C++ compiler and (of course) the gcc compiler. It also
 * makes some changes in the command line arguments when debug compiling.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>


#if !defined(__WIN32__)
#define USE_EXEC
#include <unistd.h>
#endif


#ifdef __WIN32__
#define EOL "\r\n"
#else
#define EOL "\n"
#endif

#define ARGS_INCR 20

static char *prog;

typedef struct {
    char **vec;
    int no;
    int ix;
    int chars;
} args_t;

static void
enomem(void)
{
    fprintf(stderr, "%s: Out of memory%s", prog, EOL);
    exit(1);
}

static void
save_arg(args_t *args, char *arg1, ...)
{
    char *carg;
    va_list argp;

    va_start(argp, arg1);
    carg = arg1;
    while (carg) {
	if (args->no <= args->ix) {
	    args->vec = (char **) (args->no
				   ? realloc((void *) args->vec,
					     (sizeof(char *)
					      *(args->no + ARGS_INCR + 2)))
				   : malloc((sizeof(char *)
					     *(args->no + ARGS_INCR + 2))));
	    if (!args->vec)
		enomem();
	    args->no += ARGS_INCR;
	}
	if (carg == arg1) {
	  args->vec[args->ix++] = "\"";
	  args->chars++;
	}
	args->vec[args->ix++] = carg;
	args->chars += strlen(carg);
	carg = va_arg(argp, char *);
    }
    args->vec[args->ix++] = "\"";
    args->chars++;
    args->vec[args->ix++] = " ";
    args->chars++;
    va_end(argp);
}

static int
is_prefix(char *prfx, char **str)
{
    int i;
    for (i = 0; prfx[i] && (*str)[i]; i++) {
	if (prfx[i] != (*str)[i])
	    return 0;
    }
    if (!prfx[i]) {
	*str = &(*str)[i];
	return 1;
    }
    return 0;
}

static void
cpy(char **dst, char *src)
{
    int i;
    for (i = 0; src[i]; i++)
	(*dst)[i] = src[i];
    *dst = &(*dst)[i];
}

typedef enum {
    STDLIB_NONE,
    STDLIB_MD,
    STDLIB_ML,
    STDLIB_MT
} stdlib_t;

int
main(int argc, char *argv[])
{
    int res;
    int i;
    size_t cmd_len;
    char *cmd;
    char *cmd_end;
    char *cc = NULL;
    args_t args = {0};
    int is_debug = 0;
    int is_purify = 0;
    int is_quantify = 0;
    int is_purecov = 0;
#ifdef __WIN32__
    int is_shared = 0;
    stdlib_t stdlib = STDLIB_NONE;
    char *shared_flag = "";
    char *stdlib_flag = "";
    int have_link_args = 0;
    args_t link_args = {0};

#define CHECK_FIRST_LINK_ARG						\
	if (!have_link_args) {						\
	    save_arg(&link_args, "-link", NULL);			\
	    have_link_args = 1;						\
	}
#else /* #ifdef __WIN32__ */
#define CHECK_FIRST_LINK_ARG
#endif /* #ifdef __WIN32__ */

   prog = argv[0];


    for (i = 1; i < argc; i++) {
	char *arg = argv[i];
	if (is_prefix("-CC", &arg)) {
	    cc = arg;
	}
	else if (is_prefix("-O", &arg)) {
	    if (!is_debug)
		save_arg(&args, argv[i], NULL);
	}
	else if (strcmp("-DDEBUG", arg) == 0) {
	    save_arg(&args, arg, NULL);
#ifdef __WIN32__
	set_debug:
#endif
	    if (!is_debug) {
		int j;
		is_debug = 1;
#ifdef __WIN32__
		save_arg(&args, "-Z7", NULL);
		CHECK_FIRST_LINK_ARG;
		save_arg(&link_args, "-debug", NULL);
		save_arg(&link_args, "-pdb:none", NULL);
#endif
		for (j = 0; j < args.ix; j++) {
		    char *tmp_arg = args.vec[j];
		    if (is_prefix("-O", &tmp_arg))
			args.vec[j] = "";
		}
	    }
	}
	else if (strcmp("-DPURIFY", arg) == 0) {
	    save_arg(&args, arg, NULL);
	    is_purify = 1;
	}
	else if (strcmp("-DQUANTIFY", arg) == 0) {
	    save_arg(&args, arg, NULL);
	    is_quantify = 1;
	}
	else if (strcmp("-DPURECOV", arg) == 0) {
	    save_arg(&args, arg, NULL);
	    is_purecov = 1;
	}
#ifdef __WIN32__
	else if (strcmp("-g", arg) == 0) {
	    goto set_debug;
	}
	else if (strcmp("-MD", arg) == 0)
	    stdlib = STDLIB_MD;
	else if (strcmp("-MDd", arg) == 0) {
	    stdlib = STDLIB_MD;
	    goto set_debug;
	}
	else if (strcmp("-ML", arg) == 0)
	    stdlib = STDLIB_ML;
	else if (strcmp("-MLd", arg) == 0) {
	    stdlib = STDLIB_ML;
	    goto set_debug;
	}
	else if (strcmp("-MT", arg) == 0)
	    stdlib = STDLIB_MT;
	else if (strcmp("-MTd", arg) == 0) {
	    stdlib = STDLIB_MT;
	    goto set_debug;
	}
	else if (strcmp("-shared", arg) == 0 || strcmp("-LD", arg) == 0)
	    is_shared = 1;
	else if (strcmp("-LDd", arg) == 0) {
	    is_shared = 1;
	    goto set_debug;
	}
	else if (strcmp("-Wall", arg) == 0) {
	    save_arg(&args, "-W3", NULL);
	}
	else if (is_prefix("-L", &arg)) {
	    CHECK_FIRST_LINK_ARG;
	    save_arg(&link_args, "-libpath:", arg, NULL);
	}
	else if (strcmp("-link",arg) == 0) {
	  CHECK_FIRST_LINK_ARG;
	}
#endif /* #ifdef __WIN32__ */
	else if (is_prefix("-l", &arg)) {
	    CHECK_FIRST_LINK_ARG;
	    if (is_debug && strcmp("ethread", arg) == 0)
		arg = "ethread.debug";
	    else if (is_purify && strcmp("ethread", arg) == 0)
		arg = "ethread.purify";
	    else if (is_quantify && strcmp("ethread", arg) == 0)
		arg = "ethread.quantify";
	    else if (is_purecov && strcmp("ethread", arg) == 0)
		arg = "ethread.purecov";
#ifdef __WIN32__
	    else if (strcmp("socket", arg) == 0)
		arg = "ws2_32";
	    save_arg(&link_args, arg, ".lib", NULL);
#else
	    save_arg(&args, "-l", arg, NULL);
#endif
	}
	else
	    save_arg(&args, argv[i], NULL);
    }

    if (!cc || !cc[0]) {
	fprintf(stderr, "%s: Missing compulsory -CC flag%s", prog, EOL);
	exit(1);
    }

    cmd_len = strlen(cc) + 1 + args.chars + 1;

#ifdef __WIN32__
    if (is_shared)
	shared_flag = is_debug ? "-LDd " : "-LD ";
    switch (stdlib) {
    case STDLIB_MD: stdlib_flag = is_debug ? "-MDd " : "-MD ";	break;
    case STDLIB_ML: stdlib_flag = is_debug ? "-MLd " : "-ML ";	break;
    case STDLIB_MT: stdlib_flag = is_debug ? "-MTd " : "-MT ";	break;
    case STDLIB_NONE:						break;
    }

    cmd_len += strlen(shared_flag) + strlen(stdlib_flag) + link_args.chars;
#endif

    cmd = (char *) malloc(sizeof(char) * cmd_len);

    if (!cmd)
	enomem();
    cmd_end = cmd;
    cpy(&cmd_end, cc);
    cpy(&cmd_end, " ");
#ifdef __WIN32__
    cpy(&cmd_end, stdlib_flag);
    cpy(&cmd_end, shared_flag);
#endif
    for (i = 0; i < args.ix; i++)
	cpy(&cmd_end, args.vec[i]);
#ifdef __WIN32__
    for (i = 0; i < link_args.ix; i++)
	cpy(&cmd_end, link_args.vec[i]);
#endif
    *cmd_end = '\0';

    printf("==> %s%s", cmd, EOL);
    fflush(stdout);

#ifdef USE_EXEC
    (void) execl("/bin/sh", "sh", "-c", cmd, (char *) NULL);
    perror(NULL);
    res = 1;
#else
    res = system(cmd);
#endif

    free((void *) args.vec);
#ifdef __WIN32__
    free((void *) link_args.vec);
#endif
    free((void *) cmd);

    if (res < 0)
	res = 1;
    return res;
}
