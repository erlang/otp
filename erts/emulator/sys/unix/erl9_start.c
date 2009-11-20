/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * XXX This is a temporary dummy to make sys.c happy until we'll rewrite it.
 */
unsigned preloaded_size_ring0 = 1;
unsigned char preloaded_ring0[1] = {0};

Preload pre_loaded[] = {
    {"ring0", 1, preloaded_ring0},
    {0, 0, 0}
};

int
main(int argc, char** argv)
{
    char sbuf[1024];
    struct {
	void* p;
	int sz;
    } bins[2];
    int bin_num = 0;
    FILE* fp;
    char* progname = argv[0];
    char* eq;

    argv++, argc--;

    if (argc > 0 && argv[0][0] == '-') {
	argv++, argc--;
    }
    if (argc < 1) {
	abort();
    }
    if ((fp = fopen(argv[0], "r")) == NULL) {
	abort();
    }
    
    /* Needs to be called before any memory allocation */
    erts_short_init();

    while (fgets(sbuf, sizeof sbuf, fp)) {
	if (sbuf[0] == '#') {
	    continue;		/* Comment */
	} else if (sbuf[0] == 'e' && strncmp("exec", sbuf, 4) == 0) {
	    continue;		/* Comment ;-) */
	} else if ((eq = strchr(sbuf, '=')) != NULL) {
	    char* val;
	    char* p = strchr(sbuf, '\n');
	    if (p) {
		*p = '\0';
	    }
	    *eq = '\0';
	    val = erts_read_env(sbuf);
	    if (val == NULL) {
		*eq = '=';
		erts_sys_putenv(sbuf, eq - &sbuf[0]);
	    }
	    erts_free_read_env(val);
	} else if (sbuf[0] == ':' && '0' <= sbuf[1] && sbuf[1] <= '9') {
	    int load_size = atoi(sbuf+1);
	    void* bin;
	    
	    bin = malloc(load_size);
	    if (fread(bin, 1, load_size, fp) != load_size) {
		abort();
	    }
	    bins[bin_num].p = bin;
	    bins[bin_num].sz = load_size;
	    bin_num++;
	} else if (strcmp(sbuf, "--end--\n") == 0) {
	    int rval;
	    Eterm mod = NIL;
	    char *val;

	    fclose(fp);

	    if (bin_num != 2) {
		abort();
	    }

	    val = erts_read_env("ERLBREAKHANDLER");
 	    if (val) {
		init_break_handler();
	    }
	    erts_free_read_env(val);

	    if ((rval = erts_load_module(NULL, 0, NIL, &mod, bins[0].p, bins[0].sz)) < 0) {
		fprintf(stderr, "%s: Load of initial module failed: %d\n",
			progname, rval);
		abort();
	    }
	    erts_first_process(mod, bins[1].p, bins[1].sz, argc, argv);
	    free(bins[0].p);
	    free(bins[1].p);
	    process_main();
	    abort();
	} else {
	    fprintf(stderr, "%s: bad line: %s\n", progname, sbuf);
	    abort();
	}
    }
    abort();
}
