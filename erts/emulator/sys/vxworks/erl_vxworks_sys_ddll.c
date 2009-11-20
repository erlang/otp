/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2009. All Rights Reserved.
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

/* 
 * Interface functions to the dynamic linker using dl* functions.
 * (As far as I know it works on SunOS 4, 5, Linux and FreeBSD. /Seb) 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <vxWorks.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <a_out.h>
#include <symLib.h>
#include <loadLib.h>
#include <unldLib.h>
#include <moduleLib.h>
#include <sysSymTbl.h>
#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "erl_driver.h"

#define EXT_LEN          4
#define FILE_EXT         ".eld"
#define ALT_FILE_EXT     ".o"
/* ALT_FILE_EXT must not be longer than FILE_EXT */
#define DRIVER_INIT_SUFFIX "_init"

static MODULE_ID get_mid(char *);
static FUNCPTR lookup(char *);

typedef enum {
    NoError,
    ModuleNotFound,
    ModuleNotUnloadable,
    UnknownError
} FakeSytemError;

static char *errcode_tab[] = {
    "No error",
    "Module/file not found",
    "Module cannot be unloaded",
    "Unknown error"
};

void erl_sys_ddll_init(void) {
    return;
}
/* 
 * Open a shared object
 */
int erts_sys_ddll_open2(char *full_name, void **handle, ErtsSysDdllError* err)
{
    int len;
    
    if (erts_sys_ddll_open_noext(full_name, handle, err) == ERL_DE_NO_ERROR) {
	return ERL_DE_NO_ERROR;
    }
    if ((len = sys_strlen(full_name)) > PATH_MAX-EXT_LEN) {
	return ERL_DE_LOAD_ERROR_NAME_TO_LONG;
    } else {
	static char dlname[PATH_MAX + 1];
	
	sys_strcpy(dlname, full_name);
	sys_strcpy(dlname+len, FILE_EXT);
	if (erts_sys_ddll_open_noext(dlname, handle, err) == ERL_DE_NO_ERROR) {
	    return ERL_DE_NO_ERROR;
	}
	sys_strcpy(dlname+len, ALT_FILE_EXT);
	return erts_sys_ddll_open_noext(dlname, handle, err);
    }
}
int erts_sys_ddll_open_noext(char *dlname, void **handle, ErtsSysDdllError* err)
{
    MODULE_ID mid;
    
    if((mid = get_mid(dlname)) == NULL) {
	return ERL_DE_DYNAMIC_ERROR_OFFSET - ((int) ModuleNotFound);
    }
    *handle = (void *) mid;
    return ERL_DE_NO_ERROR;
}

/* 
 * Find a symbol in the shared object
 */
#define PREALLOC_BUFFER_SIZE 256
int erts_sys_ddll_sym2(void *handle, char *func_name, void **function, ErtsSysDdllError* err)
{
    FUNCPTR proc;
    static char statbuf[PREALLOC_BUFFER_SIZE];
    char *buf = statbuf;
    int need;

    if ((proc = lookup(func_name)) == NULL) {
	if ((need = strlen(func_name)+2) > PREALLOC_BUFFER_SIZE) {
	    buf = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF,need);
	}
	buf[0] = '_';
	sys_strcpy(buf+1,func_name);
	proc = lookup(buf);
	if (buf != statbuf) {
	    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, buf);
	}
	if (proc == NULL) {
	    return  ERL_DE_LOOKUP_ERROR_NOT_FOUND;
	}
    }
    *function = (void *) proc;
    return ERL_DE_NO_ERROR;
}

/* XXX:PaN These two will be changed with new driver interface! */

/* 
 * Load the driver init function, might appear under different names depending on object arch... 
 */

int erts_sys_ddll_load_driver_init(void *handle, void **function)
{
    MODULE_ID mid = (MODULE_ID) handle;
    char *modname;
    char *cp;
    static char statbuf[PREALLOC_BUFFER_SIZE];
    char *fname = statbuf;
    int len;
    int res;
    void *func;
    int need;

    if((modname = moduleNameGet(mid)) == NULL) {
	return ERL_DE_DYNAMIC_ERROR_OFFSET - ((int) ModuleNotFound);
    }
    
    if((cp = strrchr(modname, '.')) == NULL) {
	len = strlen(modname);
    } else {
	len = cp - modname;
    }
    
    need =  len + strlen(DRIVER_INIT_SUFFIX) + 1;
    if (need > PREALLOC_BUFFER_SIZE) {
	fname = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, need); /* erts_alloc exits on failure */
    }
    sys_strncpy(fname, modname, len);
    fname[len] = '\0';
    sys_strcat(fname, DRIVER_INIT_SUFFIX);
    res = erts_sys_ddll_sym(handle, fname, &func);
    if (fname != statbuf) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, fname);
    }
    if ( res != ERL_DE_NO_ERROR) {
	return res;
    }
    *function = func;
    return ERL_DE_NO_ERROR;
}

int erts_sys_ddll_load_nif_init(void *handle, void **function, ErtsSysDdllError* err)
{
    /* NIFs not implemented for vxworks */
    return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
}

/* 
 * Call the driver_init function, whatever it's really called, simple on unix... 
*/
void *erts_sys_ddll_call_init(void *function) {
    void *(*initfn)(void) = function;
    return (*initfn)();
}
void *erts_sys_ddll_call_nif_init(void *function) {
    return erts_sys_ddll_call_init(function);
}


/* 
 * Close a chared object
 */
int erts_sys_ddll_close2(void *handle, ErtsSysDdllError* err)
{
    MODULE_ID mid = (MODULE_ID) handle;
    if (unld(mid, 0) < 0) {
	return  ERL_DE_DYNAMIC_ERROR_OFFSET - ((int) ModuleNotUnloadable);
    }
    return  ERL_DE_NO_ERROR;
}

/*
 * Return string that describes the (current) error
 */
char *erts_sys_ddll_error(int code)
{
    int actual_code;
    if (code > ERL_DE_DYNAMIC_ERROR_OFFSET) {
	return "Unspecified error";
    }
    actual_code = -1*(code - ERL_DE_DYNAMIC_ERROR_OFFSET);
    if (actual_code > ((int) UnknownError)) {
	actual_code = UnknownError;
    }
    return  errcode_tab[actual_code];
}

static FUNCPTR lookup(char *sym)
{
    FUNCPTR entry;
    SYM_TYPE type;
    
    if (symFindByNameAndType(sysSymTbl, sym, (char **)&entry,
			     &type, N_EXT | N_TEXT, N_EXT | N_TEXT) != OK) {
	return NULL ;
    }
    return entry;
}

static MODULE_ID get_mid(char* name) 
{ 
    int fd;
    MODULE_ID mid = NULL;
    
    if((fd = open(name, O_RDONLY, 0664)) >= 0) {
	mid = loadModule(fd, GLOBAL_SYMBOLS);
	close(fd);
    }
    return mid;
}

void erts_sys_ddll_free_error(ErtsSysDdllError* err)
{
    /* NYI */
}

