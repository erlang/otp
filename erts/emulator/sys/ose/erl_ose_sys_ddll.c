/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2013. All Rights Reserved.
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
 * Interface functions to the dynamic linker using dl* functions.
 * (No support in OSE, we use static linkage instead)
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"


void erl_sys_ddll_init(void) {
}

/*
 * Open a shared object
 */
int erts_sys_ddll_open(const char *full_name, void **handle, ErtsSysDdllError* err)
{
   return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
}

int erts_sys_ddll_open_noext(char *dlname, void **handle, ErtsSysDdllError* err)
{
   return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
}

/*
 * Find a symbol in the shared object
 */
int erts_sys_ddll_sym2(void *handle, const char *func_name, void **function,
		       ErtsSysDdllError* err)
{
   return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
}

/* XXX:PaN These two will be changed with new driver interface! */

/*
 * Load the driver init function, might appear under different names depending on object arch...
 */

int erts_sys_ddll_load_driver_init(void *handle, void **function)
{
    void *fn;
    int res;
    if ((res = erts_sys_ddll_sym2(handle, "driver_init", &fn, NULL)) != ERL_DE_NO_ERROR) {
	res = erts_sys_ddll_sym2(handle, "_driver_init", &fn, NULL);
    }
    if (res == ERL_DE_NO_ERROR) {
	*function = fn;
    }
    return res;
}

int erts_sys_ddll_load_nif_init(void *handle, void **function, ErtsSysDdllError* err)
{
    void *fn;
    int res;
    if ((res = erts_sys_ddll_sym2(handle, "nif_init", &fn, err)) != ERL_DE_NO_ERROR) {
	res = erts_sys_ddll_sym2(handle, "_nif_init", &fn, err);
    }
    if (res == ERL_DE_NO_ERROR) {
	*function = fn;
    }
    return res;
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
   return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
}


/*
 * Return string that describes the (current) error
 */
char *erts_sys_ddll_error(int code)
{
    return "Unspecified error";
}

void erts_sys_ddll_free_error(ErtsSysDdllError* err)
{
    if (err->str != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, err->str);
    }
}
