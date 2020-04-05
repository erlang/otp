/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
 * (As far as I know it works on SunOS 4, 5, Linux and FreeBSD. /Seb) 
 */

#include <windows.h>

#define GET_ERTS_ALC_TEST
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "global.h"
#include "erl_alloc.h"

#include "erl_driver.h"
#include "erl_win_dyn_driver.h"

#include "erl_nif.h"

#define EXT_LEN          4
#define FILE_EXT_WCHAR   L".dll"

static DWORD tls_index = 0;
static TWinDynDriverCallbacks wddc;
static TWinDynNifCallbacks nif_callbacks;

void erl_sys_ddll_init(void) {
    WCHAR cwd_buffer[MAX_PATH];

    tls_index = TlsAlloc();
    ERL_INIT_CALLBACK_STRUCTURE(wddc);

    /* LOAD_WITH_ALTERED_SEARCH_PATH removes the startup directory from the
     * search path, so we add it separately to be backwards compatible. */
    if (GetCurrentDirectoryW(sizeof(cwd_buffer), cwd_buffer)) {
        SetDllDirectoryW(cwd_buffer);
    }

#define ERL_NIF_API_FUNC_DECL(RET,NAME,ARGS) nif_callbacks.NAME = NAME
#include "erl_nif_api_funcs.h"
#undef ERL_NIF_API_FUNC_DECL
    nif_callbacks.erts_alc_test = erts_alc_test;
 
    return;
}

/* 
 * Open a shared object
 * Expecting 'full_name' as an UTF-8 string.
 */
int erts_sys_ddll_open(const char *full_name, void **handle, ErtsSysDdllError* err)
{
    HINSTANCE hinstance;
    int len;
    wchar_t* wcp;
    Sint used;
    int code;
    
    if ((len = sys_strlen(full_name)) >= MAXPATHLEN - EXT_LEN) {
	if (err != NULL) {
	    err->str = "Library name too long";
	}
	return ERL_DE_LOAD_ERROR_NAME_TO_LONG;
    }

    wcp = (wchar_t*)erts_convert_filename_to_wchar((byte*)full_name, len,
						   NULL, 0,
						   ERTS_ALC_T_TMP, &used, EXT_LEN);
    wcscpy(&wcp[used/2 - 1], FILE_EXT_WCHAR);

    /* LOAD_WITH_ALTERED_SEARCH_PATH adds the specified DLL's directory to the
     * dependency search path. This also removes the directory we started in,
     * but we've explicitly added that in in erl_sys_ddll_init. */
    if ((hinstance = LoadLibraryExW(wcp, NULL, LOAD_WITH_ALTERED_SEARCH_PATH)) == NULL) {
	code = ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError();
	if (err != NULL) {
	    err->str = erts_sys_ddll_error(code);
	}
    }
    else {
        code = ERL_DE_NO_ERROR;
	*handle = (void *) hinstance;
    }
    erts_free(ERTS_ALC_T_TMP, wcp);
    return code;
}

int erts_sys_ddll_open_noext(char *dlname, void **handle, ErtsSysDdllError* err)
{
    HINSTANCE hinstance;
    
    if ((hinstance = LoadLibrary(dlname)) == NULL) {
	int code = ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError(); 
	if (err != NULL) {
	    err->str = erts_sys_ddll_error(code);
	}
	return code;
    } else {
	*handle = (void *) hinstance;
	return ERL_DE_NO_ERROR;
    }
}

/* 
 * Find a symbol in the shared object
 */
int erts_sys_ddll_sym2(void *handle, const char *func_name, void **function,
		       ErtsSysDdllError* err)
{
    FARPROC proc;
    if ((proc = GetProcAddress( (HINSTANCE) handle, func_name)) == NULL) {
	int code = ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError();
	if (err != NULL) {
	    err->str = erts_sys_ddll_error(code);
	}
	return  code;
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
    void *fn;
    int res;
    if ((res = erts_sys_ddll_sym(handle, "driver_init", &fn)) != ERL_DE_NO_ERROR) {
	return res;
    }
    *function = fn;
    return res;
}

int erts_sys_ddll_load_nif_init(void *handle, void **function, ErtsSysDdllError* err)
{
    void *fn;
    int res;
    if ((res = erts_sys_ddll_sym2(handle, "nif_init", &fn, err)) != ERL_DE_NO_ERROR) {
	return res;
    }
    *function = fn;
    return res;
}


/* 
 * Call the driver_init function, whatever it's really called, simple on unix... 
*/
void *erts_sys_ddll_call_init(void *function) {
    void *(*initfn)(TWinDynDriverCallbacks *) = function;
    return (*initfn)(&wddc);
}

void *erts_sys_ddll_call_nif_init(void *function) {
    void *(*initfn)(TWinDynNifCallbacks *) = function;
    return (*initfn)(&nif_callbacks);
}


/* 
 * Close a chared object
 */
int erts_sys_ddll_close2(void *handle, ErtsSysDdllError* err)
{
    if (!FreeLibrary((HINSTANCE) handle)) {
	int code = ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError();
	if (err != NULL) {
	    err->str = erts_sys_ddll_error(code);
	}
	return code;
    }
    return  ERL_DE_NO_ERROR;
}

/*
 * Return string that describes the (current) error
 */
#define MAX_ERROR 255
char *erts_sys_ddll_error(int code)
{
    int actual_code;
    char *local_ptr;
    if (code > ERL_DE_DYNAMIC_ERROR_OFFSET) {
	return "Unspecified error";
    }
    actual_code = -1*(code - ERL_DE_DYNAMIC_ERROR_OFFSET);

    local_ptr = TlsGetValue(tls_index);
    if (local_ptr == NULL) {
	local_ptr = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, MAX_ERROR);
	TlsSetValue(tls_index,local_ptr);
    }
    if (!FormatMessage(
		       FORMAT_MESSAGE_FROM_SYSTEM,
		       NULL,
		       (DWORD) actual_code,
		       MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		       local_ptr,
		       MAX_ERROR, NULL )) {
	return "Unspecified error";
    } else {
	char *ptr = local_ptr + strlen(local_ptr) - 1;
	while (ptr >= local_ptr && (*ptr == '\r' || *ptr == '\n')) {
	    *ptr-- = '\0';
	}
    }
    return  local_ptr;
}

void erts_sys_ddll_free_error(ErtsSysDdllError* err)
{
    /* err->str may be either a static string or reused as thread local data,
     * so wo don't bother free it. 
     */
}

