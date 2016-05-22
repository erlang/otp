/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
#include "erl_sys_driver.h"
#include "erl_alloc.h"

static WCHAR *merge_environment(WCHAR *current, WCHAR *add);
static WCHAR *arg_to_env(WCHAR **arg);
static WCHAR **env_to_arg(WCHAR *env);
static WCHAR **find_arg(WCHAR **arg, WCHAR *str);
static int compare(const void *a, const void *b);

static erts_smp_rwmtx_t environ_rwmtx;

void
erts_sys_env_init(void)
{
    erts_smp_rwmtx_init(&environ_rwmtx, "environ");
}

int
erts_sys_putenv_raw(char *key, char *value)
{
    int res;
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = (SetEnvironmentVariable((LPCTSTR) key,
				  (LPCTSTR) value) ? 0 : 1);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    return res;
}

int
erts_sys_putenv(char *key, char *value)
{
    int res;
    WCHAR *wkey = (WCHAR *) key;
    WCHAR *wvalue = (WCHAR *) value;
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = (SetEnvironmentVariableW(wkey,
				   wvalue) ? 0 : 1);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    return res;
}

int
erts_sys_getenv(char *key, char *value, size_t *size)
{
    size_t req_size = 0;
    int res = 0;
    DWORD new_size;
    WCHAR *wkey = (WCHAR *) key;
    WCHAR *wvalue = (WCHAR *) value;
    DWORD wsize = *size / (sizeof(WCHAR) / sizeof(char));

    SetLastError(0);
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    new_size = GetEnvironmentVariableW(wkey,
				       wvalue,
				      (DWORD) wsize);
    res = !new_size && GetLastError() == ERROR_ENVVAR_NOT_FOUND ? -1 : 0;
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    if (res < 0)
	return res;
    res = new_size > wsize ? 1 : 0;
    *size = new_size * (sizeof(WCHAR) / sizeof(char));
    return res;
}
int
erts_sys_getenv__(char *key, char *value, size_t *size)
{
    size_t req_size = 0;
    int res = 0;
    DWORD new_size;

    SetLastError(0);
    new_size = GetEnvironmentVariable((LPCTSTR) key,
				      (LPTSTR) value,
				      (DWORD) *size);
    res = !new_size && GetLastError() == ERROR_ENVVAR_NOT_FOUND ? -1 : 0;
    if (res < 0)
	return res;
    res = new_size > *size ? 1 : 0;
    *size = new_size;
    return res;
}

int
erts_sys_getenv_raw(char *key, char *value, size_t *size)
{
    int res;
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    res = erts_sys_getenv__(key, value, size);
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    return res;
}

void init_getenv_state(GETENV_STATE *state)
{
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    state->environment_strings = GetEnvironmentStringsW();
    state->next_string = state->environment_strings;
}

char *getenv_string(GETENV_STATE *state)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rlocked(&environ_rwmtx));
    if (state->next_string[0] == L'\0') {
	return NULL;
    } else {
	WCHAR *res = state->next_string;
	state->next_string += wcslen(res) + 1;
	return (char *) res;
    }
}

void fini_getenv_state(GETENV_STATE *state)
{
    FreeEnvironmentStringsW(state->environment_strings);
    state->environment_strings = state->next_string = NULL;
    erts_smp_rwmtx_runlock(&environ_rwmtx);
}

int erts_sys_unsetenv(char *key)
{
    int res = 0;
    WCHAR *wkey = (WCHAR *) key;

    SetLastError(0);
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    GetEnvironmentVariableW(wkey,
                            NULL,
                            0);
    if (GetLastError() != ERROR_ENVVAR_NOT_FOUND) {
        res = (SetEnvironmentVariableW(wkey,
                                       NULL) ? 0 : 1);
    }
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    return res;
}

char*
win_build_environment(char* new_env)
{
    if (new_env == NULL) {
	return NULL;
    } else {
	WCHAR *tmp, *merged, *tmp_new;

	tmp_new = (WCHAR *) new_env;
	
	erts_smp_rwmtx_rlock(&environ_rwmtx);
	tmp = GetEnvironmentStringsW();
	merged = merge_environment(tmp, tmp_new);

	FreeEnvironmentStringsW(tmp);
	erts_smp_rwmtx_runlock(&environ_rwmtx);
	return (char *) merged;
    }
}

static WCHAR *
merge_environment(WCHAR *old, WCHAR *add)
{
    WCHAR **a_arg = env_to_arg(add);
    WCHAR **c_arg = env_to_arg(old);
    WCHAR *ret;
    int i, j;
    
    for(i = 0; c_arg[i] != NULL; ++i)
	;

    for(j = 0; a_arg[j] != NULL; ++j)
	;

    c_arg = erts_realloc(ERTS_ALC_T_TMP,
			 c_arg, (i+j+1) * sizeof(WCHAR *));

    for(j = 0; a_arg[j] != NULL; ++j){
	WCHAR **tmp;
	WCHAR *current = a_arg[j];
	WCHAR *eq_p = wcschr(current,L'=');
	int unset = (eq_p!=NULL && eq_p[1]==L'\0');

	if ((tmp = find_arg(c_arg, current)) != NULL) {
	    if (!unset) {
		*tmp = current;
	    } else {
		*tmp = c_arg[--i];
		c_arg[i] = NULL;
	    }
	} else if (!unset) {
	    c_arg[i++] = current;
	    c_arg[i] = NULL;
	}
    }
    ret = arg_to_env(c_arg);
    erts_free(ERTS_ALC_T_TMP, c_arg);
    erts_free(ERTS_ALC_T_TMP, a_arg);
    return ret;
}

static WCHAR**
find_arg(WCHAR **arg, WCHAR *str)
{
    WCHAR *tmp;
    int len;

    if ((tmp = wcschr(str, L'=')) != NULL) {
	tmp++;
	len = tmp - str;
	while (*arg != NULL){
	    if (_wcsnicmp(*arg, str, len) == 0){
		return arg;
	    }
	    ++arg;
	}
    }
    return NULL;
}

static int
compare(const void *a, const void *b)
{
    WCHAR *s1 = *((WCHAR **) a);
    WCHAR *s2 = *((WCHAR **) b);
    WCHAR *e1 = wcschr(s1,L'=');
    WCHAR *e2 = wcschr(s2,L'=');
    int ret;
    int len;
  
    if(!e1)
	e1 = s1 + wcslen(s1);
    if(!e2)
	e2 = s2 + wcslen(s2);
  
    if((e1 - s1) > (e2 - s2))
	len = (e2 - s2);
    else
	len = (e1 - s1);
  
    ret = _wcsnicmp(s1,s2,len);
    if (ret == 0)
	return ((e1 - s1) - (e2 - s2));
    else
	return ret;
}

static WCHAR**
env_to_arg(WCHAR *env)
{
    WCHAR **ret;
    WCHAR *tmp;
    int i;
    int num_strings = 0;

    for(tmp = env; *tmp != '\0'; tmp += wcslen(tmp)+1) {
	++num_strings;
    }
    ret = erts_alloc(ERTS_ALC_T_TMP, sizeof(WCHAR *) * (num_strings + 1));
    i = 0;
    for(tmp = env; *tmp != '\0'; tmp += wcslen(tmp)+1){
	ret[i++] = tmp;
    }
    ret[i] = NULL;
    return ret;
}

static WCHAR *
arg_to_env(WCHAR **arg)
{
    WCHAR *block;
    WCHAR *ptr;
    int i;
    int totlen = 1;		/* extra '\0' */

    for(i = 0; arg[i] != NULL; ++i) {
	totlen += wcslen(arg[i])+1;
    }

    /* sort the environment vector */
    qsort(arg, i, sizeof(WCHAR *), &compare);

    if (totlen == 1){
	block = erts_alloc(ERTS_ALC_T_ENVIRONMENT, 2 * sizeof(WCHAR));
	block[0] = block[1] = '\0'; 
    } else {
	block = erts_alloc(ERTS_ALC_T_ENVIRONMENT, totlen * sizeof(WCHAR));
	ptr = block;
	for(i=0; arg[i] != NULL; ++i){
	    wcscpy(ptr, arg[i]);
	    ptr += wcslen(ptr)+1;
	}
	*ptr = '\0';
    }
    return block;
}
