/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
#include "erl_sys_driver.h"
#include "erl_alloc.h"

static char* merge_environment(char *current, char *add);
static char* arg_to_env(char **arg);
static char** env_to_arg(char *env);
static char** find_arg(char **arg, char *str);
static int compare(const void *a, const void *b);

static erts_smp_rwmtx_t environ_rwmtx;

void
erts_sys_env_init(void)
{
    erts_smp_rwmtx_init(&environ_rwmtx, "environ");
}

int
erts_sys_putenv(char *key_value, int sep_ix)
{
    int res;
    char sep = key_value[sep_ix];
    ASSERT(sep == '=');
    key_value[sep_ix] = '\0';
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = (SetEnvironmentVariable((LPCTSTR) key_value,
				  (LPCTSTR) &key_value[sep_ix+1]) ? 0 : 1);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    key_value[sep_ix] = sep;
    return res;
}

int
erts_sys_getenv(char *key, char *value, size_t *size)
{
    size_t req_size = 0;
    int res = 0;
    DWORD new_size;

    erts_smp_rwmtx_rlock(&environ_rwmtx);
    SetLastError(0);
    new_size = GetEnvironmentVariable((LPCTSTR) key,
				      (LPTSTR) value,
				      (DWORD) *size);
    res = !new_size && GetLastError() == ERROR_ENVVAR_NOT_FOUND ? -1 : 0;
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    if (res < 0)
	return res;
    res = new_size > *size ? 1 : 0;
    *size = new_size;
    return res;
}

struct win32_getenv_state {
    char *env;
    char *next;
};


void init_getenv_state(GETENV_STATE *state)
{
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    state->environment_strings = (char *) GetEnvironmentStrings();
    state->next_string = state->environment_strings;
}

char *getenv_string(GETENV_STATE *state)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rlocked(&environ_rwmtx));
    if (state->next_string[0] == '\0')
	return NULL;
    else {
	char *res = state->next_string;
	state->next_string += sys_strlen(res) + 1;
	return res;
    }
}

void fini_getenv_state(GETENV_STATE *state)
{
    FreeEnvironmentStrings(state->environment_strings);
    state->environment_strings = state->next_string = NULL;
    erts_smp_rwmtx_runlock(&environ_rwmtx);
}

char*
win_build_environment(char* new_env)
{
    if (new_env == NULL) {
	return NULL;
    } else {
	char *tmp, *merged;
	
	erts_smp_rwmtx_rlock(&environ_rwmtx);
	tmp = GetEnvironmentStrings();
	merged = merge_environment(tmp, new_env);

	FreeEnvironmentStrings(tmp);
	erts_smp_rwmtx_runlock(&environ_rwmtx);
	return merged;
    }
}

static char*
merge_environment(char *old, char *add)
{
    char **a_arg = env_to_arg(add);
    char **c_arg = env_to_arg(old);
    char *ret;
    int i, j;
    
    for(i = 0; c_arg[i] != NULL; ++i)
	;

    for(j = 0; a_arg[j] != NULL; ++j)
	;

    c_arg = erts_realloc(ERTS_ALC_T_TMP,
			 c_arg, (i+j+1) * sizeof(char *));

    for(j = 0; a_arg[j] != NULL; ++j){
	char **tmp;
	char *current = a_arg[j];
	char *eq_p = strchr(current,'=');
	int unset = (eq_p!=NULL && eq_p[1]=='\0');

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

static char**
find_arg(char **arg, char *str)
{
    char *tmp;
    int len;

    if ((tmp = strchr(str, '=')) != NULL) {
	tmp++;
	len = tmp - str;
	while (*arg != NULL){
	    if (_strnicmp(*arg, str, len) == 0){
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
    char *s1 = *((char **) a);
    char *s2 = *((char **) b);
    char *e1 = strchr(s1,'=');
    char *e2 = strchr(s2,'=');
    int ret;
    int len;
  
    if(!e1)
	e1 = s1 + strlen(s1);
    if(!e2)
	e2 = s2 + strlen(s2);
  
    if((e1 - s1) > (e2 - s2))
	len = (e2 - s2);
    else
	len = (e1 - s1);
  
    ret = _strnicmp(s1,s2,len);
    if (ret == 0)
	return ((e1 - s1) - (e2 - s2));
    else
	return ret;
}

static char**
env_to_arg(char *env)
{
    char **ret;
    char *tmp;
    int i;
    int num_strings = 0;

    for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1) {
	++num_strings;
    }
    ret = erts_alloc(ERTS_ALC_T_TMP, sizeof(char *) * (num_strings + 1));
    i = 0;
    for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1){
	ret[i++] = tmp;
    }
    ret[i] = NULL;
    return ret;
}

static char*
arg_to_env(char **arg)
{
    char *block;
    char *ptr;
    int i;
    int totlen = 1;		/* extra '\0' */

    for(i = 0; arg[i] != NULL; ++i) {
	totlen += strlen(arg[i])+1;
    }

    /* sort the environment vector */
    qsort(arg, i, sizeof(char *), &compare);

    if (totlen == 1){
	block = erts_alloc(ERTS_ALC_T_ENVIRONMENT, 2);
	block[0] = block[1] = '\0'; 
    } else {
	block = erts_alloc(ERTS_ALC_T_ENVIRONMENT, totlen);
	ptr = block;
	for(i=0; arg[i] != NULL; ++i){
	    strcpy(ptr, arg[i]);
	    ptr += strlen(ptr)+1;
	}
	*ptr = '\0';
    }
    return block;
}
