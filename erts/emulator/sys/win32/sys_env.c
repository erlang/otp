/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2017. All Rights Reserved.
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

static erts_osenv_t sysenv_global_env;
static erts_rwmtx_t sysenv_rwmtx;

static void import_initial_env(void);

void erts_sys_env_init() {
    erts_rwmtx_init(&sysenv_rwmtx, "environ", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    erts_osenv_init(&sysenv_global_env);
    import_initial_env();
}

const erts_osenv_t *erts_sys_rlock_global_osenv() {
    erts_rwmtx_rlock(&sysenv_rwmtx);
    return &sysenv_global_env;
}

erts_osenv_t *erts_sys_rwlock_global_osenv() {
    erts_rwmtx_rwlock(&sysenv_rwmtx);
    return &sysenv_global_env;
}

void erts_sys_runlock_global_osenv() {
    erts_rwmtx_runlock(&sysenv_rwmtx);
}

void erts_sys_rwunlock_global_osenv() {
    erts_rwmtx_rwunlock(&sysenv_rwmtx);
}

int erts_sys_explicit_host_getenv(char *key, char *value, size_t *size) {
    size_t new_size = GetEnvironmentVariableA(key, value, (DWORD)*size);

    if(new_size == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
        return 0;
    } else if(new_size > *size) {
        return -1;
    }

    *size = new_size;
    return 1;
}

int erts_sys_explicit_8bit_putenv(char *key, char *value) {
    WCHAR *wide_key, *wide_value;
    int key_length, value_length;
    int result;

    /* Note that we do *NOT* honor the filename encoding flags (+fnu/+fnl)
     * here; the previous implementation used SetEnvironmentVariableA and
     * things may break if we step away from that. */

    key_length = MultiByteToWideChar(CP_ACP, 0, key, -1, NULL, 0);
    value_length = MultiByteToWideChar(CP_ACP, 0, value, -1, NULL, 0);

    /* Report "not found" if either string isn't convertible. */
    if(key_length == 0 || value_length == 0) {
        return 0;
    }

    wide_key = erts_alloc(ERTS_ALC_T_TMP, key_length * sizeof(WCHAR));
    wide_value = erts_alloc(ERTS_ALC_T_TMP, value_length * sizeof(WCHAR));

    MultiByteToWideChar(CP_ACP, 0, key, -1, wide_key, key_length);
    MultiByteToWideChar(CP_ACP, 0, value, -1, wide_value, value_length);

    {
        erts_osenv_data_t env_key, env_value;
        erts_osenv_t *env;

        env = erts_sys_rwlock_global_osenv();

        /* -1 to exclude the NUL terminator. */
        env_key.length = (key_length - 1) * sizeof(WCHAR);
        env_key.data = wide_key;

        env_value.length = (value_length - 1) * sizeof(WCHAR);
        env_value.data = wide_value;

        result = erts_osenv_put_native(env, &env_key, &env_value);
        erts_sys_rwunlock_global_osenv();
    }

    erts_free(ERTS_ALC_T_TMP, wide_key);
    erts_free(ERTS_ALC_T_TMP, wide_value);

    return result;
}

int erts_sys_explicit_8bit_getenv(char *key, char *value, size_t *size) {
    erts_osenv_data_t env_key, env_value;
    int key_length, value_length, result;
    WCHAR *wide_key, *wide_value;

    key_length = MultiByteToWideChar(CP_ACP, 0, key, -1, NULL, 0);

    /* Report "not found" if the string isn't convertible. */
    if(key_length == 0) {
        return 0;
    }

    wide_key = erts_alloc(ERTS_ALC_T_TMP, key_length * sizeof(WCHAR));
    MultiByteToWideChar(CP_ACP, 0, key, -1, wide_key, key_length);

    /* We assume that the worst possible size is twice the output buffer width,
     * as we could theoretically be on a code page that requires surrogates. */
    value_length = (*size) * 2;
    wide_value = erts_alloc(ERTS_ALC_T_TMP, value_length * sizeof(WCHAR));

    {
        const erts_osenv_t *env = erts_sys_rlock_global_osenv();

        /* -1 to exclude the NUL terminator. */
        env_key.length = (key_length - 1) * sizeof(WCHAR);
        env_key.data = wide_key;

        env_value.length = value_length * sizeof(WCHAR);
        env_value.data = wide_value;

        result = erts_osenv_get_native(env, &env_key, &env_value);
        erts_sys_runlock_global_osenv();
    }

    if(result == 1 && env_value.length > 0) {
        /* This function doesn't NUL-terminate if the provided size is >= 0,
         * so we pass (*size - 1) to reserve space for it and then do it
         * manually. */
        *size = WideCharToMultiByte(CP_ACP, 0, env_value.data,
            env_value.length / sizeof(WCHAR), value, *size - 1, NULL, NULL);

        if(*size == 0) {
            if(GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                result = -1;
            } else {
                result = 0;
            }
        }
    } else {
        *size = 0;
    }

    if(*size > 0) {
        value[*size] = '\0';
    }

    erts_free(ERTS_ALC_T_TMP, wide_key);
    erts_free(ERTS_ALC_T_TMP, wide_value);

    return result;
}

static void import_initial_env(void) {
    WCHAR *environment_block, *current_variable;

    environment_block = GetEnvironmentStringsW();
    current_variable = environment_block;

    while(wcslen(current_variable) > 0) {
        WCHAR *separator_index = wcschr(current_variable, L'=');

        /* We tolerate environment variables starting with '=' as the per-drive
         * working directories are stored this way. */
        if(separator_index == current_variable) {
            separator_index = wcschr(separator_index + 1, L'=');
        }

        if(separator_index != NULL && separator_index != current_variable) {
            erts_osenv_data_t env_key, env_value;

            env_key.length = (separator_index - current_variable) * sizeof(WCHAR);
            env_key.data = current_variable;

            env_value.length = (wcslen(separator_index) - 1) * sizeof(WCHAR);
            env_value.data = separator_index + 1;

            erts_osenv_put_native(&sysenv_global_env, &env_key, &env_value);
        }

        current_variable += wcslen(current_variable) + 1;
    }

    FreeEnvironmentStringsW(environment_block);
}
