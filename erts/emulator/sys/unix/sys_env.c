
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_osenv.h"
#include "erl_alloc.h"

#include "erl_thr_progress.h"

static erts_osenv_t sysenv_global_env;
static erts_rwmtx_t sysenv_rwmtx;

extern char **environ;

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

void erts_sys_rwunlock_global_osenv() {
    erts_rwmtx_rwunlock(&sysenv_rwmtx);
}

void erts_sys_runlock_global_osenv() {
    erts_rwmtx_runlock(&sysenv_rwmtx);
}

int erts_sys_explicit_8bit_putenv(char *key, char *value) {
    erts_osenv_data_t env_key, env_value;
    int result;

    env_key.length = sys_strlen(key);
    env_key.data = key;

    env_value.length = sys_strlen(value);
    env_value.data = value;

    {
        erts_osenv_t *env = erts_sys_rwlock_global_osenv();
        result = erts_osenv_put_native(env, &env_key, &env_value);
        erts_sys_rwunlock_global_osenv();
    }

    return result;
}

int erts_sys_explicit_8bit_getenv(char *key, char *value, size_t *size) {
    erts_osenv_data_t env_key, env_value;
    int result;

    env_key.length = sys_strlen(key);
    env_key.data = key;

    /* Reserve space for NUL termination. */
    env_value.length = *size - 1;
    env_value.data = value;

    {
        const erts_osenv_t *env = erts_sys_rlock_global_osenv();
        result = erts_osenv_get_native(env, &env_key, &env_value);
        erts_sys_runlock_global_osenv();
    }

    if(result == 1) {
        value[env_value.length] = '\0';
    }

    *size = env_value.length;

    return result;
}

int erts_sys_explicit_host_getenv(char *key, char *value, size_t *size) {
    char *orig_value;
    size_t length;
    
    orig_value = getenv(key);

    if(orig_value == NULL) {
        return 0;
    }

    length = sys_strlen(orig_value);

    if (length >= *size) {
        *size = length + 1;
        return -1;
    }

    sys_memcpy((void*)value, (void*)orig_value, length + 1);
    *size = length;

    return 1;
}

static void import_initial_env(void) {
    char **environ_iterator, *environ_variable;

    environ_iterator = environ;

    while ((environ_variable = *(environ_iterator++)) != NULL) {
        char *separator_index = strchr(environ_variable, '=');

        if (separator_index != NULL) {
            erts_osenv_data_t env_key, env_value;

            env_key.length = separator_index - environ_variable;
            env_key.data = environ_variable;
    
            env_value.length = sys_strlen(separator_index) - 1;
            env_value.data = separator_index + 1;

            erts_osenv_put_native(&sysenv_global_env, &env_key, &env_value);
        }
    }
}
