/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017-2018. All Rights Reserved.
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

#include "erl_osenv.h"

#include "global.h"
#include "erl_alloc.h"
#include "erl_process.h"

#define STACKBUF_SIZE (512)

typedef struct __env_rbtnode_t {
    struct __env_rbtnode_t *parent;
    struct __env_rbtnode_t *left;
    struct __env_rbtnode_t *right;

    int is_red;

    erts_osenv_data_t key;
    erts_osenv_data_t value;
} env_rbtnode_t;

#define ERTS_RBT_PREFIX env
#define ERTS_RBT_T env_rbtnode_t
#define ERTS_RBT_KEY_T erts_osenv_data_t
#define ERTS_RBT_FLAGS_T int
#define ERTS_RBT_INIT_EMPTY_TNODE(T) \
    do { \
        (T)->parent = NULL; \
        (T)->left = NULL; \
        (T)->right = NULL; \
        (T)->is_red = 0; \
    } while(0)
#define ERTS_RBT_IS_RED(T) ((T)->is_red)
#define ERTS_RBT_SET_RED(T) ((T)->is_red = 1)
#define ERTS_RBT_IS_BLACK(T) (!ERTS_RBT_IS_RED(T))
#define ERTS_RBT_SET_BLACK(T) ((T)->is_red = 0)
#define ERTS_RBT_GET_FLAGS(T) ((T)->is_red)
#define ERTS_RBT_SET_FLAGS(T, F) ((T)->is_red = F)
#define ERTS_RBT_GET_PARENT(T) ((T)->parent)
#define ERTS_RBT_SET_PARENT(T, P) ((T)->parent = P)
#define ERTS_RBT_GET_RIGHT(T) ((T)->right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->left = (L))
#define ERTS_RBT_GET_KEY(T) ((T)->key)
#define ERTS_RBT_IS_LT(KX, KY) (compare_env_keys(KX, KY) < 0)
#define ERTS_RBT_IS_EQ(KX, KY) (compare_env_keys(KX, KY) == 0)
#define ERTS_RBT_WANT_FOREACH_DESTROY
#define ERTS_RBT_WANT_FOREACH
#define ERTS_RBT_WANT_REPLACE
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_INSERT
#define ERTS_RBT_WANT_LOOKUP

static int compare_env_keys(const erts_osenv_data_t a, const erts_osenv_data_t b);

#include "erl_rbtree.h"

static int compare_env_keys(const erts_osenv_data_t a, const erts_osenv_data_t b) {
    int relation;

#ifdef __WIN32__
    /* Environment variables are case-insensitive on Windows. */
    relation = _wcsnicmp((const WCHAR*)a.data, (const WCHAR*)b.data,
                         MIN(a.length, b.length) / sizeof(WCHAR));
#else
    relation = sys_memcmp(a.data, b.data, MIN(a.length, b.length));
#endif

    if(relation != 0) {
        return relation;
    }

    if(a.length < b.length) {
        return -1;
    } else if(a.length == b.length) {
        return 0;
    } else {
        return 1;
    }
}

static void *convert_value_to_native(Eterm term, char *stackbuf,
        int stackbuf_size, Sint *length) {
    int encoding;
    void *result;

    if(is_atom(term)) {
        return NULL;
    }

    encoding = erts_get_native_filename_encoding();
    *length = erts_native_filename_need(term, encoding);

    if(*length < 0) {
        return NULL;
    } else if(*length >= stackbuf_size) {
        result = erts_alloc(ERTS_ALC_T_TMP, *length);
    } else {
        result = stackbuf;
    }

    erts_native_filename_put(term, encoding, (byte*)result);

    return result;
}

static void *convert_key_to_native(Eterm term, char *stackbuf,
        int stackbuf_size, Sint *length) {
    byte *name_iterator, *name_end;
    void *result;
    int encoding;

    result = convert_value_to_native(term, stackbuf, stackbuf_size, length);

    if(result == NULL || length == 0) {
        return NULL;
    }

    encoding = erts_get_native_filename_encoding();

    name_iterator = (byte*)result;
    name_end = &name_iterator[*length];

#ifdef __WIN32__
    /* Windows stores per-drive working directories as variables starting with
     * '=', so we skip the first character to tolerate that. */
    name_iterator = erts_raw_env_next_char(name_iterator, encoding);
#endif

    while(name_iterator < name_end) {
        if(erts_raw_env_char_is_7bit_ascii_char('=', name_iterator, encoding)) {
            if(result != stackbuf) {
                erts_free(ERTS_ALC_T_TMP, result);
            }

            return NULL;
        }

        name_iterator = erts_raw_env_next_char(name_iterator, encoding);
    }

    return result;
}

void erts_osenv_init(erts_osenv_t *env) {
    env->variable_count = 0;
    env->content_size = 0;
    env->tree = NULL;
}

static int destroy_foreach(env_rbtnode_t *node, void *_state, Sint reds) {
    erts_free(ERTS_ALC_T_ENVIRONMENT, node);
    (void)_state;
    return 1;
}

void erts_osenv_clear(erts_osenv_t *env) {
    env_rbt_foreach_destroy(&env->tree, &destroy_foreach, NULL);
    erts_osenv_init(env);
}

struct __env_merge {
    int overwrite_existing;
    erts_osenv_t *env;
};

static int merge_foreach(env_rbtnode_t *node, void *_state, Sint reds) {
    struct __env_merge *state = (struct __env_merge*)(_state);
    env_rbtnode_t *existing_node;

    existing_node = env_rbt_lookup(state->env->tree, node->key);

    if(existing_node == NULL || state->overwrite_existing) {
        erts_osenv_put_native(state->env, &node->key, &node->value);
    }
    return 1;
}

void erts_osenv_merge(erts_osenv_t *env, const erts_osenv_t *with, int overwrite) {
    struct __env_merge merge_state;

    merge_state.overwrite_existing = overwrite;
    merge_state.env = env;

    env_rbt_foreach(with->tree, merge_foreach, &merge_state);
}

struct __env_foreach_term {
    erts_osenv_foreach_term_cb_t user_callback;
    struct process *process;
    void *user_state;
};

static int foreach_term_wrapper(env_rbtnode_t *node, void *_state, Sint reds) {
    struct __env_foreach_term *state = (struct __env_foreach_term*)_state;
    Eterm key, value;

    key = erts_convert_native_to_filename(state->process,
        node->key.length, (byte*)node->key.data);
    value = erts_convert_native_to_filename(state->process,
        node->value.length, (byte*)node->value.data);

    state->user_callback(state->process, state->user_state, key, value);
    return 1;
}

void erts_osenv_foreach_term(const erts_osenv_t *env, struct process *process,
        void *state, erts_osenv_foreach_term_cb_t callback) {
    struct __env_foreach_term wrapper_state;

    wrapper_state.user_callback = callback;
    wrapper_state.user_state = state;
    wrapper_state.process = process;

    env_rbt_foreach(env->tree, foreach_term_wrapper, &wrapper_state);
}

int erts_osenv_get_term(const erts_osenv_t *env, Process *process,
        Eterm key_term, Eterm *out_term) {
    char key_stackbuf[STACKBUF_SIZE];
    erts_osenv_data_t key;
    int result;

    key.data = convert_key_to_native(key_term, key_stackbuf,
        STACKBUF_SIZE, &key.length);
    result = -1;

    if(key.data != NULL) {
        env_rbtnode_t *node;

        node = env_rbt_lookup(env->tree, key);
        result = 0;

        if(node != NULL) {
            (*out_term) = erts_convert_native_to_filename(process,
                node->value.length, (byte*)node->value.data);
            result = 1;
        }

        if(key.data != key_stackbuf) {
            erts_free(ERTS_ALC_T_TMP, key.data);
        }
    }

    return result;
}

int erts_osenv_put_term(erts_osenv_t *env, Eterm key_term, Eterm value_term) {
    char key_stackbuf[STACKBUF_SIZE], value_stackbuf[STACKBUF_SIZE];
    erts_osenv_data_t key, value;
    int result;

    key.data = convert_key_to_native(key_term, key_stackbuf,
        STACKBUF_SIZE, &key.length);
    value.data = convert_value_to_native(value_term, value_stackbuf,
        STACKBUF_SIZE, &value.length);
    result = -1;

    if(value.data != NULL && key.data != NULL) {
        result = erts_osenv_put_native(env, &key, &value);
    }

    if(value.data != NULL && value.data != value_stackbuf) {
        erts_free(ERTS_ALC_T_TMP, value.data);
    }

    if(key.data != NULL && key.data != key_stackbuf) {
        erts_free(ERTS_ALC_T_TMP, key.data);
    }

    return result;
}

int erts_osenv_unset_term(erts_osenv_t *env, Eterm key_term) {
    char key_stackbuf[STACKBUF_SIZE];
    erts_osenv_data_t key;
    int result;

    key.data = convert_key_to_native(key_term, key_stackbuf,
        STACKBUF_SIZE, &key.length);
    result = -1;

    if(key.data != NULL) {
        result = erts_osenv_unset_native(env, &key);

        if(key.data != key_stackbuf) {
            erts_free(ERTS_ALC_T_TMP, key.data);
        }
    }

    return result;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

struct __env_foreach_native {
    erts_osenv_foreach_native_cb_t user_callback;
    void *user_state;
};

static int foreach_native_wrapper(env_rbtnode_t *node, void *_state, Sint reds) {
    struct __env_foreach_native *state = (struct __env_foreach_native*)_state;

    state->user_callback(state->user_state, &node->key, &node->value);
    return 1;
}

void erts_osenv_foreach_native(const erts_osenv_t *env, void *state,
        erts_osenv_foreach_native_cb_t callback) {
    struct __env_foreach_native wrapper_state;

    wrapper_state.user_callback = callback;
    wrapper_state.user_state = state;

    env_rbt_foreach(env->tree, foreach_native_wrapper, &wrapper_state);
}

int erts_osenv_get_native(const erts_osenv_t *env,
        const erts_osenv_data_t *key,
        erts_osenv_data_t *value) {
    env_rbtnode_t *node = env_rbt_lookup(env->tree, *key);

    if(node != NULL) {
        if(value != NULL) {
            if(node->value.length > value->length) {
                return -1;
            }

            sys_memcpy(value->data, node->value.data, node->value.length);
            value->length = node->value.length;
        }

        return 1;
    }

    return 0;
}

int erts_osenv_put_native(erts_osenv_t *env, const erts_osenv_data_t *key,
        const erts_osenv_data_t *value) {
    env_rbtnode_t *old_node, *new_node;

    new_node = erts_alloc(ERTS_ALC_T_ENVIRONMENT, sizeof(env_rbtnode_t) +
        key->length + value->length);

    new_node->key.data = (char*)(&new_node[1]);
    new_node->key.length = key->length;
    new_node->value.data = &((char*)new_node->key.data)[key->length];
    new_node->value.length = value->length;

    sys_memcpy(new_node->key.data, key->data, key->length);
    sys_memcpy(new_node->value.data, value->data, value->length);

    old_node = env_rbt_lookup(env->tree, *key);

    if(old_node != NULL) {
        env->content_size -= old_node->value.length;
        env->content_size -= old_node->key.length;
        env_rbt_replace(&env->tree, old_node, new_node);
    } else {
        env_rbt_insert(&env->tree, new_node);
        env->variable_count++;
    }

    env->content_size += new_node->value.length;
    env->content_size += new_node->key.length;

    if(old_node != NULL) {
        erts_free(ERTS_ALC_T_ENVIRONMENT, old_node);
    }

    return 1;
}

int erts_osenv_unset_native(erts_osenv_t *env, const erts_osenv_data_t *key) {
    env_rbtnode_t *old_node = env_rbt_lookup(env->tree, *key);

    if(old_node != NULL) {
        env->content_size -= old_node->value.length;
        env->content_size -= old_node->key.length;
        env->variable_count -= 1;

        env_rbt_delete(&env->tree, old_node);
        erts_free(ERTS_ALC_T_ENVIRONMENT, old_node);
        return 1;
    }

    return 0;
}
