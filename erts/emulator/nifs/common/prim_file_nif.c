/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2017-2018. All Rights Reserved.
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

#define STATIC_ERLANG_NIF 1

#include "erl_nif.h"
#include "config.h"
#include "sys.h"

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#include "erl_driver.h"
#include "prim_file_nif.h"

/* NIF interface declarations */
static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv *env, void* priv_data);

static ErlNifResourceType *efile_resource_type;

static ERL_NIF_TERM am_close;

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_continue;

static ERL_NIF_TERM am_file_info;

/* File modes */
static ERL_NIF_TERM am_read;
static ERL_NIF_TERM am_write;
static ERL_NIF_TERM am_exclusive;
static ERL_NIF_TERM am_append;
static ERL_NIF_TERM am_sync;
static ERL_NIF_TERM am_skip_type_check;

/* enum efile_access_t; read and write are defined above.*/
static ERL_NIF_TERM am_read_write;
static ERL_NIF_TERM am_none;

/* enum efile_advise_t */
static ERL_NIF_TERM am_normal;
static ERL_NIF_TERM am_random;
static ERL_NIF_TERM am_sequential;
static ERL_NIF_TERM am_will_need;
static ERL_NIF_TERM am_dont_need;
static ERL_NIF_TERM am_no_reuse;

/* enum efile_filetype_t */
static ERL_NIF_TERM am_device;
static ERL_NIF_TERM am_directory;
static ERL_NIF_TERM am_regular;
static ERL_NIF_TERM am_symlink;
static ERL_NIF_TERM am_other;

/* enum efile_seek_t, 'eof' marker. */
static ERL_NIF_TERM am_bof;
static ERL_NIF_TERM am_cur;
static ERL_NIF_TERM am_eof;

static ERL_NIF_TERM read_info_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM set_permissions_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM set_owner_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM set_time_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM read_link_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM list_dir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM make_hard_link_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM make_soft_link_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rename_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM make_dir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM del_file_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM del_dir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_device_cwd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_cwd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM set_cwd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM read_file_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM open_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* Internal ops */
static ERL_NIF_TERM delayed_close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_handle_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM altname_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* All file handle operations are passed through a wrapper that handles state
 * transitions, marking it as busy during the course of the operation, and
 * closing on completion if the owner died in the middle of an operation.
 *
 * This is pretty ugly but required as there's no way to tell when it's safe to
 * asynchronously close a file; the event could have fired just before landing
 * in a system call which will fail with EBADF at best or alias a newly opened
 * fd at worst.
 *
 * The old driver got away with enqueueing the close operation on the same
 * async queue as all of its other operations, but since dirty schedulers use a
 * single global queue there's no natural way to schedule an asynchronous close
 * "behind" other operations.
 *
 * The states may transition as follows:
 *
 * IDLE ->
 *      BUSY (file_handle_wrapper) |
 *      CLOSED (owner_death_callback)
 *
 * BUSY ->
 *      IDLE (file_handle_wrapper)
 *      CLOSED (close_nif_impl)
 *      CLOSE_PENDING (owner_death_callback)
 *
 * CLOSE_PENDING ->
 *      CLOSED (file_handle_wrapper)
 *
 * Should the owner of a file die, we can't close it immediately as that could
 * potentially block a normal scheduler. When entering the CLOSED state from
 * owner_death_callback, we will instead send a message to the erts_prim_file
 * process that will then close the file through delayed_close_nif. */

typedef ERL_NIF_TERM (*file_op_impl_t)(efile_data_t *d, ErlNifEnv *env,
    int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM file_handle_wrapper(file_op_impl_t operation, ErlNifEnv *env,
    int argc, const ERL_NIF_TERM argv[]);

#define WRAP_FILE_HANDLE_EXPORT(name) \
    static ERL_NIF_TERM name ## _impl (efile_data_t *d, ErlNifEnv *env, \
        int argc, const ERL_NIF_TERM argv[]);\
    static ERL_NIF_TERM name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) { \
        return file_handle_wrapper( name ## _impl , env, argc, argv); \
    }

WRAP_FILE_HANDLE_EXPORT(read_nif)
WRAP_FILE_HANDLE_EXPORT(write_nif)
WRAP_FILE_HANDLE_EXPORT(pread_nif)
WRAP_FILE_HANDLE_EXPORT(pwrite_nif)
WRAP_FILE_HANDLE_EXPORT(seek_nif)
WRAP_FILE_HANDLE_EXPORT(sync_nif)
WRAP_FILE_HANDLE_EXPORT(truncate_nif)
WRAP_FILE_HANDLE_EXPORT(allocate_nif)
WRAP_FILE_HANDLE_EXPORT(advise_nif)
WRAP_FILE_HANDLE_EXPORT(get_handle_nif)
WRAP_FILE_HANDLE_EXPORT(ipread_s32bu_p32bu_nif)

static ErlNifFunc nif_funcs[] = {
    /* File handle ops */
    {"open_nif", 2, open_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_nif", 1, close_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_nif", 2, read_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_nif", 2, write_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"pread_nif", 3, pread_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"pwrite_nif", 3, pwrite_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"seek_nif", 3, seek_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"sync_nif", 2, sync_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"truncate_nif", 1, truncate_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"allocate_nif", 3, allocate_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"advise_nif", 4, advise_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    /* Filesystem ops */
    {"make_hard_link_nif", 2, make_hard_link_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"make_soft_link_nif", 2, make_soft_link_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"rename_nif", 2, rename_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_info_nif", 2, read_info_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"set_permissions_nif", 2, set_permissions_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"set_owner_nif", 3, set_owner_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"set_time_nif", 4, set_time_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_link_nif", 1, read_link_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"list_dir_nif", 1, list_dir_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"make_dir_nif", 1, make_dir_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"del_file_nif", 1, del_file_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"del_dir_nif", 1, del_dir_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_device_cwd_nif", 1, get_device_cwd_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"set_cwd_nif", 1, set_cwd_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_cwd_nif", 0, get_cwd_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    /* These operations are equivalent to chained calls of other operations,
     * but have been moved down to avoid excessive rescheduling. */
    {"ipread_s32bu_p32bu_nif", 3, ipread_s32bu_p32bu_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_file_nif", 1, read_file_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    /* Internal ops. */
    {"get_handle_nif", 1, get_handle_nif},
    {"delayed_close_nif", 1, delayed_close_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"altname_nif", 1, altname_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
};

ERL_NIF_INIT(prim_file, nif_funcs, load, NULL, upgrade, unload)

static ErlNifPid erts_prim_file_pid;

static void owner_death_callback(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon);

static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM prim_file_pid)
{
    ErlNifResourceTypeInit callbacks;

    if(!enif_get_local_pid(env, prim_file_pid, &erts_prim_file_pid)) {
        ASSERT(!"bad pid passed to prim_file_nif");
    }

    am_close = enif_make_atom(env, "close");

    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_continue = enif_make_atom(env, "continue");

    am_read = enif_make_atom(env, "read");
    am_write = enif_make_atom(env, "write");
    am_exclusive = enif_make_atom(env, "exclusive");
    am_append = enif_make_atom(env, "append");
    am_sync = enif_make_atom(env, "sync");
    am_skip_type_check = enif_make_atom(env, "skip_type_check");

    am_read_write = enif_make_atom(env, "read_write");
    am_none = enif_make_atom(env, "none");

    am_normal = enif_make_atom(env, "normal");
    am_random = enif_make_atom(env, "random");
    am_sequential = enif_make_atom(env, "sequential");
    am_will_need = enif_make_atom(env, "will_need");
    am_dont_need = enif_make_atom(env, "dont_need");
    am_no_reuse = enif_make_atom(env, "no_reuse");

    am_device = enif_make_atom(env, "device");
    am_directory = enif_make_atom(env, "directory");
    am_regular = enif_make_atom(env, "regular");
    am_symlink = enif_make_atom(env, "symlink");
    am_other = enif_make_atom(env, "other");

    am_file_info = enif_make_atom(env, "file_info");

    am_bof = enif_make_atom(env, "bof");
    am_cur = enif_make_atom(env, "cur");
    am_eof = enif_make_atom(env, "eof");

    callbacks.down = owner_death_callback;
    callbacks.dtor = NULL;
    callbacks.stop = NULL;

    efile_resource_type = enif_open_resource_type_x(env, "efile", &callbacks,
        ERL_NIF_RT_CREATE, NULL);

    *priv_data = NULL;

    return 0;
}

static void unload(ErlNifEnv *env, void* priv_data)
{

}

static int upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if(*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if(*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if(load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}

static ERL_NIF_TERM posix_error_to_tuple(ErlNifEnv *env, posix_errno_t posix_errno) {
    ERL_NIF_TERM error = enif_make_atom(env, erl_errno_id(posix_errno));
    return enif_make_tuple2(env, am_error, error);
}

static int get_file_data(ErlNifEnv *env, ERL_NIF_TERM opaque, efile_data_t **d) {
    return enif_get_resource(env, opaque, efile_resource_type, (void **)d);
}

static ERL_NIF_TERM file_handle_wrapper(file_op_impl_t operation, ErlNifEnv *env,
        int argc, const ERL_NIF_TERM argv[]) {

    efile_data_t *d;

    enum efile_state_t previous_state;
    ERL_NIF_TERM result;

    if(argc < 1 || !get_file_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    }

    previous_state = erts_atomic32_cmpxchg_acqb(&d->state,
        EFILE_STATE_BUSY, EFILE_STATE_IDLE);

    if(previous_state == EFILE_STATE_IDLE) {
        result = operation(d, env, argc - 1, &argv[1]);

        previous_state = erts_atomic32_cmpxchg_relb(&d->state,
            EFILE_STATE_IDLE, EFILE_STATE_BUSY);

        ASSERT(previous_state != EFILE_STATE_IDLE);

        if(previous_state == EFILE_STATE_CLOSE_PENDING) {
            /* This is the only point where a change from CLOSE_PENDING is
             * possible, and we're running synchronously, so we can't race with
             * anything else here. */
            posix_errno_t ignored;

            erts_atomic32_set_acqb(&d->state, EFILE_STATE_CLOSED);
            efile_close(d, &ignored);
        }
    } else {
        /* CLOSE_PENDING should be impossible at this point since it requires
         * a transition from BUSY; the only valid state here is CLOSED. */
        ASSERT(previous_state == EFILE_STATE_CLOSED);

        result = posix_error_to_tuple(env, EINVAL);
    }

    return result;
}

/* This is a special close operation used by the erts_prim_file process for
 * cleaning up orphaned files. It differs from the ordinary close_nif in that
 * it only works for files that have already entered the CLOSED state. */
static ERL_NIF_TERM delayed_close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t ignored;
    efile_data_t *d;

    ASSERT(argc == 1);
    if(!get_file_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_read_acqb(&d->state) == EFILE_STATE_CLOSED);
    efile_close(d, &ignored);

    return am_ok;
}

static void owner_death_callback(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon) {
    efile_data_t *d = (efile_data_t*)obj;

    (void)env;
    (void)pid;
    (void)mon;

    for(;;) {
        enum efile_state_t previous_state;

        previous_state = erts_atomic32_cmpxchg_acqb(&d->state,
            EFILE_STATE_CLOSED, EFILE_STATE_IDLE);

        switch(previous_state) {
        case EFILE_STATE_IDLE:
            {
                /* We cannot close the file here as that could block a normal
                 * scheduler, so we tell erts_prim_file to do it for us.
                 *
                 * This can in turn become a bottleneck (especially in cases
                 * like NFS failure), but it's less problematic than blocking
                 * thread progress. */
                ERL_NIF_TERM message, file_ref;

                file_ref = enif_make_resource(env, d);
                message = enif_make_tuple2(env, am_close, file_ref);

                if(!enif_send(env, &erts_prim_file_pid, NULL, message)) {
                    ERTS_INTERNAL_ERROR("Failed to defer prim_file close.");
                }

                return;
            }
        case EFILE_STATE_CLOSE_PENDING:
        case EFILE_STATE_CLOSED:
            /* We're either already closed or managed to mark ourselves for
             * closure in the previous iteration. */
            return;
        case EFILE_STATE_BUSY:
            /* Schedule ourselves to be closed once the current operation
             * finishes, retrying the [IDLE -> CLOSED] transition in case we
             * narrowly passed the [BUSY -> IDLE] one. */
            erts_atomic32_cmpxchg_nob(&d->state,
                EFILE_STATE_CLOSE_PENDING, EFILE_STATE_BUSY);
            break;
        }
    }
}

static ERL_NIF_TERM efile_filetype_to_atom(enum efile_filetype_t type) {
    switch(type) {
        case EFILE_FILETYPE_DEVICE: return am_device;
        case EFILE_FILETYPE_DIRECTORY: return am_directory;
        case EFILE_FILETYPE_REGULAR: return am_regular;
        case EFILE_FILETYPE_SYMLINK: return am_symlink;
        case EFILE_FILETYPE_OTHER: return am_other;
    }

    return am_other;
}

static ERL_NIF_TERM efile_access_to_atom(enum efile_access_t type) {
    if(type & EFILE_ACCESS_READ && !(type & EFILE_ACCESS_WRITE)) {
        return am_read;
    } else if(type & EFILE_ACCESS_WRITE && !(type & EFILE_ACCESS_READ)) {
        return am_write;
    } else if(type & EFILE_ACCESS_READ_WRITE) {
        return am_read_write;
    }

    return am_none;
}

static enum efile_modes_t efile_translate_modelist(ErlNifEnv *env, ERL_NIF_TERM list) {
    enum efile_modes_t modes;
    ERL_NIF_TERM head, tail;

    modes = 0;

    while(enif_get_list_cell(env, list, &head, &tail)) {
        if(enif_is_identical(head, am_read)) {
            modes |= EFILE_MODE_READ;
        } else if(enif_is_identical(head, am_write)) {
            modes |= EFILE_MODE_WRITE;
        } else if(enif_is_identical(head, am_exclusive)) {
            modes |= EFILE_MODE_EXCLUSIVE;
        } else if(enif_is_identical(head, am_append)) {
            modes |= EFILE_MODE_APPEND;
        } else if(enif_is_identical(head, am_sync)) {
            modes |= EFILE_MODE_SYNC;
        } else if(enif_is_identical(head, am_skip_type_check)) {
            modes |= EFILE_MODE_SKIP_TYPE_CHECK;
        } else {
            /* Modes like 'raw', 'ram', 'delayed_writes' etc are handled
             * further up the chain. */
        }

        list = tail;
    }

    if(modes & (EFILE_MODE_APPEND | EFILE_MODE_EXCLUSIVE)) {
        /* 'append' and 'exclusive' are documented as "open for writing." */
        modes |= EFILE_MODE_WRITE;
    } else if(!(modes & EFILE_MODE_READ_WRITE)) {
        /* Defaulting to read if !(W|R) is undocumented, but specifically
         * tested against in file_SUITE. */
        modes |= EFILE_MODE_READ;
    }

    return modes;
}

static ERL_NIF_TERM open_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;
    efile_data_t *d;

    ErlNifPid controlling_process;
    enum efile_modes_t modes;
    ERL_NIF_TERM result;
    efile_path_t path;

    ASSERT(argc == 2);
    if(!enif_is_list(env, argv[1])) {
        return enif_make_badarg(env);
    }

    modes = efile_translate_modelist(env, argv[1]);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_open(&path, modes, efile_resource_type, &d))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    enif_self(env, &controlling_process);

    if(enif_monitor_process(env, d, &controlling_process, &d->monitor)) {
        /* We need to close the file manually as we haven't registered a
         * destructor. */
        posix_errno_t ignored;

        erts_atomic32_set_acqb(&d->state, EFILE_STATE_CLOSED);
        efile_close(d, &ignored);

        return posix_error_to_tuple(env, EINVAL);
    }

    /* Note that we do not call enif_release_resource at this point. While it's
     * normally safe to leave resource management to the GC, efile_close is a
     * blocking operation which must not be done in the GC callback, and we
     * can't defer it as the resource is gone as soon as it returns.
     *
     * We instead keep the resource alive until efile_close is called, after
     * which it's safe to leave things to the GC. If the controlling process
     * were to die before the user had a chance to close their file, the above
     * monitor will tell the erts_prim_file process to close it for them. */
    result = enif_make_resource(env, d);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    enum efile_state_t previous_state;
    efile_data_t *d;

    ASSERT(argc == 1);
    if(!get_file_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    }

    previous_state = erts_atomic32_cmpxchg_acqb(&d->state,
        EFILE_STATE_CLOSED, EFILE_STATE_IDLE);

    if(previous_state == EFILE_STATE_IDLE) {
        posix_errno_t error;

        enif_demonitor_process(env, d, &d->monitor);

        if(!efile_close(d, &error)) {
            return posix_error_to_tuple(env, error);
        }

        return am_ok;
    } else {
        /* CLOSE_PENDING should be impossible at this point since it requires
         * a transition from BUSY; the only valid state here is CLOSED. */
        ASSERT(previous_state == EFILE_STATE_CLOSED);

        return posix_error_to_tuple(env, EINVAL);
    }
}

static ERL_NIF_TERM read_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    Sint64 bytes_read, block_size;
    SysIOVec read_vec[1];
    ErlNifBinary result;

    ASSERT(argc == 1);
    if(!enif_is_number(env, argv[0])) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[0], &block_size) || block_size < 0) {
        return posix_error_to_tuple(env, EINVAL);
    }

    if(!enif_alloc_binary(block_size, &result)) {
        return posix_error_to_tuple(env, ENOMEM);
    }

    read_vec[0].iov_base = result.data;
    read_vec[0].iov_len = result.size;

    bytes_read = efile_readv(d, read_vec, 1);
    ASSERT(bytes_read <= block_size);

    if(bytes_read < 0) {
        enif_release_binary(&result);
        return posix_error_to_tuple(env, d->posix_errno);
    } else if(bytes_read == 0) {
        enif_release_binary(&result);
        return am_eof;
    }

    if(bytes_read < block_size && !enif_realloc_binary(&result, bytes_read)) {
        ERTS_INTERNAL_ERROR("Failed to shrink read result.");
    }

    return enif_make_tuple2(env, am_ok, enif_make_binary(env, &result));
}

static ERL_NIF_TERM write_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifIOVec vec, *input = &vec;
    Sint64 bytes_written;
    ERL_NIF_TERM tail;

    ASSERT(argc == 1);
    if(!enif_inspect_iovec(env, 64, argv[0], &tail, &input)) {
        return enif_make_badarg(env);
    }

    bytes_written = efile_writev(d, input->iov, input->iovcnt);

    if(bytes_written < 0) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    if(!enif_is_empty_list(env, tail)) {
        ASSERT(bytes_written > 0);
        return enif_make_tuple2(env, am_continue, tail);
    }

    return am_ok;
}

static ERL_NIF_TERM pread_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    Sint64 bytes_read, block_size, offset;
    SysIOVec read_vec[1];
    ErlNifBinary result;

    ASSERT(argc == 2);
    if(!enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[0], &offset) ||
       !enif_get_int64(env, argv[1], &block_size) ||
       (offset < 0 || block_size < 0)) {
        return posix_error_to_tuple(env, EINVAL);
    }

    if(!enif_alloc_binary(block_size, &result)) {
        return posix_error_to_tuple(env, ENOMEM);
    }

    read_vec[0].iov_base = result.data;
    read_vec[0].iov_len = result.size;

    bytes_read = efile_preadv(d, offset, read_vec, 1);

    if(bytes_read < 0) {
        enif_release_binary(&result);
        return posix_error_to_tuple(env, d->posix_errno);
    } else if(bytes_read == 0) {
        enif_release_binary(&result);
        return am_eof;
    }

    if(bytes_read < block_size && !enif_realloc_binary(&result, bytes_read)) {
        ERTS_INTERNAL_ERROR("Failed to shrink pread result.");
    }

    return enif_make_tuple2(env, am_ok, enif_make_binary(env, &result));
}

static ERL_NIF_TERM pwrite_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifIOVec vec, *input = &vec;
    Sint64 bytes_written, offset;
    ERL_NIF_TERM tail;

    ASSERT(argc == 2);
    if(!enif_is_number(env, argv[0])
       || !enif_inspect_iovec(env, 64, argv[1], &tail, &input)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[0], &offset) || offset < 0) {
        return posix_error_to_tuple(env, EINVAL);
    }

    bytes_written = efile_pwritev(d, offset, input->iov, input->iovcnt);

    if(bytes_written < 0) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    if(!enif_is_empty_list(env, tail)) {
        ASSERT(bytes_written > 0);
        return enif_make_tuple3(env, am_continue,
            enif_make_int64(env, bytes_written), tail);
    }

    return am_ok;
}

static ERL_NIF_TERM seek_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    Sint64 new_position, offset;
    enum efile_seek_t seek;

    ASSERT(argc == 2);
    if(!enif_get_int64(env, argv[1], &offset)) {
        return enif_make_badarg(env);
    }

    if(enif_is_identical(argv[0], am_bof)) {
        seek = EFILE_SEEK_BOF;
    } else if(enif_is_identical(argv[0], am_cur)) {
        seek = EFILE_SEEK_CUR;
    } else if(enif_is_identical(argv[0], am_eof)) {
        seek = EFILE_SEEK_EOF;
    } else {
        return enif_make_badarg(env);
    }

    if(!efile_seek(d, seek, offset, &new_position)) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    return enif_make_tuple2(env, am_ok, enif_make_uint64(env, new_position));
}

static ERL_NIF_TERM sync_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int data_only;

    ASSERT(argc == 1);
    if(!enif_get_int(env, argv[0], &data_only)) {
        return enif_make_badarg(env);
    }

    if(!efile_sync(d, data_only)) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM truncate_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ASSERT(argc == 0);

    if(!efile_truncate(d)) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM allocate_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    Sint64 offset, length;

    ASSERT(argc == 2);
    if(!enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[0], &offset) ||
       !enif_get_int64(env, argv[1], &length) ||
       (offset < 0 || length < 0)) {
        return posix_error_to_tuple(env, EINVAL);
    }

    if(!efile_allocate(d, offset, length)) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM advise_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    enum efile_advise_t advise;
    Sint64 offset, length;

    ASSERT(argc == 3);
    if(!enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[0], &offset) ||
       !enif_get_int64(env, argv[1], &length) ||
       (offset < 0 || length < 0)) {
        return posix_error_to_tuple(env, EINVAL);
    }

    if(enif_is_identical(argv[2], am_normal)) {
        advise = EFILE_ADVISE_NORMAL;
    } else if(enif_is_identical(argv[2], am_random)) {
        advise = EFILE_ADVISE_RANDOM;
    } else if(enif_is_identical(argv[2], am_sequential)) {
        advise = EFILE_ADVISE_SEQUENTIAL;
    } else if(enif_is_identical(argv[2], am_will_need)) {
        advise = EFILE_ADVISE_WILL_NEED;
    } else if(enif_is_identical(argv[2], am_dont_need)) {
        advise = EFILE_ADVISE_DONT_NEED;
    } else if(enif_is_identical(argv[2], am_no_reuse)) {
        advise = EFILE_ADVISE_NO_REUSE;
    } else {
        /* The tests check for EINVAL instead of badarg. Sigh. */
        return posix_error_to_tuple(env, EINVAL);
    }

    if(!efile_advise(d, offset, length, advise)) {
        return posix_error_to_tuple(env, d->posix_errno);
    }

    return am_ok;
}

/* This undocumented function reads a pointer and then reads the data block
 * described by said pointer. It was reverse-engineered from the old
 * implementation so while all tests pass it may not be entirely correct. Our
 * current understanding is as follows:
 *
 * Pointer layout:
 *
 *     <<Size:1/integer-unit:32, Offset:1/integer-unit:32>>
 *
 * Where Offset is the -absolute- address to the data block.
 *
 * *) If we fail to read the pointer block in its entirety, we return eof.
 * *) If the provided max_payload_size is larger than Size, we return eof.
 * *) If we fail to read any data whatsoever at Offset, we return
 *    {ok, {Size, Offset, eof}}
 * *) Otherwise, we return {ok, {Size, Offset, Data}}. Note that the size
 *    of Data may be smaller than Size if we encounter EOF before we could
 *    read the entire block.
 *
 * On errors we'll return {error, posix()} regardless of whether they
 * happened before or after reading the pointer block. */
static ERL_NIF_TERM ipread_s32bu_p32bu_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    Sint64 payload_offset, payload_size;

    SysIOVec read_vec[1];
    Sint64 bytes_read;

    ErlNifBinary payload;

    ASSERT(argc == 2);
    if(!enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }

    {
        Sint64 max_payload_size, pointer_offset;
        unsigned char pointer_block[8];

        if(!enif_get_int64(env, argv[0], &pointer_offset) ||
           !enif_get_int64(env, argv[1], &max_payload_size) ||
           (pointer_offset < 0 || max_payload_size >= 1u << 31)) {
            return posix_error_to_tuple(env, EINVAL);
        }

        read_vec[0].iov_base = pointer_block;
        read_vec[0].iov_len = sizeof(pointer_block);

        bytes_read = efile_preadv(d, pointer_offset, read_vec, 1);

        if(bytes_read < 0) {
            return posix_error_to_tuple(env, d->posix_errno);
        } else if(bytes_read < sizeof(pointer_block)) {
            return am_eof;
        }

        payload_size = (Uint32)get_int32(&pointer_block[0]);
        payload_offset = (Uint32)get_int32(&pointer_block[4]);

        if(payload_size > max_payload_size) {
            return am_eof;
        }
    }

    if(!enif_alloc_binary(payload_size, &payload)) {
        return posix_error_to_tuple(env, ENOMEM);
    }

    read_vec[0].iov_base = payload.data;
    read_vec[0].iov_len = payload.size;

    bytes_read = efile_preadv(d, payload_offset, read_vec, 1);

    if(bytes_read < 0) {
        enif_release_binary(&payload);
        return posix_error_to_tuple(env, d->posix_errno);
    } else if(bytes_read == 0) {
        enif_release_binary(&payload);

        return enif_make_tuple2(env, am_ok,
                enif_make_tuple3(env,
                    enif_make_uint(env, payload_size),
                    enif_make_uint(env, payload_offset),
                    am_eof));
    }

    if(bytes_read < payload.size && !enif_realloc_binary(&payload, bytes_read)) {
        ERTS_INTERNAL_ERROR("Failed to shrink ipread payload.");
    }

    return enif_make_tuple2(env, am_ok,
        enif_make_tuple3(env,
            enif_make_uint(env, payload_size),
            enif_make_uint(env, payload_offset),
            enif_make_binary(env, &payload)));
}

static ERL_NIF_TERM get_handle_nif_impl(efile_data_t *d, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ASSERT(argc == 0);

    return efile_get_handle(env, d);
}

static ERL_NIF_TERM read_info_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_fileinfo_t info = {0};
    efile_path_t path;
    int follow_links;

    ASSERT(argc == 2);
    if(!enif_get_int(env, argv[1], &follow_links)) {
        return enif_make_badarg(env);
    }

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_read_info(&path, follow_links, &info))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    /* #file_info as declared in file.hrl */
    return enif_make_tuple(env, 14,
        am_file_info,
        enif_make_uint64(env, info.size),
        efile_filetype_to_atom(info.type),
        efile_access_to_atom(info.access),
        enif_make_int64(env, MAX(EFILE_MIN_FILETIME, info.a_time)),
        enif_make_int64(env, MAX(EFILE_MIN_FILETIME, info.m_time)),
        enif_make_int64(env, MAX(EFILE_MIN_FILETIME, info.c_time)),
        enif_make_uint(env, info.mode),
        enif_make_uint(env, info.links),
        enif_make_uint(env, info.major_device),
        enif_make_uint(env, info.minor_device),
        enif_make_uint(env, info.inode),
        enif_make_uint(env, info.uid),
        enif_make_uint(env, info.gid)
    );
}

static ERL_NIF_TERM set_permissions_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;
    unsigned int permissions;

    ASSERT(argc == 2);
    if(!enif_get_uint(env, argv[1], &permissions)) {
        return enif_make_badarg(env);
    }

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_set_permissions(&path, permissions))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM set_owner_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;
    int uid, gid;

    ASSERT(argc == 3);
    if(!enif_get_int(env, argv[1], &uid) || !enif_get_int(env, argv[2], &gid)) {
        return enif_make_badarg(env);
    }

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_set_owner(&path, uid, gid))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM set_time_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    Sint64 accessed, modified, created;
    efile_path_t path;

    ASSERT(argc == 4);
    if(!enif_get_int64(env, argv[1], &accessed)
       || !enif_get_int64(env, argv[2], &modified)
       || !enif_get_int64(env, argv[3], &created)) {
        return enif_make_badarg(env);
    }

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_set_time(&path, accessed, modified, created))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM read_link_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;
    ERL_NIF_TERM result;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_read_link(env, &path, &result))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM list_dir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;
    ERL_NIF_TERM result;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_list_dir(env, &path, &result))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM rename_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t existing_path, new_path;

    ASSERT(argc == 2);

    if((posix_errno = efile_marshal_path(env, argv[0], &existing_path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_marshal_path(env, argv[1], &new_path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_rename(&existing_path, &new_path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM make_hard_link_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t existing_path, new_path;

    ASSERT(argc == 2);

    if((posix_errno = efile_marshal_path(env, argv[0], &existing_path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_marshal_path(env, argv[1], &new_path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_make_hard_link(&existing_path, &new_path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM make_soft_link_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t existing_path, new_path;

    ASSERT(argc == 2);

    if((posix_errno = efile_marshal_path(env, argv[0], &existing_path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_marshal_path(env, argv[1], &new_path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_make_soft_link(&existing_path, &new_path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM make_dir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_make_dir(&path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM del_file_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_del_file(&path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM del_dir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_del_dir(&path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

static ERL_NIF_TERM get_device_cwd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    ERL_NIF_TERM result;
    int device_index;

    ASSERT(argc == 1);
    if(!enif_get_int(env, argv[0], &device_index)) {
        return enif_make_badarg(env);
    }

    if((posix_errno = efile_get_device_cwd(env, device_index, &result))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM get_cwd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;
    ERL_NIF_TERM result;

    ASSERT(argc == 0);

    if((posix_errno = efile_get_cwd(env, &result))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM set_cwd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_set_cwd(&path))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return am_ok;
}

/** @brief Reads an entire file into \c result, stopping after \c size bytes or
 * EOF. It will read until EOF if size is 0. */
static posix_errno_t read_file(efile_data_t *d, size_t size, ErlNifBinary *result) {
    size_t initial_buffer_size;
    ssize_t bytes_read;

    if(size == 0) {
        initial_buffer_size = 16 << 10;
    } else {
        initial_buffer_size = size;
    }

    if(!enif_alloc_binary(initial_buffer_size, result)) {
        return ENOMEM;
    }

    bytes_read = 0;

    for(;;) {
        ssize_t block_bytes_read;
        SysIOVec read_vec[1];

        read_vec[0].iov_base = result->data + bytes_read;
        read_vec[0].iov_len = result->size - bytes_read;

        block_bytes_read = efile_readv(d, read_vec, 1);

        if(block_bytes_read < 0) {
            enif_release_binary(result);
            return d->posix_errno;
        }

        bytes_read += block_bytes_read;

        if(block_bytes_read < (result->size - bytes_read)) {
            /* EOF */
            break;
        } else if(bytes_read == size) {
            break;
        }

        if(!enif_realloc_binary(result, bytes_read * 2)) {
            enif_release_binary(result);
            return ENOMEM;
        }
    }

    /* The file may have shrunk since we queried its size, so we have to do
     * this even when the size is known. */
    if(bytes_read < result->size && !enif_realloc_binary(result, bytes_read)) {
        ERTS_INTERNAL_ERROR("Failed to shrink read_file result.");
    }

    return 0;
}

static ERL_NIF_TERM read_file_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno, ignored;

    efile_fileinfo_t info = {0};
    efile_path_t path;
    efile_data_t *d;

    ErlNifBinary result;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_read_info(&path, 1, &info))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_open(&path, EFILE_MODE_READ, efile_resource_type, &d))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    posix_errno = read_file(d, info.size, &result);

    erts_atomic32_set_acqb(&d->state, EFILE_STATE_CLOSED);
    efile_close(d, &ignored);

    if(posix_errno) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return enif_make_tuple2(env, am_ok, enif_make_binary(env, &result));
}

static ERL_NIF_TERM altname_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    posix_errno_t posix_errno;

    efile_path_t path;
    ERL_NIF_TERM result;

    ASSERT(argc == 1);

    if((posix_errno = efile_marshal_path(env, argv[0], &path))) {
        return posix_error_to_tuple(env, posix_errno);
    } else if((posix_errno = efile_altname(env, &path, &result))) {
        return posix_error_to_tuple(env, posix_errno);
    }

    return enif_make_tuple2(env, am_ok, result);
}
