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

#include "erl_nif.h"
#include "config.h"
#include "sys.h"

#include "prim_file_nif.h"

#include <winioctl.h>
#include <windows.h>
#include <strsafe.h>
#include <wchar.h>

#define IS_SLASH(a)  ((a) == L'\\' || (a) == L'/')

#define FILE_SHARE_FLAGS (FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)

/* Long paths can either be in the file (?) or the device (.) namespace. UNC
 * paths are always in the file namespace. */
#define LP_FILE_PREFIX L"\\\\?\\"
#define LP_DEV_PREFIX L"\\\\.\\"
#define LP_UNC_PREFIX (LP_FILE_PREFIX L"UNC\\")

#define LP_PREFIX_SIZE (sizeof(LP_FILE_PREFIX) - sizeof(WCHAR))
#define LP_PREFIX_LENGTH (LP_PREFIX_SIZE / sizeof(WCHAR))

#define LP_UNC_PREFIX_SIZE (sizeof(LP_UNC_PREFIX) - sizeof(WCHAR))
#define LP_UNC_PREFIX_LENGTH (LP_UNC_PREFIX_SIZE / sizeof(WCHAR))

#define IS_LONG_PATH(length, data) \
    ((length) >= LP_PREFIX_LENGTH && \
         (!sys_memcmp((data), LP_FILE_PREFIX, LP_PREFIX_SIZE) || \
          !sys_memcmp((data), LP_DEV_PREFIX, LP_PREFIX_SIZE)))

#define IS_LONG_UNC_PATH(length, data) \
    ((length) >= LP_UNC_PREFIX_LENGTH && \
         !sys_memcmp((data), LP_UNC_PREFIX, LP_UNC_PREFIX_SIZE))

#define PATH_LENGTH(path) (path->size / sizeof(WCHAR) - 1)

#define ASSERT_PATH_FORMAT(path) \
    do { \
        ASSERT(IS_LONG_PATH(PATH_LENGTH(path), (path)->data)); \
        ASSERT(PATH_LENGTH(path) == wcslen((WCHAR*)path->data)); \
    } while(0)

#define TICKS_PER_SECOND (10000000ULL)
#define EPOCH_DIFFERENCE (11644473600LL)

#define FILETIME_TO_EPOCH(epoch, ft) \
    do { \
        ULARGE_INTEGER ull; \
        ull.LowPart  = (ft).dwLowDateTime; \
        ull.HighPart = (ft).dwHighDateTime; \
        (epoch) = ((ull.QuadPart / TICKS_PER_SECOND) - EPOCH_DIFFERENCE); \
    } while(0)

#define EPOCH_TO_FILETIME(ft, epoch) \
    do { \
        ULARGE_INTEGER ull; \
        ull.QuadPart = (((epoch) + EPOCH_DIFFERENCE) * TICKS_PER_SECOND); \
        (ft).dwLowDateTime = ull.LowPart; \
        (ft).dwHighDateTime = ull.HighPart; \
    } while(0)

typedef struct {
    efile_data_t common;
    HANDLE handle;
} efile_win_t;

static int windows_to_posix_errno(DWORD last_error);

static int has_invalid_null_termination(const ErlNifBinary *path) {
    const WCHAR *null_pos, *end_pos;

    null_pos = wmemchr((const WCHAR*)path->data, L'\0', path->size);
    end_pos = (const WCHAR*)&path->data[path->size] - 1;

    if(null_pos == NULL) {
        return 1;
    }

    /* prim_file:internal_name2native sometimes feeds us data that is "doubly"
     * NUL-terminated, so we'll accept any number of trailing NULs so long as
     * they aren't interrupted by anything else. */
    while(null_pos < end_pos && (*null_pos) == L'\0') {
        null_pos++;
    }

    return null_pos != end_pos;
}

static posix_errno_t get_full_path(ErlNifEnv *env, WCHAR *input, efile_path_t *result) {
    DWORD maximum_length, actual_length;
    int add_long_prefix;

    maximum_length = GetFullPathNameW(input, 0, NULL, NULL);
    add_long_prefix = 0;

    if(maximum_length == 0) {
        /* POSIX doesn't have the concept of a "path error" in the same way
         * Windows does, so we'll return ENOENT since that's what most POSIX
         * APIs would return if they were fed such garbage. */
        return ENOENT;
    }

    maximum_length += MAX(LP_PREFIX_LENGTH, LP_UNC_PREFIX_LENGTH);

    if(!enif_alloc_binary(maximum_length * sizeof(WCHAR), result)) {
        return ENOMEM;
    }

    actual_length = GetFullPathNameW(input, maximum_length, (WCHAR*)result->data, NULL);

    if(actual_length < maximum_length) {
        int is_long_path, maybe_unc_path;
        WCHAR *path_start;

        /* The APIs we use have varying path length limits and sometimes
         * behave differently when given a long-path prefix, so it's simplest
         * to always use long paths. */

        is_long_path = IS_LONG_PATH(actual_length, result->data);
        maybe_unc_path = !sys_memcmp(result->data, L"\\\\", sizeof(WCHAR) * 2);

        if(maybe_unc_path && !is_long_path) {
            /* \\localhost\c$\gurka -> \\?\UNC\localhost\c$\gurka
             *
             * Note that the length is reduced by 2 as the "\\" is replaced by
             * the UNC prefix */
            sys_memmove(result->data + LP_UNC_PREFIX_SIZE,
                &((WCHAR*)result->data)[2],
                (actual_length + 1 - 2) * sizeof(WCHAR));
            sys_memcpy(result->data, LP_UNC_PREFIX, LP_UNC_PREFIX_SIZE);
            actual_length += LP_UNC_PREFIX_LENGTH - 2;
        } else if(!is_long_path) {
            /* C:\gurka -> \\?\C:\gurka */
            sys_memmove(result->data + LP_PREFIX_SIZE, result->data,
                (actual_length + 1) * sizeof(WCHAR));
            sys_memcpy(result->data, LP_FILE_PREFIX, LP_PREFIX_SIZE);
            actual_length += LP_PREFIX_LENGTH;
        }

        path_start = (WCHAR*)result->data;

        /* We're removing trailing slashes since quite a few APIs refuse to
         * work with them, and none require them. We only check the last
         * character since GetFullPathNameW folds slashes together. */
        if(IS_SLASH(path_start[actual_length - 1])) {
            if(path_start[actual_length - 2] != L':') {
                path_start[actual_length - 1] = L'\0';
                actual_length--;
            }
        }

        if(!enif_realloc_binary(result, (actual_length + 1) * sizeof(WCHAR))) {
            enif_release_binary(result);
            return ENOMEM;
        }

        enif_make_binary(env, result);
        return 0;
    }

    /* We may end up here if the current directory changes to something longer
     * between/during GetFullPathName. There's nothing sensible we can do about
     * this. */

    enif_release_binary(result);

    return EINVAL;
}

posix_errno_t efile_marshal_path(ErlNifEnv *env, ERL_NIF_TERM path, efile_path_t *result) {
    ErlNifBinary raw_path;

    if(!enif_inspect_binary(env, path, &raw_path)) {
        return EINVAL;
    } else if(raw_path.size % sizeof(WCHAR)) {
        return EINVAL;
    }

    if(has_invalid_null_termination(&raw_path)) {
        return EINVAL;
    }

    return get_full_path(env, (WCHAR*)raw_path.data, result);
}

ERL_NIF_TERM efile_get_handle(ErlNifEnv *env, efile_data_t *d) {
    efile_win_t *w = (efile_win_t*)d;

    ERL_NIF_TERM result;
    unsigned char *bits;

    bits = enif_make_new_binary(env, sizeof(w->handle), &result);
    memcpy(bits, &w->handle, sizeof(w->handle));

    return result;
}

/** @brief Converts a native path to the preferred form in "erlang space,"
 * without path-prefixes, forward-slashes, or NUL terminators. */
static int normalize_path_result(ErlNifBinary *path) {
    WCHAR *path_iterator, *path_start, *path_end;
    int length;

    path_start = (WCHAR*)path->data;
    length = wcslen(path_start);

    ASSERT(length < path->size / sizeof(WCHAR));

    /* Get rid of the long-path prefix, if present. */

    if(IS_LONG_UNC_PATH(length, path_start)) {
        /* The first two characters (\\) are the same for both long and short
         * UNC paths. */
        sys_memmove(&path_start[2], &path_start[LP_UNC_PREFIX_LENGTH],
            (length - LP_UNC_PREFIX_LENGTH) * sizeof(WCHAR));

        length -= LP_UNC_PREFIX_LENGTH - 2;
    } else if(IS_LONG_PATH(length, path_start)) {
        length -= LP_PREFIX_LENGTH;

        sys_memmove(path_start, &path_start[LP_PREFIX_LENGTH],
            length * sizeof(WCHAR));
    }

    path_end = &path_start[length];
    path_iterator = path_start;

    /* Convert drive letters to lowercase, if present. */
    if(length >= 2 && path_start[1] == L':') {
        WCHAR drive_letter = path_start[0];

        if(drive_letter >= L'A' && drive_letter <= L'Z') {
            path_start[0] = drive_letter - L'A' + L'a';
        }
    }

    while(path_iterator < path_end) {
        if(*path_iterator == L'\\') {
            *path_iterator = L'/';
        }

        path_iterator++;
    }

    /* Truncate the result to its actual length; we don't want to include the
     * NUL terminator. */
    return enif_realloc_binary(path, length * sizeof(WCHAR));
}

/* @brief Checks whether all the given attributes are set on the object at the
 * given path. Note that it assumes false on errors. */
static int has_file_attributes(const efile_path_t *path, DWORD mask) {
    DWORD attributes = GetFileAttributesW((WCHAR*)path->data);

    if(attributes == INVALID_FILE_ATTRIBUTES) {
        return 0;
    }

    return !!((attributes & mask) == mask);
}

static int is_ignored_name(int name_length, const WCHAR *name) {
    if(name_length == 1 && name[0] == L'.') {
        return 1;
    } else if(name_length == 2 && !sys_memcmp(name, L"..", 2 * sizeof(WCHAR))) {
        return 1;
    }

    return 0;
}

static int get_drive_number(const efile_path_t *path) {
    const WCHAR *path_start;
    int length;

    ASSERT_PATH_FORMAT(path);

    path_start = (WCHAR*)path->data + LP_PREFIX_LENGTH;
    length = PATH_LENGTH(path) - LP_PREFIX_LENGTH;

    if(length >= 2 && path_start[1] == L':') {
        WCHAR drive_letter = path_start[0];

        if(drive_letter >= L'A' && drive_letter <= L'Z') {
            return drive_letter - L'A' + 1;
        } else if(drive_letter >= L'a' && drive_letter <= L'z') {
            return drive_letter - L'a' + 1;
        }
    }

    return -1;
}

/* @brief Checks whether two *paths* are on the same mount point; they don't
 * have to refer to existing or accessible files/directories. */
static int has_same_mount_point(const efile_path_t *path_a, const efile_path_t *path_b) {
    WCHAR *mount_a, *mount_b;
    int result = 0;

    mount_a = enif_alloc(path_a->size);
    mount_b = enif_alloc(path_b->size);

    if(mount_a != NULL && mount_b != NULL) {
        int length_a, length_b;

        length_a = PATH_LENGTH(path_a);
        length_b = PATH_LENGTH(path_b);

        if(GetVolumePathNameW((WCHAR*)path_a->data, mount_a, length_a)) {
            ASSERT(wcslen(mount_a) <= length_a);

            if(GetVolumePathNameW((WCHAR*)path_b->data, mount_b, length_b)) {
                ASSERT(wcslen(mount_b) <= length_b);

                result = !_wcsicmp(mount_a, mount_b);
            }
        }
    }

    if(mount_b != NULL) {
        enif_free(mount_b);
    }

    if(mount_a != NULL) {
        enif_free(mount_a);
    }

    return result;
}

/* Mirrors the PathIsRootW function of the shell API, but doesn't choke on
 * paths longer than MAX_PATH. */
static int is_path_root(const efile_path_t *path) {
    const WCHAR *path_start, *path_end, *path_iterator;
    int length;

    ASSERT_PATH_FORMAT(path);

    if(!IS_LONG_UNC_PATH(PATH_LENGTH(path), path->data)) {
        path_start = (WCHAR*)path->data + LP_PREFIX_LENGTH;
        length = PATH_LENGTH(path) - LP_PREFIX_LENGTH;

        /* A single \ refers to the root of the current working directory. */
        if(length == 1) {
            return IS_SLASH(path_start[0]);
        }

        /* Drive letter. */
        if(length == 3 && iswalpha(path_start[0]) && path_start[1] == L':') {
            return IS_SLASH(path_start[2]);
        }

        return 0;
    }

    /* Check whether we're a UNC root, eg. \\server, \\server\share */

    path_start = (WCHAR*)path->data + LP_UNC_PREFIX_LENGTH;
    length = PATH_LENGTH(path) - LP_UNC_PREFIX_LENGTH;

    path_end = &path_start[length];
    path_iterator = path_start;

    /* Server name must be at least one character. */
    if(length <= 1) {
        return 0;
    }

    /* Slide to the slash between the server and share names, if present. */
    while(path_iterator < path_end && !IS_SLASH(*path_iterator)) {
        path_iterator++;
    }

    /* Slide past the end of the string, stopping at the first slash we
     * encounter. */
    do {
        path_iterator++;
    }  while(path_iterator < path_end && !IS_SLASH(*path_iterator));

    /* If we're past the end of the string and it didnt't end with a slash,
     * then we're a root path. */
    return path_iterator >= path_end && !IS_SLASH(path_start[length - 1]);
}

posix_errno_t efile_open(const efile_path_t *path, enum efile_modes_t modes,
        ErlNifResourceType *nif_type, efile_data_t **d) {

    DWORD attributes, access_flags, open_mode;
    HANDLE handle;

    ASSERT_PATH_FORMAT(path);

    access_flags = 0;
    open_mode = 0;

    if(modes & EFILE_MODE_READ && !(modes & EFILE_MODE_WRITE)) {
        access_flags = GENERIC_READ;
        open_mode = OPEN_EXISTING;
    } else if(modes & EFILE_MODE_WRITE && !(modes & EFILE_MODE_READ)) {
        access_flags = GENERIC_WRITE;
        open_mode = CREATE_ALWAYS;
    } else if(modes & EFILE_MODE_READ_WRITE) {
        access_flags = GENERIC_READ | GENERIC_WRITE;
        open_mode = OPEN_ALWAYS;
    } else {
        return EINVAL;
    }

    if(modes & EFILE_MODE_APPEND) {
        access_flags |= FILE_APPEND_DATA;
        open_mode = OPEN_ALWAYS;
    }

    if(modes & EFILE_MODE_EXCLUSIVE) {
        open_mode = CREATE_NEW;
    }

    if(modes & EFILE_MODE_SYNC) {
        attributes = FILE_FLAG_WRITE_THROUGH;
    } else {
        attributes = FILE_ATTRIBUTE_NORMAL;
    }

    handle = CreateFileW((WCHAR*)path->data, access_flags,
        FILE_SHARE_FLAGS, NULL, open_mode, attributes, NULL);

    if(handle != INVALID_HANDLE_VALUE) {
        efile_win_t *w;

        w = (efile_win_t*)enif_alloc_resource(nif_type, sizeof(efile_win_t));
        w->handle = handle;

        EFILE_INIT_RESOURCE(&w->common, modes);
        (*d) = &w->common;

        return 0;
    } else {
        DWORD last_error = GetLastError();

        /* Rewrite all failures on directories to EISDIR to match the old
         * driver. */
        if(has_file_attributes(path, FILE_ATTRIBUTE_DIRECTORY)) {
            return EISDIR;
        }

        return windows_to_posix_errno(last_error);
    }
}

int efile_close(efile_data_t *d, posix_errno_t *error) {
    efile_win_t *w = (efile_win_t*)d;
    HANDLE handle;

    ASSERT(enif_thread_type() == ERL_NIF_THR_DIRTY_IO_SCHEDULER);
    ASSERT(erts_atomic32_read_nob(&d->state) == EFILE_STATE_CLOSED);
    ASSERT(w->handle != INVALID_HANDLE_VALUE);

    handle = w->handle;
    w->handle = INVALID_HANDLE_VALUE;

    enif_release_resource(d);

    if(!CloseHandle(handle)) {
        *error = windows_to_posix_errno(GetLastError());
        return 0;
    }

    return 1;
}

static void shift_overlapped(OVERLAPPED *overlapped, DWORD shift) {
    LARGE_INTEGER offset;

    ASSERT(shift >= 0);

    offset.HighPart = overlapped->OffsetHigh;
    offset.LowPart = overlapped->Offset;

    /* ~(Uint64)0 is a magic value ("append to end of file") which needs to be
     * preserved. Other positions resulting in overflow would have errored out
     * just prior to this point. */
    if(offset.QuadPart != ERTS_UINT64_MAX) {
        offset.QuadPart += shift;
    }

    /* All unused fields must be zeroed for the next call. */
    sys_memset(overlapped, 0, sizeof(*overlapped));
    overlapped->OffsetHigh = offset.HighPart;
    overlapped->Offset = offset.LowPart;
}

static void shift_iov(SysIOVec **iov, int *iovlen, DWORD shift) {
    SysIOVec *head_vec = (*iov);

    ASSERT(shift >= 0);

    while(shift > 0) {
        ASSERT(head_vec < &(*iov)[*iovlen]);

        if(shift < head_vec->iov_len) {
            head_vec->iov_base = (char*)head_vec->iov_base + shift;
            head_vec->iov_len -= shift;
            break;
        } else {
            shift -= head_vec->iov_len;
            head_vec++;
        }
    }

    (*iovlen) -= head_vec - (*iov);
    (*iov) = head_vec;
}

typedef BOOL (WINAPI *io_op_t)(HANDLE, LPVOID, DWORD, LPDWORD, LPOVERLAPPED);

static Sint64 internal_sync_io(efile_win_t *w, io_op_t operation,
        SysIOVec *iov, int iovlen, OVERLAPPED *overlapped) {

    Sint64 bytes_processed = 0;

    for(;;) {
        DWORD block_bytes_processed, last_error;
        BOOL succeeded;

        if(iovlen < 1) {
            return bytes_processed;
        }

        succeeded = operation(w->handle, iov->iov_base, iov->iov_len,
            &block_bytes_processed, overlapped);
        last_error = GetLastError();

        if(!succeeded && (last_error != ERROR_HANDLE_EOF)) {
            w->common.posix_errno = windows_to_posix_errno(last_error);
            return -1;
        } else if(block_bytes_processed == 0) {
            /* EOF */
            return bytes_processed;
        }

        if(overlapped != NULL) {
            shift_overlapped(overlapped, block_bytes_processed);
        }

        shift_iov(&iov, &iovlen, block_bytes_processed);

        bytes_processed += block_bytes_processed;
    }
}

Sint64 efile_readv(efile_data_t *d, SysIOVec *iov, int iovlen) {
    efile_win_t *w = (efile_win_t*)d;

    return internal_sync_io(w, ReadFile, iov, iovlen, NULL);
}

Sint64 efile_writev(efile_data_t *d, SysIOVec *iov, int iovlen) {
    efile_win_t *w = (efile_win_t*)d;

    OVERLAPPED __overlapped, *overlapped;
    Uint64 bytes_written;

    if(w->common.modes & EFILE_MODE_APPEND) {
        overlapped = &__overlapped;

        sys_memset(overlapped, 0, sizeof(*overlapped));
        overlapped->OffsetHigh = 0xFFFFFFFF;
        overlapped->Offset = 0xFFFFFFFF;
    } else {
        overlapped = NULL;
    }

    return internal_sync_io(w, WriteFile, iov, iovlen, overlapped);
}

Sint64 efile_preadv(efile_data_t *d, Sint64 offset, SysIOVec *iov, int iovlen) {
    efile_win_t *w = (efile_win_t*)d;

    OVERLAPPED overlapped;

    sys_memset(&overlapped, 0, sizeof(overlapped));
    overlapped.OffsetHigh = (offset >> 32) & 0xFFFFFFFF;
    overlapped.Offset = offset & 0xFFFFFFFF;

    return internal_sync_io(w, ReadFile, iov, iovlen, &overlapped);
}

Sint64 efile_pwritev(efile_data_t *d, Sint64 offset, SysIOVec *iov, int iovlen) {
    efile_win_t *w = (efile_win_t*)d;

    OVERLAPPED overlapped;

    sys_memset(&overlapped, 0, sizeof(overlapped));
    overlapped.OffsetHigh = (offset >> 32) & 0xFFFFFFFF;
    overlapped.Offset = offset & 0xFFFFFFFF;

    return internal_sync_io(w, WriteFile, iov, iovlen, &overlapped);
}

int efile_seek(efile_data_t *d, enum efile_seek_t seek, Sint64 offset, Sint64 *new_position) {
    efile_win_t *w = (efile_win_t*)d;

    LARGE_INTEGER large_offset, large_new_position;
    DWORD whence;

    switch(seek) {
        case EFILE_SEEK_BOF: whence = FILE_BEGIN; break;
        case EFILE_SEEK_CUR: whence = FILE_CURRENT; break;
        case EFILE_SEEK_EOF: whence = FILE_END; break;
        default: ERTS_INTERNAL_ERROR("Invalid seek parameter");
    }

    large_offset.QuadPart = offset;

    if(!SetFilePointerEx(w->handle, large_offset, &large_new_position, whence)) {
        w->common.posix_errno = windows_to_posix_errno(GetLastError());
        return 0;
    }

    (*new_position) = large_new_position.QuadPart;

    return 1;
}

int efile_sync(efile_data_t *d, int data_only) {
    efile_win_t *w = (efile_win_t*)d;

    /* Windows doesn't support data-only syncing. */
    (void)data_only;

    if(!FlushFileBuffers(w->handle)) {
        w->common.posix_errno = windows_to_posix_errno(GetLastError());
        return 0;
    }

    return 1;
}

int efile_advise(efile_data_t *d, Sint64 offset, Sint64 length, enum efile_advise_t advise) {
    /* Windows doesn't support this, but we'll pretend it does since the call
     * is only a recommendation even on systems that do support it. */

    (void)d;
    (void)offset;
    (void)length;
    (void)advise;

    return 1;
}

int efile_allocate(efile_data_t *d, Sint64 offset, Sint64 length) {
    efile_win_t *w = (efile_win_t*)d;

    (void)d;
    (void)offset;
    (void)length;

    w->common.posix_errno = ENOTSUP;

    return 0;
}

int efile_truncate(efile_data_t *d) {
    efile_win_t *w = (efile_win_t*)d;

    if(!SetEndOfFile(w->handle)) {
        w->common.posix_errno = windows_to_posix_errno(GetLastError());
        return 0;
    }

    return 1;
}

static int is_executable_file(const efile_path_t *path) {
    /* We're using the file extension in order to be quirks-compliant with the
     * old driver, which never bothered to check the actual permissions. We
     * could easily do so now (cf. GetNamedSecurityInfo) but the execute
     * permission is only relevant for files that are started with the default
     * loader, and batch files run just fine with read permission alone. */

    int length = PATH_LENGTH(path);

    if(length >= 4) {
        const WCHAR *last_four = &((WCHAR*)path->data)[length - 4];

        if (!_wcsicmp(last_four, L".exe") ||
            !_wcsicmp(last_four, L".cmd") ||
            !_wcsicmp(last_four, L".bat") ||
            !_wcsicmp(last_four, L".com")) {
            return 1;
        }
    }

    return 0;
}

/* Returns whether the path refers to a link-like object, e.g. a junction
 * point, symbolic link, or mounted folder. */
static int is_name_surrogate(const efile_path_t *path) {
    HANDLE handle;
    int result;

    handle = CreateFileW((const WCHAR*)path->data, GENERIC_READ,
                         FILE_SHARE_FLAGS, NULL, OPEN_EXISTING,
                         FILE_FLAG_OPEN_REPARSE_POINT |
                         FILE_FLAG_BACKUP_SEMANTICS,
                         NULL);
    result = 0;

    if(handle != INVALID_HANDLE_VALUE) {
        REPARSE_GUID_DATA_BUFFER reparse_buffer;
        DWORD unused_length;
        BOOL success;

        success = DeviceIoControl(handle,
                                  FSCTL_GET_REPARSE_POINT, NULL, 0,
                                  &reparse_buffer, sizeof(reparse_buffer),
                                  &unused_length, NULL);

        /* ERROR_MORE_DATA is tolerated since we're guaranteed to have filled
         * the field we want. */
        if(success || GetLastError() == ERROR_MORE_DATA) {
            result = IsReparseTagNameSurrogate(reparse_buffer.ReparseTag);
        }

        CloseHandle(handle);
     }

     return result;
}

posix_errno_t efile_read_info(const efile_path_t *path, int follow_links, efile_fileinfo_t *result) {
    BY_HANDLE_FILE_INFORMATION native_file_info;
    DWORD attributes;
    int is_link;

    sys_memset(&native_file_info, 0, sizeof(native_file_info));
    is_link = 0;

    attributes = GetFileAttributesW((WCHAR*)path->data);

    if(attributes == INVALID_FILE_ATTRIBUTES) {
        DWORD last_error = GetLastError();

        /* Querying a network share root fails with ERROR_BAD_NETPATH, so we'll
         * fake it as a directory just like local roots. */
        if(!is_path_root(path) || last_error != ERROR_BAD_NETPATH) {
            return windows_to_posix_errno(last_error);
        }

        attributes = FILE_ATTRIBUTE_DIRECTORY;
    } else if(is_path_root(path)) {
        /* Local (or mounted) roots can be queried with GetFileAttributesW but
         * lack support for GetFileInformationByHandle, so we'll skip that
         * part. */
    } else {
        HANDLE handle;

        if(attributes & FILE_ATTRIBUTE_REPARSE_POINT) {
            is_link = is_name_surrogate(path);
        }

        if(follow_links && is_link) {
            posix_errno_t posix_errno;
            efile_path_t resolved_path;

            posix_errno = internal_read_link(path, &resolved_path);

            if(posix_errno != 0) {
                return posix_errno;
            }

            return efile_read_info(&resolved_path, 0, result);
        }

        handle = CreateFileW((const WCHAR*)path->data, GENERIC_READ,
            FILE_SHARE_FLAGS, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
            NULL);

        /* The old driver never cared whether this succeeded. */
        if(handle != INVALID_HANDLE_VALUE) {
            GetFileInformationByHandle(handle, &native_file_info);
            CloseHandle(handle);
        }

        FILETIME_TO_EPOCH(result->m_time, native_file_info.ftLastWriteTime);
        FILETIME_TO_EPOCH(result->a_time, native_file_info.ftLastAccessTime);
        FILETIME_TO_EPOCH(result->c_time, native_file_info.ftCreationTime);

        if(result->m_time == -EPOCH_DIFFERENCE) {
            /* Default to 1970 just like the old driver. */
            result->m_time = 0;
        }

        if(result->a_time == -EPOCH_DIFFERENCE) {
            result->a_time = result->m_time;
        }

        if(result->c_time == -EPOCH_DIFFERENCE) {
            result->c_time = result->m_time;
        }
    }

    if(is_link) {
        result->type = EFILE_FILETYPE_SYMLINK;
        /* This should be _S_IFLNK, but the old driver always set
         * non-directories to _S_IFREG. */
        result->mode |= _S_IFREG;
    } else if(attributes & FILE_ATTRIBUTE_DIRECTORY) {
        result->type = EFILE_FILETYPE_DIRECTORY;
        result->mode |= _S_IFDIR | _S_IEXEC;
    } else {
        if(is_executable_file(path)) {
            result->mode |= _S_IEXEC;
        }

        result->type = EFILE_FILETYPE_REGULAR;
        result->mode |= _S_IFREG;
    }

    if(!(attributes & FILE_ATTRIBUTE_READONLY)) {
        result->access = EFILE_ACCESS_READ | EFILE_ACCESS_WRITE;
        result->mode |= _S_IREAD | _S_IWRITE;
    } else {
        result->access = EFILE_ACCESS_READ;
        result->mode |= _S_IREAD;
    }

    /* Propagate user mode-bits to group/other fields */
    result->mode |= (result->mode & 0700) >> 3;
    result->mode |= (result->mode & 0700) >> 6;

    result->size =
        ((Uint64)native_file_info.nFileSizeHigh << 32ull) |
        (Uint64)native_file_info.nFileSizeLow;

    result->links = MAX(1, native_file_info.nNumberOfLinks);

    result->major_device = get_drive_number(path);
    result->minor_device = 0;
    result->inode = 0;
    result->uid = 0;
    result->gid = 0;

    return 0;
}

posix_errno_t efile_set_permissions(const efile_path_t *path, Uint32 permissions) {
    DWORD attributes = GetFileAttributesW((WCHAR*)path->data);

    if(attributes == INVALID_FILE_ATTRIBUTES) {
        return windows_to_posix_errno(GetLastError());
    }

    if(permissions & _S_IWRITE) {
        attributes &= ~FILE_ATTRIBUTE_READONLY;
    } else {
        attributes |= FILE_ATTRIBUTE_READONLY;
    }

    if(SetFileAttributesW((WCHAR*)path->data, attributes)) {
        return 0;
    }

    return windows_to_posix_errno(GetLastError());
}

posix_errno_t efile_set_owner(const efile_path_t *path, Sint32 owner, Sint32 group) {
    (void)path;
    (void)owner;
    (void)group;

    return 0;
}

posix_errno_t efile_set_time(const efile_path_t *path, Sint64 a_time, Sint64 m_time, Sint64 c_time) {
    FILETIME accessed, modified, created;
    DWORD last_error, attributes;
    HANDLE handle;

    attributes = GetFileAttributesW((WCHAR*)path->data);

    if(attributes == INVALID_FILE_ATTRIBUTES) {
        return windows_to_posix_errno(GetLastError());
    }

    /* If the file is read-only, we have to make it temporarily writable while
     * setting new metadata. */
    if(attributes & FILE_ATTRIBUTE_READONLY) {
        DWORD without_readonly = attributes & ~FILE_ATTRIBUTE_READONLY;

        if(!SetFileAttributesW((WCHAR*)path->data, without_readonly)) {
            return windows_to_posix_errno(GetLastError());
        }
    }

    EPOCH_TO_FILETIME(modified, m_time);
    EPOCH_TO_FILETIME(accessed, a_time);
    EPOCH_TO_FILETIME(created, c_time);

    handle = CreateFileW((WCHAR*)path->data, GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_FLAGS, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    last_error = GetLastError();

    if(handle != INVALID_HANDLE_VALUE) {
        if(SetFileTime(handle, &created, &accessed, &modified)) {
            last_error = ERROR_SUCCESS;
        } else {
            last_error = GetLastError();
        }

        CloseHandle(handle);
    }

    if(attributes & FILE_ATTRIBUTE_READONLY) {
        SetFileAttributesW((WCHAR*)path->data, attributes);
    }

    return windows_to_posix_errno(last_error);
}

static posix_errno_t internal_read_link(const efile_path_t *path, efile_path_t *result) {
    DWORD required_length, actual_length;
    HANDLE link_handle;
    DWORD last_error;

    link_handle = CreateFileW((WCHAR*)path->data, GENERIC_READ,
        FILE_SHARE_FLAGS, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    last_error = GetLastError();

    if(link_handle == INVALID_HANDLE_VALUE) {
        return windows_to_posix_errno(last_error);
    }

    required_length = GetFinalPathNameByHandleW(link_handle, NULL, 0, 0);
    last_error = GetLastError();

    if(required_length <= 0) {
        CloseHandle(link_handle);
        return windows_to_posix_errno(last_error);
    }

    /* Unlike many other path functions (eg. GetFullPathNameW), this one
     * includes the NUL terminator in its required length. */
    if(!enif_alloc_binary(required_length * sizeof(WCHAR), result)) {
        CloseHandle(link_handle);
        return ENOMEM;
    }

    actual_length = GetFinalPathNameByHandleW(link_handle,
        (WCHAR*)result->data, required_length, 0);
    last_error = GetLastError();

    CloseHandle(link_handle);

    if(actual_length == 0 || actual_length >= required_length) {
        enif_release_binary(result);
        return windows_to_posix_errno(last_error);
    }

    /* GetFinalPathNameByHandle always prepends with "\\?\" and NUL-terminates,
     * so we never have to touch-up the resulting path. */

    ASSERT_PATH_FORMAT(result);

    return 0;
}

posix_errno_t efile_read_link(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result) {
    posix_errno_t posix_errno;
    ErlNifBinary result_bin;
    DWORD attributes;

    ASSERT_PATH_FORMAT(path);

    attributes = GetFileAttributesW((WCHAR*)path->data);

    if(attributes == INVALID_FILE_ATTRIBUTES) {
        return windows_to_posix_errno(GetLastError());
    } else if(!(attributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
        return EINVAL;
    }

    if(!is_name_surrogate(path)) {
        return EINVAL;
    }

    posix_errno = internal_read_link(path, &result_bin);

    if(posix_errno == 0) {
        if(!normalize_path_result(&result_bin)) {
            enif_release_binary(&result_bin);
            return ENOMEM;
        }

        (*result) = enif_make_binary(env, &result_bin);
    }

    return posix_errno;
}

posix_errno_t efile_list_dir(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result) {
    ERL_NIF_TERM list_head;
    WIN32_FIND_DATAW data;
    HANDLE search_handle;
    WCHAR *search_path;
    DWORD last_error;

    ASSERT_PATH_FORMAT(path);

    search_path = enif_alloc(path->size + 2 * sizeof(WCHAR));

    if(search_path == NULL) {
        return ENOMEM;
    }

    sys_memcpy(search_path, path->data, path->size);
    search_path[PATH_LENGTH(path) + 0] = L'\\';
    search_path[PATH_LENGTH(path) + 1] = L'*';
    search_path[PATH_LENGTH(path) + 2] = L'\0';

    search_handle = FindFirstFileW(search_path, &data);
    last_error = GetLastError();

    enif_free(search_path);

    if(search_handle == INVALID_HANDLE_VALUE) {
        return windows_to_posix_errno(last_error);
    }

    list_head = enif_make_list(env, 0);

    do {
        int name_length = wcslen(data.cFileName);

        if(!is_ignored_name(name_length, data.cFileName)) {
            unsigned char *name_bytes;
            ERL_NIF_TERM name_term;
            size_t name_size;

            name_size = name_length * sizeof(WCHAR);

            name_bytes = enif_make_new_binary(env, name_size, &name_term);
            sys_memcpy(name_bytes, data.cFileName, name_size);

            list_head = enif_make_list_cell(env, name_term, list_head);
        }
    } while(FindNextFileW(search_handle, &data));

    FindClose(search_handle);
    (*result) = list_head;

    return 0;
}

posix_errno_t efile_rename(const efile_path_t *old_path, const efile_path_t *new_path) {
    BOOL old_is_directory, new_is_directory;
    DWORD move_flags, last_error;

    ASSERT_PATH_FORMAT(old_path);
    ASSERT_PATH_FORMAT(new_path);

    move_flags = MOVEFILE_COPY_ALLOWED | MOVEFILE_WRITE_THROUGH;

    if(MoveFileExW((WCHAR*)old_path->data, (WCHAR*)new_path->data, move_flags)) {
        return 0;
    }

    last_error = GetLastError();

    old_is_directory = has_file_attributes(old_path, FILE_ATTRIBUTE_DIRECTORY);
    new_is_directory = has_file_attributes(new_path, FILE_ATTRIBUTE_DIRECTORY);

    switch(last_error) {
    case ERROR_SHARING_VIOLATION:
    case ERROR_ACCESS_DENIED:
        if(old_is_directory) {
            BOOL moved_into_itself;

            moved_into_itself = (old_path->size <= new_path->size) &&
                !_wcsnicmp((WCHAR*)old_path->data, (WCHAR*)new_path->data,
                    PATH_LENGTH(old_path));

            if(moved_into_itself) {
                return EINVAL;
            } else if(is_path_root(old_path)) {
                return EINVAL;
            }

            /* Renaming a directory across volumes needs to be rewritten as
             * EXDEV so that the caller can respond by simulating it with
             * copy/delete operations.
             *
             * Files are handled through MOVEFILE_COPY_ALLOWED. */
            if(!has_same_mount_point(old_path, new_path)) {
                return EXDEV;
            }
        }
        break;
    case ERROR_PATH_NOT_FOUND:
    case ERROR_FILE_NOT_FOUND:
        return ENOENT;
    case ERROR_ALREADY_EXISTS:
    case ERROR_FILE_EXISTS:
        if(old_is_directory && !new_is_directory) {
            return ENOTDIR;
        } else if(!old_is_directory && new_is_directory) {
            return EISDIR;
        } else if(old_is_directory && new_is_directory) {
            /* This will fail if the destination isn't empty. */
            if(RemoveDirectoryW((WCHAR*)new_path->data)) {
                return efile_rename(old_path, new_path);
            }

            return EEXIST;
        } else if(!old_is_directory && !new_is_directory) {
            /* This is pretty iffy; the public documentation says that the
             * operation may EACCES on some systems when either file is open,
             * which gives us room to use MOVEFILE_REPLACE_EXISTING and be done
             * with it, but the old implementation simulated Unix semantics and
             * there's a lot of code that relies on that.
             *
             * The simulation renames the destination to a scratch name to get
             * around the fact that it's impossible to open (and by extension
             * rename) a file that's been deleted while open. It has a few
             * drawbacks though;
             *
             * 1) It's not atomic as there's a small window where there's no
             *    file at all on the destination path.
             * 2) It will confuse applications that subscribe to folder
             *    changes.
             * 3) It will fail if we lack general permission to write in the
             *    same folder. */

            WCHAR *swap_path = enif_alloc(new_path->size + sizeof(WCHAR) * 64);

            if(swap_path == NULL) {
                return ENOMEM;
            } else {
                static LONGLONG unique_counter = 0;
                WCHAR *swap_path_end;

                /* We swap in the same folder as the destination to be
                 * reasonably sure that it's on the same volume. Note that
                 * we're avoiding GetTempFileNameW as it will fail on long
                 * paths. */

                sys_memcpy(swap_path, (WCHAR*)new_path->data, new_path->size);
                swap_path_end = swap_path + PATH_LENGTH(new_path);

                while(!IS_SLASH(*swap_path_end)) {
                    ASSERT(swap_path_end > swap_path);
                    swap_path_end--;
                }

                StringCchPrintfW(&swap_path_end[1], 64, L"erl-%lx-%llx.tmp",
                    GetCurrentProcessId(), unique_counter);
                InterlockedIncrement64(&unique_counter);
            }

            if(MoveFileExW((WCHAR*)new_path->data, swap_path, MOVEFILE_REPLACE_EXISTING)) {
                if(MoveFileExW((WCHAR*)old_path->data, (WCHAR*)new_path->data, move_flags)) {
                    last_error = ERROR_SUCCESS;
                    DeleteFileW(swap_path);
                } else {
                    last_error = GetLastError();
                    MoveFileW(swap_path, (WCHAR*)new_path->data);
                }
            } else {
                last_error = GetLastError();
                DeleteFileW(swap_path);
            }

            enif_free(swap_path);

            return windows_to_posix_errno(last_error);
        }

        return EEXIST;
    }

    return windows_to_posix_errno(last_error);
}

posix_errno_t efile_make_hard_link(const efile_path_t *existing_path, const efile_path_t *new_path) {
    ASSERT_PATH_FORMAT(existing_path);
    ASSERT_PATH_FORMAT(new_path);

    if(!CreateHardLinkW((WCHAR*)new_path->data, (WCHAR*)existing_path->data, NULL)) {
        return windows_to_posix_errno(GetLastError());
    }

    return 0;
}

posix_errno_t efile_make_soft_link(const efile_path_t *existing_path, const efile_path_t *new_path) {
    DWORD link_flags;

    ASSERT_PATH_FORMAT(existing_path);
    ASSERT_PATH_FORMAT(new_path);

    if(has_file_attributes(existing_path, FILE_ATTRIBUTE_DIRECTORY)) {
        link_flags = SYMBOLIC_LINK_FLAG_DIRECTORY;
    } else {
        link_flags = 0;
    }

    if(!CreateSymbolicLinkW((WCHAR*)new_path->data, (WCHAR*)existing_path->data, link_flags)) {
        return windows_to_posix_errno(GetLastError());
    }

    return 0;
}

posix_errno_t efile_make_dir(const efile_path_t *path) {
    ASSERT_PATH_FORMAT(path);

    if(!CreateDirectoryW((WCHAR*)path->data, NULL)) {
        return windows_to_posix_errno(GetLastError());
    }

    return 0;
}

posix_errno_t efile_del_file(const efile_path_t *path) {
    ASSERT_PATH_FORMAT(path);

    if(!DeleteFileW((WCHAR*)path->data)) {
        DWORD last_error = GetLastError();

        switch(last_error) {
        case ERROR_INVALID_NAME:
            /* Attempted to delete a device or similar. */
            return EACCES;
        case ERROR_ACCESS_DENIED:
            /* Windows NT reports removing a directory as EACCES instead of
             * EPERM. */
            if(has_file_attributes(path, FILE_ATTRIBUTE_DIRECTORY)) {
                return EPERM;
            }
            break;
        }

        return windows_to_posix_errno(last_error);
    }

    return 0;
}

posix_errno_t efile_del_dir(const efile_path_t *path) {
    ASSERT_PATH_FORMAT(path);

    if(!RemoveDirectoryW((WCHAR*)path->data)) {
        DWORD last_error = GetLastError();

        if(last_error == ERROR_DIRECTORY) {
            return ENOTDIR;
        }

        return windows_to_posix_errno(last_error);
    }

    return 0;
}

posix_errno_t efile_set_cwd(const efile_path_t *path) {
    const WCHAR *path_start;

    ASSERT_PATH_FORMAT(path);

    /* We have to use _wchdir since that's the only function that updates the
     * per-drive working directory, but it naively assumes that all paths
     * starting with \\ are UNC paths, so we have to skip the long-path prefix.
     *
     * _wchdir doesn't handle long-prefixed UNC paths either so we hand those
     * to SetCurrentDirectoryW instead. The per-drive working directory is
     * irrelevant for such paths anyway. */

    if(!IS_LONG_UNC_PATH(PATH_LENGTH(path), path->data)) {
        path_start = (WCHAR*)path->data + LP_PREFIX_LENGTH;

        if(_wchdir(path_start)) {
            return windows_to_posix_errno(GetLastError());
        }
    } else {
        if(!SetCurrentDirectoryW((WCHAR*)path->data)) {
            return windows_to_posix_errno(GetLastError());
        }
    }

    return 0;
}

static int is_valid_drive(int device_index) {
    WCHAR drive_path[4] = {L'?', L':', L'\\', L'\0'};

    if(device_index == 0) {
        /* Default drive; always valid. */
        return 1;
    } else if(device_index > (L'Z' - L'A' + 1)) {
        return 0;
    }

    drive_path[0] = device_index + L'A' - 1;

    switch(GetDriveTypeW(drive_path)) {
    case DRIVE_NO_ROOT_DIR:
    case DRIVE_UNKNOWN:
        return 0;
    }

    return 1;
}

posix_errno_t efile_get_device_cwd(ErlNifEnv *env, int device_index, ERL_NIF_TERM *result) {
    ErlNifBinary result_bin;

    /* _wgetdcwd might crash the entire emulator on debug builds since the CRT
     * invalid parameter handler asserts if passed a non-existent drive (Or
     * simply one that has been unmounted), so we check it ourselves to avoid
     * that. */
    if(!is_valid_drive(device_index)) {
        return EACCES;
    }

    if(!enif_alloc_binary(MAX_PATH * sizeof(WCHAR), &result_bin)) {
        return ENOMEM;
    }

    if(_wgetdcwd(device_index, (WCHAR*)result_bin.data, MAX_PATH) == NULL) {
        enif_release_binary(&result_bin);
        return EACCES;
    }

    if(!normalize_path_result(&result_bin)) {
        enif_release_binary(&result_bin);
        return ENOMEM;
    }

    (*result) = enif_make_binary(env, &result_bin);

    return 0;
}

posix_errno_t efile_get_cwd(ErlNifEnv *env, ERL_NIF_TERM *result) {
    return efile_get_device_cwd(env, 0, result);
}

posix_errno_t efile_altname(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result) {
    ErlNifBinary result_bin;

    ASSERT_PATH_FORMAT(path);

    if(is_path_root(path)) {
        /* Root paths can't be queried so we'll just return them as they are. */
        if(!enif_alloc_binary(path->size, &result_bin)) {
            return ENOMEM;
        }

        sys_memcpy(result_bin.data, path->data, path->size);
    } else {
        WIN32_FIND_DATAW data;
        HANDLE handle;

        WCHAR *name_buffer;
        int name_length;

        /* Reject path wildcards. */
        if(wcspbrk(&((const WCHAR*)path->data)[LP_PREFIX_LENGTH], L"?*")) {
            return ENOENT;
        }

        handle = FindFirstFileW((const WCHAR*)path->data, &data);

        if(handle == INVALID_HANDLE_VALUE) {
            return windows_to_posix_errno(GetLastError());
        }

        FindClose(handle);

        name_length = wcslen(data.cAlternateFileName);

        if(name_length > 0) {
            name_buffer = data.cAlternateFileName;
        } else {
            name_length = wcslen(data.cFileName);
            name_buffer = data.cFileName;
        }

        /* Include NUL-terminator; it will be removed after normalization. */
        name_length += 1;

        if(!enif_alloc_binary(name_length * sizeof(WCHAR), &result_bin)) {
            return ENOMEM;
        }

        sys_memcpy(result_bin.data, name_buffer, name_length * sizeof(WCHAR));
    }

    if(!normalize_path_result(&result_bin)) {
        enif_release_binary(&result_bin);
        return ENOMEM;
    }

    (*result) = enif_make_binary(env, &result_bin);

    return 0;
}

static int windows_to_posix_errno(DWORD last_error) {
    switch(last_error) {
    case ERROR_SUCCESS:
        return 0;
    case ERROR_INVALID_FUNCTION:
    case ERROR_INVALID_DATA:
    case ERROR_INVALID_PARAMETER:
    case ERROR_INVALID_TARGET_HANDLE:
    case ERROR_INVALID_CATEGORY:
    case ERROR_NEGATIVE_SEEK:
        return EINVAL;
    case ERROR_DIR_NOT_EMPTY:
        return EEXIST;
    case ERROR_BAD_FORMAT:
        return ENOEXEC;
    case ERROR_PATH_NOT_FOUND:
    case ERROR_FILE_NOT_FOUND:
    case ERROR_NO_MORE_FILES:
    case ERROR_INVALID_NAME:
        return ENOENT;
    case ERROR_TOO_MANY_OPEN_FILES:
        return EMFILE;
    case ERROR_ACCESS_DENIED:
    case ERROR_INVALID_ACCESS:
    case ERROR_CURRENT_DIRECTORY:
    case ERROR_SHARING_VIOLATION:
    case ERROR_LOCK_VIOLATION:
    case ERROR_INVALID_PASSWORD:
    case ERROR_DRIVE_LOCKED:
        return EACCES;
    case ERROR_INVALID_HANDLE:
        return EBADF;
    case ERROR_NOT_ENOUGH_MEMORY:
    case ERROR_OUTOFMEMORY:
    case ERROR_OUT_OF_STRUCTURES:
        return ENOMEM;
    case ERROR_INVALID_DRIVE:
    case ERROR_BAD_UNIT:
    case ERROR_NOT_READY:
    case ERROR_REM_NOT_LIST:
    case ERROR_DUP_NAME:
    case ERROR_BAD_NETPATH:
    case ERROR_NETWORK_BUSY:
    case ERROR_DEV_NOT_EXIST:
    case ERROR_BAD_NET_NAME:
        return ENXIO;
    case ERROR_NOT_SAME_DEVICE:
        return EXDEV;
    case ERROR_WRITE_PROTECT:
        return EROFS;
    case ERROR_BAD_LENGTH:
    case ERROR_BUFFER_OVERFLOW:
        return E2BIG;
    case ERROR_SEEK:
    case ERROR_SECTOR_NOT_FOUND:
        return ESPIPE;
    case ERROR_NOT_DOS_DISK:
        return ENODEV;
    case ERROR_GEN_FAILURE:
        return ENODEV;
    case ERROR_SHARING_BUFFER_EXCEEDED:
    case ERROR_NO_MORE_SEARCH_HANDLES:
        return EMFILE;
    case ERROR_HANDLE_EOF:
    case ERROR_BROKEN_PIPE:
        return EPIPE;
    case ERROR_HANDLE_DISK_FULL:
    case ERROR_DISK_FULL:
        return ENOSPC;
    case ERROR_NOT_SUPPORTED:
        return ENOTSUP;
    case ERROR_FILE_EXISTS:
    case ERROR_ALREADY_EXISTS:
    case ERROR_CANNOT_MAKE:
        return EEXIST;
    case ERROR_ALREADY_ASSIGNED:
        return EBUSY;
    case ERROR_NO_PROC_SLOTS:
        return EAGAIN;
    case ERROR_CANT_RESOLVE_FILENAME:
        return EMLINK;
    case ERROR_PRIVILEGE_NOT_HELD:
        return EPERM;
    case ERROR_ARENA_TRASHED:
    case ERROR_INVALID_BLOCK:
    case ERROR_BAD_ENVIRONMENT:
    case ERROR_BAD_COMMAND:
    case ERROR_CRC:
    case ERROR_OUT_OF_PAPER:
    case ERROR_READ_FAULT:
    case ERROR_WRITE_FAULT:
    case ERROR_WRONG_DISK:
    case ERROR_NET_WRITE_FAULT:
        return EIO;
    default: /* not to do with files I expect. */
        return EIO;
    }
}
