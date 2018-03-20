/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2017. All Rights Reserved.
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

typedef int posix_errno_t;

enum efile_modes_t {
    EFILE_MODE_READ = (1 << 0),
    EFILE_MODE_WRITE = (1 << 1), /* Implies truncating file when used alone. */
    EFILE_MODE_APPEND = (1 << 2),
    EFILE_MODE_EXCLUSIVE = (1 << 3),
    EFILE_MODE_SYNC = (1 << 4),

    EFILE_MODE_SKIP_TYPE_CHECK = (1 << 5), /* Special for device files on Unix. */
    EFILE_MODE_NO_TRUNCATE = (1 << 6), /* Special for reopening on VxWorks. */

    EFILE_MODE_READ_WRITE = EFILE_MODE_READ | EFILE_MODE_WRITE
};

enum efile_access_t {
    EFILE_ACCESS_NONE = 0,
    EFILE_ACCESS_READ = 1,
    EFILE_ACCESS_WRITE = 2,
    EFILE_ACCESS_READ_WRITE = EFILE_ACCESS_READ | EFILE_ACCESS_WRITE
};

enum efile_seek_t {
    EFILE_SEEK_BOF,
    EFILE_SEEK_CUR,
    EFILE_SEEK_EOF
};

enum efile_filetype_t {
    EFILE_FILETYPE_DEVICE,
    EFILE_FILETYPE_DIRECTORY,
    EFILE_FILETYPE_REGULAR,
    EFILE_FILETYPE_SYMLINK,
    EFILE_FILETYPE_OTHER
};

enum efile_advise_t {
    EFILE_ADVISE_NORMAL,
    EFILE_ADVISE_RANDOM,
    EFILE_ADVISE_SEQUENTIAL,
    EFILE_ADVISE_WILL_NEED,
    EFILE_ADVISE_DONT_NEED,
    EFILE_ADVISE_NO_REUSE
};

enum efile_state_t {
    EFILE_STATE_IDLE = 0,
    EFILE_STATE_BUSY = 1,
    EFILE_STATE_CLOSE_PENDING = 2,
    EFILE_STATE_CLOSED = 3
};

typedef struct {
    Sint64 size;            /* Size of file */
    Uint32 type;            /* Type of file -- one of EFILE_FILETYPE_*. */
    Uint32 access;          /* Access to file -- one of EFILE_ACCESS_*. */
    Uint32 mode;            /* Access permissions -- bit field. */
    Uint32 links;           /* Number of links to file. */
    Uint32 major_device;    /* Major device or file system. */
    Uint32 minor_device;    /* Minor device (for devices). */
    Uint32 inode;           /* Inode number. */
    Uint32 uid;             /* User id of owner. */
    Uint32 gid;             /* Group id of owner. */
    Sint64 a_time;          /* Last time the file was accessed. */
    Sint64 m_time;          /* Last time the file was modified. */
    Sint64 c_time;          /* Windows: creation time, Unix: last inode
                             * change. */
} efile_fileinfo_t;

/* The smallest value that can be converted freely between universal, local,
 * and POSIX time, as required by read_file_info/2. Corresponds to
 * {{1902,1,1},{0,0,0}} */
#define EFILE_MIN_FILETIME -2145916800

/* Initializes an efile_data_t; must be used in efile_open on success. */
#define EFILE_INIT_RESOURCE(__d, __modes) do { \
        erts_atomic32_init_acqb(&(__d)->state, EFILE_STATE_IDLE); \
        (__d)->posix_errno = 0; \
        (__d)->modes = __modes; \
    } while(0)

typedef struct {
    erts_atomic32_t state;

    posix_errno_t posix_errno;
    enum efile_modes_t modes;

    ErlNifMonitor monitor;
} efile_data_t;

typedef ErlNifBinary efile_path_t;

/* @brief Translates the given "raw name" into the format expected by the APIs
 * used by the underlying implementation. The result is transient and does not
 * need to be released.
 *
 * This may change the structure of the path and its results should never be
 * passed on to the user. Refer to the OS-specific implementation for details.
 *
 * @param path The term to translate; it must have been encoded with
 * prim_file:internal_native2name for compatibility reasons. */
posix_errno_t efile_marshal_path(ErlNifEnv *env, ERL_NIF_TERM path, efile_path_t *result);

/* @brief Returns the underlying handle as an implementation-defined term.
 *
 * This is an internal function intended to support tests and tricky
 * operations like sendfile(2). */
ERL_NIF_TERM efile_get_handle(ErlNifEnv *env, efile_data_t *d);

/* @brief Read until EOF or the given iovec has been filled.
 *
 * @return -1 on failure, or the number of bytes read on success. The return
 * value will be 0 if no bytes could be read before EOF or the end of the
 * iovec. */
Sint64 efile_readv(efile_data_t *d, SysIOVec *iov, int iovlen);

/* @brief Write the entirety of the given iovec.
 *
 * @return -1 on failure, or the number of bytes written on success. "Partial"
 * failures will be reported with -1 and not the number of bytes we managed to
 * write to disk before the failure. */
Sint64 efile_writev(efile_data_t *d, SysIOVec *iov, int iovlen);

/* @brief As \c efile_readv, but starting from a file offset. */
Sint64 efile_preadv(efile_data_t *d, Sint64 offset, SysIOVec *iov, int iovlen);

/* @brief As \c efile_writev, but starting from a file offset. */
Sint64 efile_pwritev(efile_data_t *d, Sint64 offset, SysIOVec *iov, int iovlen);

int efile_seek(efile_data_t *d, enum efile_seek_t seek, Sint64 offset, Sint64 *new_position);

int efile_sync(efile_data_t *d, int data_only);

int efile_advise(efile_data_t *d, Sint64 offset, Sint64 length, enum efile_advise_t advise);
int efile_allocate(efile_data_t *d, Sint64 offset, Sint64 length);
int efile_truncate(efile_data_t *d);

posix_errno_t efile_open(const efile_path_t *path, enum efile_modes_t modes,
        ErlNifResourceType *nif_type, efile_data_t **d);

/** @brief Closes a file. The file must have entered the CLOSED state prior to
 * calling this to prevent double close. */
int efile_close(efile_data_t *d);

/* **** **** **** **** **** **** **** **** **** **** **** **** **** **** **** */

posix_errno_t efile_read_info(const efile_path_t *path, int follow_link, efile_fileinfo_t *result);

/** @brief Sets the file times to the given values. Refer to efile_fileinfo_t
 * for a description of each. */
posix_errno_t efile_set_time(const efile_path_t *path, Sint64 a_time, Sint64 m_time, Sint64 c_time);

/** @brief On Unix, this sets the file permissions according to the docs for
 * file:write_file_info/2. On Windows it uses the "owner write permission" flag
 * to toggle whether the file is read-only or not. */
posix_errno_t efile_set_permissions(const efile_path_t *path, Uint32 permissions);

/** @brief On Unix, this will set the owner/group to the given values. It will
 * do nothing on other platforms. */
posix_errno_t efile_set_owner(const efile_path_t *path, Sint32 owner, Sint32 group);

/** @brief Resolves the final path of the given link. */
posix_errno_t efile_read_link(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result);

/** @brief Lists the contents of the given directory.
 * @param result [out] A list of all the directory/file names contained in the
 * given directory. */
posix_errno_t efile_list_dir(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result);

/** @brief Changes the name of an existing file or directory, from old_path
 * to new_path.
 *
 * If old_path and new_path refer to the same file or directory, it does
 * nothing and returns success. Otherwise if new_path already exists, it will
 * be deleted and replaced by src subject to the following conditions:
 *
 *     If old_path is a directory, new_path may be an empty directory.
 *     If old_path is a file, new_path may be a file.
 *
 * Neither of these are guaranteed to be atomic. In any other situation where
 * new_path already exists, the rename will fail.
 *
 * Some possible error codes:
 *
 * - EACCES:     Either paths or one of their parent directories can't be read
 *               and/or written.
 * - EEXIST:     new_path is a non-empty directory.
 * - EINVAL:     old_path is a root directory or new_path is a subdirectory
 *               of new_path.
 * - EISDIR:     new_path is a directory, but old_path is not.
 * - ENOTDIR:    old_path is a directory, but new_path is not.
 * - ENOENT:     old_path doesn't exist, or either path is "".
 * - EXDEV:      The paths are on different filesystems.
 *
 * The implementation of rename may allow cross-filesystem renames,
 * but the caller should be prepared to emulate it with copy and
 * delete if errno is EXDEV. */
posix_errno_t efile_rename(const efile_path_t *old_path, const efile_path_t *new_path);

posix_errno_t efile_make_hard_link(const efile_path_t *existing_path, const efile_path_t *new_path);
posix_errno_t efile_make_soft_link(const efile_path_t *existing_path, const efile_path_t *new_path);
posix_errno_t efile_make_dir(const efile_path_t *path);

posix_errno_t efile_del_file(const efile_path_t *path);
posix_errno_t efile_del_dir(const efile_path_t *path);

posix_errno_t efile_get_cwd(ErlNifEnv *env, ERL_NIF_TERM *result);
posix_errno_t efile_set_cwd(const efile_path_t *path);

/** @brief A Windows-specific function for returning the working directory of a
 * given device.
 *
 * @param device_index The drive index; 1 for A, 2 for B, etc.
 * @param result [out] The working directory of the given device
 */
posix_errno_t efile_get_device_cwd(ErlNifEnv *env, int device_index, ERL_NIF_TERM *result);

/** @brief A Windows-specific function for returning the 8.3-name of a given
 * file or directory. */
posix_errno_t efile_altname(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result);
