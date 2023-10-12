/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2017-2022. All Rights Reserved.
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

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#include "prim_file_nif.h"

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#if defined(__DARWIN__) || defined(HAVE_LINUX_FALLOC_H) || defined(HAVE_POSIX_FALLOCATE)
#include <fcntl.h>
#endif

#ifdef HAVE_LINUX_FALLOC_H
#include <linux/falloc.h>
#endif

#include <utime.h>

#define FALLBACK_RW_LENGTH ((1ull << 31) - 1)

/* Macros for testing file types. */
#ifdef NO_UMASK
#define FILE_MODE 0644
#define DIR_MODE  0755
#else
#define FILE_MODE 0666
#define DIR_MODE  0777
#endif

/* Old platforms might not have IOV_MAX defined. */
#if !defined(IOV_MAX) && defined(UIO_MAXIOV)
#define IOV_MAX UIO_MAXIOV
#elif !defined(IOV_MAX)
#define IOV_MAX 16
#endif

typedef struct {
    efile_data_t common;
    int fd;
} efile_unix_t;

static int has_invalid_null_termination(const ErlNifBinary *path) {
    const char *null_pos, *end_pos;

    null_pos = memchr(path->data, '\0', path->size);
    end_pos = (const char*)&path->data[path->size] - 1;

    if(null_pos == NULL) {
        return 1;
    }

    /* prim_file:internal_name2native sometimes feeds us data that is "doubly"
     * NUL-terminated, so we'll accept any number of trailing NULs so long as
     * they aren't interrupted by anything else. */
    while(null_pos < end_pos && (*null_pos) == '\0') {
        null_pos++;
    }

    return null_pos != end_pos;
}

posix_errno_t efile_marshal_path(ErlNifEnv *env, ERL_NIF_TERM path, efile_path_t *result) {
    if(!enif_inspect_binary(env, path, result)) {
        return EINVAL;
    }

    if(has_invalid_null_termination(result)) {
        return EINVAL;
    }

    return 0;
}

ERL_NIF_TERM efile_get_handle(ErlNifEnv *env, efile_data_t *d) {
    efile_unix_t *u = (efile_unix_t*)d;
    int fd = u->fd;
    ERL_NIF_TERM handle;
    unsigned char *bits;

    bits = enif_make_new_binary(env, sizeof(fd), &handle);
    memcpy(bits, &fd, sizeof(fd));

    return handle;
}

posix_errno_t efile_dup_handle(ErlNifEnv *env, efile_data_t *d, ErlNifEvent *handle) {
    efile_unix_t *u = (efile_unix_t*)d;
    int fd;

    if ((fd = dup(u->fd)) < 0)
        return errno;

    *handle = fd;
    return 0;
}

static int open_file_is_dir(const efile_path_t *path, int fd) {
    struct stat file_info;
    int error;

#ifndef HAVE_FSTAT
    error = stat((const char*)path->data, &file_info);
    (void)fd;
#else
    error = fstat(fd, &file_info);
    (void)path;
#endif

    /* Assume not a directory on error. */
    return error == 0 && S_ISDIR(file_info.st_mode);
}

static int get_flags(enum efile_modes_t modes) {
    int flags = 0;
    if(modes & EFILE_MODE_READ && !(modes & EFILE_MODE_WRITE)) {
        flags |= O_RDONLY;
    } else if(modes & EFILE_MODE_WRITE && !(modes & EFILE_MODE_READ)) {
        flags |= O_TRUNC | O_WRONLY | O_CREAT;
    } else if(modes & EFILE_MODE_READ_WRITE) {
        flags |= O_RDWR | O_CREAT;
    } else {
        return EINVAL;
    }

    if(modes & EFILE_MODE_APPEND) {
        flags &= ~O_TRUNC;
        flags |= O_APPEND;
    }

    if(modes & EFILE_MODE_EXCLUSIVE) {
        flags |= O_EXCL;
    }

    if(modes & EFILE_MODE_SYNC) {
#ifndef O_SYNC
        return ENOTSUP;
#else
        flags |= O_SYNC;
#endif
    }
    return flags;
}

posix_errno_t efile_open(const efile_path_t *path, enum efile_modes_t modes,
        ErlNifResourceType *nif_type, efile_data_t **d) {

    int mode, flags, fd;

    flags = get_flags(modes);

    if(modes & EFILE_MODE_DIRECTORY) {
        mode = DIR_MODE;
#ifdef O_DIRECTORY
        flags |= O_DIRECTORY;
#endif
    } else {
        mode = FILE_MODE;
    }

    do {
        fd = open((const char*)path->data, flags, mode);
    } while(fd == -1 && errno == EINTR);

    if(fd != -1) {
        efile_unix_t *u;

#ifndef O_DIRECTORY
        /* On platforms without O_DIRECTORY support, ensure that using the
         * directory flag to open a file fails. */
        if(!(modes & EFILE_MODE_SKIP_TYPE_CHECK) &&
           (modes & EFILE_MODE_DIRECTORY) && !open_file_is_dir(path, fd)) {
            close(fd);
            return ENOTDIR;
        }
#endif

        /* open() works on directories without the O_DIRECTORY flag but for
         * consistency across platforms we require that the user has requested
         * directory mode. */
        if(!(modes & EFILE_MODE_SKIP_TYPE_CHECK) &&
           !(modes & EFILE_MODE_DIRECTORY) && open_file_is_dir(path, fd)) {
            close(fd);
            return EISDIR;
        }

        u = (efile_unix_t*)enif_alloc_resource(nif_type, sizeof(efile_unix_t));
        u->fd = fd;

        EFILE_INIT_RESOURCE(&u->common, modes);
        (*d) = &u->common;

        return 0;
    }

    (*d) = NULL;
    return errno;
}

posix_errno_t efile_from_fd(int fd,
                            ErlNifResourceType *nif_type,
                            efile_data_t **d) {
    if (fcntl(fd, F_GETFL) != -1 || errno != EBADF) {
        efile_unix_t *u;

        u = (efile_unix_t*)enif_alloc_resource(nif_type, sizeof(efile_unix_t));
        u->fd = fd;

        EFILE_INIT_RESOURCE(&u->common, EFILE_MODE_FROM_ALREADY_OPEN_FD);
        (*d) = &u->common;

        return 0;
    }
    (*d) = NULL;
    return errno;
}

int efile_close(efile_data_t *d, posix_errno_t *error) {
    efile_unix_t *u = (efile_unix_t*)d;
    int fd;

    ASSERT(enif_thread_type() == ERL_NIF_THR_DIRTY_IO_SCHEDULER);
    ASSERT(erts_atomic32_read_nob(&d->state) == EFILE_STATE_CLOSED);
    ASSERT(u->fd != -1);

    fd = u->fd;
    u->fd = -1;

    enif_release_resource(d);

    /* close(2) either always closes (*BSD, Linux) or leaves the fd in an
     * undefined state (POSIX 2008, Solaris), so we must not retry on EINTR. */

    if(close(fd) < 0) {
        *error = errno;
        return 0;
    }

    return 1;
}

static void shift_iov(SysIOVec **iov, int *iovlen, ssize_t shift) {
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

Sint64 efile_readv(efile_data_t *d, SysIOVec *iov, int iovlen) {
    efile_unix_t *u = (efile_unix_t*)d;

    Sint64 bytes_read;
    ssize_t result;

    bytes_read = 0;

    do {
        int use_fallback = 0;

        if(iovlen < 1) {
            result = 0;
            break;
        }

        /* writev(2) implies readv(2) */
#ifdef HAVE_WRITEV
        result = readv(u->fd, iov, MIN(IOV_MAX, iovlen));

        /* Fall back to using read(2) if readv(2) reports that the combined
         * size of iov is greater than SSIZE_T_MAX. */
        use_fallback = (result < 0 && errno == EINVAL);
#else
        use_fallback = 1;
#endif

        if(use_fallback) {
            result = read(u->fd, iov->iov_base, iov->iov_len);

            /* Some OSs (e.g. macOS) does not allow reads greater than 2 GB,
               so if we get EINVAL in the fallback, we try with a smaller length */
            if (result < 0 && errno == EINVAL && iov->iov_len > FALLBACK_RW_LENGTH)
                result = read(u->fd, iov->iov_base, FALLBACK_RW_LENGTH);
        }

        if(result > 0) {
            shift_iov(&iov, &iovlen, result);
            bytes_read += result;
        }
    } while(result > 0 || (result < 0 && errno == EINTR));

    u->common.posix_errno = errno;

    if(result == 0 && bytes_read > 0) {
        return bytes_read;
    }

    return result;
}

Sint64 efile_writev(efile_data_t *d, SysIOVec *iov, int iovlen) {
    efile_unix_t *u = (efile_unix_t*)d;

    Sint64 bytes_written;
    ssize_t result;

    bytes_written = 0;

    do {
        int use_fallback = 0;

        if(iovlen < 1) {
            result = 0;
            break;
        }

#ifdef HAVE_WRITEV
        result = writev(u->fd, iov, MIN(IOV_MAX, iovlen));

        /* Fall back to using write(2) if writev(2) reports that the combined
         * size of iov is greater than SSIZE_T_MAX. */
        use_fallback = (result < 0 && errno == EINVAL);
#else
        use_fallback = 1;
#endif

        if(use_fallback) {
            result = write(u->fd, iov->iov_base, iov->iov_len);

            /* Some OSs (e.g. macOS) does not allow writes greater than 2 GB,
               so if we get EINVAL in the fallback, we try with a smaller length */
            if (result < 0 && errno == EINVAL && iov->iov_len > FALLBACK_RW_LENGTH)
                result = write(u->fd, iov->iov_base, FALLBACK_RW_LENGTH);
        }

        if(result > 0) {
            shift_iov(&iov, &iovlen, result);
            bytes_written += result;
        }
    } while(result > 0 || (result < 0 && errno == EINTR));

    u->common.posix_errno = errno;

    if(result == 0 && bytes_written > 0) {
        return bytes_written;
    }

    return result;
}

Sint64 efile_preadv(efile_data_t *d, Sint64 offset, SysIOVec *iov, int iovlen) {
    efile_unix_t *u = (efile_unix_t*)d;

    Uint64 bytes_read;
    Sint64 result;

#if !defined(HAVE_PREADV) && !defined(HAVE_PREAD)
    /* This function is documented as leaving the file position undefined, but
     * the old driver always reset it so there's probably code in the wild that
     * relies on this behavior. */
    off_t original_position = lseek(u->fd, 0, SEEK_CUR);

    if(original_position < 0 || lseek(u->fd, offset, SEEK_SET) < 0) {
        u->common.posix_errno = errno;
        return -1;
    }
#endif

    bytes_read = 0;

    do {
        if(iovlen < 1) {
            result = 0;
            break;
        }

#if defined(HAVE_PREADV)
        result = preadv(u->fd, iov, MIN(IOV_MAX, iovlen), offset);
#elif defined(HAVE_PREAD)
        result = pread(u->fd, iov->iov_base, iov->iov_len, offset);
#else
        result = read(u->fd, iov->iov_base, iov->iov_len);
#endif

        if(result > 0) {
            shift_iov(&iov, &iovlen, result);
            bytes_read += result;
            offset += result;
        }
    } while(result > 0 || (result < 0 && errno == EINTR));

    u->common.posix_errno = errno;

#if !defined(HAVE_PREADV) && !defined(HAVE_PREAD)
    if(result >= 0) {
        if(lseek(u->fd, original_position, SEEK_SET) < 0) {
            u->common.posix_errno = errno;
            return -1;
        }
    }
#endif

    if(result == 0 && bytes_read > 0) {
        return bytes_read;
    }

    return result;
}

Sint64 efile_pwritev(efile_data_t *d, Sint64 offset, SysIOVec *iov, int iovlen) {
    efile_unix_t *u = (efile_unix_t*)d;

    Sint64 bytes_written;
    ssize_t result;

#if !defined(HAVE_PWRITEV) && !defined(HAVE_PWRITE)
    off_t original_position = lseek(u->fd, 0, SEEK_CUR);

    if(original_position < 0 || lseek(u->fd, offset, SEEK_SET) < 0) {
        u->common.posix_errno = errno;
        return -1;
    }
#endif

    bytes_written = 0;

    do {
        if(iovlen < 1) {
            result = 0;
            break;
        }

#if defined(HAVE_PWRITEV)
        result = pwritev(u->fd, iov, MIN(IOV_MAX, iovlen), offset);
#elif defined(HAVE_PWRITE)
        result = pwrite(u->fd, iov->iov_base, iov->iov_len, offset);
#else
        result = write(u->fd, iov->iov_base, iov->iov_len);
#endif

        if(result > 0) {
            shift_iov(&iov, &iovlen, result);
            bytes_written += result;
            offset += result;
        }
    } while(result > 0 || (result < 0 && errno == EINTR));

    u->common.posix_errno = errno;

#if !defined(HAVE_PWRITEV) && !defined(HAVE_PWRITE)
    if(result >= 0) {
        if(lseek(u->fd, original_position, SEEK_SET) < 0) {
            u->common.posix_errno = errno;
            return -1;
        }
    }
#endif

    if(result == 0 && bytes_written > 0) {
        return bytes_written;
    }

    return result;
}

int efile_seek(efile_data_t *d, enum efile_seek_t seek, Sint64 offset, Sint64 *new_position) {
    efile_unix_t *u = (efile_unix_t*)d;
    off_t result;
    int whence;

    switch(seek) {
        case EFILE_SEEK_BOF: whence = SEEK_SET; break;
        case EFILE_SEEK_CUR: whence = SEEK_CUR; break;
        case EFILE_SEEK_EOF: whence = SEEK_END; break;
        default: ERTS_INTERNAL_ERROR("Invalid seek parameter");
    }

    result = lseek(u->fd, offset, whence);

    /*
     * The man page for lseek (on SunOs 5) says:
     *
     * "if fildes is a remote file descriptor and offset is negative, lseek()
     * returns the file pointer even if it is negative."
     */
    if(result < 0 && errno == 0) {
        errno = EINVAL;
    }

    if(result < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    (*new_position) = result;

    return 1;
}

int efile_sync(efile_data_t *d, int data_only) {
    efile_unix_t *u = (efile_unix_t*)d;

#if defined(HAVE_FDATASYNC) && !defined(__DARWIN__)
    if(data_only) {
        if(fdatasync(u->fd) < 0) {
            u->common.posix_errno = errno;
            return 0;
        }

        return 1;
    }
#endif

#if defined(__DARWIN__) && defined(F_BARRIERFSYNC)
    if(fcntl(u->fd, F_BARRIERFSYNC) < 0) {
#elif defined(__DARWIN__) && defined(F_FULLFSYNC)
    if(fcntl(u->fd, F_FULLFSYNC) < 0) {
#else
    if(fsync(u->fd) < 0) {
#endif
        u->common.posix_errno = errno;
        return 0;
    }

    return 1;
}

int efile_advise(efile_data_t *d, Sint64 offset, Sint64 length, enum efile_advise_t advise) {
#ifdef HAVE_POSIX_FADVISE
    efile_unix_t *u = (efile_unix_t*)d;
    int p_advise;

    switch(advise) {
        case EFILE_ADVISE_NORMAL: p_advise = POSIX_FADV_NORMAL; break;
        case EFILE_ADVISE_RANDOM: p_advise = POSIX_FADV_RANDOM; break;
        case EFILE_ADVISE_SEQUENTIAL: p_advise = POSIX_FADV_SEQUENTIAL; break;
        case EFILE_ADVISE_WILL_NEED: p_advise = POSIX_FADV_WILLNEED; break;
        case EFILE_ADVISE_DONT_NEED: p_advise = POSIX_FADV_DONTNEED; break;
        case EFILE_ADVISE_NO_REUSE: p_advise = POSIX_FADV_NOREUSE; break;
        default:
            u->common.posix_errno = EINVAL;
            return 0;
    }

    if(posix_fadvise(u->fd, offset, length, p_advise) < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    return 1;
#else
    /* We'll pretend to support this syscall as it's only a recommendation even
     * on systems that do support it. */
    return 1;
#endif
}

int efile_allocate(efile_data_t *d, Sint64 offset, Sint64 length) {
    efile_unix_t *u = (efile_unix_t*)d;
    int ret = -1;

    /* We prefer OS-specific methods, but fall back to posix_fallocate on
     * failure. It's unclear whether this has any practical benefit on
     * modern systems, but the old driver did it. */

#if defined(HAVE_FALLOCATE)
    /* Linux-specific */
    do {
        ret = fallocate(u->fd, 0, offset, length);
    } while(ret < 0 && errno == EINTR);
#elif defined(F_PREALLOCATE)
    /* Mac-specific */
    off_t original_position, eof_offset;
    fstore_t fs = {};

    if(offset < 0 || length < 0 || (offset > ERTS_SINT64_MAX - length)) {
        u->common.posix_errno = EINVAL;
        return 0;
    }

    original_position = lseek(u->fd, 0, SEEK_CUR);

    if(original_position < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    eof_offset = lseek(u->fd, 0, SEEK_END);

    if(eof_offset < 0 || lseek(u->fd, original_position, SEEK_SET) < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    if(offset + length <= eof_offset) {
        /* File is already large enough. */
        return 1;
    }

    fs.fst_flags = F_ALLOCATECONTIG;
    fs.fst_posmode = F_PEOFPOSMODE;
    fs.fst_offset = 0;
    fs.fst_length = (offset + length) - eof_offset;

    ret = fcntl(u->fd, F_PREALLOCATE, &fs);
    if(ret < 0) {
        fs.fst_flags = F_ALLOCATEALL;
        ret = fcntl(u->fd, F_PREALLOCATE, &fs);
    }

    if(ret >= 0) {
        /* We MUST truncate since F_PREALLOCATE works relative to end-of-file,
         * otherwise we will expand the file on repeated calls to
         * file:allocate/3 with the same arguments. */
        ret = ftruncate(u->fd, offset + length);
        if(ret < 0) {
            u->common.posix_errno = errno;
            return 0;
        }
    }
#elif !defined(HAVE_POSIX_FALLOCATE)
    u->common.posix_errno = ENOTSUP;
    return 0;
#endif

#ifdef HAVE_POSIX_FALLOCATE
    if(ret < 0) {
        do {
            ret = posix_fallocate(u->fd, offset, length);

            /* On Linux and Solaris for example, posix_fallocate() returns a
             * positive error number on error and it does not set errno. On
             * FreeBSD however (9.0 at least), it returns -1 on error and it
             * sets errno. */
            if (ret > 0) {
                errno = ret;
                ret = -1;
            }
        } while(ret < 0 && errno == EINTR);
    }
#endif

    if(ret < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    return 1;
}

int efile_truncate(efile_data_t *d) {
    efile_unix_t *u = (efile_unix_t*)d;
    off_t offset;

    offset = lseek(u->fd, 0, SEEK_CUR);

    if(offset < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    if(ftruncate(u->fd, offset) < 0) {
        u->common.posix_errno = errno;
        return 0;
    }

    return 1;
}

static void build_file_info(struct stat *data, efile_fileinfo_t *result) {
    if(S_ISCHR(data->st_mode) || S_ISBLK(data->st_mode)) {
        result->type = EFILE_FILETYPE_DEVICE;
    } else if(S_ISDIR(data->st_mode)) {
        result->type = EFILE_FILETYPE_DIRECTORY;
    } else if(S_ISREG(data->st_mode)) {
        result->type = EFILE_FILETYPE_REGULAR;
    } else if(S_ISLNK(data->st_mode)) {
        result->type = EFILE_FILETYPE_SYMLINK;
    } else {
        result->type = EFILE_FILETYPE_OTHER;
    }

    result->a_time = (Sint64)data->st_atime;
    result->m_time = (Sint64)data->st_mtime;
    result->c_time = (Sint64)data->st_ctime;
    result->size = data->st_size;

    result->major_device = data->st_dev;
    result->minor_device = data->st_rdev;
    result->links = data->st_nlink;
    result->inode = data->st_ino;
    result->mode = data->st_mode;
    result->uid = data->st_uid;
    result->gid = data->st_gid;
}

posix_errno_t efile_read_info(const efile_path_t *path, int follow_links, efile_fileinfo_t *result) {
    struct stat data;

    if(follow_links) {
        if(stat((const char*)path->data, &data) < 0) {
            return errno;
        }
    } else {
        if(lstat((const char*)path->data, &data) < 0) {
            return errno;
        }
    }

    build_file_info(&data, result);

#ifndef NO_ACCESS
    result->access = EFILE_ACCESS_NONE;

    if(access((const char*)path->data, R_OK) == 0) {
        result->access |= EFILE_ACCESS_READ;
    }
    if(access((const char*)path->data, W_OK) == 0) {
        result->access |= EFILE_ACCESS_WRITE;
    }
#else
    /* Just look at read/write access for owner. */
    result->access = ((data.st_mode >> 6) & 07) >> 1;
#endif

    return 0;
}

static int check_access(struct stat *st) {
    int ret = EFILE_ACCESS_NONE;

    if(st->st_uid == geteuid()) {
        if(st->st_mode & S_IRUSR) {
            ret |= EFILE_ACCESS_READ;
        }
        if(st->st_mode & S_IWUSR) {
            ret |= EFILE_ACCESS_WRITE;
        }
        return ret;
    }

    if(st->st_gid == getegid()) {
        if(st->st_mode & S_IRGRP) {
            ret |= EFILE_ACCESS_READ;
        }
        if(st->st_mode & S_IWGRP) {
            ret |= EFILE_ACCESS_WRITE;
        }
        return ret;
    }

    if(st->st_mode & S_IROTH) {
        ret |= EFILE_ACCESS_READ;
    }
    if(st->st_mode & S_IWOTH) {
        ret |= EFILE_ACCESS_WRITE;
    }
    return ret;
}

posix_errno_t efile_read_handle_info(efile_data_t *d, efile_fileinfo_t *result) {
    struct stat data;
    efile_unix_t *u = (efile_unix_t*)d;

#ifdef HAVE_FSTAT
    if(fstat(u->fd, &data) < 0) {
        return errno;
    }

    build_file_info(&data, result);
    result->access = check_access(&data);

    return 0;
#else
    return ENOTSUP;
#endif
}

posix_errno_t efile_set_permissions(const efile_path_t *path, Uint32 permissions) {
    const mode_t MUTABLE_MODES = (S_ISUID | S_ISGID | S_IRWXU | S_IRWXG | S_IRWXO);
    mode_t new_modes = permissions & MUTABLE_MODES;

    if(chmod((const char*)path->data, new_modes) < 0) {
        new_modes &= ~(S_ISUID | S_ISGID);

        if (chmod((const char*)path->data, new_modes) < 0) {
            return errno;
        }
    }

    return 0;
}

posix_errno_t efile_set_owner(const efile_path_t *path, Sint32 owner, Sint32 group) {
    if(chown((const char*)path->data, owner, group) < 0) {
        return errno;
    }

    return 0;
}

posix_errno_t efile_set_time(const efile_path_t *path, Sint64 a_time, Sint64 m_time, Sint64 c_time) {
    struct utimbuf tval;

    tval.actime = (time_t)a_time;
    tval.modtime = (time_t)m_time;

    (void)c_time;

    if(utime((const char*)path->data, &tval) < 0) {
        return errno;
    }

    return 0;
}

posix_errno_t efile_read_link(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result) {
    ErlNifBinary result_bin;

    if(!enif_alloc_binary(256, &result_bin)) {
        return ENOMEM;
    }

    for(;;) {
        ssize_t bytes_copied;

        bytes_copied = readlink((const char*)path->data, (char*)result_bin.data,
            result_bin.size);

        if(bytes_copied <= 0) {
            posix_errno_t saved_errno = errno;
            enif_release_binary(&result_bin);
            return saved_errno;
        } else if(bytes_copied < result_bin.size) {
            if(!enif_realloc_binary(&result_bin, bytes_copied)) {
                enif_release_binary(&result_bin);
                return ENOMEM;
            }

            (*result) = enif_make_binary(env, &result_bin);

            return 0;
        }

        /* The result didn't fit into the buffer, so we'll try again with a
         * larger one. */

        if(!enif_realloc_binary(&result_bin, result_bin.size * 2)) {
            enif_release_binary(&result_bin);
            return ENOMEM;
        }
    }
}

static int is_ignored_name(int name_length, const char *name) {
    if(name_length == 1 && name[0] == '.') {
        return 1;
    } else if(name_length == 2 && memcmp(name, "..", 2) == 0) {
        return 1;
    }

    return 0;
}

posix_errno_t efile_list_dir(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result) {
    ERL_NIF_TERM list_head;

    struct dirent *dir_entry;
    DIR *dir_stream;

    dir_stream = opendir((const char*)path->data);
    if(dir_stream == NULL) {
        posix_errno_t saved_errno = errno;
        *result = enif_make_list(env, 0);
        return saved_errno;
    }

    list_head = enif_make_list(env, 0);
    dir_entry = readdir(dir_stream);

    while(dir_entry != NULL) {
        int name_length = strlen(dir_entry->d_name);

        if(!is_ignored_name(name_length, dir_entry->d_name)) {
            unsigned char *name_bytes;
            ERL_NIF_TERM name_term;

            name_bytes = enif_make_new_binary(env, name_length, &name_term);
            sys_memcpy(name_bytes, dir_entry->d_name, name_length);

            list_head = enif_make_list_cell(env, name_term, list_head);
        }

        dir_entry = readdir(dir_stream);
    }

    (*result) = list_head;
    closedir(dir_stream);

    return 0;
}

posix_errno_t efile_rename(const efile_path_t *old_path, const efile_path_t *new_path) {
    if(rename((const char*)old_path->data, (const char*)new_path->data) < 0) {
        if(errno == ENOTEMPTY) {
            return EEXIST;
        }

        if(strcmp((const char*)old_path->data, "/") == 0) {
            /* Alpha reports renaming / as EBUSY and Linux reports it as EACCES
             * instead of EINVAL.*/
             return EINVAL;
        }

        return errno;
    }

    return 0;
}

posix_errno_t efile_make_hard_link(const efile_path_t *existing_path, const efile_path_t *new_path) {
    if(link((const char*)existing_path->data, (const char*)new_path->data) < 0) {
        return errno;
    }

    return 0;
}

posix_errno_t efile_make_soft_link(const efile_path_t *existing_path, const efile_path_t *new_path) {
    if(symlink((const char*)existing_path->data, (const char*)new_path->data) < 0) {
        return errno;
    }

    return 0;
}

posix_errno_t efile_make_dir(const efile_path_t *path) {
#ifdef NO_MKDIR_MODE
    if(mkdir((const char*)path->data) < 0) {
#else
    if(mkdir((const char*)path->data, DIR_MODE) < 0) {
#endif
        return errno;
    }

    return 0;
}

posix_errno_t efile_del_file(const efile_path_t *path) {
    if(unlink((const char*)path->data) < 0) {
        /* Linux sets the wrong error code. */
        if(errno == EISDIR) {
            return EPERM;
        }

        return errno;
    }

    return 0;
}

posix_errno_t efile_del_dir(const efile_path_t *path) {
    if(rmdir((const char*)path->data) < 0) {
        posix_errno_t saved_errno = errno;

        if(saved_errno == ENOTEMPTY) {
            saved_errno = EEXIST;
        }

        /* The error code might be wrong if we're trying to delete the current
         * directory. */
        if(saved_errno == EEXIST) {
            struct stat path_stat, cwd_stat;
            int has_stat;

            has_stat = (stat((const char*)path->data, &path_stat) == 0);
            has_stat &= (stat(".", &cwd_stat) == 0);

            if(has_stat && path_stat.st_ino == cwd_stat.st_ino) {
                if(path_stat.st_dev == cwd_stat.st_dev) {
                    return EINVAL;
                }
            }
        }

        return saved_errno;
    }

    return 0;
}

posix_errno_t efile_set_cwd(const efile_path_t *path) {
    if(chdir((const char*)path->data) < 0) {
        return errno;
    }

    return 0;
}

posix_errno_t efile_get_device_cwd(ErlNifEnv *env, int device_index, ERL_NIF_TERM *result) {
    (void)device_index;
    (void)result;
    (void)env;

    return ENOTSUP;
}

posix_errno_t efile_get_cwd(ErlNifEnv *env, ERL_NIF_TERM *result) {
    ErlNifBinary result_bin;
    size_t bytes_copied;

    if(!enif_alloc_binary(256, &result_bin)) {
        return ENOMEM;
    }

    while(getcwd((char*)result_bin.data, result_bin.size) == NULL) {
        posix_errno_t saved_errno = errno;

        if(saved_errno != ERANGE) {
            enif_release_binary(&result_bin);
            return saved_errno;
        } else {
            if(!enif_realloc_binary(&result_bin, result_bin.size * 2)) {
                enif_release_binary(&result_bin);
                return ENOMEM;
            }
        }
    }

    /* getcwd(2) guarantees null-termination. */
    bytes_copied = strlen((const char*)result_bin.data);

    if(!enif_realloc_binary(&result_bin, bytes_copied)) {
        enif_release_binary(&result_bin);
        return ENOMEM;
    }

    (*result) = enif_make_binary(env, &result_bin);

    return 0;
}

posix_errno_t efile_altname(ErlNifEnv *env, const efile_path_t *path, ERL_NIF_TERM *result) {
    (void)path;
    (void)result;

    return ENOTSUP;
}
