/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
/*
 * Purpose: Provides file and directory operations for OSE.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#if defined(HAVE_POSIX_FALLOCATE) && !defined(__sun) && !defined(__sun__)
#define _XOPEN_SOURCE 600
#endif
#if !defined(_GNU_SOURCE) && defined(HAVE_LINUX_FALLOC_H)
#define _GNU_SOURCE
#endif
#include "sys.h"
#include "erl_driver.h"
#include "erl_efile.h"
#if defined(DARWIN) || defined(HAVE_LINUX_FALLOC_H) || defined(HAVE_POSIX_FALLOCATE)
#include "fcntl.h"
#endif
#include "ose.h"
#include "unistd.h"
#include "sys/stat.h"
#include "dirent.h"
#include "sys/time.h"
#include "time.h"
#include "assert.h"

/* Find a definition of MAXIOV, that is used in the code later. */
#if defined IOV_MAX
#define MAXIOV IOV_MAX
#elif defined UIO_MAXIOV
#define MAXIOV UIO_MAXIOV
#else
#define MAXIOV 16
#endif

/*
 * Macros for testing file types.
 */

#define ISDIR(st) (((st).st_mode & S_IFMT) == S_IFDIR)
#define ISREG(st) (((st).st_mode & S_IFMT) == S_IFREG)
#define ISDEV(st) \
    (((st).st_mode&S_IFMT) == S_IFCHR || ((st).st_mode&S_IFMT) == S_IFBLK)
#define ISLNK(st) (((st).st_mode & S_IFLNK) == S_IFLNK)
#ifdef NO_UMASK
#define FILE_MODE 0644
#define DIR_MODE  0755
#else
#define FILE_MODE 0666
#define DIR_MODE  0777
#endif

#define IS_DOT_OR_DOTDOT(s) \
    (s[0] == '.' && (s[1] == '\0' || (s[1] == '.' && s[2] == '\0')))

/*
 * Macros for handling local file descriptors
 * and mutexes.
 *
 * Handling of files like this is necessary because OSE
 * does not allow seeking after the end of a file. So
 * what we do it emulate this by keeping track of the size
 * of the file and where the file's positions is. If a
 * write happens after eof then we pad it.
 *
 * Given time this should be rewritten to get rid of the
 * mutex and use the port lock to protect the data. This
 * could be done be done by adapting the efile api for some
 * calls to allow some meta-data to be associated with the
 * open file.
 */

#define L_FD_IS_VALID(fd_data) ((fd_data)->beyond_eof > 0)
#define L_FD_INVALIDATE(fd_data) (fd_data)->beyond_eof = 0
#define L_FD_CUR(fd_data) (fd_data)->pos
#define L_FD_OFFS_BEYOND_EOF(fd_data, offs) \
    (((fd_data)->size > offs) ? 0 : 1)

#define L_FD_FAIL -1
#define L_FD_SUCCESS 1
#define L_FD_PAD_SIZE 255

struct fd_meta {
    ErlDrvMutex *meta_mtx;
    struct fd_data *fd_data_list;
};

struct fd_data {
    int fd;
    struct fd_data *next;
    struct fd_data *prev;
    int pos;
    int beyond_eof;
    size_t size;
#ifdef DEBUG
    PROCESS owner;
#endif
};

static int l_invalidate_local_fd(int fd);
static int l_pad_file(struct fd_data *fd_data, off_t offset);
static int check_error(int result, Efile_error* errInfo);
static struct fd_data* l_new_fd(void);
static int l_remove_local_fd(int fd);
static struct fd_data* l_find_local_fd(int fd);
static int l_update_local_fd(int fd, int pos, int size);

static struct fd_meta* fdm = NULL;


/***************************************************************************/

static int
l_remove_local_fd(int fd)
{
    struct fd_data *fd_data;
    fd_data = l_find_local_fd(fd);

    if (fd_data == NULL) {
        return L_FD_FAIL;
    }
#ifdef DEBUG
    assert(fd_data->owner == current_process());
#endif
    erl_drv_mutex_lock(fdm->meta_mtx);
    /* head ? */
    if (fd_data == fdm->fd_data_list) {
        if (fd_data->next != NULL) {
            /* remove link to head */
            fd_data->next->prev = NULL;
            /* set new head */
            fdm->fd_data_list = fd_data->next;
        }
        else {
            /* head is lonely */
            fdm->fd_data_list = NULL;
        }
    }
    else { /* not head */
        if (fd_data->prev == NULL) {
            erl_drv_mutex_unlock(fdm->meta_mtx);
            return L_FD_FAIL;
        }
        else {
            if (fd_data->next != NULL) {
                fd_data->next->prev = fd_data->prev;
                fd_data->prev->next = fd_data->next;
            }
            else {
                fd_data->prev->next = NULL;
            }
        }
    }

    /* scramble values */
    fd_data->beyond_eof = -1;
    fd_data->next = NULL;
    fd_data->prev = NULL;
    fd_data->fd = -1;

    /* unlock and clean */
    driver_free(fd_data);
    erl_drv_mutex_unlock(fdm->meta_mtx);

    return L_FD_SUCCESS;
}

/***************************************************************************/

static int
l_invalidate_local_fd(int fd) {
    struct fd_data *fd_data;

    if ((fd_data = l_find_local_fd(fd)) == NULL) {
        return L_FD_FAIL;
    }

    fd_data->beyond_eof = 0;
    return L_FD_SUCCESS;
}

/****************************************************************************/

static struct fd_data*
l_find_local_fd(int fd) {
    struct fd_data *fd_data;

    fd_data = NULL;
    erl_drv_mutex_lock(fdm->meta_mtx);
    for (fd_data = fdm->fd_data_list; fd_data != NULL; ) {
        if (fd_data->fd == fd) {
#ifdef DEBUG
            assert(fd_data->owner == current_process());
#endif
            break;
        }
        fd_data = fd_data->next;
    }
    erl_drv_mutex_unlock(fdm->meta_mtx);
    return fd_data;
}

/***************************************************************************/

static struct fd_data*
l_new_fd(void) {
    struct fd_data *fd_data;

    fd_data = driver_alloc(sizeof(struct fd_data));
    if (fd_data == NULL) {
        return NULL;
    }
    erl_drv_mutex_lock(fdm->meta_mtx);
    if (fdm->fd_data_list == NULL) {
        fdm->fd_data_list = fd_data;
        fdm->fd_data_list->prev = NULL;
        fdm->fd_data_list->next = NULL;
    }
    else {
        fd_data->next = fdm->fd_data_list;
        fdm->fd_data_list = fd_data;
        fdm->fd_data_list->prev = NULL;
    }
#ifdef DEBUG
    fd_data->owner = current_process();
#endif
    erl_drv_mutex_unlock(fdm->meta_mtx);
    return fd_data;
}

/***************************************************************************/

static int
l_update_local_fd(int fd, int pos, int size) {
    struct fd_data *fd_data = NULL;

    fd_data = l_find_local_fd(fd);
    /* new fd to handle? */
    if (fd_data == NULL) {
        fd_data = l_new_fd();
        if (fd_data == NULL) {
            /* out of memory */
            return L_FD_FAIL;
        }
    }
    fd_data->size = size;
    fd_data->pos = pos;
    fd_data->fd = fd;
    fd_data->beyond_eof = 1;

    return L_FD_SUCCESS;
}

/***************************************************************************/

static int
l_pad_file(struct fd_data *fd_data, off_t offset) {
    int size_dif;
    int written = 0;
    int ret_val = L_FD_SUCCESS;
    char padding[L_FD_PAD_SIZE];

    size_dif = (offset - fd_data->size);
    memset(&padding, '\0', L_FD_PAD_SIZE);

    while (size_dif > 0) {
        written = write(fd_data->fd, padding,
                (size_dif < L_FD_PAD_SIZE) ?
                size_dif : L_FD_PAD_SIZE);
        if (written < 0 && errno != EINTR && errno != EAGAIN) {
            ret_val = -1;
            break;
        }
        size_dif -= written;
    }
    L_FD_INVALIDATE(fd_data);
    return ret_val;
}

/***************************************************************************/

static int
check_error(int result, Efile_error *errInfo) {
    if (result < 0) {
        errInfo->posix_errno = errInfo->os_errno = errno;
        return 0;
    }
    return 1;
}

/***************************************************************************/

int
efile_init() {
    fdm = driver_alloc(sizeof(struct fd_meta));
    if (fdm == NULL) {
        return L_FD_FAIL;
    }
    fdm->meta_mtx = erl_drv_mutex_create("ose_efile local fd mutex\n");
    erl_drv_mutex_lock(fdm->meta_mtx);
    fdm->fd_data_list = NULL;
    erl_drv_mutex_unlock(fdm->meta_mtx);
    return L_FD_SUCCESS;
}

/***************************************************************************/

int
efile_mkdir(Efile_error* errInfo,       /* Where to return error codes. */
        char* name)                 /* Name of directory to create. */
{
#ifdef NO_MKDIR_MODE
    return check_error(mkdir(name), errInfo);
#else
    int res = mkdir(name, DIR_MODE);
    if (res < 0 && errno == EINVAL) {
      errno = ENOENT;
    }
    return check_error(res, errInfo);
#endif
}

/***************************************************************************/

int
efile_rmdir(Efile_error* errInfo,       /* Where to return error codes. */
        char* name)                 /* Name of directory to delete. */
{
    if (rmdir(name) == 0) {
        return 1;
    }
    if (errno == ENOTEMPTY) {
        errno = EEXIST;
    }
    if (errno == EEXIST || errno == EINVAL) {
        int saved_errno = errno;
        struct stat file_stat;
        struct stat cwd_stat;

        if(stat(name, &file_stat) != 0) {
            errno = ENOENT;
            return check_error(-1, errInfo);
        }
        /*
         *  The error code might be wrong if this is the current directory.
         */
        if (stat(name, &file_stat) == 0 && stat(".", &cwd_stat) == 0 &&
                file_stat.st_ino == cwd_stat.st_ino &&
                file_stat.st_dev == cwd_stat.st_dev) {
            saved_errno = EACCES;
        }
        errno = saved_errno;
    }
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_delete_file(Efile_error* errInfo, /* Where to return error codes. */
        char* name)           /* Name of file to delete. */
{
    struct stat statbuf;

    if (stat(name, &statbuf) >= 0) {
        /* Do not let unlink() remove directories */
        if (ISDIR(statbuf)) {
            errno = EPERM;
            return check_error(-1, errInfo);
        }

        if (unlink(name) == 0) {
            return 1;
        }

        if (errno == EISDIR) {
            errno = EPERM;
            return check_error(-1, errInfo);
        }
    }
    else {
        if (errno == EINVAL) {
            errno = ENOENT;
            return check_error(-1, errInfo);
        }
    }
    return check_error(-1, errInfo);
}

/*
 *---------------------------------------------------------------------------
 *
 *      Changes the name of an existing file or directory, from src to dst.
 *      If src and dst refer to the same file or directory, does nothing
 *      and returns success.  Otherwise if dst already exists, it will be
 *      deleted and replaced by src subject to the following conditions:
 *          If src is a directory, dst may be an empty directory.
 *          If src is a file, dst may be a file.
 *      In any other situation where dst already exists, the rename will
 *      fail.
 *
 * Results:
 *      If the directory was successfully created, returns 1.
 *      Otherwise the return value is 0 and errno is set to
 *      indicate the error.  Some possible values for errno are:
 *
 *      EACCES:     src or dst parent directory can't be read and/or written.
 *      EEXIST:     dst is a non-empty directory.
 *      EINVAL:     src is a root directory or dst is a subdirectory of src.
 *      EISDIR:     dst is a directory, but src is not.
 *      ENOENT:     src doesn't exist, or src or dst is "".
 *      ENOTDIR:    src is a directory, but dst is not.
 *      EXDEV:      src and dst are on different filesystems.
 *
 * Side effects:
 *      The implementation of rename may allow cross-filesystem renames,
 *      but the caller should be prepared to emulate it with copy and
 *      delete if errno is EXDEV.
 *
 *---------------------------------------------------------------------------
 */

int
efile_rename(Efile_error* errInfo,      /* Where to return error codes. */
        char* src,                 /* Original name. */
        char* dst)                 /* New name. */
{

    /* temporary fix AFM does not recognize ./<file name>
     * in destination remove pending on adaption of AFM fix
     */

    char *dot_str;
    if (dst != NULL) {
        dot_str = strchr(dst, '.');
        if (dot_str && dot_str == dst && dot_str[1] == '/') {
            dst = dst+2;
        }
    }

    if (rename(src, dst) == 0) {
        return 1;
    }
    if (errno == ENOTEMPTY) {
        errno = EEXIST;
    }
    if (errno == EINVAL) {
        struct stat file_stat;

        if (stat(dst, &file_stat)== 0) {
            if (ISDIR(file_stat)) {
                errno = EISDIR;
            }
            else if (ISREG(file_stat)) {
                errno = ENOTDIR;
            }
            else {
                errno = EINVAL;
            }
        }
        else {
            errno = EINVAL;
        }
    }

    if (strcmp(src, "/") == 0) {
        errno = EINVAL;
    }
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_chdir(Efile_error* errInfo,   /* Where to return error codes. */
        char* name)             /* Name of directory to make current. */
{
    return check_error(chdir(name), errInfo);
}

/***************************************************************************/

int
efile_getdcwd(Efile_error* errInfo,     /* Where to return error codes. */
        int drive,                /* 0 - current, 1 - A, 2 - B etc. */
        char* buffer,             /* Where to return the current
                                     directory. */
        size_t size)              /* Size of buffer. */
{
    if (drive == 0) {
        if (getcwd(buffer, size) == NULL)
            return check_error(-1, errInfo);

        return 1;
    }

    /*
     * Drives other than 0 is not supported on Unix.
     */

    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_readdir(Efile_error* errInfo,     /* Where to return error codes. */
        char* name,               /* Name of directory to open. */
        EFILE_DIR_HANDLE* p_dir_handle,   /* Pointer to directory
                                             handle of
                                             open directory.*/
        char* buffer,             /* Pointer to buffer for
                                     one filename. */
        size_t *size)             /* in-out Size of buffer, length
                                     of name. */
{
    DIR *dp;                    /* Pointer to directory structure. */
    struct dirent* dirp;        /* Pointer to directory entry. */

    /*
     * If this is the first call, we must open the directory.
     */

    if (*p_dir_handle == NULL) {
        dp = opendir(name);
        if (dp == NULL)
            return check_error(-1, errInfo);
        *p_dir_handle = (EFILE_DIR_HANDLE) dp;
    }

    /*
     * Retrieve the name of the next file using the directory handle.
     */

    dp = *((DIR **)((void *)p_dir_handle));
    for (;;) {
        dirp = readdir(dp);
        if (dirp == NULL) {
            closedir(dp);
            return 0;
        }
        if (IS_DOT_OR_DOTDOT(dirp->d_name))
            continue;
        buffer[0] = '\0';
        strncat(buffer, dirp->d_name, (*size)-1);
        *size = strlen(dirp->d_name);
        return 1;
    }
}

/***************************************************************************/

int
efile_openfile(Efile_error* errInfo,    /* Where to return error codes. */
        char* name,              /* Name of directory to open. */
        int flags,               /* Flags to user for opening. */
        int* pfd,                /* Where to store the file
                                    descriptor. */
        Sint64 *pSize)           /* Where to store the size of the
                                    file. */
{
    struct stat statbuf;
    int fd;
    int mode;                   /* Open mode. */

    if (stat(name, &statbuf) >= 0 && !ISREG(statbuf)) {
        errno = EISDIR;
        return check_error(-1, errInfo);
    }

    switch (flags & (EFILE_MODE_READ|EFILE_MODE_WRITE)) {
        case EFILE_MODE_READ:
            mode = O_RDONLY;
            break;
        case EFILE_MODE_WRITE:
            if (flags & EFILE_NO_TRUNCATE)
                mode = O_WRONLY | O_CREAT;
            else
                mode = O_WRONLY | O_CREAT | O_TRUNC;
            break;
        case EFILE_MODE_READ_WRITE:
            mode = O_RDWR | O_CREAT;
            break;
        default:
            errno = EINVAL;
            return check_error(-1, errInfo);
    }


    if (flags & EFILE_MODE_APPEND) {
        mode &= ~O_TRUNC;
        mode |= O_APPEND;
    }

    if (flags & EFILE_MODE_EXCL) {
        mode |= O_EXCL;
    }

    fd = open(name, mode, FILE_MODE);

    if (!check_error(fd, errInfo))
        return 0;

    *pfd = fd;
    if (pSize) {
        *pSize = statbuf.st_size;
    }
    return 1;
}

/***************************************************************************/

int
efile_may_openfile(Efile_error* errInfo, char *name) {
    struct stat statbuf;        /* Information about the file */
    int result;

    result = stat(name, &statbuf);
    if (!check_error(result, errInfo))
        return 0;
    if (!ISREG(statbuf)) {
        errno = EISDIR;
        return check_error(-1, errInfo);
    }
    return 1;
}

/***************************************************************************/

void
efile_closefile(int fd)
{
    if (l_find_local_fd(fd) != NULL) {
        l_remove_local_fd(fd);
    }
    close(fd);
}

/***************************************************************************/

int
efile_fdatasync(Efile_error *errInfo, /* Where to return error codes. */
        int fd)               /* File descriptor for file to sync data. */
{
    return efile_fsync(errInfo, fd);
}

/***************************************************************************/

int
efile_fsync(Efile_error *errInfo, /* Where to return error codes. */
        int fd)               /* File descriptor for file to sync. */
{
    return check_error(fsync(fd), errInfo);
}

/***************************************************************************/

int
efile_fileinfo(Efile_error* errInfo, Efile_info* pInfo,
        char* name, int info_for_link)
{
    struct stat statbuf;        /* Information about the file */
    int result;

    result = stat(name, &statbuf);
    if (!check_error(result, errInfo)) {
        return 0;
    }

#if SIZEOF_OFF_T == 4
    pInfo->size_high = 0;
#else
    pInfo->size_high = (Uint32)(statbuf.st_size >> 32);
#endif
    pInfo->size_low = (Uint32)statbuf.st_size;

#ifdef NO_ACCESS
    /* Just look at read/write access for owner. */

    pInfo->access = ((statbuf.st_mode >> 6) & 07) >> 1;

#else
    pInfo->access = FA_NONE;
    if (access(name, R_OK) == 0)
        pInfo->access |= FA_READ;
    if (access(name, W_OK) == 0)
        pInfo->access |= FA_WRITE;

#endif

    if (ISDEV(statbuf))
        pInfo->type = FT_DEVICE;
    else if (ISDIR(statbuf))
        pInfo->type = FT_DIRECTORY;
    else if (ISREG(statbuf))
        pInfo->type = FT_REGULAR;
    else if (ISLNK(statbuf))
        pInfo->type = FT_SYMLINK;
    else
        pInfo->type = FT_OTHER;

    pInfo->accessTime   = statbuf.st_atime;
    pInfo->modifyTime   = statbuf.st_mtime;
    pInfo->cTime        = statbuf.st_ctime;

    pInfo->mode         = statbuf.st_mode;
    pInfo->links        = statbuf.st_nlink;
    pInfo->major_device = statbuf.st_dev;
    pInfo->inode        = statbuf.st_ino;
    pInfo->uid          = statbuf.st_uid;
    pInfo->gid          = statbuf.st_gid;

    return 1;
}

/***************************************************************************/

int
efile_write_info(Efile_error *errInfo, Efile_info *pInfo, char *name)
{
    /*
     * On some systems chown will always fail for a non-root user unless
     * POSIX_CHOWN_RESTRICTED is not set.  Others will succeed as long as
     * you don't try to chown a file to someone besides youself.
     */
    if (pInfo->mode != -1) {
        mode_t newMode = pInfo->mode & (S_ISUID | S_ISGID |
                S_IRWXU | S_IRWXG | S_IRWXO);
        if (chmod(name, newMode)) {
            newMode &= ~(S_ISUID | S_ISGID);
            if (chmod(name, newMode)) {
                return check_error(-1, errInfo);
            }
        }
    }

    return 1;
}

/***************************************************************************/

int
efile_write(Efile_error* errInfo,       /* Where to return error codes. */
        int flags,                  /* Flags given when file was
                                       opened. */
        int fd,                     /* File descriptor to write to. */
        char* buf,                  /* Buffer to write. */
        size_t count)               /* Number of bytes to write. */
{
    ssize_t written;                    /* Bytes written in last operation. */
    struct fd_data *fd_data;

    if ((fd_data = l_find_local_fd(fd)) != NULL) {
        if (L_FD_IS_VALID(fd_data)) {
            /* we are beyond eof and need to pad*/
            if (l_pad_file(fd_data, L_FD_CUR(fd_data)) < 0) {
                return check_error(-1, errInfo);
            }
        }
    }

    while (count > 0)  {
        if ((written = write(fd, buf, count)) < 0) {
            if (errno != EINTR) {
                return check_error(-1, errInfo);
            }
            else {
                written = 0;
            }
        }
        ASSERT(written <= count);
        buf += written;
        count -= written;
    }
    return 1;
}

/***************************************************************************/

int
efile_writev(Efile_error* errInfo,   /* Where to return error codes */
        int flags,              /* Flags given when file was
                                 * opened */
        int fd,                 /* File descriptor to write to */
        SysIOVec* iov,          /* Vector of buffer structs.
                                 * The structs may be changed i.e.
                                 * due to incomplete writes */
        int iovcnt)             /* Number of structs in vector */
{
    struct fd_data *fd_data;
    int cnt = 0;                     /* Buffers so far written */

    ASSERT(iovcnt >= 0);
    if ((fd_data = l_find_local_fd(fd)) != NULL) {
        if (L_FD_IS_VALID(fd_data)) {
            /* we are beyond eof and need to pad*/
            if (l_pad_file(fd_data, L_FD_CUR(fd_data)) < 0) {
                return check_error(-1, errInfo);
            }
        }
    }
    while (cnt < iovcnt) {
        if ((! iov[cnt].iov_base) || (iov[cnt].iov_len <= 0)) {
            /* Empty buffer - skip */
            cnt++;
        }
        else { /* Non-empty buffer */
            ssize_t w;                   /* Bytes written in this call */
            do {
                w = write(fd, iov[cnt].iov_base, iov[cnt].iov_len);
            } while (w < 0 && errno == EINTR);

            ASSERT(w <= iov[cnt].iov_len || w == -1);

            if (w < 0) {
                return check_error(-1, errInfo);
            }
            /* Move forward to next buffer to write */
            for (; cnt < iovcnt && w > 0; cnt++) {
                if (iov[cnt].iov_base && iov[cnt].iov_len > 0) {
                    if (w < iov[cnt].iov_len) {
                        /* Adjust the buffer for next write */
                        iov[cnt].iov_len -= w;
                        iov[cnt].iov_base += w;
                        w = 0;
                        break;
                    }
                    else {
                        w -= iov[cnt].iov_len;
                    }
                }
            }
            ASSERT(w == 0);
        } /* else Non-empty buffer */
    } /* while (cnt< iovcnt) */
    return 1;
}

/***************************************************************************/

int
efile_read(Efile_error* errInfo,     /* Where to return error codes. */
        int flags,                  /* Flags given when file was opened. */
        int fd,                     /* File descriptor to read from. */
        char* buf,                  /* Buffer to read into. */
        size_t count,       /* Number of bytes to read. */
        size_t *pBytesRead)         /* Where to return number of
                                       bytes read. */
{
    ssize_t n;
    struct fd_data *fd_data;

    if ((fd_data = l_find_local_fd(fd)) != NULL) {
        if (L_FD_IS_VALID(fd_data)) {
            *pBytesRead = 0;
            return 1;
        }
    }
    for (;;)  {
        if ((n = read(fd, buf, count)) >= 0) {
            break;
        }
        else if (errno != EINTR) {
            return check_error(-1, errInfo);
        }
    }
    if (fd_data != NULL && L_FD_IS_VALID(fd_data)) {
        L_FD_INVALIDATE(fd_data);
    }
    *pBytesRead = (size_t) n;
    return 1;
}

/* pread() and pwrite()                                                   */
/* Some unix systems, notably Solaris has these syscalls                  */
/* It is especially nice for i.e. the dets module to have support         */
/* for this, even if the underlying OS dosn't support it, it is           */
/* reasonably easy to work around by first calling seek, and then         */
/* calling read().                                                        */
/* This later strategy however changes the file pointer, which pread()    */
/* does not do. We choose to ignore this and say that the location        */
/* of the file pointer is undefined after a call to any of the p functions*/


int
efile_pread(Efile_error* errInfo,     /* Where to return error codes. */
        int fd,                /* File descriptor to read from. */
        Sint64 offset,            /* Offset in bytes from BOF. */
        char* buf,                     /* Buffer to read into. */
        size_t count,          /* Number of bytes to read. */
        size_t *pBytesRead)            /* Where to return
                                          number of bytes read. */
{
    int res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
    if (res) {
        return efile_read(errInfo, 0, fd, buf, count, pBytesRead);
    } else {
        return res;
    }
}


/***************************************************************************/

int
efile_pwrite(Efile_error* errInfo,  /* Where to return error codes. */
        int fd,                /* File descriptor to write to. */
        char* buf,             /* Buffer to write. */
        size_t count,          /* Number of bytes to write. */
        Sint64 offset)         /* where to write it */
{
    int res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);

    if (res) {
        return efile_write(errInfo, 0, fd, buf, count);
    } else {
        return res;
    }
}

/***************************************************************************/

int
efile_seek(Efile_error* errInfo,      /* Where to return error codes. */
        int fd,                    /* File descriptor to do the seek on. */
        Sint64 offset,             /* Offset in bytes from the given
                                      origin. */
        int origin,                /* Origin of seek (SEEK_SET, SEEK_CUR,
                                      SEEK_END). */
        Sint64 *new_location)      /* Resulting new location in file. */
{
    off_t off, result;
    off = (off_t) offset;

    switch (origin) {
        case EFILE_SEEK_SET:
            origin = SEEK_SET;
            break;
        case EFILE_SEEK_CUR:
            origin = SEEK_CUR;
            break;
        case EFILE_SEEK_END:
            origin = SEEK_END;
            break;
        default:
            errno = EINVAL;
            return check_error(-1, errInfo);
    }

    if (off != offset) {
        errno = EINVAL;
        return check_error(-1, errInfo);
    }

    errno = 0;
    result = lseek(fd, off, origin);

    if (result >= 0) {
        l_invalidate_local_fd(fd);
    }

    if (result < 0)
    {
        if (errno == ENOSYS) {
            int size, cur_pos;

            if (off < 0) {
                errno = EINVAL;
                return check_error(-1, errInfo);
            }

            cur_pos = lseek(fd, 0, SEEK_CUR);
            size = lseek(fd, 0, SEEK_END);

            if (origin == SEEK_SET) {
                result = offset;
            }
            else if (origin == SEEK_CUR) {
                result = offset + cur_pos;
            }
            else if (origin == SEEK_END) {
                result = size + offset;
            }

            /* sanity check our result */
            if (size > result) {
                return check_error(-1, errInfo);
            }

            /* store the data localy */
            l_update_local_fd(fd, result, size);

            /* reset the original file position */
            if (origin != SEEK_END) {
                lseek(fd, cur_pos, SEEK_SET);
            }
        }
        else if (errno == 0) {
            errno = EINVAL;
        }
    }

    if (new_location) {
        *new_location = result;
    }

    return 1;
}

/***************************************************************************/

int
efile_truncate_file(Efile_error* errInfo, int *fd, int flags)
{
    off_t offset;
    struct fd_data *fd_data;

    if ((fd_data = l_find_local_fd(*fd)) != NULL && L_FD_IS_VALID(fd_data)) {
        offset = L_FD_CUR(fd_data);
    }
    else {
        offset = lseek(*fd, 0, SEEK_CUR);
    }

    return check_error(((offset >= 0) &&
                (ftruncate(*fd, offset) == 0)) ? 1 : -1, errInfo);
}

/***************************************************************************/

int
efile_readlink(Efile_error* errInfo, char* name, char* buffer, size_t size)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_altname(Efile_error* errInfo, char* name, char* buffer, size_t size)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_link(Efile_error* errInfo, char* old, char* new)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_symlink(Efile_error* errInfo, char* old, char* new)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

/***************************************************************************/

int
efile_fadvise(Efile_error* errInfo, int fd, Sint64 offset,
        Sint64 length, int advise)
{
    return check_error(posix_fadvise(fd, offset, length, advise), errInfo);
}

/***************************************************************************/

static int
call_posix_fallocate(int fd, Sint64 offset, Sint64 length)
{
    int ret;

    /*
     * On Linux and Solaris for example, posix_fallocate() returns
     * a positive error number on error and it does not set errno.
     * On FreeBSD however (9.0 at least), it returns -1 on error
     * and it sets errno.
     */
    do {
        ret = posix_fallocate(fd, (off_t) offset, (off_t) length);
        if (ret > 0) {
            errno = ret;
            ret = -1;
        }
    } while (ret != 0 && errno == EINTR);

    return ret;
}

/***************************************************************************/

int
efile_fallocate(Efile_error* errInfo, int fd, Sint64 offset, Sint64 length)
{
    return check_error(call_posix_fallocate(fd, offset, length), errInfo);
}
