/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
/*
 * Purpose: Provides file and directory operations for Unix.
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
#include <utime.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/types.h>
#include <sys/uio.h>
#if defined(HAVE_SENDFILE) && (defined(__FreeBSD__) || defined(__DragonFly__))
/* Need to define __BSD_VISIBLE in order to expose prototype of sendfile */
#define __BSD_VISIBLE 1
#include <sys/socket.h>
#endif
#endif
#if defined(HAVE_SENDFILE) && (defined(__linux__) || (defined(__sun) && defined(__SVR4)))
#include <sys/sendfile.h>
#endif

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#if defined(__DARWIN__) || defined(HAVE_LINUX_FALLOC_H) || defined(HAVE_POSIX_FALLOCATE)
#include <fcntl.h>
#endif

#ifdef HAVE_LINUX_FALLOC_H
#include <linux/falloc.h>
#endif

#ifdef SUNOS4
#  define getcwd(buf, size) getwd(buf)
#endif

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

#define ISDIR(st) (S_ISDIR((st).st_mode))
#define ISREG(st) (S_ISREG((st).st_mode))
#define ISDEV(st) (S_ISCHR((st).st_mode) || S_ISBLK((st).st_mode))
#define ISLNK(st) (S_ISLNK((st).st_mode))
#ifdef NO_UMASK
#define FILE_MODE 0644
#define DIR_MODE  0755
#else
#define FILE_MODE 0666
#define DIR_MODE  0777
#endif

#define IS_DOT_OR_DOTDOT(s) \
    (s[0] == '.' && (s[1] == '\0' || (s[1] == '.' && s[2] == '\0')))

static int check_error(int result, Efile_error* errInfo);

static int
check_error(int result, Efile_error *errInfo)
{
    if (result < 0) {
	errInfo->posix_errno = errInfo->os_errno = errno;
	return 0;
    }
    return 1;
}

int
efile_init() {
   return 1;
}

int
efile_mkdir(Efile_error* errInfo,	/* Where to return error codes. */
	    char* name)			/* Name of directory to create. */
{
#ifdef NO_MKDIR_MODE
    return check_error(mkdir(name), errInfo);
#else
    return check_error(mkdir(name, DIR_MODE), errInfo);
#endif
}

int
efile_rmdir(Efile_error* errInfo,	/* Where to return error codes. */
	    char* name)			/* Name of directory to delete. */
{
    if (rmdir(name) == 0) {
	return 1;
    }
    if (errno == ENOTEMPTY) {
	errno = EEXIST;
    }
    if (errno == EEXIST) {
	int saved_errno = errno;
	struct stat file_stat;
	struct stat cwd_stat;

	/*
	 *  The error code might be wrong if this is the current directory.
	 */

	if (stat(name, &file_stat) == 0 && stat(".", &cwd_stat) == 0 &&
	    file_stat.st_ino == cwd_stat.st_ino &&
	    file_stat.st_dev == cwd_stat.st_dev) {
	    saved_errno = EINVAL;
	}
	errno = saved_errno;
    }
    return check_error(-1, errInfo);
}

int
efile_delete_file(Efile_error* errInfo,	/* Where to return error codes. */
		  char* name)		/* Name of file to delete. */
{
    if (unlink(name) == 0) {
	return 1;
    }
    if (errno == EISDIR) {	/* Linux sets the wrong error code. */
	errno = EPERM;
    }
    return check_error(-1, errInfo);
}

/*
 *---------------------------------------------------------------------------
 *
 *      Changes the name of an existing file or directory, from src to dst.
 *	If src and dst refer to the same file or directory, does nothing
 *	and returns success.  Otherwise if dst already exists, it will be
 *	deleted and replaced by src subject to the following conditions:
 *	    If src is a directory, dst may be an empty directory.
 *	    If src is a file, dst may be a file.
 *	In any other situation where dst already exists, the rename will
 *	fail.  
 *
 * Results:
 *	If the directory was successfully created, returns 1.
 *	Otherwise the return value is 0 and errno is set to
 *	indicate the error.  Some possible values for errno are:
 *
 *	EACCES:     src or dst parent directory can't be read and/or written.
 *	EEXIST:	    dst is a non-empty directory.
 *	EINVAL:	    src is a root directory or dst is a subdirectory of src.
 *	EISDIR:	    dst is a directory, but src is not.
 *	ENOENT:	    src doesn't exist, or src or dst is "".
 *	ENOTDIR:    src is a directory, but dst is not.  
 *	EXDEV:	    src and dst are on different filesystems.
 *	
 * Side effects:
 *	The implementation of rename may allow cross-filesystem renames,
 *	but the caller should be prepared to emulate it with copy and
 *	delete if errno is EXDEV.
 *
 *---------------------------------------------------------------------------
 */

int
efile_rename(Efile_error* errInfo,	/* Where to return error codes. */
	     char* src,		        /* Original name. */
	     char* dst)			/* New name. */
{
    if (rename(src, dst) == 0) {
	return 1;
    }
    if (errno == ENOTEMPTY) {
	errno = EEXIST;
    }
#if defined (sparc)
    /*
     * SunOS 4.1.4 reports overwriting a non-empty directory with a
     * directory as EINVAL instead of EEXIST (first rule out the correct
     * EINVAL result code for moving a directory into itself).  Must be
     * conditionally compiled because realpath() is only defined on SunOS.
     */

    if (errno == EINVAL) {
	char srcPath[MAXPATHLEN], dstPath[MAXPATHLEN];
	DIR *dirPtr;
	struct dirent *dirEntPtr;

#ifdef PURIFY
	memset(srcPath, '\0', sizeof(srcPath));
	memset(dstPath, '\0', sizeof(dstPath));
#endif

	if ((realpath(src, srcPath) != NULL)
		&& (realpath(dst, dstPath) != NULL)
		&& (strncmp(srcPath, dstPath, strlen(srcPath)) != 0)) {
	    dirPtr = opendir(dst);
	    if (dirPtr != NULL) {
		while ((dirEntPtr = readdir(dirPtr)) != NULL) {
		    if ((strcmp(dirEntPtr->d_name, ".") != 0) &&
			    (strcmp(dirEntPtr->d_name, "..") != 0)) {
			errno = EEXIST;
			closedir(dirPtr);
			return check_error(-1, errInfo);
		    }
		}
		closedir(dirPtr);
	    }
	}
	errno = EINVAL;
    }
#endif	/* sparc */

    if (strcmp(src, "/") == 0) {
	/*
	 * Alpha reports renaming / as EBUSY and Linux reports it as EACCES,
	 * instead of EINVAL.
	 */
	 
	errno = EINVAL;
    }

    /*
     * DEC Alpha OSF1 V3.0 returns EACCES when attempting to move a
     * file across filesystems and the parent directory of that file is
     * not writable.  Most other systems return EXDEV.  Does nothing to
     * correct this behavior.
     */

    return check_error(-1, errInfo);
}

int
efile_chdir(Efile_error* errInfo,   /* Where to return error codes. */
	    char* name)		    /* Name of directory to make current. */
{
    return check_error(chdir(name), errInfo);
}


int
efile_getdcwd(Efile_error* errInfo,	/* Where to return error codes. */
	      int drive,		/* 0 - current, 1 - A, 2 - B etc. */
	      char* buffer,		/* Where to return the current 
					   directory. */
	      size_t size)		/* Size of buffer. */
{
    if (drive == 0) {
	if (getcwd(buffer, size) == NULL)
	    return check_error(-1, errInfo);
	
#ifdef SIMSPARCSOLARIS
	/* We get "host:" prepended to the dirname - remove!. */
	{
	  int i = 0;
	  int j = 0;
	  while ((buffer[i] != ':') && (buffer[i] != '\0')) i++;
	  if (buffer[i] == ':') {
	    i++;
	    while ((buffer[j++] = buffer[i++]) != '\0');
	  }
	}
#endif	      
	return 1;
    }

    /*
     * Drives other than 0 is not supported on Unix.
     */

    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

int
efile_readdir(Efile_error* errInfo,	/* Where to return error codes. */
	      char* name,		/* Name of directory to open. */
	      EFILE_DIR_HANDLE* p_dir_handle,	/* Pointer to directory 
						   handle of
						   open directory.*/
	      char* buffer,		/* Pointer to buffer for 
					   one filename. */
	      size_t *size)		/* in-out Size of buffer, length
					   of name. */
{
    DIR *dp;			/* Pointer to directory structure. */
    struct dirent* dirp;	/* Pointer to directory entry. */

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

int
efile_openfile(Efile_error* errInfo,	/* Where to return error codes. */
	       char* name,		/* Name of directory to open. */
	       int flags,		/* Flags to user for opening. */
	       int* pfd,		/* Where to store the file 
					   descriptor. */
	       Sint64 *pSize)		/* Where to store the size of the 
					   file. */
{
    struct stat statbuf;
    int fd;
    int mode;			/* Open mode. */

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
    if (flags & EFILE_MODE_SYNC) {
#ifdef O_SYNC
	mode |= O_SYNC;
#else
	errno = ENOTSUP;
	return check_error(-1, errInfo);
#endif
    }

#ifdef HAVE_FSTAT
    while (((fd = open(name, mode, FILE_MODE)) < 0) && (errno == EINTR));
    if (!check_error(fd, errInfo)) return 0;
#endif

    if (
#ifdef HAVE_FSTAT
        fstat(fd, &statbuf) < 0
#else
        stat(name, &statbuf) < 0
#endif
        ) {
        /* statbuf is undefined: if the caller depends on it,
           i.e. invoke_read_file(), fail the call immediately */
        if (pSize && flags == EFILE_MODE_READ) {
            check_error(-1, errInfo);
#ifdef HAVE_FSTAT
            efile_closefile(fd);
#endif
            return 0;
        }
    }
    else if (! ISREG(statbuf)) {
        struct stat nullstatbuf;
	/*
	 * For UNIX only, here is some ugly code to allow
	 * /dev/null to be opened as a file.
	 */
        if ( (stat("/dev/null", &nullstatbuf) < 0)
             || (statbuf.st_ino != nullstatbuf.st_ino)
             || (statbuf.st_dev != nullstatbuf.st_dev) ) {
#ifdef HAVE_FSTAT
            efile_closefile(fd);
#endif
	    errno = EISDIR;
	    return check_error(-1, errInfo);
        }
    }

#ifndef HAVE_FSTAT
    while (((fd = open(name, mode, FILE_MODE)) < 0) && (errno == EINTR));
    if (!check_error(fd, errInfo)) return 0;
#endif

    *pfd = fd;
    if (pSize) *pSize = statbuf.st_size;
    return 1;
}

int 
efile_may_openfile(Efile_error* errInfo, char *name) {
    struct stat statbuf;	/* Information about the file */
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

void
efile_closefile(int fd)
{
    while((close(fd) < 0) && (errno == EINTR));
}

int
efile_fdatasync(Efile_error *errInfo, /* Where to return error codes. */
	    int fd)               /* File descriptor for file to sync data. */
{
#if defined(HAVE_FDATASYNC) && !defined(__DARWIN__)
    return check_error(fdatasync(fd), errInfo);
#else
    return efile_fsync(errInfo, fd);
#endif
}

int
efile_fsync(Efile_error *errInfo, /* Where to return error codes. */
	    int fd)               /* File descriptor for file to sync. */
{
#ifdef NO_FSYNC
  undefined fsync /* XXX: Really? */
#else
#if defined(__DARWIN__) && defined(F_FULLFSYNC)
    return check_error(fcntl(fd, F_FULLFSYNC), errInfo);
#else
    return check_error(fsync(fd), errInfo);
#endif /* __DARWIN__ */
#endif /* NO_FSYNC */
}

int
efile_fileinfo(Efile_error* errInfo, Efile_info* pInfo, 
	       char* name, int info_for_link)
{
    struct stat statbuf;	/* Information about the file */
    int result;

    if (info_for_link) {
	result = lstat(name, &statbuf);
    } else {
	result = stat(name, &statbuf);
    }	
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

    pInfo->accessTime   = (Sint64)statbuf.st_atime;
    pInfo->modifyTime   = (Sint64)statbuf.st_mtime;
    pInfo->cTime        = (Sint64)statbuf.st_ctime;

    pInfo->mode         = statbuf.st_mode;
    pInfo->links        = statbuf.st_nlink;
    pInfo->major_device = statbuf.st_dev;
    pInfo->minor_device = statbuf.st_rdev;
    pInfo->inode        = statbuf.st_ino;
    pInfo->uid          = statbuf.st_uid;
    pInfo->gid          = statbuf.st_gid;

    return 1;
}

int
efile_write_info(Efile_error *errInfo, Efile_info *pInfo, char *name)
{
    struct utimbuf tval;

    /*
     * On some systems chown will always fail for a non-root user unless
     * POSIX_CHOWN_RESTRICTED is not set.  Others will succeed as long as 
     * you don't try to chown a file to someone besides youself.
     */

    if (chown(name, pInfo->uid, pInfo->gid) && errno != EPERM) {
	return check_error(-1, errInfo);
    }

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

    tval.actime  = (time_t)pInfo->accessTime;
    tval.modtime = (time_t)pInfo->modifyTime;

    return check_error(utime(name, &tval), errInfo);
}


int
efile_write(Efile_error* errInfo,	/* Where to return error codes. */
	    int flags,			/* Flags given when file was 
					   opened. */
	    int fd,			/* File descriptor to write to. */
	    char* buf,			/* Buffer to write. */
	    size_t count)		/* Number of bytes to write. */
{
    ssize_t written;			/* Bytes written in last operation. */

    while (count > 0) {
	if ((written = write(fd, buf, count)) < 0) {
	    if (errno != EINTR)
		return check_error(-1, errInfo);
	    else
		written = 0;
	}
	ASSERT(written <= count);
	buf += written;
	count -= written;
    }
    return 1;
}

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
    int cnt = 0;                     /* Buffers so far written */

    ASSERT(iovcnt >= 0);
    
    while (cnt < iovcnt) {
	if ((! iov[cnt].iov_base) || (iov[cnt].iov_len <= 0)) {
	    /* Empty buffer - skip */
	    cnt++;
	} else { /* Non-empty buffer */
	    ssize_t w;                   /* Bytes written in this call */
#ifdef HAVE_WRITEV
	    int b = iovcnt - cnt;        /* Buffers to write */
	    /* Use as many buffers as MAXIOV allows */
	    if (b > MAXIOV)
		b = MAXIOV;
	    if (b > 1) {
		do {
		    w = writev(fd, &iov[cnt], b);
		} while (w < 0 && errno == EINTR);
		if (w < 0 && errno == EINVAL) {
		    goto single_write;
		}
	    } else
	    single_write:
		/* Degenerated io vector - use regular write */
#endif
		{
		    do {
			size_t iov_len = iov[cnt].iov_len;
			size_t limit = 1024*1024*1024; /* 1GB */
			if (iov_len > limit) {
			    iov_len = limit;
			}
			w = write(fd, iov[cnt].iov_base, iov_len);
		    } while (w < 0 && errno == EINTR);
		    ASSERT(w <= iov[cnt].iov_len ||
			   (w == -1 && errno != EINTR));
		}
	    if (w < 0) return check_error(-1, errInfo);
	    /* Move forward to next buffer to write */
	    for (; cnt < iovcnt && w > 0; cnt++) {
		if (iov[cnt].iov_base && iov[cnt].iov_len > 0) {
		    if (w < iov[cnt].iov_len) {
			/* Adjust the buffer for next write */
			iov[cnt].iov_len -= w;
			iov[cnt].iov_base = ((char *)iov[cnt].iov_base) + w;
			w = 0;
			break;
		    } else {
			w -= iov[cnt].iov_len;
		    }
		}
	    }
	    ASSERT(w == 0);
	} /* else Non-empty buffer */
    } /* while (cnt< iovcnt) */
    return 1;
}

int
efile_read(Efile_error* errInfo,     /* Where to return error codes. */
	   int flags,		     /* Flags given when file was opened. */
	   int fd,		     /* File descriptor to read from. */
	   char* buf,		     /* Buffer to read into. */
	   size_t count,	     /* Number of bytes to read. */
	   size_t *pBytesRead)	     /* Where to return number of 
					bytes read. */
{
    ssize_t n;

    for (;;) {
	if ((n = read(fd, buf, count)) >= 0)
	    break;
	else if (errno != EINTR)
	    return check_error(-1, errInfo);
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
	    int fd,		      /* File descriptor to read from. */
	    Sint64 offset,            /* Offset in bytes from BOF. */
	    char* buf,		      /* Buffer to read into. */
	    size_t count,	      /* Number of bytes to read. */
	    size_t *pBytesRead)	      /* Where to return 
					 number of bytes read. */
{
#if defined(HAVE_PREAD) && defined(HAVE_PWRITE)
    ssize_t n;
    off_t off = (off_t) offset;
    if (off != offset) {
	errno = EINVAL;
	return check_error(-1, errInfo);
    }
    for (;;) {
	if ((n = pread(fd, buf, count, offset)) >= 0)
	    break;
	else if (errno != EINTR)
	    return check_error(-1, errInfo);
    }
    *pBytesRead = (size_t) n;
    return 1;
#else
    {
	int res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
	if (res) {
	    return efile_read(errInfo, 0, fd, buf, count, pBytesRead);
	} else {
	    return res;
	}
    }
#endif
}



int
efile_pwrite(Efile_error* errInfo,  /* Where to return error codes. */
	     int fd,		    /* File descriptor to write to. */
	     char* buf,		    /* Buffer to write. */
	     size_t count,	    /* Number of bytes to write. */
	     Sint64 offset)	    /* where to write it */
{ 
#if defined(HAVE_PREAD) && defined(HAVE_PWRITE)
    ssize_t written;		    /* Bytes written in last operation. */
    off_t off = (off_t) offset;
    if (off != offset) {
	errno = EINVAL;
	return check_error(-1, errInfo);
    }
    
    while (count > 0) {
	if ((written = pwrite(fd, buf, count, offset)) < 0) {
	    if (errno != EINTR)
		return check_error(-1, errInfo);
	    else
		written = 0;
	}
	ASSERT(written <= count);
	buf += written;
	count -= written;
	offset += written;
    }
    return 1;
#else  /* For unix systems that don't support pread() and pwrite() */    
    {
	int res = efile_seek(errInfo, fd, offset, EFILE_SEEK_SET, NULL);
	
	if (res) {
	    return efile_write(errInfo, 0, fd, buf, count);
	} else {
	    return res;
	}
    }
#endif
}


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

    switch (origin) {
    case EFILE_SEEK_SET: origin = SEEK_SET; break;
    case EFILE_SEEK_CUR: origin = SEEK_CUR; break;
    case EFILE_SEEK_END: origin = SEEK_END; break;
    default:
	errno = EINVAL;
	return check_error(-1, errInfo);
    }
    off = (off_t) offset;
    if (off != offset) {
	errno = EINVAL;
	return check_error(-1, errInfo);
    }
    
    errno = 0;
    result = lseek(fd, off, origin);

    /*
     * Note that the man page for lseek (on SunOs 5) says:
     * 
     * "if fildes is a remote file  descriptor  and  offset  is
     * negative,  lseek()  returns  the  file pointer even if it is
     * negative."
     */

    if (result < 0 && errno == 0)
	errno = EINVAL;
    if (result < 0)
	return check_error(-1, errInfo);
    if (new_location) {
	*new_location = result;
    }
    return 1;
}


int
efile_truncate_file(Efile_error* errInfo, int *fd, int flags)
{
#ifndef NO_FTRUNCATE
    off_t offset;

    return check_error((offset = lseek(*fd, 0, 1)) >= 0 &&
		       ftruncate(*fd, offset) == 0 ? 1 : -1,
		       errInfo);
#else
    return 1;
#endif
}

int
efile_readlink(Efile_error* errInfo, char* name, char* buffer, size_t size)
{
    int len;
    ASSERT(size > 0);
    len = readlink(name, buffer, size-1);
    if (len == -1) {
	return check_error(-1, errInfo);
    }
    buffer[len] = '\0';
    return 1;
}

int
efile_altname(Efile_error* errInfo, char* name, char* buffer, size_t size)
{
    errno = ENOTSUP;
    return check_error(-1, errInfo);
}

int
efile_link(Efile_error* errInfo, char* old, char* new)
{
    return check_error(link(old, new), errInfo);
}

int
efile_symlink(Efile_error* errInfo, char* old, char* new)
{
    return check_error(symlink(old, new), errInfo);
}

int
efile_fadvise(Efile_error* errInfo, int fd, Sint64 offset,
		  Sint64 length, int advise)
{
#ifdef HAVE_POSIX_FADVISE
    return check_error(posix_fadvise(fd, offset, length, advise), errInfo);
#else
    return check_error(0, errInfo);
#endif
}

#ifdef HAVE_SENDFILE
/* For some reason the maximum size_t cannot be used as the max size
   3GB seems to work on all platforms */
#define SENDFILE_CHUNK_SIZE ((1UL << 30) -1)

/*
 * sendfile: The implementation of the sendfile system call varies
 * a lot on different *nix platforms so to make the api similar in all
 * we have to emulate some things in linux and play with variables on
 * bsd/darwin.
 *
 * All of the calls will split a command which tries to send more than
 * SENDFILE_CHUNK_SIZE of data at once.
 *
 * On platforms where *nbytes of 0 does not mean the entire file, this is
 * simulated.
 *
 * It could be possible to implement header/trailer in sendfile. Though
 * you would have to emulate it in linux and on BSD/Darwin some complex
 * calculations have to be made when using a non blocking socket to figure
 * out how much of the header/file/trailer was sent in each command.
 *
 * The semantics of the API is this:
 * Return value: 1 if all data was sent and the function does not need to
 * be called again. 0 if an error occures OR if there is more data which
 * has to be sent (EAGAIN or EINTR will be set appropriately)
 *
 * The amount of data written in a call is returned through nbytes.
 *
 */

int
efile_sendfile(Efile_error* errInfo, int in_fd, int out_fd,
	       off_t *offset, Uint64 *nbytes, struct t_sendfile_hdtl* hdtl)
{
    Uint64 written = 0;
#if defined(__linux__)
    ssize_t retval;
    do {
      /* check if *nbytes is 0 or greater than chunk size */
      if (*nbytes == 0 || *nbytes > SENDFILE_CHUNK_SIZE)
	retval = sendfile(out_fd, in_fd, offset, SENDFILE_CHUNK_SIZE);
      else
	retval = sendfile(out_fd, in_fd, offset, *nbytes);
      if (retval > 0) {
	written += retval;
	*nbytes -= retval;
      }
    } while (retval == SENDFILE_CHUNK_SIZE);
    if (written != 0) {
      /* -1 is not returned by the linux API so we have to simulate it */
      retval = -1;
      errno = EAGAIN;
    }
#elif defined(__sun) && defined(__SVR4) && defined(HAVE_SENDFILEV)
    ssize_t retval;
    size_t len;
    sendfilevec_t fdrec;
    fdrec.sfv_fd = in_fd;
    fdrec.sfv_flag = 0;
    do {
      fdrec.sfv_off = *offset;
      len = 0;
      /* check if *nbytes is 0 or greater than chunk size */
      if (*nbytes == 0 || *nbytes > SENDFILE_CHUNK_SIZE)
	fdrec.sfv_len = SENDFILE_CHUNK_SIZE;
      else
	fdrec.sfv_len = *nbytes;
      retval = sendfilev(out_fd, &fdrec, 1, &len);

      /* Sometimes sendfilev can return -1 and still send data. 
         When that happens we just pretend that no error happend. */
      if (retval != -1 || errno == EAGAIN || errno == EINTR ||
	  len != 0) {
        *offset += len;
	*nbytes -= len;
	written += len;
	if (errno != EAGAIN && errno != EINTR && len != 0)
	  retval = len;
      }
    } while (len == SENDFILE_CHUNK_SIZE);
#elif defined(__DARWIN__)
    int retval;
    off_t len;
    do {
      /* check if *nbytes is 0 or greater than chunk size */
      if(*nbytes > SENDFILE_CHUNK_SIZE)
	len = SENDFILE_CHUNK_SIZE;
      else
	len = *nbytes;
      retval = sendfile(in_fd, out_fd, *offset, &len, NULL, 0);
      if (retval != -1 || errno == EAGAIN || errno == EINTR) {
        *offset += len;
	*nbytes -= len;
	written += len;
      }
    } while (len == SENDFILE_CHUNK_SIZE);
#elif defined(__FreeBSD__) || defined(__DragonFly__)
    off_t len;
    int retval;
    do {
      if (*nbytes > SENDFILE_CHUNK_SIZE)
	retval = sendfile(in_fd, out_fd, *offset, SENDFILE_CHUNK_SIZE,
			  NULL, &len, 0);
      else
	retval = sendfile(in_fd, out_fd, *offset, *nbytes, NULL, &len, 0);
      if (retval != -1 || errno == EAGAIN || errno == EINTR) {
	*offset += len;
	*nbytes -= len;
	written += len;
      }
    } while(len == SENDFILE_CHUNK_SIZE);
#endif
    *nbytes = written;
    return check_error(retval, errInfo);
}
#endif /* HAVE_SENDFILE */

#ifdef HAVE_POSIX_FALLOCATE
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
#endif /* HAVE_POSIX_FALLOCATE */

int
efile_fallocate(Efile_error* errInfo, int fd, Sint64 offset, Sint64 length)
{
#if defined HAVE_FALLOCATE
    /* Linux specific, more efficient than posix_fallocate. */
    int ret;

    do {
        ret = fallocate(fd, FALLOC_FL_KEEP_SIZE, (off_t) offset, (off_t) length);
    } while (ret != 0 && errno == EINTR);

#if defined HAVE_POSIX_FALLOCATE
    /* Fallback to posix_fallocate if available. */
    if (ret != 0) {
        ret = call_posix_fallocate(fd, offset, length);
    }
#endif

    return check_error(ret, errInfo);
#elif defined F_PREALLOCATE
    /* Mac OS X specific, equivalent to posix_fallocate. */
    int ret;
    fstore_t fs;

    memset(&fs, 0, sizeof(fs));
    fs.fst_flags = F_ALLOCATECONTIG;
    fs.fst_posmode = F_VOLPOSMODE;
    fs.fst_offset = (off_t) offset;
    fs.fst_length = (off_t) length;

    ret = fcntl(fd, F_PREALLOCATE, &fs);

    if (-1 == ret) {
        fs.fst_flags = F_ALLOCATEALL;
        ret = fcntl(fd, F_PREALLOCATE, &fs);

#if defined HAVE_POSIX_FALLOCATE
        /* Fallback to posix_fallocate if available. */
        if (-1 == ret) {
            ret = call_posix_fallocate(fd, offset, length);
        }
#endif
    }

    return check_error(ret, errInfo);
#elif defined HAVE_POSIX_FALLOCATE
    /* Other Unixes, use posix_fallocate if available. */
    return check_error(call_posix_fallocate(fd, offset, length), errInfo);
#else
    errno = ENOTSUP;
    return check_error(-1, errInfo);
#endif
}
