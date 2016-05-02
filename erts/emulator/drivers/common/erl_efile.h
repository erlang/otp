/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 * Defines the interfaces between the generic efile driver and its
 * operating-system dependent helpers.
 */

#include "sys.h"
#include "erl_driver.h"

/*
 * Open modes for efile_openfile().
 */
#define EFILE_MODE_READ 1
#define EFILE_MODE_WRITE 2	/* Implies truncating file when used alone. */
#define EFILE_MODE_READ_WRITE 	3
#define EFILE_MODE_APPEND	4
#define EFILE_COMPRESSED 	8
#define EFILE_MODE_EXCL        16
#define EFILE_NO_TRUNCATE      32 /* Special for reopening on VxWorks */
#define EFILE_MODE_SYNC        64

/*
 * Seek modes for efile_seek().
 */
#define	EFILE_SEEK_SET	0
#define	EFILE_SEEK_CUR	1
#define	EFILE_SEEK_END	2

/*
 * File types returned by efile_fileinfo().
 */
#define FT_DEVICE 	1
#define FT_DIRECTORY 	2
#define FT_REGULAR	3
#define FT_SYMLINK	4
#define FT_OTHER	5

/*
 * Access attributes returned by efile_fileinfo() (the bits can be ORed
 * together).
 */
#define FA_NONE    	0
#define FA_WRITE 	1
#define FA_READ		2

/* Some OS'es (i.e. Windows) has filenames in wide charaqcters. That requires special handling */
/* Note that we do *not* honor alignment in the communication to the OS specific driver, */
/* which is not a problem on x86, but might be on other platforms. The OS specific efile */
/* implementation is expected to align if needed */
#ifdef __WIN32__
#define FILENAMES_16BIT 1
#endif

/* We use sendfilev if it exist on solaris */
#if !defined(HAVE_SENDFILE) && defined(HAVE_SENDFILEV)
#define HAVE_SENDFILE
#endif

/*
 * An handle to an open directory.  To be cast to the correct type
 * in the system-dependent directory functions.
 */

typedef struct _Efile_Dir_Handle* EFILE_DIR_HANDLE;

/*
 * Error information from the last call.
 */  
typedef struct _Efile_error {
    int posix_errno;		/* Posix error number, as in <errno.h>. */
    int os_errno;		/* Os-dependent error number (not used). */
} Efile_error;

/*
 * Describes what is returned by file:file_info/1.
 */

typedef struct _Efile_info {
    Uint32 size_low;		/* Size of file, lower 32 bits.. */
    Uint32 size_high;		/* Size of file, higher 32 bits. */
    Uint32 type;		/* Type of file -- one of FT_*. */
    Uint32 access;		/* Access to file -- one of FA_*. */
    Uint32 mode;		/* Access permissions -- bit field. */
    Uint32 links;		/* Number of links to file. */
    Uint32 major_device;	/* Major device or file system. */
    Uint32 minor_device;	/* Minor device (for devices). */
    Uint32 inode;		/* Inode number. */
    Uint32 uid;			/* User id of owner. */
    Uint32 gid;			/* Group id of owner. */
    Sint64 accessTime;		/* Last time the file was accessed. */
    Sint64 modifyTime;		/* Last time the file was modified. */
    Sint64 cTime;		/* Creation time (Windows) or last
				 * inode change (Unix).
				 */
} Efile_info;


#ifdef HAVE_SENDFILE
/*
 * Describes the structure of headers/trailers for sendfile
 */
struct t_sendfile_hdtl {
    SysIOVec *headers;
    int hdr_cnt;
    SysIOVec *trailers;
    int trl_cnt;
};
#endif /* HAVE_SENDFILE */

/*
 * Functions.
 */
int efile_init(void);
int efile_mkdir(Efile_error* errInfo, char* name);
int efile_rmdir(Efile_error* errInfo, char* name);
int efile_delete_file(Efile_error* errInfo, char* name);
int efile_rename(Efile_error* errInfo, char* src, char* dst);
int efile_chdir(Efile_error* errInfo, char* name);
int efile_getdcwd(Efile_error* errInfo, int drive,
		  char* buffer, size_t size);
int efile_readdir(Efile_error* errInfo, char* name, 
		  EFILE_DIR_HANDLE* dir_handle,
		  char* buffer, size_t *size);
int efile_openfile(Efile_error* errInfo, char* name, int flags,
		   int* pfd, Sint64* pSize);
void efile_closefile(int fd);
int efile_fdatasync(Efile_error* errInfo, int fd);
int efile_fsync(Efile_error* errInfo, int fd);
int efile_fileinfo(Efile_error* errInfo, Efile_info* pInfo,
		   char *name, int info_for_link);
int efile_write_info(Efile_error* errInfo, Efile_info* pInfo, char *name);
int efile_write(Efile_error* errInfo, int flags, int fd, 
		char* buf, size_t count);
int efile_writev(Efile_error* errInfo, int flags, int fd, 
		 SysIOVec* iov, int iovcnt);
int efile_read(Efile_error* errInfo, int flags, int fd, 
	       char* buf, size_t count, size_t* pBytesRead);
int efile_seek(Efile_error* errInfo, int fd, 
	       Sint64 offset, int origin, Sint64* new_location);
int efile_truncate_file(Efile_error* errInfo, int *fd, int flags);
int efile_pwrite(Efile_error* errInfo, int fd, 
		 char* buf, size_t count, Sint64 offset);
int efile_pread(Efile_error* errInfo, int fd, 
		Sint64 offset, char* buf, size_t count, size_t* pBytesRead);
int efile_readlink(Efile_error* errInfo, char *name, 
		   char* buffer, size_t size);
int efile_altname(Efile_error* errInfo, char *name, 
		  char* buffer, size_t size);
int efile_link(Efile_error* errInfo, char* old, char* new);
int efile_symlink(Efile_error* errInfo, char* old, char* new);
int efile_may_openfile(Efile_error* errInfo, char *name);
int efile_fadvise(Efile_error* errInfo, int fd, Sint64 offset, Sint64 length,
		  int advise);
#ifdef HAVE_SENDFILE
int efile_sendfile(Efile_error* errInfo, int in_fd, int out_fd,
		      off_t *offset, Uint64 *nbytes, struct t_sendfile_hdtl *hdtl);
#endif /* HAVE_SENDFILE */
int efile_fallocate(Efile_error* errInfo, int fd, Sint64 offset, Sint64 length);
