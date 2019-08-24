/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
 * _dosmaperr: maps Windows OS errors to Unix System V errno values
 *
 * Contributor: Michael Regen
 */   

/* Only use for win32 if linking to MSVCR??.DLL and not if statically linking
   to LIBCMT.LIB */
#if defined(WIN32) && defined(_MT) && defined(_DLL)

#include <errno.h>
#include <winerror.h>
#include <stdlib.h>

/* Position in table = Windows OS error -> Posix errno
** An exception for ERROR_NOT_ENOUGH_QUOTA - 1816 is in _dosmaperr
*/
static const unsigned char errMapTable[] = {
    EINVAL,      /* ERROR_SUCCESS                      0  */
    EINVAL,      /* ERROR_INVALID_FUNCTION             1  */
    ENOENT,      /* ERROR_FILE_NOT_FOUND               2  */
    ENOENT,      /* ERROR_PATH_NOT_FOUND               3  */
    EMFILE,      /* ERROR_TOO_MANY_OPEN_FILES          4  */
    EACCES,      /* ERROR_ACCESS_DENIED                5  */
    EBADF,       /* ERROR_INVALID_HANDLE               6  */
    ENOMEM,      /* ERROR_ARENA_TRASHED                7  */
    ENOMEM,      /* ERROR_NOT_ENOUGH_MEMORY            8  */
    ENOMEM,      /* ERROR_INVALID_BLOCK                9  */
    E2BIG,       /* ERROR_BAD_ENVIRONMENT             10  */
    ENOEXEC,     /* ERROR_BAD_FORMAT                  11  */
    EINVAL,      /* ERROR_INVALID_ACCESS              12  */
    EINVAL,      /* ERROR_INVALID_DATA                13  */
    EINVAL,      /* ERROR_OUTOFMEMORY                 14  */
    ENOENT,      /* ERROR_INVALID_DRIVE               15  */
    EACCES,      /* ERROR_CURRENT_DIRECTORY           16  */
    EXDEV,       /* ERROR_NOT_SAME_DEVICE             17  */
    ENOENT,      /* ERROR_NO_MORE_FILES               18  */
    EACCES,      /* ERROR_WRITE_PROTECT               19  */
    EACCES,      /* ERROR_BAD_UNIT                    20  */
    EACCES,      /* ERROR_NOT_READY                   21  */
    EACCES,      /* ERROR_BAD_COMMAND                 22  */
    EACCES,      /* ERROR_CRC                         23  */
    EACCES,      /* ERROR_BAD_LENGTH                  24  */
    EACCES,      /* ERROR_SEEK                        25  */
    EACCES,      /* ERROR_NOT_DOS_DISK                26  */
    EACCES,      /* ERROR_SECTOR_NOT_FOUND            27  */
    EACCES,      /* ERROR_OUT_OF_PAPER                28  */
    EACCES,      /* ERROR_WRITE_FAULT                 29  */
    EACCES,      /* ERROR_READ_FAULT                  30  */
    EACCES,      /* ERROR_GEN_FAILURE                 31  */
    EACCES,      /* ERROR_SHARING_VIOLATION           32  */
    EACCES,      /* ERROR_LOCK_VIOLATION              33  */
    EACCES,      /* ERROR_WRONG_DISK                  34  */
    EACCES,      /*                                   35  */
    EACCES,      /* ERROR_SHARING_BUFFER_EXCEEDED     36  */
    EINVAL,      /*                                   37  */
    EINVAL,      /* ERROR_HANDLE_EOF                  38  */
    EINVAL,      /* ERROR_HANDLE_DISK_FULL            39  */
    EINVAL,      /*                                   40  */
    EINVAL,      /*                                   41  */
    EINVAL,      /*                                   42  */
    EINVAL,      /*                                   43  */
    EINVAL,      /*                                   44  */
    EINVAL,      /*                                   45  */
    EINVAL,      /*                                   46  */
    EINVAL,      /*                                   47  */
    EINVAL,      /*                                   48  */
    EINVAL,      /*                                   49  */
    EINVAL,      /* ERROR_NOT_SUPPORTED               50  */
    EINVAL,      /* ERROR_REM_NOT_LIST                51  */
    EINVAL,      /* ERROR_DUP_NAME                    52  */
    ENOENT,      /* ERROR_BAD_NETPATH                 53  */
    EINVAL,      /* ERROR_NETWORK_BUSY                54  */
    EINVAL,      /* ERROR_DEV_NOT_EXIST               55  */
    EINVAL,      /* ERROR_TOO_MANY_CMDS               56  */
    EINVAL,      /* ERROR_ADAP_HDW_ERR                57  */
    EINVAL,      /* ERROR_BAD_NET_RESP                58  */
    EINVAL,      /* ERROR_UNEXP_NET_ERR               59  */
    EINVAL,      /* ERROR_BAD_REM_ADAP                60  */
    EINVAL,      /* ERROR_PRINTQ_FULL                 61  */
    EINVAL,      /* ERROR_NO_SPOOL_SPACE              62  */
    EINVAL,      /* ERROR_PRINT_CANCELLED             63  */
    EINVAL,      /* ERROR_NETNAME_DELETED             64  */
    EACCES,      /* ERROR_NETWORK_ACCESS_DENIED       65  */
    EINVAL,      /* ERROR_BAD_DEV_TYPE                66  */
    ENOENT,      /* ERROR_BAD_NET_NAME                67  */
    EINVAL,      /* ERROR_TOO_MANY_NAMES              68  */
    EINVAL,      /* ERROR_TOO_MANY_SESS               69  */
    EINVAL,      /* ERROR_SHARING_PAUSED              70  */
    EINVAL,      /* ERROR_REQ_NOT_ACCEP               71  */
    EINVAL,      /* ERROR_REDIR_PAUSED                72  */
    EINVAL,      /*                                   73  */
    EINVAL,      /*                                   74  */
    EINVAL,      /*                                   75  */
    EINVAL,      /*                                   76  */
    EINVAL,      /*                                   77  */
    EINVAL,      /*                                   78  */
    EINVAL,      /*                                   79  */
    EEXIST,      /* ERROR_FILE_EXISTS                 80  */
    EINVAL,      /*                                   81  */
    EACCES,      /* ERROR_CANNOT_MAKE                 82  */
    EACCES,      /* ERROR_FAIL_I24                    83  */
    EINVAL,      /* ERROR_OUT_OF_STRUCTURES           84  */
    EINVAL,      /* ERROR_ALREADY_ASSIGNED            85  */
    EINVAL,      /* ERROR_INVALID_PASSWORD            86  */
    EINVAL,      /* ERROR_INVALID_PARAMETER           87  */
    EINVAL,      /* ERROR_NET_WRITE_FAULT             88  */
    EAGAIN,      /* ERROR_NO_PROC_SLOTS               89  */
    EINVAL,      /*                                   90  */
    EINVAL,      /*                                   91  */
    EINVAL,      /*                                   92  */
    EINVAL,      /*                                   93  */
    EINVAL,      /*                                   94  */
    EINVAL,      /*                                   95  */
    EINVAL,      /*                                   96  */
    EINVAL,      /*                                   97  */
    EINVAL,      /*                                   98  */
    EINVAL,      /*                                   99  */
    EINVAL,      /* ERROR_TOO_MANY_SEMAPHORES        100  */
    EINVAL,      /* ERROR_EXCL_SEM_ALREADY_OWNED     101  */
    EINVAL,      /* ERROR_SEM_IS_SET                 102  */
    EINVAL,      /* ERROR_TOO_MANY_SEM_REQUESTS      103  */
    EINVAL,      /* ERROR_INVALID_AT_INTERRUPT_TIME  104  */
    EINVAL,      /* ERROR_SEM_OWNER_DIED             105  */
    EINVAL,      /* ERROR_SEM_USER_LIMIT             106  */
    EINVAL,      /* ERROR_DISK_CHANGE                107  */
    EACCES,      /* ERROR_DRIVE_LOCKED               108  */
    EPIPE,       /* ERROR_BROKEN_PIPE                109  */
    EINVAL,      /* ERROR_OPEN_FAILED                110  */
    EINVAL,      /* ERROR_BUFFER_OVERFLOW            111  */
    ENOSPC,      /* ERROR_DISK_FULL                  112  */
    EINVAL,      /* ERROR_NO_MORE_SEARCH_HANDLES     113  */
    EBADF,       /* ERROR_INVALID_TARGET_HANDLE      114  */
    EINVAL,      /*                                  115  */
    EINVAL,      /*                                  116  */
    EINVAL,      /* ERROR_INVALID_CATEGORY           117  */
    EINVAL,      /* ERROR_INVALID_VERIFY_SWITCH      118  */
    EINVAL,      /* ERROR_BAD_DRIVER_LEVEL           119  */
    EINVAL,      /* ERROR_CALL_NOT_IMPLEMENTED       120  */
    EINVAL,      /* ERROR_SEM_TIMEOUT                121  */
    EINVAL,      /* ERROR_INSUFFICIENT_BUFFER        122  */
    EINVAL,      /* ERROR_INVALID_NAME               123  */
    EINVAL,      /* ERROR_INVALID_LEVEL              124  */
    EINVAL,      /* ERROR_NO_VOLUME_LABEL            125  */
    EINVAL,      /* ERROR_MOD_NOT_FOUND              126  */
    EINVAL,      /* ERROR_PROC_NOT_FOUND             127  */
    ECHILD,      /* ERROR_WAIT_NO_CHILDREN           128  */
    ECHILD,      /* ERROR_CHILD_NOT_COMPLETE         129  */
    EBADF,       /* ERROR_DIRECT_ACCESS_HANDLE       130  */
    EINVAL,      /* ERROR_NEGATIVE_SEEK              131  */
    EACCES,      /* ERROR_SEEK_ON_DEVICE             132  */
    EINVAL,      /* ERROR_IS_JOIN_TARGET             133  */
    EINVAL,      /* ERROR_IS_JOINED                  134  */
    EINVAL,      /* ERROR_IS_SUBSTED                 135  */
    EINVAL,      /* ERROR_NOT_JOINED                 136  */
    EINVAL,      /* ERROR_NOT_SUBSTED                137  */
    EINVAL,      /* ERROR_JOIN_TO_JOIN               138  */
    EINVAL,      /* ERROR_SUBST_TO_SUBST             139  */
    EINVAL,      /* ERROR_JOIN_TO_SUBST              140  */
    EINVAL,      /* ERROR_SUBST_TO_JOIN              141  */
    EINVAL,      /* ERROR_BUSY_DRIVE                 142  */
    EINVAL,      /* ERROR_SAME_DRIVE                 143  */
    EINVAL,      /* ERROR_DIR_NOT_ROOT               144  */
    ENOTEMPTY,   /* ERROR_DIR_NOT_EMPTY              145  */
    EINVAL,      /* ERROR_IS_SUBST_PATH              146  */
    EINVAL,      /* ERROR_IS_JOIN_PATH               147  */
    EINVAL,      /* ERROR_PATH_BUSY                  148  */
    EINVAL,      /* ERROR_IS_SUBST_TARGET            149  */
    EINVAL,      /* ERROR_SYSTEM_TRACE               150  */
    EINVAL,      /* ERROR_INVALID_EVENT_COUNT        151  */
    EINVAL,      /* ERROR_TOO_MANY_MUXWAITERS        152  */
    EINVAL,      /* ERROR_INVALID_LIST_FORMAT        153  */
    EINVAL,      /* ERROR_LABEL_TOO_LONG             154  */
    EINVAL,      /* ERROR_TOO_MANY_TCBS              155  */
    EINVAL,      /* ERROR_SIGNAL_REFUSED             156  */
    EINVAL,      /* ERROR_DISCARDED                  157  */
    EACCES,      /* ERROR_NOT_LOCKED                 158  */
    EINVAL,      /* ERROR_BAD_THREADID_ADDR          159  */
    EINVAL,      /* ERROR_BAD_ARGUMENTS              160  */
    ENOENT,      /* ERROR_BAD_PATHNAME               161  */
    EINVAL,      /* ERROR_SIGNAL_PENDING             162  */
    EINVAL,      /*                                  163  */
    EAGAIN,      /* ERROR_MAX_THRDS_REACHED          164  */
    EINVAL,      /*                                  165  */
    EINVAL,      /*                                  166  */
    EACCES,      /* ERROR_LOCK_FAILED                167  */
    EINVAL,      /*                                  168  */
    EINVAL,      /*                                  169  */
    EINVAL,      /* ERROR_BUSY                       170  */
    EINVAL,      /*                                  171  */
    EINVAL,      /*                                  172  */
    EINVAL,      /* ERROR_CANCEL_VIOLATION           173  */
    EINVAL,      /* ERROR_ATOMIC_LOCKS_NOT_SUPPORTED 174  */
    EINVAL,      /*                                  175  */
    EINVAL,      /*                                  176  */
    EINVAL,      /*                                  177  */
    EINVAL,      /*                                  178  */
    EINVAL,      /*                                  179  */
    EINVAL,      /* ERROR_INVALID_SEGMENT_NUMBER     180  */
    EINVAL,      /*                                  181  */
    EINVAL,      /* ERROR_INVALID_ORDINAL            182  */
    EEXIST,      /* ERROR_ALREADY_EXISTS             183  */
    EINVAL,      /*                                  184  */
    EINVAL,      /*                                  185  */
    EINVAL,      /* ERROR_INVALID_FLAG_NUMBER        186  */
    EINVAL,      /* ERROR_SEM_NOT_FOUND              187  */
    ENOEXEC,     /* ERROR_INVALID_STARTING_CODESEG   188  */
    ENOEXEC,     /* ERROR_INVALID_STACKSEG           189  */
    ENOEXEC,     /* ERROR_INVALID_MODULETYPE         190  */
    ENOEXEC,     /* ERROR_INVALID_EXE_SIGNATURE      191  */
    ENOEXEC,     /* ERROR_EXE_MARKED_INVALID         192  */
    ENOEXEC,     /* ERROR_BAD_EXE_FORMAT             193  */
    ENOEXEC,     /* ERROR_ITERATED_DATA_EXCEEDS_64k  194  */
    ENOEXEC,     /* ERROR_INVALID_MINALLOCSIZE       195  */
    ENOEXEC,     /* ERROR_DYNLINK_FROM_INVALID_RING  196  */
    ENOEXEC,     /* ERROR_IOPL_NOT_ENABLED           197  */
    ENOEXEC,     /* ERROR_INVALID_SEGDPL             198  */
    ENOEXEC,     /* ERROR_AUTODATASEG_EXCEEDS_64k    199  */
    ENOEXEC,     /* ERROR_RING2SEG_MUST_BE_MOVABLE   200  */
    ENOEXEC,     /* ERROR_RELOC_CHAIN_XEEDS_SEGLIM   201  */
    ENOEXEC,     /* ERROR_INFLOOP_IN_RELOC_CHAIN     202  */
    EINVAL,      /* ERROR_ENVVAR_NOT_FOUND           203  */
    EINVAL,      /*                                  204  */
    EINVAL,      /* ERROR_NO_SIGNAL_SENT             205  */
    ENOENT,      /* ERROR_FILENAME_EXCED_RANGE       206  */
    EINVAL,      /* ERROR_RING2_STACK_IN_USE         207  */
    EINVAL,      /* ERROR_META_EXPANSION_TOO_LONG    208  */
    EINVAL,      /* ERROR_INVALID_SIGNAL_NUMBER      209  */
    EINVAL,      /* ERROR_THREAD_1_INACTIVE          210  */
    EINVAL,      /*                                  211  */
    EINVAL,      /* ERROR_LOCKED                     212  */
    EINVAL,      /*                                  213  */
    EINVAL,      /* ERROR_TOO_MANY_MODULES           214  */
    EAGAIN       /* ERROR_NESTING_NOT_ALLOWED        215  */
};

/* size of the table */
#define ERRMAPTABLESIZE (sizeof(errMapTable)/sizeof(errMapTable[0]))

/*
** void __cdecl _dosmaperr(winerrno)
** 
** Takes a Windows error number and tries to map it to a Unix System V errno.
** Sets:
**   _doserrno = Windows error number
**   errno = Unix System V errno. 
*/
void __cdecl _dosmaperr(unsigned long winerrno)
{
    _doserrno = winerrno;

    if (winerrno >= ERRMAPTABLESIZE) {
        if (winerrno == ERROR_NOT_ENOUGH_QUOTA) {     /* exception for 1816 */
            errno = ENOMEM;
        } else {
            errno = EINVAL;
        }
    } else {
        errno = (unsigned int) errMapTable[winerrno];
    }
}

#endif  /* WIN32 && _MT && _DLL */

