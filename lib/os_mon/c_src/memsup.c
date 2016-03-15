/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
 *  Purpose:  Portprogram for supervision of memory usage.
 *
 *  Synopsis: memsup
 *
 *  PURPOSE OF THIS PROGRAM
 *
 *  This program supervises the memory status of the entire system, and
 *  sends status reports upon request from the Erlang system
 *
 *  SPAWNING FROM ERLANG
 *
 *  This program is started from Erlang as follows,
 *
 *      Port = open_port({spawn, 'memsup'}, [{packet,1}]) for UNIX
 *
 *  Erlang sends one of the request condes defined in memsup.h and this program
 *  answers in one of two ways:
 *  * If the request is for simple memory data (which is used periodically
 *    for monitoring) the answer is simply sent in two packets.
 *  * If the request is for the system specific data, the answer is delivered
 *    in two packets per value, first a tag value, then the actual
 *    value. The values are delivered "as is", this interface is
 *    mainly for VxWorks.
 *  All numbers are sent as strings of hexadecimal digits.
 *
 *  SUNOS FAKING
 *
 *  When using SunOS 4, the memory report is faked. The total physical memory
 *  is always reported to be 256MB, and the used fraction to be 128MB.
 *  
 *  If capabilities, such as sysconf or procfs, is not defined on the system 
 *  memsup will fake memory usage as well.
 *  
 *  Following ordering is defined for extended memory,
 *  Linux:	procfs -> sysinfo -> sysconf -> fake
 *  Sunos:	sysconf -> fake
 *  other:	arch specific
 *  
 *  Todo:
 *  Memory retrieval should be defined by capabilities and not by archs.
 *  Ordering should be defined arch.
 *  
 *  STANDARD INPUT, OUTPUT AND ERROR
 *
 *  This program communicates with Erlang through the standard
 *  input and output file descriptors (0 and 1). These descriptors
 *  (and the standard error descriptor 2) must NOT be closed
 *  explicitely by this program at termination (in UNIX it is
 *  taken care of by the operating system itself; in VxWorks
 *  it is taken care of by the spawn driver part of the Emulator).
 *
 *  END OF FILE
 *
 *  If a read from a file descriptor returns zero (0), it means
 *  that there is no process at the other end of the connection
 *  having the connection open for writing (end-of-file).
 *
 */

#if defined(sgi) || defined(__sgi) || defined(__sgi__)
#include <sys/types.h>
#include <sys/sysmp.h>
#endif

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include <unistd.h>

#if (defined(__unix__) || defined(unix)) && !defined(USG)
#include <sys/param.h>
#endif

#include <stdarg.h>

#include <string.h>
#include <time.h>
#include <errno.h>

#ifdef BSD4_4
#include <sys/types.h>
#include <sys/sysctl.h>
#if !defined (__OpenBSD__) && !defined (__NetBSD__) 
#include <vm/vm_param.h>
#endif
#if defined (__FreeBSD__) || defined(__DragonFly__) || defined (__NetBSD__) || defined(__OpenBSD__)
#include <sys/vmmeter.h>
#endif
#endif

#if defined (__linux__)
#include <sys/sysinfo.h>
#endif

/* commands */
#include "memsup.h"

#define CMD_SIZE      1
#define MAX_CMD_BUF   10
#define ERLIN_FD      0
#define ERLOUT_FD     1


/* procfs */
#if defined(__linux__) 
#include <fcntl.h>
#define MEMINFO "/proc/meminfo"
#endif

/*  prototypes */

static void print_error(const char *,...);

#define MAIN main

/*
 * example, we want procfs information, now give them something equivalent: 
 * 
 * MemTotal:      4029352 kB	old 	HighTotal + LowTotal
 * MemFree:       1674168 kB	old	HighFree + LowFree
 * MemShared:           0 kB    old 	now always zero; not calculated
 * Buffers:        417164 kB	old	temporary storage for raw disk blocks
 * Cached:         371312 kB	old	in-memory cache for files read from the disk (the page cache)

 * Active:        1408492 kB	new

 * Inact_dirty:      7772 kB    new
 * Inact_clean:      2008 kB    new
 * Inact_target:        0 kB    new
 * Inact_laundry:       0 kB    new, and might be missing too

 * HighTotal:           0 kB
 * HighFree:            0 kB		memory area for userspace programs or for the pagecache
 * LowTotal:      4029352 kB		
 * LowFree:       1674168 kB		Highmem + kernel stuff, slab allocates here

 * SwapTotal:     4194296 kB	old	total amount of swap space available
 * SwapFree:      4194092 kB	old	Memory which has been evicted from RAM
 * Inactive:       549224 kB	2.5.41+
 * Dirty:             872 kB	2.5.41+	Memory which is waiting to get written back to the disk
 * Writeback:           0 kB	2.5.41+	Memory which is actively being written back to the disk
 * AnonPages:      787616 kB	??
 * Mapped:         113612 kB	2.5.41+	files which have been mmaped, such as libraries
 * Slab:           342864 kB	2.5.41+	in-kernel data structures cache
 * CommitLimit:   6208972 kB	??
 * Committed_AS:  1141444 kB	2.5.41+
 * PageTables:       9368 kB	2.5.41+
 * VmallocTotal: 34359738367 kB	??	total size of vmalloc memory area
 * VmallocUsed:     57376 kB	??	amount of vmalloc area which is used
 * VmallocChunk: 34359677947 kB	??	largest contigious block of vmalloc area which is free
 * ReverseMaps:      5738       2.5.41+	number of rmap pte chains
 * SwapCached:          0 kB	2.5.??+	
 * HugePages_Total:     0	2.5.??+
 * HugePages_Free:      0	2.5.??+
 * HugePages_Rsvd:      0	2.5.??+
 * Hugepagesize:     2048 kB	2.5.??
 *
 * This information should be generalized for generic platform i.e. erlang.
 */



#define F_MEM_TOTAL   (1 << 0)
#define F_MEM_FREE    (1 << 1)
#define F_MEM_BUFFERS (1 << 2)
#define F_MEM_CACHED  (1 << 3)
#define F_MEM_SHARED  (1 << 4)
#define F_SWAP_TOTAL  (1 << 5)
#define F_SWAP_FREE   (1 << 6)

typedef struct {
    unsigned int flag;
    unsigned long pagesize;
    unsigned long total;
    unsigned long free;
    unsigned long buffered;
    unsigned long cached;
    unsigned long shared;
    unsigned long total_swap;
    unsigned long free_swap;
} memory_ext;

typedef struct mem_table_struct {
  const char *name;     /* memory type name */
  unsigned long *slot; /* slot in return struct */
} mem_table_struct;


/*  static variables */

static char *program_name;

static void
send(unsigned long value, unsigned long pagesize) {
    char buf[32];
    int left, bytes, res;
    int hex_zeroes;

    for (hex_zeroes = 0; (pagesize % 16) == 0; pagesize /= 16) {
	hex_zeroes++;
    }
    
    sprintf(buf+1, "%lx", value*pagesize);
    bytes = strlen(buf+1);
    while (hex_zeroes-- > 0) {
	bytes++;
	buf[bytes] = '0';
    }
    buf[0] = (char) bytes;
    left = ++bytes;

    while (left > 0) {
	res = write(ERLOUT_FD, buf+bytes-left, left);
	if (res <= 0){
	    perror("Error writing to pipe");
	    exit(1);
	}
	left -= res;
    }
}

static void
send_tag(int value){
    unsigned char buf[2];
    int res,left;

    buf[0] = 1U;
    buf[1] = (unsigned char) value;
    left = 2;
    while(left > 0) {
	if((res = write(ERLOUT_FD, buf+left-2,left)) <= 0){
	    perror("Error writing to pipe");
	    exit(1);
	} else {
	    left -= res;
	}
    }
}

#ifdef BSD4_4
static int
get_vmtotal(struct vmtotal *vt) {
	static int vmtotal_mib[] = {CTL_VM, VM_METER};
	size_t size = sizeof *vt;

	return sysctl(vmtotal_mib, 2, vt, &size, NULL, 0) != -1;
}
#endif

#if defined(__linux__)


static int 
get_mem_procfs(memory_ext *me){
    int fd, nread;
    char buffer[4097];
    char *bp;
    unsigned long value;
    
    me->flag = 0;
    
    if ( (fd = open(MEMINFO, O_RDONLY)) < 0) return -1;

    if ( (nread = read(fd, buffer, 4096)) < 0) {
        close(fd);
	return -1;
    }
    close(fd);

    buffer[nread] = '\0';
    
    /* Total and free is NEEDED! */
    
    bp = strstr(buffer, "MemTotal:");    
    if (bp != NULL && sscanf(bp, "MemTotal: %lu kB\n", &(me->total)))  me->flag |= F_MEM_TOTAL;

    bp = strstr(buffer, "MemFree:");    
    if (bp != NULL && sscanf(bp, "MemFree: %lu kB\n", &(me->free)))    me->flag |= F_MEM_FREE;
    
    /* Extensions */
    
    bp = strstr(buffer, "Buffers:");    
    if (bp != NULL && sscanf(bp, "Buffers: %lu kB\n", &(me->buffered))) me->flag |= F_MEM_BUFFERS;
    
    bp = strstr(buffer, "Cached:");    
    if (bp != NULL && sscanf(bp, "Cached: %lu kB\n", &(me->cached)))   me->flag |= F_MEM_CACHED;
    

    /* Swap */
    
    bp = strstr(buffer, "SwapTotal:");    
    if (bp != NULL && sscanf(bp, "SwapTotal: %lu kB\n", &(me->total_swap))) me->flag |= F_SWAP_TOTAL;
    
    bp = strstr(buffer, "SwapFree:");    
    if (bp != NULL && sscanf(bp, "SwapFree: %lu kB\n", &(me->free_swap))) me->flag |= F_SWAP_FREE;
    
    me->pagesize = 1024; /* procfs defines its size in kB */
    
    return 1;   
}
#endif


/* arch specific functions */

#if defined(__linux__) && !defined(__ANDROID__)/* ifdef SYSINFO */
/* sysinfo does not include cached memory which is a problem. */
static int
get_extended_mem_sysinfo(memory_ext *me) {
    struct sysinfo info;
    me->flag = 0;
    if (sysinfo(&info) < 0) return -1;
    me->pagesize   = 1; 
    me->total      = info.totalram;
    me->free       = info.freeram;
    me->buffered   = info.bufferram;
    me->shared     = info.sharedram;
    me->total_swap = info.totalswap;
    me->free_swap  = info.freeswap;
    
    me->flag = F_MEM_TOTAL | F_MEM_FREE | F_MEM_SHARED | F_MEM_BUFFERS | F_SWAP_TOTAL | F_SWAP_FREE;

    return 1;
}
#endif


#if defined(_SC_AVPHYS_PAGES)
static int
get_extended_mem_sysconf(memory_ext *me) {
    me->total      = sysconf(_SC_PHYS_PAGES);
    me->free       = sysconf(_SC_AVPHYS_PAGES);
    me->pagesize   = sysconf(_SC_PAGESIZE);

    me->flag = F_MEM_TOTAL | F_MEM_FREE;

    return 1;
}
#endif

#if defined(BSD4_4)
static int
get_extended_mem_bsd4(memory_ext *me) {
    struct vmtotal vt;
    long pgsz;

    if (!get_vmtotal(&vt)) return 0;
    if ((pgsz = sysconf(_SC_PAGESIZE)) == -1) return 0;

    me->total      = (vt.t_free + vt.t_rm);
    me->free       = vt.t_free;
    me->pagesize   = pgsz;
    
    me->flag = F_MEM_TOTAL | F_MEM_FREE;
    
    return 1;
}
#endif

#if defined(sgi) || defined(__sgi) || defined(__sgi__)
static int
get_extended_mem_sgi(memory_ext *me) {
    struct rminfo rmi;
    if (sysmp(MP_SAGET, MPSA_RMINFO, &rmi, sizeof(rmi)) < 0)  return -1;

    me->total    = (unsigned long)(rmi.physmem);
    me->free     = (unsigned long)(rmi.freemem);
    me->pagesize = (unsigned long)getpagesize(); 
    me->flag = F_MEM_TOTAL | F_MEM_FREE;
    
    return 1;
}
#endif

static void
get_extended_mem(memory_ext *me) {
/* android */
#if defined(__ANDROID__)
    if (get_mem_procfs(me))  return;   

/* linux */
#elif defined(__linux__)
    if (get_mem_procfs(me))  return;
    if (get_extended_mem_sysinfo(me)) return;

/* bsd */
#elif defined(BSD4_4)
    if (get_extended_mem_bsd4(me))    return;

/* sgi */
#elif defined(sgi) || defined(__sgi) || defined(__sgi__)
    if (get_extended_mem_sgi(me))     return;
#endif

/* Does this exist on others than Solaris2? */
#if defined(_SC_AVPHYS_PAGES)
    if (get_extended_mem_sysconf(me)) return;

/* We fake the rest */
/* SunOS4 (for example) */
#else  
    me->free     = (1<<27);	       	/* Fake! 128 MB used */
    me->total    = (1<<28);		/* Fake! 256 MB total */
    me->pagesize = 1;
    me->flag = F_MEM_TOTAL | F_MEM_FREE;
#endif
}
    

static void 
get_basic_mem(unsigned long *tot, unsigned long *used, unsigned long *pagesize){
#if defined(_SC_AVPHYS_PAGES)	/* Does this exist on others than Solaris2? */
    unsigned long avPhys, phys, pgSz;
    
    phys = sysconf(_SC_PHYS_PAGES);
    avPhys = sysconf(_SC_AVPHYS_PAGES);
    *used = (phys - avPhys);
    *tot = phys;
    *pagesize = sysconf(_SC_PAGESIZE);
#elif defined(__linux__) && !defined(_SC_AVPHYS_PAGES)
    memory_ext me;
    if (get_mem_procfs(&me) < 0) {
        print_error("ProcFS read error");
        exit(1);
    }
    *tot      = me.total;
    *pagesize = me.pagesize;
    *used     = me.total - me.free;
#elif defined(BSD4_4)
    struct vmtotal vt;
    long pgsz;

    if (!get_vmtotal(&vt)) goto fail;
    if ((pgsz = sysconf(_SC_PAGESIZE)) == -1) goto fail;
    *tot = (vt.t_free + vt.t_rm);
    *used = vt.t_rm;
    *pagesize = pgsz;
    return;
fail:
    print_error("%s", strerror(errno));
    exit(1);
#elif defined(sgi) || defined(__sgi) || defined(__sgi__)
    struct rminfo rmi;
    if (sysmp(MP_SAGET, MPSA_RMINFO, &rmi, sizeof(rmi)) != -1) {
	*tot = (unsigned long)(rmi.physmem);
	*used = (unsigned long)(rmi.physmem - rmi.freemem);
	*pagesize = (unsigned long)getpagesize(); 
    } else {
	print_error("%s", strerror(errno));
	exit(1); 
    }
#else  /* SunOS4 */
    *used = (1<<27);	       	/* Fake! 128 MB used */
    *tot = (1<<28);		/* Fake! 256 MB total */
    *pagesize = 1;
#endif
}    

static void
simple_show_mem(void){
    unsigned long tot, used, pagesize;
    get_basic_mem(&tot, &used, &pagesize);
    send(used, pagesize);
    send(tot, pagesize);
}

static void 
extended_show_mem(void){
    memory_ext me;
    unsigned long ps;
    
    get_extended_mem(&me);
    ps = me.pagesize;
   
    if (me.flag & F_MEM_TOTAL)  { send_tag(MEM_TOTAL);        send(me.total, ps);      }
    if (me.flag & F_MEM_FREE)   { send_tag(MEM_FREE);         send(me.free, ps);       }

    /* extensions */
    if (me.flag & F_MEM_BUFFERS){ send_tag(MEM_BUFFERS);      send(me.buffered, ps);   }
    if (me.flag & F_MEM_CACHED) { send_tag(MEM_CACHED);       send(me.cached, ps);     }
    if (me.flag & F_MEM_SHARED) { send_tag(MEM_SHARED);       send(me.shared, ps);     }
    
    /* swap */
    if (me.flag & F_SWAP_TOTAL) { send_tag(SWAP_TOTAL);       send(me.total_swap, ps); }
    if (me.flag & F_SWAP_FREE)  { send_tag(SWAP_FREE);        send(me.free_swap, ps);  }
    
    /* total is system total*/
    if (me.flag & F_MEM_TOTAL)  { send_tag(MEM_SYSTEM_TOTAL); send(me.total, ps);     }
    send_tag(SHOW_SYSTEM_MEM_END);
}    

static void
message_loop(int erlin_fd)
{
    char cmdLen, cmd;
    int res;
    
    while (1){
	/*
	 *  Wait for command from Erlang
	 */
	if ((res = read(erlin_fd, &cmdLen, 1)) < 0) {
	    print_error("Error reading from Erlang");
	    return;
	}

	if (res == 1) {		/* Exactly one byte read ? */
	    if (cmdLen == 1){	/* Should be! */
		switch (read(erlin_fd, &cmd, 1)){
		case 1:	  
		    switch (cmd){
		    case SHOW_MEM:
			simple_show_mem();
			break;
		    case SHOW_SYSTEM_MEM:
			extended_show_mem();
			break;
		    default:	/* ignore all other messages */
			break;
		    }
		  break;
		  
		case 0:
		  print_error("Erlang has closed");
		  return;

		default:
		  print_error("Error reading from Erlang");
		  return;
		} /* switch() */
	    } else { /* cmdLen != 1 */
		print_error("Invalid command length (%d) received", cmdLen);
		return;
	    }
	} else {		/* Erlang end closed */
	    print_error("Erlang has closed");
	    return;
	}
    }
}

/*
 *  main
 */
int
MAIN(int argc, char **argv)
{
  program_name = argv[0];
  message_loop(ERLIN_FD);
  return 0;
}


/*
 *  print_error
 *
 */
static void
print_error(const char *format,...)
{
  va_list args;
  char buffer[256];

  va_start(args, format);
  vsnprintf(buffer, 256, format, args);
  va_end(args);
  /* try to use one write only */
  fprintf(stderr, "[os_mon] memory supervisor port (memsup): %s\r\n", buffer);
  fflush(stderr);
}
