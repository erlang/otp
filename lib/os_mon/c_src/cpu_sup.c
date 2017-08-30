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
 * CPU supervision
 *
 * Uses kstat library only available on Solaris 2
 * Compile with: gcc -o cpu_sup cpu_sup.c -lkstat
 *
 * Use open_port({spawn,Prog},[stream]) to communicate.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__DragonFly__)
#include <sys/param.h>
#include <sys/sysctl.h>
#include <limits.h>
#include <fcntl.h>
#endif
#if defined(__FreeBSD__) || defined(__DragonFly__)
#include <kvm.h>
#include <sys/user.h>
#endif

#if defined(__sun__)
#include <kstat.h>
#endif

#if (defined(__APPLE__) && defined(__MACH__))
#include <mach/mach.h>
#endif

#include <errno.h>

#if defined(__sun__) || defined(__linux__)
#include <sys/sysinfo.h>
#endif

#if defined(__linux__)

#define PROCSTAT "/proc/stat"
#define BUFFERSIZE (256)
typedef struct {
    unsigned int id;
    unsigned long long 
	/* total, */
	user, 
	nice_user, 
	kernel, 
	idle, 
	io_wait, 
	hard_irq, 
	soft_irq, 
	steal;
} cpu_t;

#endif

#if (defined(__APPLE__) && defined(__MACH__))
#define CU_OSX_VALUES (5)
#endif

#if defined(__FreeBSD__)
#include <sys/resource.h>
#include <sys/sysctl.h>
#define CU_BSD_VALUES (6)
#endif


#define FD_IN		(0)
#define FD_OUT		(1)
#define FD_ERR		(2)

#define PING		'p'
#define NPROCS		'n'
#define AVG1		'1'
#define AVG5		'5'
#define AVG15		'f'
#define UTIL		'u'
#define QUIT		'q'


#define CU_CPU_ID	(0)
#define CU_USER		(1)
#define CU_NICE_USER	(2)
#define CU_KERNEL	(3)
#define CU_IO_WAIT	(4)
#define CU_IDLE		(5)
#define CU_HARD_IRQ	(6)
#define CU_SOFT_IRQ	(7)
#define CU_STEAL	(8)

#define CU_VALUES	(9)
#define CU_KSTAT_VALUES	(5)

/*
#define CU_FLG_CPU_ID		(0 << 1)
#define CU_FLG_USER		(0 << 2)
#define CU_FLG_NICE_USER	(0 << 3)
#define CU_FLG_KERNEL		(0 << 4)
#define CU_FLG_IO_WAIT		(0 << 5)
#define CU_FLG_IDLE		(0 << 6)
#define CU_FLG_HARD_IRQ		(0 << 7)
#define CU_FLG_SOFT_IRQ		(0 << 8)
#define CU_FLG_STEAL		(0 << 9)
*/

/* util_measure
 * In:
 * 	unsigned int **result_vec
 * 	int *result_sz
 * Purpose:
 * 	Retrieve CPU utilization
 * 	result_vec has 2 + np*ne*2 entries where np is number_of_cpus
 * 	|------|------|
 * 	|  np  |  ne  |
 * 	|------|------|
 * 	|val_id| value| (One entry)
 * 	|------|------|
 * 	|val_id| value| (One entry)
 * 	|------|------|
 *  	     ......
 * 	|------|------|
 * 	|val_id| value|
 * 	|------|------|
 * 	np = number of processors
 * 	ne = number of entries per processor
 */

static void util_measure(unsigned int **result_vec, int *result_sz);

#if defined(__sun__)
static unsigned int misc_measure(char* name);
#endif
static void sendi(unsigned int data);
static void sendv(unsigned int data[], int ints);
static void error(char* err_msg);

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__DragonFly__)
static void bsd_count_procs(void);
static void bsd_loadavg(int);
#endif

#if defined(__sun__)
static kstat_ctl_t *kstat_ctl;
#endif

#if defined(__linux__)
static int processors_online() {
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
}
#endif

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__)
void getsysctl(const char *, void *, size_t);
#endif

int main(int argc, char** argv) {
  char cmd;
  int rc;
  int sz;
  unsigned int *rv;
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__)) ||defined(__FreeBSD__)
  unsigned int no_of_cpus = 0;
#endif

#if defined(__sun__)
  kstat_ctl = kstat_open();
  if(!kstat_ctl)
    error("Can't open header kstat");
#endif

#if defined(__linux__)
    no_of_cpus = processors_online(); 
    if ( (rv = (unsigned int*)malloc(sizeof(unsigned int)*(2 + 2*no_of_cpus*CU_VALUES))) == NULL) {
	error("cpu_sup: malloc error");
    }
#endif

#if (defined(__APPLE__) && defined(__MACH__))
    getsysctl("hw.ncpu", &no_of_cpus, sizeof(int));
    if ( (rv = (unsigned int*)malloc(sizeof(unsigned int)*(2 + 2*no_of_cpus*CU_OSX_VALUES))) == NULL) {
	error("cpu_sup: malloc error");
    }
#endif

#if defined(__FreeBSD__)
    getsysctl("hw.ncpu", &no_of_cpus, sizeof(int));
    if ( (rv = (unsigned int*)malloc(sizeof(unsigned int)*(2 + 2*no_of_cpus*CU_BSD_VALUES))) == NULL) {
	error("cpu_sup: malloc error");
    }
#endif

  while(1) {

    rc = read(FD_IN, &cmd, 1);
    if (rc < 0) {
      if (errno == EINTR)
	continue;
      error("Error reading from Erlang");
    }

    if(rc == 0)
      error("Erlang has closed");
    
    switch(cmd) {
    case PING:		sendi(4711);					break;
#if defined(__sun__)
    case NPROCS:	sendi(misc_measure("nproc"));			break;
    case AVG1:		sendi(misc_measure("avenrun_1min"));		break;
    case AVG5:		sendi(misc_measure("avenrun_5min"));		break;
    case AVG15:		sendi(misc_measure("avenrun_15min"));		break;
#elif defined(__OpenBSD__) || (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__) || defined(__DragonFly__)
    case NPROCS:	bsd_count_procs();				break;
    case AVG1:		bsd_loadavg(0);					break;
    case AVG5:		bsd_loadavg(1);					break;
    case AVG15:		bsd_loadavg(2);					break;
#endif
#if defined(__sun__) || defined(__linux__) || (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__)
    case UTIL:		util_measure(&rv,&sz); 	sendv(rv, sz);		break;
#endif
    case QUIT:		free((void*)rv); return 0;
    default:		error("Bad command");				break;
    }
  }
  return 0; /* suppress warnings */
}

/* ---------------------------- *
 *     BSD stat functions 	*
 * ---------------------------- */
#if defined(__OpenBSD__) || (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__) || defined(__DragonFly__)

static void bsd_loadavg(int idx) {
    double avgs[3];
    if (getloadavg(avgs, 3) < 0) {
	error(strerror(errno));
	return;
    }
    sendi((unsigned int)(avgs[idx] * 256));
}

#endif

#if defined(__OpenBSD__)

static void bsd_count_procs(void) {
    int err, nproc;
    size_t len = sizeof(nproc);
    int mib[] = { CTL_KERN, KERN_NPROCS };

    err = sysctl(mib, sizeof(mib) / sizeof(mib[0]), &nproc, &len, NULL, 0);
    if (err) {
	error(strerror(errno));
	return;
    }

    sendi((unsigned int)nproc);
}

#elif defined(__FreeBSD__) || defined(__DragonFly__)

static void bsd_count_procs(void) {
    kvm_t *kd;
    struct kinfo_proc *kp;
    char err[_POSIX2_LINE_MAX];
    int cnt = 0;

    if ((kd = kvm_open(NULL, "/dev/null", NULL, O_RDONLY, err)) == NULL) {
	error(err);
	return;
    }

#if defined(KERN_PROC_PROC)
    if ((kp = kvm_getprocs(kd, KERN_PROC_PROC, 0, &cnt)) == NULL) {
#else
    if ((kp = kvm_getprocs(kd, KERN_PROC_ALL, 0, &cnt)) == NULL) {
#endif
	error(strerror(errno));
	return;
    }

    (void)kvm_close(kd);
    sendi((unsigned int)cnt);
}

#elif (defined(__APPLE__) && defined(__MACH__))

static void bsd_count_procs(void) {
    int err;
    size_t len = 0;
    int mib[] = { CTL_KERN, KERN_PROC, KERN_PROC_ALL };

    err = sysctl(mib, sizeof(mib) / sizeof(mib[0]), NULL, &len, NULL, 0);
    if (err) {
	error(strerror(errno));
	return;
    }

    sendi((unsigned int)(len / sizeof(struct kinfo_proc)));
}

#endif

/* ---------------------------- *
 *     Linux stat functions 	*
 * ---------------------------- */

#if defined(__linux__)

static cpu_t *read_procstat(FILE *fp, cpu_t *cpu) {
    char buffer[BUFFERSIZE];

    if (fgets(buffer, BUFFERSIZE, fp) == NULL) {
	memset(cpu, 0, sizeof(cpu_t));
	return cpu;
    }
    sscanf(buffer, "cpu%u %Lu %Lu %Lu %Lu %Lu %Lu %Lu %Lu",
	&(cpu->id),
	&(cpu->user),
	&(cpu->nice_user),
	&(cpu->kernel),
	&(cpu->idle),
	&(cpu->io_wait),
	&(cpu->hard_irq),
	&(cpu->soft_irq),
	&(cpu->steal));

    return cpu;
}

static void util_measure(unsigned int **result_vec, int *result_sz) {
    int no_of_cpus = processors_online();
    int i;
    char buffer[BUFFERSIZE];
    FILE *fp;
    unsigned int *rv = NULL;
    cpu_t cpu;
 
    if ( (fp = fopen(PROCSTAT,"r")) == NULL) {
	/* Check if procfs is mounted,
	 *  otherwise:
	 *  try and try again, bad procsfs.
	 */
	*result_sz = 0;
	return;
    }

	/*ignore read*/
    if (fgets(buffer, BUFFERSIZE, fp) == NULL) {
	*result_sz = 0;
	return;
    }
    rv = *result_vec; 
    rv[0] = no_of_cpus;
    rv[1] = CU_VALUES;
    ++rv; /* first value is number of cpus */
    ++rv; /* second value is number of entries */

    for (i = 0; i < no_of_cpus; ++i) {
	read_procstat(fp, &cpu);
	      
	rv[ 0] = CU_CPU_ID;    rv[ 1] = cpu.id;
	rv[ 2] = CU_USER;      rv[ 3] = cpu.user;
	rv[ 4] = CU_NICE_USER; rv[ 5] = cpu.nice_user;
	rv[ 6] = CU_KERNEL;    rv[ 7] = cpu.kernel;
	rv[ 8] = CU_IO_WAIT;   rv[ 9] = cpu.io_wait;
	rv[10] = CU_IDLE;      rv[11] = cpu.idle;
	rv[12] = CU_HARD_IRQ;  rv[13] = cpu.hard_irq;
	rv[14] = CU_SOFT_IRQ;  rv[15] = cpu.soft_irq;
	rv[16] = CU_STEAL;     rv[17] = cpu.steal;
	rv += CU_VALUES*2;
    }	

    fclose(fp);
    *result_sz = 2 + 2*CU_VALUES * no_of_cpus;
}

#endif

/* ---------------------------- *
 *     Unix kstat functions 	*
 * ---------------------------- */

#if defined(__sun__)
static unsigned int misc_measure(char* name) {
  static kstat_t *ksp = NULL;
  kstat_named_t* entry;
  kid_t kcid;

  kcid = kstat_chain_update(kstat_ctl);

  if(kcid == -1)
    error("Error updating kstat chain");

  if (!ksp || kcid != 0) {

    /* The kstat chain changed (or we are initializing);
       find system misc entry in the new chain... */
    
    ksp = kstat_lookup(kstat_ctl,"unix",0,"system_misc");
    if(!ksp)
      error("Can't open system_misc kstat");
  }

  kstat_read(kstat_ctl,ksp,NULL);
  entry = kstat_data_lookup(ksp,name);
  if(!entry)
    return -1;
  
  if(entry->data_type != KSTAT_DATA_UINT32)
    return -1;

  return entry->value.ui32;
}


static int cpu_cmp(const void *p1, const void *p2) {
  kstat_t *ksp1 = *((kstat_t **) p1);
  kstat_t *ksp2 = *((kstat_t **) p2);
  
  if (ksp1->ks_instance > ksp2->ks_instance)
    return 1;
  if (ksp1->ks_instance < ksp2->ks_instance)
    return -1;
  return 0;
}


static void util_measure(unsigned int **result_vec, int *result_sz) {
  static int no_of_cpus = 0;
  static kstat_t **cpu_ksps = NULL;
  static unsigned int * resv = NULL;
  unsigned int *rv = NULL;
  kstat_t *ksp;
  kid_t kcid;
  int cpu_stats_read;
  int i;

  kcid = kstat_chain_update(kstat_ctl);

  if(kcid == -1)
    error("Error updating kstat chain");

  if (no_of_cpus == 0 || kcid != 0) {

    /* The kstat chain changed (or we are initializing);
       find cpu_stat entries in the new chain... */

    no_of_cpus = 0;

    for(ksp = kstat_ctl->kc_chain; ksp; ksp = ksp->ks_next) {
      if (strcmp(ksp->ks_module, "cpu_stat") == 0
	  && ksp->ks_type == KSTAT_TYPE_RAW) {
	no_of_cpus++;
	/* Assumes that modifications of the cpu_stat_t struct
	   in future releases of Solaris only are additions
	   of fields at the end of the struct. */
	if(ksp->ks_data_size < sizeof(cpu_stat_t))
	  error("Error: unexpected kstat data size");
      }
    }

    free((void *) cpu_ksps);
    if (no_of_cpus > 0) {
      cpu_ksps = (kstat_t **) malloc(no_of_cpus*sizeof(kstat_t *));
      if(!cpu_ksps)
	error("Error allocating memory");
    
      i = 0;
      for(ksp = kstat_ctl->kc_chain;
	  ksp && i < no_of_cpus;
	  ksp = ksp->ks_next) {
	if (strcmp(ksp->ks_module, "cpu_stat") == 0
	    && ksp->ks_type == KSTAT_TYPE_RAW) {
	  cpu_ksps[i++] = ksp;
	}
      }

      if (i != no_of_cpus)
	error("Error: private kstat chain copy unexpectedly changed");

      /* Erlang assumes that cpu information are sent in ascending order;
	 sort them ... */
      qsort((void  *)cpu_ksps,(size_t)no_of_cpus,sizeof(kstat_t *),cpu_cmp);

    }

    free((void *) resv);
/* kstat defined values are:
 *	CU_CPU_ID
 *	CU_USER
 *	CU_KERNEL
 *	CU_IO_WAIT
 *	CU_IDLE
 */
    resv = (unsigned int *) malloc(sizeof(unsigned int)*(2 + 2*no_of_cpus*CU_KSTAT_VALUES));
    if(!resv)
      error("Error allocating memory");

  }

    /* Read cpu utilization statistics ... */

    rv = resv;
    rv++; /*first entry is np*/
    rv++; /*second entry is ne*/
    cpu_stats_read = 0;

    for(i = 0; i < no_of_cpus; i++) {
	if (kstat_read(kstat_ctl, cpu_ksps[i], NULL) != -1) {
	    cpu_stat_t *cpu_stat = (cpu_stat_t *)cpu_ksps[i]->ks_data;
	    
	    rv[ 0] = CU_CPU_ID;    rv[ 1] = cpu_ksps[i]->ks_instance;
	    rv[ 2] = CU_USER;      rv[ 3] = cpu_stat->cpu_sysinfo.cpu[CPU_USER];
	    rv[ 4] = CU_KERNEL;    rv[ 5] = cpu_stat->cpu_sysinfo.cpu[CPU_KERNEL];
	    rv[ 6] = CU_IO_WAIT;   rv[ 7] = cpu_stat->cpu_sysinfo.cpu[CPU_WAIT];
	    rv[ 8] = CU_IDLE;      rv[ 9] = cpu_stat->cpu_sysinfo.cpu[CPU_IDLE];
	    
	    rv += CU_KSTAT_VALUES*2;
	    cpu_stats_read++;
 	}
    }
    
    resv[0] = cpu_stats_read;
    resv[1] = CU_KSTAT_VALUES;

    *result_vec = resv;
    *result_sz = 2 + 2* CU_KSTAT_VALUES * cpu_stats_read;

}
#endif

/* ---------------------------- *
 *     OSX util functions      *
 * ---------------------------- */

#if (defined(__APPLE__) && defined(__MACH__))

static void util_measure(unsigned int **result_vec, int *result_sz) {
    natural_t no_of_cpus;
    processor_info_array_t info_array;
    mach_msg_type_number_t info_count;
    mach_port_t host_port;
    kern_return_t error;
    processor_cpu_load_info_data_t *cpu_load_info = NULL;
    unsigned int *rv = NULL;
    int i;

    host_port = mach_host_self();
    error = host_processor_info(host_port, PROCESSOR_CPU_LOAD_INFO,
                                &no_of_cpus, &info_array, &info_count);
    if (error != KERN_SUCCESS) {
      *result_sz = 0;
      return;
    }
    mach_port_deallocate(mach_task_self(), host_port);
    cpu_load_info = (processor_cpu_load_info_data_t *) info_array;

    rv = *result_vec; 
    rv[0] = no_of_cpus;
    rv[1] = CU_OSX_VALUES;
    ++rv; /* first value is number of cpus */
    ++rv; /* second value is number of entries */

    for (i = 0; i < no_of_cpus; ++i) {
        rv[0] = CU_CPU_ID;    rv[1] = i;
        rv[2] = CU_USER;      rv[3] = cpu_load_info[i].cpu_ticks[CPU_STATE_USER];
        rv[4] = CU_NICE_USER; rv[5] = cpu_load_info[i].cpu_ticks[CPU_STATE_NICE];
        rv[6] = CU_KERNEL;    rv[7] = cpu_load_info[i].cpu_ticks[CPU_STATE_SYSTEM];
        rv[8] = CU_IDLE;      rv[9] = cpu_load_info[i].cpu_ticks[CPU_STATE_IDLE];
        rv += CU_OSX_VALUES*2;
    }	

    *result_sz = 2 + 2*CU_OSX_VALUES * no_of_cpus;

    error = vm_deallocate(mach_task_self(), (vm_address_t)info_array,
                        info_count * sizeof(int));
    if (error != KERN_SUCCESS)
      *result_sz = 0;
}
#endif

/* ---------------------------- *
 *  Utils for OSX and FreeBSD 	*
 * ---------------------------- */

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__)

#define EXIT_WITH(msg) (rich_error(msg, __FILE__, __LINE__))
#define RICH_BUFLEN    (213)  /* left in error(char*) */

void rich_error(const char *reason, const char *file, const int line) {
    char buf[RICH_BUFLEN];
    snprintf(buf, RICH_BUFLEN, "%s (%s:%i)", reason, file, line);
    error(buf);
}
#undef RICH_BUFLEN

void getsysctl(const char *name, void *ptr, size_t len)
{
    size_t gotlen = len;
    if (sysctlbyname(name, ptr, &gotlen, NULL, 0) != 0) {
	EXIT_WITH("sysctlbyname failed");
    }
    if (gotlen != len) {
	EXIT_WITH("sysctlbyname: unexpected length");
    }
}
#endif


/* ---------------------------- *
 *     FreeBSD stat functions 	*
 * ---------------------------- */

#if defined(__FreeBSD__)

static void util_measure(unsigned int **result_vec, int *result_sz) {
    int no_of_cpus;
    size_t size_cpu_times;
    unsigned long *cpu_times;
    unsigned int *rv = NULL;
    int i;

    getsysctl("hw.ncpu", &no_of_cpus, sizeof(int));
    /* Header constant CPUSTATES = #long values per cpu. */
    size_cpu_times = sizeof(long) * CPUSTATES * no_of_cpus;
    cpu_times = malloc(size_cpu_times);
    if (!cpu_times) {
	EXIT_WITH("badalloc");
    }
    getsysctl("kern.cp_times", cpu_times, size_cpu_times);

    rv = *result_vec;
    rv[0] = no_of_cpus;
    rv[1] = CU_BSD_VALUES;
    ++rv; /* first value is number of cpus */
    ++rv; /* second value is number of entries */

    for (i = 0; i < no_of_cpus; ++i) {
        int offset = i * CPUSTATES;
	rv[ 0] = CU_CPU_ID;    rv[ 1] = i;
	rv[ 2] = CU_USER;      rv[ 3] = cpu_times[CP_USER + offset];
	rv[ 4] = CU_NICE_USER; rv[ 5] = cpu_times[CP_NICE + offset];
	rv[ 6] = CU_KERNEL;    rv[ 7] = cpu_times[CP_SYS + offset];
	rv[ 8] = CU_IDLE;      rv[ 9] = cpu_times[CP_IDLE + offset];
	rv[10] = CU_HARD_IRQ;  rv[11] = cpu_times[CP_INTR + offset];
	rv += CU_BSD_VALUES*2;
    }

    *result_sz = 2 + 2*CU_BSD_VALUES * no_of_cpus;
}
#endif


/* ---------------------------- *
 *	 Generic functions 	*
 * ---------------------------- */

static void sendi(unsigned int data) { sendv(&data, 1); }

static void sendv(unsigned int data[], int ints) {
    static unsigned char *buf = NULL;
    static int bufsz = 0;
    int rc, di, bi, msgsz;

    /* Assumes 32-bit integers... */

    msgsz = 4*ints;

    if(bufsz < msgsz) {
	if (buf != NULL) free((void *) buf);
	buf = malloc(msgsz);
	if (!buf) error("Error allocating memory");
	bufsz = msgsz;
    }

    for(bi = 0, di = 0; di < ints; di++) {
	buf[bi++] = (data[di] >> 24) & 0xff;
	buf[bi++] = (data[di] >> 16) & 0xff;
	buf[bi++] = (data[di] >>  8) & 0xff;
	buf[bi++] = (data[di]      ) & 0xff;
    }

  bi = 0;
  do {
    rc = write(FD_OUT, &buf[bi], msgsz - bi);
    if (rc < 0) {
      if (errno == EINTR)
	continue;
      error("Error writing to Erlang");
    }
    bi += rc;
  } while(msgsz - bi > 0);

}

static void error(char* err_msg) {
  /* 
   * if we get error here we have trouble,
   * silence unnecessary warnings
   */
  char buffer[256] = "[os_mon] cpu supervisor port (cpu_sup): ";
  int i = strlen(buffer), j = 0;
  int n = strlen(err_msg);

  while(i < 253 && j < n) {
      buffer[i++] = err_msg[j++];
  }
  buffer[i++] = '\r';
  buffer[i++] = '\n';

  /* try to use one write only */
  if(write(FD_ERR, buffer, i))
     ;
  exit(-1);
}
