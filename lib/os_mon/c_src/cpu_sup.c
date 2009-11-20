/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

#if defined(__sun__)
#include <kstat.h>
#endif

#include <sys/sysinfo.h>
#include <errno.h>

#if defined(__linux__)
#include <string.h>  /* strlen */

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

static unsigned int misc_measure(char* name);
static void send(unsigned int data);
static void sendv(unsigned int data[], int ints);
static void error(char* err_msg);

#if defined(__sun__)
static kstat_ctl_t *kstat_ctl;
#endif

#if defined(__linux__)
static int processors_online() {
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
}
#endif

int main(int argc, char** argv) {
  char cmd;
  int rc;
  int sz;
  unsigned int *rv;
  unsigned int no_of_cpus = 0;

#if defined(__sun__)
  kstat_ctl = kstat_open();
  if(!kstat_ctl)
    error("Can't open header kstat");
#endif

#if defined(__linux__)
    no_of_cpus = processors_online(); 
    if ( (rv = (unsigned int*)malloc(sizeof(unsigned int)*(2 + 2*no_of_cpus*CU_VALUES))) == NULL) {
	error("cpu_cup: malloc error");
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
    case PING:		send(4711);					break;
#if defined(__sun__)
    case NPROCS:	send(misc_measure("nproc"));			break;
    case AVG1:		send(misc_measure("avenrun_1min"));		break;
    case AVG5:		send(misc_measure("avenrun_5min"));		break;
    case AVG15:		send(misc_measure("avenrun_15min"));		break;
#endif
    case UTIL:		util_measure(&rv,&sz); 	sendv(rv, sz);		break;
    case QUIT:		free((void*)rv); return 0;
    default:		error("Bad command");				break;
    }
  }
  return 0; /* supress warnings */
}
/* ---------------------------- *
 *     Linux stat functions 	*
 * ---------------------------- */

#if defined(__linux__)

static cpu_t *read_procstat(FILE *fp, cpu_t *cpu) {
    char buffer[BUFFERSIZE];

    fgets(buffer, BUFFERSIZE, fp);
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

    fgets(buffer, BUFFERSIZE, fp); /*ignore read*/
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
  
  if(entry->data_type != KSTAT_DATA_ULONG)
    return -1;

  return entry->value.ul;
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
 *	 Generic functions 	*
 * ---------------------------- */

static void send(unsigned int data) { sendv(&data, 1); }

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
  write(FD_ERR, err_msg, strlen(err_msg));
  write(FD_ERR, "\n", 1);
  exit(-1);
}


