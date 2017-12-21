/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef UNIX
#if !defined(__WIN32__)
#define UNIX 1
#endif
#endif

#ifdef UNIX
#include <errno.h>
#include <stdio.h>
#include <stdlib.h> /* rand */
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#ifdef HAVE_POLL_H
#  include <poll.h>
#endif
#endif /* UNIX */

#include "erl_driver.h"

#define CHKIO_STOP				0
#define CHKIO_USE_FALLBACK_POLLSET		1
#define CHKIO_BAD_FD_IN_POLLSET			2
#define CHKIO_FD_CHANGE				4
#define CHKIO_STEAL				5
#define CHKIO_STEAL_AUX				6
#define CHKIO_SMP_SELECT                        7
#define CHKIO_DRV_USE                           8

#define CHKIO_FALLBACK_FDS 10

#define TRACEF(x) /*erts_printf x*/

#ifdef UNIX
typedef struct {
    int fd;
    int cnt;
} ChkioFallbackFd;

typedef struct {
    ChkioFallbackFd dev_null[CHKIO_FALLBACK_FDS];
    ChkioFallbackFd dev_zero[CHKIO_FALLBACK_FDS];
    ChkioFallbackFd pipe_in[CHKIO_FALLBACK_FDS];
    ChkioFallbackFd pipe_out[CHKIO_FALLBACK_FDS];
} ChkioFallbackData;

typedef struct {
    int fds[2];
    int same_fd;
} ChkioFdChange;

typedef struct {
    int fds[2];
} ChkioBadFdInPollset;

typedef struct {
    int driver_select_fds[2];
} ChkioSteal;

typedef struct {
    int driver_select_fds[2];
} ChkioStealAux;


typedef struct chkio_smp_select {
    struct chkio_smp_select* next;
    int read_fd;
    int write_fd;
    int next_read;
    int next_write;
    int first_write;
    enum {Closed, Opened, Selected, Waiting} state;
    int wasSelected;
    unsigned rand_state;
}ChkioSmpSelect;

ChkioSmpSelect* smp_pipes;
unsigned smp_pipes_cnt;
ErlDrvMutex* smp_pipes_mtx;

typedef struct {
    int script_line;
    int fd_in;
    int fd_out;
    int fd_pipe[2];
    volatile int fd_stop_select;
    int timeouts_left;
    void* expected_callback;
    int   expected_fd;
}ChkioDrvUse;
static ChkioDrvUse drv_use_singleton;

typedef struct {
    ErlDrvPort port;
    ErlDrvTermData id;
    int test;
    void *test_data;
} ChkioDrvData;


#endif /* UNIX */

static int chkio_drv_init(void);
static void chkio_drv_finish(void);
static ErlDrvData chkio_drv_start(ErlDrvPort, char *);
static void chkio_drv_stop(ErlDrvData);
static void chkio_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void chkio_drv_ready_output(ErlDrvData, ErlDrvEvent);
static ErlDrvSSizeT chkio_drv_control(ErlDrvData, unsigned int,
				      char *, ErlDrvSizeT, char **, ErlDrvSizeT);
static void chkio_drv_timeout(ErlDrvData);
static void chkio_drv_stop_select(ErlDrvEvent, void*);


static ErlDrvEntry chkio_drv_entry = { 
    chkio_drv_init,
    chkio_drv_start,
    chkio_drv_stop,
    NULL, /* output */
    chkio_drv_ready_input,
    chkio_drv_ready_output,
    "chkio_drv",
    chkio_drv_finish,
    NULL, /* handle */
    chkio_drv_control,
    chkio_drv_timeout,
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* unused_event_callback */

    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,/* void *handle2 */
    NULL,/* process_exit */
    chkio_drv_stop_select
};


#ifdef UNIX

static void chkio_drv_use(ChkioDrvData *cddp, void* callback);

static void
stop_use_fallback_pollset(ChkioDrvData *cddp)
{
    int i;
    ChkioFallbackData *cbdp = (ChkioFallbackData *) cddp->test_data;
    if (cbdp) {
	for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
	    if (cbdp->dev_null[i].fd >= 0) {
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->dev_null[i].fd,
				  DO_WRITE,
				  0) != 0) {
		    fprintf(stderr,
			    "%s:%d: Failed to deselect dev_null fd=%d\n",
			    __FILE__, __LINE__, cbdp->dev_null[i].fd);
		    abort();
		}
		close(cbdp->dev_null[i].fd);
	    }
	    if (cbdp->dev_zero[i].fd >= 0) {
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->dev_zero[i].fd,
				  DO_READ,
				  0) != 0) {
		    fprintf(stderr,
			    "%s:%d: Failed to deselct dev_zero fd=%d\n",
			    __FILE__, __LINE__, cbdp->dev_zero[i].fd);
		    abort();
		}
		close(cbdp->dev_zero[i].fd);
	    }
	    if (cbdp->pipe_in[i].fd >= 0) {
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->pipe_in[i].fd,
				  DO_READ,
				  0) != 0) {
		    fprintf(stderr,
			    "%s:%d: Failed to deselect pipe_in fd=%d\n",
			    __FILE__, __LINE__, cbdp->pipe_in[i].fd);
		    abort();
		}
		close(cbdp->pipe_in[i].fd);
	    }
	    if (cbdp->pipe_out[i].fd >= 0) {
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->pipe_out[i].fd,
				  DO_WRITE,
				  0) != 0) {
		    fprintf(stderr,
			    "%s:%d: Failed to deselect pipe_out fd=%d\n",
			    __FILE__, __LINE__, cbdp->pipe_out[i].fd);
		    abort();
		}
		close(cbdp->pipe_out[i].fd);
	    }
	}
	driver_free((void *) cbdp);
	cddp->test_data = NULL;
    }
    cddp->test = CHKIO_STOP;
}

static void
stop_fd_change(ChkioDrvData *cddp)
{
    if (cddp->test_data) {
	ChkioFdChange *cfcp = (ChkioFdChange *) cddp->test_data;
	cddp->test_data = NULL;
	driver_cancel_timer(cddp->port);
	if (cfcp->fds[0] >= 0) {
	    driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) cfcp->fds[0], DO_READ, 0);
	    close(cfcp->fds[0]);
	    close(cfcp->fds[1]);
	}
	driver_free((void *) cfcp);
    }
}

static void
stop_bad_fd_in_pollset(ChkioDrvData *cddp)
{
    if (cddp->test_data) {
	ChkioBadFdInPollset *bfipp = (ChkioBadFdInPollset *) cddp->test_data;
	cddp->test_data = NULL;
	driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) bfipp->fds[0], DO_WRITE, 0);
	driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) bfipp->fds[1], DO_READ, 0);
	driver_free((void *) bfipp);
    }
}

static void
stop_steal(ChkioDrvData *cddp)
{
    if (cddp->test_data) {
	ChkioSteal *csp = cddp->test_data;
	cddp->test_data = NULL;
	if (csp->driver_select_fds[0] >= 0)
	    driver_select(cddp->port,
			  (ErlDrvEvent) (ErlDrvSInt) csp->driver_select_fds[0],
			  DO_READ,
			  0);
	if (csp->driver_select_fds[1] >= 0)
	    driver_select(cddp->port,
			  (ErlDrvEvent) (ErlDrvSInt) csp->driver_select_fds[1],
			  DO_WRITE,
			  0);
	driver_free(csp);
    }
}

static void
stop_steal_aux(ChkioDrvData *cddp)
{
    if (cddp->test_data) {
	ChkioStealAux *csap = cddp->test_data;
	cddp->test_data = NULL;
	if (csap->driver_select_fds[0] >= 0)
	    close(csap->driver_select_fds[0]);
	if (csap->driver_select_fds[1] >= 0)
	    close(csap->driver_select_fds[1]);
	driver_free(csap);
    }
}

static void free_smp_select(ChkioSmpSelect* pip, ErlDrvPort port)
{	
    switch (pip->state) {
    case Waiting: {
	int word;
	fprintf(stderr, "Closing pipe in state Waiting. Event lost?\n");
	for (;;) {
	    int bytes = read(pip->read_fd, &word, sizeof(word));
	    if (bytes != sizeof(word)) {
		if (bytes != 0) {
		    fprintf(stderr, "Failed to read from pipe, bytes=%d, errno=%d\n", bytes, errno);
		}
		break;
	    }
	    fprintf(stderr, "Read from pipe: %d\n", word);
	}
	abort();
    }
    case Selected:
    case Opened:
        TRACEF(("%T: Close pipe [%d->%d]\n", driver_mk_port(port), pip->write_fd,
                pip->read_fd));
        if (pip->wasSelected)
            driver_select(port, (ErlDrvEvent)(ErlDrvSInt)pip->read_fd, DO_READ|ERL_DRV_USE, 0);
        else
            close(pip->read_fd);
	close(pip->write_fd);
	pip->state = Closed;
	break;
    }
    driver_free(pip);
}

static void
stop_smp_select(ChkioDrvData *cddp)
{
    ChkioSmpSelect* pip = (ChkioSmpSelect*)cddp->test_data;
    if (pip) free_smp_select(pip, cddp->port);
    erl_drv_mutex_lock(smp_pipes_mtx);
    if (smp_pipes_cnt > 0 && --smp_pipes_cnt == 0) {
	while (smp_pipes) {
	    ChkioSmpSelect* next = smp_pipes->next;
	    free_smp_select(smp_pipes, cddp->port);
	    smp_pipes = next;
	}
    }
    erl_drv_mutex_unlock(smp_pipes_mtx);
}

#endif /* UNIX */

/* -------------------------------------------------------------------------
** Entry functions
**/

DRIVER_INIT(chkio_drv)
{
    return &chkio_drv_entry;
}


static int
chkio_drv_init(void)
{
#ifdef UNIX
    smp_pipes_mtx = erl_drv_mutex_create("smp_pipes_mtx");
#endif
    return 0;
}

static void
chkio_drv_finish(void)
{
#ifdef UNIX
    erl_drv_mutex_destroy(smp_pipes_mtx);
#endif
}


static ErlDrvData
chkio_drv_start(ErlDrvPort port, char *command)
{
#ifndef UNIX
    return NULL;
#else
    ChkioDrvData *cddp = driver_alloc(sizeof(ChkioDrvData));
    if (!cddp) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    cddp->port = port;
    cddp->id = driver_mk_port(port);
    cddp->test = CHKIO_STOP;
    cddp->test_data = NULL;
    return (ErlDrvData) cddp;
#endif
}

static void
chkio_drv_stop(ErlDrvData drv_data) {
#ifdef UNIX
    int fd;
    ChkioDrvData *cddp = (ChkioDrvData *) drv_data;

    switch (cddp->test) {
    case CHKIO_STOP:
	break;
    case CHKIO_USE_FALLBACK_POLLSET:
	stop_use_fallback_pollset(cddp);
	break;
    case CHKIO_BAD_FD_IN_POLLSET:
	stop_bad_fd_in_pollset(cddp);
	break;
    case CHKIO_FD_CHANGE:
	stop_fd_change(cddp);
	break;
    case CHKIO_STEAL:
	stop_steal(cddp);
	break;
    case CHKIO_STEAL_AUX:
	stop_steal_aux(cddp);
	break;
    case CHKIO_SMP_SELECT:
	stop_smp_select(cddp);
	break;
    case CHKIO_DRV_USE:
	chkio_drv_use(cddp, chkio_drv_stop);
	break;
    default:
	fprintf(stderr,	"%s:%d: Invalid state\n", __FILE__, __LINE__);
	abort();
	break;
    }
    cddp->test = CHKIO_STOP;

    /* Make sure erts_poll() will handle update requests soon */
    fd = open("/dev/null", O_WRONLY);
    if (fd < 0) {
	fprintf(stderr,	"%s:%d: Failed to open /dev/null\n",
		__FILE__, __LINE__);
    }
    driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) fd, DO_WRITE, 1);
    driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) fd, DO_WRITE, 0);
    close(fd);


    driver_free((void *) cddp);

#endif
}


static void
chkio_drv_ready_output(ErlDrvData drv_data, ErlDrvEvent event)
{
#ifdef UNIX
    ChkioDrvData *cddp = (ChkioDrvData *) drv_data;
    int fd = (int) (ErlDrvSInt) event;

    switch (cddp->test) {
    case CHKIO_USE_FALLBACK_POLLSET: {
	int i;
	int fd_found = 0;
	ChkioFallbackData *cbdp = (ChkioFallbackData *) cddp->test_data;
	for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
	    if (cbdp->dev_null[i].fd == fd) {
		cbdp->dev_null[i].cnt++;
		fd_found = 1;
		break;
	    }
	    if (cbdp->pipe_out[i].fd == fd) {
		cbdp->pipe_out[i].cnt++;
		fd_found = 1;
		break;
	    }
	}
	if (!fd_found)
	    driver_failure_atom(cddp->port, "output_fd_not_found");
	break;
    }
    case CHKIO_STEAL:
	break;
    case CHKIO_STEAL_AUX:
	break;
    case CHKIO_DRV_USE:
	chkio_drv_use(cddp, chkio_drv_ready_output);
	break;
    default:
	driver_failure_atom(cddp->port, "unexpected_ready_output");
	break;
    }
#endif
}

static void
chkio_drv_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
#ifdef UNIX
    ChkioDrvData *cddp = (ChkioDrvData *) drv_data;
    int fd = (int) (ErlDrvSInt) event;

    switch (cddp->test) {
    case CHKIO_USE_FALLBACK_POLLSET: {
	int i;
	int fd_found = 0;
	ChkioFallbackData *cbdp = (ChkioFallbackData *) cddp->test_data;
	for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
	    if (cbdp->dev_zero[i].fd == fd) {
		cbdp->dev_zero[i].cnt++;
		fd_found = 1;
		break;
	    }
	    if (cbdp->pipe_in[i].fd == fd) {
		cbdp->pipe_in[i].cnt++;
		fd_found = 1;
		break;
	    }
	}
	if (!fd_found)
	    driver_failure_atom(cddp->port, "input_fd_not_found");
	break;
    }
    case CHKIO_FD_CHANGE:
        /* This may be triggered when an fd is closed while being selected on. */
        break;
    case CHKIO_STEAL:
	break;
    case CHKIO_STEAL_AUX:
	break;
    case CHKIO_SMP_SELECT: {	
	ChkioSmpSelect* pip = (ChkioSmpSelect*) cddp->test_data;
	int word=123456, bytes;
	unsigned inPipe, n;
	if (pip == NULL) {
	    printf("Read event on uninitiated pipe %d\n", fd);
	    abort(); 
	}
	if (pip->state != Selected && pip->state != Waiting) { 
	    printf("Read event on pipe in strange state %d\n", pip->state);
	    abort(); 
	}
	
	TRACEF(("Got read event on fd=%d, state=%d\n", fd, pip->state));

	inPipe = (pip->next_write - pip->next_read);
	if (inPipe == 0) {
	    bytes = read(pip->read_fd, &word, sizeof(word));
	    printf("Unexpected empty pipe, expected %u -> %u, bytes=%d, word=%d, written=%d\n",
		   pip->next_read, pip->next_write-1, bytes, word,
		   (pip->next_write - pip->first_write));
	    /*abort();
    	      Allow unexpected events as it's been seen to be triggered by epoll
	      on Linux. Most of the time the unwanted events are filtered by
	      the erl_check_io layer. But when fd's are reused the events may
	      slip up to the driver.
	    */
	    break; 
	}

	n = rand_r(&pip->rand_state) % (inPipe*4);
	if (n > inPipe) n = inPipe;
	TRACEF(("Read %u of %u words in pipe\n", n, inPipe));
 	for (; n; n--) {
	    bytes = read(pip->read_fd, &word, sizeof(word));
	    if (bytes != sizeof(word)) {
	        printf("Failed to read from pipe, ret=%u errno=%d\n", bytes, errno);
		abort();	    
	    }
	    if (word != pip->next_read) {
		printf("Unexpected word in pipe %d, expected %d\n", word, pip->next_read);
		abort();
	    }
	    TRACEF(("Read %d from fd=%d\n", word, fd));
	    pip->next_read++;
	}
	pip->state = Selected; /* not Waiting anymore */
	break;
    }
    case CHKIO_DRV_USE:
	chkio_drv_use(cddp, chkio_drv_ready_input);
	break;
    default:
	driver_failure_atom(cddp->port, "unexpected_ready_input");
	break;
    }
#endif
}

static void
chkio_drv_timeout(ErlDrvData drv_data)
{
#ifdef UNIX
    ChkioDrvData *cddp = (ChkioDrvData *) drv_data;
    switch (cddp->test) {
    case CHKIO_FD_CHANGE: {
	ChkioFdChange *cfcp = cddp->test_data;
	int in_fd = cfcp->fds[0];
	int out_fd = cfcp->fds[1];
	if (in_fd >= 0) {
	    if (driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) in_fd, DO_READ, 0) < 0)
		driver_failure_atom(cddp->port, "deselect_failed");
	    (void) write(out_fd, (void *) "!", 1);
	    close(out_fd);
	    close(in_fd);
	}
	if (pipe(cfcp->fds) < 0) {
	    driver_failure_posix(cddp->port, errno);
	}
	else {
	    if (driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) cfcp->fds[0],
			      DO_READ, 1) < 0)
		driver_failure_atom(cddp->port, "select_failed");
	    if (cfcp->fds[0] == in_fd)
		cfcp->same_fd++;
	    if (driver_set_timer(cddp->port, 10) < 0)
		driver_failure_atom(cddp->port, "set_timer_failed");
	}
	break;
    }
    case CHKIO_DRV_USE:
	chkio_drv_use(cddp, chkio_drv_timeout);
	break;
    default:
	driver_failure_atom(cddp->port, "unexpected_driver_timeout");
	break;
    }
#endif /* UNIX */
}

static ErlDrvSSizeT
chkio_drv_control(ErlDrvData drv_data,
		 unsigned int command,
		 char *buf, ErlDrvSizeT len,
		 char **rbuf, ErlDrvSizeT rlen)
{
    char *res_str;
    ErlDrvSSizeT res_len = -1;
#ifndef UNIX
#ifdef __WIN32__
    res_str = "skip: windows_different";
#else
    res_str = "nyiftos";
#endif
#else
    ChkioDrvData *cddp = (ChkioDrvData *) drv_data;
    res_len = 0;
    switch (command) {
    case CHKIO_STOP: {

	/*
	 * --- STOP BEGIN ---------------------------------------------------
	 */
	switch (cddp->test) {
	case CHKIO_STOP:
	    driver_failure_atom(cddp->port, "stop_when_stopped");
	    break;
	case CHKIO_USE_FALLBACK_POLLSET: {
	    char *c;
	    int i;
	    ChkioFallbackData *cbdp = (ChkioFallbackData *) cddp->test_data;
	    c = driver_alloc(sizeof(char)*(4*20+21*CHKIO_FALLBACK_FDS*8));
	    if (!c)
		return 0;
	    *rbuf = c;
	    c += sprintf(c, "/dev/null: ");
	    for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
		c += sprintf(c, "%d=%d ",
			     cbdp->dev_null[i].fd,
			     cbdp->dev_null[i].cnt);
	    }
	    c += sprintf(c, "\n/dev/zero: ");
	    for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
		c += sprintf(c, "%d=%d ",
			     cbdp->dev_zero[i].fd,
			     cbdp->dev_zero[i].cnt);
	    }
	    c += sprintf(c, "\npipe_in: ");
	    for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
		c += sprintf(c, "%d=%d ",
			     cbdp->pipe_in[i].fd,
			     cbdp->pipe_in[i].cnt);
	    }
	    c += sprintf(c, "\npipe_out: ");
	    for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
		c += sprintf(c, "%d=%d ",
			     cbdp->pipe_out[i].fd,
			     cbdp->pipe_out[i].cnt);
	    }
	    c += sprintf(c, "\n");
	    res_len = (int) (c - *rbuf);
	    stop_use_fallback_pollset(cddp);
	    break;
	}
	case CHKIO_BAD_FD_IN_POLLSET:
	    res_str = "ok";
	    res_len = -1;
	    stop_bad_fd_in_pollset(cddp);
	    break;
	case CHKIO_FD_CHANGE: {
	    ChkioFdChange *cfcp = cddp->test_data;
	    if (!cfcp->same_fd)
		driver_failure_atom(cddp->port, "never_same_fd");
	    else {
		char *c = driver_alloc(sizeof(char)*30);
		if (!c)
		    driver_failure_posix(cddp->port, ENOMEM);
		else {
		    *rbuf = c;
		    res_len = sprintf(c, "same_fd=%d\n", cfcp->same_fd);
		}
	    }
	    stop_fd_change(cddp);
	    break;
	}
	case CHKIO_STEAL:
	    stop_steal(cddp);
	    res_str = "ok";
	    res_len = -1;
	    break;
	case CHKIO_STEAL_AUX:
	    stop_steal_aux(cddp);
	    res_str = "ok";
	    res_len = -1;
	    break;
	default:
	    driver_failure_atom(cddp->port, "invalid_state");
	    break;
	}
	break;
    }
	/*
	 * --- STOP END -----------------------------------------------------
	 */

    case CHKIO_USE_FALLBACK_POLLSET: {
	ChkioFallbackData *cbdp = driver_alloc(sizeof(ChkioFallbackData));
	cddp->test_data = (void *) cbdp;
	if (!cbdp)
	    driver_failure_posix(cddp->port, ENOMEM);
	else {
	    int i;
	    for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
		cbdp->dev_null[i].fd = -1;
		cbdp->dev_null[i].cnt = 0;
		cbdp->dev_zero[i].fd = -1;
		cbdp->dev_zero[i].cnt = 0;
		cbdp->pipe_in[i].fd = -1;
		cbdp->pipe_in[i].cnt = 0;
		cbdp->pipe_out[i].fd = -1;
		cbdp->pipe_out[i].cnt = 0;
	    }
	    for (i = 0; i < CHKIO_FALLBACK_FDS; i++) {
		int fds[2];
		cbdp->dev_null[i].fd = open("/dev/null", O_WRONLY);
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->dev_null[i].fd,
				  DO_WRITE,
				  1) != 0) {
		    driver_failure_posix(cddp->port, errno);
		    break;
		}
		cbdp->dev_zero[i].fd = open("/dev/zero", O_RDONLY);
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->dev_zero[i].fd,
				  DO_READ,
				  1) != 0) {
		    driver_failure_posix(cddp->port, errno);
		    break;
		}
		if (pipe(fds) < 0)
		    driver_failure_posix(cddp->port, errno);
		cbdp->pipe_in[i].fd = fds[0];
		cbdp->pipe_out[i].fd = fds[1];
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->pipe_in[i].fd,
				  DO_READ,
				  1) != 0) {
		    driver_failure_posix(cddp->port, EIO);
		    break;
		}
		if (i % 2 == 0)
		    (void) write(cbdp->pipe_out[i].fd, "!", 1);
		if (driver_select(cddp->port,
				  (ErlDrvEvent) (ErlDrvSInt) cbdp->pipe_out[i].fd,
				  DO_WRITE,
				  1) != 0) {
		    driver_failure_posix(cddp->port, EIO);
		    break;
		}
	    }
	    res_str = "ok";
	    res_len = -1;
	}
	break;
    }
    case CHKIO_BAD_FD_IN_POLLSET: {
	int i;
	int error = 0;
	int fds[11];
	for (i = 0; i < 11; i++)
	    fds[i] = -1;
	/* We open a bunch of fds and use the last ones so we decrease the
	   risk of selecting on a fd that someone else just opened */
	for (i = 0; i < 10; i++) {
	    fds[i] = open("/dev/null", O_WRONLY);
	    if (fds[i] < 0) {
		error = 1;
		driver_failure_posix(cddp->port, errno);
		break;
	    }
	}
	fds[10] = open("/dev/zero", O_RDONLY);
	if (fds[10] < 0) {
	    error = 1;
	    driver_failure_posix(cddp->port, errno);
	}
	for (i = 0; i < 11; i++) {
	    if (fds[i] >= 0)
		close(fds[i]);
	}
	if (!error) {
	    ChkioBadFdInPollset *bfipp;
	    bfipp = driver_alloc(sizeof(ChkioBadFdInPollset));
	    if (!bfipp)
		driver_failure_posix(cddp->port, ENOMEM);
	    else {
		bfipp->fds[0] = fds[9];
		bfipp->fds[1] = fds[10];
		cddp->test_data = (void *) bfipp;
		driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) fds[9], DO_WRITE, 1);
		driver_select(cddp->port, (ErlDrvEvent) (ErlDrvSInt) fds[10], DO_READ, 1);
	    }
	}
	res_str = "ok";
	res_len = -1;
	break;
    }
    case CHKIO_FD_CHANGE: {
	ChkioFdChange *cfcp = driver_alloc(sizeof(ChkioFdChange));
	if (!cfcp)
	    driver_failure_posix(cddp->port, ENOMEM);
	else {
	    cfcp->fds[0] = -1;
	    cfcp->fds[1] = -1;
	    cfcp->same_fd = 0;
	    cddp->test_data = cfcp;
	    driver_set_timer(cddp->port, 1);
	    res_str = "ok";
	    res_len = -1;
	}
	break;
    }
    case CHKIO_STEAL: {
	ChkioSteal *csp = driver_alloc(sizeof(ChkioSteal));
	char *c = driver_alloc(sizeof(char)*len+1);
	if (!c || !csp) {
	    if (c)
		driver_free(c);
	    if (csp)
		driver_free(csp);
	    driver_failure_posix(cddp->port, ENOMEM);
	    res_str = "error";
	    res_len = -1;
	}
	else {
	    int driver_select_fds[2];
	    cddp->test_data = csp; 
	    memcpy(c, buf, len);
	    c[len] = '\0';
	    if (sscanf(c,
		       "fds:%d:%d",
		       &driver_select_fds[0],
		       &driver_select_fds[1]) != 2)
                driver_failure_atom(cddp->port, "bad_input");
	    else {
		int res = 0;
                csp->driver_select_fds[0] = driver_select_fds[0]; /* In */
                csp->driver_select_fds[1] = driver_select_fds[1]; /* Out */

		/* Steal with driver_select() */
		if (res >= 0) {
		    res = driver_select(cddp->port,
					(ErlDrvEvent) (ErlDrvSInt) csp->driver_select_fds[0],
					DO_READ,
					1);
		    if (res < 0)
			driver_failure_atom(cddp->port,
					    "driver_select_failed_to_steal");
		}
		if (res >= 0) {
		    res = driver_select(cddp->port,
					(ErlDrvEvent) (ErlDrvSInt) csp->driver_select_fds[1],
					DO_WRITE,
					1);
		    if (res < 0)
			driver_failure_atom(cddp->port,
					    "driver_select_failed_to_steal");
		}

		res_str = res >= 0 ? "ok" : "error";
		res_len = -1;		
	    }
	    driver_free(c);
	}
	break;
    }
    case CHKIO_STEAL_AUX: {
	int read_fd;
	int write_fd;

	read_fd = open("/dev/zero", O_RDONLY);
	write_fd = open("/dev/null", O_WRONLY);
	
	if (read_fd < 0 || write_fd < 0) {
	    if (read_fd < 0)
		close(read_fd);
	    if (write_fd < 0)
		close(write_fd);
	    driver_failure_posix(cddp->port, errno);
	}
	else {
	    ChkioStealAux *csap = driver_alloc(sizeof(ChkioStealAux));
	    if (!csap) {
		driver_failure_posix(cddp->port, ENOMEM);
		res_str = "error";
		res_len = -1;
	    }
	    else {
		int res;
		cddp->test_data = csap;

		csap->driver_select_fds[0] = read_fd;
		csap->driver_select_fds[1] = write_fd;

		res = driver_select(cddp->port,
				    (ErlDrvEvent) (ErlDrvSInt) csap->driver_select_fds[0],
				    DO_READ,
				    1);
		if (res < 0)
		    driver_failure_atom(cddp->port, "driver_select_failed");
		if (res >= 0) {
		    res = driver_select(cddp->port,
					(ErlDrvEvent) (ErlDrvSInt) csap->driver_select_fds[1],
					DO_WRITE,
					1);
		    if (res < 0)
			driver_failure_atom(cddp->port, "driver_select_failed");
		}
		if (res < 0) {
		    res_str = "error";
		    res_len = -1;
		}
		else {
		    char *c = driver_alloc(sizeof(char)*(3+4*21+1));
		    if (!c) {
			res_str = "error";
			res_len = -1;
			driver_failure_posix(cddp->port, ENOMEM);
		    }
		    else {
			*rbuf = c;
			res_len = sprintf(c,
					  "fds:%d:%d",
					  csap->driver_select_fds[0],
					  csap->driver_select_fds[1]);
		    }
		}
	    }
	}
	break;
    }
    case CHKIO_SMP_SELECT: {
	ChkioSmpSelect* pip = (ChkioSmpSelect*) cddp->test_data;
	if (pip == NULL) {
	    erl_drv_mutex_lock(smp_pipes_mtx);
	    if (smp_pipes) {
		pip = smp_pipes;
		smp_pipes = smp_pipes->next;
	    }
	    else {
		cddp->test_data = driver_alloc(sizeof(ChkioSmpSelect));
		pip = (ChkioSmpSelect*) cddp->test_data;
		pip->state = Closed;
		pip->rand_state = 1;
		smp_pipes_cnt++;
	    }
	    erl_drv_mutex_unlock(smp_pipes_mtx);	    
	}
        res_str = NULL;
	{
	    int op = rand_r(&pip->rand_state);
	    switch (pip->state) {
	    case Closed: {
		int fds[2], flags;
		if (pipe(fds) < 0 ||
		    (flags = fcntl(fds[0], F_GETFL, 0)) < 0 ||
		    fcntl(fds[0], F_SETFL, flags|O_NONBLOCK) < 0) {

		    driver_failure_posix(cddp->port, errno);
		    break;
		}
		TRACEF(("%T: Created pipe [%d->%d]\n", cddp->id, fds[1], fds[0]));
		pip->read_fd = fds[0];
		pip->write_fd = fds[1];
		pip->state = Opened;
		pip->wasSelected = 0;
		pip->next_write = pip->next_read = rand_r(&pip->rand_state) % 1024;
		pip->first_write = pip->next_write;
		if (op & 1) break;
		op >>= 1;
	    }/*fall through*/
	    case Opened: {
		if (op & 1) {
		    TRACEF(("%T: Write %d to opened pipe [%d->%d]\n", cddp->id,
                            pip->next_write, pip->write_fd, pip->read_fd));
		    if (write(pip->write_fd, &pip->next_write, sizeof(int)) != sizeof(int)) {
			fprintf(stderr, "Failed to write to pipe fd=%d, errno=%d\n", pip->write_fd, errno);
			abort();
		    }
		    pip->next_write++;
		}
		op >>= 1;
		if (pip->wasSelected && (op & 1)) {
		    TRACEF(("%T: Close pipe [%d->%d]\n", cddp->id, pip->write_fd,
                            pip->read_fd));
                    drv_use_singleton.fd_stop_select = -2; /* disable stop_select asserts */
		    if (driver_select(cddp->port, (ErlDrvEvent)(ErlDrvSInt)pip->read_fd,
                                      DO_READ|ERL_DRV_USE, 0)
                        || close(pip->write_fd)) {
			fprintf(stderr, "Failed to close pipe, errno=%d\n", errno);
			abort();
		    }
		    pip->state = Closed;
		    break;
		}
		else {
		    TRACEF(("%T: Select on pipe [%d->%d]\n", cddp->id,
                            pip->write_fd, pip->read_fd));
		    if (driver_select(cddp->port, (ErlDrvEvent)(ErlDrvSInt)pip->read_fd,
                                      DO_READ|ERL_DRV_USE, 1)) {
			fprintf(stderr, "driver_select failed for fd=%d\n", pip->read_fd);
			abort();
		    }
		    pip->state = Selected;
		    pip->wasSelected = 1;
		    op >>= 1;
		    if (pip->next_write != pip->next_read) { /* pipe not empty */
			if (op & 1) {
			    pip->state = Waiting; /* Wait for reader */
			    break;
			}
			op >>= 1;
		    }
		}
	    }/*fall through*/
	    case Selected:
		if (op & 1) {
		    TRACEF(("%T: Write %d to selected pipe [%d->%d]\n", cddp->id,
			   pip->next_write, pip->write_fd, pip->read_fd));
		    if (write(pip->write_fd, &pip->next_write, sizeof(int)) != sizeof(int)) {
			fprintf(stderr, "Failed to write to pipe fd=%d, errno=%d\n", pip->write_fd, errno);
			abort();
		    }
		    pip->next_write++;
		}
		op >>= 1;
		if (op & 1) {
		    TRACEF(("%T: Deselect on pipe [%d->%d]\n", cddp->id, pip->write_fd, pip->read_fd));
		    if (driver_select(cddp->port, (ErlDrvEvent)(ErlDrvSInt)pip->read_fd, DO_READ, 0)) {
			fprintf(stderr, "driver_(de)select failed for fd=%d\n", pip->read_fd);
			abort();
		    }
		    pip->state = Opened;
		}
		op >>= 1;
		if (op & 1) {
		    TRACEF(("%T: Write %d to pipe [%d->%d] state=%d\n", cddp->id,
			    pip->next_write, pip->write_fd, pip->read_fd, pip->state));
		    if (write(pip->write_fd, &pip->next_write, sizeof(int)) != sizeof(int)) {
			fprintf(stderr, "Failed to write to pipe fd=%d, errno=%d\n", pip->write_fd, errno);
			abort();
		    }
		    pip->next_write++;
		}
		break;
            case Waiting:
                res_str = "yield";
                res_len = -1;
		break;
	    default:
		fprintf(stderr, "Strange state %d\n", pip->state);
		abort();
	    }
	    if (pip->state == Opened) { /* share unselected pipes with others */
		erl_drv_mutex_lock(smp_pipes_mtx);
		pip->next = smp_pipes;
		smp_pipes = pip;
		erl_drv_mutex_unlock(smp_pipes_mtx);
		cddp->test_data = NULL;
	    }
	    else {
		cddp->test_data = pip;
	    }
	}
        if (!res_str) {
            res_str = "ok";
            res_len = -1;
        }
	break;
    }
    case CHKIO_DRV_USE:
	chkio_drv_use(cddp, chkio_drv_control);
	res_str = "ok";
	res_len = -1;
	break;
    default:
	driver_failure_atom(cddp->port, "invalid_state");
	break;
    }
    cddp->test = command;
#endif /* UNIX */
    
    if (res_len >= 0)
	return res_len;

    res_len = strlen(res_str);
    if (res_len > rlen) {
	char *abuf = driver_alloc(sizeof(char)*res_len);
	if (!abuf)
	    return 0;
	*rbuf = abuf;
    }

    memcpy((void *) *rbuf, (void *) res_str, res_len);

    return res_len;
}

#ifdef UNIX

#define ASSERT(cond) \
    do{ \
	if (!(cond)) { assert_failed(cddp->port, #cond, __LINE__); return; } \
	/*else fprintf(stderr, "Assertion '%s' at line %d: OK\r\n", #cond, __LINE__);*/  \
    }while(0)

static void assert_print(char* str, int line)
{
    fprintf(stderr, "Assertion '%s' at line %d: FAILED\r\n", str, line);
}

static void assert_failed(ErlDrvPort port, char* str, int line)
{
    char buf[30];
    size_t bufsz = sizeof(buf);

    assert_print(str,line);

    if (erl_drv_getenv("ERL_ABORT_ON_FAILURE", buf, &bufsz) == 0
        && (strcmp("true", buf) == 0 || strcmp("yes", buf) == 0)) {
        abort();
    }
    else {
        snprintf(buf,sizeof(buf),"failed_at_line_%d",line);
        driver_failure_atom(port,buf);
    }
}

#define my_driver_select(PORT,FD,MODE,ON) \
    do{ if(driver_select(PORT, (ErlDrvEvent)(long)FD, MODE, ON) != 0) { \
	    assert_failed(cddp->port, "driver_select", __LINE__); \
	    return; \
        } \
    }while(0)


static void chkio_drv_use(ChkioDrvData *cddp, void* callback)
{
    ChkioDrvUse* cdu = (ChkioDrvUse*) cddp->test_data;
    int fd_stop_select = -1;

    /*fprintf(stderr, "Callback: %p\r\n", callback);*/

    if (cdu == NULL) {
	int ret;
	ASSERT(callback == chkio_drv_control);
	cdu = &drv_use_singleton;
	ASSERT(cdu->script_line == 0);
	cddp->test_data = cdu;
	cdu->fd_stop_select = -1;
	cdu->script_line = 1;
	cdu->fd_in = open("/dev/zero", O_RDONLY);
	ASSERT(cdu->fd_in > 0);
	cdu->fd_out = open("/dev/null", O_WRONLY);
	ASSERT(cdu->fd_out > 0);
	ret = pipe(cdu->fd_pipe);
	ASSERT(ret == 0);
    }
    else {
	if (callback == chkio_drv_timeout) {
	    if (cdu->fd_stop_select >= 0) {
		fd_stop_select = cdu->fd_stop_select;
		cdu->fd_stop_select = -1;
		fprintf(stderr,"timeout detected stop_select fd=%d\r\n", fd_stop_select);
		callback = chkio_drv_stop_select;		
		ASSERT(fd_stop_select == cdu->expected_fd);		
	    }
	    else if (--cdu->timeouts_left > 0) {
		driver_set_timer(cddp->port, 100);
		return;
	    }
	}
	ASSERT(callback == cdu->expected_callback);
    }

#define NEXT_CALLBACK(fn) \
    cdu->expected_callback = fn; \
    /*fprintf(stderr, "Next expected callback: %p\r\n", fn);*/ \
    cdu->script_line = __LINE__; break; case __LINE__: \
    fprintf(stderr, "Script line %d\r\n", cdu->script_line)

    switch (cdu->script_line) {
    case 1:
	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_READ|ERL_DRV_USE, 1);
	NEXT_CALLBACK(chkio_drv_ready_input);

	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_READ|ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_in;
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_WRITE|ERL_DRV_USE, 1);
	NEXT_CALLBACK(chkio_drv_ready_output);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_WRITE|ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_out;
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_READ|ERL_DRV_USE, 1);
	NEXT_CALLBACK(chkio_drv_ready_input);

	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_READ, 0);
	NEXT_CALLBACK(chkio_drv_timeout);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_WRITE|ERL_DRV_USE, 1);
	NEXT_CALLBACK(chkio_drv_ready_output);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_WRITE, 0);
	NEXT_CALLBACK(chkio_drv_timeout);

	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_in;
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_out;	
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_READ, 1);
	NEXT_CALLBACK(chkio_drv_ready_input);

	my_driver_select(cddp->port, cdu->fd_in, ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_in; 
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_WRITE, 1);
	NEXT_CALLBACK(chkio_drv_ready_output);

	my_driver_select(cddp->port, cdu->fd_out, ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_out;
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_pipe[0], ERL_DRV_READ|ERL_DRV_USE, 1);
	NEXT_CALLBACK(chkio_drv_timeout);

	my_driver_select(cddp->port, cdu->fd_pipe[0], ERL_DRV_USE, 0);
	my_driver_select(cddp->port, cdu->fd_pipe[0], ERL_DRV_READ|ERL_DRV_USE, 1);
	/* stop_select may or may not have been called up until now.
	   In either case it should not be called from here on. */
	cdu->fd_stop_select = -1;
	NEXT_CALLBACK(chkio_drv_timeout);

	my_driver_select(cddp->port, cdu->fd_pipe[0], ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_pipe[0]; 
	NEXT_CALLBACK(chkio_drv_stop_select);

	/* switch off USE again */
	my_driver_select(cddp->port, cdu->fd_pipe[0], ERL_DRV_USE, 0);
	cdu->expected_fd = cdu->fd_pipe[0];
	NEXT_CALLBACK(chkio_drv_stop_select);

	my_driver_select(cddp->port, cdu->fd_pipe[1], ERL_DRV_READ|ERL_DRV_WRITE|ERL_DRV_USE, 1);
	NEXT_CALLBACK(chkio_drv_ready_output);

	/* ERL_DRV_USE_NO_CALLBACK does not clear all */
	my_driver_select(cddp->port, cdu->fd_pipe[1], ERL_DRV_READ|ERL_DRV_USE_NO_CALLBACK, 0);
	NEXT_CALLBACK(chkio_drv_ready_output);

	my_driver_select(cddp->port, cdu->fd_pipe[1], ERL_DRV_WRITE|ERL_DRV_USE_NO_CALLBACK, 0);
	NEXT_CALLBACK(chkio_drv_timeout);

	cdu->script_line = 0; /* The End */
	cdu->expected_callback = chkio_drv_stop;
	break;

    case 0: /* close port */
	ASSERT(cdu->fd_stop_select < 0);
	close(cdu->fd_in); cdu->fd_in = -1;
	close(cdu->fd_out); cdu->fd_out = -1;
	close(cdu->fd_pipe[0]); cdu->fd_pipe[0] = -1;
	close(cdu->fd_pipe[1]); cdu->fd_pipe[1] = -1;
	/*driver_free(cdu); No, it's static */
	return;

    default:
	ASSERT(0);
    }
    if (cdu->script_line) {
	driver_set_timer(cddp->port, 100);
	cdu->timeouts_left = 5;
    }
    else {
	if (callback != chkio_drv_timeout) {
	    driver_cancel_timer(cddp->port);
	}
	driver_output(cddp->port, "TheEnd", 6);
    }
}

#endif /* UNIX */

static void chkio_drv_stop_select(ErlDrvEvent e, void* null)
{
#ifdef UNIX
    /*fprintf(stderr,"STOP_SELECT\r\n");*/
    if (!(null == NULL)) {
	assert_print("null==NULL", __LINE__); abort();
    }
    if (!(drv_use_singleton.fd_stop_select < 0)) {
	assert_print("fd_stop_select<0", __LINE__); abort(); 
    }
    /* fd_stop_select counting is disabled if this is set to -2 */
    if (drv_use_singleton.fd_stop_select == -2) {
        TRACEF(("closing %d\n", (int)(long)e));
        close((int)(long)e);
    } else
        drv_use_singleton.fd_stop_select = (int)(long)e;
    /* Can't call chkio_drv_use directly here. That could even be recursive.
     * Next timeout will detect it instead.
     */
#endif /* UNIX */
}


