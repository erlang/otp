#include <stdio.h>
#include "erl_driver.h"
#include <errno.h>
#include <string.h>
#include <assert.h>


/* -------------------------------------------------------------------------
** Data types
**/

struct my_thread {
    struct my_thread* next;
    ErlDrvTid tid;
};

typedef struct _erl_drv_data {
    ErlDrvPort   erlang_port;
    ErlDrvTermData caller;
    struct my_thread* threads;
} EchoDrvData;

struct remote_send_term {
    struct my_thread thread;
    ErlDrvTermData port;
    ErlDrvTermData caller;
    int len;
    char buf[1]; /* buf[len] */
};

#define ECHO_DRV_NOOP                 0
#define ECHO_DRV_OUTPUT               1
#define ECHO_DRV_OUTPUT2              2
#define ECHO_DRV_OUTPUT_BINARY        3
#define ECHO_DRV_OUTPUTV              4
#define ECHO_DRV_SET_TIMER            5
#define ECHO_DRV_FAILURE_EOF          6
#define ECHO_DRV_FAILURE_ATOM         7
#define ECHO_DRV_FAILURE_POSIX        8
#define ECHO_DRV_FAILURE              9
#define ECHO_DRV_OUTPUT_TERM         10
#define ECHO_DRV_DRIVER_OUTPUT_TERM  11
#define ECHO_DRV_SEND_TERM           12
#define ECHO_DRV_DRIVER_SEND_TERM    13
#define ECHO_DRV_SAVE_CALLER         14
#define ECHO_DRV_REMOTE_SEND_TERM    15


/* -------------------------------------------------------------------------
** Entry struct
**/

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command);
static void         echo_drv_stop(ErlDrvData drv_data);
static void         echo_drv_output(ErlDrvData drv_data, char *buf,
				    ErlDrvSizeT len);
static void         echo_drv_outputv(ErlDrvData drv_data, ErlIOVec *iov);
static void         echo_drv_finish(void);
static ErlDrvSSizeT echo_drv_control(ErlDrvData drv_data,
				     unsigned int command,
				     char *buf,  ErlDrvSizeT len,
				     char **rbuf, ErlDrvSizeT rlen);
static void echo_drv_timeout(ErlDrvData drv_data);
static ErlDrvSSizeT echo_drv_call(ErlDrvData drv_data,
                                  unsigned int command,
                                  char *buf, ErlDrvSizeT len,
                                  char **rbuf, ErlDrvSizeT rlen,
                                  unsigned int *flags);

static ErlDrvEntry echo_drv_entry = {
    NULL, /* init */
    echo_drv_start,
    echo_drv_stop,
    echo_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "echo_drv",
    echo_drv_finish,
    NULL, /* handle */
    echo_drv_control,
    echo_drv_timeout, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    echo_drv_call, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

static void* send_term_thread(void *);

/* -------------------------------------------------------------------------
** Entry functions
**/

DRIVER_INIT(echo_drv)
{
    char buff[5];
    size_t size = sizeof(buff);

    if (erl_drv_getenv("OUTPUTV", buff, &size) == -1) {
        echo_drv_entry.outputv = NULL;
    } else {
        echo_drv_entry.outputv = echo_drv_outputv;
    }

    return &echo_drv_entry;
}

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command)
{
    EchoDrvData *echo_drv_data_p = driver_alloc(sizeof(EchoDrvData));
    echo_drv_data_p->erlang_port = port;
    echo_drv_data_p->caller = driver_caller(port);
    echo_drv_data_p->threads = NULL;
    return echo_drv_data_p;
}

static void echo_drv_stop(EchoDrvData *data_p)
{
    struct my_thread* thr = data_p->threads;

    while (thr) {
        struct my_thread* next = thr->next;
        void* exit_value;
        int ret = erl_drv_thread_join(thr->tid, &exit_value);
        assert(ret == 0 && exit_value == NULL);
        driver_free(thr);
        thr = next;
    }
    driver_free(data_p);
}

static void echo_drv_outputv(ErlDrvData drv_data, ErlIOVec *iov)
{
    return;
}

static void echo_drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {
    EchoDrvData* data_p = (EchoDrvData *) drv_data;
    ErlDrvPort port = data_p->erlang_port;

    switch (buf[0]) {
    case ECHO_DRV_OUTPUT:
    {
        driver_output(port, buf+1, len-1);
        break;
    }
    case ECHO_DRV_OUTPUT2:
    {
         driver_output2(port, "a", 1, buf+1, len-1);
         break;
    }
    case ECHO_DRV_OUTPUT_BINARY:
    {
        ErlDrvBinary *bin = driver_alloc_binary(len-1);
        memcpy(&bin->orig_bytes, buf+1, len-1);
        driver_output_binary(port, "a", 1, bin, 1, len - 2);
        driver_free_binary(bin);
        break;
    }
    case ECHO_DRV_OUTPUTV:
    {
        ErlIOVec iov;
        ErlDrvSizeT sz;
        driver_enq(port, buf + 1, len - 1);
        sz = driver_peekqv(port, &iov);
        driver_outputv(port, "a", 1, &iov, 0);
        driver_deq(port, sz);
        break;
    }
    case ECHO_DRV_SET_TIMER:
    {
        driver_set_timer(port, 10);
        break;
    }
    case ECHO_DRV_FAILURE_EOF:
    {
        driver_failure_eof(port);
        break;
    }
    case ECHO_DRV_FAILURE_ATOM:
    {
        driver_failure_atom(port, buf+1);
        break;
    }
    case ECHO_DRV_FAILURE_POSIX:
    {
        driver_failure_posix(port, EAGAIN);
        break;
    }
    case ECHO_DRV_FAILURE:
    {
        driver_failure(port, buf[1]);
        break;
    }
    case ECHO_DRV_OUTPUT_TERM:
    case ECHO_DRV_DRIVER_OUTPUT_TERM:
    case ECHO_DRV_SEND_TERM:
    case ECHO_DRV_DRIVER_SEND_TERM:
    {
        ErlDrvTermData term[] = {
            ERL_DRV_ATOM, driver_mk_atom("echo"),
            ERL_DRV_PORT, driver_mk_port(port),
            ERL_DRV_BUF2BINARY, (ErlDrvTermData)(buf+1),
                                (ErlDrvTermData)(len - 1),
            ERL_DRV_TUPLE, 3};
        switch (buf[0]) {
        case ECHO_DRV_OUTPUT_TERM:
            erl_drv_output_term(driver_mk_port(port), term, sizeof(term) / sizeof(ErlDrvTermData));
            break;
        case ECHO_DRV_DRIVER_OUTPUT_TERM:
            driver_output_term(port, term, sizeof(term) / sizeof(ErlDrvTermData));
            break;
        case ECHO_DRV_SEND_TERM:
            driver_send_term(port, data_p->caller,
                             term, sizeof(term) / sizeof(ErlDrvTermData));
            break;
        case ECHO_DRV_DRIVER_SEND_TERM:
            erl_drv_send_term(driver_mk_port(port), data_p->caller,
                              term, sizeof(term) / sizeof(ErlDrvTermData));
            break;
        }
        break;
    }
    case ECHO_DRV_REMOTE_SEND_TERM:
    {
        struct remote_send_term *t = driver_alloc(sizeof(struct remote_send_term) + len);
        t->len = len-1;
        t->port = driver_mk_port(port);
        t->caller = data_p->caller;
        memcpy(t->buf, buf+1, t->len);
        erl_drv_thread_create("tmp_thread", &t->thread.tid, send_term_thread, t, NULL);
        t->thread.next = data_p->threads;
        data_p->threads = &t->thread;
        break;
    }
    case ECHO_DRV_SAVE_CALLER:
        data_p->caller = driver_caller(port);
        break;
    default:
        break;
    }
}

static void echo_drv_finish() {

}

static ErlDrvSSizeT echo_drv_control(ErlDrvData drv_data,
				     unsigned int command,
				     char *buf, ErlDrvSizeT len,
				     char **rbuf, ErlDrvSizeT rlen)
{
    if ((len - 1) > rlen)
        *rbuf = driver_alloc(len - 1);
    memcpy(*rbuf, buf+1, len-1);
    return len-1;
}

static void echo_drv_timeout(ErlDrvData drv_data)
{

}

static ErlDrvSSizeT echo_drv_call(ErlDrvData drv_data,
                                  unsigned int command,
                                  char *buf, ErlDrvSizeT len,
                                  char **rbuf, ErlDrvSizeT rlen,
                                  unsigned int *flags)
{
    if ((len - command) > rlen)
        *rbuf = driver_alloc(len - command);
    memcpy(*rbuf, buf+command, len-command);
    return len-command;
}

static void* send_term_thread(void *a)
{
    struct remote_send_term *t = (struct remote_send_term*)a;
    ErlDrvTermData term[] = {
            ERL_DRV_ATOM, driver_mk_atom("echo"),
            ERL_DRV_PORT, t->port,
            ERL_DRV_BUF2BINARY, (ErlDrvTermData)(t->buf),
                                (ErlDrvTermData)(t->len),
            ERL_DRV_TUPLE, 3};
    erl_drv_send_term(t->port, t->caller,
                      term, sizeof(term) / sizeof(ErlDrvTermData));
    return NULL;
}
