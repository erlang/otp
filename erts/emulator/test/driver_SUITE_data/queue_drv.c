#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

/*
 * Data operations. To use, send code using erlang:port_control/2,
 * then send the data to the port.
 */

#define PUSHQ 0
#define ENQ 1
#define PUSHQ_BIN 2
#define ENQ_BIN 3
#define PUSHQV 4
#define ENQV 5

/*
 * Control operations. Data is returned directly.
 */
#define DEQ 6
#define BYTES_QUEUED 7
#define READ_HEAD 8

static ErlDrvPort erlang_port;
static unsigned opcode;		/* Opcode for next operation. */
static ErlDrvData queue_start(ErlDrvPort, char*);
static void queue_stop(ErlDrvData), queue_read(ErlDrvData, char*, ErlDrvSizeT);
static void queue_outputv(ErlDrvData, ErlIOVec*);
static ErlDrvSSizeT control(ErlDrvData, unsigned int,
			    char*, ErlDrvSizeT, char**, ErlDrvSizeT);
static ErlDrvBinary* read_head(ErlDrvPort, int bytes);

static ErlDrvEntry queue_driver_entry =
{
    NULL,
    queue_start,
    queue_stop,
    queue_read,
    NULL,
    NULL,
    "queue_drv",
    NULL,
    NULL,
    control,
    NULL,
    queue_outputv,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(queue_drv)
{
    erlang_port = (ErlDrvPort) -1;
    return &queue_driver_entry;
}

static ErlDrvData queue_start(ErlDrvPort port, char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    erlang_port = port;
    opcode = 0xFFFFFFFF;
    set_port_control_flags(erlang_port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData)port;
}

/* messages from Erlang */
static void queue_read(ErlDrvData port, char *buf, ErlDrvSizeT len)
{
}

static void queue_stop(ErlDrvData port)
{
    erlang_port = (ErlDrvPort) -1;
}

static ErlDrvSSizeT
control(ErlDrvData drv_data, unsigned command,
	char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen)
{
    ErlDrvBinary* b;

    switch (command) {
    case PUSHQ: 
    case ENQ:
    case PUSHQ_BIN:
    case ENQ_BIN:
    case PUSHQV:
    case ENQV:
	opcode = command;
	*rbuf = NULL;
	return 0;
    case DEQ:
	*rbuf = NULL;
	if (len != 4) {
	    driver_failure_atom(erlang_port, "deq: bad length");
	} else {
	    int n = get_int32(buf);
	    driver_deq(erlang_port, n);
	}
	return 0;
    case BYTES_QUEUED:
 	*rbuf = (char*)(b = driver_alloc_binary(4));
	put_int32(driver_sizeq(erlang_port), b->orig_bytes);
	return 0;
    case READ_HEAD:
	if (len != 4) {
	    driver_failure_atom(erlang_port, "read_head: bad length");
	    return 0;
	} else {
	    int n = get_int32(buf);
	    *rbuf = (char *) read_head(erlang_port, n);
	    return 0;		/* Ignored anyway */
	}
    default:
	driver_failure_atom(erlang_port, "bad opcode to control()");
	return 0;
    }
}

static void
queue_outputv(ErlDrvData drv_data, ErlIOVec* ev) 
{
    ErlDrvBinary* bin;
    ErlDrvPort ix = (ErlDrvPort) drv_data;
    int i = ev->vsize - 1;
    int offset;

    switch (opcode) {
    case PUSHQ: 
	driver_pushq(ix, ev->iov[i].iov_base, ev->iov[i].iov_len);
	break;
    case ENQ:
	driver_enq(ix, ev->iov[i].iov_base, ev->iov[i].iov_len);
	break;
    case PUSHQ_BIN:
    case ENQ_BIN:
	if (ev->binv[i] != NULL) {
 	    bin = ev->binv[i];
	    offset = ev->iov[i].iov_base - bin->orig_bytes;
 	} else {
 	    bin = driver_alloc_binary(ev->iov[i].iov_len);
 	    memcpy(bin->orig_bytes, ev->iov[i].iov_base, ev->iov[i].iov_len);
	    offset = 0;
 	}
	if (opcode == PUSHQ_BIN) {
	    driver_pushq_bin(ix, bin, offset, ev->iov[i].iov_len);
	} else {
	    driver_enq_bin(ix, bin, offset, ev->iov[i].iov_len);
	}
	if (ev->binv[i] == NULL) {
	    driver_free_binary(bin);
	}
	break;
    case PUSHQV:
	driver_pushqv(ix, ev, 0);
	break;
    case ENQV:
	driver_enqv(ix, ev, 0);
	break;
    default:
	fprintf(stderr, "[queue_drv] Bad opcode %d\n", opcode);
	driver_failure_atom(ix, "bad_opcode");
	break;
    }
}

static ErlDrvBinary*
read_head(ErlDrvPort ix, int bytes)
{
    int len_io_queue;
    SysIOVec* iov = driver_peekq(ix, &len_io_queue); 
    int bytes_left = bytes;
    int copied = 0;
    ErlDrvBinary* b;
    int iv;

    b = driver_alloc_binary(bytes);
    iv = 0;
    while (bytes_left > 0 && iv < len_io_queue) {
	int n = (iov[iv].iov_len < bytes_left) ? iov[iv].iov_len : bytes_left;
	memcpy(b->orig_bytes+copied, iov[iv].iov_base, n);
	copied += n;
	bytes_left -= n;
	iv++;
    }
    return b;
}
