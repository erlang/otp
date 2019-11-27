/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2019. All Rights Reserved.
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
 * User Supplied Socket Implementation (ussi)
 * for test purpose.
 */
#include <stdlib.h>
#include <string.h>
#include "ei.h"

struct my_ctx
{
    void* ctx;
};

/*
 * To minimize effort but still test a different context format
 * we cheat and wrap the existing TCP default callbacks.
 */
extern ei_socket_callbacks ei_default_socket_callbacks;

static int my_socket(void **ctx, void *setup_ctx)
{
    struct my_ctx *myctx = malloc(sizeof(struct my_ctx));
    int ret;
    ret = ei_default_socket_callbacks.socket(&myctx->ctx, NULL);
    *ctx = myctx;
    return ret;
}

static int my_close(void *ctx)
{
    struct my_ctx *myctx = ctx;
    int ret = ei_default_socket_callbacks.close(myctx->ctx);
    free(myctx);
    return ret;
}

static int my_get_fd(void *ctx, int *fd)
{
    struct my_ctx *myctx = ctx;
    return ei_default_socket_callbacks.get_fd(myctx->ctx, fd);
}

static int my_hs_packet_header_size(void *ctx, int *sz)
{
    struct my_ctx *myctx = ctx;
    return ei_default_socket_callbacks.handshake_packet_header_size(myctx->ctx, sz);
}

static int my_connect_handshake_complete(void *ctx)
{
    struct my_ctx *myctx = ctx;
    return ei_default_socket_callbacks.connect_handshake_complete(myctx->ctx);
}

static int my_accept_handshake_complete(void *ctx)
{
    struct my_ctx *myctx = ctx;
    return ei_default_socket_callbacks.accept_handshake_complete(myctx->ctx);
}

static int my_listen(void *ctx, void *addr, int *len, int backlog)
{
    struct my_ctx *myctx = ctx;
    return ei_default_socket_callbacks.listen(myctx->ctx, addr, len, backlog);
}

static int my_accept(void **ctx, void *addr, int *len, unsigned tmo)
{
    struct my_ctx *listen_ctx = *ctx;
    struct my_ctx *conn_ctx = malloc(sizeof(struct my_ctx));
    int ret;
    *conn_ctx = *listen_ctx;
    ret = ei_default_socket_callbacks.accept(&conn_ctx->ctx, addr, len, tmo);
    if (ret == 0)
        *ctx = conn_ctx;
    else
        free(conn_ctx);
    return ret;
}

static int my_connect(void *ctx, void *addr, int len, unsigned tmo)
{
    struct my_ctx *myctx = ctx;
    return ei_default_socket_callbacks.connect(myctx->ctx, addr, len, tmo);
}

static void* memdup(const void* mem, int nbytes)
{
    void *p = malloc(nbytes);
    memcpy(p, mem, nbytes);
    return p;
}

static void scramble(void* bytes, int nbytes)
{
/* Would be nice to really test that only our callbacks are used
   and the default ones are not.
   Need corresponding Erlang distribution impl to work.

    unsigned char *p = bytes;
    int i;
    for (i=0; i < nbytes; ++i)
        p[i] = ~p[i];
*/
}

/* our own iovec struct to avoid config dependency HAVE_WRITEV */
struct my_iovec {
    void  *iov_base;    /* Starting address */
    size_t iov_len;     /* Number of bytes to transfer */
};

static int my_writev(void *ctx, const void *viov, int iovcnt, ssize_t *len, unsigned tmo)
{
    struct my_ctx *myctx = ctx;
    struct my_iovec *iov;
    int i, ret;

    /* create mutable copy of both iovec and data */
    iov = memdup(viov, sizeof(struct my_iovec) * iovcnt);
    for (i=0; i < iovcnt; ++i) {
        iov[i].iov_base = memdup(iov[i].iov_base, iov[i].iov_len);
        scramble(iov[i].iov_base, iov[i].iov_len);
    }

    ret = ei_default_socket_callbacks.writev(myctx->ctx, viov, iovcnt, len, tmo);

    for (i=0; i < iovcnt; ++i)
        free(iov[i].iov_base);
    free(iov);
    return ret;
}

static int my_write(void *ctx, const char* buf, ssize_t *len, unsigned tmo)
{
    struct my_ctx *myctx = ctx;
    unsigned char* copy = memdup(buf, *len);
    int i, ret;

    scramble(copy, *len);
    ret = ei_default_socket_callbacks.write(myctx->ctx, copy, len, tmo);
    free(copy);
    return ret;
}

static int my_read(void *ctx, char* buf, ssize_t *len, unsigned tmo)
{
    struct my_ctx *myctx = ctx;
    int ret, i;

    ret = ei_default_socket_callbacks.read(myctx->ctx, buf, len, tmo);
    if (ret == 0)
        scramble(buf, *len);
    return ret;
}

ei_socket_callbacks my_ussi = {
    0, /* flags */
    my_socket,
    my_close,
    my_listen,
    my_accept,
    my_connect,
    my_writev,
    my_write,
    my_read,
    my_hs_packet_header_size,
    my_connect_handshake_complete,
    my_accept_handshake_complete,
    my_get_fd
};

void my_ussi_init(void)
{
    my_ussi.flags = ei_default_socket_callbacks.flags;
    if (!ei_default_socket_callbacks.writev)
        my_ussi.writev = NULL;
}
