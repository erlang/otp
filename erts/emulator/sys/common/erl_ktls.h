/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright 2026 Zeyu Zhang <zzy___1@sina.com>
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
 * The kernel TLS (Linux kTLS) state of a TCP socket that ssl handed over
 * ({ktls, true}), kept the same way by inet_drv and the socket NIF.
 *
 * ssl derives every new key, in whichever process handles the key change;
 * the socket keeps what that needs (the cipher suite and both traffic
 * secrets) and makes each change one atomic request: a key is installed
 * only if the secret it was derived from is still the current one, so
 * any number of processes can handle the same KeyUpdate or call
 * ssl:update_keys/2. A peer's KeyUpdate stays unread in the kernel until
 * the request that installs its key reads it. A KeyUpdate the peer
 * requested is owed instead of waited for when it cannot be sent: it then
 * goes out, and its key in, before any more data. An alert is queued
 * behind the output the socket still holds, and ends the stream.
 *
 * The includer defines ERTS_KTLS_ALLOC(Size) and ERTS_KTLS_FREE(Ptr), and
 * holds the socket's locks around every call.
 *
 * Requests, <<Op, Rest/binary>>, and their replies:
 *   INIT:    <<Suite:2/binary, SecretLen, RxSecret, TxSecret, RxInfoLen,
 *             RxInfo, TxInfoLen, TxInfo, InfoLen:32, Info, Early/binary>>
 *            installs the handshake's keys (Info: struct
 *            tls12_crypto_info_*) and stores the secrets, the opaque ssl
 *            Info and the Early data the handshake read; reply <<0>>
 *   STATE:   reply State = <<Suite:2/binary, SecretLen, RxSecret, TxSecret>>
 *   INFO:    reply Info
 *   CONTROL: the record at the head of the stream: <<0>> application
 *            data or none, <<1, Type, Data/binary>> a record it read,
 *            <<2, StateLen, State, Type, Data/binary>> a record holding a
 *            KeyUpdate, left unread, <<3>> the end of the stream
 *   UPDATE:  <<Flags, SecretLen,
 *              RxOld, RxNew, RxInfoLen, RxInfo,         if Flags has RX
 *              TxOld, TxNew, TxInfoLen, TxInfo, Request if Flags has TX>>
 *            RX installs RxInfo and reads the KeyUpdate record, TX sends a
 *            KeyUpdate with Request and installs TxInfo; reply <<0>> done,
 *            <<1>> RxOld is not the receive secret (another process
 *            installed this KeyUpdate's key), <<2>> TxOld is not the send
 *            secret (derive again); EAGAIN when the KeyUpdate cannot be
 *            sent now, unless Flags has MAY_OWE
 *   ALERT:   <<Level, Description>>; reply <<0>> sent, <<1>> queued
 *   KEY:     <<Dir, InfoLen, Info>> installs a key, with no state
 *   RECORD:  <<Type, Data/binary>> sends one record, with no state
 */

#ifndef ERL_KTLS_H__
#define ERL_KTLS_H__

#include <stddef.h>
#include <string.h>
#include <errno.h>
#ifdef __linux__
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#if defined(TCP_ULP)
#define ERTS_KTLS 1
#endif
#endif

#define ERTS_KTLS_MAX_SECRET 48
#define ERTS_KTLS_MAX_INFO   56     /* struct tls12_crypto_info_aes_gcm_256 */
#define ERTS_KTLS_MAX_RECORD 16384
#define ERTS_KTLS_BUF_SIZE   (ERTS_KTLS_MAX_RECORD + 128)

#define ERTS_KTLS_INIT    1
#define ERTS_KTLS_STATE   2
#define ERTS_KTLS_INFO    3
#define ERTS_KTLS_CONTROL 4
#define ERTS_KTLS_UPDATE  5
#define ERTS_KTLS_ALERT   6
#define ERTS_KTLS_KEY     7
#define ERTS_KTLS_RECORD  8

#define ERTS_KTLS_RX      1
#define ERTS_KTLS_TX      2
#define ERTS_KTLS_MAY_OWE 4

#define ERTS_KTLS_ALERT_NONE   0
#define ERTS_KTLS_ALERT_QUEUED 1
#define ERTS_KTLS_ALERT_SENT   2

typedef struct {
    unsigned char suite[2];
    unsigned char secret_len;   /* 0 until INIT */
    unsigned char rx_secret[ERTS_KTLS_MAX_SECRET];
    unsigned char tx_secret[ERTS_KTLS_MAX_SECRET]; /* of the owed key if any */
    unsigned char owed_len;     /* a KeyUpdate is owed, then owed_info */
    unsigned char owed_info[ERTS_KTLS_MAX_INFO];
    unsigned char alert;
    unsigned char alert_rec[2];
    char          *info;
    size_t        info_len;
    char          *early;       /* early data not yet delivered */
    size_t        early_len;
    size_t        early_pos;
} ErtsKtls;

#ifdef ERTS_KTLS

#define ERTS_KTLS_SOL_TLS         282
#define ERTS_KTLS_TLS_TX          1
#define ERTS_KTLS_TLS_RX          2
#define ERTS_KTLS_SET_RECORD_TYPE 1
#define ERTS_KTLS_GET_RECORD_TYPE 2
#define ERTS_KTLS_ALERT_RECORD    21
#define ERTS_KTLS_HANDSHAKE       22
#define ERTS_KTLS_APPLICATION     23
#define ERTS_KTLS_KEY_UPDATE      24

static inline void erts_ktls_wipe(void *p, size_t n)
{
    memset(p, 0, n);
    __asm__ __volatile__("" : : "r" (p) : "memory"); /* keep the memset */
}

static inline void erts_ktls_free(ErtsKtls *tls)
{
    erts_ktls_wipe(tls->rx_secret, sizeof(tls->rx_secret));
    erts_ktls_wipe(tls->tx_secret, sizeof(tls->tx_secret));
    erts_ktls_wipe(tls->owed_info, sizeof(tls->owed_info));
    if (tls->info != NULL)
        ERTS_KTLS_FREE(tls->info);
    if (tls->early != NULL)
        ERTS_KTLS_FREE(tls->early);
    tls->info = NULL;
    tls->early = NULL;
}

/* Copies up to len bytes of early data to buf */
static inline size_t erts_ktls_read_early(ErtsKtls *tls, void *buf, size_t len)
{
    size_t n;

    if (tls == NULL || tls->early == NULL)
        return 0;
    n = tls->early_len - tls->early_pos;
    if (n > len)
        n = len;
    memcpy(buf, tls->early + tls->early_pos, n);
    tls->early_pos += n;
    if (tls->early_pos == tls->early_len) {
        ERTS_KTLS_FREE(tls->early);
        tls->early = NULL;
    }
    return n;
}

/* 0 when Data went out as one record of Type, else an errno */
static inline int erts_ktls_send_record(int fd, unsigned char type,
                                        const void *data, size_t len)
{
    char cbuf[CMSG_SPACE(1)];
    struct iovec iov;
    struct msghdr m;
    struct cmsghdr *c;
    ssize_t n;

    memset(&m, 0, sizeof(m));
    memset(cbuf, 0, sizeof(cbuf));
    iov.iov_base = (void *) data;
    iov.iov_len = len;
    m.msg_iov = &iov;
    m.msg_iovlen = 1;
    m.msg_control = cbuf;
    m.msg_controllen = sizeof(cbuf);
    c = CMSG_FIRSTHDR(&m);
    c->cmsg_level = ERTS_KTLS_SOL_TLS;
    c->cmsg_type = ERTS_KTLS_SET_RECORD_TYPE;
    c->cmsg_len = CMSG_LEN(1);
    *CMSG_DATA(c) = type;
    n = sendmsg(fd, &m, MSG_DONTWAIT | MSG_NOSIGNAL);
    if (n == (ssize_t) len)
        return 0;
    return n < 0 ? errno : EIO;
}

/* The record at the head of the stream, its type from the control
 * message: its length, 0 at the end of the stream, or -1 and errno */
static inline ssize_t erts_ktls_recv_record(int fd, unsigned char *type,
                                            void *buf, size_t len, int flags)
{
    char cbuf[CMSG_SPACE(1)];
    struct iovec iov;
    struct msghdr m;
    struct cmsghdr *c;
    ssize_t n;

    memset(&m, 0, sizeof(m));
    iov.iov_base = buf;
    iov.iov_len = len;
    m.msg_iov = &iov;
    m.msg_iovlen = 1;
    m.msg_control = cbuf;
    m.msg_controllen = sizeof(cbuf);
    n = recvmsg(fd, &m, flags | MSG_DONTWAIT);
    c = n > 0 ? CMSG_FIRSTHDR(&m) : NULL;
    *type = (c != NULL && c->cmsg_level == ERTS_KTLS_SOL_TLS &&
             c->cmsg_type == ERTS_KTLS_GET_RECORD_TYPE) ?
        *CMSG_DATA(c) : ERTS_KTLS_APPLICATION;
    return n;
}

static inline int erts_ktls_set_key(int fd, int dir,
                                    const unsigned char *info, size_t len)
{
    (void) setsockopt(fd, IPPROTO_TCP, TCP_ULP, "tls", sizeof("tls"));
    if (setsockopt(fd, ERTS_KTLS_SOL_TLS, dir, info, (socklen_t) len) != 0)
        return errno;
    return 0;
}

/* Sends a KeyUpdate with the current key, then installs the next one */
static inline int erts_ktls_key_update(int fd, unsigned char request,
                                       const unsigned char *info, size_t len)
{
    unsigned char msg[5] = {ERTS_KTLS_KEY_UPDATE, 0, 0, 1, 0};
    int err;

    msg[4] = request;
    err = erts_ktls_send_record(fd, ERTS_KTLS_HANDSHAKE, msg, sizeof(msg));
    return err != 0 ? err : erts_ktls_set_key(fd, ERTS_KTLS_TLS_TX, info, len);
}

/* Before any more data: the owed KeyUpdate. 0, EAGAIN or an errno */
static inline int erts_ktls_flush_owed(ErtsKtls *tls, int fd)
{
    int err;

    if (tls == NULL || tls->owed_len == 0)
        return 0;
    err = erts_ktls_key_update(fd, 0, tls->owed_info, tls->owed_len);
    if (err == EAGAIN || err == EWOULDBLOCK)
        return EAGAIN;
    erts_ktls_wipe(tls->owed_info, sizeof(tls->owed_info));
    tls->owed_len = 0;
    return err;
}

/* Once no output is queued: the queued alert. 0, EAGAIN or an errno */
static inline int erts_ktls_flush_alert(ErtsKtls *tls, int fd)
{
    int err;

    if (tls == NULL || tls->alert != ERTS_KTLS_ALERT_QUEUED)
        return 0;
    err = erts_ktls_send_record(fd, ERTS_KTLS_ALERT_RECORD, tls->alert_rec,
                                sizeof(tls->alert_rec));
    if (err == EAGAIN || err == EWOULDBLOCK)
        return EAGAIN;
    tls->alert = ERTS_KTLS_ALERT_SENT;
    return err;
}

static inline size_t erts_ktls_put_state(const ErtsKtls *tls, unsigned char *p)
{
    size_t sl = tls->secret_len;

    p[0] = tls->suite[0];
    p[1] = tls->suite[1];
    p[2] = (unsigned char) sl;
    memcpy(p + 3, tls->rx_secret, sl);
    memcpy(p + 3 + sl, tls->tx_secret, sl);
    return 3 + 2 * sl;
}

static inline int erts_ktls_init(ErtsKtls *tls, int fd, int out_queued,
                                 const unsigned char *p, size_t len)
{
    const unsigned char *end = p + len, *suite = p, *rx, *tx, *rx_info,
        *tx_info, *info;
    size_t sl, rx_len, tx_len, info_len, early_len;
    char *info_copy = NULL, *early_copy = NULL;
    int err;

    if (tls->secret_len != 0)
        return EISCONN;
    if (len < 3 || (sl = p[2]) == 0 || sl > ERTS_KTLS_MAX_SECRET ||
        (size_t) (end - p) < 3 + 2 * sl + 1)
        return EINVAL;
    rx = p + 3;
    tx = rx + sl;
    p = tx + sl;
    rx_len = *p++;
    if (rx_len > ERTS_KTLS_MAX_INFO || (size_t) (end - p) < rx_len + 1)
        return EINVAL;
    rx_info = p;
    p += rx_len;
    tx_len = *p++;
    if (tx_len > ERTS_KTLS_MAX_INFO || (size_t) (end - p) < tx_len + 4)
        return EINVAL;
    tx_info = p;
    p += tx_len;
    info_len = ((size_t) p[0] << 24) | ((size_t) p[1] << 16) |
        ((size_t) p[2] << 8) | (size_t) p[3];
    p += 4;
    if ((size_t) (end - p) < info_len)
        return EINVAL;
    info = p;
    p += info_len;
    early_len = (size_t) (end - p);
    /* The send key must follow the records ssl encrypted itself */
    if (out_queued)
        return EAGAIN;
    if ((info_len > 0 && (info_copy = ERTS_KTLS_ALLOC(info_len)) == NULL) ||
        (early_len > 0 && (early_copy = ERTS_KTLS_ALLOC(early_len)) == NULL)) {
        if (info_copy != NULL)
            ERTS_KTLS_FREE(info_copy);
        return ENOMEM;
    }
    if ((err = erts_ktls_set_key(fd, ERTS_KTLS_TLS_TX, tx_info, tx_len)) != 0 ||
        (err = erts_ktls_set_key(fd, ERTS_KTLS_TLS_RX, rx_info, rx_len)) != 0) {
        if (info_copy != NULL)
            ERTS_KTLS_FREE(info_copy);
        if (early_copy != NULL)
            ERTS_KTLS_FREE(early_copy);
        return err;
    }
    tls->suite[0] = suite[0];
    tls->suite[1] = suite[1];
    tls->secret_len = (unsigned char) sl;
    memcpy(tls->rx_secret, rx, sl);
    memcpy(tls->tx_secret, tx, sl);
    if (info_len > 0)
        memcpy(info_copy, info, info_len);
    tls->info = info_copy;
    tls->info_len = info_len;
    if (early_len > 0)
        memcpy(early_copy, p, early_len);
    tls->early = early_copy;
    tls->early_len = early_len;
    tls->early_pos = 0;
    return 0;
}

static inline int erts_ktls_control(ErtsKtls *tls, int fd, unsigned char *buf,
                                    size_t *reply_len)
{
    size_t sl = erts_ktls_put_state(tls, buf + 2), hdr = 3 + sl, i;
    unsigned char *rec = buf + hdr, type, type2;
    ssize_t n, n2;

    n = erts_ktls_recv_record(fd, &type, rec, ERTS_KTLS_MAX_RECORD, MSG_PEEK);
    if (n < 0 && errno != EAGAIN && errno != EWOULDBLOCK)
        return errno;
    if (n == 0) {
        buf[0] = 3;
        return 0;
    }
    if (n < 0 || type == ERTS_KTLS_APPLICATION)
        return 0;
    if (type == ERTS_KTLS_HANDSHAKE) {
        for (i = 0; i + 4 <= (size_t) n;
             i += 4 + (((size_t) rec[i + 1] << 16) |
                       ((size_t) rec[i + 2] << 8) | (size_t) rec[i + 3]))
            if (rec[i] == ERTS_KTLS_KEY_UPDATE) {
                buf[0] = 2;
                buf[1] = (unsigned char) sl;
                buf[2 + sl] = type;
                *reply_len = hdr + (size_t) n;
                return 0;
            }
    }
    n2 = erts_ktls_recv_record(fd, &type2, rec, ERTS_KTLS_MAX_RECORD, 0);
    if (n2 != n || type2 != type)
        return n2 < 0 ? errno : EIO;
    memmove(buf + 2, rec, (size_t) n);
    buf[0] = 1;
    buf[1] = type;
    *reply_len = 2 + (size_t) n;
    return 0;
}

static inline int erts_ktls_update(ErtsKtls *tls, int fd,
                                   const unsigned char *p, size_t len,
                                   unsigned char *buf)
{
    const unsigned char *end = p + len, *rx_old = NULL, *rx_new = NULL,
        *rx_info = NULL, *tx_old = NULL, *tx_new = NULL, *tx_info = NULL;
    size_t sl, rx_len = 0, tx_len = 0;
    unsigned char flags, request = 0, type;
    ssize_t n;
    int err;

    if (len < 2)
        return EINVAL;
    flags = p[0];
    sl = p[1];
    p += 2;
    if (sl != tls->secret_len)
        return EINVAL;
    if (flags & ERTS_KTLS_RX) {
        if ((size_t) (end - p) < 2 * sl + 1)
            return EINVAL;
        rx_old = p;
        rx_new = p + sl;
        p += 2 * sl;
        rx_len = *p++;
        if (rx_len > ERTS_KTLS_MAX_INFO || (size_t) (end - p) < rx_len)
            return EINVAL;
        rx_info = p;
        p += rx_len;
    }
    if (flags & ERTS_KTLS_TX) {
        if ((size_t) (end - p) < 2 * sl + 1)
            return EINVAL;
        tx_old = p;
        tx_new = p + sl;
        p += 2 * sl;
        tx_len = *p++;
        if (tx_len > ERTS_KTLS_MAX_INFO || (size_t) (end - p) < tx_len + 1)
            return EINVAL;
        tx_info = p;
        p += tx_len;
        request = *p++;
    }
    if (p != end || (rx_old != NULL && tx_old != NULL &&
                     !(flags & ERTS_KTLS_MAY_OWE)))
        return EINVAL;
    if (rx_old != NULL && memcmp(rx_old, tls->rx_secret, sl) != 0) {
        buf[0] = 1;
        return 0;
    }
    if (tx_old != NULL && memcmp(tx_old, tls->tx_secret, sl) != 0) {
        buf[0] = 2;
        return 0;
    }
    if (tx_old != NULL && tls->alert != ERTS_KTLS_ALERT_NONE)
        return EPIPE;
    if (rx_old != NULL) {
        if ((err = erts_ktls_set_key(fd, ERTS_KTLS_TLS_RX, rx_info, rx_len)) != 0)
            return err;
        n = erts_ktls_recv_record(fd, &type, buf, ERTS_KTLS_MAX_RECORD, 0);
        if (n <= 0 || type != ERTS_KTLS_HANDSHAKE)
            return n < 0 ? errno : EIO;
        memcpy(tls->rx_secret, rx_new, sl);
    }
    buf[0] = 0;
    if (tx_old != NULL) {
        if (tls->owed_len != 0) {
            /* The owed KeyUpdate also answers this request */
            if (flags & ERTS_KTLS_MAY_OWE)
                return 0;
            if ((err = erts_ktls_flush_owed(tls, fd)) != 0)
                return err;
        }
        err = erts_ktls_key_update(fd, request, tx_info, tx_len);
        if ((err == EAGAIN || err == EWOULDBLOCK) && (flags & ERTS_KTLS_MAY_OWE)) {
            memcpy(tls->owed_info, tx_info, tx_len);
            tls->owed_len = (unsigned char) tx_len;
            err = 0;
        }
        if (err != 0)
            return err == EWOULDBLOCK ? EAGAIN : err;
        memcpy(tls->tx_secret, tx_new, sl);
    }
    return 0;
}

/* Handles the request in req, len bytes, on socket fd; out_queued: the
 * socket still holds output (inet_drv). The reply is at *reply, in buf
 * (ERTS_KTLS_BUF_SIZE bytes) or the stored info. Returns 0 or an errno. */
static inline int erts_ktls_request(ErtsKtls *tls, int fd, int out_queued,
                                    const unsigned char *req, size_t len,
                                    unsigned char *buf,
                                    const unsigned char **reply,
                                    size_t *reply_len)
{
    int err;

    *reply = buf;
    *reply_len = 1;
    buf[0] = 0;
    if (len < 1)
        return EINVAL;
    switch (req[0]) {
    case ERTS_KTLS_INIT:
        return erts_ktls_init(tls, fd, out_queued, req + 1, len - 1);
    case ERTS_KTLS_KEY:
        if (len < 3 || req[2] > ERTS_KTLS_MAX_INFO || len != 3 + (size_t) req[2])
            return EINVAL;
        if (req[1] == ERTS_KTLS_TLS_TX && out_queued)
            return EAGAIN;
        return erts_ktls_set_key(fd, req[1], req + 3, req[2]);
    case ERTS_KTLS_RECORD:
        if (len < 2)
            return EINVAL;
        if (out_queued)
            return EAGAIN;
        return erts_ktls_send_record(fd, req[1], req + 2, len - 2);
    default:
        break;
    }
    if (tls->secret_len == 0)
        return ENOTCONN;
    switch (req[0]) {
    case ERTS_KTLS_STATE:
        *reply_len = erts_ktls_put_state(tls, buf);
        return 0;
    case ERTS_KTLS_INFO:
        *reply = (const unsigned char *) tls->info;
        *reply_len = tls->info_len;
        return 0;
    case ERTS_KTLS_CONTROL:
        return erts_ktls_control(tls, fd, buf, reply_len);
    case ERTS_KTLS_UPDATE:
        return erts_ktls_update(tls, fd, req + 1, len - 1, buf);
    case ERTS_KTLS_ALERT:
        if (len != 3)
            return EINVAL;
        if (tls->alert == ERTS_KTLS_ALERT_NONE) {
            tls->alert_rec[0] = req[1];
            tls->alert_rec[1] = req[2];
            tls->alert = ERTS_KTLS_ALERT_QUEUED;
        }
        if (!out_queued &&
            (err = erts_ktls_flush_alert(tls, fd)) != 0 && err != EAGAIN)
            return err;
        buf[0] = tls->alert == ERTS_KTLS_ALERT_QUEUED;
        return 0;
    default:
        return EINVAL;
    }
}

#endif /* ERTS_KTLS */

#endif /* ERL_KTLS_H__ */
