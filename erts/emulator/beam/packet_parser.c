/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

/* A protocol decoder. Simple packet length extraction as well as packet
 * body parsing with protocol specific callback interfaces (http and ssl).
 *
 * Code ripped out from inet_drv.c to also be used by BIF decode_packet.
 */
#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif

#include "packet_parser.h"

#include <ctype.h>
#include "sys.h"

/* #define INET_DRV_DEBUG 1 */
#ifdef INET_DRV_DEBUG
#   define DEBUG 1
#   undef DEBUGF
#   define DEBUGF(X) printf X
#endif

#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
                      (((unsigned char*) (s))[1] << 8)  | \
                      (((unsigned char*) (s))[2]))

#define get_little_int32(s) ((((unsigned char*) (s))[3] << 24) | \
                             (((unsigned char*) (s))[2] << 16)  | \
                             (((unsigned char*) (s))[1] << 8) | \
                             (((unsigned char*) (s))[0]))

#if !defined(__WIN32__) && !defined(HAVE_STRNCASECMP)
#define STRNCASECMP my_strncasecmp

static int my_strncasecmp(const char *s1, const char *s2, size_t n)
{
    int i;

    for (i=0;i<n-1 && s1[i] && s2[i] && toupper(s1[i]) == toupper(s2[i]);++i)
	;
    return (toupper(s1[i]) - toupper(s2[i]));
}
	

#else
#define  STRNCASECMP strncasecmp
#endif


#define HTTP_HDR_HASH_SIZE  53
#define HTTP_METH_HASH_SIZE 13
#define HTTP_MAX_NAME_LEN 50

static char tspecial[128];

static const char* http_hdr_strings[] = {
    "Cache-Control",
    "Connection",
    "Date",
    "Pragma",
    "Transfer-Encoding",
    "Upgrade",
    "Via",
    "Accept",
    "Accept-Charset",
    "Accept-Encoding",
    "Accept-Language",
    "Authorization",
    "From",
    "Host",
    "If-Modified-Since",
    "If-Match",
    "If-None-Match",
    "If-Range",
    "If-Unmodified-Since",
    "Max-Forwards",
    "Proxy-Authorization",
    "Range",
    "Referer",
    "User-Agent",
    "Age",
    "Location",
    "Proxy-Authenticate",
    "Public",
    "Retry-After",
    "Server",
    "Vary",
    "Warning",
    "Www-Authenticate",
    "Allow",
    "Content-Base",
    "Content-Encoding",
    "Content-Language",
    "Content-Length",
    "Content-Location",
    "Content-Md5",
    "Content-Range",
    "Content-Type",
    "Etag",
    "Expires",
    "Last-Modified",
    "Accept-Ranges",
    "Set-Cookie",
    "Set-Cookie2",
    "X-Forwarded-For",
    "Cookie",
    "Keep-Alive",
    "Proxy-Connection",
    NULL
};


static const char* http_meth_strings[] = {
    "OPTIONS",
    "GET",
    "HEAD",
    "POST",
    "PUT",
    "DELETE",
    "TRACE",
    NULL
};

static http_atom_t http_hdr_table[sizeof(http_hdr_strings)/sizeof(char*)];
static http_atom_t http_meth_table[sizeof(http_meth_strings)/sizeof(char*)];

static http_atom_t* http_hdr_hash[HTTP_HDR_HASH_SIZE];
static http_atom_t* http_meth_hash[HTTP_METH_HASH_SIZE];

#define CRNL(ptr) (((ptr)[0] == '\r') && ((ptr)[1] == '\n'))
#define NL(ptr)   ((ptr)[0] == '\n')
#define SP(ptr)   (((ptr)[0] == ' ') || ((ptr)[0] == '\t'))
#define is_tspecial(x) ((((x) > 32) && ((x) < 128)) ? tspecial[(x)] : 1)

#define hash_update(h,c) do { \
        unsigned long __g; \
        (h) = ((h) << 4) + (c); \
        if ((__g = (h) & 0xf0000000)) { \
            (h) ^= (__g >> 24); \
            (h) ^= __g; \
        } \
    } while(0)

static void http_hash_insert(const char* name, http_atom_t* entry,
                             http_atom_t** hash, int hsize)
{
    unsigned long h = 0;
    const unsigned char* ptr = (const unsigned char*) name;
    int ix;
    int len = 0;

    while (*ptr != '\0') {
        hash_update(h, *ptr);
        ptr++;
        len++;
    }
    ix = h % hsize;

    entry->next = hash[ix];
    entry->h    = h;
    entry->name = name;
    entry->len  = len;
    entry->atom = driver_mk_atom((char*)name);

    hash[ix] = entry;
}


static int http_init(void)
{
    int i;
    unsigned char* ptr;

    for (i = 0; i < 33; i++)
        tspecial[i] = 1;
    for (i = 33; i < 127; i++)
        tspecial[i] = 0;
    for (ptr = (unsigned char*)"()<>@,;:\\\"/[]?={} \t"; *ptr != '\0'; ptr++)
        tspecial[*ptr] = 1;

    for (i = 0; i < HTTP_HDR_HASH_SIZE; i++)
        http_hdr_hash[i] = NULL;
    for (i = 0; http_hdr_strings[i] != NULL; i++) {
        ASSERT(strlen(http_hdr_strings[i]) <= HTTP_MAX_NAME_LEN);
        http_hdr_table[i].index = i;
        http_hash_insert(http_hdr_strings[i], 
                         &http_hdr_table[i], 
                         http_hdr_hash, HTTP_HDR_HASH_SIZE);
    }

    for (i = 0; i < HTTP_METH_HASH_SIZE; i++)
        http_meth_hash[i] = NULL;
    for (i = 0; http_meth_strings[i] != NULL; i++) {
        http_meth_table[i].index = i;
        http_hash_insert(http_meth_strings[i],
                         &http_meth_table[i], 
                         http_meth_hash, HTTP_METH_HASH_SIZE);
    }
    return 0;
}


#define CDR_MAGIC "GIOP"

struct cdr_head {
    unsigned char magic[4];        /* 4 bytes must be 'GIOP' */
    unsigned char major;           /* major version */ 
    unsigned char minor;           /* minor version */
    unsigned char flags;           /* bit 0: 0 == big endian, 1 == little endian 
                                      bit 1: 1 == more fragments follow */
    unsigned char message_type;    /* message type ... */
    unsigned char message_size[4]; /* size in (flags bit 0 byte order) */
};

#define TPKT_VRSN 3

struct tpkt_head {
    unsigned char vrsn;             /* contains TPKT_VRSN */
    unsigned char reserved;
    unsigned char packet_length[2]; /* size incl header, big-endian (?) */
};

void packet_parser_init()
{
    static int done = 0;
    if (!done) {
        done = 1;
        http_init();
    }
}

/* Return > 0 Total packet length.in bytes
 *        = 0 Length unknown, need more data.
 *        < 0 Error, invalid format.
 */
int packet_get_length(enum PacketParseType htype,
                      const char* ptr, unsigned n, /* Bytes read so far */
                      unsigned max_plen,     /* Max packet length, 0=no limit */
                      unsigned trunc_len,    /* Truncate (lines) if longer, 0=no limit */
                      char     delimiter,    /* Line delimiting character */
                      int*     statep)       /* Protocol specific state */
{
    unsigned hlen, plen;

    switch (htype) {
    case TCP_PB_RAW:
        if (n == 0) goto more;
        else {
            DEBUGF((" => nothing remain packet=%d\r\n", n));        
            return n;
        }

    case TCP_PB_1:
        /* TCP_PB_1:    [L0 | Data] */
        hlen = 1;
        if (n < hlen) goto more;
        plen = get_int8(ptr);
        goto remain;

    case TCP_PB_2:
        /* TCP_PB_2:    [L1,L0 | Data] */
        hlen = 2;
        if (n < hlen) goto more;
        plen = get_int16(ptr);
        goto remain;

    case TCP_PB_4:
        /* TCP_PB_4:    [L3,L2,L1,L0 | Data] */
        hlen = 4;
        if (n < hlen) goto more;
        plen = get_int32(ptr);
        goto remain;

    case TCP_PB_RM:
        /* TCP_PB_RM:    [L3,L2,L1,L0 | Data] 
        ** where MSB (bit) is used to signal end of record
        */
        hlen = 4;
        if (n < hlen) goto more;
        plen = get_int32(ptr) & 0x7fffffff;
        goto remain;

    case TCP_PB_LINE_LF: {
        /* TCP_PB_LINE_LF:  [Data ... Delimiter]  */
        const char* ptr2;
        if ((ptr2 = memchr(ptr, delimiter, n)) == NULL) {
            if (n > max_plen && max_plen != 0) { /* packet full */
                DEBUGF((" => packet full (no NL)=%d\r\n", n));
                goto error;
            }
            else if (n >= trunc_len && trunc_len!=0) { /* buffer full */
                DEBUGF((" => line buffer full (no NL)=%d\r\n", n));
                return trunc_len;
            }
            goto more;
        }
        else {
            int len = (ptr2 - ptr) + 1; /* including newline */
            if (len > max_plen && max_plen!=0) {
                DEBUGF((" => packet_size %d exceeded\r\n", max_plen));
                goto error;
            }
            if (len > trunc_len && trunc_len!=0) {
                DEBUGF((" => truncated line=%d\r\n", trunc_len));
                return trunc_len;
            }
            DEBUGF((" => nothing remain packet=%d\r\n", len));
            return len;
        }
    }

    case TCP_PB_ASN1: {
        /* TCP_PB_ASN1: handles long (4 bytes) or short length format */
        const char* tptr = ptr;
        int length;
        int nn = n;
        
        if (n < 2) goto more;
        nn--;
        if ((*tptr++ & 0x1f) == 0x1f) { /* Long tag format */
            while (nn && ((*tptr & 0x80) == 0x80)) {
                tptr++;
                nn--;
            }
            if (nn < 2) goto more;
            tptr++;
            nn--;
        }
        
        /* tptr now point to length field and nn characters remain */
        length = *tptr & 0x7f;
        if ((*tptr & 0x80) == 0x80) {   /* Long length format */
            tptr++;
            nn--;
            if (nn < length) goto more;
            switch (length) {
            case 0: plen = 0; break;
            case 1: plen = get_int8(tptr);  tptr += 1; break;
            case 2: plen = get_int16(tptr); tptr += 2; break;
            case 3: plen = get_int24(tptr); tptr += 3; break;
            case 4: plen = get_int32(tptr); tptr += 4; break;
            default: goto error; /* error */
            }
        }
        else {
            tptr++;
            plen = length;
        }
        hlen = (tptr-ptr);
        goto remain;
    }    
    
    case TCP_PB_CDR: {
        const struct cdr_head* hp;
        hlen = sizeof(struct cdr_head);
        if (n < hlen) goto more;
        hp = (struct cdr_head*) ptr;
        if (sys_memcmp(hp->magic, CDR_MAGIC, 4) != 0)
            goto error;
        if (hp->flags & 0x01) /* Byte ordering flag */
            plen = get_little_int32(hp->message_size);
        else
            plen = get_int32(hp->message_size);
        goto remain;
    }
    
    case TCP_PB_FCGI: {
        const struct fcgi_head* hp;
        hlen = sizeof(struct fcgi_head);
        if (n < hlen) goto more;
        hp = (struct fcgi_head*) ptr;
        if (hp->version != FCGI_VERSION_1)
                goto error;
        plen = ((hp->contentLengthB1 << 8) | hp->contentLengthB0)
               + hp->paddingLength;
        goto remain;
    }
    case TCP_PB_HTTPH:
    case TCP_PB_HTTPH_BIN:
        *statep = !0;
    case TCP_PB_HTTP:
    case TCP_PB_HTTP_BIN:
        /* TCP_PB_HTTP:  data \r\n(SP data\r\n)*  */
        plen = n;
        if (((plen == 1) && NL(ptr)) || ((plen == 2) && CRNL(ptr)))
            goto done;
        else {
            const char* ptr1 = ptr;
            int   len = plen;
            
	    if (!max_plen) {
		/* This is for backward compatibility with old user of decode_packet
		 * that might use option 'line_length' to limit accepted length of
		 * http lines.
		 */
		max_plen = trunc_len;
	    }

            while (1) {
                const char* ptr2 = memchr(ptr1, '\n', len);
                
                if (ptr2 == NULL) {
                    if (max_plen != 0) {
                        if (n >= max_plen) /* packet full */
                            goto error;
                    }
                    goto more;
                }
                else {
                    plen = (ptr2 - ptr) + 1;

                    if (*statep == 0) {
                        if (max_plen != 0 && plen > max_plen)
                            goto error;
                        goto done;
                    }

                    if (plen < n) {
                        if (SP(ptr2+1) && plen>2) {
                            /* header field value continue on next line */
                            ptr1 = ptr2+1;
                            len = n - plen;
                        }
                        else {
                            if (max_plen != 0 && plen > max_plen)
                                goto error;
                            goto done;
                        }
                    }
                    else {
                        if (max_plen != 0 && plen > max_plen)
                            goto error;
                        goto more;
                    }
                }
            }
        }
    case TCP_PB_TPKT: {
        const struct tpkt_head* hp;
        hlen = sizeof(struct tpkt_head);
        if (n < hlen)
            goto more;
        hp = (struct tpkt_head*) ptr;
        if (hp->vrsn == TPKT_VRSN) {
            plen = get_int16(hp->packet_length) - hlen;
        } else {
            goto error;
	}
        goto remain;
    }
    
    case TCP_PB_SSL_TLS:
        hlen = 5;
        if (n < hlen) goto more;        
        if ((ptr[0] & 0x80) && ptr[2] == 1) {
            /* Ssl-v2 Client hello <<1:1, Len:15, 1:8, Version:16>>  */
            plen = (get_int16(&ptr[0]) & 0x7fff) - 3;
        } 
        else {
            /* <<ContentType:8, Version:16, Length:16>> */
            plen = get_int16(&ptr[3]);
        }
        goto remain;
    
    default:
        DEBUGF((" => case error\r\n"));
        return -1;
    }

more:
    return 0;

remain:
    {
        int tlen = hlen + plen;
	if ((max_plen != 0 && plen > max_plen)
	    || tlen < (int)hlen) { /* wrap-around protection */
	    return -1;
	}
	return tlen;
    }		

done:
    return plen;

error:
    return -1;
}


static http_atom_t* http_hash_lookup(const char* name, int len,
                                     unsigned long h,
                                     http_atom_t** hash, int hsize)
{
    int ix = h % hsize;
    http_atom_t* ap = hash[ix];

    while (ap != NULL) {
        if ((ap->h == h) && (ap->len == len) && 
            (strncmp(ap->name, name, len) == 0))
            return ap;
        ap = ap->next;
    }
    return NULL;
}

static void
http_parse_absoluteURI(PacketHttpURI* uri, const char* uri_ptr, int uri_len)
{
    const char* p;
    
    if ((p = memchr(uri_ptr, '/', uri_len)) == NULL) {
        /* host [":" port] */
        uri->s2_ptr = "/";
        uri->s2_len = 1;
    }
    else {
        int n = (p - uri_ptr);
        uri->s2_ptr = p;
        uri->s2_len = uri_len - n;
        uri_len = n;
    }

    uri->s1_ptr = uri_ptr;
    uri->port = 0; /* undefined */
    /* host[:port]  */
    if ((p = memchr(uri_ptr, ':', uri_len)) == NULL) {
        uri->s1_len = uri_len;
    }
    else {
        int n = (p - uri_ptr);
        int port = 0;        
        uri->s1_len = n;
        n = uri_len - (n+1);
        p++;
        while(n && isdigit((int) *p)) {
            port = port*10 + (*p - '0');
            n--;
            p++;
        }
        if (n==0 && port!=0)
            uri->port = port;
  }
}

/*
** Handle URI syntax:
**
**  Request-URI    = "*" | absoluteURI | abs_path
**  absoluteURI    = scheme ":" *( uchar | reserved )
**  net_path       = "//" net_loc [ abs_path ]
**  abs_path       = "/" rel_path
**  rel_path       = [ path ] [ ";" params ] [ "?" query ]
**  path           = fsegment *( "/" segment )
**  fsegment       = 1*pchar
**  segment        = *pchar
**  params         = param *( ";" param )
**  param          = *( pchar | "/" )
**  query          = *( uchar | reserved )
**
**  http_URL       = "http:" "//" host [ ":" port ] [ abs_path ]
**
**  host           = <A legal Internet host domain name
**                   or IP address (in dotted-decimal form),
**                   as defined by Section 2.1 of RFC 1123>
**  port           = *DIGIT
**
**  {absoluteURI, <scheme>, <host>, <port>, <path+params+query>}
**       when <scheme> = http | https
**  {scheme, <scheme>, <chars>}
**       wheb <scheme> is something else then http or https
**  {abs_path,  <path>}
**
**  <string>  (unknown form)
**
*/
static void http_parse_uri(PacketHttpURI* uri, const char* uri_ptr, int uri_len)
{
    if ((uri_len == 1) && (uri_ptr[0] == '*'))
        uri->type = URI_STAR;
    else if ((uri_len <= 1) || (uri_ptr[0] == '/')) {
        uri->type = URI_ABS_PATH;
        uri->s1_ptr = uri_ptr;
        uri->s1_len = uri_len;
    }
    else if ((uri_len>=7) && (STRNCASECMP(uri_ptr, "http://", 7) == 0)) {
        uri_len -= 7;
        uri_ptr += 7;
        uri->type = URI_HTTP;
        http_parse_absoluteURI(uri, uri_ptr, uri_len);
    }
    else if ((uri_len>=8) && (STRNCASECMP(uri_ptr, "https://", 8) == 0)) {
        uri_len -= 8;
        uri_ptr += 8;    
        uri->type = URI_HTTPS;
        http_parse_absoluteURI(uri, uri_ptr, uri_len);
    }
    else {
        char* ptr;
        if ((ptr = memchr(uri_ptr, ':', uri_len)) == NULL) {
            uri->type = URI_STRING;
            uri->s1_ptr = uri_ptr;
            uri->s1_len = uri_len;
        }        
        else {
            int slen = ptr - uri_ptr;
            uri->type = URI_SCHEME;
            uri->s1_ptr = uri_ptr;
            uri->s1_len = slen;
            uri->s2_ptr = uri_ptr + (slen+1);
            uri->s2_len = uri_len - (slen+1);
        }
    }
}

/*
** parse http message:
**  http_eoh                          - end of headers
**  {http_header,   Key, Value}       - Key = atom() | string()
**  {http_request,  Method,Url,Version}
**  {http_response, Version, Status, Message}
**  {http_error,    Error-Line}
*/
int packet_parse_http(const char* buf, int len, int* statep,
                      PacketCallbacks* pcb, void* arg)
{
    const char* ptr = buf;
    const char* p0;
    int n = len;

    /* remove trailing CRNL (accept NL as well) */
    if ((n >= 2) && (buf[n-2] == '\r'))
        n -= 2;
    else if ((n >= 1) && (buf[n-1] == '\n'))
        n -= 1;

    if (*statep == 0) {
        /* start-line = Request-Line | Status-Line */

        if (n >= 5 && (strncmp(buf, "HTTP/", 5) == 0)) {
            int major  = 0;
            int minor  = 0;
            int status = 0;
            /* Status-Line = HTTP-Version SP
             *              Status-Code SP Reason-Phrase
             *              CRNL
             * HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
             */
            ptr += 5;
            n -= 5;
            p0 = ptr;
            while (n && isdigit((int) *ptr)) {
                major = 10*major + (*ptr - '0');
                ptr++;
                n--;
            }
            if (ptr==p0 || !n || (*ptr != '.'))
                return -1;
            ptr++;
            n--;
            p0 = ptr;
            while (n && isdigit((int) *ptr)) {
                minor = 10*minor + (*ptr - '0');
                ptr++;
                n--;
            }
            if (ptr==p0) return -1;
            p0 = ptr;
            while (n && SP(ptr)) {
                ptr++; n--;
            }
            if (ptr==p0) return -1;
            
            while (n && isdigit((int) *ptr)) {
                status = 10*status + (*ptr - '0');
                ptr++;
                n--;
            }
            p0 = ptr;
            while (n && SP(ptr)) {
                ptr++; n--;
            }
            if (ptr==p0 && n>0) return -1;
            
            /* NOTE: the syntax allows empty reason phrases */
            (*statep) = !0;
            
            return pcb->http_response(arg, major, minor, status,
                                      ptr, n);
        }
        else {
            /* Request-Line = Method SP Request-URI SP HTTP-Version CRLF */
            http_atom_t* meth;
            const char* meth_ptr = buf;
            int         meth_len;
            PacketHttpURI uri;
            const char*   uri_ptr;
            int           uri_len;
            int major  = 0;
            int minor  = 0;
            unsigned long h = 0;

            while (n && !is_tspecial((unsigned char)*ptr)) {
                hash_update(h, (int)*ptr);
                ptr++;
                n--;
            }
            meth_len = ptr - meth_ptr;
            if (n == 0 || meth_len == 0 || !SP(ptr)) return -1;

            meth = http_hash_lookup(meth_ptr, meth_len, h,
                                    http_meth_hash, HTTP_METH_HASH_SIZE);

            while (n && SP(ptr)) {
                ptr++; n--;
            }
            uri_ptr = ptr;
            while (n && !SP(ptr)) {
                ptr++; n--;
            }
            if ((uri_len = (ptr - uri_ptr)) == 0)
                return -1;
            while (n && SP(ptr)) {
                ptr++; n--;
            }
            if (n == 0) {
                (*statep) = !0;
                http_parse_uri(&uri, uri_ptr, uri_len);
                return pcb->http_request(arg, meth, meth_ptr, meth_len,
                                         &uri, 0, 9);
            }
            if (n < 8)
                return -1;
            if (strncmp(ptr, "HTTP/", 5) != 0)
                return -1;
            ptr += 5;
            n   -= 5;

            p0 = ptr;
            while (n && isdigit((int) *ptr)) {
                major = 10*major + (*ptr - '0');
                ptr++;
                n--;
            }            
            if (ptr==p0 || !n || (*ptr != '.'))
                return -1;
            ptr++;
            n--;
            p0 = ptr;
            while (n && isdigit((int) *ptr)) {
                minor = 10*minor + (*ptr - '0');
                ptr++;
                n--;
            }
            if (ptr==p0) return -1;

            (*statep) = !0;
            http_parse_uri(&uri, uri_ptr, uri_len);
            return pcb->http_request(arg, meth, meth_ptr, meth_len,
                                     &uri, major, minor);
        }
    }
    else {
        int up = 1;      /* make next char uppercase */
        http_atom_t* name;
        char name_buf[HTTP_MAX_NAME_LEN];
        const char* name_ptr = name_buf;
        int  name_len;
        unsigned long h;

        if (n == 0) {
            /* end of headers */
            *statep = 0;  /* reset state (for next request) */
            return pcb->http_eoh(arg);
        }
        h = 0;
        name_len = 0;
        while (!is_tspecial((unsigned char)*ptr)) {
            if (name_len < HTTP_MAX_NAME_LEN) {
                int c = *ptr;
                if (up) {
                    if (islower(c)) {
                        c = toupper(c);
                    }
                    up = 0;
                }
                else {
                    if (isupper(c))
                        c = tolower(c);
                    else if (c == '-')
                        up = 1;
                }                            
                name_buf[name_len] = c;
                hash_update(h, c);
            }
            name_len++;
            ptr++;
            if (--n == 0) return -1;
        }
        while (n && SP(ptr)) { /* Skip white space before ':' */
            ptr++; n--;
        } 
        if (*ptr != ':') {
            return -1;
        }
        if (name_len <= HTTP_MAX_NAME_LEN) {
            name = http_hash_lookup(name_buf, name_len, h,
                                    http_hdr_hash, HTTP_HDR_HASH_SIZE);
        } 
        else {
            /* Is it ok to return original name without case adjustments? */
            name_ptr = buf;
            name = NULL;
        }
        ptr++;
        n--;
        /* Skip white space after ':' */
        while (n && SP(ptr)) {
            ptr++; n--;
        }
        return pcb->http_header(arg, name, name_ptr, name_len,
                                ptr, n);
    }
    return -1;
}   

int packet_parse_ssl(const char* buf, int len,
                     PacketCallbacks* pcb, void* arg)
{
    /* Check for ssl-v2 client hello */
    if ((buf[0] & 0x80) && buf[2] == 1) {
        unsigned major = (unsigned char) buf[3];
        unsigned minor = (unsigned char) buf[4];
        char prefix[4];
        /* <<1:8,Length:24,Data/binary>> */
        prefix[0] = 1;
        put_int24(len-3,&prefix[1]);
        return pcb->ssl_tls(arg, 22, major, minor, buf+3, len-3, prefix, sizeof(prefix));
    } 
    else {
        /* ContentType (1 byte), ProtocolVersion (2 bytes), Length (2 bytes big-endian) */
        unsigned type  = (unsigned char) buf[0];
        unsigned major = (unsigned char) buf[1];
        unsigned minor = (unsigned char) buf[2];
        return pcb->ssl_tls(arg, type, major, minor, buf+5, len-5, NULL, 0);
    }
}

