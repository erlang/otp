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
 * Tty driver that reads one character at the time and provides a
 * smart line for output.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#include "erl_driver.h"
#include "win_con.h"

#define TRUE 1
#define FALSE 0

static int cols;		/* Number of columns available. */
static int rows;		/* Number of rows available. */

/* The various opcodes. */
#define OP_PUTC 0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4
#define OP_PUTC_SYNC 5

/* Control op */
#define CTRL_OP_GET_WINSIZE 100
#define CTRL_OP_GET_UNICODE_STATE 101
#define CTRL_OP_SET_UNICODE_STATE 102

static int lbuf_size = BUFSIZ;
Uint32 *lbuf;		/* The current line buffer */
int llen;		        /* The current line length */
int lpos;                       /* The current "cursor position" in the line buffer */

/* 
 * Tags used in line buffer to show that these bytes represent special characters,
 * Max unicode is 0x0010ffff, so we have lots of place for meta tags... 
 */
#define CONTROL_TAG 0x10000000U /* Control character, value in first position */
#define ESCAPED_TAG 0x01000000U /* Escaped character, value in first position */
#define TAG_MASK    0xFF000000U

#define MAXSIZE (1 << 16)

#define ISPRINT(c) (isprint(c) || (128+32 <= (c) && (c) < 256))

#define DEBUGLOG(X) /* nothing */

/*
 * XXX These are used by win_con.c (for command history).
 * Should be cleaned up.
 */


#define NL '\n'

/* Main interface functions. */
static int ttysl_init(void);
static ErlDrvData ttysl_start(ErlDrvPort, char*);
static void ttysl_stop(ErlDrvData);
static ErlDrvSSizeT ttysl_control(ErlDrvData, unsigned int,
				  char *, ErlDrvSizeT, char **, ErlDrvSizeT);
static void ttysl_from_erlang(ErlDrvData, char*, ErlDrvSizeT);
static void ttysl_from_tty(ErlDrvData, ErlDrvEvent);
static Sint16 get_sint16(char *s);

static ErlDrvPort ttysl_port;

extern ErlDrvEvent console_input_event;
extern HANDLE console_thread;

static HANDLE ttysl_in = INVALID_HANDLE_VALUE; /* Handle for console input. */
static HANDLE ttysl_out = INVALID_HANDLE_VALUE;	/* Handle for console output */

/* Functions that work on the line buffer. */
static int start_lbuf();
static int stop_lbuf();
static int put_chars();
static int move_rel();
static int ins_chars();
static int del_chars();
static int step_over_chars(int n);
static int insert_buf();
static int write_buf();
static void move_cursor(int, int);

/* Define the driver table entry. */
struct erl_drv_entry ttsl_driver_entry = {
    ttysl_init,
    ttysl_start,
    ttysl_stop,
    ttysl_from_erlang,
    ttysl_from_tty,
    NULL,
    "tty_sl",
    NULL,
    NULL,
    ttysl_control,
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};

static int utf8_mode = 0;

static int ttysl_init()
{
    lbuf = NULL;		/* For line buffer handling */
    ttysl_port = (ErlDrvPort)-1;
    return 0;
}

static ErlDrvData ttysl_start(ErlDrvPort port, char* buf)
{
    if ((int)ttysl_port != -1 || console_thread == NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }
    start_lbuf();
    utf8_mode = 1;
    driver_select(port, console_input_event, ERL_DRV_READ, 1);
    ttysl_port = port;
    return (ErlDrvData)ttysl_port;/* Nothing important to return */
}

#define DEF_HEIGHT 24
#define DEF_WIDTH 80

static void ttysl_get_window_size(Uint32 *width, Uint32 *height)
{
    *width = ConGetColumns();
    *height = ConGetRows();
}
    

static ErlDrvSSizeT ttysl_control(ErlDrvData drv_data,
			 unsigned int command,
			 char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen)
{
    char resbuff[2*sizeof(Uint32)];
    ErlDrvSizeT res_size;

    command -= ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER;
    switch (command) {
    case CTRL_OP_GET_WINSIZE:
	{
	    Uint32 w,h;
	    ttysl_get_window_size(&w,&h);
	    memcpy(resbuff,&w,sizeof(Uint32));
	    memcpy(resbuff+sizeof(Uint32),&h,sizeof(Uint32));
	    res_size = 2*sizeof(Uint32);
	}
	break;
    case CTRL_OP_GET_UNICODE_STATE:
	*resbuff = (utf8_mode) ? 1 : 0;
	res_size = 1;
	break;
    case CTRL_OP_SET_UNICODE_STATE:
	if (len != 0) {
	    int m = (int) *buf;
	    *resbuff = (utf8_mode) ? 1 : 0;
	    res_size = 1;
	    utf8_mode = (m) ? 1 : 0;
	} else {
	    return 0;
	}
	break;
    default:
	return -1;
    }
    if (rlen < res_size) {
	*rbuf = driver_alloc(res_size);
    }
    memcpy(*rbuf,resbuff,res_size);
    return res_size;
}


static void ttysl_stop(ErlDrvData ttysl_data)
{
    if ((int)ttysl_port != -1) {
        driver_select(ttysl_port, console_input_event, ERL_DRV_READ, 0);
    }

    ttysl_in = ttysl_out = INVALID_HANDLE_VALUE;
    stop_lbuf();
    ttysl_port = (ErlDrvPort)-1;
}

static int put_utf8(int ch, byte *target, int sz, int *pos)
{
    Uint x = (Uint) ch;
    if (x < 0x80) {
    if (*pos >= sz) {
	return -1;
    }
	target[(*pos)++] = (byte) x;
    }
    else if (x < 0x800) {
	if (((*pos) + 1) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((byte) (x >> 6)) | 
			    ((byte) 0xC0));
	target[(*pos)++] = (((byte) (x & 0x3F)) | 
			    ((byte) 0x80));
    } else if (x < 0x10000) {
	if ((x >= 0xD800 && x <= 0xDFFF) ||
	    (x == 0xFFFE) ||
	    (x == 0xFFFF)) { /* Invalid unicode range */
	    return -1;
	}
	if (((*pos) + 2) >= sz) {
	    return -1;
	}

	target[(*pos)++] = (((byte) (x >> 12)) | 
			    ((byte) 0xE0));
	target[(*pos)++] = ((((byte) (x >> 6)) & 0x3F)  | 
			    ((byte) 0x80));
	target[(*pos)++] = (((byte) (x & 0x3F)) | 
			    ((byte) 0x80));
    } else if (x < 0x110000) { /* Standard imposed max */
	if (((*pos) + 3) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((byte) (x >> 18)) | 
			    ((byte) 0xF0));
	target[(*pos)++] = ((((byte) (x >> 12)) & 0x3F)  | 
			    ((byte) 0x80));
	target[(*pos)++] = ((((byte) (x >> 6)) & 0x3F)  | 
			    ((byte) 0x80));
	target[(*pos)++] = (((byte) (x & 0x3F)) | 
			    ((byte) 0x80));
    } else {
	return -1;
    }
    return 0;
}
    

static int pick_utf8(byte *s, int sz, int *pos) 
{
    int size = sz - (*pos);
    byte *source;
    Uint unipoint;

    if (size > 0) {
	source = s + (*pos);
	if (((*source) & ((byte) 0x80)) == 0) {
	    unipoint = (int) *source;
	    ++(*pos);
	    return (int) unipoint;
	} else if (((*source) & ((byte) 0xE0)) == 0xC0) {
	    if (size < 2) {
		return -2;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((*source) < 0xC2) /* overlong */) {
		return -1;
	    }
	    (*pos) += 2;
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0x1F))) << 6) |
		((Uint) (source[1] & ((byte) 0x3F))); 	
	    return (int) unipoint;
	} else if (((*source) & ((byte) 0xF0)) == 0xE0) {
	    if (size < 3) {
		return -2;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xE0) && (source[1] < 0xA0)) /* overlong */ ) {
		return -1;
	    }
	    if ((((*source) & ((byte) 0xF)) == 0xD) && 
		((source[1] & 0x20) != 0)) {
		return -1;
	    }
	    if (((*source) == 0xEF) && (source[1] == 0xBF) &&
		((source[2] == 0xBE) || (source[2] == 0xBF))) {
		return -1;
	    }
	    (*pos) += 3;
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0xF))) << 12) |
		(((Uint) (source[1] & ((byte) 0x3F))) << 6) |
		((Uint) (source[2] & ((byte) 0x3F))); 	 	
	    return (int) unipoint;
	} else if (((*source) & ((byte) 0xF8)) == 0xF0) {
	    if (size < 4) {
		return -2 ;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		((source[3] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xF0) && (source[1] < 0x90)) /* overlong */) {
		return -1;
	    }
	    if ((((*source) & ((byte)0x7)) > 0x4U) ||
		((((*source) & ((byte)0x7)) == 0x4U) && 
		 ((source[1] & ((byte)0x3F)) > 0xFU))) {
		return -1;
	    }
	    (*pos) += 4;
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0x7))) << 18) |
		(((Uint) (source[1] & ((byte) 0x3F))) << 12) |
		(((Uint) (source[2] & ((byte) 0x3F))) << 6) |
		((Uint) (source[3] & ((byte) 0x3F))); 	 	
	    return (int) unipoint;
	} else {
	    return -1;
	}
    } else {
	return -1;
    }
}

static int octal_or_hex_positions(Uint c) 
{
    int x = 0;
    Uint ch = c;
    if (!ch) {
	return 1;
    }
    while(ch) {
	++x;
	ch >>= 3;
    }
    if (x <= 3) {
	return 3;
    }
    /* \x{H ...} format when larger than \777 */
    x = 0;
    ch = c;
    while(ch) {
	++x;
	ch >>= 4;
    }
    return x+3;
}

static void octal_or_hex_format(Uint ch, byte *buf, int *pos)
{
    static byte hex_chars[] = { '0','1','2','3','4','5','6','7','8','9',
				'A','B','C','D','E','F'};
    int num = octal_or_hex_positions(ch);
    if (num != 3) {
	buf[(*pos)++] = 'x';
	buf[(*pos)++] = '{';
	num -= 3;
	while(num--) {
	    buf[(*pos)++] = hex_chars[((ch >> (4*num)) & 0xFU)];
	}
	buf[(*pos)++] = '}';
    } else {
	while(num--) {
	    buf[(*pos)++] = ((byte) ((ch >> (3*num)) & 0x7U) + '0');
	}
    }	
}

/*
 * Check that there is enough room in all buffers to copy all pad chars
 * and stiff we need If not, realloc lbuf.
 */
static int check_buf_size(byte *s, int n)
{
    int pos = 0;
    int ch;
    int size = 10;

    while(pos < n) {
	/* Indata is always UTF-8 */
	if ((ch = pick_utf8(s,n,&pos)) < 0) {
	    /* XXX temporary allow invalid chars */
	    ch = (int) s[pos];
	    DEBUGLOG(("Invalid UTF8:%d",ch));
	    ++pos;
	} 
	if (utf8_mode) { /* That is, terminal is UTF8 compliant */
	    if (ch >= 128 || isprint(ch)) {
		DEBUGLOG(("Printable(UTF-8:%d):%d",pos,ch));
		size++; /* Buffer contains wide characters... */
	    } else if (ch == '\t') {
		size += 8;
	    } else {
		DEBUGLOG(("Magic(UTF-8:%d):%d",pos,ch));
		size += 2;
	    }
	} else {
	    if (ch <= 255 && isprint(ch)) {
		DEBUGLOG(("Printable:%d",ch));
		size++;
	    } else if (ch == '\t') 
		size += 8;
	    else if (ch >= 128) {
		DEBUGLOG(("Non printable:%d",ch));
		size += (octal_or_hex_positions(ch) + 1);
	    }
	    else {
		DEBUGLOG(("Magic:%d",ch));
		size += 2;
	    }
	}
    }
		
    if (size + lpos >= lbuf_size) {

	lbuf_size = size + lpos + BUFSIZ;
	if ((lbuf = driver_realloc(lbuf, lbuf_size * sizeof(Uint32))) == NULL) {
	    driver_failure(ttysl_port, -1);
	    return(0);
	}
    }
    return(1);
}


static void ttysl_from_erlang(ErlDrvData ttysl_data, char* buf, ErlDrvSizeT count)
{
    if (lpos > MAXSIZE) 
	put_chars((byte*)"\n", 1);

    switch (buf[0]) {
    case OP_PUTC:
    case OP_PUTC_SYNC:
	DEBUGLOG(("OP: Putc(%I64u)",(unsigned long long)count-1));
	if (check_buf_size((byte*)buf+1, count-1) == 0)
	    return; 
	put_chars((byte*)buf+1, count-1);
	break;
    case OP_MOVE:
	move_rel(get_sint16(buf+1));
	break;
    case OP_INSC:
	if (check_buf_size((byte*)buf+1, count-1) == 0)
	    return;
	ins_chars((byte*)buf+1, count-1);
	break;
    case OP_DELC:
	del_chars(get_sint16(buf+1));
	break;
    case OP_BEEP:
	ConBeep();
	break;
    default:
	/* Unknown op, just ignore. */
	break;
    }

    if (buf[0] == OP_PUTC_SYNC) {
        /* On windows we do a blocking write to the tty so we just
           send the ack immidiately. If at some point in the future
           someone has a problem with tty output being blocking
           this has to be changed. */
        ErlDrvTermData spec[] = {
            ERL_DRV_PORT, driver_mk_port(ttysl_port),
            ERL_DRV_ATOM, driver_mk_atom("ok"),
            ERL_DRV_TUPLE, 2
        };
        erl_drv_output_term(driver_mk_port(ttysl_port), spec,
                            sizeof(spec) / sizeof(spec[0]));
    }
    return;
}

extern int read_inbuf(char *data, int n);

static void ttysl_from_tty(ErlDrvData ttysl_data, ErlDrvEvent fd)
{
   Uint32 inbuf[64];
   byte t[1024];
   int i,pos,tpos;

   i = ConReadInput(inbuf,1);

   pos = 0;
   tpos = 0;

   while (pos < i) {
       while (tpos < 1020 && pos < i) { /* Max 4 bytes for UTF8 */
	   put_utf8((int) inbuf[pos++], t, 1024, &tpos);
       }
       driver_output(ttysl_port, (char *) t, tpos);
       tpos = 0;
   }
}

/*
 * Gets signed 16 bit integer from binary buffer.
 */
static Sint16
get_sint16(char *s)
{
    return ((*s << 8) | ((byte*)s)[1]);
}


static int start_lbuf(void)
{
    if (!lbuf && !(lbuf = ( Uint32*) driver_alloc(lbuf_size * sizeof(Uint32))))
      return FALSE;
    llen = 0;
    lpos = 0;
    return TRUE;
}

static int stop_lbuf(void)
{
    if (lbuf) {
	driver_free(lbuf);
	lbuf = NULL;
    }
    llen = 0; /* To avoid access error in win_con:AddToCmdHistory during exit*/
    return TRUE;
}

/* Put l bytes (in UTF8) from s into the buffer and output them. */
static int put_chars(byte *s, int l)
{
    int n;

    n = insert_buf(s, l);
    if (n > 0)
      write_buf(lbuf + lpos - n, n);
    if (lpos > llen)
      llen = lpos;
    return TRUE;
}

/*
 * Move the current postition forwards or backwards within the current
 * line. We know about padding.
 */
static int move_rel(int n)
{
    int npos;			/* The new position */

    /* Step forwards or backwards over the buffer. */
    npos = step_over_chars(n);

    /* Calculate move, updates pointers and move the cursor. */
    move_cursor(lpos, npos);
    lpos = npos;
    return TRUE;
}

/* Insert characters into the buffer at the current position. */
static int ins_chars(byte *s, int l)
{
    int n, tl;
    Uint32 *tbuf = NULL;    /* Suppress warning about use-before-set */

    /* Move tail of buffer to make space. */
    if ((tl = llen - lpos) > 0) {
	if ((tbuf = driver_alloc(tl * sizeof(Uint32))) == NULL)
	    return FALSE;
	memcpy(tbuf, lbuf + lpos, tl * sizeof(Uint32));
    }
    n = insert_buf(s, l);
    if (tl > 0) {
	memcpy(lbuf + lpos, tbuf, tl * sizeof(Uint32));
	driver_free(tbuf);
    }
    llen += n;
    write_buf(lbuf + (lpos - n), llen - (lpos - n));
    move_cursor(llen, lpos);
    return TRUE;
}

/*
 * Delete characters in the buffer. Can delete characters before (n < 0)
 * and after (n > 0) the current position. Cursor left at beginning of
 * deleted block.
 */
static int del_chars(int n)
{
    int i, l, r;
    int pos;

    /*update_cols();*/

    /* Step forward or backwards over n logical characters. */
    pos = step_over_chars(n);

    if (pos > lpos) {
	l = pos - lpos;		/* Buffer characters to delete */
	r = llen - lpos - l;	/* Characters after deleted */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memcpy(lbuf + lpos, lbuf + pos, r * sizeof(Uint32));
	llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(lbuf + lpos, r);
	for (i = l ; i > 0; --i)
	  ConPutChar(' ');
	move_cursor(llen + l, lpos);
    }
    else if (pos < lpos) {
	l = lpos - pos;		/* Buffer characters */
	r = llen - lpos;	/* Characters after deleted */
	move_cursor(lpos, lpos-l);	/* Move back */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memcpy(lbuf + pos, lbuf + lpos, r * sizeof(Uint32));
	lpos -= l;
	llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(lbuf + lpos, r);
	for (i = l ; i > 0; --i)
	  ConPutChar(' ');
	move_cursor(llen + l, lpos);
    }
    return TRUE;
}


/* Step over n logical characters, check for overflow. */
static int step_over_chars(int n)
{
    Uint32 *c, *beg, *end;

    beg = lbuf;
    end = lbuf + llen;
    c = lbuf + lpos;
    for ( ; n > 0 && c < end; --n) {
	c++;
	while (c < end && (*c & TAG_MASK) && ((*c & ~TAG_MASK) == 0))
	    c++;
    }
    for ( ; n < 0 && c > beg; n++) {
	--c;
	while (c > beg && (*c & TAG_MASK) && ((*c & ~TAG_MASK) == 0))
	    --c;
    }
    return c - lbuf;
}

static int insert_buf(byte *s, int n)
{
    int pos = 0;
    int buffpos = lpos;
    int ch;

    while (pos < n) {
	if ((ch = pick_utf8(s,n,&pos)) < 0) {
	    /* XXX temporary allow invalid chars */
	    ch = (int) s[pos];
	    DEBUGLOG(("insert_buf: Invalid UTF8:%d",ch));
	    ++pos;
	}
	if ((utf8_mode && (ch >= 128 || isprint(ch))) || (ch <= 255 && isprint(ch))) {
	    DEBUGLOG(("insert_buf: Printable(UTF-8):%d",ch));
	    lbuf[lpos++] = (Uint32) ch;
	} else if (ch >= 128) { /* not utf8 mode */
	    int nc = octal_or_hex_positions(ch);
	    lbuf[lpos++] = ((Uint32) ch) | ESCAPED_TAG;
	    while (nc--) {
		lbuf[lpos++] = ESCAPED_TAG;
	    }
	} else if (ch == '\t') {
	    do {
		lbuf[lpos++] = (CONTROL_TAG | ((Uint32) ch));
		ch = 0;
	    } while (lpos % 8);
	} else if (ch == '\n' || ch == '\r') {
	    write_buf(lbuf + buffpos, lpos - buffpos);
	    ConPutChar('\r');
	    if (ch == '\n')
		ConPutChar('\n');
	    if (llen > lpos) {
		memcpy(lbuf, lbuf + lpos, llen - lpos);
	    }
	    llen -= lpos;
	    lpos = buffpos = 0;
	} else {
	    DEBUGLOG(("insert_buf: Magic(UTF-8):%d",ch));
	    lbuf[lpos++] = ch | CONTROL_TAG;
	    lbuf[lpos++] = CONTROL_TAG;
	}
    }
    return lpos - buffpos; /* characters "written" into 
			      current buffer (may be less due to newline) */
}
static int write_buf(Uint32 *s, int n)
{
    int i;

    /*update_cols();*/

    while (n > 0) {
	if (!(*s & TAG_MASK) ) {
	    ConPutChar(*s);
	    --n;
	    ++s;
	}
	else if (*s == (CONTROL_TAG | ((Uint32) '\t'))) {
	    ConPutChar(' ');
	    --n; s++;
	    while (n > 0 && *s == CONTROL_TAG) {
		ConPutChar(' ');
		--n; s++;
	    }
	} else if (*s & CONTROL_TAG) {
	    ConPutChar('^');
	    ConPutChar((*s == 0177) ? '?' : *s | 0x40);
	    n -= 2;
	    s += 2;
	} else if (*s & ESCAPED_TAG) {
	    Uint32 ch = *s & ~(TAG_MASK);
	    byte *octbuff;
	    byte octtmp[256];
	    int octbytes;
	    DEBUGLOG(("Escaped: %d", ch));
	    octbytes = octal_or_hex_positions(ch);
	    if (octbytes > 256) {
		octbuff = driver_alloc(octbytes);
	    } else {
		octbuff = octtmp;
	    }
	    octbytes = 0;
	    octal_or_hex_format(ch, octbuff, &octbytes);
	     DEBUGLOG(("octbytes: %d", octbytes));
	    ConPutChar('\\');
	    for (i = 0; i < octbytes; ++i) {
		ConPutChar(octbuff[i]);
	    }
	    n -= octbytes+1;
	    s += octbytes+1;
	    if (octbuff != octtmp) {
		driver_free(octbuff);
	    }
	} else {
	    DEBUGLOG(("Very unexpected character %d",(int) *s));
	    ++n;
	    --s;
	}
    }
    return TRUE;
}


static void
move_cursor(int from, int to)
{
    ConSetCursor(from,to);
}
