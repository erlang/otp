/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
 * Tty driver that reads one character at the time and provides a
 * smart line for output.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_driver.h"

static int ttysl_init(void);
static ErlDrvData ttysl_start(ErlDrvPort, char*);

#ifdef HAVE_TERMCAP  /* else make an empty driver that can not be opened */

#include "sys.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <locale.h>
#include <unistd.h>
#include <termios.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#if !defined(HAVE_SETLOCALE) || !defined(HAVE_NL_LANGINFO) || !defined(HAVE_LANGINFO_H)
#define PRIMITIVE_UTF8_CHECK 1
#else
#include <langinfo.h>
#endif

#define TRUE 1
#define FALSE 0

/* Termcap functions. */
int tgetent(char* bp, char *name);
int tgetnum(char* cap);
int tgetflag(char* cap);
char *tgetstr(char* cap, char** buf);
char *tgoto(char* cm, int col, int line);
int tputs(char* cp, int affcnt, int (*outc)(int c));

/* Terminal capabilites in which we are interested. */
static char *capbuf;
static char *up, *down, *left, *right;
static int cols, xn;
static volatile int cols_needs_update = FALSE;

/* The various opcodes. */
#define OP_PUTC 0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4
/* Control op */
#define CTRL_OP_GET_WINSIZE 100
#define CTRL_OP_GET_UNICODE_STATE 101
#define CTRL_OP_SET_UNICODE_STATE 102



static int lbuf_size = BUFSIZ;
static Uint32 *lbuf;		/* The current line buffer */
static int llen;		/* The current line length */
static int lpos;                /* The current "cursor position" in the line buffer */

/* 
 * Tags used in line buffer to show that these bytes represent special characters,
 * Max unicode is 0x0010ffff, so we have lots of place for meta tags... 
 */
#define CONTROL_TAG 0x10000000U /* Control character, value in first position */
#define ESCAPED_TAG 0x01000000U /* Escaped character, value in first position */
#define TAG_MASK    0xFF000000U

#define MAXSIZE (1 << 16)

#define COL(_l) ((_l) % cols)
#define LINE(_l) ((_l) / cols)

#define NL '\n'

/* Main interface functions. */
static void ttysl_stop(ErlDrvData);
static void ttysl_from_erlang(ErlDrvData, char*, ErlDrvSizeT);
static void ttysl_from_tty(ErlDrvData, ErlDrvEvent);
static void ttysl_stop_select(ErlDrvEvent, void*);
static Sint16 get_sint16(char*);

static ErlDrvPort ttysl_port;
static int ttysl_fd;
static FILE *ttysl_out;

/* Functions that work on the line buffer. */
static int start_lbuf(void);
static int stop_lbuf(void);
static int put_chars(byte*,int);
static int move_rel(int);
static int ins_chars(byte *,int);
static int del_chars(int);
static int step_over_chars(int);
static int insert_buf(byte*,int);
static int write_buf(Uint32 *,int);
static int outc(int c);
static int move_cursor(int,int);

/* Termcap functions. */
static int start_termcap(void);
static int stop_termcap(void);
static int move_left(int);
static int move_right(int);
static int move_up(int);
static int move_down(int);
static void update_cols(void);

/* Terminal setting functions. */
static int tty_init(int,int,int,int);
static int tty_set(int);
static int tty_reset(int);
static ErlDrvSSizeT ttysl_control(ErlDrvData, unsigned int,
				  char *, ErlDrvSizeT, char **, ErlDrvSizeT);
#ifdef ERTS_NOT_USED
static RETSIGTYPE suspend(int);
#endif
static RETSIGTYPE cont(int);
static RETSIGTYPE winch(int);

/*#define LOG_DEBUG*/

#ifdef LOG_DEBUG
FILE *debuglog = NULL;

#define DEBUGLOG(X) 				\
do {						\
    if (debuglog != NULL) {			\
	my_debug_printf X;    			\
    }						\
} while (0)

static void my_debug_printf(char *fmt, ...)
{
    char buffer[1024];
    va_list args;

    va_start(args, fmt);
    erts_vsnprintf(buffer,1024,fmt,args);
    va_end(args);
    erts_fprintf(debuglog,"%s\n",buffer);
    //erts_printf("Debuglog = %s\n",buffer);
}

#else

#define DEBUGLOG(X)

#endif

static int utf8_mode = 0;
static byte utf8buf[4]; /* for incomplete input */
static int utf8buf_size; /* size of incomplete input */

#  define IF_IMPL(x) x
#else
#  define IF_IMPL(x) NULL
#endif /* HAVE_TERMCAP */

/* Define the driver table entry. */
struct erl_drv_entry ttsl_driver_entry = {
    ttysl_init,
    ttysl_start,
    IF_IMPL(ttysl_stop),
    IF_IMPL(ttysl_from_erlang),
    IF_IMPL(ttysl_from_tty),
    NULL,
    "tty_sl",
    NULL,
    NULL,
    IF_IMPL(ttysl_control),
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0, /* ERL_DRV_FLAGs */
    NULL,
    NULL, /* process_exit */
    IF_IMPL(ttysl_stop_select)
};


static int ttysl_init(void)
{
#ifdef HAVE_TERMCAP
    ttysl_port = (ErlDrvPort)-1;
    ttysl_fd = -1;
    lbuf = NULL;		/* For line buffer handling */
    capbuf = NULL;		/* For termcap handling */
#endif
#ifdef LOG_DEBUG
    {
	char *dl;
	if ((dl = getenv("TTYSL_DEBUG_LOG")) != NULL && *dl) {
	    debuglog = fopen(dl,"w+");
	    if (debuglog != NULL)
		setbuf(debuglog,NULL);
	}
	DEBUGLOG(("Debuglog = %s(0x%ld)\n",dl,(long) debuglog));
    }
#endif
    return 0;
}

static ErlDrvData ttysl_start(ErlDrvPort port, char* buf)
{
#ifndef HAVE_TERMCAP
    return ERL_DRV_ERROR_GENERAL;
#else
    char *s, *t, *l;
    int canon, echo, sig;	/* Terminal characteristics */
    int flag;
    extern int using_oldshell; /* set this to let the rest of erts know */

    utf8buf_size = 0;
    if (ttysl_port != (ErlDrvPort)-1)
      return ERL_DRV_ERROR_GENERAL;

    if (!isatty(0) || !isatty(1))
	return ERL_DRV_ERROR_GENERAL;

    /* Set the terminal modes to default leave as is. */
    canon = echo = sig = 0;

    /* Parse the input parameters. */
    for (s = strchr(buf, ' '); s; s = t) {
	s++;
	/* Find end of this argument (start of next) and insert NUL. */
	if ((t = strchr(s, ' '))) {
	    *t = '\0';
	}
	if ((flag = ((*s == '+') ? 1 : ((*s == '-') ? -1 : 0)))) {
	    if (s[1] == 'c') canon = flag;
	    if (s[1] == 'e') echo = flag;
	    if (s[1] == 's') sig = flag;
	}
	else if ((ttysl_fd = open(s, O_RDWR, 0)) < 0)
	      return ERL_DRV_ERROR_GENERAL;
    }
    if (ttysl_fd < 0)
      ttysl_fd = 0;

    if (tty_init(ttysl_fd, canon, echo, sig) < 0 ||
	tty_set(ttysl_fd) < 0) {
	ttysl_port = (ErlDrvPort)-1;
	tty_reset(ttysl_fd);
	return ERL_DRV_ERROR_GENERAL;
    }

    /* Set up smart line and termcap stuff. */
    if (!start_lbuf() || !start_termcap()) {
	stop_lbuf();		/* Must free this */
	tty_reset(ttysl_fd);
	return ERL_DRV_ERROR_GENERAL;
    }

    /* Open the terminal and set the terminal */
    ttysl_out = fdopen(ttysl_fd, "w");

#ifdef PRIMITIVE_UTF8_CHECK
    setlocale(LC_CTYPE, "");  /* Set international environment, 
				 ignore result */
    if (((l = getenv("LC_ALL"))   && *l) ||
	((l = getenv("LC_CTYPE")) && *l) ||
	((l = getenv("LANG"))     && *l)) {
	if (strstr(l, "UTF-8"))
	    utf8_mode = 1;
    }
    
#else
    l = setlocale(LC_CTYPE, "");  /* Set international environment */
    if (l != NULL) {
	utf8_mode = (strcmp(nl_langinfo(CODESET), "UTF-8") == 0);
	DEBUGLOG(("setlocale: %s\n",l));
    }
#endif
    DEBUGLOG(("utf8_mode is %s\n",(utf8_mode) ? "on" : "off"));
    sys_sigset(SIGCONT, cont);
    sys_sigset(SIGWINCH, winch);

    driver_select(port, (ErlDrvEvent)(UWord)ttysl_fd, ERL_DRV_READ|ERL_DRV_USE, 1);
    ttysl_port = port;

    /* we need to know this when we enter the break handler */
    using_oldshell = 0;

    return (ErlDrvData)ttysl_port;	/* Nothing important to return */
#endif /* HAVE_TERMCAP */
}

#ifdef HAVE_TERMCAP

#define DEF_HEIGHT 24
#define DEF_WIDTH 80
static void ttysl_get_window_size(Uint32 *width, Uint32 *height)
{
#ifdef TIOCGWINSZ 
    struct winsize ws;
    if (ioctl(ttysl_fd,TIOCGWINSZ,&ws) == 0) {
	*width = (Uint32) ws.ws_col;
	*height = (Uint32) ws.ws_row;
	if (*width <= 0) 
	    *width = DEF_WIDTH;
	if (*height <= 0) 
	    *height = DEF_HEIGHT;
	return;
    }
#endif
    *width = DEF_WIDTH;
    *height = DEF_HEIGHT;
}
    
static ErlDrvSSizeT ttysl_control(ErlDrvData drv_data,
				  unsigned int command,
				  char *buf, ErlDrvSizeT len,
				  char **rbuf, ErlDrvSizeT rlen)
{
    char resbuff[2*sizeof(Uint32)];
    ErlDrvSizeT res_size;
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
	if (len > 0) {
	    int m = (int) *buf;
	    *resbuff = (utf8_mode) ? 1 : 0;
	    res_size = 1;
	    utf8_mode = (m) ? 1 : 0;
	} else {
	    return 0;
	}
	break;
    default:
	return 0;
    }
    if (rlen < res_size) {
	*rbuf = driver_alloc(res_size);
    }
    memcpy(*rbuf,resbuff,res_size);
    return res_size;
}


static void ttysl_stop(ErlDrvData ttysl_data)
{
    if (ttysl_port != (ErlDrvPort)-1) {
	stop_lbuf();
	stop_termcap();
	tty_reset(ttysl_fd);
	driver_select(ttysl_port, (ErlDrvEvent)(UWord)ttysl_fd, ERL_DRV_READ|ERL_DRV_USE, 0);
	sys_sigset(SIGCONT, SIG_DFL);
	sys_sigset(SIGWINCH, SIG_DFL);
    }
    ttysl_port = (ErlDrvPort)-1;
    ttysl_fd = -1;
    /* return TRUE; */
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
		DEBUGLOG(("Printable(UTF-8:%d):%d",(pos - opos),ch));
		size++; /* Buffer contains wide characters... */
	    } else if (ch == '\t') {
		size += 8;
	    } else {
		DEBUGLOG(("Magic(UTF-8:%d):%d",(pos - opos),ch));
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
	DEBUGLOG(("OP: Putc(%lu)",(unsigned long) count-1));
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
	outc('\007');
	break;
    default:
	/* Unknown op, just ignore. */
	break;
    }
    fflush(ttysl_out);
    return; /* TRUE; */
}

static void ttysl_from_tty(ErlDrvData ttysl_data, ErlDrvEvent fd)
{
    byte b[1024];
    ssize_t i;
    int ch = 0, pos = 0;
    int left = 1024;
    byte *p = b;
    byte t[1024];
    int tpos;

    if (utf8buf_size > 0) {
	memcpy(b,utf8buf,utf8buf_size);
	left -= utf8buf_size;
	p += utf8buf_size;
	utf8buf_size = 0;
    }
    
    if ((i = read((int)(SWord)fd, (char *) p, left)) >= 0) {
	if (p != b) {
	    i += (p - b);
	}
	if (utf8_mode) { /* Hopefully an UTF8 terminal */
	    while(pos < i && (ch = pick_utf8(b,i,&pos)) >= 0)
		;
	    if (ch == -2 && i - pos <= 4) {
		/* bytes left to care for */
		utf8buf_size = i -pos;
		memcpy(utf8buf,b+pos,utf8buf_size);
	    } else if (ch == -1) {
		DEBUGLOG(("Giving up on UTF8 mode, invalid character"));
		utf8_mode = 0;
		goto latin_terminal;
	    }
	    driver_output(ttysl_port, (char *) b, pos);
	} else {
	latin_terminal:
	    tpos = 0;
	    while (pos < i) {
		while (tpos < 1020 && pos < i) { /* Max 4 bytes for UTF8 */
		    put_utf8((int) b[pos++], t, 1024, &tpos);
		}
		driver_output(ttysl_port, (char *) t, tpos);
		tpos = 0;
	    }
	}
    } else {
	driver_failure(ttysl_port, -1);
    }
}

static void ttysl_stop_select(ErlDrvEvent e, void* _)
{
    int fd = (int)(long)e;
    if (fd != 0) {
	close(fd);
    }
}

/* Procedures for putting and getting integers to/from strings. */
static Sint16 get_sint16(char *s)
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

    update_cols();

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
	  outc(' ');
	if (COL(llen+l) == 0 && xn)
	{
	   outc(' ');
	   move_left(1);
	}
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
	  outc(' ');
	if (COL(llen+l) == 0 && xn)
	{
	   outc(' ');
	   move_left(1);
	}
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

/*
 * Insert n characters into the buffer at lpos.
 * Know about pad characters and treat \n specially.
 */

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
	} else if (ch == '\e' || ch == '\n' || ch == '\r') {
	    write_buf(lbuf + buffpos, lpos - buffpos);
            if (ch == '\e') {
                outc('\e');
            } else {
                outc('\r');
                if (ch == '\n')
                    outc('\n');
            }
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
	


/*
 * Write n characters in line buffer starting at s. Be smart about
 * non-printables. Know about pad characters and that \n can never
 * occur normally.
 */

static int write_buf(Uint32 *s, int n)
{
    byte ubuf[4];
    int ubytes = 0, i;
    byte lastput = ' ';

    update_cols();

    while (n > 0) {
	if (!(*s & TAG_MASK) ) {
	    if (utf8_mode) {
		ubytes = 0;
		if (put_utf8((int) *s, ubuf, 4, &ubytes) == 0) {
		    for (i = 0; i < ubytes; ++i) {
			outc(ubuf[i]);
		    }
		    lastput = 0; /* Means the last written character was multibyte UTF8 */
		}
	    } else {
		outc((byte) *s);
		lastput = (byte) *s;
	    }
	    --n;
	    ++s;
	}
	else if (*s == (CONTROL_TAG | ((Uint32) '\t'))) {
	    outc(lastput = ' ');
	    --n; s++;
	    while (n > 0 && *s == CONTROL_TAG) {
		outc(lastput = ' ');
		--n; s++;
	    }
	} else if (*s & CONTROL_TAG) {
	    outc('^');
	    outc(lastput = ((byte) ((*s == 0177) ? '?' : *s | 0x40)));
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
	    outc('\\');
	    for (i = 0; i < octbytes; ++i) {
		outc(lastput = octbuff[i]);
		DEBUGLOG(("outc: %d", (int) lastput));
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
    /* Check landed in first column of new line and have 'xn' bug. */
    n = s - lbuf;
    if (COL(n) == 0 && xn && n != 0) {
	if (n >= llen) {
	    outc(' ');
	} else if (lastput == 0) { /* A multibyte UTF8 character */
	    for (i = 0; i < ubytes; ++i) {
		outc(ubuf[i]);
	    }
	} else {
	    outc(lastput);
	}
	move_left(1);
    }
    return TRUE;
}


/* The basic procedure for outputting one character. */
static int outc(int c)
{
    return (int)putc(c, ttysl_out);
}

static int move_cursor(int from, int to)
{
    int dc, dl;

    update_cols();

    dc = COL(to) - COL(from);
    dl = LINE(to) - LINE(from);
    if (dl > 0)
      move_down(dl);
    else if (dl < 0)
      move_up(-dl);
    if (dc > 0)
      move_right(dc);
    else if (dc < 0)
      move_left(-dc);
    return TRUE;
}

static int start_termcap(void)
{
    int eres;
    size_t envsz = 1024;
    char *env = NULL;
    char *c;

    capbuf = driver_alloc(1024);
    if (!capbuf)
	goto false;
    eres = erl_drv_getenv("TERM", capbuf, &envsz);
    if (eres == 0)
	env = capbuf;
    else if (eres < 0)
	goto false;
    else /* if (eres > 1) */ {
      char *envbuf = driver_alloc(envsz);
      if (!envbuf)
	  goto false;
      while (1) {
	  char *newenvbuf;
	  eres = erl_drv_getenv("TERM", envbuf, &envsz);
	  if (eres == 0)
	      break;
	  newenvbuf = driver_realloc(envbuf, envsz);
	  if (eres < 0 || !newenvbuf) {
	      env = newenvbuf ? newenvbuf : envbuf;
	      goto false;
	  }
	  envbuf = newenvbuf;
      }
      env = envbuf;
    }
    if (tgetent((char*)lbuf, env) <= 0)
      goto false;
    if (env != capbuf) {
	env = NULL;
	driver_free(env);
    }
    c = capbuf;
    cols = tgetnum("co");
    if (cols <= 0)
	cols = DEF_WIDTH;
    xn = tgetflag("xn");
    up = tgetstr("up", &c);
    if (!(down = tgetstr("do", &c)))
      down = "\n";
    if (!(left = tgetflag("bs") ? "\b" : tgetstr("bc", &c)))
      left = "\b";		/* Can't happen - but does on Solaris 2 */
    right = tgetstr("nd", &c);
    if (up && down && left && right)
      return TRUE;
 false:
    if (env && env != capbuf)
	driver_free(env);
    if (capbuf)
	driver_free(capbuf);
    capbuf = NULL;
    return FALSE;
}

static int stop_termcap(void)
{
    if (capbuf) driver_free(capbuf);
    capbuf = NULL;
    return TRUE;
}

static int move_left(int n)
{
    while (n-- > 0)
      tputs(left, 1, outc);
    return TRUE;
}

static int move_right(int n)
{
    while (n-- > 0)
      tputs(right, 1, outc);
    return TRUE;
}

static int move_up(int n)
{
    while (n-- > 0)
      tputs(up, 1, outc);
    return TRUE;
}

static int move_down(int n)
{
    while (n-- > 0)
      tputs(down, 1, outc);
    return TRUE;
}
		    

/*
 * Updates cols if terminal has resized (SIGWINCH). Should be called
 * at the start of any function that uses the COL or LINE macros. If
 * the terminal is resized after calling this function but before use
 * of the macros, then we may write to the wrong screen location.
 *
 * We cannot call this from the SIGWINCH handler because it uses
 * ioctl() which is not a safe function as listed in the signal(7)
 * man page.
 */
static void update_cols(void)
{
    Uint32 width, height;
 
    if (cols_needs_update) {
	cols_needs_update = FALSE;
	ttysl_get_window_size(&width, &height);
	cols = width;
    }
}
		    

/*
 * Put a terminal device into non-canonical mode with ECHO off.
 * Before doing so we first save the terminal's current mode,
 * assuming the caller will call the tty_reset() function
 * (also in this file) when it's done with raw mode.
 */

static struct termios tty_smode, tty_rmode;

static int tty_init(int fd, int canon, int echo, int sig)
{
    if (tcgetattr(fd, &tty_rmode) < 0)
      return -1;
    tty_smode = tty_rmode;

    /* Default characteristics for all usage including termcap output. */
    tty_smode.c_iflag &= ~ISTRIP;

    /* Turn canonical (line mode) on off. */
    if (canon > 0) {
	tty_smode.c_iflag |= ICRNL;
	tty_smode.c_lflag |= ICANON;
	tty_smode.c_oflag |= OPOST;
	tty_smode.c_cc[VEOF] = tty_rmode.c_cc[VEOF];
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = tty_rmode.c_cc[VDSUSP];
#endif
    }
    if (canon < 0) {
	tty_smode.c_iflag &= ~ICRNL;
	tty_smode.c_lflag &= ~ICANON;
	tty_smode.c_oflag &= ~OPOST;
	/* Must get these really right or funny effects can occur. */
	tty_smode.c_cc[VMIN] = 1;
	tty_smode.c_cc[VTIME] = 0;
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = 0;
#endif
    }

    /* Turn echo on or off. */
    if (echo > 0)
      tty_smode.c_lflag |= ECHO;
    if (echo < 0)
      tty_smode.c_lflag &= ~ECHO;

    /* Set extra characteristics for "RAW" mode, no signals. */
    if (sig > 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON);
#endif
	tty_smode.c_lflag |= (ISIG|IEXTEN);
    }
    if (sig < 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif	
	tty_smode.c_lflag &= ~(ISIG|IEXTEN);
    }
    return 0;
}

/*
 * Set/restore a terminal's mode to whatever it was on the most
 * recent call to the tty_init() function above.
 */

static int tty_set(int fd)
{
    DEBUGF(("Setting tty...\n"));

    if (tcsetattr(fd, TCSANOW, &tty_smode) < 0)
	return(-1);
    return(0);
}

static int tty_reset(int fd)         /* of terminal device */
{
    DEBUGF(("Resetting tty...\n"));

    if (tcsetattr(fd, TCSANOW, &tty_rmode) < 0)
	return(-1);
    
    return(0);
}

/* 
 * Signal handler to cope with signals so that we can reset the tty
 * to the orignal settings
 */

#ifdef ERTS_NOT_USED
/* XXX: A mistake that it isn't used, or should it be removed? */

static RETSIGTYPE suspend(int sig)
{
    if (tty_reset(ttysl_fd) < 0) {
	fprintf(stderr,"Can't reset tty \n");
	exit(1);
    }

    sys_sigset(sig, SIG_DFL);	/* Set signal handler to default */
    sys_sigrelease(sig);	/* Allow 'sig' to come through */
    kill(getpid(), sig);	/* Send ourselves the signal */
    sys_sigblock(sig);		/* Reset to old mask */
    sys_sigset(sig, suspend);	/* Reset signal handler */ 

    if (tty_set(ttysl_fd) < 0) {
	fprintf(stderr,"Can't set tty raw \n");
	exit(1);
    }
}

#endif

static RETSIGTYPE cont(int sig)
{
    if (tty_set(ttysl_fd) < 0) {
	fprintf(stderr,"Can't set tty raw\n");
	exit(1);
    }
}

static RETSIGTYPE winch(int sig)
{
    cols_needs_update = TRUE;
}
#endif /* HAVE_TERMCAP */
