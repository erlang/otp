/*<copyright>
 * <year>1999-2008</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
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
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
/*
 * Purpose: Various routines for debug printouts and logs.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "debuglog.h"
#include "esock_utils.h"

#ifndef __WIN32__
static char tr_format_buf[256];
static char *tr_format(const char *format);
static int vfprintclistf(FILE *fp, const char *format, va_list args);
#endif

int debug = 0;
int debugmsg = 0;
FILE *ssllogfp = NULL;
FILE *__locallogfp = NULL;

void open_ssllog(char *path)
{
    ssllogfp = openlog(path);
}

void close_ssllog(void)
{
    if (ssllogfp)
	closelog(ssllogfp);
}

FILE *openlog(char *s)
{
    FILE *fp;
    time_t t = time(NULL);

    if ((fp = fopen(s, "a"))) {
	setbuf(fp, NULL);
	fprintf(fp, "===== Opened [%s] %s", s, ctime(&t));
    }
    return fp;
}

void closelog(FILE *fp)
{
    time_t t = time(NULL);

    if (fp) {
	fprintf(fp, "Closed %s", ctime(&t));
	fclose(fp);
    }
}

int __debugprintf(const char *format, ...)
{
    va_list args;
    int ret;
#ifndef __WIN32__
    char *newformat;

    va_start(args, format);
    newformat = tr_format(format);
    ret = vfprintf(stderr, newformat, args);
    if (newformat != format && newformat != tr_format_buf)
	esock_free(newformat);
#else
    va_start(args, format);
    ret = vfprintf(stderr, format, args);
#endif
    va_end(args);
    if (ssllogfp) { 
        va_start(args, format);
	vfprintf(ssllogfp, format, args);
        va_end(args);
    }
    return ret;
}

int __debugprintclistf(const char *format, ...)
{
    va_list args;
    int ret;
#ifndef __WIN32__
    char *newformat;

    va_start(args, format);
    newformat = tr_format(format);
    ret = vfprintclistf(stderr, newformat, args);
    if (newformat != format && newformat != tr_format_buf)
	esock_free(newformat);
#else
    va_start(args, format);
    ret = vfprintclistf(stderr, format, args);
#endif
    if (ssllogfp) 
	vfprintclistf(ssllogfp, format, args);
    va_end(args);
    return ret;
}

int __debuglogf(const char *format, ...)
{
    va_list args;
    int ret;

    va_start(args, format);
    ret = vfprintf(__locallogfp, format, args);
    va_end(args);
    return ret;
}

#ifndef __WIN32__

/* Insert `\r' before each `\n' i format */
static char *tr_format(const char *format)
{
    char *newformat, *s, *t;
    int len;

    len = strlen(format);
    if ((newformat = (len > 127) ? esock_malloc(len) : tr_format_buf)) {
	for (s = (char *)format, t = newformat; *s; *t++ = *s++)
	    if (*s == '\n') 
		*t++ = '\r';
	*t = '\0';
    } else
	newformat = (char *)format;
    return newformat;
}

#endif

/* This function is for printing arrays of characters with formats
 * %FPa or %FPb, where F and P are the ordinary specifiers for 
 * field width and precision, respectively. 
 * 
 * The conversion specifier `a' implies hex-string output, while 
 * the `b' specifier provides character output (for non-printable
 * characters a `.' is written.
 *
 * The F specifier contains the width for each character. The 
 * P specifier tells how many characters to print.
 *
 * Example: Suppose we have a function myprintf(char *format, ...)
 * that calls our vfprintclistf(), and that
 *
 * char buf[] = "h\r\n";
 * len = 3;
 *
 * Then 
 *
 * myprintf("%.2b", buf)         prints     "h."
 * myprintf("%2.3b", buf)        prints     "h . . "
 * myprintf("%3.*a", len, buf)   prints     "68 0d 0a"
 *  
 */

static int vfprintclistf(FILE *fp, const char *format, va_list args)
{

    int i, len, width, prec, written = 0;
    char *s, *prevs, *fstart;
    unsigned char *buf;

    if (!format || !*format)
	return 0;
    
    /* %{[0-9]*|\*}{.{[0-9]*|\*}{a|b} */

    prevs = (char *)format;	/* format is const */
    s = strchr(format, '%');
    while (s && *s) {
	if (s - prevs > 0)
	    written += fprintf(fp, "%.*s", s - prevs, prevs);
	width = prec = 0;
	fstart = s;
	s++;
	if (*s != '%') {	/* otherwise it is not a format */
	    if (*s == '*') {	/* width in arg */
		s++;
		width = va_arg(args, int);
	    } else if ((len = strspn(s, "0123456789"))) { /* const width */
		width = atoi(s);
		s += len;
	    } else 
		width = 0;
	    if (*s == '.') {	/* precision specified */
		s++;
		if (*s == '*') { /* precision in arg */
		    s++;
		    prec = va_arg(args, int);
		} else if ((len = strspn(s, "0123456789"))) { /* const prec */
		    prec = atoi(s);
		    s += len;
		} else		/* no precision value, defaults to zero */
		    prec = 0;
	    }  else
		prec = 0;	/* no precision defaults to zero */
	    if (*s == 'a' || *s == 'b') { /* only valid specifiers */
		buf = va_arg(args, unsigned char *);
		if (*s == 'a') {
		    for (i = 0; i < prec; i++) 
			written += fprintf(fp, "%*.2x", width, buf[i]);
		}else if (*s == 'b') {
		    for (i = 0; i < prec; i++) {
			if (isprint(buf[i]))
			    written += fprintf(fp, "%*c", width, buf[i]);
			else
			    written += fprintf(fp, "%*c", width, '.');
		    }
		}
	    } else {
		fprintf(stderr, "fprintclistf: format \"%s\" invalid.\n", 
			format);
		va_end(args);
		return written;
	    }
	}
	s++;
	/* Now s points to the next character after the format */
	prevs = s;
	s = strchr(s, '%');
    }
    if (format + strlen(format) + 1 - prevs > 0)
	written += fprintf(fp, "%s", prevs);
    return written;
}

