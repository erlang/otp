/*<copyright>
 * <year>1998-2008</year>
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
 * Purpose:  Debug functions and macros.
 *
 */

#ifndef __DEBUGLOG_H_
#define __DEBUGLOG_H_

#include <stdio.h>
#include "esock_ssl.h"

#define DEBUGF(x)  if (debug) __debugprintf x;
#define DEBUGMSGF(x)  if (debugmsg) __debugprintclistf x;
#define LOGF(fp, x) if (fp) { __locallogfp = fp; __debuglogf x; }
#define SSLDEBUGF()  if (debug) { esock_ssl_print_errors_fp(stderr); \
    if (ssllogfp) esock_ssl_print_errors_fp(ssllogfp); }

int  debug;
int  debugmsg;
FILE *ssllogfp;
FILE *__locallogfp;

void open_ssllog(char *path);
void close_ssllog(void);
FILE *openlog(char *);
void closelog(FILE *);
int __debugprintf(const char *, ...);
int __debugprintclistf(const char *, ...);
int __debuglogf(const char *, ...);

#endif
