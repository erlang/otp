/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#ifndef _ELOG_UTIL_H
#define _ELOG_UTIL_H
/*
 * Module: elog_util
 * Purpouse: Utilities for other elog modules.
 */

#include "elog_global.h"

/*
 * A table of eventlog categories in an NT system. NOTE that the NUM_CATEGORIES
 * constant has to be updated if this table is!
 */
extern char *category_tab[];

#define NUM_CATEGORIES 3

/*
 * Table for translation of severity constants o strings
 */
typedef struct _severity_tab {
  WORD type;
  char *desc;
} SeverityEntry;

extern SeverityEntry severity_tab[];
extern int severity_tab_len;

char *lookup_severity(WORD severity);
/*
 * Lookup in the above table.
 */

/*
 * Safe malloc.
 */
void *my_malloc(size_t siz);
#define malloc(X) my_malloc(X)

/*
 * Error reporting
 */
void output_error(DWORD error, char *extra);
/*
 * Formats a GetLastError() errorcode to text,
 * prepended with 'extra':. Writes to stderr.
 */

/*
 * I don't like mistyped strings...
 */
#define SYSTEM "System"
#define APPLICATION "Application"
#define SECURITY "Security"
/*
 * The mask to get the printable message id value
 */
#define EVENT_ID_MASK 0x0000FFFF

#endif /* _ELOG_UTIL_H */

