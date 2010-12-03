/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2010. All Rights Reserved.
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
 * Darwin needs conversion!
 * http://developer.apple.com/library/mac/#qa/qa2001/qa1235.html
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#if !defined(__WIN32__)
#include <locale.h>
#if !defined(HAVE_SETLOCALE) || !defined(HAVE_NL_LANGINFO) || !defined(HAVE_LANGINFO_H)
#define PRIMITIVE_UTF8_CHECK 1
#else
#include <langinfo.h>
#endif
#endif

/* Written once and only once */

static int filename_encoding = ERL_FILENAME_UNKNOWN;
#if defined(__WIN32__) || defined(__DARWIN__)
static int user_filename_encoding = ERL_FILENAME_UTF8; /* Default unicode on windows */
#else
static int user_filename_encoding = ERL_FILENAME_LATIN1;
#endif
void erts_set_user_requested_filename_encoding(int encoding)
{
    user_filename_encoding = encoding;
}

int erts_get_user_requested_filename_encoding(void)
{
    return user_filename_encoding;
}

void erts_init_sys_common_misc(void)
{
#if defined(__WIN32__)
    /* win_efile will totally fail if this is not set. */
    filename_encoding = ERL_FILENAME_WIN_WCHAR;
#else
    if (user_filename_encoding != ERL_FILENAME_UNKNOWN) {
	filename_encoding = user_filename_encoding;
    } else {
	char *l;
	filename_encoding = ERL_FILENAME_LATIN1;
#  ifdef PRIMITIVE_UTF8_CHECK
	setlocale(LC_CTYPE, "");  /* Set international environment, 
				     ignore result */
	if (((l = getenv("LC_ALL"))   && *l) ||
	    ((l = getenv("LC_CTYPE")) && *l) ||
	    ((l = getenv("LANG"))     && *l)) {
	    if (strstr(l, "UTF-8")) {
		filename_encoding = ERL_FILENAME_UTF8;
	    } 
	}
	
#  else
	l = setlocale(LC_CTYPE, "");  /* Set international environment */
	if (l != NULL) {
	    if (strcmp(nl_langinfo(CODESET), "UTF-8") == 0) {
		filename_encoding = ERL_FILENAME_UTF8;
	    }
	}
#  endif
    }
#  if defined(__DARWIN__)
    if (filename_encoding == ERL_FILENAME_UTF8) {
	filename_encoding = ERL_FILENAME_UTF8_MAC;
    }
#  endif
#endif
}

int erts_get_native_filename_encoding(void)
{
    return filename_encoding;
}
