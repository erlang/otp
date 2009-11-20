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
#include "elog_global.h"
#include "elog_util.h"

char *category_tab[] = {
  SYSTEM,
  APPLICATION,
  SECURITY,
  NULL
};

SeverityEntry severity_tab[] = {
  {EVENTLOG_ERROR_TYPE , "Error"},
  {EVENTLOG_WARNING_TYPE, "Warning"},
  {EVENTLOG_INFORMATION_TYPE, "Informational"},
  {EVENTLOG_AUDIT_SUCCESS, "Audit_Success"},
  {EVENTLOG_AUDIT_FAILURE, "Audit_Faulure"},
  {0, "Severity_Unknown"}
};

int severity_tab_len = 
sizeof(severity_tab)/sizeof(SeverityEntry);

char *lookup_severity(WORD severity){
  int i;
  for(i=0;i<severity_tab_len-1;++i)
    if(severity_tab[i].type == severity)
      return severity_tab[i].desc;
  return severity_tab[i].desc;
}


void output_error(DWORD error, char *extra){
    char *msgbuf;
    FormatMessage( 
		  FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		  NULL,
		  error,
		  DEFAULT_LANGID, 
		  (LPTSTR) &msgbuf,    
		  0,    
		  NULL );
    fprintf(stderr,"%s: %s", extra, msgbuf);
    LocalFree(msgbuf);
}


#undef malloc
void *my_malloc(size_t siz){
  void *x;
  if((x = malloc(siz)) == NULL){
    fputs("Out of memory.", stderr);
    exit(1);
  }
  return x;
}
