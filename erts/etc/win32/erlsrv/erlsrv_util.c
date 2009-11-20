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
#include <windows.h>
#include <winsvc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erlsrv_global.h"
#include "erlsrv_util.h"
#include "erlsrv_logmess.h"

char *service_name = "";
char *real_service_name = "";

void log_warning(char *mess){
  HANDLE logh;
  char *strings[] = {service_name, mess , NULL};

  if(!(logh = RegisterEventSource(NULL,APP_NAME)))
    return;
  ReportEvent(logh, EVENTLOG_WARNING_TYPE, 0, MSG_WARNING,
	      NULL, 2, 0, strings, NULL);
  DeregisterEventSource(logh);
}

void log_error(char *mess){
  HANDLE logh;
  char *strings[] = {service_name, mess , NULL};

  if(!(logh = RegisterEventSource(NULL,APP_NAME)))
    return;
  ReportEvent(logh, EVENTLOG_ERROR_TYPE, 0, MSG_ERROR,
	      NULL, 2, 0, strings, NULL);
  DeregisterEventSource(logh);
}

void log_info(char *mess){
  HANDLE logh;
  char *strings[] = {service_name, mess , NULL};

  if(!(logh = RegisterEventSource(NULL,APP_NAME)))
    return;
  ReportEvent(logh, EVENTLOG_INFORMATION_TYPE, 0, MSG_INFO,
	      NULL, 2, 0, strings, NULL);
  DeregisterEventSource(logh);
}

#ifndef NDEBUG
void log_debug(char *mess){
  char *buff=malloc(strlen(mess)+100);
  sprintf(buff,"DEBUG! %s",mess);
  log_info(buff);
  free(buff);
}
#endif

char *envdup(char *env){
  char *tmp;
  int len;
  for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1)
    ;
  len = (tmp - env) + 1;
  if(len == 1)
    ++len;
  tmp = malloc(len);
  memcpy(tmp,env,len);
  return tmp;
}

char **env_to_arg(char *env){
  char **ret;
  char *tmp;
  int i;
  int num_strings = 0;
  for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1)
    ++num_strings;
  /* malloc enough to insert ONE string */
  ret = malloc(sizeof(char *) * (num_strings + 2));
  i = 0;
  for(tmp = env; *tmp != '\0'; tmp += strlen(tmp)+1){
    ret[i++] = strdup(tmp);
  }
  ret[i] = NULL;
  free(env);
  return ret;
}

static int compare(const void *a, const void *b){
  char *s1 = *((char **) a);
  char *s2 = *((char **) b);
  char *e1 = strchr(s1,'=');
  char *e2 = strchr(s2,'=');
  int ret;
  int len;
  
  if(!e1)
    e1 = s1 + strlen(s1);
  if(!e2)
    e2 = s2 + strlen(s2);
  
  if((e1 - s1) > (e2 - s2))
    len = (e2 - s2);
  else
    len = (e1 - s1);
  
  ret = _strnicmp(s1,s2,len);
  if(ret == 0)
    return ((e1 - s1) - (e2 - s2));
  else
    return ret;
}

char *arg_to_env(char **arg){
  char *block;
  char *pek;
  int i;
  int totlen = 1; /* extra '\0' */

  for(i=0;arg[i] != NULL;++i)
    totlen += strlen(arg[i])+1;
  /* sort the environment vector */
  qsort(arg,i,sizeof(char *),&compare);
  if(totlen == 1){
    block = malloc(2);
    block[0] = block[1] = '\0'; 
  } else {
    block = malloc(totlen);
    pek = block;
    for(i=0; arg[i] != NULL; ++i){
      strcpy(pek, arg[i]);
      free(arg[i]);
      pek += strlen(pek)+1;
    }
    *pek = '\0';
  }
  free(arg);
  return block;
}
