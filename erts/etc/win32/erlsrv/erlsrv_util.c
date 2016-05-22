/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#include <windows.h>
#include <winsvc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erlsrv_global.h"
#include "erlsrv_util.h"
#include "erlsrv_logmess.h"

wchar_t *service_name = L"";
wchar_t *real_service_name = L"";

void log_warning(wchar_t *mess){
  HANDLE logh;
  wchar_t *strings[] = {service_name, mess , NULL};

  if(!(logh = RegisterEventSourceW(NULL,APP_NAME)))
    return;
  ReportEventW(logh, EVENTLOG_WARNING_TYPE, 0, MSG_WARNING,
	       NULL, 2, 0, strings, NULL);
  DeregisterEventSource(logh);
}

void log_error(wchar_t *mess){
  HANDLE logh;
  wchar_t *strings[] = {service_name, mess , NULL};

  if(!(logh = RegisterEventSourceW(NULL,APP_NAME)))
    return;
  ReportEventW(logh, EVENTLOG_ERROR_TYPE, 0, MSG_ERROR,
	       NULL, 2, 0, strings, NULL);
  DeregisterEventSource(logh);
}

void log_info(wchar_t *mess){
  HANDLE logh;
  wchar_t *strings[] = {service_name, mess , NULL};

  if(!(logh = RegisterEventSourceW(NULL,APP_NAME)))
    return;
  ReportEventW(logh, EVENTLOG_INFORMATION_TYPE, 0, MSG_INFO,
	       NULL, 2, 0, strings, NULL);
  DeregisterEventSource(logh);
}

#ifndef NDEBUG
void log_debug(wchar_t *mess){
  wchar_t *buff=malloc((wcslen(mess)+100)*sizeof(wchar_t));
  swprintf(buff,wcslen(mess)+100,L"DEBUG! %s",mess);
  log_info(buff);
  free(buff);
}
#endif

wchar_t *envdup(wchar_t *env){
  wchar_t *tmp;
  int len;
  for(tmp = env; *tmp != L'\0'; tmp += wcslen(tmp)+1)
    ;
  len = (tmp - env) + 1;
  if(len == 1)
    ++len;
  tmp = malloc(len*sizeof(wchar_t));
  memcpy(tmp,env,len*sizeof(wchar_t));
  return tmp;
}

wchar_t **env_to_arg(wchar_t *env){
  wchar_t **ret;
  wchar_t *tmp;
  int i;
  int num_strings = 0;
  for(tmp = env; *tmp != L'\0'; tmp += wcslen(tmp)+1)
    ++num_strings;
  /* malloc enough to insert ONE string */
  ret = malloc(sizeof(wchar_t *) * (num_strings + 2));
  i = 0;
  for(tmp = env; *tmp != L'\0'; tmp += wcslen(tmp)+1){
    ret[i++] = wcsdup(tmp);
  }
  ret[i] = NULL;
  free(env);
  return ret;
}

static int compare(const void *a, const void *b){
  wchar_t *s1 = *((wchar_t **) a);
  wchar_t *s2 = *((wchar_t **) b);
  wchar_t *e1 = wcschr(s1,L'=');
  wchar_t *e2 = wcschr(s2,L'=');
  int ret;
  int len;
  
  if(!e1)
    e1 = s1 + wcslen(s1);
  if(!e2)
    e2 = s2 + wcslen(s2);
  
  if((e1 - s1) > (e2 - s2))
    len = (e2 - s2);
  else
    len = (e1 - s1);
  
  ret = _wcsnicmp(s1,s2,len);
  if(ret == 0)
    return ((e1 - s1) - (e2 - s2));
  else
    return ret;
}

wchar_t *arg_to_env(wchar_t **arg){
  wchar_t *block;
  wchar_t *pek;
  int i;
  int totlen = 1; /* extra '\0' */

  for(i=0;arg[i] != NULL;++i)
    totlen += wcslen(arg[i])+1;
  /* sort the environment vector */
  qsort(arg,i,sizeof(wchar_t *),&compare);
  if(totlen == 1){
      block = malloc(2*sizeof(wchar_t));
    block[0] = block[1] = L'\0'; 
  } else {
    block = malloc(totlen*sizeof(wchar_t));
    pek = block;
    for(i=0; arg[i] != NULL; ++i){
      wcscpy(pek, arg[i]);
      free(arg[i]);
      pek += wcslen(pek)+1;
    }
    *pek = L'\0';
  }
  free(arg);
  return block;
}
