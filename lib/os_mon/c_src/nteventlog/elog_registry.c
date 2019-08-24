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
#include "elog_global.h"
#include "elog_util.h"
#include "elog_registry.h"

/*
 * Constants for get/set_regkeys
 */
#define APP_ROOT_KEY "SOFTWARE\\Ericsson\\Erlang"
#define APP_SUB_KEY "EventLogReader"
#define APP_VERSION "1.0"
#define LATEST_RECORD_NAME "LatestRecord"
#define LATEST_TIME_NAME "LatestTime"
/*
 * Constants for get_messagefiles
 */
#define LOG_KEY_TMPL "SYSTEM\\CurrentControlSet\\Services\\EventLog\\%s\\%s"
#define EVENT_MSG_FILE_NAME "EventMessageFile"
#define PARAM_MSG_FILE_NAME "ParameterMessageFile"
MessageFiles get_messagefiles(const char *category, const char *facility,
			      char *eventbuff, int eventbufflen,
			      char *parambuff, int parambufflen){
  char *b1 = malloc(strlen(LOG_KEY_TMPL)+strlen(category)+strlen(facility)+1);
  HKEY key;
  char name[1024];
  char val[MAX_PATH];
  MessageFiles mf = { NULL, NULL };
  DWORD namelen, vallen, type, i, ret;

  if(!b1)
    return mf;
  sprintf(b1,LOG_KEY_TMPL,category,facility);
  if(RegOpenKeyEx(HKEY_LOCAL_MACHINE, b1, 0, KEY_QUERY_VALUE, &key) !=
     ERROR_SUCCESS){
    free(b1);
    return mf;
  }
  free(b1);
  for(namelen=1024,vallen=MAX_PATH,i=0;
      ERROR_SUCCESS == RegEnumValue(key,
				    i,
				    name,
				    &namelen,
				    NULL,
				    &type,
				    (LPBYTE) val,
				    &vallen);
      namelen=1024,vallen=MAX_PATH,++i){
    if(!strcmp(name,EVENT_MSG_FILE_NAME)){
      if(type == REG_EXPAND_SZ){
	ret = ExpandEnvironmentStrings(val,eventbuff,eventbufflen);
	if(((int) ret) > eventbufflen || !ret)
	  break;
      } else {
	if(((int) strlen(val)) >= eventbufflen)
	  break;
	else
	  strcpy(eventbuff,val);
      }
      mf.event = eventbuff;
    } else if(!strcmp(name,PARAM_MSG_FILE_NAME)){
      if(type == REG_EXPAND_SZ){
	ret = ExpandEnvironmentStrings(val,parambuff,parambufflen);
	if(((int) ret) > parambufflen || !ret)
	  break;
      } else {
	if(((int) strlen(val)) >= parambufflen)
	  break;
	else
	  strcpy(parambuff,val);
      }
      mf.param = parambuff;
    }
  }
  RegCloseKey(key);
  return mf;
}


int create_regkeys(char *identifier){
  HKEY key,key2;
  DWORD dispositions;
  int i,j;
  char *values[] = {
    LATEST_RECORD_NAME,
    LATEST_TIME_NAME,
    NULL
  };

  DWORD zero = 0;

  if(RegCreateKeyEx(HKEY_LOCAL_MACHINE,
		    APP_ROOT_KEY "\\" APP_SUB_KEY "\\"
		    APP_VERSION,
		    0,
		    NULL,
		    REG_OPTION_NON_VOLATILE,
		    KEY_CREATE_SUB_KEY,
		    NULL,
		    &key,
		    &dispositions) != ERROR_SUCCESS){
    return -1;
  }
  if(RegCreateKeyEx(key, 
		    identifier, 
		    0,
		    NULL,
		    REG_OPTION_NON_VOLATILE,
		    KEY_CREATE_SUB_KEY,
		    NULL,
		    &key2,
		    &dispositions)){
    RegCloseKey(key);
    return -1;
  }
  RegCloseKey(key);
  for(i=0; category_tab[i] != NULL; ++i){
    if(RegCreateKeyEx(key2,
		      category_tab[i],
		      0,
		      NULL,
		      REG_OPTION_NON_VOLATILE,
		      KEY_SET_VALUE,
		      NULL,
		      &key,
		      &dispositions) != ERROR_SUCCESS){
      RegCloseKey(key2);
      return -1;
    }
    for(j=0; values[j] != NULL; ++j){
      if(RegSetValueEx(key, 
		       values[j], 
		       0, 
		       REG_DWORD,
		       (BYTE *) &zero,
		       sizeof(DWORD)) != ERROR_SUCCESS){
	RegCloseKey(key);
	RegCloseKey(key2);
	return -1;
      }
    }
    RegCloseKey(key);
  }
  RegCloseKey(key2);
  return 0;
}
  

int set_regkeys(char *identifier, RegKeys *keys, int num_keys){
  HKEY key;
  char knbuff[SMALLBUFSIZ];
  int i;
  for(i=0; i<num_keys; ++i){
    sprintf(knbuff,"%s\\%s\\%s\\%s\\%s", 
	    APP_ROOT_KEY,
	    APP_SUB_KEY,
	    APP_VERSION,
	    identifier,
	    keys[i].facility_name);
    if(RegOpenKeyEx(HKEY_LOCAL_MACHINE,
		    knbuff,
		    0,
		    KEY_SET_VALUE,
		    &key) != ERROR_SUCCESS){
      return -1;
    }
    if(RegSetValueEx(key, 
		     LATEST_RECORD_NAME,
		     0,
		     REG_DWORD,
		     (BYTE *) &(keys[i].latest_record),
		     sizeof(DWORD)) != ERROR_SUCCESS){
      RegCloseKey(key);
      return -1;
    }
    if(RegSetValueEx(key, 
		     LATEST_TIME_NAME,
		     0,
		     REG_DWORD,
		     (BYTE *) &(keys[i].latest_time),
		     sizeof(DWORD)) != ERROR_SUCCESS){
      RegCloseKey(key);
      return -1;
    }
    RegCloseKey(key);
  }
  return 0;
}


int get_regkeys(char *identifier, RegKeys *keys, int *num_keys /* in out */){
  HKEY key,key2;
  int i;
  char knbuff[SMALLBUFSIZ];
  DWORD knlen;
  char cnbuff[SMALLBUFSIZ];
  DWORD cnlen;
  FILETIME ft;
  DWORD type, data, datasiz;

  sprintf(knbuff,"%s\\%s\\%s\\%s", 
	  APP_ROOT_KEY,
	  APP_SUB_KEY,
	  APP_VERSION,
	  identifier);
  if(RegOpenKeyEx(HKEY_LOCAL_MACHINE,
		  knbuff, 
		  0,
		  KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS,
		  &key) != ERROR_SUCCESS){
    if(create_regkeys(identifier)!= 0){
#ifdef HARDDEBUG
      fprintf(stderr,"Failed creating regkeys\n");
#endif
      return -1;
    } else {
      if(RegOpenKeyEx(HKEY_LOCAL_MACHINE,
		      knbuff, 
		      0,
		      KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS,
		      &key) != ERROR_SUCCESS){
#ifdef HARDDEBUG
	fprintf(stderr,"Failed opening regkeys\n");
#endif
	return -1;
      }
    }
  }
  for(i = 0, knlen = SMALLBUFSIZ, cnlen = SMALLBUFSIZ; 
      i < *num_keys &&
	ERROR_SUCCESS == RegEnumKeyEx(key,
				      i,
				      knbuff,
				      &knlen,
				      NULL,
				      cnbuff,
				      &cnlen,
				      &ft);
      ++i, knlen = SMALLBUFSIZ, cnlen = SMALLBUFSIZ){
    if(RegOpenKeyEx(key, 
		    knbuff, 
		    0, 
		    KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS,
		    &key2) != ERROR_SUCCESS)
      continue;
    strncpy(keys[i].facility_name, knbuff, 
	    MAX_FACILITY_NAME);
    keys[i].facility_name[MAX_FACILITY_NAME - 1] = '\0';
    datasiz = sizeof(DWORD);
    if(RegQueryValueEx(key2, 
		       LATEST_RECORD_NAME,
		       NULL,
		       &type,
		       (BYTE *) &data,
		       &datasiz) != ERROR_SUCCESS)
      keys[i].latest_record = 0;
    else
      keys[i].latest_record = data;
    if(RegQueryValueEx(key2, 
		       LATEST_TIME_NAME,
		       NULL,
		       &type,
		       (BYTE *) &data,
		       &datasiz) != ERROR_SUCCESS)
      keys[i].latest_time = 0;
    else
      keys[i].latest_time = data;
    RegCloseKey(key2);
  }
  *num_keys = i;
  if(!*num_keys){
#ifdef HARDDEBUG
    fprintf(stderr,"get_regkeys got none!\n");
#endif
    return -1;
  }
  return 0;
}
