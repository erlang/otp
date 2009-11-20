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
#include "erlsrv_registry.h"

#define LOG_TYPE "System"
#define LOG_ROOT \
"SYSTEM\\CurrentControlSet\\Services\\EventLog\\" LOG_TYPE "\\"
#define LOG_APP_KEY APP_NAME


#define BASE_KEY HKEY_LOCAL_MACHINE
#define PRODUCT_NAME APP_NAME
#define OLD_PRODUCT_VERSION "1.0"
#define PRODUCT_VERSION "1.1"
#define PROG_KEY "SOFTWARE\\Ericsson\\Erlang\\" PRODUCT_NAME "\\" PRODUCT_VERSION
#define OLD_PROG_KEY "SOFTWARE\\Ericsson\\Erlang\\" PRODUCT_NAME "\\" OLD_PRODUCT_VERSION

#define MAX_KEY_LEN MAX_PATH

static const char * const noString = "\0";

#define MAX_MANDATORY_REG_ENTRY 10 /* InternalServiceName == reg_entries[10] */
static RegEntry reg_entries[] = {
  {"StopAction",REG_SZ,NULL},
  {"OnFail",REG_DWORD,NULL},
  {"Machine",REG_EXPAND_SZ,NULL},
  {"Env", REG_MULTI_SZ,NULL},
  {"WorkDir", REG_EXPAND_SZ,NULL},
  {"Priority",REG_DWORD,NULL},
  {"SName",REG_SZ,NULL},
  {"Name",REG_SZ,NULL},
  {"Args",REG_EXPAND_SZ,NULL},
  {"DebugType",REG_DWORD,NULL},
  {"InternalServiceName",REG_SZ,NULL},
  /* Non mandatory follows */
  {"Comment",REG_SZ,NULL}
};


int num_reg_entries = sizeof(reg_entries)/sizeof(RegEntry);

RegEntry *empty_reg_tab(void){
  RegEntry *ret = malloc(num_reg_entries * sizeof(RegEntry));
  memcpy(ret,reg_entries,num_reg_entries * sizeof(RegEntry));
  return ret;
}

void free_keys(RegEntry *keys){
  int i;

  for(i=0;i<num_reg_entries && keys[i].name != NULL;++i){
    if((keys[i].type == REG_SZ || keys[i].type == REG_EXPAND_SZ ||
	keys[i].type == REG_MULTI_SZ) && 
       keys[i].data.bytes != noString){
      free(keys[i].data.bytes);
      if(keys[i].type == REG_EXPAND_SZ && 
	 keys[i].data.expand.unexpanded != noString)
	free(keys[i].data.expand.unexpanded);
    }
  }
  free(keys);
}

void free_all_keys(RegEntryDesc *descs){
  RegEntryDesc *tmp = descs;
  for(;tmp->servicename != NULL; ++tmp){
    free_keys(tmp->entries);
    free(tmp->servicename);
  }
  free(descs);
}

RegEntry *get_keys(char *servicename){
  RegEntry *res = NULL;
  HKEY prog_key;
  int key_opened = 0;
  int i;
  DWORD ret;
  char *copy;
  char *tmpbuf;
  DWORD tmpbuflen;

  char key_to_open[MAX_KEY_LEN];

  DWORD val_type;
  char *val_data = malloc(MAX_KEY_LEN);
  DWORD val_datalen;
  DWORD val_datasiz = MAX_KEY_LEN;

  if(strlen(PROG_KEY) + strlen(servicename) + 2 > MAX_KEY_LEN)
    goto error;
  sprintf(key_to_open,"%s\\%s",PROG_KEY,servicename);
  
  if(RegOpenKeyEx(BASE_KEY,
		  key_to_open,
		  0,
		  KEY_QUERY_VALUE,
		  &prog_key) != ERROR_SUCCESS)
    goto error;
  key_opened = 1;

  res = malloc(num_reg_entries*sizeof(RegEntry));
  for(i=0;i<num_reg_entries;++i)
    res[i].name = NULL;

  for(i=0;i<num_reg_entries;++i){
    for(;;){
      val_datalen = val_datasiz;
      ret = RegQueryValueEx(prog_key,
			    reg_entries[i].name,
			    NULL,
			    &val_type,
			    (BYTE *) val_data,
			    &val_datalen);
      if(ret == ERROR_SUCCESS){
	if(reg_entries[i].type == val_type)
	  break;
	else
	  goto error;
      } else if(ret == ERROR_MORE_DATA){
	val_data = realloc(val_data,val_datasiz = val_datalen);
      } else if (i > MAX_MANDATORY_REG_ENTRY && ret == ERROR_FILE_NOT_FOUND) {
	  /* Non mandatory entries, look at the type... */
	  switch (reg_entries[i].type){
	  case REG_EXPAND_SZ:
	  case REG_SZ:
	  case REG_MULTI_SZ:
	      val_datalen = 0;
	      break;
	  case REG_DWORD:
	      { 
		  DWORD dummy = 0;
		  memcpy(val_data,&dummy,(val_datalen = sizeof(DWORD)));
	      }
	      break;
	  default:
	      goto error;
	  }
	  break; /* for(;;) */
      } else {
	goto error;
      }
    }
    res[i] = reg_entries[i];
    copy = NULL;
    switch(reg_entries[i].type){
    case REG_EXPAND_SZ:
      if(!val_datalen || val_data[0] == '\0'){
	copy = (char *) noString;
	res[i].data.expand.unexpanded = (char *) noString;
      } else {
	tmpbuf = malloc(MAX_KEY_LEN);
	tmpbuflen = (DWORD) MAX_KEY_LEN;
	for(;;){
	  ret = ExpandEnvironmentStrings(val_data,tmpbuf,tmpbuflen);
	  if(!ret){
	    free(tmpbuf);
	    goto error;
	  }else if(ret > tmpbuflen){
	    tmpbuf=realloc(tmpbuf,tmpbuflen=ret);
	  } else {
	    copy = strdup(tmpbuf);
	    free(tmpbuf);
	    break;
	  }
	}
	res[i].data.expand.unexpanded = strdup(val_data);
      }
    case REG_MULTI_SZ:
    case REG_SZ:
      if(!copy){
	if(!val_datalen || 
	   ((val_datalen == 1 && val_data[0] == '\0') ||
	    (val_datalen == 2 && val_data[0] == '\0' && 
	     val_data[1] == '\0'))){
	  copy = (char *) noString;
	} else {
	  copy = malloc(val_datalen);
	  memcpy(copy,val_data,val_datalen);
	}
      }
      res[i].data.bytes = copy;
      break;
    case REG_DWORD:
      memcpy(&res[i].data.value,val_data,sizeof(DWORD));
      break;
    default:
      goto error;
    }
  }
  RegCloseKey(prog_key);
  free(val_data);
  return res;
error:
  free(val_data);
  if(res != NULL) 
    free_keys(res);
  if(key_opened)
    RegCloseKey(prog_key);
  return NULL;
}

int set_keys(char *servicename, RegEntry *keys){
  HKEY prog_key;
  int key_opened = 0;
  int i;
  char key_to_open[MAX_KEY_LEN];
  DWORD disposition;

  if(strlen(PROG_KEY) + strlen(servicename) + 2 > MAX_KEY_LEN)
    goto error;
  sprintf(key_to_open,"%s\\%s",PROG_KEY,servicename);
  
  if(RegOpenKeyEx(BASE_KEY,
		  key_to_open,
		  0,
		  KEY_SET_VALUE,
		  &prog_key) != ERROR_SUCCESS){
    if(RegCreateKeyEx(BASE_KEY,
		      key_to_open, 
		      0, 
		      NULL, 
		      REG_OPTION_NON_VOLATILE,
		      KEY_SET_VALUE, 
		      NULL, 
		      &prog_key, 
		      &disposition) != ERROR_SUCCESS)
    goto error;
  }
  key_opened = 1;

  
  for(i=0;i<num_reg_entries;++i){
    void *ptr;
    DWORD siz;
    int j;
    switch(keys[i].type){
    case REG_SZ:
      ptr = keys[i].data.bytes;
      siz = strlen(ptr)+1;
      break;
    case REG_EXPAND_SZ:
      ptr = keys[i].data.expand.unexpanded;
      siz = strlen(ptr)+1;
      break;
    case REG_MULTI_SZ:
      ptr = keys[i].data.bytes;
      for(j=0;!(((char *)ptr)[j] == '\0' && 
		((char *)ptr)[j+1] == '\0');++j)
	;
      siz=(DWORD)j+2;
      break;
    case REG_DWORD:
      ptr = &keys[i].data.value;
      siz = sizeof(DWORD);
      break;
    default:
      goto error;
    }
#ifdef HARDDEBUG
    fprintf(stderr,"%s %s:%d\n",keys[i].name,
	    (keys[i].type == REG_DWORD) ? "(dword)" : ptr,siz);
#endif
    if(RegSetValueEx(prog_key,
		     keys[i].name,
		     0,
		     keys[i].type,
		     ptr,
		     siz) != ERROR_SUCCESS)
      goto error;
  }
  RegCloseKey(prog_key);
  return 0;
error:
  if(key_opened)
    RegCloseKey(prog_key);
  return 1;
}

static int do_remove_keys(char *servicename, const char *prog_key_name){
  HKEY prog_key;
  if(RegOpenKeyEx(BASE_KEY,
		  prog_key_name,
		  0,
		  KEY_ALL_ACCESS,
		  &prog_key) != ERROR_SUCCESS)
    return -1;
  if(RegDeleteKey(prog_key,servicename) != ERROR_SUCCESS){
    RegCloseKey(prog_key);
    return -1;
  }
  RegCloseKey(prog_key);
  return 0;
}

int remove_keys(char *servicename){
    int ret;

    if((ret = do_remove_keys(servicename, PROG_KEY)) < 0){
	if(!do_remove_keys(servicename, OLD_PROG_KEY))
	    return 1;
	else
	    return -1;
    } 
    return ret;
}
  

RegEntryDesc *get_all_keys(void){
  RegEntryDesc *res = malloc(10*sizeof(RegEntryDesc));
  int res_siz = 10;
  int ndx = 0;
  HKEY prog_key;
  int key_opened = 0;
  DWORD enum_index;
  char name[MAX_KEY_LEN];
  DWORD namelen;
  char class[MAX_KEY_LEN];
  DWORD classlen;
  FILETIME ft;
  
  res[ndx].servicename = NULL;
  if(RegOpenKeyEx(BASE_KEY, PROG_KEY, 0, 
		  KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS,
		  &prog_key) != ERROR_SUCCESS)
    goto error;
  key_opened = 1;
  for(enum_index = 0, namelen = MAX_KEY_LEN, classlen = MAX_KEY_LEN;
      ERROR_SUCCESS == RegEnumKeyEx(prog_key,
				   enum_index,
				   name,
				   &namelen,
				   NULL,
				   class,
				   &classlen,
				   &ft);
      ++enum_index, namelen = MAX_KEY_LEN, classlen = MAX_KEY_LEN){
    if(ndx >= res_siz - 1)
      res = realloc(res, (res_siz += 10)*sizeof(RegEntryDesc));
    if(!(res[ndx].entries = get_keys(name)))
      goto error;
    res[ndx].servicename = strdup(name);
    res[++ndx].servicename = NULL;
  }
  RegCloseKey(prog_key);
  return res;
error:
  if(key_opened)
    RegCloseKey(prog_key);
  free_all_keys(res);
  return NULL;
}

int register_logkeys(void){
  HKEY key;
  DWORD disposition;
  DWORD types = EVENTLOG_ERROR_TYPE |
    EVENTLOG_WARNING_TYPE |
    EVENTLOG_INFORMATION_TYPE;
  DWORD catcount=1;
  char filename[2048];
  DWORD fnsiz=2048;

  if(RegCreateKeyEx(HKEY_LOCAL_MACHINE,
		    LOG_ROOT LOG_APP_KEY, 0, 
		    NULL, REG_OPTION_NON_VOLATILE,
		    KEY_SET_VALUE, NULL, 
		    &key, &disposition) != ERROR_SUCCESS)
    return -1;
  if(!GetModuleFileName(NULL, filename, fnsiz))
    return -1;
  if(RegSetValueEx(key, "EventMessageFile",
		0, REG_EXPAND_SZ, (LPBYTE) filename,
		strlen(filename)+1) != ERROR_SUCCESS)
    return -1;
  if(RegSetValueEx(key, "TypesSupported",
		0, REG_DWORD, (LPBYTE) &types,
		sizeof(DWORD)) != ERROR_SUCCESS)
    return -1;
  return 0;
}

