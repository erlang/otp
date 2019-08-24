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
#include "erlsrv_registry.h"

#define LOG_TYPE L"System"
#define LOG_ROOT L"SYSTEM\\CurrentControlSet\\Services\\EventLog\\" LOG_TYPE L"\\"
#define LOG_APP_KEY APP_NAME


#define BASE_KEY HKEY_LOCAL_MACHINE
#define PRODUCT_NAME APP_NAME
#define OLD_PRODUCT_VERSION L"1.0"
#define PRODUCT_VERSION L"1.1"
#define PROG_KEY L"SOFTWARE\\Ericsson\\Erlang\\" PRODUCT_NAME L"\\" PRODUCT_VERSION
#define OLD_PROG_KEY L"SOFTWARE\\Ericsson\\Erlang\\" PRODUCT_NAME L"\\" OLD_PRODUCT_VERSION

#define MAX_KEY_LEN MAX_PATH

static const wchar_t * const noString = L"\0";

#define MAX_MANDATORY_REG_ENTRY 10 /* InternalServiceName == reg_entries[10] */
static RegEntry reg_entries[] = {
  {L"StopAction",REG_SZ,NULL},
  {L"OnFail",REG_DWORD,NULL},
  {L"Machine",REG_EXPAND_SZ,NULL},
  {L"Env", REG_MULTI_SZ,NULL},
  {L"WorkDir", REG_EXPAND_SZ,NULL},
  {L"Priority",REG_DWORD,NULL},
  {L"SName",REG_SZ,NULL},
  {L"Name",REG_SZ,NULL},
  {L"Args",REG_EXPAND_SZ,NULL},
  {L"DebugType",REG_DWORD,NULL},
  {L"InternalServiceName",REG_SZ,NULL},
  /* Non mandatory follows */
  {L"Comment",REG_SZ,NULL}
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
       keys[i].data.string != noString){
      free(keys[i].data.string);
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

RegEntry *get_keys(wchar_t *servicename){
  RegEntry *res = NULL;
  HKEY prog_key;
  int key_opened = 0;
  int i;
  DWORD ret;
  wchar_t *copy;
  wchar_t *tmpbuf;
  DWORD tmpbuflen;

  wchar_t key_to_open[MAX_KEY_LEN];

  DWORD val_type;
  wchar_t *val_data = (wchar_t *)malloc(MAX_KEY_LEN * sizeof(wchar_t));
  DWORD val_datalen;
  DWORD val_datasiz = MAX_KEY_LEN;

  if(wcslen(PROG_KEY) + wcslen(servicename) + 2 > MAX_KEY_LEN)
    goto error;
  swprintf(key_to_open,MAX_KEY_LEN,L"%s\\%s",PROG_KEY,servicename);
  
  if(RegOpenKeyExW(BASE_KEY,
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
      ret = RegQueryValueExW(prog_key,
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
      if(!val_datalen || val_data[0] == L'\0'){
	  copy = (wchar_t *) noString;
	res[i].data.expand.unexpanded = (wchar_t *) noString;
      } else {
	tmpbuf = (wchar_t *) malloc(MAX_KEY_LEN * sizeof(wchar_t));
	tmpbuflen = (DWORD) MAX_KEY_LEN;
	for(;;){
	  ret = ExpandEnvironmentStringsW(val_data,tmpbuf,tmpbuflen);
	  if(!ret){
	    free(tmpbuf);
	    goto error;
	  }else if(ret > tmpbuflen){
	    tmpbuf=realloc(tmpbuf,(tmpbuflen=ret)*sizeof(wchar_t));
	  } else {
	    copy = wcsdup(tmpbuf);
	    free(tmpbuf);
	    break;
	  }
	}
	res[i].data.expand.unexpanded = wcsdup(val_data);
      }
    case REG_MULTI_SZ:
    case REG_SZ:
      if(!copy){
	if(!val_datalen || 
	   ((val_datalen == 2 && val_data[0] == L'\0') ||
	    (val_datalen == 4 && val_data[0] == L'\0' && 
	     val_data[1] == L'\0'))){
	  copy = (wchar_t *) noString;
	} else {
	  copy = malloc(val_datalen);         /* val_datalen in bytes */
	  memcpy(copy,val_data,val_datalen); 
	}
      }
      res[i].data.string = copy;
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

int set_keys(wchar_t *servicename, RegEntry *keys){
  HKEY prog_key;
  int key_opened = 0;
  int i;
  wchar_t key_to_open[MAX_KEY_LEN];
  DWORD disposition;

  if(wcslen(PROG_KEY) + wcslen(servicename) + 2 > MAX_KEY_LEN)
    goto error;
  swprintf(key_to_open,MAX_KEY_LEN,L"%s\\%s",PROG_KEY,servicename);
  
  if(RegOpenKeyExW(BASE_KEY,
		   key_to_open,
		   0,
		   KEY_SET_VALUE,
		   &prog_key) != ERROR_SUCCESS){
    if(RegCreateKeyExW(BASE_KEY,
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
      ptr = keys[i].data.string;
      siz = (wcslen(ptr)+1)*sizeof(wchar_t);
      break;
    case REG_EXPAND_SZ:
      ptr = keys[i].data.expand.unexpanded;
      siz = (wcslen(ptr)+1)*sizeof(wchar_t);
      break;
    case REG_MULTI_SZ:
      ptr = keys[i].data.string;
      for(j=0;!(((wchar_t *)ptr)[j] == L'\0' && 
		((wchar_t *)ptr)[j+1] == L'\0');++j)
	;
      siz=(j+2)*sizeof(wchar_t);
      break;
    case REG_DWORD:
      ptr = &keys[i].data.value;
      siz = sizeof(DWORD);
      break;
    default:
      goto error;
    }
#ifdef HARDDEBUG
    fprintf(stderr,"%S %S:%d\n",keys[i].name,
	    (keys[i].type == REG_DWORD) ? L"(dword)" : ptr,siz);
#endif
    if(RegSetValueExW(prog_key,
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

static int do_remove_keys(wchar_t *servicename, const wchar_t *prog_key_name){
  HKEY prog_key;
  if(RegOpenKeyExW(BASE_KEY,
		   prog_key_name,
		   0,
		   KEY_ALL_ACCESS,
		   &prog_key) != ERROR_SUCCESS)
    return -1;
  if(RegDeleteKeyW(prog_key,servicename) != ERROR_SUCCESS){
    RegCloseKey(prog_key);
    return -1;
  }
  RegCloseKey(prog_key);
  return 0;
}

int remove_keys(wchar_t *servicename){
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
  wchar_t name[MAX_KEY_LEN];
  DWORD namelen;
  wchar_t class[MAX_KEY_LEN];
  DWORD classlen;
  FILETIME ft;
  
  res[ndx].servicename = NULL;
  if(RegOpenKeyExW(BASE_KEY, PROG_KEY, 0, 
		   KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS,
		   &prog_key) != ERROR_SUCCESS)
    goto error;
  key_opened = 1;
  for(enum_index = 0, namelen = MAX_KEY_LEN, classlen = MAX_KEY_LEN;
      ERROR_SUCCESS == RegEnumKeyExW(prog_key,
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
    res[ndx].servicename = wcsdup(name);
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
  wchar_t filename[2048];
  DWORD fnsiz=2048;

  if(RegCreateKeyExW(HKEY_LOCAL_MACHINE,
		     LOG_ROOT LOG_APP_KEY, 0, 
		     NULL, REG_OPTION_NON_VOLATILE,
		     KEY_SET_VALUE, NULL, 
		     &key, &disposition) != ERROR_SUCCESS)
    return -1;
  if(!GetModuleFileNameW(NULL, filename, fnsiz))
    return -1;
  if(RegSetValueExW(key, L"EventMessageFile",
		    0, REG_EXPAND_SZ, (LPBYTE) filename,
		    (wcslen(filename)+1)*sizeof(wchar_t)) != ERROR_SUCCESS)
    return -1;
  if(RegSetValueExW(key, L"TypesSupported",
		    0, REG_DWORD, (LPBYTE) &types,
		    sizeof(DWORD)) != ERROR_SUCCESS)
    return -1;
  return 0;
}

