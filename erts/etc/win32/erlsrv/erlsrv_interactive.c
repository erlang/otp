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
#include <assert.h>
#include "erlsrv_global.h"
#include "erlsrv_registry.h"
#include "erlsrv_interactive.h"
#include "erlsrv_util.h" /* service_name */

#define DBG fwprintf(stderr,L"argv[0]:%s line %d\n",argv[0],__LINE__)
/* #define HARDDEBUG 1 */

#include <fcntl.h>

/* Really HAS to correcpond to the enum in erlsrv_registry.h */
static wchar_t *arg_tab[] = {
  L"stopaction", L"st",
  L"onfail", L"on",
  L"machine", L"m",
  L"env", L"e",
  L"workdir", L"w",
  L"priority", L"p",
  L"sname", L"sn",
  L"name", L"n",
  L"args", L"ar",
  L"debugtype", L"d",
  L"internalservicename",L"i",
  L"comment",L"c",
  NULL, NULL
};

static wchar_t *generate_real_service_name(wchar_t *display_name){
  SYSTEMTIME systime;
  FILETIME ftime;
  int len=(wcslen(display_name)+(8*2)+1);
  wchar_t *buff;
  wchar_t *tmp = _wcsdup(display_name);
  int i;
  buff = (wchar_t*) malloc(len*sizeof(wchar_t));

  /* 2 Hex chars for each byte in a DWORD */
  GetSystemTime(&systime);
  SystemTimeToFileTime(&systime,&ftime);
  /* Remove trailing version info to avoid user confusion */
  for(i = (wcslen(tmp)-1);i > 0; --i)
    if(tmp[i] == L'_'){
      tmp[i] = L'\0';
      break;
    }
  swprintf(buff,len,L"%s%08x%08x",tmp,
	   ftime.dwHighDateTime,
	   ftime.dwLowDateTime);
  free(tmp);
  return buff;
}

static int lookup_arg(wchar_t *arg){
  int i;
  if(*arg != L'-' && *arg != L'/')
    return -1;
  for(i=0; arg_tab[i] != NULL; i += 2){
    if(!_wcsnicmp(arg_tab[i],arg+1,wcslen(arg+1)) &&
       !_wcsnicmp(arg_tab[i+1],arg+1,wcslen(arg_tab[i+1])))
      return (i / 2);
  }
  return -1;
}



wchar_t *edit_env(wchar_t *edit, wchar_t *oldenv){
  wchar_t **arg;
  wchar_t *value;
  wchar_t *name = wcsdup(edit);
  int i;
  wchar_t *tmp;
  arg = env_to_arg(oldenv);
  value = wcschr(name,L'=');
  if(value){
    *(value++) = L'\0';
    if(*value == L'\0')
      value = NULL;
  }
  for(i=0;arg[i] != NULL; ++i){
    tmp = wcschr(arg[i],L'=');
    if(((int) wcslen(name)) == (tmp - arg[i]) &&
       !_wcsnicmp(name,arg[i], tmp - arg[i]))
      break;
  }
  if(arg[i] != NULL){
    free(arg[i]);
    if(value){
      arg[i] = wcsdup(edit);
    } else {
      do {
	arg[i] = arg[i+1];
	++i;
      } while(arg[i] != NULL);
    }
  } else if(value){ /* add to arg, which is always allocated
		     to hold one extra environment variable*/
    arg[i] = wcsdup(edit);
    arg[i+1] = NULL;
  }
  free(name);
  return arg_to_env(arg);
}

int last_error = 0;

void print_last_error(void){
  char *mes;
  FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		NULL,    
		(last_error) ? last_error : GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
		(LPTSTR) &mes,    
		0,    
		NULL );
  fwprintf(stderr,L"Error: %S",mes);
  LocalFree(mes);
}

static int get_last_error(void)
{
  return (last_error) ? last_error : GetLastError();
}

static BOOL install_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  wchar_t filename[MAX_PATH + 3];
  DWORD fnsiz=MAX_PATH;
  wchar_t dependant[] = { L'L',L'a',L'n',L'm',L'a',L'n',
			  L'W',L'o',L'r',L'k',L's',L't',
			  L'a',L't',L'i',L'o',L'n',L'\0',L'\0'};
  
  if(!(fnsiz = GetModuleFileNameW(NULL, filename, fnsiz)))
    return FALSE;
  if(wcschr(filename,L' ')){
    memmove(filename+1,filename,fnsiz*sizeof(wchar_t));
    filename[0] = L'\"'; /* " */
    filename[fnsiz+1] = L'\"'; /* " */
    filename[fnsiz+2] = L'\0';
  }
  if((scm = OpenSCManager(NULL, 
			  NULL,  
			  SC_MANAGER_CONNECT | 
			  SC_MANAGER_CREATE_SERVICE))
     == NULL){
      last_error = GetLastError();
      return FALSE;
  }
  service = CreateServiceW(scm,
			   real_service_name,
			   service_name,
			   SERVICE_ALL_ACCESS & 
			   ~(SERVICE_PAUSE_CONTINUE),
			   SERVICE_WIN32_OWN_PROCESS,
			   SERVICE_AUTO_START,
			   SERVICE_ERROR_NORMAL,
			   filename,
			   NULL,
			   NULL,
			   dependant,
			   NULL, 
			   NULL);
  if(service == NULL){
    CloseServiceHandle(scm);
    last_error = GetLastError();
    return FALSE;
  }
  CloseServiceHandle(service);
  CloseServiceHandle(scm);
  return TRUE;
}

static BOOL remove_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  if((scm = OpenSCManager(NULL, 
			  NULL,  
			  GENERIC_WRITE))
     == NULL)
    return FALSE;
  service = OpenServiceW(scm,
			 real_service_name,
			 SERVICE_ALL_ACCESS);
  if(service == NULL){
    CloseServiceHandle(scm);
    return FALSE;
  }
  if(!DeleteService(service)){
    last_error = GetLastError();
    return FALSE;
  }
  CloseServiceHandle(service);
  CloseServiceHandle(scm);
  return TRUE;
}

static BOOL open_service_control(SC_HANDLE *scm, SC_HANDLE *service){
  if((*scm = OpenSCManager(NULL, 
			  NULL,  
			  SC_MANAGER_ALL_ACCESS))
     == NULL)
    return FALSE;
  *service = OpenServiceW(*scm,
			  real_service_name,
			  SERVICE_ALL_ACCESS);
  if(service == NULL){
    CloseServiceHandle(*scm);
    return FALSE;
  }
  return TRUE;
}

static BOOL open_service_config(SC_HANDLE *scm, SC_HANDLE *service){
    if((*scm = OpenSCManager(NULL, 
			     NULL,  
			     /*GENERIC_WRITE | GENERIC_EXECUTE*/ 
			     SC_MANAGER_ALL_ACCESS))
       == NULL){
	last_error = GetLastError();
	return FALSE;
    }
    *service = OpenServiceW(*scm,
			    real_service_name,
			    /*GENERIC_WRITE*/
			    SERVICE_ALL_ACCESS);
    if(service == NULL){
	last_error = GetLastError();
	CloseServiceHandle(*scm);
	return FALSE;
    }
    return TRUE;
}

static BOOL set_service_comment(wchar_t *comment) {
    SC_HANDLE scm; 
    SC_HANDLE service;
    SERVICE_DESCRIPTIONW sd;
    BOOL ret = TRUE;
    sd.lpDescription = comment;
    if (!open_service_config(&scm,&service)) {
	return FALSE;
    }
    if (!ChangeServiceConfig2W(service,SERVICE_CONFIG_DESCRIPTION,&sd)) {
	last_error = GetLastError();
	ret = FALSE;
    }
    CloseServiceHandle(service);
    CloseServiceHandle(scm);
    return ret;
}

static BOOL wait_service_trans(DWORD initial, DWORD passes, DWORD goal,
			       int timeout)
{
  SC_HANDLE scm;
  SC_HANDLE service;
  int moved = 0;
  BOOL ret;
  int i;
  SERVICE_STATUS stat;

  if(! open_service_config(&scm,&service))
    return FALSE;
  for(i = 0; i < timeout; ++i){
    if(!QueryServiceStatus(service,&stat)){
      last_error = GetLastError();
      ret = FALSE;
      goto out;
    }
    if(stat.dwCurrentState == initial){
      if(moved){
	ret = FALSE;

	/*
	 * The exitcode is usually strange when we tried to stop and failed,
	 * to report a timeout is more appropriate.
	 */
	if(goal == SERVICE_STOPPED)
	    last_error = ERROR_SERVICE_REQUEST_TIMEOUT;
	else
	    last_error = stat.dwWin32ExitCode;
	goto out;
      }
    } else if(stat.dwCurrentState == passes){
      moved = 1;
    } else if(stat.dwCurrentState == goal){
      ret = TRUE;
      goto out;
    }
    Sleep(1000);
  }
  ret = FALSE;
  last_error = ERROR_SERVICE_REQUEST_TIMEOUT;
out:
  CloseServiceHandle(scm);
  CloseServiceHandle(service);
  return ret;  
}

static BOOL stop_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  BOOL ret;
  SERVICE_STATUS ss;

  if(!open_service_control(&scm,&service)){
#ifdef HARDDEBUG
    fwprintf(stderr,L"Failed to open service.\n");
#endif
    return FALSE;
  }
  ret = ControlService(service,SERVICE_CONTROL_STOP,&ss);
  if(!ret){
    last_error = GetLastError();
  }
  CloseServiceHandle(service);
  CloseServiceHandle(scm);
#ifdef HARDDEBUG
  if(!ret)
  {
    fwprintf(stderr,L"Failed to control service.\n");
    print_last_error();
  }
#endif
  return ret;
}

static BOOL start_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  BOOL ret;

  if(!open_service_control(&scm,&service))
    return FALSE;

  ret = StartService(service,0,NULL);
  if(!ret){
    last_error = GetLastError();
  }
  CloseServiceHandle(service);
  CloseServiceHandle(scm);
  return ret;
}

static BOOL disable_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  BOOL ret;

  if(!open_service_config(&scm,&service))
    return FALSE;

  ret = ChangeServiceConfig(service,
			    SERVICE_NO_CHANGE,
			    SERVICE_DISABLED,
			    SERVICE_NO_CHANGE,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    NULL);
  if(!ret){
    last_error = GetLastError();
  }
			    
  CloseServiceHandle(service);
  CloseServiceHandle(scm);
  return ret;
}

static BOOL enable_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  BOOL ret;

if(!open_service_config(&scm,&service))
    return FALSE;

    ret = ChangeServiceConfig(service,
			    SERVICE_NO_CHANGE,
			    SERVICE_AUTO_START,
			    SERVICE_NO_CHANGE,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    NULL);
			    
  if(!ret){
    last_error = GetLastError();
  }
  CloseServiceHandle(service);
  CloseServiceHandle(scm);
  return ret;
}

static BOOL set_interactive(BOOL interactive){
    SC_HANDLE scm;
    SC_HANDLE service;
    BOOL ret;
    
    if(!open_service_config(&scm,&service))
	return FALSE;

    ret = ChangeServiceConfig(service,
			      SERVICE_WIN32_OWN_PROCESS | ((interactive) ?
			      SERVICE_INTERACTIVE_PROCESS : 0),
			      SERVICE_NO_CHANGE,
			      SERVICE_NO_CHANGE,
			      NULL,
			      NULL,
			      NULL,
			      NULL,
			      NULL,
			      NULL,
			      NULL);
    
    if(!ret){
	last_error = GetLastError();
    }
    CloseServiceHandle(service);
    CloseServiceHandle(scm);
    return ret;
}


RegEntry *old_entries = NULL;

BOOL fetch_current(RegEntry *new){
  int i;

  if(!(old_entries = get_keys(service_name)))
    return FALSE;
  for(i=0;i<num_reg_entries;++i)
    new[i] = old_entries[i];
  return TRUE;
}

void cleanup_old(){
  if(old_entries != NULL)
    free_keys(old_entries);
}

BOOL fill_in_defaults(RegEntry *new){
  wchar_t filename[MAX_PATH];
  wchar_t *ptr;


  if(!GetModuleFileNameW(NULL, filename, MAX_PATH))
    return FALSE;
  for(ptr = filename + wcslen(filename) - 1;
      ptr > filename && *ptr != L'\\';
      --ptr)
    ;
  if(*ptr == L'\\')
    ++ptr;
  *ptr = L'\0';

  ptr = (wchar_t*) malloc((wcslen(filename)+wcslen(ERLANG_MACHINE)+1)*sizeof(wchar_t));
  wcscpy(ptr,filename);
  wcscat(ptr,ERLANG_MACHINE);

  new[StopAction].data.string = L"";
  new[OnFail].data.value = ON_FAIL_IGNORE;
  new[Machine].data.string = ptr;
  new[Machine].data.expand.unexpanded =  ptr;
  new[Env].data.string = L"\0";
  new[WorkDir].data.string = new[WorkDir].data.expand.unexpanded = L"";
  new[Priority].data.value = NORMAL_PRIORITY_CLASS;
  new[SName].data.string = service_name;
  new[Name].data.string = L"";
  new[Args].data.string = new[Args].data.expand.unexpanded = L"";
  new[DebugType].data.value = DEBUG_TYPE_NO_DEBUG;
  new[InternalServiceName].data.string = real_service_name;
  new[Comment].data.string = L"";
  return TRUE;
}

int do_usage(wchar_t *arg0){
  wprintf(L"Usage:\n");
  wprintf(L"%s {set | add} <servicename>\n"
	 L"\t[-st[opaction] [<erlang shell command>]]\n"
	 L"\t[-on[fail] [{reboot | restart | restart_always}]]\n"
	 L"\t[-m[achine] [<erl-command>]]\n"
	 L"\t[-e[nv] [<variable>[=<value>]]]\n"
	 L"\t[-w[orkdir] [<directory>]]\n"
	 L"\t[-p[riority] [{low|high|realtime}]]\n"
	 L"\t[{-sn[ame] | -n[ame]} [<nodename>]]\n"
	 L"\t[-d[ebugtype] [{new|reuse|console}]]\n"
	 L"\t[-ar[gs] [<limited erl arguments>]]\n\n"
	 L"%s {start | start_disabled | stop | disable | enable} <servicename>\n\n"
	 L"%s remove <servicename>\n\n"
	 L"%s rename <servicename> <servicename>\n\n"
	 L"%s list [<servicename>]\n\n"
	 L"%s help\n\n",
	 arg0,arg0,arg0,arg0,arg0,arg0);
  wprintf(L"Manipulates Erlang system services on Windows NT.\n\n");
  wprintf(L"When no parameter to an option is specified, the option\n"
	 L"is reset to it's default value. To set an empty argument\n"
	 L"list, give option -args as last option on command line "
	 L"with\n"
	 L"no arguments.\n\n");
  wprintf(L"See Erlang documentation for full description.\n");
  return 0;
}

int do_manage(int argc, wchar_t **argv){
  wchar_t *action = argv[1];
  RegEntry *current = empty_reg_tab();
  
  if(argc < 3){
    fwprintf(stderr,L"%s: No servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  service_name = argv[2];
  if(!fetch_current(current)){
    fwprintf(stderr,L"%s: The service %s is not an erlsrv controlled service.\n",
	    argv[0],service_name);    
    return 1;
  }
  real_service_name = _wcsdup(current[InternalServiceName].data.string);
  free_keys(current);
  
  if(!_wcsicmp(action,L"start")){
    if(!start_service()){
      fwprintf(stderr,L"%s: Failed to start service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      if(!wait_service_trans(SERVICE_STOPPED, SERVICE_START_PENDING,
			     SERVICE_RUNNING, 60)){
	fwprintf(stderr,L"%s: Failed to start service %s.\n",
		argv[0],service_name);
	print_last_error();
	return 1;
      }
      wprintf(L"%s: Service %s started.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  if(!_wcsicmp(action,L"start_disabled")){
    if(!enable_service()){
      fwprintf(stderr,L"%s: Failed to enable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } 
    if(!start_service() && get_last_error() != ERROR_SERVICE_ALREADY_RUNNING){
      fwprintf(stderr,L"%s: Failed to start service %s.\n",
	      argv[0],service_name);
      print_last_error();
      goto failure_starting;
    }
    
    if(!wait_service_trans(SERVICE_STOPPED, SERVICE_START_PENDING,
			   SERVICE_RUNNING, 60)){
      fwprintf(stderr,L"%s: Failed to start service %s.\n",
	      argv[0],service_name);
      print_last_error();
      goto failure_starting;
    }

    if(!disable_service()){
      fwprintf(stderr,L"%s: Failed to disable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } 
    wprintf(L"%s: Service %s started.\n",
	   argv[0],service_name);
    return 0;
  failure_starting:
    if(!disable_service()){
      fwprintf(stderr,L"%s: Failed to disable service %s.\n",
	      argv[0],service_name);
      print_last_error();
    } 
    return 1;
  }
  if(!_wcsicmp(action,L"stop")){
    if(!stop_service()){
      fwprintf(stderr,L"%s: Failed to stop service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      if(!wait_service_trans(SERVICE_RUNNING, SERVICE_STOP_PENDING,
			     SERVICE_STOPPED, 60)){
	fwprintf(stderr,L"%s: Failed to stop service %s.\n",
		argv[0],service_name);
	print_last_error();
	return 1;
      }
      wprintf(L"%s: Service %s stopped.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  if(!_wcsicmp(action,L"disable")){
#if 0
    if(stop_service()){
      wprintf(L"%s: Service %s stopped.\n",
	     argv[0],service_name);
    }
#endif
    if(!disable_service()){
      fwprintf(stderr,L"%s: Failed to disable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      wprintf(L"%s: Service %s disabled.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  if(!_wcsicmp(action,L"enable")){
    if(!enable_service()){
      fwprintf(stderr,L"%s: Failed to enable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      wprintf(L"%s: Service %s enabled.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  fwprintf(stderr,L"%s: Unrecignized argument %s.\n",
	  argv[0],action);
  return 1;
}

int do_add_or_set(int argc, wchar_t **argv){
  RegEntry *new_entries;
  RegEntry *default_entries;
  int add = 0;
  int i;
  int current;
  int set_comment = 0;
  new_entries = empty_reg_tab();
  default_entries = empty_reg_tab();
  if(argc < 3){
    fwprintf(stderr,L"%s: No servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  service_name = argv[2];
  if(!_wcsicmp(argv[1],L"add")){
    if(fetch_current(default_entries)){
      fwprintf(stderr,L"%s: A service with the name %s already "
	      L"exists.\n",
	      argv[0],service_name);
      return 1;
    }
    real_service_name = generate_real_service_name(service_name);
    if(!fill_in_defaults(new_entries)){
      fwprintf(stderr,L"%s: Internal error.\n", argv[0]);
      return 1;
    }
    add = 1;
  } else {
    if(!fetch_current(new_entries)){
      fwprintf(stderr,L"%s: No service with the name %s exists.\n",
	      argv[0], service_name);
      return 1;
    }
    real_service_name = new_entries[InternalServiceName].data.string;
  }

  if(!fill_in_defaults(default_entries)){
    fwprintf(stderr,L"%s: Internal error.\n", argv[0]);
    return 1;
  }

  /* make sure env is malloced... */
  new_entries[Env].data.string = envdup(new_entries[Env].data.string);

  for(i = 3; i < argc; ++i){
    switch((current = lookup_arg(argv[i]))){
    case Comment:
	set_comment = 1;
    case Machine:
    case WorkDir:
    case Args:
      if(i+1 >= argc){
	new_entries[current].data.string = 
	  default_entries[current].data.string;
	new_entries[current].data.expand.unexpanded = 
	  default_entries[current].data.expand.unexpanded;
      } else {
	new_entries[current].data.expand.unexpanded =
	  new_entries[current].data.string = argv[i+1];
	++i;
      }
      break;
    case SName:
      new_entries[Name].data.string = L"";
    case StopAction:
    case Name:
      if(i+1 >= argc ||
	 *argv[i+1] == L'-' || *argv[i+1] == L'/'){
	new_entries[current].data.string = 
	  default_entries[current].data.string;
      } else {
	new_entries[current].data.string = argv[i+1];
	++i;
      }
      break;
    case OnFail:
      if(i+1 >= argc ||
	 *argv[i+1] == L'-' || *argv[i+1] == L'/'){
	new_entries[current].data.value = 
	  default_entries[current].data.value;
      } else {
	if(!_wcsicmp(argv[i+1],L"reboot"))
	  new_entries[current].data.value = ON_FAIL_REBOOT;
	else if(!_wcsicmp(argv[i+1],L"restart"))
	  new_entries[current].data.value = ON_FAIL_RESTART;
	else if(!_wcsicmp(argv[i+1],L"restart_always"))
	  new_entries[current].data.value = ON_FAIL_RESTART_ALWAYS;
	else {
	  fwprintf(stderr,L"%s: Unrecognized keyword value %s.\n",
		  argv[0],argv[i+1]);
	  return 1;
	}
	++i;
      }
      break;
    case DebugType:
      if(i+1 >= argc ||
	 *argv[i+1] == L'-' || *argv[i+1] == L'/'){
	new_entries[current].data.value = 
	  default_entries[current].data.value;
      } else {
	if(!_wcsicmp(argv[i+1],L"new"))
	  new_entries[current].data.value = DEBUG_TYPE_NEW;
	else if(!_wcsicmp(argv[i+1],L"reuse"))
	  new_entries[current].data.value = DEBUG_TYPE_REUSE;
	else if(!_wcsicmp(argv[i+1],L"console"))
	  new_entries[current].data.value = DEBUG_TYPE_CONSOLE;
	else {
	  fwprintf(stderr,L"%s: Unrecognized keyword value %s.\n",
		  argv[0],argv[i+1]);
	  return 1;
	}
	++i;
      }
      break;
    case Priority:
      if(i+1 >= argc ||
	 *argv[i+1] == L'-' || *argv[i+1] == L'/'){
	new_entries[current].data.value = 
	  default_entries[current].data.value;
      } else {
	if(!_wcsicmp(argv[i+1],L"high"))
	  new_entries[current].data.value = HIGH_PRIORITY_CLASS;
	else if(!_wcsicmp(argv[i+1],L"low"))
	  new_entries[current].data.value = IDLE_PRIORITY_CLASS;
	else if(!_wcsicmp(argv[i+1],L"realtime"))
	  new_entries[current].data.value = REALTIME_PRIORITY_CLASS;
	else {
	  fwprintf(stderr,L"%s: Unrecognized keyword value %s.\n",
		  argv[0],argv[i+1]);
	  return 1;
	}
	++i;
      }
      break;
      
    case Env:
      if(i+1 >= argc ||
	 *argv[i+1] == L'-' || *argv[i+1] == L'/'){
	fwprintf(stderr,L"%s: %s requires a parameter.\n",
		argv[0],argv[i]);
	return 1;
      }
      new_entries[current].data.string = 
	edit_env(argv[i+1], new_entries[current].data.string);
      ++i;
      break;
    case InternalServiceName:
	if (!add) {
	    fwprintf(stderr,L"%s: %s only allowed when adding a new service.\n",
		    argv[0],argv[i]);
	    return 1;
	}
	if(i+1 >= argc){
	    fwprintf(stderr,L"%s: %s requires a parameter.\n",
		    argv[0],argv[i]);
	    return 1;
	}
	new_entries[InternalServiceName].data.expand.unexpanded =
	    new_entries[InternalServiceName].data.string = argv[i+1];
	++i;
	/* Discard old, should maybe be fred' but we'll exit anyway */
	real_service_name = new_entries[InternalServiceName].data.string;
	break;
    default:
      fwprintf(stderr,L"%s: Unrecognized option %s.\n", argv[0],
	      argv[i]);
      return 1;
    }
  }
  if(*new_entries[SName].data.string && 
     *new_entries[Name].data.string){
#if 0
    fwprintf(stderr,L"%s: Both -sname and -name specified.\n",
	    argv[0]);
    return 1;
#else
    new_entries[SName].data.string = L"";
#endif
  }
  if(add && !(*new_entries[SName].data.string) &&
     !(*new_entries[Name].data.string)){
    fwprintf(stderr,L"%s: Neither -sname nor -name specified.\n",
	    argv[0]);
    return 1;
  }

  if(add && !install_service()){
    fwprintf(stderr,L"%s: Unable to register %s service with service manager.\n",
	    argv[0], service_name);
    print_last_error();
    return 1;
  }
  if(!set_interactive(new_entries[DebugType].data.value == 
		      DEBUG_TYPE_CONSOLE)){
      fwprintf(stderr,L"%s: Warning, could not set correct interactive mode. %s\n",
	    argv[0], service_name);
      print_last_error();
      /* Not severe or??? */
  }
  /* Update registry */
  register_logkeys();
  set_keys(service_name, new_entries);
  /* Update service comment if needed */
  if(set_comment) {
      if (!set_service_comment(new_entries[Comment].data.string)) {
	  fwprintf(stderr,L"%s: Warning, could not set correct "
		  L"service description (comment) %s",
		  argv[0], service_name);
	  print_last_error();
      }
  }

  /* As I do this, I should also clean up the new entries, which is
     somewhat harder as I really dont know what is and what is not
     malloced, but we'll exit anyway, so... */
  cleanup_old();
  if(add)
    wprintf(L"%s: Service %s added to system.\n",
	   argv[0], service_name);
  else
    wprintf(L"%s: Service %s updated.\n",
	   argv[0], service_name);
  return 0;
}

int do_rename(int argc, wchar_t **argv){
  RegEntry *current = empty_reg_tab();
  RegEntry *dummy = empty_reg_tab();
  SC_HANDLE scm;
  SC_HANDLE service;
  if(argc < 3){
    fwprintf(stderr,L"%s: No old servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  if(argc < 4){
    fwprintf(stderr,L"%s: No new servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  }
  service_name = argv[3];
  if(fetch_current(dummy)){
    fwprintf(stderr,L"%s: A service with the name %s already "
	    L"exists.\n",
	    argv[0],service_name);
    return 1;
  }
  service_name = argv[2];

  if(!fetch_current(current)){
    fwprintf(stderr,L"%s: Error, old service name %s does not exist.\n",
	    argv[0],service_name);
    return 1;
  }
  real_service_name = _wcsdup(current[InternalServiceName].data.string);
  if(!open_service_config(&scm,&service)){
    fwprintf(stderr,L"%s: Error, unable to communicate with service control"
	    L" manager.\n",
	    argv[0]);
    print_last_error();
    return 1;
  }
  if(!ChangeServiceConfigW(service,
			   SERVICE_NO_CHANGE,
			   SERVICE_NO_CHANGE,
			   SERVICE_NO_CHANGE,
			   NULL,
			   NULL,
			   NULL,
			   NULL,
			   NULL,
			   NULL,
			   argv[3])){
    fwprintf(stderr,L"%s: Error, unable to communicate with service control"
	    L" manager.\n",
	    argv[0]);
    print_last_error();
    CloseServiceHandle(scm);
    CloseServiceHandle(service);
    return 1;
  }
  CloseServiceHandle(scm);
  CloseServiceHandle(service);
    
  if(remove_keys(service_name) != 0)
    fwprintf(stderr,L"%s: Warning, old service parameter keys could not "
	    L"be removed, continuing.\n", argv[0]);
  /* Update registry */
  register_logkeys();
  set_keys(argv[3], current);
  
  wprintf(L"%s: Service %s renamed to %s.\n",
	 argv[0], service_name, argv[3]);
  return 0;
}

int do_remove(int argc, wchar_t **argv){
  RegEntry *current = empty_reg_tab();
  int rem_res;
  BOOL found;

  if(argc < 3){
    fwprintf(stderr,L"%s: No servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  service_name = argv[2];
  found = fetch_current(current);
  if(found){
      real_service_name = _wcsdup(current[InternalServiceName].data.string);
  } else {
      real_service_name = _wcsdup(service_name);
  }
  if(found)
      free_keys(current);
  if(stop_service() && !wait_service_trans(SERVICE_RUNNING, 
					   SERVICE_STOP_PENDING,
					   SERVICE_STOPPED, 60)){
      fwprintf(stderr,L"%s: Failed to stop running service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
  }
  if(!remove_service()){
    fwprintf(stderr,L"%s: Unable to remove service (not enough "
	    L"privileges?)\n",argv[0]);
    print_last_error();
    return 1;
  }

  if((rem_res = remove_keys(service_name)) > 0){
    fwprintf(stderr,L"%s: Warning, service parameter keys belonged to old "
	    L"erlsrv version.\n", argv[0]);
    /* Backward compatibility... */
  } else if(rem_res < 0) {
    fwprintf(stderr,L"%s: Error, service parameter keys nonexistent.\n", 
	    argv[0]);
    return 1;
  }
  wprintf(L"%s: Service %s removed from system.\n",
	 argv[0], service_name);
  return 0;
}

BOOL list_one(wchar_t *servicename, RegEntry *keys, BOOL longlist){
  wchar_t *onfail;
  wchar_t *prio;
  wchar_t *debugtype;
  switch(keys[OnFail].data.value){
  case ON_FAIL_RESTART:
    onfail = L"restart";
    break;
  case ON_FAIL_RESTART_ALWAYS:
    onfail = L"restart_always";
    break;
  case ON_FAIL_REBOOT:
    onfail = L"reboot";
    break;
  default:
    onfail = L"ignore";
  }
  switch(keys[DebugType].data.value){
  case DEBUG_TYPE_NEW:
    debugtype = L"new";
    break;
  case DEBUG_TYPE_REUSE:
    debugtype = L"reuse";
    break;
  case DEBUG_TYPE_CONSOLE:
    debugtype = L"console";
    break;
  default:
    debugtype = L"none";
  }
  switch(keys[Priority].data.value){
  case HIGH_PRIORITY_CLASS:
    prio = L"high";
    break;
  case IDLE_PRIORITY_CLASS:
    prio = L"low";
    break;
  case REALTIME_PRIORITY_CLASS:
    prio = L"realtime";
    break;
  case NORMAL_PRIORITY_CLASS:
    prio = L"default";
    break;
  default:
    prio = L"unknown/faulty";
  }
  

  if(longlist){
    wchar_t *env = envdup(keys[Env].data.string);
    wchar_t **arg = env_to_arg(env);
    wchar_t **pek = arg;
    wprintf(L"Service name: %s\n",
	   servicename);
    wprintf(L"StopAction: %s\n",
	   keys[StopAction].data.string);
    wprintf(L"OnFail: %s\n",onfail);
    wprintf(L"Machine: %s\n",
	   keys[Machine].data.expand.unexpanded);
    wprintf(L"WorkDir: %s\n",
	   keys[WorkDir].data.expand.unexpanded);
    if(*keys[SName].data.string)
      wprintf(L"SName: %s\n",
	     keys[SName].data.string);
    else
      wprintf(L"Name: %s\n",
	     keys[Name].data.string);
    wprintf(L"Priority: %s\n",prio);
    wprintf(L"DebugType: %s\n",debugtype);
    wprintf(L"Args: %s\n",
	   keys[Args].data.expand.unexpanded);
    wprintf(L"InternalServiceName: %s\n",
	   keys[InternalServiceName].data.string);
    wprintf(L"Comment: %s\n",
	   keys[Comment].data.string);
    wprintf(L"Env:\n");
    while(*pek){
      wprintf(L"\t%s\n",*pek);
      ++pek;
    }
    /* env is easier to free...*/
    env = arg_to_env(arg);
    free(env);
  } else {
    wprintf(L"%s\t%s\t%s\t%s\t%s\n",
	   servicename,
	   (*keys[Name].data.string) ?
	   keys[Name].data.string :
	   keys[SName].data.string,
	   prio,
	   onfail,
	   keys[Args].data.expand.unexpanded);
  }
  return TRUE;
}
      

int do_list(int argc, wchar_t **argv){
  if(argc < 3){
    RegEntryDesc *all_keys = get_all_keys();
    if(!all_keys){
      fwprintf(stderr,L"%s: No services found in registry.\n",
	      argv[0]);
      return 0;
    }
    wprintf(L"Service\t(S)Name\tPrio\tOnFail\tArgs\n");
    while(all_keys->servicename){
      list_one(all_keys->servicename,all_keys->entries,FALSE);
      ++all_keys;
    }
    return 0;
  } else {
    RegEntry *keys;
    service_name = argv[2];
    keys  = get_keys(service_name);
    if(!keys){
      fwprintf(stderr,L"%s: Could not retrieve any "
	       L"registered data for %s.\n",argv[0],service_name);
      return 1;
    }
    list_one(service_name, keys, TRUE);
  }
  return 0;
}

#define READ_CHUNK 100
#define ARGV_CHUNK 20

wchar_t *safe_get_line(void){
    int lsize = READ_CHUNK;
    wchar_t *line = malloc(READ_CHUNK*sizeof(wchar_t));
    int pos = 0;
    int ch;

    while((ch = getwchar()) != EOF && ch != L'\n'){
	if(pos + 1 >= lsize){
	    line = realloc(line,(lsize += READ_CHUNK)*sizeof(wchar_t));
	    assert(line);
	}
	line[pos++] = ch;
    }
    if(ch == EOF || !pos){
	free(line);
	return NULL;
    }
    line[pos] = L'\0';
    return line;
}


void read_arguments(int *pargc, wchar_t ***pargv){
    int argc = 0;
    int asize = ARGV_CHUNK;
    wchar_t **argv = malloc(ARGV_CHUNK*sizeof(wchar_t *));
    wchar_t *tmp;

    argv[0] = (*pargv)[0];
    argc = 1;
    while((tmp = safe_get_line()) != NULL){
	if(argc + 1 >= asize){
	    argv = realloc(argv,(asize += ARGV_CHUNK)*sizeof(wchar_t *));
	    assert(argv);
	}
	argv[argc++] = tmp;
    }
    argv[argc] = NULL;
    *pargc = argc;
    *pargv = argv;
}

/* Create a free-for-all ACL to set on the semaphore */
PACL get_acl(PSECURITY_DESCRIPTOR secdescp)  
{
  DWORD acl_length = 0;  
  PSID auth_users_sidp = NULL;  
  PACL aclp = NULL;  
  SID_IDENTIFIER_AUTHORITY ntauth = SECURITY_NT_AUTHORITY;  
  
  if(!InitializeSecurityDescriptor(secdescp, SECURITY_DESCRIPTOR_REVISION)) {
    return NULL;
  }
  
  if(!AllocateAndInitializeSid(&ntauth, 
			       1, 
			       SECURITY_AUTHENTICATED_USER_RID, 
			       0, 0, 0, 0, 0, 0, 0, 
			       &auth_users_sidp)) {
    return NULL;
  }
  
  acl_length =   sizeof(ACL) +  
    sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD) +  
    GetLengthSid(auth_users_sidp);  
  
  if((aclp = (PACL) HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, acl_length)) == NULL) {
    FreeSid(auth_users_sidp);  
    return NULL;
  }
  
  if(!InitializeAcl(aclp, acl_length, ACL_REVISION)) {
    FreeSid(auth_users_sidp);  
    HeapFree(GetProcessHeap(), 0, aclp);
    return NULL;
  }
  
  if(!AddAccessAllowedAce(aclp, ACL_REVISION, SEMAPHORE_ALL_ACCESS, auth_users_sidp)) {
    FreeSid(auth_users_sidp);  
    HeapFree(GetProcessHeap(), 0, aclp);
    return NULL;
  }
  
  if(!SetSecurityDescriptorDacl(secdescp, TRUE, aclp, FALSE)) {
    FreeSid(auth_users_sidp);  
    HeapFree(GetProcessHeap(), 0, aclp);
    return NULL;
  }
  return aclp;  
} 

static HANDLE lock_semaphore = NULL;

int take_lock(void) {
  SECURITY_ATTRIBUTES attr;  
  PACL aclp;  
  SECURITY_DESCRIPTOR secdesc;  
  
  if ((aclp = get_acl(&secdesc)) == NULL) {
    return -1;
  }
  
  memset(&attr,0,sizeof(attr));
  attr.nLength = sizeof(attr);  
  attr.lpSecurityDescriptor = &secdesc;  
  attr.bInheritHandle = FALSE;  
  
  if ((lock_semaphore = CreateSemaphore(&attr, 1, 1, ERLSRV_INTERACTIVE_GLOBAL_SEMAPHORE)) == NULL) {
    return -1;
  }
  
  if (WaitForSingleObject(lock_semaphore,INFINITE) != WAIT_OBJECT_0) {
    return -1;
  }
  
  HeapFree(GetProcessHeap(), 0, aclp);
  return 0;
}

void release_lock(void) {
  ReleaseSemaphore(lock_semaphore,1,NULL);
}
    


int interactive_main(int argc, wchar_t **argv){
  wchar_t *action = argv[1];
  int res;

  _setmode(_fileno(stdin), _O_U8TEXT);  /* set stdin to UTF8 */
  _setmode(_fileno(stdout), _O_U8TEXT); /* set stdout to UTF8 */
  _setmode(_fileno(stderr), _O_U8TEXT); /* set stderr to UTF8 */

  if (take_lock() != 0) {
    fwprintf(stderr,L"%s: unable to acquire global lock (%s).\n",argv[0],
	    ERLSRV_INTERACTIVE_GLOBAL_SEMAPHORE);
    return 1;
  }
  
  if(!_wcsicmp(action,L"readargs")){
    read_arguments(&argc,&argv);
    action = argv[1];
  }

#ifdef HARDDEBUG
  {int i;
      for(i=0; i < argc; i++) {
	  fwprintf(stderr, L"%s ", argv[i]);
      }
      fwprintf(stderr, L"\n");
  }
#endif

  if(!_wcsicmp(action,L"set") || !_wcsicmp(action,L"add"))
    res = do_add_or_set(argc,argv);
  else if(!_wcsicmp(action,L"rename"))
    res = do_rename(argc,argv);
  else if(!_wcsicmp(action,L"remove"))
    res = do_remove(argc,argv);
  else if(!_wcsicmp(action,L"list"))
    res = do_list(argc,argv);
  else if(!_wcsicmp(action,L"start") ||
	  !_wcsicmp(action,L"start_disabled") ||
	  !_wcsicmp(action,L"stop") ||
	  !_wcsicmp(action,L"enable") ||
	  !_wcsicmp(action,L"disable"))
    res =  do_manage(argc,argv);
  else if(_wcsicmp(action,L"?") &&
	  _wcsicmp(action,L"/?") &&
	  _wcsicmp(action,L"-?") &&
	  *action != L'h' &&
	  *action != L'H') {
    fwprintf(stderr,L"%s: action %s not implemented.\n",argv[0],action);
    do_usage(argv[0]);
    res = 1;
  } else {
    do_usage(argv[0]);
    res = 0;
  }
  release_lock();
  return res;
}

