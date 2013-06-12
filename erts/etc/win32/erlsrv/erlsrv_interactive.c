/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
#include <assert.h>
#include "erlsrv_global.h"
#include "erlsrv_registry.h"
#include "erlsrv_interactive.h"
#include "erlsrv_util.h" /* service_name */

#define DBG fprintf(stderr,"argv[0]:%s line %d\n",argv[0],__LINE__)

/* Really HAS to correcpond to the enum in erlsrv_registry.h */
static char *arg_tab[] = {
  "stopaction", "st",
  "onfail", "on",
  "machine", "m",
  "env", "e",
  "workdir", "w",
  "priority", "p",
  "sname", "sn",
  "name", "n",
  "args", "ar",
  "debugtype", "d",
  "internalservicename","i",
  "comment","c",
  NULL, NULL
};

static char *generate_real_service_name(char *display_name){
  SYSTEMTIME systime;
  FILETIME ftime;
  char *buff = malloc(strlen(display_name)+
		      (8*2)+1);
  char *tmp = _strdup(display_name);
  int i;
  /* 2 Hex chars for each byte in a DWORD */
  GetSystemTime(&systime);
  SystemTimeToFileTime(&systime,&ftime);
  /* Remove trailing version info to avoid user confusion */
  for(i = (strlen(tmp)-1);i > 0; --i)
    if(tmp[i] == '_'){
      tmp[i] = '\0';
      break;
    }
  sprintf(buff,"%s%08x%08x",tmp,ftime.dwHighDateTime,
	  ftime.dwLowDateTime);
  free(tmp);
  return buff;
}

static int lookup_arg(char *arg){
  int i;
  if(*arg != '-' && *arg != '/')
    return -1;
  for(i=0; arg_tab[i] != NULL; i += 2){
    if(!_strnicmp(arg_tab[i],arg+1,strlen(arg+1)) &&
       !_strnicmp(arg_tab[i+1],arg+1,strlen(arg_tab[i+1])))
      return (i / 2);
  }
  return -1;
}



char *edit_env(char *edit, char *oldenv){
  char **arg;
  char *value;
  char *name = strdup(edit);
  int i;
  char *tmp;
  arg = env_to_arg(oldenv);
  value = strchr(name,'=');
  if(value){
    *(value++) = '\0';
    if(*value == '\0')
      value = NULL;
  }
  for(i=0;arg[i] != NULL; ++i){
    tmp = strchr(arg[i],'=');
    if(((int) strlen(name)) == (tmp - arg[i]) &&
       !_strnicmp(name,arg[i], tmp - arg[i]))
      break;
  }
  if(arg[i] != NULL){
    free(arg[i]);
    if(value){
      arg[i] = strdup(edit);
    } else {
      do {
	arg[i] = arg[i+1];
	++i;
      } while(arg[i] != NULL);
    }
  } else if(value){ /* add to arg, which is always allocated
		     to hold one extra environment variable*/
    arg[i] = strdup(edit);
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
    fprintf(stderr,"Error: %s",mes);
    LocalFree(mes);
}

static int get_last_error(void)
{
  return (last_error) ? last_error : GetLastError();
}

static BOOL install_service(void){
  SC_HANDLE scm;
  SC_HANDLE service;
  char filename[MAX_PATH + 3];
  DWORD fnsiz=MAX_PATH;
  char dependant[] = { 'L','a','n','m','a','n',
		       'W','o','r','k','s','t',
		       'a','t','i','o','n','\0','\0'};
  
  if(!(fnsiz = GetModuleFileName(NULL, filename, fnsiz)))
    return FALSE;
  if(strchr(filename,' ')){
    memmove(filename+1,filename,fnsiz);
    filename[0] ='\"'; /* " */
    filename[fnsiz+1] = '\"'; /* " */
    filename[fnsiz+2] = '\0';
  }
  if((scm = OpenSCManager(NULL, 
			  NULL,  
			  SC_MANAGER_CONNECT | 
			  SC_MANAGER_CREATE_SERVICE))
     == NULL){
      last_error = GetLastError();
      return FALSE;
  }
  service = CreateService(scm,
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
  service = OpenService(scm,
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
  *service = OpenService(*scm,
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
    *service = OpenService(*scm,
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

static BOOL set_service_comment(char *comment) {
    SC_HANDLE scm; 
    SC_HANDLE service;
    SERVICE_DESCRIPTION sd;
    BOOL ret = TRUE;
    sd.lpDescription = comment;
    if (!open_service_config(&scm,&service)) {
	return FALSE;
    }
    if (!ChangeServiceConfig2(service,SERVICE_CONFIG_DESCRIPTION,&sd)) {
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
    fprintf(stderr,"Failed to open service.\n");
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
    fprintf(stderr,"Failed to control service.\n");
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
  char filename[MAX_PATH];
  char *ptr;


  if(!GetModuleFileName(NULL, filename, MAX_PATH))
    return FALSE;
  for(ptr = filename + strlen(filename) - 1;
      ptr > filename && *ptr != '\\';
      --ptr)
    ;
  if(*ptr == '\\')
    ++ptr;
  *ptr = '\0';

  ptr = malloc(strlen(filename)+strlen(ERLANG_MACHINE)+1);
  strcpy(ptr,filename);
  strcat(ptr,ERLANG_MACHINE);

  new[StopAction].data.bytes = "";
  new[OnFail].data.value = ON_FAIL_IGNORE;
  new[Machine].data.bytes = ptr;
  new[Machine].data.expand.unexpanded = ptr;
  new[Env].data.bytes = "\0";
  new[WorkDir].data.bytes = new[WorkDir].data.expand.unexpanded =
    "";
  new[Priority].data.value = NORMAL_PRIORITY_CLASS;
  new[SName].data.bytes = service_name;
  new[Name].data.bytes = "";
  new[Args].data.bytes = new[Args].data.expand.unexpanded = "";
  new[DebugType].data.value = DEBUG_TYPE_NO_DEBUG;
  new[InternalServiceName].data.bytes = real_service_name;
  new[Comment].data.bytes = "";
  return TRUE;
}

int do_usage(char *arg0){
  printf("Usage:\n");
  printf("%s {set | add} <servicename>\n"
	 "\t[-st[opaction] [<erlang shell command>]]\n"
	 "\t[-on[fail] [{reboot | restart | restart_always}]]\n"
	 "\t[-m[achine] [<erl-command>]]\n"
	 "\t[-e[nv] [<variable>[=<value>]]]\n"
	 "\t[-w[orkdir] [<directory>]]\n"
	 "\t[-p[riority] [{low|high|realtime}]]\n"
	 "\t[{-sn[ame] | -n[ame]} [<nodename>]]\n"
	 "\t[-d[ebugtype] [{new|reuse|console}]]\n"
	 "\t[-ar[gs] [<limited erl arguments>]]\n\n"
	 "%s {start | start_disabled | stop | disable | enable} <servicename>\n\n"
	 "%s remove <servicename>\n\n"
	 "%s rename <servicename> <servicename>\n\n"
	 "%s list [<servicename>]\n\n"
	 "%s help\n\n",
	 arg0,arg0,arg0,arg0,arg0,arg0);
  printf("Manipulates Erlang system services on Windows NT.\n\n");
  printf("When no parameter to an option is specified, the option\n"
	 "is reset to it's default value. To set an empty argument\n"
	 "list, give option -args as last option on command line "
	 "with\n"
	 "no arguments.\n\n");
  printf("See Erlang documentation for full description.\n");
  return 0;
}

int do_manage(int argc,char **argv){
  char *action = argv[1];
  RegEntry *current = empty_reg_tab();
  
  if(argc < 3){
    fprintf(stderr,"%s: No servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  service_name = argv[2];
  if(!fetch_current(current)){
    fprintf(stderr,"%s: The service %s is not an erlsrv controlled service.\n",
	    argv[0],service_name);    
    return 1;
  }
  real_service_name = _strdup(current[InternalServiceName].data.bytes);
  free_keys(current);
  
  if(!_stricmp(action,"start")){
    if(!start_service()){
      fprintf(stderr,"%s: Failed to start service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      if(!wait_service_trans(SERVICE_STOPPED, SERVICE_START_PENDING,
			     SERVICE_RUNNING, 60)){
	fprintf(stderr,"%s: Failed to start service %s.\n",
		argv[0],service_name);
	print_last_error();
	return 1;
      }
      printf("%s: Service %s started.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  if(!_stricmp(action,"start_disabled")){
    if(!enable_service()){
      fprintf(stderr,"%s: Failed to enable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } 
    if(!start_service() && get_last_error() != ERROR_SERVICE_ALREADY_RUNNING){
      fprintf(stderr,"%s: Failed to start service %s.\n",
	      argv[0],service_name);
      print_last_error();
      goto failure_starting;
    }
    
    if(!wait_service_trans(SERVICE_STOPPED, SERVICE_START_PENDING,
			   SERVICE_RUNNING, 60)){
      fprintf(stderr,"%s: Failed to start service %s.\n",
	      argv[0],service_name);
      print_last_error();
      goto failure_starting;
    }

    if(!disable_service()){
      fprintf(stderr,"%s: Failed to disable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } 
    printf("%s: Service %s started.\n",
	   argv[0],service_name);
    return 0;
  failure_starting:
    if(!disable_service()){
      fprintf(stderr,"%s: Failed to disable service %s.\n",
	      argv[0],service_name);
      print_last_error();
    } 
    return 1;
  }
  if(!_stricmp(action,"stop")){
    if(!stop_service()){
      fprintf(stderr,"%s: Failed to stop service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      if(!wait_service_trans(SERVICE_RUNNING, SERVICE_STOP_PENDING,
			     SERVICE_STOPPED, 60)){
	fprintf(stderr,"%s: Failed to stop service %s.\n",
		argv[0],service_name);
	print_last_error();
	return 1;
      }
      printf("%s: Service %s stopped.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  if(!_stricmp(action,"disable")){
#if 0
    if(stop_service()){
      printf("%s: Service %s stopped.\n",
	     argv[0],service_name);
    }
#endif
    if(!disable_service()){
      fprintf(stderr,"%s: Failed to disable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      printf("%s: Service %s disabled.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  if(!_stricmp(action,"enable")){
    if(!enable_service()){
      fprintf(stderr,"%s: Failed to enable service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
    } else {
      printf("%s: Service %s enabled.\n",
	     argv[0],service_name);
      return 0;
    }
  }
  fprintf(stderr,"%s: Unrecignized argument %s.\n",
	  argv[0],action);
  return 1;
}

int do_add_or_set(int argc, char **argv){
  RegEntry *new_entries;
  RegEntry *default_entries;
  int add = 0;
  int i;
  int current;
  int set_comment = 0;
  new_entries = empty_reg_tab();
  default_entries = empty_reg_tab();
  if(argc < 3){
    fprintf(stderr,"%s: No servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  service_name = argv[2];
  if(!_stricmp(argv[1],"add")){
    if(fetch_current(default_entries)){
      fprintf(stderr,"%s: A service with the name %s already "
	      "exists.\n",
	      argv[0],service_name);
      return 1;
    }
    real_service_name = generate_real_service_name(service_name);
    if(!fill_in_defaults(new_entries)){
      fprintf(stderr,"%s: Internal error.\n", argv[0]);
      return 1;
    }
    add = 1;
  } else {
    if(!fetch_current(new_entries)){
      fprintf(stderr,"%s: No service with the name %s exists.\n",
	      argv[0], service_name);
      return 1;
    }
    real_service_name = new_entries[InternalServiceName].data.bytes;
  }

  if(!fill_in_defaults(default_entries)){
    fprintf(stderr,"%s: Internal error.\n", argv[0]);
    return 1;
  }

  /* make sure env is malloced... */
  new_entries[Env].data.bytes = envdup(new_entries[Env].data.bytes);

  for(i = 3; i < argc; ++i){
    switch((current = lookup_arg(argv[i]))){
    case Comment:
	set_comment = 1;
    case Machine:
    case WorkDir:
    case Args:
      if(i+1 >= argc){
	new_entries[current].data.bytes = 
	  default_entries[current].data.bytes;
	new_entries[current].data.expand.unexpanded = 
	  default_entries[current].data.expand.unexpanded;
      } else {
	new_entries[current].data.expand.unexpanded =
	  new_entries[current].data.bytes = argv[i+1];
	++i;
      }
      break;
    case SName:
      new_entries[Name].data.bytes = "";
    case StopAction:
    case Name:
      if(i+1 >= argc ||
	 *argv[i+1] == '-' || *argv[i+1] == '/'){
	new_entries[current].data.bytes = 
	  default_entries[current].data.bytes;
      } else {
	new_entries[current].data.bytes = argv[i+1];
	++i;
      }
      break;
    case OnFail:
      if(i+1 >= argc ||
	 *argv[i+1] == '-' || *argv[i+1] == '/'){
	new_entries[current].data.value = 
	  default_entries[current].data.value;
      } else {
	if(!_stricmp(argv[i+1],"reboot"))
	  new_entries[current].data.value = ON_FAIL_REBOOT;
	else if(!_stricmp(argv[i+1],"restart"))
	  new_entries[current].data.value = ON_FAIL_RESTART;
	else if(!_stricmp(argv[i+1],"restart_always"))
	  new_entries[current].data.value = ON_FAIL_RESTART_ALWAYS;
	else {
	  fprintf(stderr,"%s: Unrecognized keyword value %s.\n",
		  argv[0],argv[i+1]);
	  return 1;
	}
	++i;
      }
      break;
    case DebugType:
      if(i+1 >= argc ||
	 *argv[i+1] == '-' || *argv[i+1] == '/'){
	new_entries[current].data.value = 
	  default_entries[current].data.value;
      } else {
	if(!_stricmp(argv[i+1],"new"))
	  new_entries[current].data.value = DEBUG_TYPE_NEW;
	else if(!_stricmp(argv[i+1],"reuse"))
	  new_entries[current].data.value = DEBUG_TYPE_REUSE;
	else if(!_stricmp(argv[i+1],"console"))
	  new_entries[current].data.value = DEBUG_TYPE_CONSOLE;
	else {
	  fprintf(stderr,"%s: Unrecognized keyword value %s.\n",
		  argv[0],argv[i+1]);
	  return 1;
	}
	++i;
      }
      break;
    case Priority:
      if(i+1 >= argc ||
	 *argv[i+1] == '-' || *argv[i+1] == '/'){
	new_entries[current].data.value = 
	  default_entries[current].data.value;
      } else {
	if(!_stricmp(argv[i+1],"high"))
	  new_entries[current].data.value = HIGH_PRIORITY_CLASS;
	else if(!_stricmp(argv[i+1],"low"))
	  new_entries[current].data.value = IDLE_PRIORITY_CLASS;
	else if(!_stricmp(argv[i+1],"realtime"))
	  new_entries[current].data.value = REALTIME_PRIORITY_CLASS;
	else {
	  fprintf(stderr,"%s: Unrecognized keyword value %s.\n",
		  argv[0],argv[i+1]);
	  return 1;
	}
	++i;
      }
      break;
      
    case Env:
      if(i+1 >= argc ||
	 *argv[i+1] == '-' || *argv[i+1] == '/'){
	fprintf(stderr,"%s: %s requires a parameter.\n",
		argv[0],argv[i]);
	return 1;
      }
      new_entries[current].data.bytes = 
	edit_env(argv[i+1],
		 new_entries[current].data.bytes);
      ++i;
      break;
    case InternalServiceName:
	if (!add) {
	    fprintf(stderr,"%s: %s only allowed when adding a new service.\n",
		    argv[0],argv[i]);
	    return 1;
	}
	if(i+1 >= argc){
	    fprintf(stderr,"%s: %s requires a parameter.\n",
		    argv[0],argv[i]);
	    return 1;
	}
	new_entries[InternalServiceName].data.expand.unexpanded =
	    new_entries[InternalServiceName].data.bytes = argv[i+1];
	++i;
	/* Discard old, should maybe be fred' but we'll exit anyway */
	real_service_name = new_entries[InternalServiceName].data.bytes;
	break;
    default:
      fprintf(stderr,"%s: Unrecognized option %s.\n", argv[0],
	      argv[i]);
      return 1;
    }
  }
  if(*new_entries[SName].data.bytes && 
     *new_entries[Name].data.bytes){
#if 0
    fprintf(stderr,"%s: Both -sname and -name specified.\n",
	    argv[0]);
    return 1;
#else
    new_entries[SName].data.bytes = "";
#endif
  }
  if(add && !(*new_entries[SName].data.bytes) &&
     !(*new_entries[Name].data.bytes)){
    fprintf(stderr,"%s: Neither -sname nor -name specified.\n",
	    argv[0]);
    return 1;
  }
  if(add && !install_service()){
    fprintf(stderr,"%s: Unable to register service with service manager.\n",
	    argv[0], service_name);
    print_last_error();
    return 1;
  }
  if(!set_interactive(new_entries[DebugType].data.value == 
		      DEBUG_TYPE_CONSOLE)){
      fprintf(stderr,"%s: Warning, could not set correct interactive mode.\n",
	    argv[0], service_name);
      print_last_error();
      /* Not severe or??? */
  }
  /* Update registry */
  register_logkeys();
  set_keys(service_name, new_entries);
  /* Update service comment if needed */
  if(set_comment) {
      if (!set_service_comment(new_entries[Comment].data.bytes)) {
	  fprintf(stderr,"%s: Warning, could not set correct "
		  "service description (comment)",
		  argv[0], service_name);
	  print_last_error();
      }
  }

  /* As I do this, I should also clean up the new entries, which is
     somewhat harder as I really dont know what is and what is not
     malloced, but we'll exit anyway, so... */
  cleanup_old();
  if(add)
    printf("%s: Service %s added to system.\n",
	   argv[0], service_name);
  else
    printf("%s: Service %s updated.\n",
	   argv[0], service_name);
  return 0;
}

int do_rename(int argc, char **argv){
  RegEntry *current = empty_reg_tab();
  RegEntry *dummy = empty_reg_tab();
  SC_HANDLE scm;
  SC_HANDLE service;
  if(argc < 3){
    fprintf(stderr,"%s: No old servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  if(argc < 4){
    fprintf(stderr,"%s: No new servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  }
  service_name = argv[3];
  if(fetch_current(dummy)){
    fprintf(stderr,"%s: A service with the name %s already "
	    "exists.\n",
	    argv[0],service_name);
    return 1;
  }
  service_name = argv[2];

  if(!fetch_current(current)){
    fprintf(stderr,"%s: Error, old service name %s does not exist.\n",
	    argv[0],service_name);
    return 1;
  }
  real_service_name = _strdup(current[InternalServiceName].data.bytes);
  if(!open_service_config(&scm,&service)){
    fprintf(stderr,"%s: Error, unable to communicate with service control"
	    " manager.\n",
	    argv[0]);
    print_last_error();
    return 1;
  }
  if(!ChangeServiceConfig(service,
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
    fprintf(stderr,"%s: Error, unable to communicate with service control"
	    " manager.\n",
	    argv[0]);
    print_last_error();
    CloseServiceHandle(scm);
    CloseServiceHandle(service);
    return 1;
  }
  CloseServiceHandle(scm);
  CloseServiceHandle(service);
    
  if(remove_keys(service_name) != 0)
    fprintf(stderr,"%s: Warning, old service parameter keys could not "
	    "be removed, continuing.\n", argv[0]);
  /* Update registry */
  register_logkeys();
  set_keys(argv[3], current);
  
  printf("%s: Service %s renamed to %s.\n",
	 argv[0], service_name, argv[3]);
  return 0;
}

int do_remove(int argc, char **argv){
  RegEntry *current = empty_reg_tab();
  int rem_res;
  BOOL found;

  if(argc < 3){
    fprintf(stderr,"%s: No servicename given!\n",argv[0]);
    do_usage(argv[0]);
    return 1;
  } 
  service_name = argv[2];
  found = fetch_current(current);
  if(found){
      real_service_name = _strdup(current[InternalServiceName].data.bytes);
  } else {
      real_service_name = _strdup(service_name);
  }
  if(found)
      free_keys(current);
  if(stop_service() && !wait_service_trans(SERVICE_RUNNING, 
					   SERVICE_STOP_PENDING,
					   SERVICE_STOPPED, 60)){
      fprintf(stderr,"%s: Failed to stop running service %s.\n",
	      argv[0],service_name);
      print_last_error();
      return 1;
  }
  if(!remove_service()){
    fprintf(stderr,"%s: Unable to remove service (not enough "
	    "privileges?)\n",argv[0]);
    print_last_error();
    return 1;
  }

  if((rem_res = remove_keys(service_name)) > 0){
    fprintf(stderr,"%s: Warning, service parameter keys belonged to old "
	    "erlsrv version.\n", argv[0]);
    /* Backward compatibility... */
  } else if(rem_res < 0) {
    fprintf(stderr,"%s: Error, service parameter keys nonexistent.\n", 
	    argv[0]);
    return 1;
  }
  printf("%s: Service %s removed from system.\n",
	 argv[0], service_name);
  return 0;
}

BOOL list_one(char *servicename, RegEntry *keys, BOOL longlist){
  char *onfail;
  char *prio;
  char *debugtype;
  switch(keys[OnFail].data.value){
  case ON_FAIL_RESTART:
    onfail = "restart";
    break;
  case ON_FAIL_RESTART_ALWAYS:
    onfail = "restart_always";
    break;
  case ON_FAIL_REBOOT:
    onfail = "reboot";
    break;
  default:
    onfail = "ignore";
  }
  switch(keys[DebugType].data.value){
  case DEBUG_TYPE_NEW:
    debugtype = "new";
    break;
  case DEBUG_TYPE_REUSE:
    debugtype = "reuse";
    break;
  case DEBUG_TYPE_CONSOLE:
    debugtype = "console";
    break;
  default:
    debugtype = "none";
  }
  switch(keys[Priority].data.value){
  case HIGH_PRIORITY_CLASS:
    prio = "high";
    break;
  case IDLE_PRIORITY_CLASS:
    prio = "low";
    break;
  case REALTIME_PRIORITY_CLASS:
    prio = "realtime";
    break;
  case NORMAL_PRIORITY_CLASS:
    prio = "default";
    break;
  default:
    prio = "unknown/faulty";
  }
  

  if(longlist){
    char *env = envdup(keys[Env].data.bytes);
    char **arg = env_to_arg(env);
    char **pek = arg;
    printf("Service name: %s\n",
	   servicename);
    printf("StopAction: %s\n",
	   keys[StopAction].data.bytes);
    printf("OnFail: %s\n",onfail);
    printf("Machine: %s\n",
	   keys[Machine].data.expand.unexpanded);
    printf("WorkDir: %s\n",
	   keys[WorkDir].data.expand.unexpanded);
    if(*keys[SName].data.bytes)
      printf("SName: %s\n",
	     keys[SName].data.bytes);
    else
      printf("Name: %s\n",
	     keys[Name].data.bytes);
    printf("Priority: %s\n",prio);
    printf("DebugType: %s\n",debugtype);
    printf("Args: %s\n",
	   keys[Args].data.expand.unexpanded);
    printf("InternalServiceName: %s\n",
	   keys[InternalServiceName].data.bytes);
    printf("Comment: %s\n",
	   keys[Comment].data.bytes);
    printf("Env:\n");
    while(*pek){
      printf("\t%s\n",*pek);
      ++pek;
    }
    /* env is easier to free...*/
    env = arg_to_env(arg);
    free(env);
  } else {
    printf("%s\t%s\t%s\t%s\t%s\n",
	   servicename,
	   (*keys[Name].data.bytes) ? 
	   keys[Name].data.bytes :
	   keys[SName].data.bytes,
	   prio,
	   onfail,
	   keys[Args].data.expand.unexpanded);
  }
  return TRUE;
}
      

int do_list(int argc, char **argv){
  if(argc < 3){
    RegEntryDesc *all_keys = get_all_keys();
    if(!all_keys){
      fprintf(stderr,"%s: No services found in registry.\n",
	      argv[0]);
      return 0;
    }
    printf("Service\t(S)Name\tPrio\tOnFail\tArgs\n");
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
      fprintf(stderr,"%s: Could not retrieve any "
	      "registered data for %s.\n",argv[0],service_name);
      return 1;
    }
    list_one(service_name, keys, TRUE);
  }
  return 0;
}

#define READ_CHUNK 100
#define ARGV_CHUNK 20

char *safe_get_line(void){
    int lsize = READ_CHUNK;
    char *line = malloc(READ_CHUNK);
    int pos = 0;
    int ch;

    while((ch = getchar()) != EOF && ch != '\n'){
	if(pos + 1 >= lsize){
	    line = realloc(line,(lsize += READ_CHUNK));
	    assert(line);
	}
	line[pos++] = ch;
    }
    if(ch == EOF || !pos){
	free(line);
	return NULL;
    }
    line[pos] = '\0';
    return line;
}


void read_arguments(int *pargc, char ***pargv){
    int argc = 0;
    int asize = ARGV_CHUNK;
    char **argv = malloc(ARGV_CHUNK*sizeof(char *));
    char *tmp;

    argv[0] = (*pargv)[0];
    argc = 1;
    while((tmp = safe_get_line()) != NULL){
	if(argc + 1 >= asize){
	    argv = realloc(argv,(asize += ARGV_CHUNK)*sizeof(char *));
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
    


int interactive_main(int argc, char **argv){
  char *action = argv[1];
  int res;
  
  if (take_lock() != 0) {
    fprintf(stderr,"%s: unable to acquire global lock (%s).\n",argv[0],
	    ERLSRV_INTERACTIVE_GLOBAL_SEMAPHORE);
    return 1;
  }
  
  if(!_stricmp(action,"readargs")){
    read_arguments(&argc,&argv);
    action = argv[1];
  }
  if(!_stricmp(action,"set") || !_stricmp(action,"add"))
    res = do_add_or_set(argc,argv);
  else if(!_stricmp(action,"rename"))
    res = do_rename(argc,argv);
  else if(!_stricmp(action,"remove"))
    res = do_remove(argc,argv);
  else if(!_stricmp(action,"list"))
    res = do_list(argc,argv);
  else if(!_stricmp(action,"start") ||
	  !_stricmp(action,"start_disabled") ||
	  !_stricmp(action,"stop") ||
	  !_stricmp(action,"enable") ||
	  !_stricmp(action,"disable"))
    res =  do_manage(argc,argv);
  else if(_stricmp(action,"?") &&
	  _stricmp(action,"/?") &&
	  _stricmp(action,"-?") &&
	  *action != 'h' &&
	  *action != 'H') {
    fprintf(stderr,"%s: action %s not implemented.\n",argv[0],action);
    do_usage(argv[0]);
    res = 1;
  } else {
    do_usage(argv[0]);
    res = 0;
  }
  release_lock();
  return res;
}

