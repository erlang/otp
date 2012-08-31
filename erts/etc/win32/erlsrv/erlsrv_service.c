/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2012. All Rights Reserved.
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
#include "erlsrv_util.h"
#include "erlsrv_service.h"

static HANDLE eventStop;

static HANDLE eventKillErlang;
  
static CRITICAL_SECTION crit;

static SERVICE_STATUS_HANDLE statusHandle;

static DWORD currentState;

static void fill_status(SERVICE_STATUS *status){
  status->dwServiceType = SERVICE_WIN32_OWN_PROCESS;
  status->dwCurrentState = 0;
  status->dwControlsAccepted = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
  status->dwWin32ExitCode = NO_ERROR;
  status->dwServiceSpecificExitCode = 0;
  status->dwCheckPoint = 0;
  status->dwWaitHint = 0;
}

static BOOL set_start_pending(int waithint, int checkpoint){
  SERVICE_STATUS stat;
  fill_status(&stat);
  EnterCriticalSection(&crit);
  currentState = stat.dwCurrentState = SERVICE_START_PENDING;
  LeaveCriticalSection(&crit);
  stat.dwControlsAccepted = 0;
  stat.dwCheckPoint = checkpoint;
  stat.dwWaitHint = waithint;
  return SetServiceStatus(statusHandle, &stat);
}

static BOOL set_stop_pending(int waithint, int checkpoint){
  SERVICE_STATUS stat;
  fill_status(&stat);
  EnterCriticalSection(&crit);
  currentState = stat.dwCurrentState = SERVICE_STOP_PENDING;
  LeaveCriticalSection(&crit);
  stat.dwControlsAccepted = 0;
  stat.dwCheckPoint = checkpoint;
  stat.dwWaitHint = waithint;
  return SetServiceStatus(statusHandle, &stat);
}

static BOOL set_running(){
  SERVICE_STATUS stat;
  fill_status(&stat);
  EnterCriticalSection(&crit);
  currentState = stat.dwCurrentState = SERVICE_RUNNING;
  LeaveCriticalSection(&crit);
  return SetServiceStatus(statusHandle, &stat);
}

static BOOL set_stopped(int error){
  SERVICE_STATUS stat;
  fill_status(&stat);
  EnterCriticalSection(&crit);
  currentState = stat.dwCurrentState = SERVICE_STOPPED;
  LeaveCriticalSection(&crit);
  stat.dwWin32ExitCode = error;
  return SetServiceStatus(statusHandle, &stat);
} 

static BOOL reset_current(){
  SERVICE_STATUS stat;
  fill_status(&stat);
  EnterCriticalSection(&crit);
  stat.dwCurrentState = currentState;
  LeaveCriticalSection(&crit);
  return SetServiceStatus(statusHandle, &stat);
}

static VOID WINAPI handler(DWORD control){
  char buffer[1024];
  sprintf(buffer,"handler called with control = %d.",(int) control);
  log_debug(buffer);
  switch(control){
  case SERVICE_CONTROL_STOP:
  case SERVICE_CONTROL_SHUTDOWN:
    set_stop_pending(30000,1);
    SetEvent(eventStop);
    return;
  default:
    reset_current();
    break;
  }
  return;
}

typedef struct _server_info {
  RegEntry *keys;
  PROCESS_INFORMATION info;
  HANDLE erl_stdin;
  char *event_name;
} ServerInfo;


typedef struct {
  BOOL initialized;
  TOKEN_DEFAULT_DACL *defdacl;
  PACL newacl;
  PSID adminsid;
} SaveAclStruct;


static BOOL reset_acl(SaveAclStruct *save_acl){
    HANDLE tokenh;
    
    if(!save_acl->initialized)
      return FALSE;
    if(!OpenProcessToken(GetCurrentProcess(),
			 TOKEN_READ|TOKEN_WRITE,&tokenh)){
      log_warning("Failed to open access token.");
      return FALSE;
    } 
    save_acl->initialized = FALSE;
    if(!SetTokenInformation(tokenh,
			    TokenDefaultDacl,
			    save_acl->defdacl,
			    sizeof(TOKEN_DEFAULT_DACL))){
      log_warning("Failed to get default ACL from token.");
      CloseHandle(tokenh);
      LocalFree(save_acl->defdacl);
      LocalFree(save_acl->newacl);
      FreeSid(save_acl->adminsid);
      return FALSE;
    }
    CloseHandle(tokenh);
    LocalFree(save_acl->defdacl);
    LocalFree(save_acl->newacl);
    FreeSid(save_acl->adminsid);
    return TRUE;
}
  

static BOOL new_acl(SaveAclStruct *save_acl){
    HANDLE tokenh;
    TOKEN_DEFAULT_DACL newdacl;
    DWORD required;
    PACL oldacl;
    PACL newacl;
    int i;
    ACL_SIZE_INFORMATION si;
    size_t newsize;
    PSID extra_sid;
    SID_IDENTIFIER_AUTHORITY nt_auth = SECURITY_NT_AUTHORITY;  
    TOKEN_DEFAULT_DACL dummy;

    save_acl->initialized = FALSE;
    if(!OpenProcessToken(GetCurrentProcess(),
			 TOKEN_READ|TOKEN_WRITE,&tokenh)){
      log_warning("Failed to open access token.");
      return FALSE;
    } 
    save_acl->defdacl = &dummy;
    required = sizeof(TOKEN_DEFAULT_DACL);
    GetTokenInformation(tokenh,
			TokenDefaultDacl,
			&(save_acl->defdacl),
			sizeof(TOKEN_DEFAULT_DACL),
			&required);
    if(required == 0){
      log_warning("Failed to get any ACL info from token.");
      CloseHandle(tokenh);
      return FALSE;
    }
    save_acl->defdacl = LocalAlloc(LPTR,required);
    if(!GetTokenInformation(tokenh,
			    TokenDefaultDacl,
			    save_acl->defdacl,
			    required,
			    &required)){
#ifdef HARDDEBUG
	{
	  char *mes;
	  FormatMessage(
			FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
			NULL,    
			GetLastError(),
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
			(LPTSTR) &mes,    
			0,    
			NULL );
	  log_info(mes);
	  LocalFree(mes);
	}
#endif 
      log_warning("Failed to get default ACL from token.");
      CloseHandle(tokenh);
      return FALSE;
    }

    oldacl = save_acl->defdacl->DefaultDacl;
    if(!GetAclInformation(oldacl, &si, sizeof(si), 
			  AclSizeInformation)){
      log_warning("Failed to get size information for ACL");
      CloseHandle(tokenh);
      return FALSE;
    }

    if(!AllocateAndInitializeSid(&nt_auth,
				 2,
				 SECURITY_BUILTIN_DOMAIN_RID,
				 DOMAIN_ALIAS_RID_ADMINS,
				 0,
				 0,
				 0,
				 0,
				 0,
				 0,
				 &extra_sid)){
      log_warning("Failed to initialize administrator SID.");
      CloseHandle(tokenh);
      return FALSE;
    }

    newsize = si.AclBytesInUse + sizeof(ACL) +
      sizeof(ACCESS_ALLOWED_ACE) + GetLengthSid(extra_sid);
    
    newacl = LocalAlloc(LPTR,newsize);
    
    if(!InitializeAcl(newacl, newsize, ACL_REVISION)){
      log_warning("Failed to initialize new ACL.");
      LocalFree(newacl);
      FreeSid(extra_sid);
      CloseHandle(tokenh);
      return FALSE;
    }

    for(i=0;i<((int)si.AceCount);++i){
      ACE_HEADER *ace_header;
      if (!GetAce (oldacl, i, &ace_header)){
	log_warning("Failed to get ACE from old ACL.");
	LocalFree(newacl);
	FreeSid(extra_sid);
	CloseHandle(tokenh);
	return FALSE;
      }
      if(!AddAce(newacl,ACL_REVISION,0xffffffff,ace_header,
		 ace_header->AceSize)){
	log_warning("Failed to set ACE in new ACL.");
	LocalFree(newacl);
	FreeSid(extra_sid);
	CloseHandle(tokenh);
	return FALSE;
      }
    }  
    if(!AddAccessAllowedAce(newacl,
			   ACL_REVISION2, 
			   PROCESS_ALL_ACCESS,
			   extra_sid)){
   	log_warning("Failed to add system ACE to new ACL.");
	LocalFree(newacl);
	FreeSid(extra_sid);
	return FALSE;
    }
    
    newdacl.DefaultDacl = newacl;
    if(!SetTokenInformation(tokenh,
			    TokenDefaultDacl,
			    &newdacl,
			    sizeof(newdacl))){
      log_warning("Failed to set token information");
      LocalFree(newacl);
      FreeSid(extra_sid);
      CloseHandle(tokenh);
      return FALSE;
    }
    save_acl->initialized = TRUE;
    save_acl->newacl = newacl;
    save_acl->adminsid = extra_sid;
    CloseHandle(tokenh);

    return TRUE;
}

static char **find_arg(char **arg, char *str){
    char *tmp;
    int len;

    str = strdup(str);
    if((tmp = strchr(str,'=')) == NULL)
	goto fail;
    tmp++;
    *tmp = '\0';
    len = tmp - str;
    while(*arg != NULL){
	if(!_strnicmp(*arg,str,len)){
	    free(str);
	    return arg;
	}
	++arg;
    }
fail:
    free(str);
    return NULL;
}
    
static char **merge_environment(char *current, char *add){
    char **c_arg = env_to_arg(envdup(current));
    char **a_arg = env_to_arg(envdup(add));
    char **new;
    char **tmp;
    int i,j;
    
    for(i=0;c_arg[i] != NULL;++i)
	;
    for(j=0;a_arg[j] != NULL;++j)
	;

    new = malloc(sizeof(char *)*(i + j + 3));

    for(i = 0; c_arg[i] != NULL; ++i)
	new[i] = strdup(c_arg[i]);

    new[i] = NULL;

    for(j = 0; a_arg[j] != NULL; ++j){
	if((tmp = find_arg(new,a_arg[j])) != NULL){
	    free(*tmp);
	    *tmp = strdup(a_arg[j]);
	} else {
	    new[i++] = strdup(a_arg[j]);
	    new[i] = NULL;
	}
    }	    
    free(arg_to_env(c_arg));
    free(arg_to_env(a_arg));
    return new;
}


static char *get_next_debug_file(char *prefix){
    char *buffer = malloc(strlen(prefix)+12);
    int i;
    for(i=1;i<100;++i){
	sprintf(buffer,"%s.%d",prefix,i);
	if(GetFileAttributes(buffer) == 0xFFFFFFFF)
	    return buffer;
    }
    return NULL;
}



static BOOL start_a_service(ServerInfo *srvi){
  STARTUPINFO start;
  char execbuff[MAX_PATH*4]; /* FIXME: Can get overflow! */
  char namebuff[MAX_PATH];
  char errbuff[MAX_PATH*4]; /* hmmm.... */
  HANDLE write_pipe = NULL, read_pipe = NULL;
  SECURITY_ATTRIBUTES pipe_security;
  SECURITY_ATTRIBUTES attr;
  HANDLE nul;
  SaveAclStruct save_acl;
  char *my_environ;
  BOOL console_allocated = FALSE;

  if(!(*(srvi->keys[Env].data.bytes))){
      my_environ = NULL;
  } else {
      char *tmp;
      char **merged = merge_environment((tmp = GetEnvironmentStrings()),
					srvi->keys[Env].data.bytes);
      FreeEnvironmentStrings(tmp);
      my_environ = arg_to_env(merged);
  }
      
  if(!*(srvi->keys[Machine].data.bytes) || 
     (!*(srvi->keys[SName].data.bytes) && 
	 !*(srvi->keys[Name].data.bytes))){
    log_error("Not enough parameters for erlang service.");
    if(my_environ)
	free(my_environ);
    return FALSE;
  }

  if(*(srvi->keys[SName].data.bytes))
    sprintf(namebuff,"-nohup -sname %s",srvi->keys[SName].data.bytes);
  else
    sprintf(namebuff,"-nohup -name %s",srvi->keys[Name].data.bytes);
  
  if(srvi->keys[DebugType].data.value == DEBUG_TYPE_CONSOLE)
      strcat(namebuff," -keep_window");

  if (srvi->event_name != NULL) {
      sprintf(execbuff,"\"%s\" -service_event %s %s %s",
	      srvi->keys[Machine].data.bytes,
	      srvi->event_name,
	      namebuff,
	      srvi->keys[Args].data.bytes);
  } else {
      sprintf(execbuff,"\"%s\" %s %s",
	      srvi->keys[Machine].data.bytes,
	      namebuff,
	      srvi->keys[Args].data.bytes);
  }

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  start.dwFlags = STARTF_USESHOWWINDOW;
  start.wShowWindow = SW_HIDE;

  /* Console debugging implies no working StopAction */
  if(srvi->keys[DebugType].data.value == DEBUG_TYPE_CONSOLE) {
      COORD coord = {80,999};
      if(console_allocated = AllocConsole())
	  SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE),coord);
      else
	  log_warning("Unable to allocate debugging console!");
  } else if(*(srvi->keys[StopAction].data.bytes) || 
	    srvi->keys[DebugType].data.value != DEBUG_TYPE_NO_DEBUG){
    pipe_security.nLength = sizeof(pipe_security);
    pipe_security.lpSecurityDescriptor = NULL;
    pipe_security.bInheritHandle = TRUE;
    if(!CreatePipe(&read_pipe,&write_pipe,&pipe_security,0)){
	log_error("Could not create pipe for erlang service.");
	if(my_environ)
	    free(my_environ);
	return FALSE;
    }
    if(srvi->keys[DebugType].data.value != DEBUG_TYPE_NO_DEBUG){
	char *filename;
	if(*(srvi->keys[WorkDir].data.bytes)){
	    filename = malloc(strlen(srvi->keys[WorkDir].data.bytes) + 1 +
			      strlen(service_name)+strlen(".debug")+1);
	    sprintf(filename,"%s\\%s.debug",
		    srvi->keys[WorkDir].data.bytes,
		    service_name);
	} else {
	    filename = malloc(strlen(service_name)+strlen(".debug")+1);
	    sprintf(filename,"%s.debug",service_name);
	} 
	log_debug(filename);

	if(srvi->keys[DebugType].data.value == DEBUG_TYPE_NEW){
	    char *tmpfn = get_next_debug_file(filename);
	    if(tmpfn){
		free(filename);
		filename = tmpfn;
	    } else {
		log_warning("Number of debug files exceeds system defined "
			    "limit, reverting to DebugType: reuse. ");
	    }
	}
		

	nul = CreateFile(filename,
			 GENERIC_READ | GENERIC_WRITE,
			 FILE_SHARE_READ | FILE_SHARE_WRITE,
			 &pipe_security,
			 CREATE_ALWAYS,
			 FILE_ATTRIBUTE_NORMAL,
			 NULL);
	free(filename);
    } else { /* Not debugging */
	nul = CreateFile("NUL",
			 GENERIC_READ | GENERIC_WRITE,
			 FILE_SHARE_READ | FILE_SHARE_WRITE,
			 &pipe_security,
			 OPEN_EXISTING,
			 FILE_ATTRIBUTE_NORMAL,
			 NULL);
    }
    if(nul == NULL){
	log_error((srvi->keys[DebugType].data.value != DEBUG_TYPE_NO_DEBUG) 
		  ? "Could not create debug file. "
		  "(Working directory not valid?)" 
		  : "Cold not open NUL!");
	start.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	start.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    }
    start.hStdOutput = nul;
    start.hStdError = nul;
    start.hStdInput = read_pipe;
    start.dwFlags |= STARTF_USESTDHANDLES;
  }
  
  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  new_acl(&save_acl);

  if(!CreateProcess(NULL,
		    execbuff,
		    &attr,
		    NULL,
		    (read_pipe != NULL),
		    CREATE_DEFAULT_ERROR_MODE | 
		    (srvi->keys[Priority].data.value),
		    my_environ,
		    (*(srvi->keys[WorkDir].data.bytes)) ?
		    srvi->keys[WorkDir].data.bytes : NULL,
		    &start,
		    &(srvi->info))){
    sprintf(errbuff,"Could not start erlang service "
	    "with commandline \"%s\".",
	    service_name,
	    execbuff
	    );
    log_error(errbuff);
    if(read_pipe != NULL){
      CloseHandle(read_pipe);
      CloseHandle(write_pipe);
      if(nul != NULL)
	CloseHandle(nul);
    }
    if(console_allocated) 
	FreeConsole();
    reset_acl(&save_acl);
    if(my_environ)
	free(my_environ);
    return FALSE;
  }
  if(console_allocated) 
      FreeConsole();
#ifdef HARDDEBUG
  sprintf(errbuff,
	  "Started %s with the following commandline: "
	  "%s",service_name,execbuff);
  log_debug(errbuff);
#endif
  if(read_pipe != NULL){
    CloseHandle(read_pipe);
    if(nul != NULL)
      CloseHandle(nul);
    srvi->erl_stdin = write_pipe;
  }

  reset_acl(&save_acl);
  if(my_environ)
      free(my_environ);
  return TRUE;
}

static HANDLE create_erlang_event(char *event_name)
{
    HANDLE e;
    if ((e = OpenEvent(EVENT_ALL_ACCESS,FALSE,event_name)) == NULL) {
	if ((e = CreateEvent(NULL, TRUE, FALSE, event_name)) == NULL) {
	    log_warning("Could not create or access erlang termination event");
	}
    } else {
	if (!ResetEvent(e)) {
	    log_warning("Could not reset erlang termination event.");
	}
    }
    return e;
}

static BOOL stop_erlang(ServerInfo *srvi, int waithint, 
			int *checkpoint){
  DWORD written = 0;
  char *action = srvi->keys[StopAction].data.bytes;
  DWORD towrite = strlen(action)+1;
  char *toerl;
  DWORD exitcode;
  int i;
  int kill;
  
  if(towrite > 2 && srvi->erl_stdin != NULL){
    toerl = malloc(towrite+1);
    strcpy(toerl,action);
    strcat(toerl,"\n");
    WriteFile(srvi->erl_stdin, toerl, towrite, &written,0);
    free(toerl);
    /* Give it 45 seconds to terminate */
    for(i=0;i<45;++i){
      if(WaitForSingleObject(srvi->info.hProcess, 1000) == 
	 WAIT_OBJECT_0){
	  GetExitCodeProcess(srvi->info.hProcess,&exitcode);
	  CloseHandle(srvi->info.hProcess);
	  CloseHandle(srvi->info.hThread);
	  return TRUE;
      }
      ++(*checkpoint);
      set_stop_pending(waithint,*checkpoint);
    }
    log_warning("StopAction did not terminate erlang. Trying forced kill.");
  } 
  log_debug("Terminating erlang...");
  kill = 1;
  if(eventKillErlang != NULL && SetEvent(eventKillErlang) != 0){
    for(i=0;i<10;++i){
	if(WaitForSingleObject(srvi->info.hProcess, 1000) == WAIT_OBJECT_0){
          kill = 0;
	  break;
	}
	++(*checkpoint);
	set_stop_pending(waithint,*checkpoint);
    }
  } else {
#ifdef HARDDEBUG
	{
	  char *mes;
	  FormatMessage(
			FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
			NULL,    
			GetLastError(),
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
			(LPTSTR) &mes,    
			0,    
			NULL );
	  log_info(mes);
	  LocalFree(mes);
	}
#endif 
    log_debug("Could not send control event to Erlang process");
  }
  if(kill){
    log_warning("Using TerminateProcess to kill erlang.");
    if(!TerminateProcess(srvi->info.hProcess,NO_ERROR))
      log_error("TerminateProcess failed");
  }
  GetExitCodeProcess(srvi->info.hProcess,&exitcode);
  CloseHandle(srvi->info.hProcess);
  CloseHandle(srvi->info.hThread);
  if (eventKillErlang != NULL) {
      ResetEvent(eventKillErlang);
  }
  return TRUE;
}

static BOOL enable_privilege(void) {
	HANDLE ProcessHandle;
	DWORD DesiredAccess = TOKEN_ADJUST_PRIVILEGES;
	HANDLE TokenHandle;
	TOKEN_PRIVILEGES Tpriv;
	LUID luid;
	ProcessHandle = GetCurrentProcess();
	OpenProcessToken(ProcessHandle, DesiredAccess, &TokenHandle);
	LookupPrivilegeValue(0,SE_SHUTDOWN_NAME,&luid);
	Tpriv.PrivilegeCount = 1;
	Tpriv.Privileges[0].Luid = luid;
	Tpriv.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
	return AdjustTokenPrivileges(TokenHandle,FALSE,&Tpriv,0,0,0);
}

static BOOL pull_service_name(void){
  SC_HANDLE scm;
  DWORD sz = 1024;
  static char service_name_buff[1024];
  if((scm = OpenSCManager(NULL, 
			  NULL,  
			  GENERIC_READ))
     == NULL){
    return FALSE;
  }
  if(!GetServiceDisplayName(scm,real_service_name,service_name_buff,&sz))
    return FALSE;
  CloseServiceHandle(scm);
  service_name = service_name_buff;
  return TRUE;
}
  

static VOID WINAPI service_main_loop(DWORD argc, char **argv){
  int waithint = 30000;
  int checkpoint = 1;
  RegEntry *keys;
  RegEntry *save_keys;
  ServerInfo srvi;
  HANDLE harr[2];
  FILETIME creationt,exitt,kernelt,usert;
  LONGLONG creationl,exitl,diffl;
  char event_name[MAX_PATH] = "ErlSrv_";
  char executable_name[MAX_PATH];
#ifdef DEBUG
  char errorbuff[2048]; /* FIXME... */
#endif
  int success_wait = NO_SUCCESS_WAIT;

  real_service_name = argv[0];
  if(!pull_service_name()){
    log_error("Could not get Display name of erlang service.");
    set_stopped(ERROR_CANTREAD);
    return;
  }

  SetEnvironmentVariable((LPCTSTR) SERVICE_ENV, (LPCTSTR) service_name);

  strncat(event_name, service_name, MAX_PATH - strlen(event_name));
  event_name[MAX_PATH - 1] = '\0';

  if(!GetModuleFileName(NULL, executable_name, MAX_PATH)){
      log_error("Unable to retrieve module file name, " EXECUTABLE_ENV 
		" will not be set.");
  } else {
      char quoted_exe_name[MAX_PATH+4];
      sprintf(quoted_exe_name, "\"%s\"", executable_name);
      SetEnvironmentVariable((LPCTSTR) EXECUTABLE_ENV,
			     (LPCTSTR) quoted_exe_name);
  }

  log_debug("Here we go, service_main_loop...");
  currentState = SERVICE_START_PENDING;
  InitializeCriticalSection(&crit);
  eventStop = CreateEvent(NULL,FALSE,FALSE,NULL); 
  if ((eventKillErlang = create_erlang_event(event_name)) != NULL) {
      srvi.event_name = event_name;
  } else {
      srvi.event_name = NULL;
  }
  statusHandle = RegisterServiceCtrlHandler(real_service_name, &handler);
  if(!statusHandle)
    return;
  set_start_pending(waithint,checkpoint);
  keys = get_keys(service_name);
  if(!keys){
    log_error("Could not get registry keys for erlang service.");
    set_stopped(ERROR_CANTREAD);
    return;
  }
  srvi.keys = keys;
  srvi.erl_stdin = NULL; 
  
  ++checkpoint;
  if(!start_a_service(&srvi)){
    log_error("Could not start erlang machine");
    set_stopped(ERROR_PROCESS_ABORTED);
    if (eventKillErlang != NULL) {
	CloseHandle(eventKillErlang);
    }
    free_keys(keys);
    return;
  }
  set_start_pending(waithint,checkpoint);
  set_running();
  success_wait = INITIAL_SUCCESS_WAIT;
  harr[0] = srvi.info.hProcess;
  harr[1] = eventStop;
  for(;;){
    DWORD ret;
    ret = WaitForMultipleObjects((DWORD) 2,
				 harr,
				 FALSE,
				 (success_wait == NO_SUCCESS_WAIT) ? 
				 INFINITE :
				 SUCCESS_WAIT_TIME);
    if(ret == WAIT_TIMEOUT){
      /* Just do the "success reporting" and continue */
      if(success_wait == INITIAL_SUCCESS_WAIT){
	log_info("Erlang service started successfully.");
      } else {
	log_warning("Erlang service restarted");
      }
      success_wait = NO_SUCCESS_WAIT;
      continue;
    }
    if(ret == WAIT_FAILED || (int)(ret-WAIT_OBJECT_0) >= 2){
      set_stopped(WAIT_FAILED);
      log_error("Internal error, could not wait for objects.");
      if (eventKillErlang != NULL) {
	  CloseHandle(eventKillErlang);
      }
      free_keys(keys);
      return;
    }
    ret -= WAIT_OBJECT_0;
    if(((int) ret) == 1){
      /* Stop service... */
      checkpoint = 2; /* 1 is taken by the handler */
      set_stop_pending(waithint,checkpoint);
      if(stop_erlang(&srvi,waithint,&checkpoint)){
	log_debug("Erlang machine is stopped");
	CloseHandle(eventStop);
	if (eventKillErlang != NULL) {
	    CloseHandle(eventKillErlang);
	}
	set_stopped(NO_ERROR);
	if(srvi.erl_stdin)
	  CloseHandle(srvi.erl_stdin);
	free_keys(keys);
	return;
      } else {
	log_warning("Unable to stop erlang service.");
	set_running();
	continue;
      }
    }
    /* Reload the registry keys, they may have changed. */
    save_keys = keys;
    keys = get_keys(service_name);
    if(!keys){
      log_error("Could not reload registry keys.");
      keys = srvi.keys = save_keys;
    } else {
#ifdef HARDDEBUG
      sprintf(errorbuff,"Reloaded the registry keys because %s stopped.",
	      service_name);
      log_debug(errorbuff);
#endif /* HARDDEBUG */
      free_keys(save_keys);
      srvi.keys = keys;
    }
    if(srvi.keys[OnFail].data.value == ON_FAIL_RESTART || 
       srvi.keys[OnFail].data.value == ON_FAIL_RESTART_ALWAYS){
      if(!GetProcessTimes(srvi.info.hProcess,&creationt,
			  &exitt,&kernelt,&usert)){
	DWORD rcode = GetLastError();
	log_error("Could not get process time of terminated process.");
	CloseHandle(srvi.info.hProcess);
	CloseHandle(srvi.info.hThread);
	CloseHandle(eventStop);
	if(srvi.erl_stdin)
	  CloseHandle(srvi.erl_stdin);
	set_stopped(rcode);
	if (eventKillErlang != NULL) {
	    CloseHandle(eventKillErlang);
	}
	free_keys(keys);
	return;
      }
      CloseHandle(srvi.info.hProcess);
      CloseHandle(srvi.info.hThread);
      if(srvi.erl_stdin)
	CloseHandle(srvi.erl_stdin);
      srvi.erl_stdin = NULL;
      memcpy(&creationl,&creationt,sizeof(FILETIME));
      memcpy(&exitl,&exitt,sizeof(FILETIME));
      diffl = exitl - creationl;
      diffl /= 10000000;
#ifdef DEBUG
      sprintf(errorbuff,"Process lived for %d seconds", (int) diffl);
      log_debug(errorbuff);
#endif      

      if(diffl > CYCLIC_RESTART_LIMIT || 
	 srvi.keys[OnFail].data.value == ON_FAIL_RESTART_ALWAYS){
	if(!start_a_service(&srvi)){
	  log_error("Unable to restart failed erlang service, "
		    "aborting.");
	  CloseHandle(eventStop);
	  set_stopped(ERROR_PROCESS_ABORTED);
	  if (eventKillErlang != NULL) {
	      CloseHandle(eventKillErlang);
	  }
	  free_keys(keys);
	  return;
	}
	log_warning("Restarted erlang machine.");
	if(diffl <= CYCLIC_RESTART_LIMIT)
	  log_warning("Possible cyclic restarting of erlang machine.");
	success_wait = RESTART_SUCCESS_WAIT;
	harr[0] = srvi.info.hProcess;
      } else {
	if(success_wait == INITIAL_SUCCESS_WAIT){
	  log_error("Erlang machine stopped instantly "
		    "(distribution name conflict?). "
		    "The service is not restarted, ignoring OnFail option.");
	} else {
	  log_error("Erlang machine seems to die "
		    "continously, not restarted.");
	}
	CloseHandle(eventStop);
	set_stopped(ERROR_PROCESS_ABORTED);
	if (eventKillErlang != NULL) {
	    CloseHandle(eventKillErlang);
	}
	free_keys(keys);
	return;
      }
    } else if(srvi.keys[OnFail].data.value == ON_FAIL_REBOOT){
      log_error("Rebooting because erlang machine stopped.");
      enable_privilege();
      if(!InitiateSystemShutdown("",NULL,0,TRUE,TRUE)){
	log_error("Failed to reboot!");
#ifdef HARDDEBUG
	{
	  char *mes;
	  FormatMessage(
			FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
			NULL,    
			GetLastError(),
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
			(LPTSTR) &mes,    
			0,    
			NULL );
	  log_debug(mes);
	  LocalFree(mes);
	}
#endif 
	CloseHandle(srvi.info.hProcess);
	CloseHandle(eventStop);
	if(srvi.erl_stdin != NULL)
	  CloseHandle(srvi.erl_stdin);
	set_stopped(NO_ERROR);
	if (eventKillErlang != NULL) {
	    CloseHandle(eventKillErlang);
	}
	free_keys(keys);
	return;
      }
    } else {
      DWORD ecode = NO_ERROR;
      if(success_wait == NO_SUCCESS_WAIT){
	log_warning("Erlang machine voluntarily stopped. "
		    "The service is not restarted as OnFail "
		    "is set to ignore.");
      } else {
	log_error("Erlang machine stopped instantly "
		  "(distribution name conflict?). "
		  "The service is not restarted as OnFail is set to ignore.");
	ecode = ERROR_PROCESS_ABORTED;
      }
      CloseHandle(srvi.info.hProcess);
      CloseHandle(eventStop);
      if(srvi.erl_stdin != NULL)
	CloseHandle(srvi.erl_stdin);
      set_stopped(ecode);
      if (eventKillErlang != NULL) {
	  CloseHandle(eventKillErlang);
      }
      free_keys(keys);
      return;
    }      
  }
}

int service_main(int argc, char **argv){
  char dummy_name[] = "";
  SERVICE_TABLE_ENTRY serviceTable[] = 
  { 
    { dummy_name,
      (LPSERVICE_MAIN_FUNCTION) service_main_loop},
    { NULL, NULL }
  };
  BOOL success;
  success = 
       StartServiceCtrlDispatcher(serviceTable);
  if (!success)
    log_error("Could not initiate service");
  log_debug("service_main done its job");
  return 0;
}
  
