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
/*
 * This is a simple command that gives some interesting
 * system information on NT.
 * It is run as a port program by the nt test suite to find out priorities
 * of programs etc.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(VXWORKS)
int nt_info(int argc, char **argv){
    printf("Hello Älvsjö!\n");
    return 0;
}
#elif !defined(__WIN32__)
int main(int argc, char **argv){
    printf("Hello Älvsjö!\n");
    return 0;
}
#else /* Windows NT, here we go... */

#include <windows.h>


int erlang_format = 0;

#if 0
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
#endif

typedef BOOL (WINAPI *tEnumProcesses)(DWORD *, DWORD, DWORD *);
typedef BOOL (WINAPI *tEnumProcessModules)(HANDLE, HMODULE *, DWORD, DWORD *);
typedef DWORD (WINAPI *tGetModuleBaseName)(HANDLE, HMODULE, char *, DWORD);

static tGetModuleBaseName pGetModuleBaseName = NULL;
static tEnumProcessModules pEnumProcessModules = NULL;
static tEnumProcesses pEnumProcesses = NULL;

static BOOL init_fpointers(void){
    HINSTANCE instance = LoadLibrary("PSAPI.DLL");
    if(instance == NULL){
	fprintf(stderr,"Failed to load PSAPI.DLL.\n");
	return FALSE;
    }
    if((pEnumProcesses = 
	(tEnumProcesses) GetProcAddress(instance,"EnumProcesses")) ==
       NULL){
	fprintf(stderr,"Failed to find EnumProcesses in DLL.\n");
	return FALSE;
    }
    if((pEnumProcessModules = 
	(tEnumProcessModules) GetProcAddress(instance,"EnumProcessModules")) ==
       NULL){
	fprintf(stderr,"Failed to find EnumProcessModules in DLL.\n");
	return FALSE;
    }
    if((pGetModuleBaseName = 
	(tGetModuleBaseName) GetProcAddress(instance,"GetModuleBaseNameA")) ==
       NULL){
	fprintf(stderr,"Failed to find GetModuleBaseName in DLL.\n");
	return FALSE;
    }
    return TRUE;
}
	

void one_line(DWORD pid){
    char pname[MAX_PATH] = "???";
    HMODULE hmod = NULL;
    DWORD dummy;
    DWORD priority = -1;
    struct {
	DWORD sym;
	char *txt;
    } tab[] = {
	{HIGH_PRIORITY_CLASS,"high"},
	{IDLE_PRIORITY_CLASS, "idle"},
	{NORMAL_PRIORITY_CLASS,"normal"},
	{REALTIME_PRIORITY_CLASS, "realtime"}
    };
    int tabsiz = sizeof(tab)/sizeof(*tab);
    char *class = "???";
    int i;

    HANDLE hproc = OpenProcess(PROCESS_QUERY_INFORMATION |
			       PROCESS_VM_READ,
			       FALSE, pid );
    if(!hproc)
	goto print;
    if(!(*pEnumProcessModules)(hproc,&hmod,sizeof(hmod),&dummy))
	goto print;
    if(!(*pGetModuleBaseName)(hproc,hmod,pname,sizeof(pname)))
	goto print;
    if(!(priority = GetPriorityClass(hproc)))
	goto print;
    for(i=0;i<tabsiz;++i)
	if(tab[i].sym == priority)
	    class = tab[i].txt;
print:
    if(erlang_format)
	printf("{\"%s\", %lu, \"%s\"}%s", pname, pid, class,
	       (erlang_format > 1) ? "" : "\n");
    else
	printf("%-32s %8lu %-9s\n", pname, pid, class);
    if(hproc)
	CloseHandle(hproc);
    if(hmod)
	CloseHandle(hmod);
}
	
int do_simple_ps(void){
    DWORD procs[1024];
    DWORD num_procs;
    DWORD needed;
    int i;

    if(!(*pEnumProcesses)(procs,sizeof(procs),&needed)){
	fprintf(stderr,"Failed to EnumProcesses\n");
	return 1;
    }
    num_procs = needed / sizeof(DWORD);
    if(erlang_format > 1)
	printf("[");
    for(i=0;i<num_procs;++i){
	one_line(procs[i]);
	if(erlang_format > 1 && i < num_procs -1)
	    printf(", ");
    }
    if(erlang_format > 1)
	printf("]. \n");
    return 0;
}

int main(int argc, char **argv){
    if(argc>1 && !strcmp(argv[1],"-e"))
	erlang_format = 1;
    else if(argc>1 && !strcmp(argv[1],"-E"))
	erlang_format = 2;
    if(!init_fpointers())
	return 1;
    return do_simple_ps();
}
#endif
