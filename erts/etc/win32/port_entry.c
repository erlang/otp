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
** This is an entry point for port programs,
** it is used to set the console control handler of the process when
** erlang process is run as a service.
** Note that this entry point is only for 
** Console programs, Windowing programs can just route the WM_QUERYENDSESSION
** and WM_ENDSESSION to the default window procedure to aquire the same 
** functionality.
**
** Creator Patrik Nyblom
**
** Notes:
** You would really not want to use ANY of the standard library in this 
** routine, the standard library is not yet initiated...
*/
#include <windows.h>

/* 
** The runtime libraries startup routine in the Microsoft Visual C CRT
*/
extern void mainCRTStartup(void);

/* 
** A Console control handler that ignores the logoff events,
** and lets the default handler take care of other events.
*/   
BOOL WINAPI erl_port_default_handler(DWORD ctrl){
    if(ctrl == CTRL_LOGOFF_EVENT)
	return TRUE;
    if(ctrl == CTRL_SHUTDOWN_EVENT)
	return TRUE;
    return FALSE;
}

/*
** This is the entry point, it takes no parameters and never returns.
*/
void erl_port_entry(void){
    char buffer[2];
    /* 
     * We assume we're running as a service if this environment variable
     * is defined
     */
    if(GetEnvironmentVariable("ERLSRV_SERVICE_NAME",buffer,(DWORD) 2)){
#ifdef HARDDEBUG
	DWORD dummy;
	WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),
		  "Setting handler\r\n",17,&dummy, NULL);
#endif /* HARDDEBUG */ 
	/*
	** Actually set the control handler
	*/
	SetConsoleCtrlHandler(&erl_port_default_handler, TRUE);
    }
    /* 
    ** Call the CRT's real startup routine.
    */
    mainCRTStartup();
}
