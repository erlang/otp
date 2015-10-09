/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2012. All Rights Reserved.
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
 * start_erl.c - Windows NT start_erl
 *
 * Author: Mattias Nilsson
 */

#define WIN32_LEAN_AND_MEAN
#define STRICT
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <assert.h>

wchar_t *progname;

/*
 * If CASE_SENSITIVE_OPTIONS is specified, options are case sensitive 
 * (lower case).
 * The reason for this switch is that _strnicmp is Microsoft specific.
 */
#ifndef CASE_SENSITIVE_OPTIONS
#define strnicmp strncmp
#else
#define strnicmp _strnicmp
#endif

#define RELEASE_SUBDIR L"\\releases"
#define ERTS_SUBDIR_PREFIX L"\\erts-"
#define BIN_SUBDIR L"\\bin"
#define REGISTRY_BASE L"Software\\Ericsson\\Erlang\\"
#define DEFAULT_DATAFILE L"start_erl.data"

/* Global variables holding option values and command lines */
wchar_t *CommandLineStart = NULL;
wchar_t *ErlCommandLine = NULL;
wchar_t *MyCommandLine = NULL;
wchar_t *DataFileName = NULL;
wchar_t *RelDir = NULL;
wchar_t *BootFlagsFile = NULL;
wchar_t *BootFlags = NULL;
wchar_t *RegistryKey = NULL;
wchar_t *BinDir = NULL;
wchar_t *RootDir = NULL;
wchar_t *VsnDir = NULL;
wchar_t *Version = NULL;
wchar_t *Release = NULL;
BOOL NoConfig=FALSE;
PROCESS_INFORMATION ErlProcessInfo;

/*
 * Error reason printout 'the microsoft way'
 */
void ShowLastError(void)
{
    LPVOID	lpMsgBuf;
    DWORD	dwErr;

    dwErr = GetLastError();
    if( dwErr == ERROR_SUCCESS )
	return;

    FormatMessage( 
		  FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		  NULL,
		  dwErr,
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
		  (LPTSTR) &lpMsgBuf,
		  0,
		  NULL 
		  );
    fprintf(stderr, lpMsgBuf);
    LocalFree( lpMsgBuf );
}

/*
 * Exit the program and give a nice and firm explanation of why
 * and how you can avoid it.
 */
void exit_help(char *err)
{
    ShowLastError();
    fprintf(stderr, "** Error: %s\n", err);

    printf("Usage:\n%S\n"
	   "      [<erlang options>] ++\n"
	   "      [-data <datafile>]\n"
	   "      {-rootdir <erlang root directory> | \n"
	   "       -reldir <releasedir>}\n"
	   "      [-bootflags <bootflagsfile>]\n"
	   "      [-noconfig]\n", progname);

    exit(0);
}


/*
 * Splits the command line into two strings:
 * 1. Options to the Erlang node (ErlCommandLine)
 * 2. Options to this program (MyCommandLine)
 */
void split_commandline(void)
{
    wchar_t	*cmdline = CommandLineStart;

    progname=cmdline;

    /* Remove the first (quoted) string (our program name) */
    if(*cmdline == L'"') {
	cmdline++;         /* Skip " */
	while( (*cmdline != L'\0') && (*cmdline++) != L'"' )
	    ;
    } else {
	while( (*cmdline != L'\0') && (*cmdline++) != L' ' )
	    ;
    }

    while( (*cmdline) == L' ' )
	cmdline++;

    if( *cmdline == L'\0') {
	ErlCommandLine = L"";
	MyCommandLine = L"";
	return;
    }

    cmdline[-1] = L'\0';

    /* Start from the end of the string and search for "++ " 
       (PLUS PLUS SPACE) */
    ErlCommandLine = cmdline;
    if(wcsncmp(cmdline,L"++ ",3))
	cmdline = wcsstr(cmdline,L" ++ ");
    if( cmdline == NULL ) {
	MyCommandLine = L"";
	return;
    }
    /* Terminate the ErlCommandLine where MyCommandLine starts */
    *cmdline++ = '\0';

    /* Skip 'whitespace--whitespace' (WHITESPACE MINUS MINUS WHITESPACE) */
    while( (*cmdline) == L' ' )
	cmdline++;
    while( (*cmdline) == L'+' )
	cmdline++;
    while( (*cmdline) == L' ' )
	cmdline++;

    MyCommandLine = cmdline;

#ifdef _DEBUG
    fprintf(stderr, "ErlCommandLine: '%S'\n", ErlCommandLine);
    fprintf(stderr, "MyCommandLine:  '%S'\n", MyCommandLine);
#endif
}


/* 
 * 'Smart' unquoting of a string: \" becomes " and " becomes  (nothing)
 * Skips any leading spaces and parses up to NULL or end of quoted string.
 * Calls exit_help() if an unterminated quote is detected.
 */
wchar_t * unquote_optionarg(wchar_t *str, wchar_t **strp)
{
    /* This one is realloc:ed later */
    wchar_t	*newstr = (wchar_t *)malloc((wcslen(str)+1)*sizeof(wchar_t));
    int		i = 0, inquote = 0;

    assert(newstr);
    assert(str);

    /* Skip leading spaces */
    while( *str == L' ' )
	str++;

    /* Loop while in quote or until EOS or unquoted space 
     */
    while( (inquote==1)  || ( (*str!=0) && (*str!=L' ') )  ) {
	switch( *str ) {
	case L'\\':
	    /* If we are inside a quoted string we should convert \c to c */
	    if( inquote  && str[1] == L'"' )
		str++;
	    newstr[i++]=*str++;
	    break;
	case L'"':
	    inquote = 1-inquote;
	    *str++;
	    break;
	default:
	    newstr[i++]=*str++;
	    break;
	}

	if( (*str == 0) && (inquote==1) ) {
	    exit_help("Unterminated quote.");
	}
    }
    newstr[i++] = 0x00;

    /* Update the supplied pointer (used for continued parsing of options) */
    *strp = str;

    /* Adjust memblock of newstr */
    newstr = (wchar_t *)realloc(newstr, i*sizeof(wchar_t));
    assert(newstr);
    return(newstr);
}


/*
 * Parses MyCommandLine and tries to fill in all the required option 
 * variables (in one way or another).
 */
void parse_commandline(void)
{
    wchar_t *cmdline = MyCommandLine;

    while( *cmdline != L'\0' ) {
	switch( *cmdline ) {
	case '-':		/* Handle both -arg and /arg */
	case '/':
	    *cmdline++;
	    if( _wcsnicmp(cmdline, L"data", 4) == 0) {
		DataFileName = unquote_optionarg(cmdline+4, &cmdline);
	    } else if( _wcsnicmp(cmdline, L"rootdir", 7) == 0) {
		RootDir = unquote_optionarg(cmdline+7, &cmdline);
#ifdef _DEBUG
		fprintf(stderr, "RootDir: '%S'\n", RootDir);
#endif
	    } else if( _wcsnicmp(cmdline, L"reldir", 6) == 0) {
		RelDir = unquote_optionarg(cmdline+6, &cmdline);
#ifdef _DEBUG
		fprintf(stderr, "RelDir: '%S'\n", RelDir);
#endif
	    } else if( _wcsnicmp(cmdline, L"bootflags", 9) == 0) {
		BootFlagsFile = unquote_optionarg(cmdline+9, &cmdline);
	    } else if( _wcsnicmp(cmdline, L"noconfig", 8) == 0) {
		NoConfig=TRUE;
#ifdef _DEBUG
		fprintf(stderr, "NoConfig=TRUE\n");
#endif
	    } else {
		fprintf(stderr, "Unkown option: '%S'\n", cmdline);
		exit_help("Unknown command line option");
	    }
	    break;
	default:
	    cmdline++;
	    break;
	}
    }
}


/*
 * Read the data file specified and get the version and release number
 * from it.
 *
 * This function also construct the correct RegistryKey from the version 
 * information retrieved.
 */
void read_datafile(void)
{
    FILE	*fp;
    wchar_t	*newname;
    long	size;
    char        *ver;
    char        *rel;

    if(!DataFileName){
	DataFileName = malloc((wcslen(DEFAULT_DATAFILE) + 1)*sizeof(wchar_t));
	wcscpy(DataFileName,DEFAULT_DATAFILE);
    }
    /* Is DataFileName relative or absolute ? */
    if( (DataFileName[0] != L'\\') && (wcsncmp(DataFileName+1, L":\\", 2)!=0) ) {
	/* Relative name, we have to prepend RelDir to it. */
	if( !RelDir ) {
	    exit_help("Need -reldir when -data filename has relative path.");
	} else {
	    size = (wcslen(DataFileName)+wcslen(RelDir)+2);
	    newname = (wchar_t *)malloc(size*sizeof(wchar_t));
	    assert(newname);
	    swprintf(newname, size, L"%s\\%s", RelDir, DataFileName);
	    free(DataFileName);
	    DataFileName=newname;
	}
    }

#ifdef _DEBUG
    fprintf(stderr, "DataFileName: '%S'\n", DataFileName);
#endif

    if( (fp=_wfopen(DataFileName, L"rb")) == NULL) {
	exit_help("Cannot find the datafile.");
    }

    fseek(fp, 0, SEEK_END);
    size=ftell(fp);
    fseek(fp, 0, SEEK_SET);

    ver = (char *)malloc(size+1);
    rel = (char *)malloc(size+1);
    assert(ver);
    assert(rel);

    if( (fscanf(fp, "%s %s", ver, rel)) == 0) {
	fclose(fp);
	exit_help("Format error in datafile.");
    }

    fclose(fp);

    size = MultiByteToWideChar(CP_UTF8, 0, ver, -1, NULL, 0);
    Version = malloc(size*sizeof(wchar_t));
    assert(Version);
    MultiByteToWideChar(CP_UTF8, 0, ver, -1, Version, size);
    free(ver);

    size = MultiByteToWideChar(CP_UTF8, 0, rel, -1, NULL, 0);
    Release = malloc(size*sizeof(wchar_t));
    assert(Release);
    MultiByteToWideChar(CP_UTF8, 0, rel, -1, Release, size);
    free(rel);

#ifdef _DEBUG
    fprintf(stderr, "DataFile version: '%S'\n", Version);
    fprintf(stderr, "DataFile release: '%S'\n", Release);
#endif
}


/*
 * Read the bootflags. This file contains extra command line options to erl.exe
 */
void read_bootflags(void)
{
    FILE	*fp;
    long	fsize;
    wchar_t	*newname;
    char        *bootf;

    if(BootFlagsFile) {
	/* Is BootFlagsFile relative or absolute ? */
	if( (BootFlagsFile[0] != L'\\') && 
	    (wcsncmp(BootFlagsFile+1, L":\\", 2)!=0) ) {
	    /* Relative name, we have to prepend RelDir\\Version to it. */
	    if( !RelDir ) {
		exit_help("Need -reldir when -bootflags "
			  "filename has relative path.");
	    } else {
		int len = wcslen(BootFlagsFile)+
		    wcslen(RelDir)+wcslen(Release)+3;
		newname = (wchar_t *)malloc(len*sizeof(wchar_t));
		assert(newname);
		swprintf(newname, len, L"%s\\%s\\%s", RelDir, Release, BootFlagsFile);
		free(BootFlagsFile);
		BootFlagsFile=newname;
	    }
	}
	
#ifdef _DEBUG
	fprintf(stderr, "BootFlagsFile: '%S'\n", BootFlagsFile);
#endif
	
	if( (fp=_wfopen(BootFlagsFile, L"rb")) == NULL) {
	    exit_help("Could not open BootFlags file.");
	}
	
	fseek(fp, 0, SEEK_END);
	fsize=ftell(fp);
	fseek(fp, 0, SEEK_SET);
	
	bootf = (char *)malloc(fsize+1);
	assert(bootf);
	if( (fgets(bootf, fsize+1, fp)) == NULL) {
	    exit_help("Error while reading BootFlags file");
	}
	fclose(fp);
	
	/* Adjust buffer size */
	bootf = (char *)realloc(bootf, strlen(bootf)+1);
	assert(bootf);
	
	/* Strip \r\n from BootFlags */
	fsize = strlen(bootf);
	while( fsize > 0 && 
	       ( (bootf[fsize-1] == '\r') || 
		(bootf[fsize-1] == '\n') ) ) {
	    bootf[--fsize]=0;
	}
	fsize = MultiByteToWideChar(CP_UTF8, 0, bootf, -1, NULL, 0);
	BootFlags = malloc(fsize*sizeof(wchar_t));
	assert(BootFlags);
	MultiByteToWideChar(CP_UTF8, 0, bootf, -1, BootFlags, fsize);
	free(bootf);
    } else {
	/* Set empty BootFlags */
	BootFlags = L"";
    }
    
#ifdef _DEBUG
    fprintf(stderr, "BootFlags: '%S'\n", BootFlags);
#endif
}


long start_new_node(void)
{
    wchar_t			*CommandLine;
    unsigned long		i;
    STARTUPINFOW		si;
    DWORD			dwExitCode;

    i = wcslen(RelDir) + wcslen(Release) + 4;
    VsnDir = (wchar_t *)malloc(i*sizeof(wchar_t));
    assert(VsnDir);
    swprintf(VsnDir, i, L"%s\\%s", RelDir, Release);

    if( NoConfig ) {
	i = wcslen(BinDir) + wcslen(ErlCommandLine) +
	    wcslen(BootFlags) + 64;
	CommandLine = (wchar_t *)malloc(i*sizeof(wchar_t));
	assert(CommandLine);
	swprintf(CommandLine,
		 i,
		 L"\"%s\\erl.exe\" -boot \"%s\\start\" %s %s",
		 BinDir,
		 VsnDir,
		 ErlCommandLine,
		 BootFlags);
    } else {
	i = wcslen(BinDir) + wcslen(ErlCommandLine)
	    + wcslen(BootFlags) + wcslen(VsnDir)*2 + 64;
	CommandLine = (wchar_t *)malloc(i*sizeof(wchar_t));
	assert(CommandLine);
	swprintf(CommandLine,
		 i,
		 L"\"%s\\erl.exe\" -boot \"%s\\start\" -config \"%s\\sys\" %s %s",
		 BinDir,
		 VsnDir,
		 VsnDir,
		 ErlCommandLine,
		 BootFlags);
    }

#ifdef _DEBUG
    fprintf(stderr, "CommandLine: '%S'\n", CommandLine);
#endif

    /* Initialize the STARTUPINFO structure. */
    memset(&si, 0, sizeof(STARTUPINFOW));
    si.cb = sizeof(STARTUPINFOW);
    si.lpTitle = NULL;
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    /* Create the new Erlang process */
    if( (CreateProcessW(
			NULL,  /* pointer to name of executable module    */
			CommandLine,	/* pointer to command line string    */
			NULL,  /* pointer to process security attributes  */
			NULL,  /* pointer to thread security attributes   */
			TRUE,  /* handle inheritance flag                 */
			GetPriorityClass(GetCurrentProcess()),	
			/* creation flags                          */
			NULL,  /* pointer to new environment block        */
			BinDir,/* pointer to current directory name       */
			&si,	  /* pointer to STARTUPINFO                  */
			&ErlProcessInfo /* pointer to PROCESS_INFORMATION */
			)) == FALSE) {
	ShowLastError();
	exit_help("Failed to start new node");
    }

#ifdef _DEBUG
    fprintf(stderr, "Waiting for Erlang to terminate.\n");
#endif
    if(MsgWaitForMultipleObjects(1,&ErlProcessInfo.hProcess, FALSE, 
				 INFINITE, QS_POSTMESSAGE) == WAIT_OBJECT_0+1){
	if(PostThreadMessage(ErlProcessInfo.dwThreadId,
			     WM_USER,
			     (WPARAM) 0, 
			     (LPARAM) 0)){
	    /* Wait 10 seconds for erl process to die, elsee terminate it. */
	    if(WaitForSingleObject(ErlProcessInfo.hProcess, 10000) 
	       != WAIT_OBJECT_0){
		TerminateProcess(ErlProcessInfo.hProcess,0);
	    }
	} else {
	   TerminateProcess(ErlProcessInfo.hProcess,0);
	}
    } 
    GetExitCodeProcess(ErlProcessInfo.hProcess, &dwExitCode);
#ifdef _DEBUG
    fprintf(stderr, "Erlang terminated.\n");
#endif

    free(CommandLine);
    return(dwExitCode);
}


/*
 * Try to make the needed options complete by looking through the data file,
 * environment variables and registry entries.
 */
void complete_options(void)
{
    int len;
    /* Try to find a descent RelDir */
    if( !RelDir ) {
	DWORD sz = 32;
	while (1) {
	    DWORD nsz;
	    if (RelDir)
		free(RelDir);
	    RelDir = malloc(sz*sizeof(wchar_t));
	    if (!RelDir) {
		fprintf(stderr, "** Error : failed to allocate memory\n");
		exit(1);
	    }
	    SetLastError(0);
	    nsz = GetEnvironmentVariableW(L"RELDIR", RelDir, sz);
	    if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
		free(RelDir);
		RelDir = NULL;
		break;
	    }
	    else if (nsz <= sz)
		break;
	    else
		sz = nsz;
	}
	if (RelDir == NULL) {
	  if (!RootDir) {
	    /* Impossible to find all data... */
	    exit_help("Need either Root directory nor Release directory.");
	  }
	  /* Ok, construct our own RelDir from RootDir */
          sz = wcslen(RootDir)+wcslen(RELEASE_SUBDIR)+1;
	  RelDir = (wchar_t *) malloc(sz * sizeof(wchar_t));
	  assert(RelDir);
	  swprintf(RelDir, sz, L"%s" RELEASE_SUBDIR, RootDir);
	  read_datafile();
	} else {
	    read_datafile();
	}
    } else {
	read_datafile();
    }
    if( !RootDir ) {
	/* Try to construct RootDir from RelDir */
	wchar_t *p;
	RootDir = malloc((wcslen(RelDir)+1)*sizeof(wchar_t));
	wcscpy(RootDir,RelDir);
	p = RootDir+wcslen(RootDir)-1;
	if (p >= RootDir && (*p == L'/' || *p == L'\\'))
	    --p;
	while (p >= RootDir && *p != L'/' &&  *p != L'\\')
	    --p;
	if (p <= RootDir) { /* Empty RootDir is also an error */
	    exit_help("Cannot determine Root directory from "
		      "Release directory.");
	}
	*p = L'\0';
    }
	    
    len = wcslen(RootDir)+wcslen(ERTS_SUBDIR_PREFIX)+
	wcslen(Version)+wcslen(BIN_SUBDIR)+1;
    BinDir = (wchar_t *) malloc(len * sizeof(wchar_t));
    assert(BinDir);
    swprintf(BinDir, len, L"%s" ERTS_SUBDIR_PREFIX L"%s" BIN_SUBDIR, RootDir, Version);

    read_bootflags();
    
#ifdef _DEBUG
    fprintf(stderr, "RelDir: '%S'\n", RelDir);
    fprintf(stderr, "BinDir: '%S'\n", BinDir);
#endif
}




BOOL WINAPI LogoffHandlerRoutine( DWORD dwCtrlType )
{
    if(dwCtrlType == CTRL_LOGOFF_EVENT) {
	return TRUE;
    }
    if(dwCtrlType == CTRL_SHUTDOWN_EVENT) {
	return TRUE;
    }

    return FALSE;
}




int main(void)
{
    DWORD	dwExitCode;
    wchar_t	*cmdline;

    /* Make sure a logoff does not distrurb us. */
    SetConsoleCtrlHandler(LogoffHandlerRoutine, TRUE);
	
    cmdline = GetCommandLineW();
    assert(cmdline);

    CommandLineStart = (wchar_t *) malloc((wcslen(cmdline) + 1)*sizeof(wchar_t));
    assert(CommandLineStart);
    wcscpy(CommandLineStart,cmdline);

    split_commandline();
    parse_commandline();
    complete_options();

    /* We now have all the options we need in order to fire up a new node.. */
    dwExitCode = start_new_node();

    return( (int) dwExitCode );
}


