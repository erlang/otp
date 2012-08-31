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

char *progname;

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

#define RELEASE_SUBDIR "\\releases"
#define ERTS_SUBDIR_PREFIX "\\erts-"
#define BIN_SUBDIR "\\bin"
#define REGISTRY_BASE "Software\\Ericsson\\Erlang\\"
#define DEFAULT_DATAFILE "start_erl.data"

/* Global variables holding option values and command lines */
char *CommandLineStart = NULL;
char *ErlCommandLine = NULL;
char *MyCommandLine = NULL;
char *DataFileName = NULL;
char *RelDir = NULL;
char *BootFlagsFile = NULL;
char *BootFlags = NULL;
char *RegistryKey = NULL;
char *BinDir = NULL;
char *RootDir = NULL;
char *VsnDir = NULL;
char *Version = NULL;
char *Release = NULL;
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

    printf("Usage:\n%s\n"
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
    char	*cmdline = CommandLineStart;

    progname=cmdline;

    /* Remove the first (quoted) string (our program name) */
    if(*cmdline == '"') {
	cmdline++;         /* Skip " */
	while( (*cmdline != '\0') && (*cmdline++) != '"' )
	    ;
    } else {
	while( (*cmdline != '\0') && (*cmdline++) != ' ' )
	    ;
    }

    while( (*cmdline) == ' ' )
	cmdline++;

    if( *cmdline == '\0') {
	ErlCommandLine = "";
	MyCommandLine = "";
	return;
    }

    cmdline[-1] = '\0';

    /* Start from the end of the string and search for "++ " 
       (PLUS PLUS SPACE) */
    ErlCommandLine = cmdline;
    if(strncmp(cmdline,"++ ",3))
	cmdline = strstr(cmdline," ++ ");
    if( cmdline == NULL ) {
	MyCommandLine = "";
	return;
    }
    /* Terminate the ErlCommandLine where MyCommandLine starts */
    *cmdline++ = '\0';

    /* Skip 'whitespace--whitespace' (WHITESPACE MINUS MINUS WHITESPACE) */
    while( (*cmdline) == ' ' )
	cmdline++;
    while( (*cmdline) == '+' )
	cmdline++;
    while( (*cmdline) == ' ' )
	cmdline++;

    MyCommandLine = cmdline;

#ifdef _DEBUG
    fprintf(stderr, "ErlCommandLine: '%s'\n", ErlCommandLine);
    fprintf(stderr, "MyCommandLine:  '%s'\n", MyCommandLine);
#endif
}


/* 
 * 'Smart' unquoting of a string: \" becomes " and " becomes  (nothing)
 * Skips any leading spaces and parses up to NULL or end of quoted string.
 * Calls exit_help() if an unterminated quote is detected.
 */
char * unquote_optionarg(char *str, char **strp)
{
    char	*newstr = (char *)malloc(strlen(str)+1);  /* This one is 
							     realloc:ed later */
    int		i = 0, inquote = 0;

    assert(newstr);
    assert(str);

    /* Skip leading spaces */
    while( *str == ' ' )
	str++;

    /* Loop while in quote or until EOS or unquoted space 
     */
    while( (inquote==1)  || ( (*str!=0) && (*str!=' ') )  ) {
	switch( *str ) {
	case '\\':
	    /* If we are inside a quoted string we should convert \c to c */
	    if( inquote  && str[1] == '"' )
		str++;
	    newstr[i++]=*str++;
	    break;
	case '"':
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
    newstr = (char *)realloc(newstr, i);
    assert(newstr);
    return(newstr);
}


/*
 * Parses MyCommandLine and tries to fill in all the required option 
 * variables (in one way or another).
 */
void parse_commandline(void)
{
    char *cmdline = MyCommandLine;

    while( *cmdline != '\0' ) {
	switch( *cmdline ) {
	case '-':		/* Handle both -arg and /arg */
	case '/':
	    *cmdline++;
	    if( strnicmp(cmdline, "data", 4) == 0) {
		DataFileName = unquote_optionarg(cmdline+4, &cmdline);
	    } else if( strnicmp(cmdline, "rootdir", 7) == 0) {
		RootDir = unquote_optionarg(cmdline+7, &cmdline);
#ifdef _DEBUG
		fprintf(stderr, "RootDir: '%s'\n", RootDir);
#endif
	    } else if( strnicmp(cmdline, "reldir", 6) == 0) {
		RelDir = unquote_optionarg(cmdline+6, &cmdline);
#ifdef _DEBUG
		fprintf(stderr, "RelDir: '%s'\n", RelDir);
#endif
	    } else if( strnicmp(cmdline, "bootflags", 9) == 0) {
		BootFlagsFile = unquote_optionarg(cmdline+9, &cmdline);
	    } else if( strnicmp(cmdline, "noconfig", 8) == 0) {
		NoConfig=TRUE;
#ifdef _DEBUG
		fprintf(stderr, "NoConfig=TRUE\n");
#endif
	    } else {
		fprintf(stderr, "Unkown option: '%s'\n", cmdline);
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
    char	*newname;
    long	size;

    if(!DataFileName){ 
      DataFileName = malloc(strlen(DEFAULT_DATAFILE) + 1);
      strcpy(DataFileName,DEFAULT_DATAFILE);
    }
    /* Is DataFileName relative or absolute ? */
    if( (DataFileName[0] != '\\') && (strncmp(DataFileName+1, ":\\", 2)!=0) ) {
	/* Relative name, we have to prepend RelDir to it. */
	if( !RelDir ) {
	    exit_help("Need -reldir when -data filename has relative path.");
	} else {
	    newname = (char *)malloc(strlen(DataFileName)+strlen(RelDir)+2);
	    assert(newname);
	    sprintf(newname, "%s\\%s", RelDir, DataFileName);
	    free(DataFileName);
	    DataFileName=newname;
	}
    }

#ifdef _DEBUG
    fprintf(stderr, "DataFileName: '%s'\n", DataFileName);
#endif

    if( (fp=fopen(DataFileName, "rb")) == NULL) {
	exit_help("Cannot find the datafile.");
    }

    fseek(fp, 0, SEEK_END);
    size=ftell(fp);
    fseek(fp, 0, SEEK_SET);

    Version = (char *)malloc(size+1);
    Release = (char *)malloc(size+1);
    assert(Version);
    assert(Release);

    if( (fscanf(fp, "%s %s", Version, Release)) == 0) {
	fclose(fp);
	exit_help("Format error in datafile.");
    }

    fclose(fp);

#ifdef _DEBUG
    fprintf(stderr, "DataFile version: '%s'\n", Version);
    fprintf(stderr, "DataFile release: '%s'\n", Release);
#endif
}


/*
 * Read the bootflags. This file contains extra command line options to erl.exe
 */
void read_bootflags(void)
{
    FILE	*fp;
    long	fsize;
    char	*newname;
    
    if(BootFlagsFile) {
	/* Is BootFlagsFile relative or absolute ? */
	if( (BootFlagsFile[0] != '\\') && 
	    (strncmp(BootFlagsFile+1, ":\\", 2)!=0) ) {
	    /* Relative name, we have to prepend RelDir\\Version to it. */
	    if( !RelDir ) {
		exit_help("Need -reldir when -bootflags "
			  "filename has relative path.");
	    } else {
		newname = (char *)malloc(strlen(BootFlagsFile)+
					 strlen(RelDir)+strlen(Release)+3);
		assert(newname);
		sprintf(newname, "%s\\%s\\%s", RelDir, Release, BootFlagsFile);
		free(BootFlagsFile);
		BootFlagsFile=newname;
	    }
	}
	
#ifdef _DEBUG
	fprintf(stderr, "BootFlagsFile: '%s'\n", BootFlagsFile);
#endif
	
	if( (fp=fopen(BootFlagsFile, "rb")) == NULL) {
	    exit_help("Could not open BootFlags file.");
	}
	
	fseek(fp, 0, SEEK_END);
	fsize=ftell(fp);
	fseek(fp, 0, SEEK_SET);
	
	BootFlags = (char *)malloc(fsize+1);
	assert(BootFlags);
	if( (fgets(BootFlags, fsize+1, fp)) == NULL) {
	    exit_help("Error while reading BootFlags file");
	}
	fclose(fp);
	
	/* Adjust buffer size */
	BootFlags = (char *)realloc(BootFlags, strlen(BootFlags)+1);
	assert(BootFlags);
	
	/* Strip \r\n from BootFlags */
	fsize = strlen(BootFlags);
	while( fsize > 0 && 
	       ( (BootFlags[fsize-1] == '\r') || 
		(BootFlags[fsize-1] == '\n') ) ) {
	    BootFlags[--fsize]=0;
	}
	
    } else {
	/* Set empty BootFlags */
	BootFlags = "";
    }
    
#ifdef _DEBUG
    fprintf(stderr, "BootFlags: '%s'\n", BootFlags);
#endif
}


long start_new_node(void)
{
    char				*CommandLine;
    unsigned long		i;
    STARTUPINFO			si;
    DWORD				dwExitCode;

    i = strlen(RelDir) + strlen(Release) + 4;
    VsnDir = (char *)malloc(i);
    assert(VsnDir);
    sprintf(VsnDir, "%s\\%s", RelDir, Release);

    if( NoConfig ) {
	i = strlen(BinDir) + strlen(ErlCommandLine) + 
	    strlen(BootFlags) + 64;
	CommandLine = (char *)malloc(i);
	assert(CommandLine);
	sprintf(CommandLine, 
		"\"%s\\erl.exe\" -boot \"%s\\start\" %s %s", 
		BinDir, 
		VsnDir, 
		ErlCommandLine, 
		BootFlags);
    } else {
	i = strlen(BinDir) + strlen(ErlCommandLine) 
	    + strlen(BootFlags) + strlen(VsnDir)*2 + 64;
	CommandLine = (char *)malloc(i);
	assert(CommandLine);
	sprintf(CommandLine, 
		"\"%s\\erl.exe\" -boot \"%s\\start\" -config \"%s\\sys\" %s %s",  
		BinDir, 
		VsnDir, 
		VsnDir, 
		ErlCommandLine, 
		BootFlags);
    }

#ifdef _DEBUG
    fprintf(stderr, "CommandLine: '%s'\n", CommandLine);
#endif

    /* Initialize the STARTUPINFO structure. */
    memset(&si, 0, sizeof(STARTUPINFO));
    si.cb = sizeof(STARTUPINFO);
    si.lpTitle = NULL;
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    /* Create the new Erlang process */
    if( (CreateProcess(
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
    /* Try to find a descent RelDir */
    if( !RelDir ) {
	DWORD sz = 32;
	while (1) {
	    DWORD nsz;
	    if (RelDir)
		free(RelDir);
	    RelDir = malloc(sz);
	    if (!RelDir) {
		fprintf(stderr, "** Error : failed to allocate memory\n");
		exit(1);
	    }
	    SetLastError(0);
	    nsz = GetEnvironmentVariable((LPCTSTR) "RELDIR",
					 (LPTSTR) RelDir,
					 sz);
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
	  RelDir = (char *) malloc(strlen(RootDir)+strlen(RELEASE_SUBDIR)+1);
	  assert(RelDir);
	  sprintf(RelDir, "%s" RELEASE_SUBDIR, RootDir);
	  read_datafile();
	} else {
	    read_datafile();
	}
    } else {
	read_datafile();
    }
    if( !RootDir ) {
	/* Try to construct RootDir from RelDir */
	char *p;
	RootDir = malloc(strlen(RelDir)+1);
	strcpy(RootDir,RelDir);
	p = RootDir+strlen(RootDir)-1;
	if (p >= RootDir && (*p == '/' || *p == '\\'))
	    --p;
	while (p >= RootDir && *p != '/' &&  *p != '\\')
	    --p;
	if (p <= RootDir) { /* Empty RootDir is also an error */
	    exit_help("Cannot determine Root directory from "
		      "Release directory.");
	}
	*p = '\0';
    }
	    
    
    BinDir = (char *) malloc(strlen(RootDir)+strlen(ERTS_SUBDIR_PREFIX)+
			     strlen(Version)+strlen(BIN_SUBDIR)+1);
    assert(BinDir);
    sprintf(BinDir, "%s" ERTS_SUBDIR_PREFIX "%s" BIN_SUBDIR, RootDir, Version);

    read_bootflags();
    
#ifdef _DEBUG
    fprintf(stderr, "RelDir: '%s'\n", RelDir);
    fprintf(stderr, "BinDir: '%s'\n", BinDir);
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
    char	*cmdline;

    /* Make sure a logoff does not distrurb us. */
    SetConsoleCtrlHandler(LogoffHandlerRoutine, TRUE);
	
    cmdline = GetCommandLine();
    assert(cmdline);

    CommandLineStart = (char *) malloc(strlen(cmdline) + 1);
    assert(CommandLineStart);
    strcpy(CommandLineStart,cmdline);

    split_commandline();
    parse_commandline();
    complete_options();

    /* We now have all the options we need in order to fire up a new node.. */
    dwExitCode = start_new_node();

    return( (int) dwExitCode );
}


