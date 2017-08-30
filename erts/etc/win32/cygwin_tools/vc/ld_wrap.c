/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <dirent.h>
#include <sys/cygwin.h>

    

#ifdef CCP_POSIX_TO_WIN_A
#define NEW_CYGPATH_INTERFACE
#endif

#ifdef NEW_CYGPATH_INTERFACE
#define GET_WIN32_SIZE(Posix) \
cygwin_conv_path (CCP_POSIX_TO_WIN_A | CCP_ABSOLUTE, (Posix), NULL, 0)
#define CONVERT_TO_WIN32(Posix,Win32,Size) \
cygwin_conv_path (CCP_POSIX_TO_WIN_A | CCP_ABSOLUTE, (Posix), \
		  (Win32), (Size))
#else
#define GET_WIN32_SIZE(Posix) PATH_MAX
#define CONVERT_TO_WIN32(Posix,Win32,Size) \
((cygwin32_conv_to_full_win32_path((Posix),(Win32)) >= 0) ? 0 : -1)
#endif

/*#define HARDDEBUG 1*/

#ifdef HARDDEBUG
#define DEBUGF(X) printf X
#else
#define DEBUGF(X) /* noop */
#endif
char *tmpobjdir = "";

char *add_to(char *src,char *add) {
    int len = strlen(src)+strlen(add)+1;
    char *n;

    if (strlen(src) == 0) {
	n = malloc(len);
	strcpy(n,add);
	return n;
    }
    n = realloc(src,len);
    strcat(n,add);
    return n;
}

void error(char *str) 
{
    fprintf(stderr,"%s\n",str);
    exit(1);
}


char *dyn_get_short(char *longp) 
{
    int size;
    char *shortp;
    size = GetShortPathName(longp,NULL,0);
    if (size <= 0) {
	return NULL;
    }
    shortp = malloc(size);
    if (GetShortPathName(longp,shortp,size) != size - 1) {
	free(shortp);
	return NULL;
    }
    return shortp;
}

char *do_cyp(char *posix)
{
    ssize_t size;
    char *win32;
    size = GET_WIN32_SIZE(posix);
    char *ret = NULL;
    if (size < 0) {
	fprintf(stderr,"Could not cygpath %s, errno = %d\n",
		posix,errno);
    } else {
	win32 = (char *) malloc (size);
	if (CONVERT_TO_WIN32(posix,
			     win32, size)) {
	    fprintf(stderr,"Could not cygpath %s, errno = %d\n",
		    posix,errno);
	} else {
	    char *w32_short = dyn_get_short(win32);
	    DEBUGF(("win32 = %s, w32_short = %s\n",win32, (w32_short == NULL) ? "NULL" : w32_short));
	    if (w32_short == NULL) {
		char *rest = malloc(size);
		char *first = malloc(size);
		int x = 0;
		int y = strlen(win32) - 1;
		strcpy(first,win32);
		while (w32_short == NULL) {
		    while ( y > 0 && first[y] != '\\') {
			rest[x++] = first[y--];
		    }
		    if (y > 0) {
			rest[x++] = first[y];
			first[y--] = '\0'; 
		    } else {
			break;
		    }
		    w32_short = dyn_get_short(first);
		    DEBUGF(("first = %s, w32_short = %s\n",first, (w32_short == NULL) ? "NULL" : w32_short));
		}
		if (w32_short != NULL) {
		    y = strlen(w32_short);
		    w32_short = realloc(w32_short,y+1+x);
		    /* spool back */
		    while ( x > 0) {
			w32_short[y++] = rest[--x];
		    }
		    w32_short[y] = '\0';
		} else {
		    w32_short = malloc(strlen(win32)+1);
		    strcpy(w32_short,win32); /* last resort */
		}
		free(first);
		free(rest);
	    }   
	    ret = w32_short;
	    while (*ret) {
		if (*ret == '\\') {
		    *ret = '/';
		}
		++ret;
	    }
	    ret = w32_short;
	}    
	free(win32);
    }
    return ret;
}



char *save = "";

void save_args(int argc, char **argv) 
{
    int i;
    for(i = 0; i < argc; ++i) {
	save = add_to(save,argv[i]);
	save = add_to(save," ");
    }
}

char *progname="ld_wrap";

int my_create_pipe(HANDLE *read_p, HANDLE *write_p)
{
    char name_buff[1000];
    SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};
    static int counter = 0;

    ++counter;

    sprintf(name_buff,"\\\\.\\pipe\\%s_%d_%d",progname,getpid(),counter);
    sa.bInheritHandle = FALSE;
    if ((*read_p = CreateNamedPipe(name_buff,
				   PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED,
				   PIPE_TYPE_BYTE | PIPE_READMODE_BYTE,
				   1,
				   0,
				   0,
				   2000,
				   &sa)) == INVALID_HANDLE_VALUE || 
	*read_p == NULL) {
	return 0;
    }
    sa.bInheritHandle = TRUE;
    if ((*write_p = CreateFile(name_buff,
			       GENERIC_WRITE,
			       0, /* No sharing */
			       &sa,
			       OPEN_EXISTING,
			       FILE_ATTRIBUTE_NORMAL,
			       NULL)) == INVALID_HANDLE_VALUE || 
	*write_p == NULL) {
	CloseHandle(*read_p);
	return 0;
    }
    return 1;
}

void forwardenv(void)
{
  char *(envs[]) = {"LIB","INCLUDE","LIBPATH", NULL};
  char **p = envs;
  while (*p != NULL) {
    char *val = getenv(*p);
    if (val != NULL) {
      SetEnvironmentVariable(*p,val);
    }
    ++p;
  }
}

HANDLE do_run(char *commandline, HANDLE *out, HANDLE *err) 
{
    STARTUPINFO start;
    HANDLE write_pipe_stdout = NULL, read_pipe_stdout = NULL;
    HANDLE write_pipe_stderr = NULL, read_pipe_stderr = NULL;
    SECURITY_ATTRIBUTES pipe_security;
    SECURITY_ATTRIBUTES attr;
    PROCESS_INFORMATION info;


    memset(&start,0,sizeof(start));
    memset(&pipe_security,0,sizeof(pipe_security));
    memset(&attr,0,sizeof(attr));
    memset(&info,0,sizeof(info));


    pipe_security.nLength = sizeof(pipe_security);
    pipe_security.lpSecurityDescriptor = NULL;
    pipe_security.bInheritHandle = TRUE;

    if(!my_create_pipe(&read_pipe_stdout,&write_pipe_stdout)){
	error("Could not create stdout pipes!");
    }
    if(!my_create_pipe(&read_pipe_stderr,&write_pipe_stderr)){
	error("Could not create stderr pipes!");
    }
    start.cb = sizeof (start);
    start.dwFlags = STARTF_USESHOWWINDOW;
    start.wShowWindow = SW_HIDE;
    start.hStdOutput = write_pipe_stdout;
    start.hStdError = write_pipe_stderr;
    start.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    start.dwFlags |= STARTF_USESTDHANDLES;

    attr.nLength = sizeof(attr);
    attr.lpSecurityDescriptor = NULL;
    attr.bInheritHandle = TRUE;
    forwardenv(); /* Cygwin and windows environment variables... sigh... */
    if(!CreateProcess(NULL,
		      commandline,
		      &attr,
		      NULL,
		      TRUE,
		      CREATE_DEFAULT_ERROR_MODE,
		      NULL,
		      NULL,
		      &start,
		      &info)){
	error("Could not create process");
    }
    *out = read_pipe_stdout;
    *err = read_pipe_stderr;
    CloseHandle(write_pipe_stdout);
    CloseHandle(write_pipe_stderr);
    return info.hProcess;
}
#define HANDLE_STDOUT 0
#define HANDLE_STDERR 1
#define HANDLE_PROC 2

#ifdef HARDDEBUG
char *prefix = "";
#endif

int handle_overlapped(HANDLE fd, OVERLAPPED *ovp, char *buffer, 
		      int bufflen, int get_old, FILE *whereto, int *skip)
{
    DWORD res,read,err;
    char *ptr;

    DEBUGF(("In handle_overlapped(%d,0x%08x,0x%08x,%d,%d), prefix = %s\n",
	    fd,ovp,buffer,bufflen,get_old,prefix));
    /* hämta resultat av gamla först */
    if (get_old) {
	res = GetOverlappedResult(fd,ovp,&read,TRUE);
	DEBUGF(("read = %d, res = %d, GetLastError() = %d\n",read,res,GetLastError()));
	if (!res) {
	    return 0;
	}
	buffer[read] = '\0';
	ptr = buffer;
	while(*skip && *ptr != '\0') {
	    if (*ptr == '\n') {
		--(*skip);
	    }
	    ++ptr;
	}
	if(*ptr != '\0') {
	    fprintf(whereto,"%s",ptr);
	}
    }

    ResetEvent(ovp->hEvent);

    for(;;) {
	res = ReadFile(fd,buffer,bufflen-1,&read,ovp);

	if (!res) {
	    err = GetLastError();
	    if (err == ERROR_IO_PENDING) {
		DEBUGF(("Error I/O Pending\n"));
		return 1;
	    } 
	    DEBUGF(("ReadFileFailed for %s, %d\n",prefix,err));
	    return 0;
	}
	buffer[read] = '\0';
	ptr = buffer;
	while(*skip && *ptr != '\0') {
	    if (*ptr == '\n') {
		--(*skip);
	    }
	    ++ptr;
	}
	if(*ptr != '\0') {
	    fprintf(whereto,"%s",ptr);
	}
    }
}


int run(char *commandline,int skipout,int skiperr)
{
    HANDLE harr[3];
    HANDLE real_stdout,real_stderr;
    OVERLAPPED ov_out,ov_err;
    char outbuff[1024],errbuff[1024];
    DWORD ret,exitcode;
    HANDLE wait[3];
    int map[3];
    DWORD nwait = 3;
    int i,j;
    unsigned living_handles = 0x7;

    harr[HANDLE_STDOUT] = CreateEvent(NULL,
				      TRUE,
				      FALSE, /*not signalled */
				      NULL);  
    harr[HANDLE_STDERR] = CreateEvent(NULL,
				      TRUE,
				      FALSE,/*not signalled */
				      NULL);  

    memset(&ov_out,0,sizeof(ov_out));
    memset(&ov_err,0,sizeof(ov_err));

    ov_out.hEvent = harr[HANDLE_STDOUT];
    ov_err.hEvent = harr[HANDLE_STDERR];

    harr[HANDLE_PROC] = do_run(commandline,&real_stdout,&real_stderr);

#ifdef HARDDEBUG
    prefix = "STDOUT";
#endif
    handle_overlapped(real_stdout,&ov_out,outbuff,1024,0,stdout,&skipout);
#ifdef HARDDEBUG
    prefix = "STDERR";
#endif
    handle_overlapped(real_stderr,&ov_err,errbuff,1024,0,stderr,&skiperr);

    for(;;) {
	nwait = 0;
        for(i=0;i<3;++i) {
	    if ((living_handles & (1U << i))) {
		map[nwait] = i;
		wait[nwait++] = harr[i];
	    }
	}

	ret = WaitForMultipleObjects(nwait,
				     wait,
				     FALSE,
				     INFINITE);
	DEBUGF(("Wait returned %d\n",ret));

	if (ret == WAIT_FAILED) {
	    error("Wait failed");
	}

	ret -= WAIT_OBJECT_0;
	
	switch (map[ret]) {
	case HANDLE_PROC:
	    
	    DEBUGF(("Process died!\n"));
	    GetExitCodeProcess(harr[HANDLE_PROC],&exitcode);
	    if ((living_handles &= (~(1U<<HANDLE_PROC))) == 0) {
		goto done;
	    }
	    --nwait;
	    break;
	case HANDLE_STDOUT:
#ifdef HARDDEBUG
	    prefix = "STDOUT";
#endif
	    if (!handle_overlapped(real_stdout,&ov_out, outbuff,1024,1,stdout,&skipout)) {
		if ((living_handles &= (~(1U<<HANDLE_STDOUT))) == 0) {
		    goto done;
		}
	    }
	    break;
	case HANDLE_STDERR:
#ifdef HARDDEBUG
	    prefix = "STDERR";
#endif
	    if (!handle_overlapped(real_stderr,&ov_err, errbuff,1024,1,stderr,&skiperr)){
		if ((living_handles &= (~(1U<<HANDLE_STDERR))) == 0) {
		    goto done;
		}
	    }
	    break;
	default:
	    error("Unexpected wait result");
	}
    }
 done:
    CloseHandle(harr[HANDLE_PROC]);
    CloseHandle(harr[HANDLE_STDOUT]);
    CloseHandle(harr[HANDLE_STDERR]);
    CloseHandle(real_stdout);
    CloseHandle(real_stderr);
    return (int) exitcode;
}

int main(int argc, char **argv)
{
    int i;
    int x;
    char *s;
    char *mpath;
    char *debuglog;
    char *remove;
    FILE *debugfile;
    FILE *tmpfile;
    int filefound;
    int retval = 0;

    char *kernel_libs="kernel32.lib advapi32.lib";
    char *gdi_libs="gdi32.lib user32.lib comctl32.lib comdlg32.lib shell32.lib";
    char *default_libraries = "";
    char *cmd = "";
    char *stdlib = "MSVCRT.LIB";
    int debug_build = 0;
    int stdlib_forced = 0;
    int build_dll = 0;
    char *output_filename = "";
    char *linkadd_pdb = "";
    char *linktype = "";
    char *manifest = "";
    char *outputres = "";

    save_args(argc,argv);
    //fprintf(stderr,"ld_wrap!\n");

    default_libraries = add_to(default_libraries,kernel_libs);
    default_libraries = add_to(default_libraries," ");
    default_libraries = add_to(default_libraries,gdi_libs);
    
    for(i = 1; i < argc; ++i) {
	if (argv[i][0] == '-') {
	    char *opt = argv[i]+1;
	    switch(*opt) {
	    case 'D':
		if(strcmp(opt,"DLL")) {
		    goto filename;
		}
		build_dll = 1;
		break;
	    case 'd':
		if(!strncmp(opt,"def:",4)) {
		    mpath = do_cyp(opt+4);
		    cmd = add_to(cmd," -def:\"");
		    cmd = add_to(cmd,mpath);
		    cmd = add_to(cmd,"\"");
		    free(mpath);
		} else if(strcmp(opt,"dll")) {
		    goto filename;
		} else {
		    build_dll = 1;
		}
		break;
	    case 'L':
		mpath = do_cyp(opt+1);
		cmd = add_to(cmd," -libpath:\"");
		cmd = add_to(cmd,mpath);
		cmd = add_to(cmd,"\"");
		free(mpath);
		break;
	    case 'l':
		if(!strcmp(opt,"lMSVCRT") || !strcmp(opt,"lmsvcrt")) {
		    stdlib = "MSVCRT.LIB";
		    stdlib_forced = 1;
		} else if(!strcmp(opt,"lMSVCRTD") || !strcmp(opt,"lmsvcrtd")) {
		    stdlib = "MSVCRTD.LIB";
		    stdlib_forced = 1;
		} else if(!strcmp(opt,"lLIBCMT") || !strcmp(opt,"llibcmt")) {
		    stdlib = "LIBCMT.LIB";
		    stdlib_forced = 1;
		} else if(!strcmp(opt,"lLIBCMTD") || !strcmp(opt,"llibcmtd")) {
		    stdlib = "LIBCMTD.LIB";
		    stdlib_forced = 1;
		} else if(!strcmp(opt,"lsocket")) {
		    default_libraries = add_to(default_libraries," ");
		    default_libraries = add_to(default_libraries,"WS2_32.LIB");
		} else {
		    mpath = do_cyp(opt+1);
		    cmd = add_to(cmd," \"");
		    cmd = add_to(cmd,mpath);
		    cmd = add_to(cmd,"\"");
		    free(mpath);
		}
		break;
	    case 'g':
		debug_build = 1;
		break;
	    case 'p':
		if (strcmp(opt,"pdb:none")) {
		    goto filename;
		}
		break;
	    case 'i':
		if (!strncmp(opt,"implib:",7)) {
		    mpath = do_cyp(opt+7);
		    cmd = add_to(cmd," -implib:\"");
		    cmd = add_to(cmd,mpath);
		    cmd = add_to(cmd,"\"");
		    free(mpath);
		} else if (strcmp(opt,"incremental:no")) {
		    goto filename;
		}
		break;
	    case 'o':
		if (!strcmp(opt,"o")) {
		    ++i;
		    if (i >= argc) {
			error("-o without filename");
		    }
		    output_filename = do_cyp(argv[i]);
		} else {
		    output_filename = do_cyp(opt+1);
		}
		break;
	    default:
		goto filename;
	    }
	    continue;
	}
    filename:
	s = argv[i];
	if (*s == '/') {
	    mpath = do_cyp(s);
	    cmd = add_to(cmd," \"");
	    cmd = add_to(cmd,mpath);
	    cmd = add_to(cmd,"\"");
	    free(mpath);
	} else {
	    cmd = add_to(cmd," \"");
	    cmd = add_to(cmd,s);
	    cmd = add_to(cmd,"\"");
	}
    }
    if ((debuglog = getenv("LD_SH_DEBUG_LOG")) != NULL) {
	debugfile = fopen(debuglog,"wb+");
	if (debugfile) {
	    fprintf(debugfile,"----------------\n");
	}
    } else {
	debugfile = NULL;
    }

    if (debug_build) {
	if (!stdlib_forced) {
	    stdlib = "MSVCRTD.LIB";
	}
    }

    s = add_to("",output_filename);
    x = strlen(s);
    
    if (x >= 4 && (!strcmp(s+x-4,".exe") || !strcmp(s+x-4,".EXE") ||
		   !strcmp(s+x-4,".dll") || !strcmp(s+x-4,".DLL"))) {
	*(s+x-3) = '\0';
	linkadd_pdb = add_to(linkadd_pdb,"-pdb:\"");
	linkadd_pdb = add_to(linkadd_pdb,s);
	linkadd_pdb = add_to(linkadd_pdb,"pdb\"");
    } else if (!x) {
	linkadd_pdb = add_to(linkadd_pdb,"-pdb:\"a.pdb\"");
    } else {
	linkadd_pdb = add_to(linkadd_pdb,"-pdb:\"");
	linkadd_pdb = add_to(linkadd_pdb,s);
	linkadd_pdb = add_to(linkadd_pdb,".pdb\"");
    }
    free(s);

    
    linktype = add_to(linktype,"-debug ");
    linktype = add_to(linktype,linkadd_pdb);

    free(linkadd_pdb);

    s = add_to("",output_filename);
    x = strlen(s);

    if (build_dll) {
	if (x >= 4 && (!strcmp(s+x-4,".exe") || !strcmp(s+x-4,".EXE") ||
		       !strcmp(s+x-4,".dll") || !strcmp(s+x-4,".DLL"))) {
	    
	    if (!strcmp(s+x-4,".exe") || !strcmp(s+x-4,".EXE")) {
		fprintf(stderr,"Warning, output set to .exe when building DLL");
	    }
	    mpath = cmd;
	    cmd = add_to("","-dll -out:\"");
	    cmd = add_to(cmd,s);
	    cmd = add_to(cmd,"\" ");
	    cmd = add_to(cmd,mpath);
	    if (*mpath) {
		free(mpath);
	    }

	    outputres = add_to(outputres,output_filename);
	    outputres = add_to(outputres,";2");
	    manifest = add_to(manifest,output_filename);
	    manifest = add_to(manifest,".manifest");
	} else if (x == 0) {
	    mpath = cmd;
	    cmd = add_to("","-dll -out:\"a.dll\" ");
	    cmd = add_to(cmd,mpath);
	    if (*mpath) {
		free(mpath);
	    }

	    outputres = add_to(outputres,"a.dll;2");
	    manifest = add_to(manifest,"a.dll.manifest");
	} else {
	    mpath = cmd;
	    cmd = add_to("","-dll -out:\"");
	    cmd = add_to(cmd,s);
	    cmd = add_to(cmd,".dll\" ");
	    cmd = add_to(cmd,mpath);
	    if (*mpath) {
		free(mpath);
	    }

	    outputres = add_to(outputres,output_filename);
	    outputres = add_to(outputres,".dll;2");
	    manifest = add_to(manifest,output_filename);
	    manifest = add_to(manifest,".dll.manifest");
	}
    } else {
	if (x >= 4 && (!strcmp(s+x-4,".exe") || !strcmp(s+x-4,".EXE") ||
		       !strcmp(s+x-4,".dll") || !strcmp(s+x-4,".DLL"))) {
	    
	    if (!strcmp(s+x-4,".dll") || !strcmp(s+x-4,".DLL")) {
		fprintf(stderr,"Warning, output set to .exe when building DLL");
	    }
	    mpath = cmd;
	    cmd = add_to("","-out:\"");
	    cmd = add_to(cmd,s);
	    cmd = add_to(cmd,"\" ");
	    cmd = add_to(cmd,mpath);
	    if (*mpath) {
		free(mpath);
	    }

	    outputres = add_to(outputres,output_filename);
	    outputres = add_to(outputres,";1");
	    manifest = add_to(manifest,output_filename);
	    manifest = add_to(manifest,".manifest");
	} else if (x == 0) {
	    mpath = cmd;
	    cmd = add_to("","-out:\"a.exe\" ");
	    cmd = add_to(cmd,mpath);
	    if (*mpath) {
		free(mpath);
	    }

	    outputres = add_to(outputres,"a.exe;1");
	    manifest = add_to(manifest,"a.exe.manifest");
	} else {
	    mpath = cmd;
	    cmd = add_to("","-out:\"");
	    cmd = add_to(cmd,s);
	    cmd = add_to(cmd,".exe\" ");
	    cmd = add_to(cmd,mpath);
	    if (*mpath) {
		free(mpath);
	    }

	    outputres = add_to(outputres,output_filename);
	    outputres = add_to(outputres,".exe;1");
	    manifest = add_to(manifest,output_filename);
	    manifest = add_to(manifest,".exe.manifest");
	}
    }
	    
    s = cmd;
    cmd = add_to("","link.exe ");
    cmd = add_to(cmd,linktype);
    cmd = add_to(cmd," -nologo -incremental:no ");
    cmd = add_to(cmd,s);
    cmd = add_to(cmd," ");
    cmd = add_to(cmd,stdlib);
    cmd = add_to(cmd," ");
    cmd = add_to(cmd,default_libraries);

    if (*s) {
	free(s);
    }
	

    if (debugfile) {
	fprintf(debugfile,"%s\n",save);
	fprintf(debugfile,"%s\n",cmd);
    }
    
    retval = run(cmd,0,0);
    
    
    mpath = do_cyp(manifest);
    filefound = 0;
    tmpfile = fopen(mpath,"rb");
    if (tmpfile != NULL) {
	filefound = 1;
	fclose(tmpfile);
    }
    if (retval == 0 && filefound) {
	s = add_to("","mt.exe -nologo -manifest \"");
	s = add_to(s,manifest);
	s = add_to(s,"\" -outputresource:\"");
	s = add_to(s,outputres);
	s = add_to(s,"\"");
	if (debugfile) {
	    fprintf(debugfile,"%s\n",s);
	}
	retval = run(s,0,0);
	if (*s) {
	    free(s);
	}
	if (retval) {
	    /* cleanup needed */
	    remove = add_to("",outputres);
	    x = strlen(remove);
	    remove[x-2] = '\0';
	    if (debugfile) {
		fprintf(debugfile,"remove %s\n",remove);
	    }
	    DeleteFile(remove);
	    free(remove);
	}
	if (debugfile) {
	    fprintf(debugfile,"remove %s\n",manifest);
	}
	DeleteFile(manifest);
    }
    return retval;
}
	    
	    
		

			
		    
		    
		
	    
		    








