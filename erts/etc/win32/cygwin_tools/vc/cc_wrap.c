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

void maybe_cleanup(void)
{
    DIR *dir;
    struct dirent *dent;
    if (*tmpobjdir == '\0') {
	return;
    }
    if (!(dir = opendir(tmpobjdir))) {
	return;
    }
    while((dent = readdir(dir)) != NULL) {
	char *fullname = add_to("",tmpobjdir);
	fullname = add_to(fullname,"/");
	fullname = add_to(fullname,dent->d_name);
	unlink(fullname);
	free(fullname);
    }
    closedir(dir);
    rmdir(tmpobjdir);
}

    
    

void error(char *str) 
{
    fprintf(stderr,"%s\n",str);
    maybe_cleanup();
    exit(1);
}


char **add_to_src(char **srcarr, char *add)
{
    int num;
    if (srcarr == NULL) {
	srcarr = malloc(sizeof(char *)*2);
	srcarr[0]=malloc(strlen(add)+1);
	strcpy(srcarr[0],add);
	srcarr[1] = NULL;
    } else {
	for(num = 0; srcarr[num] != NULL; ++num)
	    ;
	num +=1;
	srcarr = realloc(srcarr,sizeof(char *)*(num+1));
	srcarr[num-1] = malloc(strlen(add)+1);
	strcpy(srcarr[num-1],add);
	srcarr[num] = NULL;
    }
    return srcarr;
}



char *object_name(char *source) {
    char *tmp = add_to("",source);
    int j = strlen(tmp)-2;
    if (j < 0) {
	j = 0;
    }
    while(j>0 && tmp[j] != '.') {
	--j;
    }
    if (tmp[j] == '.') {
	++j;
    }
    tmp[j++] = 'o';
    tmp[j] = '\0';
    return tmp;
}

char *exe_name(char *source) {
    char *tmp = add_to("",source);
    int j = strlen(tmp)-2;
    if (j < 0) {
	j = 0;
    }
    while(j>0 && tmp[j] != '.') {
	--j;
    }
    if (tmp[j] == '.') {
	++j;
    }
    tmp[j] = '\0';
    return add_to(tmp,"exe");
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

char *progname="cc_wrap";

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
  char *(envs[]) = {"LIB","INCLUDE","LIBPATH", "LD_SH_DEBUG_LOG", NULL};
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
	fprintf(stderr,"Could not run %s, last error: %d\n",commandline,GetLastError());
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
    FILE *debugfile;

    char *common_cflags="-nologo -D__WIN32__ -DWIN32 -DWINDOWS -D_WIN32 -DNT -D_CRT_SECURE_NO_DEPRECATE";
    char *md = "-MD";
    char *debug_flags = "";
    char *optimize_flags = "";
    char *outfile = "";
    char *cmd = "";
    char **sources = NULL;
    char *accum_objects = "";
    char *linkcmd = "";
    
    int md_forced = 0;
    int preprocessing = 0;
    int debug_build = 0;
    int optimized_build = 0;
    int linking = 1;
    int retval;

    save_args(argc,argv);
    
    for(i = 1; i < argc; ++i) {
	if (argv[i][0] == '-') {
	    char *opt = argv[i]+1;
	    switch(*opt) {
	    case 'W':
		if(strcmp(opt,"Wall")) {
		    goto filename;
		}
		break;
	    case 'c':
		if(strlen(opt) > 1) {
		    goto filename;
		}
		linking = 0;
		break;
	    case 'E':
		if(strlen(opt) > 1) {
		    if (opt[1] == 'H') {
			cmd = add_to(cmd," ");
			cmd = add_to(cmd,opt);
		    } else {
			goto filename;
		    }
		}
		preprocessing = 1;
		linking = 0;
		break;
	    case 'O':
		/* ignore what opt is requested, set hard */
		optimize_flags = "-Ox -Zi";
		debug_flags = "";
		debug_build = 0;
		if (!md_forced) {
		    md = "-MD";
		}
		optimized_build = 1;
		break;
	    case 'g':
		if (strcmp(opt,"g") && strcmp(opt,"ggdb")) {
		    goto filename;
		}
		if (!optimized_build) {
		    debug_flags = "-Z7";
		    if (!md_forced) {
			md = "-MDd";
		    }
		    linkcmd = add_to(linkcmd," -g");
		    debug_build = 1;
		}
		break;
	    case 'm':
	    case 'M':
		if(!strcmp(opt,"mt") || !strcmp(opt,"MT")) {
		    md = "-MT";
		} else if (!strcmp(opt,"md") || !strcmp(opt,"MD")) {
		    md = "-MD";
		} else if (!strcmp(opt,"ml") || !strcmp(opt,"ML")) {
		    md = "-ML";
		} else if (!strcmp(opt,"mdd") || !strcmp(opt,"MDd") || 
			   !strcmp(opt,"MDD")) {
		    md = "-MDd";
		} else if (!strcmp(opt,"mtd") || !strcmp(opt,"MTd") || 
			   !strcmp(opt,"MTD")) {
		    md = "-MTd";
		} else if (!strcmp(opt,"mld") || !strcmp(opt,"MLd") || 
			   !strcmp(opt,"MLD")) {
		    md = "-MLd";
		} else {
		    goto filename;
		}
		md_forced = 1;
		break;
	    case 'o':
		if (!strcmp(opt,"o")) {
		    ++i;
		    if (i >= argc) {
			error("-o without filename");
		    }
		    outfile = argv[i];
		} else {
		    outfile = opt+1;
		}
		break;
	    case 'I':
		if(opt[1] == '/') {
		    mpath = do_cyp(opt+1);
		    cmd = add_to(cmd," -I\"");
		    cmd = add_to(cmd,mpath);
		    cmd = add_to(cmd,"\"");
		    free(mpath);
		} else {
		    cmd = add_to(cmd," ");
		    cmd = add_to(cmd,opt);
		}
		break;
	    case 'D':
		cmd = add_to(cmd," -");
		cmd = add_to(cmd,opt);
	    case 'l':
		linkcmd = add_to(linkcmd," -");
		linkcmd = add_to(linkcmd,opt);
		break;
	    default:
		goto filename;
	    }
	    continue;
	}
    filename:
	s = argv[i];
	x = strlen(s);
	if (x > 1 && s[x-1] == 'c' && s[x-2] == '.') {
	    /* C source */
	    sources = add_to_src(sources,s);
	} else if (x > 3 && !strcmp(s + (x - 4),".cpp")) {
	    /* C++ */
	    sources = add_to_src(sources,s);
	} else 	if (x > 1 && s[x-1] == 'o' && s[x-2] == '.') { 
	    linkcmd = add_to(linkcmd," ");
	    linkcmd = add_to(linkcmd,s);
	} else {
	    /* Pass rest to linker */
	    linkcmd = add_to(linkcmd," ");
	    linkcmd = add_to(linkcmd,s);
	}
    }
    if ((debuglog = getenv("CC_SH_DEBUG_LOG")) != NULL) {
	debugfile = fopen(debuglog,"wb+");
	if (debugfile) {
	    fprintf(debugfile,"----------------\n");
	}
    } else {
	debugfile = NULL;
    }

    tmpobjdir = add_to("","/tmp/tmpobj");
    {
	char pidstr[100];
	pid_t pid = getpid();
	sprintf(pidstr,"%d",pid);
	tmpobjdir = add_to(tmpobjdir,pidstr);
    }
    mkdir(tmpobjdir,0777);
    if (sources != NULL) { 
	char *output_filename;
	char *output_flag;
	char *params;
	for (i=0;sources[i] != NULL; ++i) {
	    if (!linking) {
		int x = strlen(outfile);
		if (x > 1 && outfile[x-1] == 'o' && outfile[x-2] == '.') {
		    if (*outfile != '/') {
			/* non absolute object */
			if (i > 0) {
			    error("Single object multiple sources");
			}
			output_filename = add_to("",outfile);
		    } else {
			if (i > 0) {
			    error("Single object multiple sources");
			}
			output_filename = do_cyp(outfile);
		    }
		} else {
		    char *tmp = object_name(sources[i]);

		    /*fprintf(stderr,"sources[i] = %s\ntmp = %s\n",
		      sources[i],tmp);*/

		    if (!x || outfile[0] != '/') {
			/* non absolute directory */
			output_filename = add_to("",outfile);
		    } else {
			output_filename = do_cyp(outfile);
		    }
		    /*fprintf(stderr,"output_filename = %s\n",output_filename);*/
		    if (*output_filename != '\0') {
			output_filename = add_to(output_filename,"/");
		    }
		    output_filename = add_to(output_filename,tmp);
		    free(tmp);
		}
	    } else {
		char *tmp = object_name(sources[i]);
		output_filename = add_to("",tmpobjdir);
		output_filename = add_to(output_filename,"/");
		output_filename = add_to(output_filename,tmp);
		accum_objects = add_to(accum_objects," ");
		accum_objects = add_to(accum_objects,output_filename);
		/* reform to dos path */
		free(output_filename);
		output_filename = do_cyp(tmpobjdir);
		output_filename = add_to(output_filename,"/");
		output_filename = add_to(output_filename,tmp);
	    }
	    mpath = do_cyp(sources[i]);
	    if (preprocessing) {
		output_flag = add_to("","-E");
	    } else {
		output_flag = add_to("","-c -Fo");
		output_flag = add_to(output_flag,output_filename);
	    }
	    params = add_to("","cl.exe ");
	    params = add_to(params,common_cflags);
	    params = add_to(params," ");
	    params = add_to(params,md);
	    params = add_to(params," ");
	    params = add_to(params,debug_flags);
	    params = add_to(params," ");
	    params = add_to(params,optimize_flags);
	    params = add_to(params," ");
	    params = add_to(params,cmd);
	    params = add_to(params," ");
	    params = add_to(params,output_flag);
	    params = add_to(params," ");
	    params = add_to(params,mpath);
	    free(output_filename);
	    free(output_flag);
	    free(mpath);
	    
	    if (debugfile) {
		fprintf(debugfile,"%s\n",save);
		fprintf(debugfile,"%s\n",params);
	    }
	    if (preprocessing) {
		retval = run(params,0,1);
	    } else {
		retval = run(params,1,0);
	    }
	    if (retval != 0) {
		maybe_cleanup();
		return retval;
	    }
	    free(params);
	}
    }
    if (linking) {
	char *out_spec;
	char *stdlib;
	char *params;
	if (strlen(outfile) == 0) {
	    if (sources != NULL && sources[0] != NULL) {
		char *tmp = exe_name(sources[0]);
		out_spec = add_to("","-o ");
		out_spec = add_to(out_spec,tmp);
		free(tmp);
	    } else {
		out_spec = add_to("","");
	    }
	} else {
	    out_spec = add_to("","-o ");
	    out_spec = add_to(out_spec,outfile);
	}
	if (!strcmp(md,"-ML")) {
	    stdlib="-lLIBC";
	} else if (!strcmp(md,"-MLd")) {
	    stdlib="-lLIBCD";
	} else if (!strcmp(md,"-MD")) {
	    stdlib="-lMSVCRT";
	} else if (!strcmp(md,"-MDd")) {
	    stdlib="-lMSVCRTD";
	} else if (!strcmp(md,"-MT")) {
	    stdlib="-lLIBCMT";
	} else if (!strcmp(md,"-MTd")) {
	    stdlib="-lLIBMTD";
	} else {
	    stdlib = "";
	}
#if 0
	params = add_to("","bash ld.sh ");
#else
	params = add_to("","ld_wrap.exe ");
#endif
	params = add_to(params,accum_objects);
	params = add_to(params," ");
	params = add_to(params,out_spec);
	params = add_to(params," ");
	params = add_to(params,linkcmd);
	params = add_to(params," ");
	params = add_to(params,stdlib);
	free(out_spec);
	free(accum_objects);
	if (debugfile) {
	    fprintf(debugfile,"%s\n",params);
	}
	if (retval = run(params,0,0) != 0) {
	    maybe_cleanup();
	    return retval;
	}
	free(params);
    }
    maybe_cleanup();
    return 0;
}
	    
	    
		

			
		    
		    
		
	    
		    








