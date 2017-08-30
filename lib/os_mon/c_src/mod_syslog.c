/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<string.h>
#include 	<unistd.h>
#include 	<sys/types.h>
#include 	<sys/stat.h>
#include 	<signal.h>

	/* arguments */
#define		MODE		1
#define		OWNPATH		2
#define		SYSLOGCONF	3
#define		NARG		3

	/* status codes */
#define		OK		0
#define 	ARG_ERROR	-1
#define 	FILE_ERROR	-2
#define 	KILL_ERROR	-3
#define		PIPE_NOT_FOUND	1

	/* hardwired */
#define 	SYSLOG_PID	"/etc/syslog.pid"
#define 	PIPENAME	"syslog.otp"
#define		SYSLOGCONF_ORIG	"syslog.conf.ORIG"
#define		SYSLOGCONF_OTP	"syslog.conf.OTP"
#define 	BUFFER_SIZE	1001
#define 	MAXNAME_SIZE	1001
#define		FALSE		0
#define		TRUE		1
#define		WAIT		1

int	main(int argc, char *argv[])
/*	usage: mod_syslog mode ownpath syslogconf */
{
    int	syslogd_pid, n_lines_copied=0;
    int	otp_sw, find_sw=FALSE;
    char	buf[BUFFER_SIZE], pipename[MAXNAME_SIZE], srcname[MAXNAME_SIZE];
    FILE	*srcfile, *destfile, *pidfile;
    
    void	make_exit(int);
    
    /* enough arguments? */
    if(argc < NARG+1)
	make_exit(ARG_ERROR);
    
    /* enable OTP or not */
    if(strcmp(argv[MODE], "otp") == 0)
	otp_sw = TRUE;
    else if(strcmp(argv[MODE], "nootp") == 0)
	otp_sw = FALSE;
    else
	make_exit(ARG_ERROR);
    
    /* make pipename */
    strcpy(pipename, argv[OWNPATH]);
    strcat(pipename, "/");
    strcat(pipename, PIPENAME);
    
    /* remove old pipe and make a new one */
    if(otp_sw){
	/* remove */
	unlink(pipename);

	/* make a new */
	if(mknod(pipename, S_IFIFO | S_IRUSR | S_IWUSR | S_IRGRP |
		 S_IWGRP | S_IROTH | S_IWOTH, (dev_t)0) != 0)
	    make_exit(FILE_ERROR);
    }
    
    /* make source filename */
    strcpy(srcname, argv[OWNPATH]);
    strcat(srcname, "/");
    strcat(srcname, otp_sw?SYSLOGCONF_OTP:SYSLOGCONF_ORIG);
    
    /* open source and destination, exit if error */
    if((srcfile = fopen(srcname, "r")) == NULL ||
       (destfile = fopen(argv[SYSLOGCONF], "w")) == NULL)
	make_exit(FILE_ERROR);
    
    /* copy source and destination, exit if error */
    while(fgets(buf, BUFFER_SIZE-1, srcfile) != NULL){
	/*	find_sw |= strstr(buf, PIPENAME) != NULL;	*/
	n_lines_copied++;
	if(fputs(buf, destfile) == EOF)
	    make_exit(FILE_ERROR);
    }
    if(ferror(srcfile) || n_lines_copied == 0)
	make_exit(FILE_ERROR);
    
    /* open pidfile, exit if error */
    if((pidfile = fopen(SYSLOG_PID, "r")) == NULL)
	make_exit(FILE_ERROR);
    
    /* read pid for syslogd, exit if error */
    if(fscanf(pidfile, "%d", &syslogd_pid) == 0)
	make_exit(FILE_ERROR);
    
    /* send HUP to syslogd, exit if error */
    if(syslogd_pid < 0 || kill((pid_t)syslogd_pid, SIGHUP) != 0)
	make_exit(KILL_ERROR);
    
    /* remove pipe */
    if(!otp_sw){
	sleep((unsigned)WAIT);
	unlink(pipename);
    }
    
    /* ending successful or with warning */
    /* if(!find_sw && otp_sw)
       make_exit(PIPE_NOT_FOUND);
       else */
    make_exit(OK);
    return 0;
}

void	make_exit(int exit_code)
{
    printf("%d", exit_code);
    exit(0);
}
