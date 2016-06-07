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
#include 	<stropts.h>
#include 	<poll.h>
#include 	<unistd.h>
#include 	<sys/types.h>
#include 	<sys/stat.h>
#include	<fcntl.h>
#include	<sys/uio.h>

	/* arguments */
#define		OWNPATH		1
#define		NARG		1

	/* status codes */
#define		OK		0
#define 	NOT_OK		1

	/* hardwired constants*/
#define 	PIPENAME	"syslog.otp"
#define		DIE_CMD		"die"
#define		ONLY_STDIN_CMD	"only_stdin"
#define 	BUFFER_SIZE	1001
#define 	MAXPATH_SIZE	1001
#define		STDIN		0
#define		STDOUT		1
#define		HEADER_SIZE	2
#define		WAIT		10
#define		INTERVAL	1
#define		FALSE		0
#define		TRUE		1
#define		FDS_STDIN	0
#define		FDS_PIPE	1

int	main(int argc, char *argv[])
/*	usage: ferrule ownpath */
{
    int		i, pipe_fd;
    long	int	nfds=2L;
    char	buf[BUFFER_SIZE], pipename[MAXPATH_SIZE], packet_size[2];
    struct	pollfd	fds[2];
    struct	stat	stat_buf;
    
    /* enough arguments? */
    if(argc < NARG+1)
	exit(NOT_OK);
    
    /* make pipe name */
    strcpy(pipename, argv[OWNPATH]);
    strcat(pipename, "/");
    strcat(pipename, PIPENAME);
    
    /* wait for creation of pipe */
    for(i=WAIT; i>0; i--)
	if(stat(pipename, &stat_buf) == 0)
	    break;
	else{
	    if(i == 0)
		exit(NOT_OK);
	    else
		sleep(INTERVAL);
	}
    
    /* open pipe, exit if error */
    if((pipe_fd = open(pipename, O_RDONLY | O_NONBLOCK)) == -1)
	exit(NOT_OK);
    
    /* setup for pipe */
    fds[FDS_PIPE].fd = pipe_fd;
    fds[FDS_PIPE].events = POLLRDNORM;
    fds[FDS_PIPE].revents = 0;
    
    /* setup for stdin */
    fds[FDS_STDIN].fd = STDIN;
    fds[FDS_STDIN].events = POLLRDNORM;
    fds[FDS_STDIN].revents = 0;
    
    /* loop */
    while(1){
	/* wait for input */
	if(poll(&fds[0], nfds, INFTIM) == -1)
	    exit(NOT_OK);

	/* analyse input from pipe */
	if(fds[FDS_PIPE].revents != 0){

	    /* pipe error, error exit */
	    if((fds[FDS_PIPE].revents & (POLLHUP | POLLERR | POLLNVAL)) != 0)
		exit(NOT_OK);

	    /* read pipe and write to stdout, exit if error */
	    if((fds[FDS_PIPE].revents & POLLRDNORM) != 0){
		i=0;
		do{
		    read(pipe_fd, &buf[i++], (size_t)1);
		}while(buf[i-1] != '\n');
		i--;

		/* send if string is not empty */
		if(i != 0){
		    /* make packet size, [0]=MSB, [1]=LSB */
		    packet_size[0] = (i >> 8) & 0xff;
		    packet_size[1] = i & 0xff;

		    /* send to OTP process */
		    if(write(STDOUT, packet_size, HEADER_SIZE) != HEADER_SIZE)
			exit(NOT_OK);
		    if(write(STDOUT, buf, i) != i)
			exit(NOT_OK);
		}
	    }
	}

	/* analyse input from stdin */
	if(fds[FDS_STDIN].revents != 0){

	    /* error exit */
	    if((fds[FDS_STDIN].revents & (POLLHUP | POLLERR | POLLNVAL)) != 0)
		exit(NOT_OK);

	    /* read bytes */
	    if((fds[FDS_STDIN].revents & POLLRDNORM) != 0){
		/* get packet size, [0]=MSB, [1]=LSB */
		if(read(STDIN, packet_size, HEADER_SIZE) != HEADER_SIZE)
		    exit(NOT_OK);
		i = (packet_size[0] << 8) | packet_size[1];

		/* get packet */
		if(read(STDIN, buf, i) != i)
		    exit(NOT_OK);

		/* check if die command */
		if(strncmp(DIE_CMD, buf, i) == FALSE)
		    exit(OK);

		/* check if only_stdin command */
		if(strncmp(ONLY_STDIN_CMD, buf, i) == FALSE)
		    nfds=1L;
	    }
	}
    }
}
