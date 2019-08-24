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

/* Purpose:    Binary file driver interface , used for code loading */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "erl_driver.h"
#include <errno.h>

#ifdef NO_UMASK
#define FILE_MODE 0644
#else
#define FILE_MODE 0666
#endif

static int this_port;

static long bin_start();
static int bin_init(),bin_stop(),bin_erlang_read();
static int read_fill(), write_fill();

const struct erl_drv_entry bin_driver_entry = {
    bin_init,
    bin_start,
    bin_stop,
    bin_erlang_read,
    NULL,
    NULL,
    "binary_filer"
};



static int bin_init()
{
    this_port = -1;
    return 0;
}


static long bin_start(port,buf) 
int port;
char *buf;
{

    if (this_port != -1)
	return(-1);
    this_port = port;
    return(port);
}


static int bin_stop()
{
    this_port = -1;
}

 
static int bin_erlang_read(port,buf,count)
long port;
char *buf;
int count;

{
    struct stat statbuf;
    int i,size,fd,rval;
    char *t,*rbuf;
    
    buf[count] = '\0';
    switch (*buf) {
    case 'c' :
	if (chdir(buf+1) == 0)     /* sucsess */
	    driver_output(this_port,"y",1);
	else
	    driver_output(this_port,"n",1);
	return 0;
    case 'f':
#ifdef MSDOS
	if ((fd = open(buf+1,O_RDONLY|O_BINARY)) < 0) {
#else
	if ((fd = open(buf+1,O_RDONLY, 0)) < 0) {
#endif
	    driver_output(this_port,"n",1);
	    return 0;
	}
	if (stat(buf+1,&statbuf) < 0) {
	    driver_output(this_port,"n",1);
	    close(fd);
	    return 0;
	}
	if (S_ISREG(statbuf.st_mode)) 
	    size = statbuf.st_size;
	else
	    size = BUFSIZ;
	    
	if ((rbuf = (char*) driver_alloc(1 + size)) == NULL) {
	    fprintf(stderr,"Out of memory\n");
	    close(fd);
	    driver_output(this_port,"n",1);
	    return 0;
	}
	if (S_ISREG(statbuf.st_mode)) {
	    if (read_fill(fd,1+rbuf,size) != size) {
		driver_free(rbuf);
		close(fd);
		driver_output(this_port,"n",1);
		return 0;;
	    }
	}
	/* The idea here is that if it's a regular file read the entire 
	 * entire file and if it's a device file or a tty try to read 
	 * until errno != EINTR 
         */

	else {
	    while (1) {
		rval = read(fd,1+rbuf,size);
		if (rval < 0 && errno == EINTR)
		    continue;
		if (rval < 0) {
		    driver_free(rbuf);
		    close(fd);
		    driver_output(this_port,"n",1);
		    return 0;
		}
		size = rval;
		break;
	    }
	}
	rbuf[0] = 'y';
	driver_output(this_port,rbuf,1+size);
	driver_free(rbuf);
	close(fd);
	return 0;
    case 'w':
#ifdef MSDOS
	if ((fd = open(buf+1, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY,
		       FILE_MODE)) < 0) {
#else
	if ((fd = open(buf+1, O_WRONLY | O_CREAT | O_TRUNC, FILE_MODE)) < 0) {
#endif
	    driver_output(this_port,"n",1);
	    return 0;
	}
	t = buf+1; i = 1;
	while(*t && i++ < count) t++;
	t++;
	/* t now points to the contents of what we shall write */
	size = count - 1 - i;
	
	/* gotta write_fill if we are writing to a slow device 
	   such as /dev/audio */
	
	if (write_fill (fd,t,size) !=  size) {
	    driver_output(this_port,"n",1);
	    close(fd);
	    return 0;
	}
	driver_output(this_port,"y",1);
	close(fd);
	return 0;
    default:
	driver_failure(this_port,-1);
	return 0;
    }
}


static int write_fill(fd, buf, len)
char *buf;
{
    int i, done = 0;
    
    do {
	if ((i = write(fd, buf+done, len-done)) < 0) {
	    if (errno != EINTR)
		return (i);
	    i = 0;
	}
	done += i;
    } while (done < len);
    return (len);
}


static int read_fill(fd, buf, len)
char *buf;
{
    int i, got = 0;

    do {
	if ((i = read(fd, buf+got, len-got)) <= 0) {
	    if (i == 0 || errno != EINTR)
		return (i);
	    i = 0;
	}
	got += i;
    } while (got < len);
    return (len);
}
    
