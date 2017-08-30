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
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#ifdef __WIN32__
#include <winsock2.h>
#include <direct.h>
#include <windows.h>
#include <winbase.h>
#else /* not __WIN32__ */
#include <errno.h> 
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h> 
#include <netdb.h>
#endif
#include "rmod_random__s.h"
#include "ei_connect.h"

/* Used functions */
extern int gethostname(char *buf, int buflen);
static int getport(int sockd);
static int getlisten(int port);
static int init(ei_cnode *ec, int *sd, int *portnr, int *epmd_fd);
void terminate(int *fd, int *sd, int *epmd_fd);
static void server_loop(ei_cnode *ec, int fd, int sd);

/* change these, or even better, make command-line args to program... */
#define COOKIE "flash"
#define SERVER "babbis"
#define NODENAMESZ 512
#define HOSTNAMESZ 256
#define INBUFSZ 1024
#define OUTBUFSZ 1024


int main(int argc, char **argv)
{
    int sd;
    int portnr;
    int epmd_fd;
    ei_cnode ec;

    /* crate file descriptors */
    if (init(&ec, &sd, &portnr, &epmd_fd) < 0)
	return -1;

    /* start server loop */
    server_loop(&ec, sd, epmd_fd);

    return 0;
}



static void server_loop(ei_cnode *ec, int sd, int epmd_fd)
{
    ErlConnect conn;
    erlang_msg msg;
    int status=1;
    CORBA_Environment *env;
  
    /* Create and init CORBA_Environment */
    env = CORBA_Environment_alloc(INBUFSZ,OUTBUFSZ);
  
    while (status >= 0) {
	status = 1;
    
	if ((env->_fd = ei_accept(ec, sd, &conn)) < 0) {       
	    /* error */
	    fprintf(stderr,"Accept failed: %s\n",strerror(errno));
	} else {
	    /* connection */
	    fprintf(stderr,"Accepted connection from %s\n",conn.nodename);
      
	    while (status >= 0) {

		/* write message to buffer */
		status = ei_receive_encoded(env->_fd, &env->_inbuf, &env->_inbufsz, &msg, &env->_iin); 
		switch(status) {
		case ERL_SEND:
		case ERL_REG_SEND :
		    /* do transaction with fd */
		    rmod_random__switch(NULL,env);
	  
		    switch(env->_major) {
		    case CORBA_NO_EXCEPTION: /* Success */
			break;
		    case CORBA_SYSTEM_EXCEPTION: /* System exception */
			printf("Request failure, reason : %s\n",(char *) CORBA_exception_value(env));
			CORBA_exception_free(env);
			break;
		    default:	/* Should not come here */
			CORBA_exception_free(env);
			break;
		    }
	  
		    /* send outdata */
		    if (env->_iout > 0) 
			ei_send_encoded(env->_fd,&env->_caller,env->_outbuf,env->_iout);
		    break;
	  
		case ERL_TICK :
		    break;
		default :	/* < 0 */
		    printf("Connection terminated\n");
		    break;
		}  
	    }
	}
	status=0;		/* restart */
    }
  
    /* close file descriptors */
    terminate(&env->_fd, &sd, &epmd_fd);
  
    /* Free env & buffers */
    CORBA_free(env->_inbuf);
    CORBA_free(env->_outbuf);
    CORBA_free(env);
} 



static int init(int *sd, int *portnr, int *epmd_fd)
{
    char host[HOSTNAMESZ];
    char servernode[NODENAMESZ];
    struct hostent *h;

    /* get the host name */
    if ((gethostname(host,HOSTNAMESZ)))
	fprintf(stderr,"can't find own hostname\n");
    else {
	/* identify host */
	if (!(h = erl_gethostbyname(host)))
	    fprintf(stdout,"can't find own ip address\n");
	else {

	    /* get a listen port. 0 means let system choose port number */
	    *sd = getlisten(0);
      
	    /* what port did we get? */
	    /* this call not necessary if we specified port in call to getlisten() */
	    *portnr = getport(*sd);
      
	    /* make the nodename server@host */
	    sprintf(servernode,"%s@%s",SERVER,host);
      
	    /* initiate */
	    /* cnode, host, alive, alive@host, addr, cookie, creation */
	    if (ei_connect_xinit(ec, host, SERVER, servernode,
				 (Erl_IpAddr)(h->h_addr_list[0]),
				 COOKIE, 0) == 0) {
		/* let epmd know we are here */
		*epmd_fd = ei_publish(ec, *portnr);
		if (*epmd_fd >= 0)
		    return 0;
	    }
	} 
    }
    return -1;
}


void terminate(int *fd, int *sd, int *epmd_fd) {

    close(*fd);

    /* remove info from epnd */
    close(*epmd_fd);

    /* return socket */
    close(*sd);

}



/* tells you what port you are using on given socket */
static int getport(int sockd)
{
    struct sockaddr_in addr;
    int namelen = sizeof(addr);
    int i;

    memset(&addr,0,sizeof(addr));
  
    if ((i = getsockname(sockd,(struct sockaddr *)&addr,&namelen))<0)
	return i;
  
    return ntohs(addr.sin_port);
}



/* return a listen socket, bound to given port */
/* specify port = 0 to let system assign port */
static int getlisten(int port)
{
    int sockd;
    struct sockaddr_in inaddr;
    int opt = 1;
    int i;

    /* get listen socket */
    if ((sockd = socket(AF_INET,SOCK_STREAM,0)) < 0) return sockd;
  
    if ((i=setsockopt(sockd,SOL_SOCKET,SO_REUSEADDR,(void *)&opt,sizeof(opt)))<0) 
	return i;

    /* bind to requested port */
    memset(&inaddr,0,sizeof(inaddr));
    inaddr.sin_family = AF_INET;              
    inaddr.sin_addr.s_addr = htonl(INADDR_ANY); 
    inaddr.sin_port = htons(port);

    if ((i = bind(sockd,(struct sockaddr*) &inaddr, sizeof(inaddr))) < 0)
	return i;

    listen(sockd,5);

    return sockd;
}
