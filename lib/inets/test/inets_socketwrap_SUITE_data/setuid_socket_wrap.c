/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
 * setuid_socket_wrap.c
 *
 * ./a.out [-s [tag,][addr]:[port]]* [-d [tag,][addr]:[port]]* 
 *         [-r [tag,]proto]* [-p erl_path]* -- program args
 *
 * Where: -s = stream socket, -d datagram socket and -r means raw socket.
 *
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif

struct sock_list {
    struct sock_list *next;
    int fd;
    int type;
    int protocol;
    struct sockaddr_in addr;
    char *arg;
};

int parse_addr(addr, str)
    struct sockaddr_in *addr;
    char *str;
{
    int port = 0;
    char *cp;
    struct hostent *hp;
    struct servent *se;

    if ((cp = strrchr(str, (int)':')) != NULL)
        *cp++ = '\0';
    if (cp) {
        if (!isdigit((int)cp[0])) {
            if ((se = getservbyname(cp, "tcp")) != NULL) {
                port = ntohs(se->s_port);
	    } else {
		fprintf(stderr, "unknown port %s\n", cp);
		return -1;
	    }
        } else {
            port = atoi(cp);
        }
    }
    if (port < 0 || port > 0xffff) {
	fprintf(stderr, "bad port number %d\n", port);
        return -1;
    }
    
    bzero(addr, sizeof(*addr));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    if (*str == '\000') {
	addr->sin_addr.s_addr = INADDR_ANY;
    } else {
	if ((addr->sin_addr.s_addr = inet_addr(str)) == INADDR_NONE) {
	    if ((hp = gethostbyname(str)) == NULL) {
		fprintf(stderr, "\"%s\" unknown host or address!\n", str);
		return -1;
	    } else {
		bcopy(hp->h_addr_list[0], &addr->sin_addr.s_addr,hp->h_length);
	    }
	}
    }
    return 0;
}

struct sock_list *new_entry(type, argstr)
    int type;
    char *argstr;
{
    struct sock_list *sle;
    char *cp;
    
    sle = (struct sock_list *)malloc(sizeof(struct sock_list));
    if (!sle)
	return NULL;
    sle->next = NULL;
    sle->fd = -1;

    if ((cp = strchr(argstr, (int)',')) != NULL) {
	*cp++ = '\0';
	sle->arg = argstr;
	argstr = cp;
    } else {
	sle->arg = "-fd";
    }
    sle->type = type;
    switch (type) {
        case SOCK_RAW: {
	    struct protoent *pe;
	    pe = getprotobyname(argstr);
	    if (!pe) {
		fprintf(stderr, "Unknown protocol: %s\n", argstr);
		free(sle);
		return NULL;
	    }
	    sle->protocol = pe->p_proto;
	    break;
	}
        case SOCK_STREAM:
        case SOCK_DGRAM:
	    sle->protocol = 0;
	    if (parse_addr(&sle->addr, argstr) < 0) {
		free(sle);
		return NULL;
	    }
	    break;
    }
    return sle;
}

int open_socket(sle)
    struct sock_list *sle;
{
    sle->fd = socket(AF_INET, sle->type, sle->protocol);
    if (sle->fd < 0) {
	perror("socket");
	return -1;
    }
    if (sle->type != SOCK_RAW) {
#if 0
	printf("binding fd %d to %s:%d\n", sle->fd,
	       inet_ntoa(sle->addr.sin_addr), ntohs(sle->addr.sin_port));
#endif
	if (bind(sle->fd, (struct sockaddr *)&sle->addr, sizeof(sle->addr))<0){
	    perror("bind");
	    close(sle->fd);
	    return -1;
	}
    }
    return sle->fd;
}

int main(argc, argv)
    int argc;
    char *argv[];
{
    struct sock_list *sl = NULL, *sltmp = NULL;
    int count = 0;
    int c;
    char *run_prog = NULL;
    
    while ((c = getopt(argc, argv, "s:d:r:p:")) != EOF)
	switch (c) {
	case 's':
	    sltmp = new_entry(SOCK_STREAM, optarg);
	    if (!sltmp) {
		exit(1);
	    }
	    sltmp->next = sl;
	    sl = sltmp;
	    count++;
	    break;
	case 'd':
	    sltmp = new_entry(SOCK_DGRAM, optarg);
	    if (!sltmp) {
		exit(1);
	    }
	    sltmp->next = sl;
	    sl = sltmp;
	    count++;
	    break;
	case 'r':
	    sltmp = new_entry(SOCK_RAW, optarg);
	    if (!sltmp) {
		exit(1);
	    }
	    sltmp->next = sl;
	    sl = sltmp;
	    count++;
	    break;
	case 'p':
	  run_prog = optarg;
	  break;
	default:
	    exit(1);
	}
    argc -= optind;
    argv += optind;

    for(sltmp = sl; sltmp != NULL; sltmp = sltmp->next)
	if (open_socket(sltmp) < 0) {
	    fprintf(stderr, "failed to create socket!\n");
	    exit(1);
	}

    setuid(getuid());
    
    {
	int i;
	char **newargv;
	char *run_prog_name;

	newargv = (char **)malloc((1 + 2*count + argc + 1) * sizeof(char*));

	if ((run_prog_name = strrchr(run_prog, (int)'/')) == NULL)
	    run_prog_name = run_prog;
	else
	    run_prog_name++;

	i = 0;
	newargv[i++] = run_prog_name;

	for (; argc; argc--, argv++, i++)
	    newargv[i] = *argv;
	for(sltmp = sl; sltmp != NULL; ) {
	    char *fd_str = (char *)malloc(8);
	    if (!fd_str) exit(1);
	    sprintf(fd_str, "%d", sltmp->fd);
	    if (sltmp->arg && *(sltmp->arg))
		newargv[i++] = sltmp->arg;
	    newargv[i++] = fd_str;
	    sl = sltmp;
	    sltmp = sltmp->next;
	    free(sl);
	}
	newargv[i] = (char *)NULL;
	execv(run_prog, newargv);
	perror("exec");
	exit(1);
    }
    exit(0);
}
