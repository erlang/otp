#if defined(__WIN32__)
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>

#else /* Unix */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#ifdef HAVE_LINUX_TCP_H
#ifdef HAVE_SANE_LINUX_TCP_H
#include <linux/tcp.h>
#endif
#endif
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <signal.h>

#endif

#define C_GET_IPPROTO_TCP          1
#define C_GET_IPPROTO_IP           2
#define C_GET_SOL_SOCKET           3
#define C_GET_SOL_IP               4

#define C_GET_TCP_KEEPIDLE        11
#define C_GET_TCP_LINGER2         12
#define C_GET_TCP_INFO            13
#define C_GET_SO_REUSEADDR        14
#define C_GET_SO_KEEPALIVE        15
#define C_GET_SO_LINGER           16

#define C_GET_LINGER_SIZE         21
#define C_GET_TCP_INFO_SIZE       22

#define C_GET_OFF_LINGER_L_ONOFF  31
#define C_GET_OFF_LINGER_L_LINGER 32
#define C_GET_OFF_TCPI_SACKED     33
#define C_GET_OFF_TCPI_OPTIONS    34

#define C_GET_SIZ_LINGER_L_ONOFF  41
#define C_GET_SIZ_LINGER_L_LINGER 42
#define C_GET_SIZ_TCPI_SACKED     43
#define C_GET_SIZ_TCPI_OPTIONS    44

#define C_QUIT                    99

int get_command(void)
{
    char buff[256];
    int res;
    if (fgets(buff,256,stdin) == NULL)
	exit(1);
    sscanf(buff,"%d",&res);
    return res;
}

void put_answer(int x)
{
    printf("%d\n",x);
}

int main(void){
    int x;
    int res;
    setbuf(stdin,NULL);
    setbuf(stdout,NULL);
    do {
	x = get_command();

	switch(x) {
#ifdef IPPROTO_TCP
	case C_GET_IPPROTO_TCP: 
	    res = IPPROTO_TCP;
	    break;        
#endif
#ifdef IPPROTO_IP
	case C_GET_IPPROTO_IP: 
	    res = IPPROTO_IP;
	    break;         
#endif
#ifdef SOL_SOCKET
	case C_GET_SOL_SOCKET: 
	    res = SOL_SOCKET;
	    break;
#endif         
#ifdef SOL_IP
	case C_GET_SOL_IP : 
	    res = SOL_IP;
	    break;            
#endif
#ifdef TCP_KEEPIDLE	    
	case C_GET_TCP_KEEPIDLE: 
	    res = TCP_KEEPIDLE;
	    break;       
#endif
#ifdef TCP_LINGER2
	case C_GET_TCP_LINGER2: 
	    res = TCP_LINGER2;
	    break;        
#endif
#ifdef TCP_INFO
	case C_GET_TCP_INFO: 
	    res = TCP_INFO;
	    break;         
#endif
#ifdef SO_REUSEADDR  
	case C_GET_SO_REUSEADDR: 
	    res = SO_REUSEADDR;
	    break;       
#endif
#ifdef SO_KEEPALIVE
	case C_GET_SO_KEEPALIVE: 
	    res = SO_KEEPALIVE;
	    break;       
#endif
#ifdef SO_LINGER
	case C_GET_SO_LINGER: 
	    res = SO_LINGER;
	    break;          
#endif
#ifdef SO_LINGER
	case C_GET_LINGER_SIZE: 
	    res = sizeof(struct linger);
	    break;        
#endif
#if defined(TCP_INFO) && defined(HAVE_LINUX_TCP_H)
	case C_GET_TCP_INFO_SIZE: 
	    res = sizeof(struct tcp_info);
	    break;      
#endif
#ifdef SO_LINGER
	case C_GET_OFF_LINGER_L_ONOFF:
	    {
		struct linger l;
		res = ((char *) &(l.l_onoff)) - ((char *) &l);
	    }
	    break; 
	case C_GET_OFF_LINGER_L_LINGER: 
	    {
		struct linger l;
		res = ((char *) &(l.l_linger)) - ((char *) &l);
	    }
	    break;
#endif
#if defined(TCP_INFO) && defined(HAVE_LINUX_TCP_H)
	case C_GET_OFF_TCPI_SACKED: 
	    {
		struct tcp_info ti;
		res = ((char *) &(ti.tcpi_sacked)) - ((char *) &(ti));
	    }
	    break;    
	case C_GET_OFF_TCPI_OPTIONS: 
	    {
		struct tcp_info ti;
		res = ((char *) &(ti.tcpi_options)) - ((char *) &(ti));
	    }
	    break;   
#endif
#ifdef SO_LINGER
	case C_GET_SIZ_LINGER_L_ONOFF: 
	    {
		struct linger l;
		res = sizeof(l.l_onoff);
	    }
	    break; 
	case C_GET_SIZ_LINGER_L_LINGER: 
	    {
		struct linger l;
		res = sizeof(l.l_linger);
	    }
	    break;
#endif
#if defined(TCP_INFO) && defined(HAVE_LINUX_TCP_H)
	case C_GET_SIZ_TCPI_SACKED: 
	    {
		struct tcp_info ti;
		res = sizeof(ti.tcpi_sacked);
	    }
	    break;    
	case C_GET_SIZ_TCPI_OPTIONS: 
	    {
		struct tcp_info ti;
		res = sizeof(ti.tcpi_options);
	    }
	    break;   
#endif
	case C_QUIT:
	    res = 0;
	    break;
	default:
	    res = -1;
	}
	put_answer(res);
    } while (x != C_QUIT);
    return 0;
}
