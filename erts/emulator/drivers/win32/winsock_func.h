/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

typedef struct _WinSockFuncs {
  int (WSAAPI *WSAStartup)(WORD wVersionRequired, LPWSADATA lpWSAData);
  int (WSAAPI *WSACleanup)(void);
  int (WSAAPI *WSAGetLastError)(void);
  DWORD (WSAAPI *WSAWaitForMultipleEvents) (DWORD cEvents,
					    const WSAEVENT FAR * lphEvents,
					    BOOL fWaitAll,
					    DWORD dwTimeout,
					    BOOL fAlertable);
  WSAEVENT (WSAAPI *WSACreateEvent)(void);
  BOOL (WSAAPI *WSACloseEvent)(WSAEVENT hEvent);

  BOOL (WSAAPI *WSASetEvent)(WSAEVENT hEvent);
  BOOL (WSAAPI *WSAResetEvent)(WSAEVENT hEvent);
  int (WSAAPI *WSAEventSelect)(SOCKET s, WSAEVENT hEventObject,
			       long lNetworkEvents);
  int (WSAAPI *WSAEnumNetworkEvents)(SOCKET s,
				     WSAEVENT hEventObject,
				     LPWSANETWORKEVENTS lpNetworkEvents);
  int (WSAAPI *WSAIoctl)(SOCKET s,
			 DWORD dwIoControlCode,
			 LPVOID lpvInBuffer,
			 DWORD cbInBuffer,
			 LPVOID lpvOUTBuffer,
			 DWORD cbOUTBuffer,
			 LPDWORD lpcbBytesReturned,
			 LPWSAOVERLAPPED lpOverlapped,
			 LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionROUTINE
			 );
   SOCKET (WSAAPI *accept)(SOCKET s, struct sockaddr FAR *addr,
			  int FAR *addrlen);
  int (WSAAPI *bind)(SOCKET s, const struct sockaddr FAR *addr,
		     int namelen);
  int (WSAAPI *closesocket)(SOCKET s);
  int (WSAAPI *connect)(SOCKET s, const struct sockaddr FAR *name,
			int namelen);
  int (WSAAPI *ioctlsocket)(SOCKET s, long cmd, u_long FAR *argp);
  int (WSAAPI *getsockopt)(SOCKET s, int level, int optname,
				char FAR * optval, int FAR *optlen);
  u_long (WSAAPI *htonl)(u_long hostlong);
  u_short (WSAAPI *htons)(u_short hostshort);
  unsigned long (WSAAPI *inet_addr)(const char FAR * cp);
  char FAR * (WSAAPI *inet_ntoa)(struct in_addr in);
  int (WSAAPI *listen)(SOCKET s, int backlog);
  u_short (WSAAPI *ntohs)(u_short netshort);
  int (WSAAPI *recv)(SOCKET s, char FAR * buf, int len, int flags);
  int (WSAAPI *send)(SOCKET s, const char FAR * buf, int len, int flags);
  int (WSAAPI *setsockopt)(SOCKET s, int level, int optname,
			   const char FAR * optval, int optlen);
  int (WSAAPI *shutdown)(SOCKET s, int how);
  SOCKET (WSAAPI *socket)(int af, int type, int protocol);
  struct hostent FAR * (WSAAPI *gethostbyname)(const char FAR * name);
  struct hostent FAR * (WSAAPI *gethostbyaddr)(const char FAR *addr,
					       int addrlen, int addrtype);
  int (WSAAPI *gethostname)(char FAR * name, int namelen);
  struct servent FAR * (WSAAPI *getservbyname)(const char FAR * name,
					       const char FAR * proto);
  struct servent FAR * (WSAAPI *getservbyport)(int port,
					       const char FAR * proto);
  int (WSAAPI *getsockname)(SOCKET sock, struct sockaddr FAR *name,
			    int FAR *namelen);

  /*
   * New, added for inet_drv.
   */

  int (WSAAPI *getpeername)(SOCKET s, struct sockaddr FAR * name,
			    int FAR * namelen);
  u_long (WSAAPI *ntohl)(u_long netlong);
  int (WSAAPI *WSASend)(SOCKET s, LPWSABUF lpBuffers, DWORD dwBufferCount,
			LPDWORD lpNumberOfBytesSent, DWORD dwFlags,
			LPWSAOVERLAPPED lpOverlapped,
			LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
  int (WSAAPI *sendto)(SOCKET s, const char FAR * buf, int len,
		       int flags, const struct sockaddr FAR * to, int tolen);
  int (WSAAPI *recvfrom)(SOCKET s, char FAR * buf, int len, int flags,
			 struct sockaddr FAR * from, int FAR * fromlen);
} WinSockFuncs;


extern WinSockFuncs winSock;

extern int tcp_lookup_functions(void);
