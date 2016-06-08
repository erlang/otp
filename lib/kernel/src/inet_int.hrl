%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%----------------------------------------------------------------------------
%% Interface constants.
%% 
%% This section must be "identical" to the corresponding in inet_drv.c
%% 

%% family codes to open
-define(INET_AF_UNSPEC,       0).
-define(INET_AF_INET,         1).
-define(INET_AF_INET6,        2).
-define(INET_AF_ANY,          3). % Fake for ANY in any address family
-define(INET_AF_LOOPBACK,     4). % Fake for LOOPBACK in any address family
-define(INET_AF_LOCAL,        5). % For Unix Domain address family
-define(INET_AF_UNDEFINED,    6). % For any unknown address family

%% type codes to open and gettype - INET_REQ_GETTYPE
-define(INET_TYPE_STREAM,     1).
-define(INET_TYPE_DGRAM,      2).
-define(INET_TYPE_SEQPACKET,  3).

%% socket modes, INET_LOPT_MODE
-define(INET_MODE_LIST,	      0).
-define(INET_MODE_BINARY,     1).

%% deliver mode, INET_LOPT_DELIVER
-define(INET_DELIVER_PORT,    0).
-define(INET_DELIVER_TERM,    1).

%% active socket, INET_LOPT_ACTIVE
-define(INET_PASSIVE, 0).
-define(INET_ACTIVE,  1).
-define(INET_ONCE,    2). % Active once then passive
-define(INET_MULTI,   3). % Active N then passive

%% state codes (getstatus, INET_REQ_GETSTATUS)
-define(INET_F_OPEN,         16#0001).
-define(INET_F_BOUND,        16#0002).
-define(INET_F_ACTIVE,       16#0004).
-define(INET_F_LISTEN,       16#0008).
-define(INET_F_CON,          16#0010).
-define(INET_F_ACC,          16#0020).
-define(INET_F_LST,          16#0040).
-define(INET_F_BUSY,         16#0080).

%% request codes (erlang:port_control/3)
-define(INET_REQ_OPEN,          1).
-define(INET_REQ_CLOSE,         2).
-define(INET_REQ_CONNECT,       3).
-define(INET_REQ_PEER,          4).
-define(INET_REQ_NAME,          5).
-define(INET_REQ_BIND,          6).
-define(INET_REQ_SETOPTS,       7).
-define(INET_REQ_GETOPTS,       8).
-define(INET_REQ_GETIX,         9).
%% -define(INET_REQ_GETIF,      10). OBSOLETE
-define(INET_REQ_GETSTAT,       11).
-define(INET_REQ_GETHOSTNAME,   12).
-define(INET_REQ_FDOPEN,        13).
-define(INET_REQ_GETFD,         14).
-define(INET_REQ_GETTYPE,       15).
-define(INET_REQ_GETSTATUS,     16).
-define(INET_REQ_GETSERVBYNAME, 17).
-define(INET_REQ_GETSERVBYPORT, 18).
-define(INET_REQ_SETNAME,       19).
-define(INET_REQ_SETPEER,       20).
-define(INET_REQ_GETIFLIST,     21).
-define(INET_REQ_IFGET,         22).
-define(INET_REQ_IFSET,         23).
-define(INET_REQ_SUBSCRIBE,     24).
-define(INET_REQ_GETIFADDRS,    25).
-define(INET_REQ_ACCEPT,        26).
-define(INET_REQ_LISTEN,        27).
-define(INET_REQ_IGNOREFD,      28).
-define(INET_REQ_GETLADDRS,     29).
-define(INET_REQ_GETPADDRS,     30).

%% TCP requests
%%-define(TCP_REQ_ACCEPT,         40). MOVED
%%-define(TCP_REQ_LISTEN,         41). MERGED
-define(TCP_REQ_RECV,           42).
-define(TCP_REQ_UNRECV,         43).
-define(TCP_REQ_SHUTDOWN,       44).
%% UDP and SCTP requests
-define(PACKET_REQ_RECV,        60).
%%-define(SCTP_REQ_LISTEN,        61). MERGED
-define(SCTP_REQ_BINDX,	        62). %% Multi-home SCTP bind
-define(SCTP_REQ_PEELOFF,       63).

%% subscribe codes, INET_REQ_SUBSCRIBE
-define(INET_SUBS_EMPTY_OUT_Q,  1).

%% reply codes for *_REQ_*
-define(INET_REP_ERROR,    0).
-define(INET_REP_OK,       1).
-define(INET_REP,          2).

%% INET, TCP and UDP options:
-define(INET_OPT_REUSEADDR,      0).
-define(INET_OPT_KEEPALIVE,      1).
-define(INET_OPT_DONTROUTE,      2).
-define(INET_OPT_LINGER,         3).
-define(INET_OPT_BROADCAST,      4).
-define(INET_OPT_OOBINLINE,      5).
-define(INET_OPT_SNDBUF,         6).
-define(INET_OPT_RCVBUF,         7).
-define(INET_OPT_PRIORITY,       8).
-define(INET_OPT_TOS,            9).
-define(TCP_OPT_NODELAY,         10).
-define(UDP_OPT_MULTICAST_IF,    11).
-define(UDP_OPT_MULTICAST_TTL,   12).
-define(UDP_OPT_MULTICAST_LOOP,  13).
-define(UDP_OPT_ADD_MEMBERSHIP,  14).
-define(UDP_OPT_DROP_MEMBERSHIP, 15).
-define(INET_OPT_IPV6_V6ONLY,    16).
% "Local" options: codes start from 20:
-define(INET_LOPT_BUFFER,        20).
-define(INET_LOPT_HEADER,        21).
-define(INET_LOPT_ACTIVE,        22).
-define(INET_LOPT_PACKET,        23).
-define(INET_LOPT_MODE,          24).
-define(INET_LOPT_DELIVER,       25).
-define(INET_LOPT_EXITONCLOSE,   26).
-define(INET_LOPT_TCP_HIWTRMRK,  27).
-define(INET_LOPT_TCP_LOWTRMRK,  28).
-define(INET_LOPT_TCP_SEND_TIMEOUT, 30).
-define(INET_LOPT_TCP_DELAY_SEND,   31).
-define(INET_LOPT_PACKET_SIZE,   32).
-define(INET_LOPT_READ_PACKETS,  33).
-define(INET_OPT_RAW,            34).
-define(INET_LOPT_TCP_SEND_TIMEOUT_CLOSE, 35).
-define(INET_LOPT_MSGQ_HIWTRMRK,  36).
-define(INET_LOPT_MSGQ_LOWTRMRK,  37).
-define(INET_LOPT_NETNS,          38).
-define(INET_LOPT_TCP_SHOW_ECONNRESET, 39).
-define(INET_LOPT_LINE_DELIM,     40).
% Specific SCTP options: separate range:
-define(SCTP_OPT_RTOINFO,	 	100).
-define(SCTP_OPT_ASSOCINFO,	 	101).
-define(SCTP_OPT_INITMSG,	 	102).
-define(SCTP_OPT_AUTOCLOSE,	 	103).
-define(SCTP_OPT_NODELAY,		104).
-define(SCTP_OPT_DISABLE_FRAGMENTS,	105).
-define(SCTP_OPT_I_WANT_MAPPED_V4_ADDR, 106).
-define(SCTP_OPT_MAXSEG,		107).
-define(SCTP_OPT_SET_PEER_PRIMARY_ADDR, 108).
-define(SCTP_OPT_PRIMARY_ADDR,		109).
-define(SCTP_OPT_ADAPTATION_LAYER,	110).
-define(SCTP_OPT_PEER_ADDR_PARAMS,	111).
-define(SCTP_OPT_DEFAULT_SEND_PARAM,	112).
-define(SCTP_OPT_EVENTS,		113).
-define(SCTP_OPT_DELAYED_ACK_TIME,	114).
-define(SCTP_OPT_STATUS,		115).
-define(SCTP_OPT_GET_PEER_ADDR_INFO,	116).

%% interface options, INET_REQ_IFGET and INET_REQ_IFSET
-define(INET_IFOPT_ADDR,      1).
-define(INET_IFOPT_BROADADDR, 2).
-define(INET_IFOPT_DSTADDR,   3).
-define(INET_IFOPT_MTU,       4).
-define(INET_IFOPT_NETMASK,   5).
-define(INET_IFOPT_FLAGS,     6).
-define(INET_IFOPT_HWADDR,    7). %% where support (e.g linux)

%% packet byte values, INET_LOPT_PACKET
-define(TCP_PB_RAW,     0).
-define(TCP_PB_1,       1).
-define(TCP_PB_2,       2).
-define(TCP_PB_4,       3).
-define(TCP_PB_ASN1,    4).
-define(TCP_PB_RM,      5).
-define(TCP_PB_CDR,     6).
-define(TCP_PB_FCGI,    7).
-define(TCP_PB_LINE_LF, 8).
-define(TCP_PB_TPKT,    9).
-define(TCP_PB_HTTP,    10).
-define(TCP_PB_HTTPH,   11).
-define(TCP_PB_SSL_TLS, 12).
-define(TCP_PB_HTTP_BIN,13).
-define(TCP_PB_HTTPH_BIN,14).


%% getstat, INET_REQ_GETSTAT
-define(INET_STAT_RECV_CNT,  1).
-define(INET_STAT_RECV_MAX,  2).
-define(INET_STAT_RECV_AVG,  3).
-define(INET_STAT_RECV_DVI,  4).
-define(INET_STAT_SEND_CNT,  5).
-define(INET_STAT_SEND_MAX,  6).
-define(INET_STAT_SEND_AVG,  7).
-define(INET_STAT_SEND_PEND, 8).
-define(INET_STAT_RECV_OCT,  9).
-define(INET_STAT_SEND_OCT,  10).

%% interface stuff, INET_IFOPT_FLAGS
-define(INET_IFNAMSIZ,          16).
-define(INET_IFF_UP,            16#0001).
-define(INET_IFF_BROADCAST,     16#0002).
-define(INET_IFF_LOOPBACK,      16#0004).
-define(INET_IFF_POINTTOPOINT,  16#0008).
-define(INET_IFF_RUNNING,       16#0010).
-define(INET_IFF_MULTICAST,     16#0020).
%%
-define(INET_IFF_DOWN,          16#0100).
-define(INET_IFF_NBROADCAST,    16#0200).
-define(INET_IFF_NPOINTTOPOINT, 16#0800).

%% SCTP Flags for "sctp_sndrcvinfo":
%% INET_REQ_SETOPTS:SCTP_OPT_DEFAULT_SEND_PARAM
-define(SCTP_FLAG_UNORDERED, 1). 	% sctp_unordered
-define(SCTP_FLAG_ADDR_OVER, 2). 	% sctp_addr_over
-define(SCTP_FLAG_ABORT,     4). 	% sctp_abort
-define(SCTP_FLAG_EOF,	     8). 	% sctp_eof
-define(SCTP_FLAG_SNDALL,   16). 	% sctp_sndall, NOT YET IMPLEMENTED.

%% SCTP Flags for "sctp_paddrparams", and the corresp Atoms:
-define(SCTP_FLAG_HB_ENABLE,		1).	% sctp_hb_enable
-define(SCTP_FLAG_HB_DISABLE,		2).	% sctp_hb_disable
-define(SCTP_FLAG_HB_DEMAND,		4).	% sctp_hb_demand
-define(SCTP_FLAG_PMTUD_ENABLE,		8).	% sctp_pmtud_enable
-define(SCTP_FLAG_PMTUD_DISABLE,       16).	% sctp_pmtud_disable
-define(SCTP_FLAG_SACKDELAY_ENABLE,    32).	% sctp_sackdelay_enable
-define(SCTP_FLAG_SACKDELAY_DISABLE,   64).	% sctp_sackdelay_disable

%%
%% End of interface constants.
%%----------------------------------------------------------------------------

-define(LISTEN_BACKLOG, 5).     %% default backlog

%% 5 secs need more ???
-define(INET_CLOSE_TIMEOUT, 5000).

%%
%% Port/socket numbers: network standard functions
%%
-define(IPPORT_ECHO,             7).
-define(IPPORT_DISCARD,          9).
-define(IPPORT_SYSTAT,           11).
-define(IPPORT_DAYTIME,          13).
-define(IPPORT_NETSTAT,          15).
-define(IPPORT_FTP,              21).
-define(IPPORT_TELNET,           23).
-define(IPPORT_SMTP,             25).
-define(IPPORT_TIMESERVER,       37).
-define(IPPORT_NAMESERVER,       42).
-define(IPPORT_WHOIS,            43).
-define(IPPORT_MTP,              57).

%%
%% Port/socket numbers: host specific functions
%%
-define(IPPORT_TFTP,             69).
-define(IPPORT_RJE,              77).
-define(IPPORT_FINGER,           79).
-define(IPPORT_TTYLINK,          87).
-define(IPPORT_SUPDUP,           95).

%%
%% UNIX TCP sockets
%%
-define(IPPORT_EXECSERVER,       512).
-define(IPPORT_LOGINSERVER,      513).
-define(IPPORT_CMDSERVER,        514).
-define(IPPORT_EFSSERVER,        520).

%%
%% UNIX UDP sockets
%%
-define(IPPORT_BIFFUDP,          512).
-define(IPPORT_WHOSERVER,        513).
-define(IPPORT_ROUTESERVER,      520). %% 520+1 also used


%%
%% Ports < IPPORT_RESERVED are reserved for
%% privileged processes (e.g. root).
%% Ports > IPPORT_USERRESERVED are reserved
%% for servers, not necessarily privileged.
%%
-define(IPPORT_RESERVED,         1024).
-define(IPPORT_USERRESERVED,     5000).

%% standard port for socks
-define(IPPORT_SOCKS,           1080).

%%
%% Int to bytes
%%
-define(int8(X), [(X) band 16#ff]).

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int24(X), [((X) bsr 16) band 16#ff,
		   ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
	[((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(intAID(X), % For SCTP AssocID
        ?int32(X)).

%% Bytes to unsigned
-define(u64(X7,X6,X5,X4,X3,X2,X1,X0), 
	( ((X7) bsl 56) bor ((X6) bsl 48) bor ((X5) bsl 40) bor
	  ((X4) bsl 32) bor ((X3) bsl 24) bor ((X2) bsl 16) bor 
	  ((X1) bsl 8) bor (X0)  )).

-define(u32(X3,X2,X1,X0), 
	(((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u24(X2,X1,X0),
	(((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u16(X1,X0),
	(((X1) bsl 8) bor (X0))).
 
-define(u8(X0), (X0)).

%% Bytes to signed
-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

-define(i24(X2,X1,X0),
        (?u24(X2,X1,X0) - 
         (if (X2) > 127 -> 16#1000000; true -> 0 end))).
	
-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(i8(X0),
	(?u8(X0) -
	 (if (X0) > 127 -> 16#100; true -> 0 end))).

%% macro for use in guard for checking ip address {A,B,C,D}
-define(ip(A,B,C,D),
	(((A) bor (B) bor (C) bor (D)) band (bnot 16#ff)) =:= 0).

-define(ip6(A,B,C,D,E,F,G,H), 
	(((A) bor (B) bor (C) bor (D) bor (E) bor (F) bor (G) bor (H)) 
	 band (bnot 16#ffff)) =:= 0).

-define(ether(A,B,C,D,E,F), 
	(((A) bor (B) bor (C) bor (D) bor (E) bor (F)) 
	 band (bnot 16#ff)) =:= 0).

-define(port(P), (((P) band bnot 16#ffff) =:= 0)).

%% default options (when inet_drv port is started)
%%
%% bufsz   = INET_MIN_BUFFER (8K)
%% header  = 0
%% packet  = 0 (raw)
%% mode    = list
%% deliver = term
%% active  = false
%%
-record(connect_opts, 
	{ 
	  ifaddr = any,     %% bind to interface address
	  port   = 0,       %% bind to port (default is dynamic port)
	  fd     = -1,      %% fd >= 0 => already bound
	  opts   = []       %% [{active,true}] added in inet:connect_options
	 }).

-record(listen_opts, 
	{ 
	  ifaddr = any,              %% bind to interface address
	  port   = 0,                %% bind to port (default is dynamic port)
	  backlog = ?LISTEN_BACKLOG, %% backlog
	  fd      = -1,              %% %% fd >= 0 => already bound
	  opts   = []                %% [{active,true}] added in 
	                             %% inet:listen_options
	 }).

-record(udp_opts,
	{
	  ifaddr = any,
	  port   = 0,
	  fd     = -1,
	  opts   = [{active,true}]
	 }).

-define(SCTP_DEF_BUFSZ, 65536).
-define(SCTP_DEF_IFADDR, any).
-record(sctp_opts,
	{
	  ifaddr,
	  port   = 0,
	  fd	 = -1,
	  type   = seqpacket,
	  opts   = [{mode,	  binary},
		    {buffer,	  ?SCTP_DEF_BUFSZ},
		    {sndbuf,	  ?SCTP_DEF_BUFSZ},
		    {recbuf,	  1024},
		    {sctp_events, undefined}%,
		    %%{active,      true}
		   ]
        }).

%% The following Tags are purely internal, used for marking items in the
%% send buffer:
-define(SCTP_TAG_SEND_ANC_INITMSG,	0).
-define(SCTP_TAG_SEND_ANC_PARAMS,	1).
-define(SCTP_TAG_SEND_DATA,		2).
