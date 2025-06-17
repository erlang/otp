%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
%%
%% Definition for Domain Name System
%%

%%
%% Currently defined opcodes
%%
-define(QUERY,    16#0).	%% standard query
-define(IQUERY,   16#1).	%% inverse query
-define(STATUS,   16#2).	%% nameserver status query
-define(NOTIFY,   16#4).	%% notify
-define(UPDATE,   16#5).	%% dynamic update

%%
%% Currently defined response codes
%%
-define(NOERROR,  0).		%% no error
-define(FORMERR,  1).		%% format error
-define(SERVFAIL, 2).		%% server failure
-define(NXDOMAIN, 3).		%% non existent domain
-define(NOTIMP,	  4).		%% not implemented
-define(REFUSED,  5).		%% query refused
-define(YXDOMAIN, 6).		%% name exists when it should not (DDNS)
-define(YXRRSET,  7).		%% RR set exists when it should not (DDNS)
-define(NXRRSET,  8).		%% RR set that should exist does not (DDNS)
-define(NOTAUTH,  9).		%% server not authoritative for zone (DDNS)
-define(NOTZONE,  10).		%% name not contained in zone (DDNS)
-define(BADVERS,  16).		%% bad version EDNS pseudo-rr RFC6891: 6.1.3
-define(BADSIG,   16).		%% TSIG Signature Failure (TSIG)
-define(BADKEY,   17).		%% Key not recognized (TSIG)
-define(BADTIME,  18).		%% Signature out of time window (TSIG)
-define(BADTRUNC, 22).		%% Bad Truncation (TSIG)

%%
%% Type values for resources and queries
%%
-define(T_A,		1).		%% host address
-define(T_NS,		2).		%% authoritative server
-define(T_MD,		3).		%% mail destination
-define(T_MF,		4).		%% mail forwarder
-define(T_CNAME,	5).		%% connonical name
-define(T_SOA,		6).		%% start of authority zone
-define(T_MB,		7).		%% mailbox domain name
-define(T_MG,		8).		%% mail group member
-define(T_MR,		9).		%% mail rename name
-define(T_NULL,		10).		%% null resource record
-define(T_WKS,		11).		%% well known service
-define(T_PTR,		12).		%% domain name pointer
-define(T_HINFO,	13).		%% host information
-define(T_MINFO,	14).		%% mailbox information
-define(T_MX,		15).		%% mail routing information
-define(T_TXT,		16).		%% text strings
-define(T_AAAA,         28).            %% ipv6 address
%% LOC (RFC 1876)
-define(T_LOC,          29).            %% location information
%% SRV (RFC 2052)
-define(T_SRV,          33).            %% services
%% NAPTR (RFC 2915)
-define(T_NAPTR,        35).            %% naming authority pointer
-define(T_OPT,          41).            %% EDNS pseudo-rr RFC6891(7)
%% SPF (RFC 4408)
-define(T_SPF,          99).            %% server policy framework
%%      non standard
-define(T_UINFO,	100).		%% user (finger) information
-define(T_UID,		101).		%% user ID
-define(T_GID,		102).		%% group ID
-define(T_UNSPEC,	103).		%% Unspecified format (binary data)
-define(T_TSIG,		250).		%% transaction signature
-define(T_IXFR,		251).		%% incremental zone transfer
-define(T_AXFR,		252).		%% zone transfer
-define(T_MAILB,	253).		%% transfer mailbox records
-define(T_MAILA,	254).		%% transfer mail agent records
-define(T_ANY,		255).		%% wildcard match
%% URI (RFC 7553)
-define(T_URI,		256).		%% uniform resource identifier
%% CAA (RFC 6844)
-define(T_CAA,		257).		%% certification authority authorization

%%
%% Symbolic Type values for resources and queries
%%
-define(S_A,		a).		%% host address
-define(S_NS,		ns).		%% authoritative server
-define(S_MD,		md).		%% mail destination
-define(S_MF,		mf).		%% mail forwarder
-define(S_CNAME,	cname).		%% connonical name
-define(S_SOA,		soa).		%% start of authority zone
-define(S_MB,		mb).		%% mailbox domain name
-define(S_MG,		mg).		%% mail group member
-define(S_MR,		mr).		%% mail rename name
-define(S_NULL,		null).		%% null resource record
-define(S_WKS,		wks).		%% well known service
-define(S_PTR,		ptr).		%% domain name pointer
-define(S_HINFO,	hinfo).		%% host information
-define(S_MINFO,	minfo).		%% mailbox information
-define(S_MX,		mx).		%% mail routing information
-define(S_TXT,		txt).		%% text strings
-define(S_AAAA,         aaaa).          %% ipv6 address
%% LOC (RFC 1876)
-define(S_LOC,          loc).           %% location information
%% SRV (RFC 2052)
-define(S_SRV,          srv).           %% services
%% NAPTR (RFC 2915)
-define(S_NAPTR,        naptr).         %% naming authority pointer
-define(S_OPT,          opt).           %% EDNS pseudo-rr RFC6891(7)
%% SPF (RFC 4408)
-define(S_SPF,          spf).           %% server policy framework
%%      non standard
-define(S_UINFO,	uinfo).		%% user (finger) information
-define(S_UID,		uid).		%% user ID
-define(S_GID,		gid).		%% group ID
-define(S_UNSPEC,	unspec).        %% Unspecified format (binary data)
-define(S_TSIG,		tsig).		%% transaction signature
-define(S_IXFR,		ixfr).		%% incremental zone transfer
-define(S_AXFR,		axfr).		%% zone transfer
-define(S_MAILB,	mailb).		%% transfer mailbox records
-define(S_MAILA,	maila).		%% transfer mail agent records
-define(S_ANY,		any).		%% wildcard match
%% URI (RFC 7553)
-define(S_URI,		uri).		%% uniform resource identifier
%% CAA (RFC 6844)
-define(S_CAA,		caa).		%% certification authority authorization

%%
%% Values for class field
%%
-define(C_IN,		1).      	%% the arpa internet
-define(C_CHAOS,	3).		%% for chaos net at MIT
-define(C_HS,		4).		%% for Hesiod name server at MIT
-define(C_NONE,		254).		%% for DDNS (RFC2136, section 2.4)
-define(C_ANY,		255).		%% wildcard match

%%
%% TSIG Algorithms and Identifiers (RFC8945, section 6)
%%
-define(T_TSIG_HMAC_MD5,		"HMAC-MD5.SIG-ALG.REG.INT").
-define(T_TSIG_GSS_TSIG,		"gss-tsig").
-define(T_TSIG_HMAC_SHA1,		"hmac-sha1").
-define(T_TSIG_HMAC_SHA1_96,		"hmac-sha1_96").
-define(T_TSIG_HMAC_SHA224,		"hmac-sha224").
-define(T_TSIG_HMAC_SHA256,		"hmac-sha256").
-define(T_TSIG_HMAC_SHA256_128,		"hmac-sha256-128").
-define(T_TSIG_HMAC_SHA384,		"hmac-sha384").
-define(T_TSIG_HMAC_SHA384_192,		"hmac-sha384-192").
-define(T_TSIG_HMAC_SHA512,		"hmac-sha512").
-define(T_TSIG_HMAC_SHA512_256,		"hmac-sha512-256").
% map mostly to crypto:hmac_hash_algorithm()
-define(S_TSIG_HMAC_MD5,		md5).
-define(S_TSIG_GSS_TSIG,		gss_tsig).
-define(S_TSIG_HMAC_SHA1,		sha).
-define(S_TSIG_HMAC_SHA1_96,		{sha,96}).
-define(S_TSIG_HMAC_SHA224,		sha224).
-define(S_TSIG_HMAC_SHA256,		sha256).
-define(S_TSIG_HMAC_SHA256_128,		{sha256,128}).
-define(S_TSIG_HMAC_SHA384,		sha384).
-define(S_TSIG_HMAC_SHA384_192,		{sha384,192}).
-define(S_TSIG_HMAC_SHA512,		sha512).
-define(S_TSIG_HMAC_SHA512_256,		{sha512,256}).

%%
%% Structure for query header, the order of the fields is machine and
%% compiler dependent, in our case, the bits within a byte are assignd
%% least significant first, while the order of transmission is most
%% significant first.  This requires a somewhat confusing rearrangement.
%%
-record(dns_header,
	{
	 id = 0,       %% ushort query identification number
	 %% byte F0
	 qr = 0,       %% :1   response flag
	 opcode = 0,   %% :4   purpose of message
	 aa = 0,       %% :1   authoritative answer
	 tc = 0,       %% :1   truncated message
	 rd = 0,       %% :1   recursion desired
	 %% byte F1
	 ra = 0,       %% :1   recursion available
	 pr = 0,       %% :1   primary server required (non standard)
	               %% :2   unused bits
	 rcode = 0     %% :4   response code
	}).

-record(dns_rec,
	{
	 header,       %% dns_header record
	 qdlist = [],  %% list of question (for UPDATE 'zone') entries
	 anlist = [],  %% list of answer (for UPDATE 'prequisites') entries
	 nslist = [],  %% list of authority (for UPDATE 'update') entries
	 arlist = []   %% list of resource entries
	}).

%% DNS resource record
-record(dns_rr,
	{
	 domain = "",   %% resource domain
	 type = any,    %% resource type
	 class = in,    %% resource class
	 cnt = 0,       %% access count
	 ttl = 0,       %% time to live
	 data = [],     %% raw data
	 %%
	 tm,            %% creation time
         bm = "",       %% Used to be defined as:
         %%                Bitmap storing domain character case information
         %%       but now; Case normalized domain
         func = false   %% Was: Optional function calculating the data field.
         %%                Now: cache-flush Class flag from mDNS RFC 6762
	}).

-define(DNS_UDP_PAYLOAD_SIZE, 1280).

-record(dns_rr_opt,           %% EDNS RR OPT (RFC6891), dns_rr{type=opt}
	{
	  domain = "",        %% should be the root domain
	  type = opt,
	  udp_payload_size = ?DNS_UDP_PAYLOAD_SIZE, %% RFC6891(6.2.3 CLASS)
	  ext_rcode = 0,      %% RFC6891(6.1.3 EXTENDED-RCODE)
	  version = 0,        %% RFC6891(6.1.3 VERSION)
	  z = 0,              %% RFC6891(6.1.3 Z)
	  data = [],          %% RFC6891(6.1.2 RDATA)
          do = false          %% RFC6891(6.1.3 DO)
	 }).

-record(dns_rr_tsig,          %% TSIG RR OPT (RFC8945), dns_rr{type=tsig}
	{
	  domain = "",        %% name of the key
	  type = tsig,
	  offset,             %% position of RR in packet
	  %% RFC8945(4.2 TSIG Record Format)
	  algname,
	  now,
	  fudge,
	  mac,
	  original_id = #dns_header{}#dns_header.id,
	  error = #dns_header{}#dns_header.rcode,
	  other_data = <<>>
	 }).

-record(dns_query,
	{
	 domain,                    %% query domain
	 type,                      %% query type
	 class,                     %% query class
         unicast_response = false   %% mDNS RFC 6762 Class flag
	 }).
