%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%% Dns & resolver defintions
%%

-define(RES_TIMEOUT, 2000).      %% milli second between retries 
-define(RES_RETRY,   3).         %% number of retry
-define(RES_FILE_UPDATE_TM, 5).  %% seconds between file_info

-define(CACHE_LIMIT, 100).          %% number of cached dns_rr
-define(CACHE_REFRESH, 60*60*1000). %% refresh interval

-define(PACKETSZ,  512).         %% maximum packet size
-define(MAXDNAME,  256).         %% maximum domain name
-define(MAXCDNAME, 255).         %% maximum compressed domain name
-define(MAXLABEL,  63).		 %% maximum length of domain label
%%  Number of bytes of fixed size data in query structure 
-define(QFIXEDSZ,  4).
%% number of bytes of fixed size data in resource record 
-define(RRFIXEDSZ, 10).

%%
%% Internet nameserver port number
%%
-define(NAMESERVER_PORT, 53).
