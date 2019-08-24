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
