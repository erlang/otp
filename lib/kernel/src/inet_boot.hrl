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
%% Defines used for erlang boot/load protocol
%%

-define(EBOOT_PORT, 4368).    %% same as epmd but for udp !

-define(EBOOT_REQUEST, "EBOOTQ").
-define(EBOOT_REPLY,   "EBOOTR").

-define(EBOOT_RETRY,                 3). % number of retry before sleep
-define(EBOOT_REQUEST_DELAY,       500). % delay between retry
-define(EBOOT_SHORT_RETRY_SLEEP, 10000). % initial sleep time between boot attempt's
-define(EBOOT_UNSUCCESSFUL_TRIES,   10). % retries before longer sleep
-define(EBOOT_LONG_RETRY_SLEEP,  60000). % sleep time after a number of unsuccessful tries 
