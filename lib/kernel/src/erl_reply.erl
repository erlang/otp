%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(erl_reply).

%% Syncronisation with erl_start (erl_interface)

-export([reply/1]).

%% send Msg to Addr:Port
%% all args are atoms since we call this from erl command line

-spec reply([atom()]) -> 'ok' | 'reply_done'.

reply([Addr,Port,Msg]) ->
    Ip = ip_string_to_tuple(atom_to_list(Addr)),
    P = list_to_integer(atom_to_list(Port)),
    M = atom_to_list(Msg),
    {ok, S} = gen_tcp:connect(Ip,P,[]),
    ok = gen_tcp:send(S,M),
    gen_tcp:close(S),
    reply_done;
reply(_) ->
    error_logger:error_msg("erl_reply: Can't find address and port "
			   "to reply to~n").

%% convert ip number to tuple
ip_string_to_tuple(Ip) ->
    [Ip1,Ip2,Ip3,Ip4] = string:lexemes(Ip,"."),
    {list_to_integer(Ip1),
     list_to_integer(Ip2),
     list_to_integer(Ip3),
     list_to_integer(Ip4)}.

