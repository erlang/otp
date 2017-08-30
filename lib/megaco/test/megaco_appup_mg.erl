%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Used when performing testing appup tests
%% 
%% {ok, P} = megaco_appup_mg:start().
%% megaco_appup_mg:stop(P).
%% megaco_appup_mg:verbosity(P,silence).
%% megaco_appup_mg:verbosity(P,debug).
%% megaco_appup_mg:timeout(P,100).
%% megaco_appup_mg:aam(P,10).
%% megaco_appup_mg:aam(P,15).
%% megaco_appup_mg:aat(P,2000).
%% megaco_appup_mg:aat(P,10000).
%% 
%%----------------------------------------------------------------------

-module(megaco_appup_mg).

%% API
-export([start1/0, start2/0, start3/0, start4/0]).
-export([start/0,  start/1,  start/2]).
-export([verbosity/2, timeout/2]).
-export([aat/2, aam/2]).

%% Internal
-export([main/4]).

%% Constants:
-define(TIMEOUT,1000).
-define(MAXCOUNT,1).

%% Wrapper macros
-define(START(Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(node(), Mid, Enc, Transp, Conf, Verb)).
-define(STOP(Pid),        megaco_test_mg:stop(Pid)).
-define(SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(NOTIF_RAR(Pid),   megaco_test_mg:notify_request_and_reply(Pid)).
-define(GRP_REQ(Pid,N),   megaco_test_mg:group_requests(Pid,N)).
-define(VERBOSITY(Pid,V), megaco_test_mg:verbosity(Pid,V)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start1() -> start(text,   tcp).
start2() -> start(text,   udp).
start3() -> start(binary, tcp).
start4() -> start(binary, udp).

start() ->
    start(text, tcp).

start(tcp) ->
    start(text, tcp);
start(udp) ->
    start(text, udp).

start(Encoding, Transport) ->
    proc_lib:start_link(?MODULE, main, [self(), Encoding, Transport, 1]).

stop(Pid) ->
    Pid ! stop.

verbosity(Pid, V) ->
    Pid ! {verbosity, V}.

timeout(Pid, T) ->
    Pid ! {timeout, T}.

aat(Pid, Val) ->
    uci(Pid, accu_ack_timer, Val).

aam(Pid, Val) ->
    uci(Pid, accu_ack_maxcount, Val).

uci(Pid, Item, Val) ->
    Pid ! {update_conn_info, Item, Val}.


%% -------------------------------------------------------------------------

%% - start function - 
main(Parent, Encoding, Transport, MaxCount) ->
    Mg = init(Encoding, Transport, MaxCount),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Mg, ?TIMEOUT, ?TIMEOUT).

%% - init - 
init(Encoding, Transport, MaxCount) ->
    verify_encoding(Encoding),
    verify_transport(Transport),

    Mid      = {deviceName, "mg"},
    Config   = [{auto_ack,          true} %, 
		%{trans_timer,       2000}, 
		%{trans_ack_maxcount, MaxCount}
	       ],
    
    d("start MG"),
    {ok, Mg} = ?START(Mid, Encoding, Transport, Config, debug),

    d("service change"),
    ok = ?SERV_CHANGE(Mg),

    ?GRP_REQ(Mg, MaxCount),

    Mg.

%% - main loop - 
loop(Mg, Timeout, To) when To =< 0 ->
    ?NOTIF_RAR(Mg),
    loop(Mg, Timeout, Timeout);
loop(Mg, Timeout, To) ->
    Start = t(),
    receive
	stop ->
	    d("stop"),
	    megaco_test_mg:stop(Mg),
	    exit(normal);

	{update_conn_info, Item, Val} ->
	    d("update_conn_info -> ~p:~p", [Item, Val]),
	    megaco_test_mg:update_conn_info(Mg, Item, Val),
	    loop(Mg, Timeout, To - (t() - Start));

	{request_group_size, Size} when Size > 1 ->
	    d("request_group_size -> ~p", [Size]),
	    ?GRP_REQ(Mg, Size),
	    loop(Mg, Timeout, To - (t() - Start));

	{verbosity, V} ->
	    d("verbosity: ~p", [V]),
	    ?VERBOSITY(Mg, V),
	    loop(Mg, Timeout, To - (t() - Start));

	{timeout, T} ->
	    d("timeout: ~p", [T]),
	    T1 = T - Timeout,
	    loop(Mg, T, To - (t() - Start) + T1);

	Any ->
	    error("received unknown request: ~n~p", [Any]),
	    loop(Mg, Timeout, To - (t() - Start))

    after To ->
	    ?NOTIF_RAR(Mg),
	    loop(Mg, Timeout, Timeout)
    end.
    
    

%% -------------------------------------------------------------------------

verify_encoding(text)   -> ok;
verify_encoding(binary) -> ok;
verify_encoding(Encoding) -> exit({invalid_encoding, Encoding}).

verify_transport(tcp)       -> ok;
verify_transport(udp)       -> ok;
verify_transport(Transport) -> exit({invalid_transport, Transport}).

%% - 

sleep(X) -> receive after X -> ok end.

t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


%% - 

error(F, A) ->
    d("ERROR: " ++ F, A).

d(F) ->
    d(F, []).

d(F, A) ->
    io:format("~pAMG-" ++ F ++ "~n", [self()|A]).
