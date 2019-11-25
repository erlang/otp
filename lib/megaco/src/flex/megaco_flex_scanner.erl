%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose : Scanner for text encoded Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_flex_scanner).

-export([is_enabled/0, is_reentrant_enabled/0, is_scanner_port/2]).
-export([start/0, start/1, stop/1, scan/2]).

-define(NUM_SCHED(),           erlang:system_info(schedulers)).
-define(SCHED_ID(),            erlang:system_info(scheduler_id)).
-define(SMP_SUPPORT_DEFAULT(), erlang:system_info(smp_support)).

-dialyzer({nowarn_function, is_enabled/0}).
is_enabled() -> 
    (true =:= ?ENABLE_MEGACO_FLEX_SCANNER).

-dialyzer({nowarn_function, is_reentrant_enabled/0}).
is_reentrant_enabled() ->
    (true =:= ?MEGACO_REENTRANT_FLEX_SCANNER).

is_scanner_port(Port, Port) when is_port(Port) ->
    true;
is_scanner_port(Port, Ports) when is_tuple(Ports) ->
    is_own_port(Port, Ports);
is_scanner_port(_, _) ->
    false.

is_own_port(Port, Ports) ->
    is_own_port(Port, size(Ports), Ports).

is_own_port(_Port, 0, _Ports) ->
    false;
is_own_port(Port, N, Ports) when (N > 0) ->
    case element(N, Ports) of
	Port ->
	    true;
	_ ->
	    is_own_port(Port, N-1, Ports)
    end.

	    
%%----------------------------------------------------------------------
%% Start the flex scanner
%%----------------------------------------------------------------------

start() ->
    start(?SMP_SUPPORT_DEFAULT()).

start(SMP) when ((SMP =:= true) orelse (SMP =:= false)) ->
    (catch do_start(is_reentrant_enabled() andalso SMP)).

do_start(SMP) ->
    Path = lib_dir(),
    erl_ddll:start(), 
    load_driver(Path),
    PortOrPorts = open_drv_port(SMP),
    {ok, PortOrPorts}.


lib_dir() ->
    case code:priv_dir(megaco) of
	{error, Reason} ->
	    throw({error, {priv_dir, Reason}});
	P when is_list(P) ->
	    P ++ "/lib"
    end.
    

load_driver(Path) ->
    case erl_ddll:load_driver(Path, drv_name()) of
	ok ->
	    ok;
	{error, Reason} ->
	    case (catch erl_ddll:format_error(Reason)) of
		FormatReason when is_list(FormatReason) ->
		    throw({error, {load_driver, FormatReason}});
		_ ->
		    throw({error, {load_driver, Reason}})
	    end
    end.


open_drv_port(true) ->
    open_drv_ports(?NUM_SCHED(), []);
open_drv_port(_) ->
    open_drv_port().

open_drv_ports(0, Acc) ->
    list_to_tuple(Acc);
open_drv_ports(N, Acc) when is_integer(N) andalso (N > 0) ->
    Port = open_drv_port(),
    open_drv_ports(N-1, [Port | Acc]).

open_drv_port() ->
    case (catch erlang:open_port({spawn, drv_name()}, [binary])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', Reason} ->
	    erl_ddll:unload_driver(drv_name()),
	    throw({error, {open_port, Reason}})
    end.

drv_name() ->
    case erlang:system_info(threads) of
	true ->
	    "megaco_flex_scanner_drv_mt";
	false ->
	    "megaco_flex_scanner_drv"
    end.


%%----------------------------------------------------------------------
%% Stop the flex scanner
%%----------------------------------------------------------------------

stop(Port) when is_port(Port) ->
    erlang:port_close(Port), 
    erl_ddll:unload_driver(drv_name()),
    stopped;
stop(Ports) when is_tuple(Ports) ->
    stop(tuple_to_list(Ports));
stop(Ports) when is_list(Ports) ->
    lists:foreach(fun(Port) ->  erlang:port_close(Port) end, Ports),
    erl_ddll:unload_driver(drv_name()),
    stopped.


%%----------------------------------------------------------------------
%% Scan a message
%%----------------------------------------------------------------------

scan(Binary, Port) when is_port(Port) ->
    do_scan(Binary, Port);
scan(Binary, Ports) when is_tuple(Ports) ->
%%     p("scan -> entry with"
%%       "~n   Ports: ~p", [Ports]),
    do_scan(Binary, select_port(Ports)).

do_scan(Binary, Port) ->
%%     p("do_scan -> entry with"
%%       "~n   Port: ~p", [Port]),
    case erlang:port_control(Port, $s, Binary) of
	[] ->
	    receive
		{tokens, Tokens, LatestLine} ->
%% 		    p("do_scan -> OK with:"
%% 		      "~n   length(Tokens): ~p"
%% 		      "~n   LatestLine:     ~p", [length(Tokens), LatestLine]),
		    Vsn = version(Tokens),
		    {ok, Tokens, Vsn, LatestLine} 
	    after 5000 ->
%%  		    p("do_scan -> ERROR", []),
		    {error, "Driver term send failure", 1}
	    end;
	Reason ->
%% 	    p("do_scan -> port control failed: "
%% 	      "~n   Reason: ~p", [Reason]),
	    {error, Reason, 1}
    end.

select_port(Ports) ->
    SchedId = ?SCHED_ID(),
    %% lists:nth(SchedId, Ports).
    element(SchedId, Ports).

version([]) ->
    99; % Let the parser deal with this
version([{'SafeChars',_,"!/1"}|_]) ->
    1;
version([{'SafeChars',_,"megaco/1"}|_]) ->
    1;
version([{'SafeChars',_,"!/2"}|_]) ->
    2;
version([{'SafeChars',_,"megaco/2"}|_]) ->
    2;
version([{'SafeChars',_,"!/3"}|_]) ->
    3;
version([{'SafeChars',_,"megaco/3"}|_]) ->
    3;
version([{'SafeChars',_,[$!, $/ | Vstr]}|_]) ->
    guess_version(Vstr);
version([{'SafeChars',_,[$m, $e, $g, $a, $c, $o, $/ | Vstr]}|_]) ->
    guess_version(Vstr);
version([_|T]) ->
    version(T).


guess_version([C]) when (48 =< C) and (C =< 57) ->
    C-48;
guess_version(Str) when is_list(Str) ->
    case (catch list_to_integer(Str)) of
	I when is_integer(I) ->
	    I;
	_ ->
	    99 % Let the parser deal with this
    end;
guess_version(_) ->
    99. % Let the parser deal with this


%% p(F, A) ->
%%     io:format("~w [~p,~p] " ++ F ++ "~n", [?MODULE, self(), ?SCHED_ID() | A]).
