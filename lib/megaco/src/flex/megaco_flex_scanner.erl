%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2001-2025. All Rights Reserved.
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
-moduledoc """
Interface module to the flex scanner linked in driver.

This module contains the public interface to the flex scanner linked in driver.
The flex scanner performs the scanning phase of text message decoding.

The flex scanner is written using a tool called _flex_. In order to be able to
compile the flex scanner driver, this tool has to be available.

By default the flex scanner reports line-number of an error. But it can be built
without line-number reporting. Instead token number is used. This will speed up
the scanning some 5-10%. Use `--disable-megaco-flex-scanner-lineno` when
configuring the application.

The scanner will, by default, be built as a reentrant scanner _if_ the flex
utility supports this (it depends on the version of flex). It is possible to
explicitly disable this even when flex support this. Use
`--disable-megaco-reentrant-flex-scanner` when configuring the application.
""".

-export([is_enabled/0, is_reentrant_enabled/0, is_scanner_port/2]).
-export([start/0, start/1, stop/1, scan/2]).

-export_type([
              megaco_ports/0
             ]).


%%----------------------------------------------------------------------

-doc """
Return value of a successful (flex) scanner start.
""".
-type megaco_ports() :: port() | tuple().


%%----------------------------------------------------------------------

-define(NUM_SCHED(),           erlang:system_info(schedulers)).
-define(SCHED_ID(),            erlang:system_info(scheduler_id)).
-define(SMP_SUPPORT_DEFAULT(), erlang:system_info(smp_support)).


%%----------------------------------------------------------------------

-ifndef(ENABLE_MEGACO_FLEX_SCANNER).
%% This crap is hopefully temporary!
%% It is because our current doc build
%% script (specs file generation) has
%% no way to pass this value in as the
%% normal compilation (erlc) does.
-define(ENABLE_MEGACO_FLEX_SCANNER, true).
-endif.

-ifndef(MEGACO_REENTRANT_FLEX_SCANNER).
%% This crap is hopefully temporary!
%% It is because our current doc build
%% script (specs file generation) has
%% no way to pass this value in as the
%% normal compilation (erlc) does.
-define(MEGACO_REENTRANT_FLEX_SCANNER, true).
-endif.

-dialyzer({nowarn_function, is_enabled/0}).
-doc false.
-spec is_enabled() -> boolean().
is_enabled() -> 
    (true =:= ?ENABLE_MEGACO_FLEX_SCANNER).

-dialyzer({nowarn_function, is_reentrant_enabled/0}).
-doc """
Is the flex scanner reentrant or not.
""".
-spec is_reentrant_enabled() -> boolean().
is_reentrant_enabled() ->
    (true =:= ?MEGACO_REENTRANT_FLEX_SCANNER).


%%----------------------------------------------------------------------

-doc """
Checks if a port is a flex scanner port or not (useful when if a port exits).
""".
-spec is_scanner_port(Port, PortOrPorts) -> boolean() when
      Port        :: port(),
      PortOrPorts :: megaco_ports().
          
is_scanner_port(Port, Port) when is_port(Port) ->
    true;
is_scanner_port(Port, Ports) when is_tuple(Ports) ->
    is_own_port(Port, Ports);
is_scanner_port(_, _) ->
    false.

is_own_port(Port, Ports) when is_tuple(Ports)->
    is_own_port(Port, tuple_size(Ports), Ports).

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

-doc """
This function is used to start the flex scanner. It locates the library and
loads the linked in driver.

On a single core system or if it's a non-reentrant scanner, a single port is
created. On a multi-core system with a reentrant scanner, several ports will be
created (one for each scheduler).

Note that the process that calls this function _must_ be permanent. If it dies,
the port(s) will exit and the driver unload.
""".
-spec start() -> {ok, PortOrPorts} | {error, Reason} when
      PortOrPorts :: megaco_ports(),
      Reason      :: term().

start() ->
    start(?SMP_SUPPORT_DEFAULT()).

-doc false.
start(SMP) when ((SMP =:= true) orelse (SMP =:= false)) ->
    (catch do_start(is_reentrant_enabled() andalso SMP)).

do_start(SMP) ->
    Path = lib_dir(),
    _ = erl_ddll:start(), 
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
	    _ = erl_ddll:unload_driver(drv_name()),
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

-doc """
This function is used to stop the flex scanner. It also unloads the driver.
""".
-spec stop(PortOrPorts) -> stopped when
      PortOrPorts :: megaco_ports().

stop(Port) when is_port(Port) ->
    erlang:port_close(Port), 
    _ = erl_ddll:unload_driver(drv_name()),
    stopped;
stop(Ports) when is_tuple(Ports) ->
    lists:foreach(fun(Port) ->
                          erlang:port_close(Port)
                  end, tuple_to_list(Ports)),
    _ = erl_ddll:unload_driver(drv_name()),
    stopped.


%%----------------------------------------------------------------------
%% Scan a message
%%----------------------------------------------------------------------

-doc "Scans a megaco message and generates a token list to be passed on the parser.".
-spec scan(Binary, PortOrPorts) ->
          {ok, Tokens, Version, LatestLine} |
          {error, Reason, LatestLine} when
      Binary      :: binary(),
      PortOrPorts :: megaco_ports(),
      Tokens      :: list(),
      Version     :: megaco_encoder:protocol_version(),
      LatestLine  :: non_neg_integer(),
      Reason      :: term().

scan(Binary, Port) when is_port(Port) ->
    do_scan(Binary, Port);
scan(Binary, Ports) when is_tuple(Ports) ->
    do_scan(Binary, select_port(Ports)).

do_scan(Binary, Port) ->
    case erlang:port_control(Port, $s, Binary) of
	[] ->
	    receive
		{tokens, Tokens, LatestLine} ->
		    Version = version(Tokens),
		    {ok, Tokens, Version, LatestLine} 
	    after 5000 ->
		    {error, "Driver term send failure", 1}
	    end;
	Reason ->
	    {error, Reason, 1}
    end.

select_port(Ports) ->
    SchedId = ?SCHED_ID(),
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
