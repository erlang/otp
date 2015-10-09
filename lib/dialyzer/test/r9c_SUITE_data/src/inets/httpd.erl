%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd.erl,v 1.1 2008/12/17 09:53:33 mikpe Exp $
%%
-module(httpd).
-export([multi_start/1, multi_start_link/1,
	 start/0, start/1, start/2,
	 start_link/0, start_link/1, start_link/2,
	 start_child/0,start_child/1,
	 multi_stop/1,
	 stop/0,stop/1,stop/2,
	 stop_child/0,stop_child/1,stop_child/2,
	 multi_restart/1,
	 restart/0,restart/1,restart/2,
	 parse_query/1]).

%% Optional start related stuff...
-export([load/1,        load_mime_types/1,
	 start2/1,      start2/2,
	 start_link2/1, start_link2/2,
	 stop2/1]).

%% Management stuff
-export([block/0,block/1,block/2,block/3,block/4,
	 unblock/0,unblock/1,unblock/2]).

%% Debugging and status info stuff...
-export([verbosity/3,verbosity/4]).
-export([get_status/1,get_status/2,get_status/3,
	 get_admin_state/0,get_admin_state/1,get_admin_state/2,
	 get_usage_state/0,get_usage_state/1,get_usage_state/2]).

-include("httpd.hrl").

-define(D(F, A), io:format("~p:" ++ F ++ "~n", [?MODULE|A])).


%% start

start() ->
    start("/var/tmp/server_root/conf/8888.conf").

start(ConfigFile) ->
    %% ?D("start(~s) -> entry", [ConfigFile]),
    start(ConfigFile, []).

start(ConfigFile, Verbosity) when list(ConfigFile), list(Verbosity) ->
    httpd_sup:start(ConfigFile, Verbosity).


%% start_link

start_link() ->
    start("/var/tmp/server_root/conf/8888.conf").

start_link(ConfigFile) ->
    start_link(ConfigFile, []).

start_link(ConfigFile, Verbosity) when list(ConfigFile), list(Verbosity) ->
    httpd_sup:start_link(ConfigFile, Verbosity).


%% start2 & start_link2

start2(Config) ->
    start2(Config, []).

start2(Config, Verbosity) when list(Config), list(Verbosity) ->
    httpd_sup:start2(Config, Verbosity).

start_link2(Config) ->
    start_link2(Config, []).

start_link2(Config, Verbosity) when list(Config), list(Verbosity) ->
    httpd_sup:start_link2(Config, Verbosity).


%% stop

stop() ->
  stop(8888).

stop(Port) when integer(Port) ->
    stop(undefined, Port);
stop(Pid) when pid(Pid) ->
    httpd_sup:stop(Pid);
stop(ConfigFile) when list(ConfigFile) ->
    %% ?D("stop(~s) -> entry", [ConfigFile]),
    httpd_sup:stop(ConfigFile).

stop(Addr, Port) when integer(Port) ->
    httpd_sup:stop(Addr, Port).

stop2(Config) when list(Config) ->
    httpd_sup:stop2(Config).

%% start_child

start_child() ->
    start_child("/var/tmp/server_root/conf/8888.conf").

start_child(ConfigFile) ->
    start_child(ConfigFile, []).

start_child(ConfigFile, Verbosity) ->
    inets_sup:start_child(ConfigFile, Verbosity).


%% stop_child

stop_child() ->
  stop_child(8888).

stop_child(Port) ->
    stop_child(undefined,Port).

stop_child(Addr, Port) when integer(Port) ->
    inets_sup:stop_child(Addr, Port).


%% multi_start

multi_start(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mstart(ConfigFiles);
	Error ->
	    Error
    end.

mstart(ConfigFiles) ->
    mstart(ConfigFiles,[]).
mstart([],Results) ->
    {ok,lists:reverse(Results)};
mstart([H|T],Results) ->
    Res = start(H),
    mstart(T,[Res|Results]).


%% multi_start_link

multi_start_link(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mstart_link(ConfigFiles);
	Error ->
	    Error
    end.

mstart_link(ConfigFiles) ->
    mstart_link(ConfigFiles,[]).
mstart_link([],Results) ->
    {ok,lists:reverse(Results)};
mstart_link([H|T],Results) ->
    Res = start_link(H),
    mstart_link(T,[Res|Results]).


%% multi_stop

multi_stop(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mstop(ConfigFiles);
	Error ->
	    Error
    end.

mstop(ConfigFiles) ->
    mstop(ConfigFiles,[]).
mstop([],Results) ->
    {ok,lists:reverse(Results)};
mstop([H|T],Results) ->
    Res = stop(H),
    mstop(T,[Res|Results]).


%% multi_restart

multi_restart(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mrestart(ConfigFiles);
	Error ->
	    Error
    end.

mrestart(ConfigFiles) ->
    mrestart(ConfigFiles,[]).
mrestart([],Results) ->
    {ok,lists:reverse(Results)};
mrestart([H|T],Results) ->
    Res = restart(H),
    mrestart(T,[Res|Results]).


%% restart

restart() -> restart(undefined,8888).

restart(Port) when integer(Port) ->
    restart(undefined,Port);
restart(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    restart(Addr,Port);
	Error ->
	    Error
    end.


restart(Addr,Port) when integer(Port) ->
    do_restart(Addr,Port).

do_restart(Addr,Port) when integer(Port) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:restart(Pid);
	_ ->
	    {error,not_started}
    end.


%%% =========================================================
%%% Function:    block/0, block/1, block/2, block/3, block/4
%%%              block()
%%%              block(Port)
%%%              block(ConfigFile)
%%%              block(Addr,Port)
%%%              block(Port,Mode)
%%%              block(ConfigFile,Mode)
%%%              block(Addr,Port,Mode)
%%%              block(ConfigFile,Mode,Timeout)
%%%              block(Addr,Port,Mode,Timeout)
%%%
%%% Returns:     ok | {error,Reason}
%%%
%%% Description: This function is used to block an HTTP server.
%%%              The blocking can be done in two ways,
%%%              disturbing or non-disturbing. Default is disturbing.
%%%              When a HTTP server is blocked, all requests are rejected
%%%              (status code 503).
%%%
%%%              disturbing:
%%%              By performing a disturbing block, the server
%%%              is blocked forcefully and all ongoing requests
%%%              are terminated. No new connections are accepted.
%%%              If a timeout time is given then, on-going requests
%%%              are given this much time to complete before the
%%%              server is forcefully blocked. In this case no new
%%%              connections is accepted.
%%%
%%%              non-disturbing:
%%%              A non-disturbing block is more gracefull. No
%%%              new connections are accepted, but the ongoing
%%%              requests are allowed to complete.
%%%              If a timeout time is given, it waits this long before
%%%              giving up (the block operation is aborted and the
%%%              server state is once more not-blocked).
%%%
%%% Types:       Port       -> integer()
%%%              Addr       -> {A,B,C,D} | string() | undefined
%%%              ConfigFile -> string()
%%%              Mode       -> disturbing | non_disturbing
%%%              Timeout    -> integer()
%%%
block() -> block(undefined,8888,disturbing).

block(Port) when integer(Port) ->
    block(undefined,Port,disturbing);

block(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    block(Addr,Port,disturbing);
	Error ->
	    Error
    end.

block(Addr,Port) when integer(Port) ->
    block(Addr,Port,disturbing);

block(Port,Mode) when integer(Port), atom(Mode) ->
    block(undefined,Port,Mode);

block(ConfigFile,Mode) when list(ConfigFile), atom(Mode) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    block(Addr,Port,Mode);
	Error ->
	    Error
    end.


block(Addr,Port,disturbing) when integer(Port) ->
    do_block(Addr,Port,disturbing);
block(Addr,Port,non_disturbing) when integer(Port) ->
    do_block(Addr,Port,non_disturbing);

block(ConfigFile,Mode,Timeout) when list(ConfigFile), atom(Mode), integer(Timeout) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    block(Addr,Port,Mode,Timeout);
	Error ->
	    Error
    end.


block(Addr,Port,non_disturbing,Timeout) when integer(Port), integer(Timeout) ->
    do_block(Addr,Port,non_disturbing,Timeout);
block(Addr,Port,disturbing,Timeout) when integer(Port), integer(Timeout) ->
    do_block(Addr,Port,disturbing,Timeout).

do_block(Addr,Port,Mode) when integer(Port), atom(Mode) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:block(Pid,Mode);
	_ ->
	    {error,not_started}
    end.


do_block(Addr,Port,Mode,Timeout) when integer(Port), atom(Mode) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:block(Pid,Mode,Timeout);
	_ ->
	    {error,not_started}
    end.


%%% =========================================================
%%% Function:    unblock/0, unblock/1, unblock/2
%%%              unblock()
%%%              unblock(Port)
%%%              unblock(ConfigFile)
%%%              unblock(Addr,Port)
%%%
%%% Description: This function is used to reverse a previous block
%%%              operation on the HTTP server.
%%%
%%% Types:       Port       -> integer()
%%%              Addr       -> {A,B,C,D} | string() | undefined
%%%              ConfigFile -> string()
%%%
unblock()                        -> unblock(undefined,8888).
unblock(Port) when integer(Port) -> unblock(undefined,Port);

unblock(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    unblock(Addr,Port);
	Error ->
	    Error
    end.

unblock(Addr,Port) when integer(Port) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:unblock(Pid);
	_ ->
	    {error,not_started}
    end.


verbosity(Port,Who,Verbosity) ->
    verbosity(undefined,Port,Who,Verbosity).

verbosity(Addr,Port,Who,Verbosity) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:verbosity(Pid,Who,Verbosity);
	_ ->
	    not_started
    end.


%%% =========================================================
%%% Function:    get_admin_state/0, get_admin_state/1, get_admin_state/2
%%%              get_admin_state()
%%%              get_admin_state(Port)
%%%              get_admin_state(Addr,Port)
%%%
%%% Returns:     {ok,State} | {error,Reason}
%%%
%%% Description: This function is used to retrieve the administrative
%%%              state of the HTTP server.
%%%
%%% Types:       Port    -> integer()
%%%              Addr    -> {A,B,C,D} | string() | undefined
%%%              State   -> unblocked | shutting_down | blocked
%%%              Reason  -> term()
%%%
get_admin_state()                        -> get_admin_state(undefined,8888).
get_admin_state(Port) when integer(Port) -> get_admin_state(undefined,Port);

get_admin_state(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    unblock(Addr,Port);
	Error ->
	    Error
    end.

get_admin_state(Addr,Port) when integer(Port) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:get_admin_state(Pid);
	_ ->
	    {error,not_started}
    end.



%%% =========================================================
%%% Function:    get_usage_state/0, get_usage_state/1, get_usage_state/2
%%%              get_usage_state()
%%%              get_usage_state(Port)
%%%              get_usage_state(Addr,Port)
%%%
%%% Returns:     {ok,State} | {error,Reason}
%%%
%%% Description: This function is used to retrieve the usage
%%%              state of the HTTP server.
%%%
%%% Types:       Port    -> integer()
%%%              Addr    -> {A,B,C,D} | string() | undefined
%%%              State   -> idle | active | busy
%%%              Reason  -> term()
%%%
get_usage_state()                        -> get_usage_state(undefined,8888).
get_usage_state(Port) when integer(Port) -> get_usage_state(undefined,Port);

get_usage_state(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    unblock(Addr,Port);
	Error ->
	    Error
    end.

get_usage_state(Addr,Port) when integer(Port) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:get_usage_state(Pid);
	_ ->
	    {error,not_started}
    end.



%%% =========================================================
%% Function:    get_status(ConfigFile)        -> Status
%%              get_status(Port)              -> Status
%%              get_status(Addr,Port)         -> Status
%%              get_status(Port,Timeout)      -> Status
%%              get_status(Addr,Port,Timeout) -> Status
%%
%% Arguments:   ConfigFile -> string()
%%                            Configuration file from which Port and
%%                            BindAddress will be extracted.
%%              Addr       -> {A,B,C,D} | string()
%%                            Bind Address of the http server
%%              Port       -> integer()
%%                            Port number of the http server
%%              Timeout    -> integer()
%%                            Timeout time for the call
%%
%% Returns:     Status -> list()
%%
%% Description: This function is used when the caller runs in the
%%              same node as the http server or if calling with a
%%              program such as erl_call (see erl_interface).
%%

get_status(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok,Addr,Port} ->
	    get_status(Addr,Port);
	Error ->
	    Error
    end;

get_status(Port) when integer(Port) ->
    get_status(undefined,Port,5000).

get_status(Port,Timeout) when integer(Port), integer(Timeout) ->
    get_status(undefined,Port,Timeout);

get_status(Addr,Port) when list(Addr), integer(Port) ->
    get_status(Addr,Port,5000).

get_status(Addr,Port,Timeout) when integer(Port) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:get_status(Pid,Timeout);
	_ ->
	    not_started
    end.


%% load config

load(ConfigFile) ->
    httpd_conf:load(ConfigFile).

load_mime_types(MimeTypesFile) ->
    httpd_conf:load_mime_types(MimeTypesFile).


%% parse_query

parse_query(String) ->
  {ok, SplitString} = regexp:split(String,"[&;]"),
  foreach(SplitString).

foreach([]) ->
  [];
foreach([KeyValue|Rest]) ->
  {ok, Plus2Space, _} = regexp:gsub(KeyValue,"[\+]"," "),
  case regexp:split(Plus2Space,"=") of
    {ok,[Key|Value]} ->
      [{httpd_util:decode_hex(Key),
	httpd_util:decode_hex(lists:flatten(Value))}|foreach(Rest)];
    {ok,_} ->
      foreach(Rest)
  end.


%% get_addr_and_port

get_addr_and_port(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    Port = httpd_util:key1search(ConfigList,port,80),
	    Addr = httpd_util:key1search(ConfigList,bind_address),
	    {ok,Addr,Port};
	Error ->
	    Error
    end.


%% make_name

make_name(Addr,Port) ->
    httpd_util:make_name("httpd",Addr,Port).


%% Multi stuff
%%

read_multi_file(File) ->
    read_mfile(file:open(File,[read])).

read_mfile({ok,Fd}) ->
    read_mfile(read_line(Fd),Fd,[]);
read_mfile(Error) ->
    Error.

read_mfile(eof,_Fd,SoFar) ->
    {ok,lists:reverse(SoFar)};
read_mfile({error,Reason},_Fd,SoFar) ->
    {error,Reason};
read_mfile([$#|Comment],Fd,SoFar) ->
    read_mfile(read_line(Fd),Fd,SoFar);
read_mfile([],Fd,SoFar) ->
    read_mfile(read_line(Fd),Fd,SoFar);
read_mfile(Line,Fd,SoFar) ->
    read_mfile(read_line(Fd),Fd,[Line|SoFar]).

read_line(Fd)      -> read_line1(io:get_line(Fd,[])).
read_line1(eof)    -> eof;
read_line1(String) -> httpd_conf:clean(String).
