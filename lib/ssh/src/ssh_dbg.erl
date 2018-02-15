%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

%%% Purpose:
%%%  This module implements support for using the Erlang trace in a simple way for ssh
%%%  debugging.
%%%
%%%  Begin the session with ssh_dbg:start(). This will do a dbg:start() if needed and
%%%  then dbg:p/2 to set some flags.
%%%
%%%  Next select trace points to activate: for example plain text printouts of messages
%%%  sent or received. This is switched on and off with ssh_dbg:on(TracePoint(s)) and
%%%  ssh_dbg:off(TracePoint(s)).  For example:
%%%
%%%      ssh_dbg:on(messages)         -- switch on printing plain text messages
%%%      ssh_dbg:on([alg,terminate])  -- switch on printing info about algorithm negotiation
%%%      ssh_dbg:on()                 -- switch on all ssh debugging
%%%
%%%  To switch, use the off/0 or off/1 function in the same way, for example:
%%%
%%%      ssh_dbg:off(alg)             -- switch off algorithm negotiation tracing, but keep all other
%%%      ssh_dbg:off()                -- switch off all ssh debugging
%%%
%%%  Present the trace result with some other method than the default io:format/2:
%%%      ssh_dbg:start(fun(Format,Args) ->
%%%                        my_special( io_lib:format(Format,Args) )
%%%                    end)
%%%

-module(ssh_dbg).

-export([start/0, start/1, 
         stop/0,
         start_server/0,
         start_tracer/0, start_tracer/1,
         on/1,  on/0,
         off/1, off/0,
         go_on/0
	]).

-export([shrink_bin/1,
         reduce_state/1,
         wr_record/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_connect.hrl").
-include("ssh_auth.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%%================================================================

-define(ALL_DBG_TYPES, get_all_dbg_types()).

start() -> start(fun io:format/2).

start(IoFmtFun) when is_function(IoFmtFun,2) ; is_function(IoFmtFun,3) ->
    start_server(),
    catch dbg:start(),
    start_tracer(IoFmtFun),
    dbg:p(all, get_all_trace_flags()),
    ?ALL_DBG_TYPES.

stop() ->
    try
        dbg:stop_clear(),
        gen_server:stop(?SERVER)
    catch
        _:_ -> ok
    end.

start_server() ->
    gen_server:start({local,?SERVER}, ?MODULE, [], []).


start_tracer() -> start_tracer(fun io:format/2).

start_tracer(WriteFun) when is_function(WriteFun,2) ->
    start_tracer(fun(F,A,S) -> WriteFun(F,A), S end);
start_tracer(WriteFun) when is_function(WriteFun,3) ->
    start_tracer(WriteFun, undefined).


start_tracer(WriteFun, InitAcc) when is_function(WriteFun, 3) ->
    Handler = 
        fun(Arg, Acc0) ->
                try_all_types_in_all_modules(gen_server:call(?SERVER, get_on),
                                             Arg, WriteFun,
                                             Acc0)
        end,
    dbg:tracer(process, {Handler,InitAcc}).

%%%----------------------------------------------------------------
on() -> on(?ALL_DBG_TYPES).
on(Type) -> switch(on, Type).


off() -> off(?ALL_DBG_TYPES). % A bit overkill...
off(Type) -> switch(off, Type).
    
go_on() ->
    IsOn = gen_server:call(?SERVER, get_on),
    on(IsOn).

%%%----------------------------------------------------------------
shrink_bin(B) when is_binary(B), size(B)>256 -> {'*** SHRINKED BIN',
						 size(B),
						 element(1,split_binary(B,64)),
						 '...',
						 element(2,split_binary(B,size(B)-64))
						};
shrink_bin(L) when is_list(L) -> lists:map(fun shrink_bin/1, L);
shrink_bin(T) when is_tuple(T) -> list_to_tuple(shrink_bin(tuple_to_list(T)));
shrink_bin(X) -> X.

%%%----------------------------------------------------------------    
%% Replace last element (the state) with "#<state-name>{}"
reduce_state(T) ->
    try
        erlang:setelement(size(T), 
                          T,
                          lists:concat(['#',element(1,element(size(T),T)),'{}'])
                         )
    catch
        _:_ ->
            T
    end.

%%%================================================================
-record(data, {
          types_on = []
         }).

%%%----------------------------------------------------------------
init(_) ->
    {ok, #data{}}.

%%%----------------------------------------------------------------
handle_call({switch,on,Types}, _From, D) ->
    NowOn = lists:usort(Types ++ D#data.types_on),
    call_modules(on, Types, NowOn),
    {reply, {ok,NowOn}, D#data{types_on = NowOn}};

handle_call({switch,off,Types}, _From, D) ->
    StillOn = D#data.types_on -- Types,
    call_modules(off, Types, StillOn),
    call_modules(on, StillOn, StillOn),
    {reply, {ok,StillOn}, D#data{types_on = StillOn}};

handle_call(get_on, _From, D) ->
    {reply, D#data.types_on, D};

handle_call(C, _From, D) ->
    io:format('*** Unknown call: ~p~n',[C]),
    {reply, {error,{unknown_call,C}}, D}.
    

handle_cast(C, D) ->
    io:format('*** Unknown cast: ~p~n',[C]),
    {noreply, D}.
    
handle_info(C, D) ->
    io:format('*** Unknown info: ~p~n',[C]),
    {noreply, D}.


%%%================================================================

%%%----------------------------------------------------------------
ssh_modules_with_trace() ->
    {ok,AllSshModules} = application:get_key(ssh, modules),
    [M || M <- AllSshModules,
          lists:member({dbg_trace,3}, M:module_info(exports))].

%%%----------------------------------------------------------------
get_all_trace_flags() ->
    get_all_trace_flags(ssh_modules_with_trace()).

get_all_trace_flags(Modules) ->
    lists:usort(
      lists:flatten(
        lists:foldl(
          fun(Type, Acc) ->
                  call_modules(flags, Type, undefined, Acc, Modules)
          end, [timestamp], ?ALL_DBG_TYPES))).

%%%----------------------------------------------------------------
get_all_dbg_types() ->
    lists:usort(
      lists:flatten(
        call_modules(points, undefined) )).

%%%----------------------------------------------------------------
call_modules(Cmnd, Type) ->
    call_modules(Cmnd, Type, undefined).

call_modules(Cmnd, Type, Arg) ->
    call_modules(Cmnd, Type, Arg, []).

call_modules(Cmnd, Type, Arg, Acc0) ->
    call_modules(Cmnd, Type, Arg, Acc0, ssh_modules_with_trace()).

call_modules(Cmnd, Types, Arg, Acc0, Modules) when is_list(Types) ->
    lists:foldl(
       fun(Type, Acc) ->
               call_modules(Cmnd, Type, Arg, Acc, Modules)
       end, Acc0, Types);

call_modules(Cmnd, Type, Arg, Acc0, Modules) ->
    lists:foldl(
      fun(Mod, Acc) ->
              try Mod:dbg_trace(Cmnd, Type, Arg)
              of
                  Result -> [Result|Acc]
              catch
                  _:_ -> Acc
              end
      end, Acc0, Modules).

%%%----------------------------------------------------------------
switch(X, Type) when is_atom(Type) ->
    switch(X, [Type]);

switch(X, Types) when is_list(Types) -> 
    case whereis(?SERVER) of
        undefined ->
            start();
        _ ->
            ok
    end,
    case lists:usort(Types) -- ?ALL_DBG_TYPES of
        [] ->
            gen_server:call(?SERVER, {switch,X,Types});
        L ->
            {error, {unknown, L}}
    end.

%%%----------------------------------------------------------------
%%% Format of trace messages are described in reference manual for erlang:trace/4
%%%   {call,MFA}
%%%   {return_from,{M,F,N},Result}
%%%   {send,Msg,To}
%%%   {'receive',Msg}

trace_pid({trace,Pid,_}) -> Pid;
trace_pid({trace,Pid,_,_}) -> Pid;
trace_pid({trace,Pid,_,_,_}) -> Pid;
trace_pid({trace,Pid,_,_,_,_}) -> Pid;
trace_pid({trace,Pid,_,_,_,_,_}) -> Pid;
trace_pid({trace_ts,Pid,_,_TS}) -> Pid;
trace_pid({trace_ts,Pid,_,_,_TS}) -> Pid;
trace_pid({trace_ts,Pid,_,_,_,_TS}) -> Pid;
trace_pid({trace_ts,Pid,_,_,_,_,_TS}) -> Pid;
trace_pid({trace_ts,Pid,_,_,_,_,_,_TS}) -> Pid.

trace_ts({trace_ts,_Pid,_,TS}) -> ts(TS);
trace_ts({trace_ts,_Pid,_,_,TS}) -> ts(TS);
trace_ts({trace_ts,_Pid,_,_,_,TS}) -> ts(TS);
trace_ts({trace_ts,_Pid,_,_,_,_,TS}) -> ts(TS);
trace_ts({trace_ts,_Pid,_,_,_,_,_,TS}) -> ts(TS);
trace_ts(_) -> "-".

trace_info({trace,_Pid,A}) -> A;
trace_info({trace,_Pid,A,B}) -> {A,B};
trace_info({trace,_Pid,A,B,C}) -> {A,B,C};
trace_info({trace,_Pid,A,B,C,D}) -> {A,B,C,D};
trace_info({trace,_Pid,A,B,C,D,E}) -> {A,B,C,D,E};
trace_info({trace_ts,_Pid,A,_TS}) -> A;
trace_info({trace_ts,_Pid,A,B,_TS}) -> {A,B};
trace_info({trace_ts,_Pid,A,B,C,_TS}) -> {A,B,C};
trace_info({trace_ts,_Pid,A,B,C,D,_TS}) -> {A,B,C,D};
trace_info({trace_ts,_Pid,A,B,C,D,E,_TS}) -> {A,B,C,D,E}.


try_all_types_in_all_modules(TypesOn, Arg, WriteFun, Acc0) ->
    SshModules = ssh_modules_with_trace(),
    TS = trace_ts(Arg),
    PID = trace_pid(Arg),
    INFO = trace_info(Arg),
    lists:foldl(
      fun(Type, Acc1) ->
              lists:foldl(
                fun(SshMod,Acc) ->
                        try WriteFun("~n~s ~p ~s~n", 
                                     [lists:flatten(TS), PID, lists:flatten(SshMod:dbg_trace(format,Type,INFO))],
                                     Acc)
                        catch
                            _:_ -> Acc
                        end
                end, Acc1, SshModules)
      end, Acc0, TypesOn).

%%%----------------------------------------------------------------
wr_record(T, Fs, BL) when is_tuple(T) ->
    wr_record(tuple_to_list(T), Fs, BL);
wr_record([_Name|Values], Fields, BlackL) ->
    W = case Fields of
	    [] -> 0;
	    _ -> lists:max([length(atom_to_list(F)) || F<-Fields])
	end,
    [io_lib:format("  ~*p: ~p~n",[W,Tag,Value]) || {Tag,Value} <- lists:zip(Fields,Values),
                                                   not lists:member(Tag,BlackL)
    ].

%%%----------------------------------------------------------------
ts({_,_,Usec}=Now) when is_integer(Usec) ->
    {_Date,{HH,MM,SS}} = calendar:now_to_local_time(Now),
    io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.6.0w",[HH,MM,SS,Usec]);
ts(_) ->
    "-".
