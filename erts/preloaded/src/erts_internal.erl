%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
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
%% As the module name imply, this module is here for ERTS internal
%% functionality. As an application programmer you should *never*
%% call anything in this module directly. Functions exported by
%% this module may change behaviour or be removed at any time
%% without any notice whatsoever. Everything in this module is
%% intentionally left undocumented, and should remain so.
%%

-module(erts_internal).

-export([await_port_send_result/3]).
-export([cmp_term/2]).
-export([map_to_tuple_keys/1]).
-export([port_command/3, port_connect/2, port_close/1,
	 port_control/3, port_call/3, port_info/1, port_info/2]).

-export([request_system_task/3]).

-export([check_process_code/2]).

%%
%% Await result of send to port
%%

await_port_send_result(Ref, Busy, Ok) ->
    receive
	{Ref, false} -> Busy;
	{Ref, _} -> Ok
    end.

%%
%% Statically linked port NIFs
%%

-spec erts_internal:port_command(Port, Data, OptionList) -> Result when
      Port :: port() | atom(),
      Data :: iodata(),
      OptionList :: [Option],
      Option :: force | nosuspend,
      Result :: boolean() | reference() | badarg | notsup.
port_command(_Port, _Data, _OptionList) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_connect(Port, Pid) -> Result when
      Port :: port() | atom(),
      Pid :: pid(),
      Result :: true | reference() | badarg.
port_connect(_Port, _Pid) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_close(Port) -> Result when
      Port :: port() | atom(),
      Result :: true | reference() | badarg.
port_close(_Port) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_control(Port, Operation, Data) -> Result when
      Port :: port() | atom(),
      Operation :: integer(),
      Data :: iodata(),
      Result :: string() | binary() | reference() | badarg.
port_control(_Port, _Operation, _Data) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_call(Port, Operation, Data) -> Result when
      Port :: port() | atom(),
      Operation :: integer(),
      Data :: term(),
      Result :: {ok, term()} | reference() | badarg.
port_call(_Port, _Operation, _Data) ->
    erlang:nif_error(undefined).

-type port_info_1_result_item() ::
      {registered_name, RegName :: atom()} |
      {id, Index :: non_neg_integer()} |
      {connected, Pid :: pid()} |
      {links, Pids :: [pid()]} |
      {name, String :: string()} |
      {input, Bytes :: non_neg_integer()} |
      {output, Bytes :: non_neg_integer()} |
      {os_pid, OsPid :: non_neg_integer() | 'undefined'}.

-spec erts_internal:port_info(Port) -> Result when
      Port :: port() | atom(),
      Result :: [port_info_1_result_item()] | undefined | reference() | badarg | [].
port_info(_Result) ->
    erlang:nif_error(undefined).

-type port_info_2_item() ::
      registered_name |
      id |
      connected |
      links |
      name |
      input |
      output |
      os_pid |
      monitors |
      memory |
      parallelism |
      queue_size |
      locking.

-type port_info_2_result_item() ::
      {registered_name, RegName :: atom()} |
      [] | % No registered name
      {id, Index :: non_neg_integer()} |
      {connected, Pid :: pid()} |
      {links, Pids :: [pid()]} |
      {name, String :: string()} |
      {input, Bytes :: non_neg_integer()} |
      {output, Bytes :: non_neg_integer()} |
      {os_pid, OsPid :: non_neg_integer() | 'undefined'} |
      {monitors, Monitors :: [{process, pid()}]} |
      {memory, MemSz :: non_neg_integer()} |
      {parallelism, Boolean :: boolean()} |
      {queue_size, QSz :: non_neg_integer()} |
      {locking, Locking :: 'false' | 'port_level' | 'driver_level'}.

-spec erts_internal:port_info(Port, Item) -> Result when
      Port :: port() | atom(),
      Item :: port_info_2_item(),
      Result :: port_info_2_result_item() | undefined | reference() | badarg.

port_info(_Result, _Item) ->
    erlang:nif_error(undefined).

-spec request_system_task(Pid, Prio, Request) -> 'ok' when
      Prio :: 'max' | 'high' | 'normal' | 'low',
      Request :: {'garbage_collect', term()}
	       | {'check_process_code', term(), module(), boolean()},
      Pid :: pid().

request_system_task(_Pid, _Prio, _Request) ->
    erlang:nif_error(undefined).

-spec check_process_code(Module, OptionList) -> boolean() when
      Module :: module(),
      Option :: {allow_gc, boolean()},
      OptionList :: [Option].
check_process_code(_Module, _OptionList) ->
    erlang:nif_error(undefined).

%% term compare where integer() < float() = true

-spec cmp_term(A,B) -> Result when
    A :: term(),
    B :: term(),
    Result :: -1 | 0 | 1.

cmp_term(_A,_B) ->
    erlang:nif_error(undefined).

%% return the internal key tuple for map keys
-spec map_to_tuple_keys(M) -> Keys when
    M :: map(),
    Keys :: tuple().

map_to_tuple_keys(_M) ->
    erlang:nif_error(undefined).
