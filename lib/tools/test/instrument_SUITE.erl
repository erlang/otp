%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(instrument_SUITE).

-export([all/0, suite/0]).
-export(['+Mim true'/1, '+Mis true'/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,10}}].

all() -> 
    ['+Mim true', '+Mis true'].


%% Check that memory data can be read and processed
'+Mim true'(Config) when is_list(Config) ->
    Node = start_slave("+Mim true"),
    MD = rpc:call(Node, instrument, memory_data, []),
    [{total,[{sizes,S1,S2,S3},{blocks,B1,B2,B3}]}]
	= rpc:call(Node, instrument, memory_status, [total]),
    stop_slave(Node),
    true = S1 =< S2,
    true = S2 =< S3,
    true = B1 =< B2,
    true = B2 =< B3,
    MDS = instrument:sort(MD),
    {Low, High} = instrument:mem_limits(MDS),
    true = Low < High,
    {_, AL} = MDS,
    SumBlocks = instrument:sum_blocks(MD),
    case SumBlocks of
        N when is_integer(N) ->
            N = lists:foldl(fun ({_,_,Size,_}, Sum) ->
                                    Size+Sum
                            end, 0, AL),
            true = N =< S3;
        Other ->
            ct:fail(Other)
    end,
    lists:foldl(
      fun ({TDescr,Addr,Size,Proc}, MinAddr) ->
              true = TDescr /= invalid_type,
              true = is_integer(TDescr),
              true = is_integer(Addr),
              true = is_integer(Size),
              true = Addr >= MinAddr,
              case Proc of
                  {0, Number, Serial} ->
                      true = is_integer(Number),
                      true = is_integer(Serial);
                  undefined ->
                      ok;
                  BadProc ->
                      ct:fail({badproc, BadProc})
              end,
              NextMinAddr = Addr+Size,
              true = NextMinAddr =< High,
              NextMinAddr
      end, Low, AL),
    {_, DAL} = instrument:descr(MDS),
    lists:foreach(
      fun ({TDescr,_,_,Proc}) ->
              true = TDescr /= invalid_type,
              true = is_atom(TDescr) orelse is_list(TDescr),
              true = is_pid(Proc) orelse Proc == undefined
      end, DAL),
    ASL = lists:map(fun ({_,A,S,_}) -> {A,S} end, AL),
    ASL = lists:map(fun ({_,A,S,_}) -> {A,S} end, DAL),
    instrument:holes(MDS),
    {comment, "total status - sum of blocks = " ++ integer_to_list(S1-SumBlocks)}.

%% Check that memory data can be read and processed
'+Mis true'(Config) when is_list(Config) ->
    Node = start_slave("+Mis true"),
    [{total,[{sizes,S1,S2,S3},{blocks,B1,B2,B3}]}]
	= rpc:call(Node, instrument, memory_status, [total]),
    true = S1 =< S2,
    true = S2 =< S3,
    true = B1 =< B2,
    true = B2 =< B3,
    true = is_list(rpc:call(Node,instrument,memory_status,[allocators])),
    true = is_list(rpc:call(Node,instrument,memory_status,[classes])),
    true = is_list(rpc:call(Node,instrument,memory_status,[types])),
    ok.

start_slave(Args) ->
    MicroSecs = erlang:monotonic_time(),
    Name = "instr" ++ integer_to_list(MicroSecs),
    Pa = filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(list_to_atom(Name),
                                        slave,
                                        [{args, "-pa " ++ Pa ++ " " ++ Args}]),
    Node.


stop_slave(Node) ->
    true = test_server:stop_node(Node).
