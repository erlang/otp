%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019. All Rights Reserved.
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
-module(erl_call_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, smoke/1, test_connect_to_host_port/1]).

all() ->
    [smoke,
     test_connect_to_host_port].

smoke(Config) when is_list(Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),

    RetNodeName = start_node_and_get_node_name(Name),

    halt_node(Name),

    [_, Hostname] = string:lexemes(atom_to_list(node()), "@"),
    NodeName = list_to_atom(Name ++ "@" ++ Hostname),
    NodeName = list_to_atom(RetNodeName),
    ok.


test_connect_to_host_port(Config) when is_list(Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),
    Port = start_node_and_get_port(Name),
    AddressCaller =
        fun(Address) ->
                  get_erl_call_result(["-address",
                                       Address,
                                       "-a",
                                       "erlang length [[1,2,3,4,5,6,7,8,9]]"])
        end,
    "9" = AddressCaller(erlang:integer_to_list(Port)),
    "9" = AddressCaller(":" ++ erlang:integer_to_list(Port)),
    [_, Hostname] = string:lexemes(atom_to_list(node()), "@"),
    "9" = AddressCaller(Hostname ++ ":" ++ erlang:integer_to_list(Port)),
    FailedRes = AddressCaller("80"),
    case string:find(FailedRes, "80") of
        nomatch -> ct:fail("Incorrect error message");
        _ -> ok
    end,
    halt_node(Name),
    ok.

%
% Utility functions...
%


halt_node(Name) ->
    [_, Hostname] = string:lexemes(atom_to_list(node()), "@"),
    NodeName = list_to_atom(Name ++ "@" ++ Hostname),
    io:format("NodeName: ~p~n~n", [NodeName]),

    pong = net_adm:ping(NodeName),
    rpc:cast(NodeName, erlang, halt, []).

start_node_and_get_node_name(Name) ->
    NameSwitch = case net_kernel:longnames() of
                     true ->
                         "-name";
                     false ->
                         "-sname"
                 end,
    string:trim(get_erl_call_result(["-s",
                                     NameSwitch,
                                     Name, "-a",
                                     "erlang node"]),
                both,
                "'").

start_node_and_get_port(Name) ->
    start_node_and_get_node_name(Name),
    {ok, NamePortList} = net_adm:names(),
    {value, {_, Port}}
        = lists:search(fun({N, _}) ->
                               string:equal(N, Name)
                       end,
                       NamePortList),
    Port.

find_erl_call() ->
    ErlCallName = case os:type() of
                      {win32, _} -> "erl_call.exe";
                      _ -> "erl_call"
                  end,
    LibDir = code:lib_dir(erl_interface),
    InstalledErlCall = filename:join([LibDir, "bin", ErlCallName]),
    TargetDir = erlang:system_info(system_architecture),
    TargetErlCall = filename:join([LibDir, "bin", TargetDir, ErlCallName]),
    
    try
        lists:foreach(fun (F) ->
                              io:format("Checking: \"~ts\"~n", [F]),
                              case file:read_file_info(F) of
                                  {ok, _} ->
                                      throw(F);
                                  _ ->
                                      ok
                              end
                      end,
                      [InstalledErlCall, TargetErlCall]),
        exit({missing, erl_call})
    catch
        throw:ErlCall ->
            ErlCall
    end.


get_erl_call_result(ArgsList) ->
    ErlCall = find_erl_call(),
    io:format("erl_call: \"~ts\"\n~nargs list: ~p~n", [ErlCall, ArgsList]),
    CmdRes = get_port_res(open_port({spawn_executable, ErlCall},
                                    [{args, ArgsList}, eof, stderr_to_stdout]), []),
    io:format("CmdRes: ~p~n", [CmdRes]),
    CmdRes.

get_port_res(Port, Acc) when is_port(Port) ->
    receive
        {Port, {data, Data}} ->
            get_port_res(Port, [Acc|Data]);
        {Port, eof} ->
            lists:flatten(Acc)
    end.
            
