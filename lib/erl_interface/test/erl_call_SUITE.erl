%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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

-export([all/0, smoke/1,
         random_cnode_name/1,
         test_connect_to_host_port/1,
         unresolvable_hostname/1,
         timeout/1,
         test_fetch_stdout/1]).

all() ->
    [smoke,
     random_cnode_name,
     test_connect_to_host_port,
     unresolvable_hostname,
     timeout,
     test_fetch_stdout].

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


random_cnode_name(Config) when is_list(Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),

    try
        CNodeName = start_node_and_get_c_node_name(Name, []),
        [_, Hostname] = string:lexemes(atom_to_list(node()), "@"),
        DefaultName = list_to_atom("c17@" ++ Hostname),
        check_eq(CNodeName, DefaultName),

        CNodeName_r = start_node_and_get_c_node_name(Name, ["-r"]),
        [CNode_r, Hostname] = string:lexemes(atom_to_list(CNodeName_r), "@"),
        check_regex(CNode_r, "^c[0-9]+$"),

        CNodeName_R = start_node_and_get_c_node_name(Name, ["-R"]),
        [CNode_R, Hostname] = string:lexemes(atom_to_list(CNodeName_R), "@"),
        check_regex(CNode_R, "^[0-9A-Z]+$"),

        %% we should get the same recycled node name again
        CNodeName_R2 = start_node_and_get_c_node_name(Name, ["-R"]),
        check_eq(CNodeName_R, CNodeName_R2),

        %% Check that it works with static ports using address
        {ok,Nodes} = erl_epmd:names('localhost'),
        Port = proplists:get_value(nodename(),Nodes),
        MyName = get_node_name_from_self(
                         ["-R","-address","localhost:"++integer_to_list(Port)]),
        check_eq(MyName, node())

    after
        halt_node(Name)
    end,
    ok.

nodename() ->
    hd(string:split(atom_to_list(node()), "@")).

check_eq(X,Y) ->
    {Y,X} = {X,Y}.

check_regex(String, Regex) ->
    {ok, RE} = re:compile(Regex),
    {{match,[{0,_}]}, _} = {re:run(String, RE), String},
    true.

test_connect_to_host_port(Config) when is_list(Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),
    try
        test_connect_to_host_port_do(Name)
    after
        halt_node(Name)
    end,
    ok.


test_connect_to_host_port_do(Name) ->
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
    ok.

test_fetch_stdout(Config) when is_list(Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ "fetch_stdout"
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),
    try
        test_fetch_stdout_do(Name)
    after
        halt_node(Name)
    end,
    ok.


test_fetch_stdout_do(Name) ->
    Port = start_node_and_get_port(Name),
    %% Test that the -fetch_stdout option works
    "hejok" = get_erl_call_result(["-address",
                                   erlang:integer_to_list(Port),
                                   "-a",
                                   "io format [[104,101,106]]",
                                   "-fetch_stdout"]),
    %% Test that the -fetch_stdout option works together with
    %% -no_result_term
    "hej" = get_erl_call_result(["-address",
                                 erlang:integer_to_list(Port),
                                 "-a",
                                 "io format [[104,101,106]]",
                                 "-fetch_stdout",
                                 "-no_result_term"]),
    %% Test that we can print several times
    MultiPrintCodeStr =
        "io:format(\"hej\"),io:format(\"hej\"),io:format(\"hej\").\n",
    "hejhejhej" = get_erl_call_result(["-address",
                                   erlang:integer_to_list(Port),
                                   "-a",
                                   erlang_eval_call_string(MultiPrintCodeStr),
                                   "-fetch_stdout",
                                   "-no_result_term"]),
    %% Test that we can print from a sub-process
    SubProcPrintStr =
        "begin\n"
        "    P = self(),\n"
        "    Printer = fun() ->\n"
        "                      io:format(\"subhej\"),\n"
        "                      receive\n"
        "                          hej -> P ! hej\n"
        "                      end\n"
        "              end,\n"
        "    PrinterPid = spawn(Printer),\n"
        "    PrinterPid ! hej,\n"
        "    receive\n"
        "        hej -> ok\n"
        "    end\n"
        "end.\n",
    "subhej" = get_erl_call_result(["-address",
                                    erlang:integer_to_list(Port),
                                    "-a",
                                    erlang_eval_call_string(SubProcPrintStr),
                                    "-fetch_stdout",
                                    "-no_result_term"]),
    %% Test that the remote group leader supports the multi-requests
    %% request
    TriggerMultiRequestsRequestStr =
        "begin\n"
        "    %% Create multi request\n"
        "    MultiReqCollectGL =\n"
        "        spawn(\n"
        "          fun() ->\n"
        "                  (fun GL(ReqList) ->\n"
        "                           receive\n"
        "                               {io_request, From, ReplyAs, Req} ->\n"
        "                                   From ! {io_reply, ReplyAs, ok},\n"
        "                                   GL([Req | ReqList]);\n"
        "                               {get_reqs, Pid} ->\n"
        "                                   Pid ! {multi_req,\n"
        "                                          {requests, lists:reverse(ReqList)}}\n"
        "                           end\n"
        "                   end)([])\n"
        "          end),\n"
        "    OldGL = erlang:group_leader(),\n"
        "    erlang:group_leader(MultiReqCollectGL, self()),\n"
        "    io:format(\"test1\"),\n"
        "    io:format(\"test2\"),\n"
        "    io:format(\"test3\"),\n"
        "    MultiReqCollectGL ! {get_reqs, self()},\n"
        "    MultiReqsRequest =\n"
        "        receive\n"
        "            {multi_req, R} -> R\n"
        "        end,\n"
        "    erlang:group_leader(OldGL, self()),\n"
        "    %% Send multi request\n"
        "    erlang:group_leader() ! {io_request,self(), self(), MultiReqsRequest},\n"
        "    Me = self(),\n"
        "    receive {io_reply, Me, ok} -> ok end,\n"
        "    %% Send normal request\n"
        "    io:format(\"test4\")\n"
        "end.\n",
    "test1test2test3test4" =
        get_erl_call_result(["-address",
                             erlang:integer_to_list(Port),
                             "-a",
                             erlang_eval_call_string(TriggerMultiRequestsRequestStr),
                             "-fetch_stdout",
                             "-no_result_term"]),
    ok.

%% OTP-16604: Tests that erl_call works even when the local hostname cannot be
%% resolved.
unresolvable_hostname(_Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),
    Opt = "-__uh_test__",

    try
        CNodeName = start_node_and_get_c_node_name(Name, [Opt]),
        [_, Hostname] = string:lexemes(atom_to_list(node()), "@"),
        DefaultName = list_to_atom("c17@" ++ Hostname),
        check_eq(CNodeName, DefaultName)
    after
        halt_node(Name)
    end,

    ok.

%% OTP-16604: Test the -timeout option
timeout(_Config) ->
    Name = atom_to_list(?MODULE)
        ++ "-"
        ++ integer_to_list(erlang:system_time(microsecond)),
    Opts = ["-timeout", "3"],

    try
        [] = start_node_and_apply(Name, "timer sleep [10000]", Opts)
    after
        halt_node(Name)
    end,

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
    string:trim(start_node_and_apply(Name, "erlang node", []),
                both,
                "'").

start_node_and_get_c_node_name(Name, Opts) ->
    Str = start_node_and_apply(Name, "erlang nodes [hidden]", Opts),
    {ok, [{'[',_}, {atom, _, CNode}, {']',_}], _} = erl_scan:string(Str),
    CNode.

start_node_and_apply(Name, MfaStr, Opts) ->
    NameSwitch = case net_kernel:longnames() of
                     true ->
                         "-name";
                     false ->
                         "-sname"
                 end,
    get_erl_call_result(Opts ++
                            ["-s",
                             NameSwitch,
                             Name, "-a",
                             MfaStr]).

start_node_and_get_port(Name) ->
    start_node_and_get_node_name(Name),
    {ok, NamePortList} = net_adm:names(),
    {value, {_, Port}}
        = lists:search(fun({N, _}) ->
                               string:equal(N, Name)
                       end,
                       NamePortList),
    Port.

get_node_name_from_self(Opts) ->
    Str = apply_on_self("erlang node []", Opts),
    {ok, [{atom, _, Node}], _} = erl_scan:string(Str),
    Node.

apply_on_self(MfaStr, Opts) ->
    get_erl_call_result(Opts ++ ["-a",MfaStr]).

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

erlang_eval_call_string(CodeStr) ->
    lists:flatten(io_lib:format("erl_eval eval_str [~w]",[CodeStr])).
