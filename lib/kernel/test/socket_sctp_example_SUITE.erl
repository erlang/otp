%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(socket_sctp_example_SUITE).

-export([
         suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
         init_per_group/2,    end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2, 

         simple_ipv4/1,
         simple_ipv6/1
        ]).


-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("kernel_test_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LOGGER,     socket_test_logger).
-define(MESSAGE_1,
        "The quick brown fox jumps over a lazy dog").
-define(MESSAGE_2,
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog").
-define(MESSAGE_3,
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog"
        "The quick brown fox jumps over a lazy dog").

-define(MESSAGES, [{?MESSAGE_1, 3000},
                   ?MESSAGE_2,
                   {?MESSAGE_3, 2000}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%======================================================================
%% Common Test interface functions
%%
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,2}}].

all() -> 
    [
     {group, std}
    ].

groups() -> 
    [
     {std,   [], std_cases()}
    ].

std_cases() ->
    [
     simple_ipv4,
     simple_ipv6
    ].


%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->

    ?P("~s -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [?FUNCTION_NAME, Config0, erlang:nodes()]),

    try socket:info() of
        #{} ->
            has_support_sctp(),
            case ?LIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;
    
                Config1 when is_list(Config1) ->
    
                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),
    
                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),
    
                    socket:use_registry(false),
                    case quiet_mode(Config1) of
                        default ->
                            case ?LOGGER:start() of
                                ok ->
                                    Config1;
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end;
                        Quiet ->
                            case ?LOGGER:start(Quiet) of
                                ok ->
                                    [{esock_test_quiet, Quiet} | Config1];
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end
                    end
            end
    catch
        error : notsup ->
            {skip, "ESock Not Supported"};
        error : undef ->
            {skip, "ESock Not Configured"}
    end.

end_per_suite(Config0) when is_list(Config0) ->

    ?P("~s -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [?FUNCTION_NAME, Config0, erlang:nodes()]),

    %% Stop the local monitor
    kernel_test_sys_monitor:stop(),

    (catch ?LOGGER:stop()),

    Config1 = ?LIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
       "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(GroupName, Config) ->
    ?P("~s(~w) -> entry with"
       "~n      Config: ~p", [?FUNCTION_NAME, GroupName, Config]),

    WhereAreWe = filename:dirname(code:which(?MODULE)),
    LibDir  = code:lib_dir(kernel),
    ExDir   = filename:join([LibDir, "examples", "socket_sctp"]),
    SrcDir  = filename:join([ExDir, "src"]),
    ok      = filelib:ensure_dir(SrcDir),

    ?P("~s(~w) -> find files when"
       "~n   WhereAreWe: ~p"
       "~n   LibDir:  ~p"
       "~n   ExDir:   ~p"
       "~n   SrcDir:  ~p", [?FUNCTION_NAME, GroupName,
                            WhereAreWe,
                            LibDir, ExDir, SrcDir]),
    
    Files   = find_files(SrcDir, ".*\\.erl$"),

    ?P("~s(~w) -> compile files:"
       "~n   ~p", [?FUNCTION_NAME, GroupName, Files]),
    ok      = compile_files(Files, SrcDir, WhereAreWe),

    ?P("~s(~w) -> done", [?FUNCTION_NAME, GroupName]),
    [{dir, WhereAreWe} | Config].

end_per_group(GroupName, Config) ->
    ?P("~s(~w) -> entry with"
       "~n      Config: ~p", [?FUNCTION_NAME, GroupName, Config]),
    Config.


%%
%% -----
%%

init_per_testcase(Case, Config) ->

    ?P("~s(~w) -> entry with"
      "~n   Config: ~p", [?FUNCTION_NAME, Case, Config]),

    Config.    

end_per_testcase(Case, Config) ->

    ?P("~s(~w) -> entry with"
      "~n   Config: ~p", [?FUNCTION_NAME, Case, Config]),

    Config.


quiet_mode(Config) ->
    case lists:keysearch(esock_test_quiet, 1, Config) of
        {value, {esock_test_quiet, Quiet}} ->
            Quiet;
        false ->
            case os:getenv("ESOCK_TEST_QUIET") of
                "true"  -> true;
                "false" -> false;
                _       -> default
            end
    end.


%% ------------------ simple ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_ipv4(Config) when is_list(Config) ->
    ?P("~s -> entry with"
       "~n   Config: ~p", [?FUNCTION_NAME, Config]),
    HasDomainSupport = fun() -> has_support_ipv4() end,
    simple(inet, HasDomainSupport, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_ipv6(Config) when is_list(Config) ->
    ?P("~s -> entry with"
       "~n   Config: ~p", [?FUNCTION_NAME, Config]),
    HasDomainSupport = fun() -> has_support_ipv6() end,
    simple(inet6, HasDomainSupport, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple(Domain, HasDomainSupport, Config)
  when ((Domain =:= inet) orelse (Domain =:= inet6)) andalso
       is_function(HasDomainSupport) ->
    ?P("~s -> entry with"
       "~n   Domain: ~p", [?FUNCTION_NAME, Domain]),
    Cond = fun() ->
                   ?P("~s:cond -> entry", [?FUNCTION_NAME]),
                   HasDomainSupport()
           end,
    Pre  = fun() ->
                   ?P("~s:pre -> initial config", [?FUNCTION_NAME]),
                   Dir    = ?config(dir,    Config),
                   PaArgs = "-pa " ++ Dir,
                   ServerNodeName  = ?UNIQ_NODE_NAME("server"),
                   ClientNodeName1 = ?UNIQ_NODE_NAME("client1"),
                   ClientNodeName2 = ?UNIQ_NODE_NAME("client2"),
                   ClientNodeName3 = ?UNIQ_NODE_NAME("client3"),
                   NodeNames       = [ServerNodeName,
                                      ClientNodeName1,
                                      ClientNodeName2,
                                      ClientNodeName3],
                   [ServerNode | ClientNodes] = start_nodes(NodeNames, PaArgs),
                   ?P("~s:pre -> Nodes started", [?FUNCTION_NAME]),
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{server_node  => ServerNode,
                             client_nodes => ClientNodes,
                             domain       => Domain,
                             addr         => Addr};
                       {error, Reason} ->
                           ?P("~s:pre -> Failed get local address:"
                              "~n      Reason: ~p", [Reason]),
                           stop_nodes([ServerNode | ClientNodes]),
                           skip({local_address, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ?P("~s:tc -> begin", [?FUNCTION_NAME]),
                   do_simple(State)
           end,
    Post = fun(#{server_node  := ServerNode,
                 client_nodes := ClientNodes}) ->
                   ?P("~s:post -> try stop nodes", [?FUNCTION_NAME]),
                   stop_nodes([ServerNode | ClientNodes]),
                   ok;
              (X) ->
                   ?P("~s:post -> invalid config: "
                      "~n      ~p", [?FUNCTION_NAME, X]),
                   ok
           end,
    ?TC_TRY(?FUNCTION_NAME,
            Cond, Pre, TC, Post).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_simple(#{domain       := Domain,
            server_node  := ServerNode,
            client_nodes := ClientNodes}) ->

    ?P("~s -> entry with"
       "~n   Domain:       ~p"
       "~n   Server Node:  ~p"
       "~n   Client Nodes: ~p",
       [?FUNCTION_NAME, Domain, ServerNode, ClientNodes]),

    ?P("~s -> try start server on"
       "~n   Node: ~p",
       [?FUNCTION_NAME, ServerNode]),
    {ok, {Server, ServerMRef, ServerSA}} =
        socket_sctp_server:start_monitor(ServerNode,
                                         #{domain => Domain,
                                           debug  => true}),

    ?P("~s -> server started: "
       "~n   Server:      ~p"
       "~n   Server MRef: ~p"
       "~n   Server SA:   ~p", [?FUNCTION_NAME, Server, ServerMRef, ServerSA]),

    Clients =
        case do_simple_start_clients(ServerSA, ClientNodes) of
            {ok, Cs} ->
                Cs;
            {error, Reason} ->
                socket_sctp_server:stop(Server),
                ct:fail({fail_starting_clients, Reason})
        end,

    ?P("~s -> try stop clients", [?FUNCTION_NAME]),
    do_simple_stop_clients(Clients),

    ?P("~s -> try stop server", [?FUNCTION_NAME]),
    socket_sctp_server:stop(Server),

    ?P("~s -> done", [?FUNCTION_NAME]),
    ok.


do_simple_start_clients(Domain, ClientNodes) ->
    do_simple_start_clients(Domain, ClientNodes, 1, []).

do_simple_start_clients(_ServerSA, [], _NextID, RevClients) ->
    {ok, lists:reverse(RevClients)};
do_simple_start_clients(ServerSA, [Node|Nodes], ID, RevClients) ->
    ?P("~s -> try start client ~w on ~p", [?FUNCTION_NAME, ID, Node]),
    case socket_sctp_client:start_monitor(Node,
                                          ID,
                                          ServerSA,
                                          ?MESSAGES,
                                          #{debug => false}) of
        {ok, Client} ->
            ?P("~s -> client ~w started"
               "~n   ~p", [?FUNCTION_NAME, ID, Client]),
            do_simple_start_clients(ServerSA, Nodes, ID+1,
                                    [{ID, Client}|RevClients]);
        {error, Reason} = ERROR ->
            ?P("~s -> Failed start client ~w:"
               "~n   Node:   ~p"
               "~n   Reason: ~p", [?FUNCTION_NAME, ID, Node, Reason]),
            do_simple_stop_clients(RevClients),
            ERROR
    end.


do_simple_stop_clients(Clients) ->
    lists:foreach(fun({ID, {Client, _MRef, _SA}}) ->
                          ?P("~s -> try stop client ~w: ~p",
                             [?FUNCTION_NAME, ID, Client]),
                          socket_sctp_client:stop(Client)
                  end,
                  Clients).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_files(Dir, RE) ->
    filelib:fold_files(Dir, RE, false,
                       fun(P, A) -> [P|A] end,
                       []).

compile_files([], _IncDir, _OutDir) ->
    ok;
compile_files([File|Files], IncDir, OutDir) ->
    Opts = [warnings_as_errors,
            return_errors,
            {i,      IncDir},
            {outdir, OutDir}],
    case compile:file(File, Opts) of
        {ok, _Module} -> 
            compile_files(Files, IncDir, OutDir);
        error ->
            ?P("<ERROR> Failed compiling: "
                   "~n   File: ~p", [File]),
            exit({compile, File});
        {error, Errors, Warnings} ->
            ?P("<ERROR> Failed compiling: "
                   "~n   File:     ~p"
                   "~n   Errors:   ~p"
                   "~n   Warnings: ~p", [File, Errors, Warnings]),
            exit({compile, File})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test case condition functions.

has_support_sctp() ->
    case os:type() of
        {win32, _} ->
            skip("SCTP Not Supported");
        {unix, netbsd} ->
            %% XXX We will have to investigate this later...
            skip("SCTP Not Supported");
        _ ->
            case socket:is_supported(sctp) of
                true ->
                    ok;
                false ->
                    skip("SCTP Not Supported")
            end
    end.


%% The idea is that this function shall test if the test host has 
%% support for IPv4 or IPv6. If not, there is no point in running
%% corresponding tests.
%% Currently we just skip.
has_support_ipv4() ->
    ?LIB:has_support_ipv4().

has_support_ipv6() ->
    ?LIB:has_support_ipv6().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_nodes(Nodes, Args) ->
    start_nodes(Nodes, [], Args).

start_nodes([], RevNodes, _Args) ->
    lists:reverse(RevNodes);
start_nodes([NodeName|NodeNames], RevNodes, Args) ->
    ?P("~s -> try start node:"
       "~n      ~p", [?FUNCTION_NAME, NodeName]),
    case ?START_NODE(NodeName, Args) of
        {ok, Node} ->
            ?P("~s -> node started:"
               "~n      ~p", [?FUNCTION_NAME, Node]),
            start_nodes(NodeNames, [Node|RevNodes], Args);
        {error, Reason} ->
            ?P("<ERROR> Failed starting node:"
               "~n   Name:   ~p"
               "~n   Reason: ~p", [NodeName, Reason]),
            stop_nodes(RevNodes),
            skip({failed_start_node, NodeName, Reason})
    end.

stop_nodes(Nodes) ->
    lists:foreach(fun(N) ->
                          ?P("~s -> try stop node ~p", [?FUNCTION_NAME, N]),
                          ?STOP_NODE(N)
                  end, Nodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip(Reason) ->
    throw({skip, Reason}).


