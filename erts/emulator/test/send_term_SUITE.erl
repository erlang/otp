%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(send_term_SUITE).

-export([all/0, suite/0, basic/1]).

-export([generate_external_terms_files/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() -> 
    [basic].

basic(Config) when is_list(Config) ->
    Drv = "send_term_drv",
    P = start_driver(Config, Drv),

    [] = term(P, 0),
    Self = self(),
    {blurf,42,[],[-42,{}|"abc"++P],"kalle",3.1416,Self,#{}} = term(P, 1),

    Map41 = maps:from_list([{blurf, 42},
			    {[], [-42,{}|"abc"++P]},
			    {"kalle", 3.1416},
			    {Self, #{}}]),
    Map41 = term(P, 41),

    Map42 = maps:from_list([{42, []},
			    {[-42,{}|"abc"++P], "kalle"},
			    {3.1416, Self},
			    {#{}, blurf}]),
    Map42 = term(P, 42),
    Deep = lists:seq(0, 199),
    Deep = term(P, 2),
    {B1,B2} = term(P, 3),
    B1 = list_to_binary(lists:seq(0, 255)),
    B2 = list_to_binary(lists:seq(23, 255-17)),

    %% Pid sending. We need another process.
    Child = spawn_link(fun() ->
				     erlang:port_command(P, [4])
			     end),
    {Self,Child} = receive_any(),

    %% ERL_DRV_EXT2TERM
    ExpectExt2Term = expected_ext2term_drv(proplists:get_value(data_dir, Config)),
    ExpectExt2Term = term(P, 5),

    %% ERL_DRV_INT, ERL_DRV_UINT
    case erlang:system_info({wordsize, external}) of
	      4 ->
		  {-1, 4294967295} = term(P, 6);
	      8 ->
		  {-1, 18446744073709551615} = term(P, 6)
	  end,

    %% ERL_DRV_BUF2BINARY
    ExpectedBinTup = {<<>>,
			    <<>>,
			    list_to_binary(lists:duplicate(17,17)),
			    list_to_binary(lists:duplicate(1024,17))},
    ExpectedBinTup = term(P, 7),

    %% single terms
    Singles = [{[], 8}, % ERL_DRV_NIL
               {'', 9}, % ERL_DRV_ATOM
               {an_atom, 10}, % ERL_DRV_ATOM
               {-4711, 11}, % ERL_DRV_INT
               {4711, 12}, % ERL_DRV_UINT
               {P, 13}, % ERL_DRV_PORT
               {<<>>, 14}, % ERL_DRV_BINARY
               {<<"hejsan">>, 15}, % ERL_DRV_BINARY
               {<<>>, 16}, % ERL_DRV_BUF2BINARY
               {<<>>, 17}, % ERL_DRV_BUF2BINARY
               {<<"hoppsan">>, 18}, % ERL_DRV_BUF2BINARY
               {"", 19}, % ERL_DRV_STRING
               {"", 20}, % ERL_DRV_STRING
               {"hippsan", 21}, % ERL_DRV_STRING
               {{}, 22}, % ERL_DRV_TUPLE
               {[], 23}, % ERL_DRV_LIST
               {Self, 24}, % ERL_DRV_PID
               {[], 25}, % ERL_DRV_STRING_CONS
               {[], 27}, % ERL_DRV_EXT2TERM
               {18446744073709551615, 28}, % ERL_DRV_UINT64
               {20233590931456, 29}, % ERL_DRV_UINT64
               {4711, 30}, % ERL_DRV_UINT64
               {0, 31}, % ERL_DRV_UINT64
               {9223372036854775807, 32}, % ERL_DRV_INT64
               {20233590931456, 33}, % ERL_DRV_INT64
               {4711, 34}, % ERL_DRV_INT64
               {0, 35}, % ERL_DRV_INT64
               {-1, 36}, % ERL_DRV_INT64
               {-4711, 37}, % ERL_DRV_INT64
               {-20233590931456, 38}, % ERL_DRV_INT64
               {-9223372036854775808, 39},
	       {#{}, 40}], % ERL_DRV_MAP
    {Terms, Ops} = lists:unzip(Singles),
    Terms = term(P,Ops),

    AFloat = term(P, 26), % ERL_DRV_FLOAT
    true = AFloat < 0.001,
    true = AFloat > -0.001,

    %% Failure cases.
    [] = term(P, 127),
    receive
	      Any ->
		  ct:fail("Unexpected: ~p\n", [Any])
	  after 0 ->
		  ok
	  end,

    ok = chk_temp_alloc(),

    %% In a private heap system, verify that there are no binaries
    %% left for the process.
    erlang:garbage_collect(),		%Get rid of binaries.
    case erlang:system_info(heap_type) of
	private ->
	    {binary,[]} = process_info(self(), binary);
	_ -> ok
    end,

    stop_driver(P, Drv),
    ok.

term(P, Op) ->
    erlang:port_command(P, [Op]),
    receive_any().

receive_any() ->
    receive
	Any -> Any
    end.

chk_temp_alloc() ->
    case erlang:system_info({allocator,temp_alloc}) of
	false ->
	    %% Temp alloc is not enabled
	    ok;
	TIL ->
	    %% Verify that we havn't got anything allocated by temp_alloc
	    lists:foreach(
	      fun ({instance, _, TI}) ->
		      {value, {mbcs, MBCInfo}}
			  = lists:keysearch(mbcs, 1, TI),
		      {value, {blocks, 0, _, _}}
			  = lists:keysearch(blocks, 1, MBCInfo),
		      {value, {sbcs, SBCInfo}}
			  = lists:keysearch(sbcs, 1, TI),
		      {value, {blocks, 0, _, _}}
			  = lists:keysearch(blocks, 1, SBCInfo)
	      end,
	      TIL),
	    ok
    end.
	    

%% Start/stop drivers.
start_driver(Config, Name) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, Name),
    open_port({spawn, Name}, []).

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.

stop_driver(Port, Name) ->
    true = erlang:port_close(Port),
    receive
	{Port,Message} ->
	    ct:fail({strange_message_from_port,Message})
    after 0 ->
	    ok
    end,

    %% Unload the driver.
    ok = erl_ddll:unload_driver(Name),
    ok = erl_ddll:stop().

get_external_terms(DataDir) ->    
    {ok, Bin} = file:read_file([DataDir, "ext_terms.bin"]),
    binary_to_term(Bin).

expected_ext2term_drv(DataDir) ->
    make_expected_ext2term_drv(get_external_terms(DataDir)).

make_expected_ext2term_drv([]) ->
    [];
make_expected_ext2term_drv([T|Ts]) ->
    [{T, T} | make_expected_ext2term_drv(Ts)].

%%
%% Generation of send_term_SUITE_data/ext_terms.h and
%% send_term_SUITE_data/ext_terms.bin
%%
%% These files should normally not need to be regenerated,
%% but we may want that if we introduce new types or make
%% backward incompatible changes to the external format.
%%

generate_external_terms_files(BaseDir) ->
    {ok,Node} = slave:start(hostname(), a_node),
    RPid = rpc:call(Node, erlang, self, []),
    true = is_pid(RPid),
    RRef = rpc:call(Node, erlang, make_ref, []),
    true = is_reference(RRef),
    RPort = hd(rpc:call(Node, erlang, ports, [])),
    true = is_port(RPort),
    slave:stop(Node),
    Terms = [{4711, -4711, [an_atom, "a list"]},
             [1000000000000000000000,-1111111111111111, "blupp!", blipp],
             {RPid, {RRef, RPort}, self(), hd(erlang:ports()), make_ref()},
             {{}, [], [], fun () -> ok end, <<"hej hopp trallalaaaaaaaaaaaaaaa">>},
             [44444444444444444444444,-44444444444, "b!", blippppppp],
             {4711, RPid, {RRef, RPort}, -4711, [an_atom, "a list"]},
             {RPid, {RRef, RPort}, hd(processes()), hd(erlang:ports())},
             {4711, -4711, [an_atom, "a list"]},
             {4711, -4711, [atom, "list"]},
             {RPid, {RRef, RPort}, hd(processes()), hd(erlang:ports())},
             {4444444444444444444,-44444, {{{{{{{{{{{{}}}}}}}}}}}}, make_ref()},
             {444444444444444444444,-44444, [[[[[[[[[[[1]]]]]]]]]]], make_ref()},
             {444444444444444444,-44444, {{{{{{{{{{{{2}}}}}}}}}}}}, make_ref()},
             {4444444444444444444444,-44444, {{{{{{{{{{{{3}}}}}}}}}}}}, make_ref()},
             {44444444444444444444,-44444, {{{{{{{{{{{{4}}}}}}}}}}}}, make_ref()},
             {4444444444444444,-44444, [[[[[[[[[[[5]]]]]]]]]]], make_ref()},
             {444444444444444444444,-44444, {{{{{{{{{{{{6}}}}}}}}}}}}, make_ref()},
             {444444444444444,-44444, {{{{{{{{{{{{7}}}}}}}}}}}}, make_ref()},
             {4444444444444444444,-44444, {{{{{{{{{{{{8}}}}}}}}}}}}, make_ref()},
             #{},
             #{1 => 11, 2 => 22, 3 => 33},
             maps:from_list([{K,K*11} || K <- lists:seq(1,100)])],
    ok = file:write_file(filename:join([BaseDir,
                                        "send_term_SUITE_data",
                                        "ext_terms.bin"]),
                         term_to_binary(Terms, [compressed])),
    {ok, IoDev} = file:open(filename:join([BaseDir,
                                           "send_term_SUITE_data",
                                           "ext_terms.h"]),
                            [write]),
    write_ext_terms_h(IoDev, Terms),
    file:close(IoDev).

write_ext_terms_h(IoDev, Terms) ->
    write_license(IoDev),
    io:format(IoDev, "#ifndef EXT_TERMS_H__~n",[]),
    io:format(IoDev, "#define EXT_TERMS_H__~n",[]),
    {ExtTerms, MaxSize} = make_ext_terms(Terms),
    io:format(IoDev,
              "static struct {~n"
              "  unsigned char ext[~p];~n"
              "  int ext_size;~n"
              "  unsigned char cext[~p];~n"
              "  int cext_size;~n"
              "} ext_terms[] = {~n",[MaxSize, MaxSize]),
    E = write_ext_terms_h(IoDev, ExtTerms, 0),
    io:format(IoDev, "};~n",[]),
    io:format(IoDev, "#define NO_OF_EXT_TERMS ~p~n", [E]),
    io:format(IoDev, "#endif~n",[]).

make_ext_terms([]) ->
    {[], 0};
make_ext_terms([T|Ts]) ->
    E = term_to_binary(T),
    ESz = size(E),
    CE = term_to_binary(T, [compressed]),
    CESz = size(CE),
    true = CESz =< ESz, % Assertion
    {ExtTerms, MaxSize} = make_ext_terms(Ts),
    NewMaxSize = case MaxSize < ESz of
		     true -> ESz;
		     false -> MaxSize
		 end,
    {[{E, ESz, CE, CESz} | ExtTerms], NewMaxSize}.

write_ext_terms_h(IoDev, [], N) ->
    io:format(IoDev, "~n",[]),
    N;
write_ext_terms_h(IoDev, [ET|ETs], 0) ->
    write_ext_term(IoDev, ET),
    write_ext_terms_h(IoDev, ETs, 1);
write_ext_terms_h(IoDev, [ET|ETs], N) ->
    io:format(IoDev, ",~n",[]),
    write_ext_term(IoDev, ET),
    write_ext_terms_h(IoDev, ETs, N+1).

write_ext_term(IoDev, {E, ESz, CE, CESz}) ->
    ESz = write_bytes(IoDev, "  {{", binary_to_list(E), 0),
    io:format(IoDev,
	      ",~n"
	      "   ~p,~n",
	      [ESz]),
    CESz = write_bytes(IoDev, "   {", binary_to_list(CE), 0),
    io:format(IoDev,
	      ",~n"
	      "   ~p}",
	      [CESz]).

write_bytes(IoDev, _, [], N) ->
    io:format(IoDev, "}",[]),
    N;
write_bytes(IoDev, Prefix, [B|Bs], N) ->
    io:format(IoDev, "~s~w", [Prefix, B]),
    write_bytes(IoDev, ",", Bs, N+1).

write_license(IoDev) ->
    S = "/* ``Licensed under the Apache License, Version 2.0 (the \"License\");~n"
        " * you may not use this file except in compliance with the License.~n"
        " * You may obtain a copy of the License at~n"
        " * ~n"
        " *     http://www.apache.org/licenses/LICENSE-2.0~n"
        " * ~n"
        " * Unless required by applicable law or agreed to in writing, software~n"
        " * distributed under the License is distributed on an \"AS IS\" BASIS,~n"
        " * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.~n"
        " * See the License for the specific language governing permissions and~n"
        " * limitations under the License.~n"
	" * ~n"
	" * The Initial Developer of the Original Code is Ericsson AB.~n"
	" * Portions created by Ericsson are Copyright 2007, Ericsson AB.~n"
	" * All Rights Reserved.''~n"
	" * ~n"
	" *     $Id$~n"
	" */~n"
	"~n"
	"/*~n"
	" * Do not modify this file. This file and ext_terms.bin were~n"
	" * automatically generated by send_term_SUITE:generate_external_terms_files/1~n"
	" * and needs to be consistent with each other.~n"
	" */~n",
    io:format(IoDev, S, []).


hostname() ->    
    hostname(atom_to_list(node())).

hostname([$@ | Hostname]) ->
    list_to_atom(Hostname);
hostname([_C | Cs]) ->
    hostname(Cs).
