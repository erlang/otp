%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
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

-module(send_term_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,basic/1]).
-export([init_per_testcase/2,end_per_testcase/2]).

-export([generate_external_terms_files/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


basic(Config) when is_list(Config) ->
    Drv = "send_term_drv",
    ?line P = start_driver(Config, Drv),

    ?line [] = term(P, 0),
    ?line Self = self(),
    ?line {blurf,42,[],[-42,{}|"abc"++P],"kalle",3.1416,Self} = term(P, 1),
    ?line Deep = lists:seq(0, 199),
    ?line Deep = term(P, 2),
    ?line {B1,B2} = term(P, 3),
    ?line B1 = list_to_binary(lists:seq(0, 255)),
    ?line B2 = list_to_binary(lists:seq(23, 255-17)),

    %% Pid sending. We need another process.
    ?line Child = spawn_link(fun() ->
				     erlang:port_command(P, [4])
			     end),
    ?line {Self,Child} = receive_any(),

    %% ERL_DRV_EXT2TERM
    ?line ExpectExt2Term = expected_ext2term_drv(?config(data_dir, Config)),
    ?line ExpectExt2Term = term(P, 5),

    %% ERL_DRV_INT, ERL_DRV_UINT
    ?line case erlang:system_info({wordsize, external}) of
	      4 ->
		  ?line {-1, 4294967295} = term(P, 6);
	      8 ->
		  ?line {-1, 18446744073709551615} = term(P, 6)
	  end,

    %% ERL_DRV_BUF2BINARY
    ?line ExpectedBinTup = {<<>>,
			    <<>>,
			    list_to_binary(lists:duplicate(17,17)),
			    list_to_binary(lists:duplicate(1024,17))},
    ?line ExpectedBinTup = term(P, 7),

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
               {-9223372036854775808, 39}], % ERL_DRV_INT64
    ?line {Terms, Ops} = lists:unzip(Singles),
    ?line Terms = term(P,Ops),

    AFloat = term(P, 26), % ERL_DRV_FLOAT
    ?line true = AFloat < 0.001,
    ?line true = AFloat > -0.001,

    %% Failure cases.
    ?line [] = term(P, 127),
    ?line receive
	      Any ->
		  ?line io:format("Unexpected: ~p\n", [Any]),
		  ?line ?t:fail()
	  after 0 ->
		  ok
	  end,

    ?line ok = chk_temp_alloc(),

    %% In a private heap system, verify that there are no binaries
    %% left for the process.
    ?line erlang:garbage_collect(),		%Get rid of binaries.
    case erlang:system_info(heap_type) of
	private ->
	    ?line {binary,[]} = process_info(self(), binary);
	_ -> ok
    end,

    ?line stop_driver(P, Drv),
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
	    ?line ok;
	TIL ->
	    %% Verify that we havn't got anything allocated by temp_alloc
	    lists:foreach(
	      fun ({instance, _, TI}) ->
		      ?line {value, {mbcs, MBCInfo}}
			  = lists:keysearch(mbcs, 1, TI),
		      ?line {value, {blocks, 0, _, _}}
			  = lists:keysearch(blocks, 1, MBCInfo),
		      ?line {value, {sbcs, SBCInfo}}
			  = lists:keysearch(sbcs, 1, TI),
		      ?line {value, {blocks, 0, _, _}}
			  = lists:keysearch(blocks, 1, SBCInfo)
	      end,
	      TIL),
	    ?line ok
    end.
	    

%% Start/stop drivers.
start_driver(Config, Name) ->
    Path = ?config(data_dir, Config),
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
    ?line true = erlang:port_close(Port),
    receive
	{Port,Message} ->
	    ?t:fail({strange_message_from_port,Message})
    after 0 ->
	    ok
    end,

    %% Unload the driver.
    ok = erl_ddll:unload_driver(Name),
    ?line ok = erl_ddll:stop().

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
    Terms =
	[{4711, -4711, [an_atom, "a list"]},
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
	 {4444444444444444444,-44444, {{{{{{{{{{{{8}}}}}}}}}}}}, make_ref()}],
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
    S =	"/* ``The contents of this file are subject to the Erlang Public License,~n"
	" * Version 1.1, (the \"License\"); you may not use this file except in~n"
	" * compliance with the License. You should have received a copy of the~n"
	" * Erlang Public License along with this software. If not, it can be~n"
	" * retrieved via the world wide web at http://www.erlang.org/.~n"
	" * ~n"
	" * Software distributed under the License is distributed on an \"AS IS\"~n"
	" * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See~n"
	" * the License for the specific language governing rights and limitations~n"
	" * under the License.~n"
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
