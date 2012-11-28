%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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
-module(cover_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2,
	 suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([start/1, compile/1, analyse/1, misc/1, stop/1, 
	 distribution/1, reconnect/1, die_and_reconnect/1,
	 dont_reconnect_after_stop/1, stop_node_after_disconnect/1,
	 export_import/1,
	 otp_5031/1, eif/1, otp_5305/1, otp_5418/1, otp_6115/1, otp_7095/1,
         otp_8188/1, otp_8270/1, otp_8273/1, otp_8340/1]).

-include_lib("test_server/include/test_server.hrl").

%%----------------------------------------------------------------------
%% The following directory structure is assumed:
%%  cwd __________________________________________
%%  |  \   \   \   \   \     \                    \
%%  a   b   cc   d   f  d1   compile_beam_____  otp_6115
%%                      |      \    \  \  \   \    \  \
%%                      e      crypt v  w  x   d   f1  f2
%%                                             |
%%                                             y
%%----------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case whereis(cover_server) of
	undefined ->
	    [start, compile, analyse, misc, stop,
	     distribution, reconnect, die_and_reconnect,
	     dont_reconnect_after_stop, stop_node_after_disconnect,
	     export_import, otp_5031, eif, otp_5305, otp_5418,
	     otp_6115, otp_7095, otp_8188, otp_8270, otp_8273,
	     otp_8340];
	_pid ->
	    {skip,
	     "It looks like the test server is running "
	     "cover. Can't run cover test."}
    end.

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

init_per_testcase(TC, Config) when TC =:= misc; 
				   TC =:= compile; 
				   TC =:= analyse;
				   TC =:= distribution;
				   TC =:= otp_5031;
				   TC =:= stop ->
    case code:which(crypto) of
	Path when is_list(Path) ->
	    init_per_testcase(dummy_tc, Config);
	_Else ->
	    {skip, "No crypto file to test with"}
    end;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(TestCase, _Config) ->
    case lists:member(TestCase,[start,compile,analyse,misc]) of
	true -> ok;
	false -> cover:stop()
    end,
    ok.

start(suite) -> [];
start(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(data_dir, Config)),

    ?line Files = lsfiles(),
    ?line remove(files(Files, ".out")),

    ?line {ok, Pid} = cover:start(),
    ?line {error, {already_started, Pid}} = cover:start().

compile(suite) -> [];
compile(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(data_dir, Config)),

    ?line Result1 = cover:compile_directory(),
    ?line SortedResult = lists:sort(Result1),
    ?line {ok, CWD} = file:get_cwd(),
    ?line Result2 = cover:compile_directory(CWD),
    ?line SortedResult = lists:sort(Result2),
    ?line [{error,_DFile},{ok,a},{ok,b},{ok,cc},{ok,f}] = SortedResult,
    ?line [{ok,e}] = cover:compile_directory("d1"),
    ?line {error,enoent} = cover:compile_directory("d2"),

    ?line {ok,a} = cover:compile(a),
    ?line {ok,b} = compile:file(b),
    ?line code:purge(b),
    ?line {module,b} = code:load_file(b),
    ?line {ok,d} = cover:compile("d.erl", [{d,'AGE',42}]),
    ?line {error,_BBFile} = cover:compile(bb),

    ?line StdlibDir = code:lib_dir(stdlib),
    ?line Lists = filename:join([StdlibDir, "src", "lists.erl"]),
    ?line {error, Lists} = cover:compile(Lists),

    %% For compiling beam: using dummy files v,w,x,y and z
    ?line file:set_cwd("compile_beam"),
    ?line {ok,_} = compile:file(v,[debug_info,report]),
    ?line {ok,_} = compile:file(w,[debug_info,report]),
    ?line {ok,_} = compile:file(x),
    ?line {ok,_} = compile:file("d/y",[debug_info,{outdir,"d"},report]),
    ?line Key = "A Krypto Key",
    CryptoWorks = crypto_works(),
    case CryptoWorks of
	false ->
	    {ok,_} = compile:file(crypt, [debug_info,report]),
	    {ok,crypt} = cover:compile_beam("crypt.beam");
	true ->
	    {ok,_} = compile:file(crypt, [{debug_info_key,Key},report]),
	    {error,{encrypted_abstract_code,_}} =
		cover:compile_beam("crypt.beam"),
	    ok = beam_lib:crypto_key_fun(simple_crypto_fun(Key)),
	    {ok,crypt} = cover:compile_beam("crypt.beam")
    end,
    Path = filename:join([?config(data_dir, Config), "compile_beam", "v.erl"]),
    ?line {ok,v} = cover:compile_beam(v),
    {source,Path} = lists:keyfind(source, 1, v:module_info(compile)),
    ?line {ok,w} = cover:compile_beam("w.beam"),
    ?line {error,{no_abstract_code,"./x.beam"}} = cover:compile_beam(x),
    ?line {error,{already_cover_compiled,no_beam_found,a}}=cover:compile_beam(a),
    ?line {error,non_existing} = cover:compile_beam(z),
    ?line [{ok,y}] = cover:compile_beam_directory("d"),
    ?line Result3 = lists:sort(cover:compile_beam_directory()),
    ?line [{error,{no_abstract_code,_XBeam}},{ok,crypt},{ok,v},{ok,w}] = Result3,
    ?line {error,enoent} = cover:compile_beam_directory("d2"),
    ?line decompile([v,w,y]),
    ?line Files = lsfiles(),
    ?line remove(files(Files, ".beam")).

crypto_works() ->
    try crypto:start() of
	{error,{already_started,crypto}} -> true;
	ok -> true
    catch
	error:_ ->
	    false
    end.

simple_crypto_fun(Key) ->
    fun(init) -> ok;
       ({debug_info, des3_cbc, crypt, _}) -> Key
    end.

analyse(suite) -> [];
analyse(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(data_dir, Config)),

    ?line done = a:start(5),

    ?line {ok, {a,{17,2}}} = cover:analyse(a, coverage, module),
    ?line {ok, [{{a,start,1},{6,0}},
		{{a,stop,1},{0,1}},
		{{a,pong,1},{1,0}},
		{{a,loop,3},{5,1}},
		{{a,trycatch,1},{4,0}},
		{{a,exit_kalle,0},{1,0}}]} = cover:analyse(a, coverage, function),
    ?line {ok, [{{a,start,1,1},{6,0}},
		{{a,stop,1,1},{0,1}},
		{{a,pong,1,1},{1,0}},
		{{a,loop,3,1},{3,1}},
		{{a,loop,3,2},{2,0}},
		{{a,trycatch,1,1},{4,0}},
		{{a,exit_kalle,0,1},{1,0}}]} = cover:analyse(a, coverage, clause),
    ?line {ok, [{{a,9},{1,0}},
		{{a,10},{1,0}},
		{{a,11},{1,0}},
		{{a,13},{1,0}},
		{{a,14},{1,0}},
		{{a,15},{1,0}},
		{{a,21},{0,1}},
		{{a,26},{1,0}},
		{{a,31},{1,0}},
		{{a,32},{1,0}},
		{{a,34},{1,0}},
		{{a,36},{0,1}},
		{{a,39},{1,0}},
		{{a,40},{1,0}},
		{{a,44},{1,0}},
		{{a,47},{1,0}},
		{{a,49},{1,0}},
		{{a,51},{1,0}},
		{{a,55},{1,0}}]} = cover:analyse(a, coverage, line),

    ?line {ok, {a,15}} = cover:analyse(a, calls, module),
    ?line {ok, [{{a,start,1},1},
		{{a,stop,1},0},
		{{a,pong,1},5},
		{{a,loop,3},6},
		{{a,trycatch,1},2},
		{{a,exit_kalle,0},1}]} = cover:analyse(a, calls, function),
    ?line {ok, [{{a,start,1,1},1},
		{{a,stop,1,1},0},
		{{a,pong,1,1},5},
		{{a,loop,3,1},5},
		{{a,loop,3,2},1},
		{{a,trycatch,1,1},2},
		{{a,exit_kalle,0,1},1}]} = cover:analyse(a, calls, clause),
    ?line {ok, [{{a,9},1},
		{{a,10},1},
		{{a,11},1},
		{{a,13},1},
		{{a,14},1},
		{{a,15},1},
		{{a,21},0},
		{{a,26},5},
		{{a,31},5},
		{{a,32},5},
		{{a,34},5},
		{{a,36},0},
		{{a,39},1},
		{{a,40},1},
		{{a,44},2},
		{{a,47},1},
		{{a,49},1},
		{{a,51},2},
		{{a,55},1}]} = cover:analyse(a, calls, line),

    ?line {ok, [{{a,start,1},{6,0}},
		{{a,stop,1},{0,1}},
		{{a,pong,1},{1,0}},
		{{a,loop,3},{5,1}},
		{{a,trycatch,1},{4,0}},
		{{a,exit_kalle,0},{1,0}}]} = cover:analyse(a),
    ?line {ok, {a,{17,2}}} = cover:analyse(a, module),
    ?line {ok, [{{a,start,1},1},
		{{a,stop,1},0},
		{{a,pong,1},5},
		{{a,loop,3},6},
		{{a,trycatch,1},2},
		{{a,exit_kalle,0},1}]} = cover:analyse(a, calls),

    ?line {ok, "a.COVER.out"} = cover:analyse_to_file(a),
    ?line {ok, "e.COVER.out"} = cover:analyse_to_file(e),
    ?line {ok, "a.COVER.html"} = cover:analyse_to_file(a,[html]),
    ?line {ok, "e.COVER.html"} = cover:analyse_to_file(e,[html]),

    %% analyse_to_file of file which is compiled from beam
    ?line {ok,f} = compile:file(f,[debug_info]),
    ?line code:purge(f),
    ?line {module,f} = code:load_file(f),
    ?line {ok,f} = cover:compile_beam(f),
    ?line f:f2(),
    ?line {ok, "f.COVER.out"} = cover:analyse_to_file(f),

    %% Source code can be found via source
    ?line {ok,v} = compile:file("compile_beam/v",[debug_info]),
    ?line code:purge(v),
    ?line {module,v} = code:load_file(v),
    ?line {ok,v} = cover:compile_beam(v),
    {ok,"v.COVER.out"} = cover:analyse_to_file(v),

    %% Source code cannot be found
    {ok,_} = file:copy("compile_beam/z.erl", "z.erl"),
    {ok,z} = compile:file(z,[debug_info]),
    code:purge(z),
    {module,z} = code:load_file(z),
    {ok,z} = cover:compile_beam(z),
    ok = file:delete("z.erl"),
    {error,no_source_code_found} = cover:analyse_to_file(z),
    code:purge(z),
    code:delete(z),

    ?line {error,{not_cover_compiled,b}} = cover:analyse(b),
    ?line {error,{not_cover_compiled,g}} = cover:analyse(g),
    ?line {error,{not_cover_compiled,b}} = cover:analyse_to_file(b),
    ?line {error,{not_cover_compiled,g}} = cover:analyse_to_file(g).

misc(suite) -> [];
misc(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(data_dir, Config)),

    ?line [a,cc,crypt,d,e,f,v] = lists:sort(cover:modules()),

    ?line {ok,cc} = compile:file(cc),
    ?line code:purge(cc),
    ?line {module,cc} = code:load_file(cc),
    ?line [a,crypt,d,e,f,v] = lists:sort(cover:modules()),

    ?line {file, _File} = cover:is_compiled(a),
    ?line false = cover:is_compiled(b),
    ?line false = cover:is_compiled(g),

    ?line ok = cover:reset(a),
    ?line {ok, {a,{0,19}}} = cover:analyse(a, module),
    ?line ok = cover:reset().

stop(suite) -> [];
stop(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(data_dir, Config)),

    ?line cover_compiled = code:which(a),
    ?line {ok,d} = compile:file(d, [{d,'AGE',42}]),
    ?line code:purge(d),
    ?line {module,d} = code:load_file(d),
    ?line ok = cover:stop(),
    ?line Beam = code:which(a),
    ?line true = is_unloaded(Beam),

    ?line Files = lsfiles(),
    ?line remove(files(Files, ".out")),
    ?line remove(files(Files, ".html")),
    ?line remove(files(Files, ".beam")).

distribution(suite) -> [];
distribution(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line ok = file:set_cwd(DataDir),

    ?line {ok,N1} = ?t:start_node(cover_SUITE_distribution1,slave,[]),
    ?line {ok,N2} = ?t:start_node(cover_SUITE_distribution2,slave,[]),
    ?line {ok,N3} = ?t:start_node(cover_SUITE_distribution3,slave,[]),
    ?line {ok,N4} = ?t:start_node(cover_SUITE_distribution4,slave,[]),

    %% Check that an already compiled module is loaded on new nodes
    ?line {ok,f} = cover:compile(f),
    ?line {ok,[_,_,_,_]} = cover:start(nodes()),
    ?line cover_compiled = code:which(f),
    ?line cover_compiled = rpc:call(N1,code,which,[f]),
    ?line cover_compiled = rpc:call(N2,code,which,[f]),
    ?line cover_compiled = rpc:call(N3,code,which,[f]),
    ?line cover_compiled = rpc:call(N4,code,which,[f]),

    %% Check that a node cannot be started twice
    ?line {ok,[]} = cover:start(N2),

    %% Check that the current node (i.e. the main node) is not started with
    %% start/1 and not stopped with stop/1
    ?line {ok,[]} = cover:start(node()),
    ?line ok = cover:stop(node()),
    ?line true = is_pid(whereis(cover_server)),

    %% Check that a new compiled module is loaded on all existing nodes
    ?line compile:file("compile_beam/v",[debug_info]),
    ?line {ok,v} = cover:compile_beam(v),
    ?line cover_compiled = code:which(v),
    ?line cover_compiled = rpc:call(N1,code,which,[v]),
    ?line cover_compiled = rpc:call(N2,code,which,[v]),
    ?line cover_compiled = rpc:call(N3,code,which,[v]),
    ?line cover_compiled = rpc:call(N4,code,which,[v]),
    
    %% this is lost when the node is killed
    ?line rpc:call(N3,f,f2,[]),
    ?line rpc:call(N3,erlang,halt,[]),

    %% this should be visible in analyse
    ?line rpc:call(N1,f,f1,[]),

    %% Check that data is collected from remote node when stopped
    ?line ok = cover:stop(N1),
    ?line N1Beam = rpc:call(N1,code,which,[f]),
    ?line true = is_unloaded(N1Beam),
    ?line check_f_calls(1,0),

    %% Call f:f1() again on another node and check that number of calls is
    %% accumulated.
    ?line f:f1(),
    ?line check_f_calls(2,0),
    
    %% Check that reset works on all nodes
    ?line f:f1(),
    ?line rpc:call(N2,f,f1,[]),
    ?line ok = cover:reset(f),
    ?line check_f_calls(0,0),
    
    %% Check that data is collected from all nodes
    ?line rpc:call(N2,f,f1,[]),
    ?line f:f2(),
    ?line check_f_calls(1,1),

    %% Check that same data is not fetched again (i.e. that analyse does
    %% reset on the remote node(s))
    ?line check_f_calls(1,1),

    %% Another checn that data is not fetched twice, i.e. when flushed
    %% then analyse should not add the same data again.
    ?line rpc:call(N4,f,f2,[]),
    ?line ok = cover:flush(N4),
    ?line check_f_calls(1,2),

    %% Check that flush collects data so calls are not lost if node is killed
    ?line rpc:call(N4,f,f2,[]),
    ?line ok = cover:flush(N4),
    ?line rpc:call(N4,erlang,halt,[]),
    ?line check_f_calls(1,3),

    %% Check that stop() unloads on all nodes
    ?line ok = cover:stop(),
    ?line timer:sleep(100), %% Give nodes time to unload on slow machines.
    ?line LocalBeam = code:which(f),
    ?line N2Beam = rpc:call(N2,code,which,[f]),
    ?line true = is_unloaded(LocalBeam),
    ?line true = is_unloaded(N2Beam),

    %% Check that cover_server on remote node does not die if main node dies
    ?line {ok,[N1]} = cover:start(N1),
    ?line true = is_pid(N1Server = rpc:call(N1,erlang,whereis,[cover_server])),
    ?line exit(whereis(cover_server),kill),
    ?line timer:sleep(100),
    ?line N1Server = rpc:call(N1,erlang,whereis,[cover_server]),

    %% Cleanup
    ?line Files = lsfiles(),
    ?line remove(files(Files, ".beam")),
    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2).

%% Test that a lost node is reconnected
reconnect(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = file:set_cwd(DataDir),

    {ok,a} = compile:file(a),
    {ok,b} = compile:file(b),
    {ok,f} = compile:file(f),

    {ok,N1} = ?t:start_node(cover_SUITE_reconnect,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    {ok,a} = cover:compile(a),
    {ok,f} = cover:compile(f),
    {ok,[N1]} = cover:start(nodes()),

    %% Some calls to check later
    rpc:call(N1,f,f1,[]),
    cover:flush(N1),
    rpc:call(N1,f,f1,[]),

    %% This will cause a call to f:f2() when nodes()==[] on N1
    rpc:cast(N1,f,call_f2_when_isolated,[]),

    %% Disconnect and check that node is removed from main cover node
    net_kernel:disconnect(N1),
    timer:sleep(500), % allow some to detect disconnect and for f:f2() call
    [] = cover:which_nodes(),

    %% Do some add one module (b) and remove one module (a)
    code:purge(a),
    {module,a} = code:load_file(a),
    {ok,b} = cover:compile(b),
    cover_compiled = code:which(b),

    [] = cover:which_nodes(),
    check_f_calls(1,0), % only the first call - before the flush

    %% Reconnect the node and check that b and f are cover compiled but not a
    net_kernel:connect_node(N1),
    timer:sleep(100),
    [N1] = cover:which_nodes(), % we are reconnected
    cover_compiled = rpc:call(N1,code,which,[b]),
    cover_compiled = rpc:call(N1,code,which,[f]),
    ABeam = rpc:call(N1,code,which,[a]),
    false = (cover_compiled==ABeam),

    %% Ensure that we have:
    %% * one f1 call from before the flush,
    %% * one f1 call from after the flush but before disconnect
    %% * one f2 call when disconnected
    check_f_calls(2,1),

    cover:stop(),
    ?t:stop_node(N1),
    ok.

%% Test that a lost node is reconnected - also if it has been dead
die_and_reconnect(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = file:set_cwd(DataDir),

    {ok,f} = compile:file(f),

    NodeName = cover_SUITE_die_and_reconnect,
    {ok,N1} = ?t:start_node(NodeName,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    %% {ok,a} = cover:compile(a),
    {ok,f} = cover:compile(f),
    {ok,[N1]} = cover:start(nodes()),

    %% Some calls to check later
    rpc:call(N1,f,f1,[]),
    cover:flush(N1),
    rpc:call(N1,f,f1,[]),

    %% Kill the node
    rpc:call(N1,erlang,halt,[]),
    [] = cover:which_nodes(),

    check_f_calls(1,0), % only the first call - before the flush

    %% Restart the node and check that cover reconnects
    {ok,N1} = ?t:start_node(NodeName,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    timer:sleep(100),
    [N1] = cover:which_nodes(), % we are reconnected
    cover_compiled = rpc:call(N1,code,which,[f]),

    %% One more call...
    rpc:call(N1,f,f1,[]),

    %% Ensure that no more calls are counted
    check_f_calls(2,0),

    cover:stop(),
    ?t:stop_node(N1),
    ok.

%% Test that a stopped node is not marked as lost, i.e. that it is not
%% reconnected if it is restarted (OTP-10638)
dont_reconnect_after_stop(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = file:set_cwd(DataDir),

    {ok,f} = compile:file(f),

    NodeName = cover_SUITE_dont_reconnect_after_stop,
    {ok,N1} = ?t:start_node(NodeName,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    {ok,f} = cover:compile(f),
    {ok,[N1]} = cover:start(nodes()),

    %% A call to check later
    rpc:call(N1,f,f1,[]),

    %% Stop cover on the node, then terminate the node
    cover:stop(N1),
    rpc:call(N1,erlang,halt,[]),
    [] = cover:which_nodes(),

    check_f_calls(1,0),

    %% Restart the node and check that cover does not reconnect
    {ok,N1} = ?t:start_node(NodeName,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    timer:sleep(300),
    [] = cover:which_nodes(),
    Beam = rpc:call(N1,code,which,[f]),
    false = (Beam==cover_compiled),

    %% One more call...
    rpc:call(N1,f,f1,[]),
    cover:flush(N1),

    %% Ensure that the last call is not counted
    check_f_calls(1,0),

    cover:stop(),
    ?t:stop_node(N1),
    ok.

%% Test that a node which is stopped while it is marked as lost is not
%% reconnected if it is restarted (OTP-10638)
stop_node_after_disconnect(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = file:set_cwd(DataDir),

    {ok,f} = compile:file(f),

    NodeName = cover_SUITE_stop_node_after_disconnect,
    {ok,N1} = ?t:start_node(NodeName,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    {ok,f} = cover:compile(f),
    {ok,[N1]} = cover:start(nodes()),

    %% A call to check later
    rpc:call(N1,f,f1,[]),

    %% Flush the node, then terminate the node to make it marked as lost
    cover:flush(N1),
    rpc:call(N1,erlang,halt,[]),

    check_f_calls(1,0),

    %% Stop cover on node
    cover:stop(N1),

    %% Restart the node and check that cover does not reconnect
    {ok,N1} = ?t:start_node(NodeName,peer,
			    [{args," -pa " ++ DataDir},{start_cover,false}]),
    timer:sleep(300),
    [] = cover:which_nodes(),
    Beam = rpc:call(N1,code,which,[f]),
    false = (Beam==cover_compiled),

    %% One more call...
    rpc:call(N1,f,f1,[]),
    cover:flush(N1),

    %% Ensure that the last call is not counted
    check_f_calls(1,0),

    cover:stop(),
    ?t:stop_node(N1),
    ok.

export_import(suite) -> [];
export_import(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line ok = file:set_cwd(DataDir),
    ?line PortCount = length(erlang:ports()),

    %% Export one module
    ?line {ok,f} = cover:compile(f),
    ?line f:f1(),
    %% check that no info is written about where data comes from when no
    %% files are imported
    ?line ?t:capture_start(),
    ?line check_f_calls(1,0),
    ?line [] = ?t:capture_get(),
    ?line ?t:capture_stop(),
    ?line ok = cover:export("f_exported",f),
    ?line check_f_calls(1,0),
    ?line ok = cover:stop(),
    
    %% Check that same data exists after import and that info is written about
    %% data comming from imported file
    ?line ok = cover:import("f_exported"),
    ?line ?t:capture_start(),
    ?line check_f_calls(1,0),
    ?line [Text1] = ?t:capture_get(),
    ?line "Analysis includes data from imported files"++_ = lists:flatten(Text1),
    ?line ?t:capture_stop(),

    %% Export all modules
    ?line {ok,a} = cover:compile(a),
    ?line ?t:capture_start(),
    ?line ok = cover:export("all_exported"),
    ?line [] = ?t:capture_get(),
%    ?line "Export includes data from imported files"++_ = lists:flatten(Text2),
    ?line ?t:capture_stop(),
    ?line ok = cover:stop(),
    ?line ok = cover:import("all_exported"),
    ?line check_f_calls(1,0),

    %% Check that data is reset when module is compiled again, and that
    %% warning is written when data is deleted for imported module.
    ?line ?t:capture_start(),
    ?line {ok,f} = cover:compile(f),
    ?line timer:sleep(10), % capture needs some time
    ?line [Text3] = ?t:capture_get(),
    ?line "WARNING: Deleting data for module f imported from" ++ _ = 
	lists:flatten(Text3),
    ?line ?t:capture_stop(),
    ?line check_f_calls(0,0),
    
    %% Check that data is summed up when first compiled and then imported
    %% The module which has been compiled (f) is loaded from the file 
    %% all_exported again (since it has been reset during cover compiling), 
    %% but the other module (a) is not loaded since it is already loaded    
    ?line f:f1(),
    ?line f:f2(),
    ?line ok = cover:import("f_exported"),
    ?line ?t:capture_start(),
    ?line ok = cover:import("all_exported"),
    ?line [Text4] = ?t:capture_get(), % a is not loaded again
    ?line "WARNING: Module a already imported from " ++ _ = lists:flatten(Text4),
    ?line ?t:capture_stop(),
    ?line check_f_calls(3,1),

    %% Check that warning is written when same file is imported twice,
    %% and that data is not imported again
    ?line ?t:capture_start(),
    ?line ok = cover:import("all_exported"),
    ?line [Text5,Text6] = ?t:capture_get(),
    ?line "WARNING: Module f already imported from " ++ _ = lists:flatten(Text5),
    ?line "WARNING: Module a already imported from " ++ _ = lists:flatten(Text6),
    ?line ?t:capture_stop(),
    ?line check_f_calls(3,1),

    %% Check that reset removes all data and that the file which has been
    %% reset can be imported again with no warning
    ?line cover:reset(f),
    ?line check_f_calls(0,0),
    ?line ?t:capture_start(),
    ?line ok = cover:import("all_exported"),
    ?line [Text7] = ?t:capture_get(), % warning only on mod a
    ?line "WARNING: Module a already imported from " ++ _ = lists:flatten(Text7),
    ?line ?t:capture_stop(),
    ?line check_f_calls(1,0),

    %% same as above - only reset all
    ?line cover:reset(),
    ?line check_f_calls(0,0),
    ?line ?t:capture_start(),
    ?line ok = cover:import("all_exported"),
    ?line [] = ?t:capture_get(), % no warnings
    ?line ?t:capture_stop(),
    ?line check_f_calls(1,0),

    %% Check no raw files are left open
    ?line PortCount = length(erlang:ports()),

    %% Cleanup
    ?line ok = cover:stop(),
    ?line Files = lsfiles(),
    ?line remove(["f_exported","all_exported"|files(Files, ".beam")]).


otp_5031(suite) -> [];
otp_5031(Config) when is_list(Config) ->

    Dog = ?t:timetrap(?t:seconds(10)),

    ?line {ok,N1} = ?t:start_node(cover_SUITE_distribution1,slave,[]),
    ?line {ok,[N1]} = cover:start(N1),
    ?line {error,not_main_node} = rpc:call(N1,cover,modules,[]),
    ?line cover:stop(),
    
    ?t:timetrap_cancel(Dog),
    ok.

eif(doc) ->
    ["Test the \'Exclude Included Functions\' functionality"];
eif(suite) ->
    [];
eif(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(filename:join(?config(data_dir, Config),
					  "included_functions")),
    ?line {ok, cover_inc} = compile:file(cover_inc,[debug_info]),
    ?line {ok, cover_inc} = cover:compile_beam(cover_inc),

    %% This function will cause an included function to be executed.
    %% The analysis should only show the lines that actually exist
    %% in cover_inc.beam - not the ones from the included file.
    ?line cover_inc:func(),
    ?line {ok, [_, _]} = cover:analyse(cover_inc, line),
    ?line cover:stop(),
    ok.
    
otp_5305(suite) -> [];
otp_5305(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(priv_dir, Config)),

    File = "t.erl",
    Test = <<"-module(t).
              -export([t/0]).
              -include_lib(\"stdlib/include/ms_transform.hrl\").
              t() ->
                  ets:fun2ms(fun(X) -> X end).
             ">>,
    ?line ok = file:write_file(File, Test),
    ?line {ok, t} = cover:compile(File),
    ?line cover:stop(),
    ?line ok = file:delete(File),

    ok.

otp_5418(suite) -> [];
otp_5418(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(priv_dir, Config)),

    File = "t.erl",
    Test = <<"-module(t).
             ">>,
    ?line ok = file:write_file(File, Test),
    ?line {ok, t} = cover:compile(File),
    ?line {ok,{t,{0,0}}} = cover:analyse(t, module),
    ?line cover:stop(),
    ?line ok = file:delete(File),

    ok.

otp_6115(Config) when is_list(Config) ->
    ?line {ok, CWD} = file:get_cwd(),
    ?line Dir = filename:join(?config(data_dir, Config), otp_6115),
    ?line ok = file:set_cwd(Dir),
    ?line {ok, f1} = compile:file(f1, [debug_info]),
    ?line {ok, f2} = compile:file(f2, [debug_info]),

    %% Cover compile f1, but not f2
    ?line {ok, f1} = cover:compile(f1),

    %% If f1 is cover compiled, a process P is started with a
    %% reference to the fun created in start_fail/0, and cover:stop() is
    %% called, then P should be killed.
    %% This is because (the fun held by P) references the cover
    %% compiled code which should be *unloaded* when cover:stop() is
    %% called -- running cover compiled code when there is no cover
    %% server and thus no ets tables to bump counters in, makes no
    %% sense.
    Pid1 = f1:start_a(),
    Pid2 = f1:start_b(),

    %% Now stop cover
    ?line cover:stop(),
    
    %% Ensure that f1 is loaded (and not cover compiled), and that
    %% both Pid1 and Pid2 are dead.
    case code:which(f1) of
	Beam when is_list(Beam) ->
	    ok;
	Other ->
	    ?line ?t:fail({"f1 is not reloaded", Other})
    end,
    case process_info(Pid1) of
	undefined ->
	    ok;
	_PI1 ->
	    RefToOldP1 = erlang:check_process_code(Pid1, f1),
	    ?t:fail({"Pid1 still alive", RefToOldP1})
    end,
    case process_info(Pid2) of
	undefined ->
	    ok;
	_PI2 ->
	    RefToOldP2 = erlang:check_process_code(Pid1, f2),
	    ?t:fail({"Pid2 still alive", RefToOldP2})
    end,

    ?line file:set_cwd(CWD),
    ok.

otp_7095(doc) ->
    ["andalso/orelse"];
otp_7095(suite) -> [];
otp_7095(Config) when is_list(Config) ->
    ?line ok = file:set_cwd(?config(priv_dir, Config)),

    File = "t.erl",
    Test = <<"-module(t).
              -export([t/0]).
              t() ->
                  t1(),
                  t2(),
                  t3(),
                  t4(),
                  t5(),
                  put(t6, 0),
                  0  = t6(),
                  1 = erase(t6),
                  t7(),
                  put(t8, 0),
                  {'EXIT',{{badarg,0},_}} = (catch t8()),
                  1 = erase(t8),
                  t9(),
                  ok.

              t1() ->
                  false        % 20
                    andalso
                  true.        % 22

              t2() ->
                  true         % 25
                    andalso
                  true.        % 27

              t3() ->
                  false        % 30
                    orelse
                  true.        % 32

              t4() ->
                  true         % 35
                    orelse
                  true.        % 37

              t5() ->
                  true         % 40
                    andalso
                  true         % 42
                    andalso 
                  false.       % 44

              t6() ->
                  true andalso % 47
                  add_one(t6). % 48

              t7() ->
                  true         % 51
                    andalso
                  false        % 53
                    andalso 
                  not_ok.      % 55

              t8() ->
                  true         % 58
                    andalso 
                  true         % 60
                    andalso
                  add_one(t8)  % 62
                    andalso
                  false.       % 64

              t9() ->
                  if           % 67
                      true -> 
                          true % 69
                            andalso 
                          false % 71
                  end
                    orelse
                  case ok of   % 74
                      true -> 
                          a;   % 76
                      _ -> 
                          true % 78
                  end.

              add_one(T) ->
                  put(T, get(T) + 1). % 82
             ">>,
    ?line ok = file:write_file(File, Test),
    ?line {ok, t} = cover:compile(File),
    ?line ok = t:t(),
    ?line {ok,[{{t,4},1},{{t,5},1},{{t,6},1},{{t,7},1},{{t,8},1},{{t,9},1},
               {{t,10},1},{{t,11},1},{{t,12},1},{{t,13},1},{{t,14},1},
               {{t,15},1},{{t,16},1},{{t,17},1},
               {{t,20},1},{{t,22},0},
               {{t,25},1},{{t,27},1},
               {{t,30},1},{{t,32},1},
               {{t,35},1},{{t,37},0},
               {{t,40},1},{{t,42},1},{{t,44},1},
               {{t,47},1},{{t,48},1},
               {{t,51},1},{{t,53},1},{{t,55},0},
               {{t,58},1},{{t,60},1},{{t,62},1},{{t,64},0},
               {{t,67},1},{{t,69},1},{{t,71},1},{{t,74},1},
                    {{t,76},0},{{t,78},1},
               {{t,82},2}]} = cover:analyse(t, calls, line),
    ?line cover:stop(),
    ?line ok = file:delete(File),

    ok.

otp_8270(doc) ->
    ["OTP-8270. Bug."];
otp_8270(suite) -> [];
otp_8270(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line ok = file:set_cwd(DataDir),

    ?line PrivDir = ?config(priv_dir, Config),

    As = [{args," -pa " ++ PrivDir}],
    ?line {ok,N1} = ?t:start_node(cover_n1,slave,As),
    ?line {ok,N2} = ?t:start_node(cover_n2,slave,As),
    ?line {ok,N3} = ?t:start_node(cover_n3,slave,As),
    
    timer:sleep(500),
    cover:start(nodes()),

    Test = <<
     "-module(m).\n"
     "-compile(export_all).\n"
     "t() -> t(0).\n"
     "l() ->\n"
     "   catch ets:tab2list(cover_internal_data_table).\n"
     "t(Sz) ->\n"
     "   case ets:info(cover_internal_data_table, size) of\n"
     "       Sz ->\n"
     "           m:t(Sz); % Not a local call! Newly loaded code is entered.\n"
     "       NSz ->\n"
     "           % error_logger:info_msg(\"~p: ~p ~p change~n L1 ~p~n\", \n"
     "           % [node(), Sz, NSz, l()]),\n"
     "           m:t(NSz)\n"
     "   end.\n">>,
    ?line _File = c_mod(m, Test, Config),
    Fun = fun m:t/0,
    ?line Pid1 = spawn(Fun),
    ?line Pid2 = spawn(N1, Fun),
    ?line Pid3 = spawn(N2, Fun),
    ?line Pid4 = spawn(N3, Fun),

    ?line {ok, m} = cover:compile_beam(m),

    timer:sleep(1000),

    ?line Info = erlang:process_info(Pid1),
    ?line N1_info = rpc:call(N1, erlang, process_info, [Pid2]),
    ?line N2_info = rpc:call(N2, erlang, process_info, [Pid3]),
    ?line N3_info = rpc:call(N3, erlang, process_info, [Pid4]),

    ?line true = is_list(Info),
    ?line {N1,true} = {N1,is_list(N1_info)},
    ?line {N2,true} = {N2,is_list(N2_info)},
    ?line {N3,true} = {N3,is_list(N3_info)},

    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2),
    ?line ?t:stop_node(N3),
    ok.

otp_8273(doc) ->
    ["OTP-8273. Bug."];
otp_8273(suite) -> [];
otp_8273(Config) when is_list(Config) ->
    Test = <<"-module(t).
              -export([t/0]).
              t() ->
                  foo = true andalso foo,
                  bar = false orelse bar,
                  ok.
             ">>,
    ?line File = cc_mod(t, Test, Config),
    ?line ok = t:t(),
    ?line cover:stop(),
    ?line ok = file:delete(File),

    ok.

otp_8340(doc) ->
    ["OTP-8340. Bug."];
otp_8340(suite) -> [];
otp_8340(Config) when is_list(Config) ->
    ?line [{{t,1},1},{{t,2},1},{{t,4},1}] = 
        analyse_expr(<<"<< \n"
                       " <<3:2, \n"
                       "   SeqId:62>> \n"
                       "      || SeqId <- [64] >>">>, Config),

    ok.

otp_8188(doc) ->
    ["Clauses on the same line."];
otp_8188(suite) -> [];
otp_8188(Config) when is_list(Config) ->
    %% This example covers the bug report:
    Test = <<"-module(t).
              -export([test/1]).

              -define(FOOBAR(X),
                      case X of
                          ok -> true;
                          _ -> false
                      end).

              test(X)->
                  _Res = 
                  ?FOOBAR(X).
             ">>,
    ?line File = cc_mod(t, Test, Config),
    ?line false = t:test(nok),
    ?line {ok,[{{t,11},1},{{t,12},1}]} = cover:analyse(t, calls, line),
    ?line cover:stop(),
    ?line ok = file:delete(File),

    %% Bit string comprehensions are now traversed;
    %% the handling of list comprehensions has been improved:
    comprehension_8188(Config),

    %% Variants of the reported bug:
    bug_8188(Config),
    ok.

bug_8188(Cf) ->
    ?line [{{t,1},1},{{t,2},1},{{t,3},1}] =
        analyse_expr(<<"A = 3,\n" % 1
                       "    case A of\n" % 1
                       "        2 -> two; 3 -> three end, A + 2">>,  % 1
                     Cf),

    ?line [{{t,1},1},
           {{t,2},0},
           {{t,3},1},
           {{t,4},1},
           {{t,5},1},
           {{t,6},0},
           {{t,7},1},
           {{t,9},2}] =
        analyse_expr(<<"case two() of\n" % 1
                       "     1 -> 2;\n" % 0
                       "    _  -> begin 3 end\n" % 1
                       "              +\n" % 1
                       "          begin 4 end end, case two() of\n" % 1
                       "                               1 -> a;\n" % 0
                       "                               2 -> b; 3 -> c\n" % 1
                       "                           end.\n"
                       "two() -> 2">>, Cf), % 2

    ?line [{{t,1},1}, {{t,2},1}, {{t,3},1},
           {{t,4},1}, {{t,5},1}, {{t,6},0}] =
        analyse_expr(<<"    self() ! 1,\n"
                       "    receive \n"
                       "        X=1 -> a;\n"
                       "        X=2 -> b end, case X of \n"
                       "                          1 -> a;\n"
                       "                          2 -> b\n"
                       "                      end">>, Cf),

    T0 = <<"t1(X) ->\n "
           "case X of\n"
           "  1 -> A=a,B=A,B; % bump Li\n"
           "  2 -> b; 3 -> case X of % 2 -> b shall bump Li\n"
           "                  3 -> a; % bump Li\n"
           "                  2 -> b end; 4 -> d end, case X of  % Li\n"
           "                                            1 -> a;\n"
           "                                            2 -> b; 3 -> c;\n"
           "                                            4 -> d\n"
           "                                          end">>,

    T1 = [<<"a = t1(1). ">>,T0],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},1}, {{t,4},0},
           {{t,5},0}, {{t,6},1}, {{t,7},1}, {{t,8},0}, {{t,9},0}] = 
        analyse_expr(T1, Cf),

    T2 = [<<"b = t1(2). ">>,T0],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},1},
           {{t,5},0}, {{t,6},1}, {{t,7},0}, {{t,8},1}, {{t,9},0}] = 
        analyse_expr(T2, Cf),

    T3 = [<<"c = t1(3). ">>,T0],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},1},
           {{t,5},1}, {{t,6},1}, {{t,7},0}, {{t,8},1}, {{t,9},0}] = 
        analyse_expr(T3, Cf),

    T4 = [<<"d = t1(4). ">>,T0],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},0},
           {{t,5},0}, {{t,6},1}, {{t,7},0}, {{t,8},0}, {{t,9},1}] = 
        analyse_expr(T4, Cf),

    ?line [{{t,1},1},{{t,2},1},{{t,3},1},{{t,4},1},{{t,5},1}] =
        analyse_expr(
          <<"2 = x3(1). "
            "x3(X) ->\n"
            "    case X of \n"
            "        1 -> case X of\n"
            "                 1 -> a, Y = 2;\n"
            "                 2 -> b, Y = 3 end, Y; 2 -> Y = 4 end, Y">>, Cf),
    
    ?line [{{t,1},1},{{t,2},1},{{t,3},1},{{t,4},1}] = 
        analyse_expr(
          <<"1 = x4(1). "
            "x4(X) ->\n"
            "  case X of\n"
            "    1 -> case X of\n"
            "           1 -> Y = 1 end, case X of 1 -> Y = 1 end, Y end">>,
          Cf),

    T10 = <<"t1(X) ->\n"
            "if\n"
            "  X =:= 1 -> a;\n"
            "  X =:= 2 -> b; X =:= 3 -> c end, case X of \n"
            "                        1 -> a;\n"
            "                        2 -> b; 3 -> c end, case X of\n"
            "                                              1 -> a;\n"
            "                                              2 -> b; 3 -> c\n"
            "                                            end">>,
    T11 = [<<"a = t1(1). ">>,T10],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},1}, {{t,4},1},
           {{t,5},1}, {{t,6},1}, {{t,7},1}, {{t,8},0}] = 
        analyse_expr(T11, Cf),

    T12 = [<<"b = t1(2). ">>,T10],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},1},
           {{t,5},0}, {{t,6},1}, {{t,7},0}, {{t,8},1}] = 
        analyse_expr(T12, Cf),

    T13 = [<<"c = t1(3). ">>,T10],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},1},
           {{t,5},0}, {{t,6},1}, {{t,7},0}, {{t,8},1}] = 
        analyse_expr(T13, Cf),

    T20 = <<"t1(X) ->\n"
            "case X of\n"
            "        1 -> a;\n"
            "        2 -> b; 3 -> case X of\n"
            "                         1 -> a;\n"
            "                         2 -> b; 3 -> c end end, case X of\n"
            "                                             1 -> a;\n"
            "                                             2 -> b; 3 -> c\n"
            "                                         end">>,

    T21 = [<<"a = t1(1). ">>,T20],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},1}, {{t,4},0},
           {{t,5},0}, {{t,6},1}, {{t,7},1}, {{t,8},0}] = 
        analyse_expr(T21, Cf),

    T22 = [<<"b = t1(2). ">>,T20],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},1},
           {{t,5},0}, {{t,6},1}, {{t,7},0}, {{t,8},1}] = 
        analyse_expr(T22, Cf),

    T23 = [<<"c = t1(3). ">>,T20],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0}, {{t,4},1},
           {{t,5},0}, {{t,6},1}, {{t,7},0}, {{t,8},1}] = 
        analyse_expr(T23, Cf),

    T30 = <<
        "t1(X) ->\n"
        "case X of\n"
        " 1 -> a;\n"
        " 2 -> b; 3 -> case X of 1 -> a; 2 -> b; 3 -> c end end, case X of\n"
        "                                                 1 -> a;\n"
        "                                                 2 -> b; 3 -> c\n"
        "                                                end\n">>,

    T31 = [<<"a = t1(1). ">>,T30],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},1},
           {{t,4},1}, {{t,5},1}, {{t,6},0}] =
        analyse_expr(T31, Cf),

    T32 = [<<"b = t1(2). ">>,T30],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0},
           {{t,4},1}, {{t,5},0}, {{t,6},1}] =
        analyse_expr(T32, Cf),

    T33 = [<<"c = t1(3). ">>,T30],
    ?line [{{t,1},1}, {{t,2},1}, {{t,3},0},
           {{t,4},1}, {{t,5},0}, {{t,6},1}] =
        analyse_expr(T33, Cf),

    %% 'try' now traverses the body as a body...
    ?line [{{t,1},1},{{t,2},1},{{t,3},1},{{t,4},0},{{t,6},1}] = 
        analyse_expr(<<"try \n"
                       "    B = 2, \n"
                       "    C = erlang:error(foo), \n"
                       "    {B,C} \n"
                       "catch _:_ -> \n"
                       "    foo \n"
                       "end">>, Cf),

    %% receive after:
    ?line [{{t,1},1},{{t,2},0},{{t,3},1}] = 
        analyse_expr(<<"receive \n"
                       "    X=1 -> a; \n"
                       "    X=2 -> b after begin 10 end -> X=3 end">>, Cf),
    ?line [{{t,1},1},{{t,2},0},{{t,3},1}] =
        analyse_expr(<<"receive \n"
                       "    X=1 -> a; \n"
                       "    X=2 -> b after 10 -> begin X=3 end end">>, Cf),
    ok.

comprehension_8188(Cf) ->
    ?line  [{{t,1},1}] = 
        analyse_expr(<<"[begin X end || X <- [1,2,3], X > 1]">>, Cf),
    ?line [{{t,1},1},{{t,2},1}] = 
        analyse_expr(<<"[begin X end || \n"
                       "    X <- [1,2,3], X > 1]">>, Cf),
    ?line [{{t,1},1},{{t,2},1},{{t,3},3}] = 
        analyse_expr(<<"[begin X end || \n "
                       "    X <- [1,2,3], \n "
                       "    X > 1]">>, Cf),
    ?line [{{t,1},1},{{t,3},1},{{t,4},3}] = 
        analyse_expr(<<"[begin X end || \n "
                       "    X <- \n "
                       "        [1,2,3], \n "
                       "    X > 1]">>, Cf),
    ?line [{{t,1},1},{{t,2},2}] =
        analyse_expr(<<"[  \n "
                       "   X || X <- [1,2,3], X > 1]">>, Cf),
    ?line [{{t,1},1},{{t,2},2},{{t,3},3}] =
        analyse_expr(<<"[ \n"
                       "  X || X <- [1,2,3], \n"
                       "  X > 1]">>, Cf),
    ?line [{{t,1},1},{{t,2},1},{{t,3},2}] =
        analyse_expr(<<"[ \n "
                       "   X || X <- [1,2,3], X > 1, \n"
                       "   X > 2]">>, Cf),

    ?line [{{t,1},1},
           {{t,3},2},
           {{t,5},1},
           {{t,7},1},
           {{t,8},0},
           {{t,12},3},
           {{t,15},2},
           {{t,17},2},
           {{t,18},1}] =
        analyse_expr(<<"[ \n" % 1
                       "  begin\n"
                       "      X * 2\n" % 2
                       "  end ||\n"
                       "    X <- [1,\n" % 1
                       "          case two() of\n"
                       "              2 -> 2;\n" % 1
                       "              _ -> two\n" % 0
                       "          end,\n"
                       "          3],\n"
                       "    begin\n"
                       "        math:sqrt(X) > 1.0\n" % 3
                       "    end,\n"
                       "    begin\n"
                       "        true\n" % 2
                       "    end,\n"
                       "    true]. \n" % 2
                       "  two() -> 2">>, Cf), % 1

    ?line [{{t,1},1},
           {{t,2},2},
           {{t,3},1},
           {{t,5},1},
           {{t,6},0},
           {{t,9},3},
           {{t,10},2},
           {{t,11},2},
           {{t,12},1}] =
        analyse_expr(<<"[ \n"
                       "      X * 2 || \n" % 2
                       "    X <- [1,\n" % 1
                       "          case two() of\n"
                       "              2 -> 2;\n" % 1
                       "              _ -> two\n" % 0
                       "          end,\n"
                       "          3],\n"
                       "    math:sqrt(X) > 1.0,\n" % 3
                       "    true,\n" % 2
                       "    true]. \n" % 2
                       "  two() -> 2">>, Cf), % 1

    ?line [{{t,1},1},
           {{t,2},2},
           {{t,3},1},
           {{t,4},1},
           {{t,5},0},
           {{t,8},1},
           {{t,9},0},
           {{t,12},3},
           {{t,13},2},
           {{t,14},2}] = 
        analyse_expr(<<"<< \n" % 1
                       " << (X*2) >> || \n" % 2
                       "    <<X>> <= << (case two() of\n"
                       "                     2 -> 1;\n" % 1
                       "                     _ -> 2\n" % 0
                       "                 end)/integer,\n"
                       "                (case two() of \n"
                       "                    2 -> 2;\n" % 1
                       "                    _ -> two\n" % 0
                       "                 end)/integer,\n"
                       "                3 >>, \n"
                       "    math:sqrt(X) > 1.0,\n" % 3
                       "    true >>.\n" % 2
                       "two() -> 2">>, Cf),

    ?line [{{t,1},1},
           {{t,2},4},
           {{t,4},1},
           {{t,6},1},
           {{t,7},0},
           {{t,10},3},
           {{t,11},2},
           {{t,12},4},
           {{t,13},1}] =
        analyse_expr(<<"<< \n" % 1
                       " << (2)\n" % 4
                       "     :(8) >> || \n"
                       "    <<X>> <= << 1,\n" % 1
                       "                (case two() of \n"
                       "                    2 -> 2;\n" % 1
                       "                    _ -> two\n" % 0
                       "                 end)/integer,\n"
                       "                3 >>, \n"
                       "    math:sqrt(X) > 1.0,\n" % 3
                       "    <<_>> <= << 1, 2 >>,\n" % 2
                       "    true >>.\n" % 4
                       "two() -> 2">>, Cf), % 1

    ok.

%%--Auxiliary------------------------------------------------------------

analyse_expr(Expr, Config) ->
    Binary = [<<"-module(t). "
                "-export([t/0]). "
                "t() -> ">>, Expr, <<".\n">>],
    File = cc_mod(t, Binary, Config),
    t:t(),
    {ok, Result} = cover:analyse(t, calls, line),
    ok = file:delete(File),
    Result.

cc_mod(M, Binary, Config) ->
    {ok, Dir} = file:get_cwd(),
    PrivDir = ?config(priv_dir, Config),
    ok = file:set_cwd(PrivDir),
    File = atom_to_list(M) ++ ".erl",
    try 
        ok = file:write_file(File, Binary),
        {ok, M} = cover:compile(File),
        filename:join(PrivDir, File)
    after file:set_cwd(Dir)
    end.

c_mod(M, Binary, Config) ->
    {ok, Dir} = file:get_cwd(),
    PrivDir = ?config(priv_dir, Config),
    ok = file:set_cwd(PrivDir),
    File = atom_to_list(M) ++ ".erl",
    try 
        ok = file:write_file(File, Binary),
        {ok, M} = compile:file(File, [debug_info]),
        code:purge(M),
        AbsFile = filename:rootname(File, ".erl"),
        code:load_abs(AbsFile, M),
        filename:join(PrivDir, File)
    after file:set_cwd(Dir)
    end.

lsfiles() ->
    {ok, CWD} = file:get_cwd(),
    lsfiles(CWD).

lsfiles(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    Files.

files(Files, Ext) ->
    lists:filter(fun(File) ->
			 case filename:extension(File) of
			     Ext -> true;
			     _ -> false
			 end
		 end,
		 Files).

remove([File|Files]) ->
    ok = file:delete(File),
    remove(Files);
remove([]) ->
    ok.

decompile([Mod|Mods]) ->
    code:purge(Mod),
    code:delete(Mod),
    decompile(Mods);
decompile([]) ->
    ok.

is_unloaded(What) ->
    if
	is_list(What) -> true;
	What==non_existing -> true;
	true -> false
    end.

check_f_calls(F1,F2) ->
    {ok,[{{f,f1,0},F1},{{f,f2,0},F2}|_]} = cover:analyse(f,calls,function).
