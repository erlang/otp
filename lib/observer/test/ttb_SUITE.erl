%%
%% %CopyrightBegin%
%%
%%
%% Copyright Ericsson AB 2002-2022. All Rights Reserved.
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

-module(ttb_SUITE).

-compile(export_all).
%% Test functions
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 file/1,file_no_pi/1,file_fetch/1,wrap/1,wrap_merge/1,
	 wrap_merge_fetch_format/1,write_config1/1,write_config2/1,
	 write_config3/1,history/1,write_trace_info/1,seq_trace/1,
	 diskless/1,otp_4967_1/1,otp_4967_2/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([foo/0]).

-include_lib("common_test/include/ct.hrl").

-define(default_timeout, test_server:minutes(1)).
-define(OUTPUT, "handler_output").
-define(FNAME, "temptest").
-define(DIRNAME, "ddtemp").

init_per_testcase(Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    ttb:stop(),
    rm(?OUTPUT),
    [rm(Upload) || Upload<-filelib:wildcard("ttb_upload*")],
    rm(?DIRNAME),
    [rm(At) || At <- filelib:wildcard("*@*")],
    rm("ttb_last_config"),
    %% Workaround for bug(?) in test_server - if the test case fails
    %% with a timetrap timeout, then end_per_testcase will run with
    %% faulty group_leader - which in turn makes test_server:stop_node
    %% hang (stop_node is called by most of the cleanup functions).
    %% Therefore we do the cleanup before each testcase instead - this
    %% is obviously not 100% correct, but it will at least make sure
    %% that the nodes which are to be started in a test case at are
    %% terminated.
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    %% try apply(?MODULE,Case,[cleanup,Config])
    %% catch error:undef -> ok
    %% end,
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [file, file_no_pi, file_fetch, wrap, wrap_merge,
     wrap_merge_fetch_format, write_config1, write_config2,
     write_config3, history, write_trace_info, seq_trace,
     diskless, diskless_wrap, otp_4967_1, otp_4967_2,
     fetch_when_no_option_given, basic_ttb_run_ip_port, basic_ttb_run_file_port,
     return_fetch_dir_implies_fetch, logfile_name_in_fetch_dir, upload_to_my_logdir,
     upload_to_my_existing_logdir, fetch_with_options_not_as_list,
     error_when_formatting_multiple_files_4393, format_on_trace_stop,
     trace_to_remote_files_on_localhost_with_different_pwd,
     trace_to_local_files_on_localhost_with_different_pwd,
     trace_to_remote_files_on_localhost_with_different_pwd_abs,
     changing_cwd_on_control_node, changing_cwd_on_remote_node,
     changing_cwd_on_control_node_with_local_trace,
     one_command_trace_setup, dbg_style_fetch, shell_tracing_init,
     only_one_state_for_format_handler, only_one_state_with_default_format_handler,
     only_one_state_with_initial_format_handler, run_trace_with_shortcut1,
     run_trace_with_shortcut2, run_trace_with_shortcut3, run_trace_with_shortcut4,
     cant_specify_local_and_flush, trace_sorted_by_default,disable_sorting,
     trace_resumed_after_node_restart, trace_resumed_after_node_restart_ip,
     trace_resumed_after_node_restart_wrap,
     trace_resumed_after_node_restart_wrap_mult
].

groups() -> 
    [].

init_per_suite(Config) ->
    clean_priv_dir(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


file(suite) ->
    [];
file(doc) ->
    ["Start tracing on multiple nodes, single file"];
file(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"file"),
    {ok,[Node]} =
	ttb:tracer(Node,[{file, File},
			 {handler,{fun myhandler/4, S}}]),
    {ok,[{S,[{matched,Node,_}]}]} = ttb:p(S,call),
    {ok,[OtherNode]} =
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[]} = ttb:tracer([Node,OtherNode],
				  [{file, File},
				   {handler,{fun myhandler/4, S}}]),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(filename:join(Privdir,atom_to_list(Node)++"-file")),
    ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-file")),

    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace,
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

file(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).

file_no_pi(suite) ->
    [];
file_no_pi(doc) ->
    ["Start tracing on multiple nodes, single file, no process information"];
file_no_pi(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"file"),
    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}},
				     {process_info,false}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(filename:join(Privdir,atom_to_list(Node)++"-file")),
    ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-file")),

    [{trace_ts,LocalProc,call,{?MODULE,foo,[]}, {_,_,_}},
	   end_of_trace,
	   {trace_ts,RemoteProc,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    true = is_pid(LocalProc),
    true = is_pid(RemoteProc),
    ok.

file_no_pi(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).


file_fetch(suite) ->
    [];
file_fetch(doc) ->
    ["stop with the fetch option, i.e. collect all files when ttb is stopped"];
file_fetch(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    ThisDir = filename:join(Privdir,this),
    ok = file:make_dir(ThisDir),
    OtherDir = filename:join(Privdir,other),
    ok = file:make_dir(OtherDir),
    ThisFile = filename:join(ThisDir,"file_fetch"),
    OtherFile = filename:join(OtherDir,"file_fetch"),

    %% I'm setting priv_dir as cwd, so ttb_upload directory is created there
    %% and not in any other strange place!
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Privdir),

    {ok,[Node]} =
	ttb:tracer(Node,[{file, ThisFile},
			 {handler,{fun myhandler/4, S}}]),
    {ok,[OtherNode]} =
	ttb:tracer([OtherNode],[{file, OtherFile},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    test_server:capture_start(),
    ttb:stop([return_fetch_dir]),
    test_server:capture_stop(),
    [StoreString] = test_server:capture_get(),
    UploadDir =
	lists:last(string:lexemes(lists:flatten(StoreString),"$ \n")),

    %% check that files are no longer in original directories...
    ok = check_gone(ThisDir,atom_to_list(Node)++"-file_fetch"),
    ok = check_gone(ThisDir,atom_to_list(Node)++"-file_fetch.ti"),
%    false = lists:member(TrcLog,ThisList),
%    false = lists:member(TIFile,ThisList),
    
    {ok,OtherList} = file:list_dir(OtherDir),
    false = lists:member(atom_to_list(OtherNode)++"-file_fetch",OtherList),
    false = lists:member(atom_to_list(OtherNode)++"-file_fetch.ti",
			       OtherList),
    
    %% but instead in ttb_upload directory, where they can be formatted
    ok = ttb:format(filename:join(UploadDir,
					atom_to_list(Node)++"-file_fetch")),
    ok = ttb:format(filename:join(UploadDir,
					atom_to_list(OtherNode)++"-file_fetch")),

    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace,
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    ok = file:set_cwd(Cwd),
    ok.

file_fetch(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).


wrap(suite) ->
    [];
wrap(doc) ->
    ["Start tracing on multiple nodes, wrap files"];
wrap(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"wrap"),
    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, {wrap,File,200,3}},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(filename:join(Privdir,
					atom_to_list(Node)++"-wrap.*.wrp")),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-wrap.*.wrp")),
    [{trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    %% Check that merge does not crash even if the timestamp flag is not on.
    ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-wrap.*.wrp"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-wrap.*.wrp")],[{disable_sort,true}]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

wrap(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).

wrap_merge(suite) ->
    [];
wrap_merge(doc) ->
    ["Start tracing on multiple nodes, wrap files, merge logs from both nodes"];
wrap_merge(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"wrap_merge"),
    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, {wrap,File,200,3}},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]}=ttb:p(all,[call,timestamp]),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-wrap_merge.*.wrp"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-wrap_merge.*.wrp")]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,_,call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,_,call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   end_of_trace] = flush(),
    ok.

wrap_merge(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).


wrap_merge_fetch_format(suite) ->
    [];
wrap_merge_fetch_format(doc) ->
    ["Start tracing on multiple nodes, wrap files, fetch and format at stop"];
wrap_merge_fetch_format(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"wrap_merge_fetch_format"),

    %% I'm setting priv_dir as cwd, so ttb_upload directory is created there
    %% and not in any other strange place!
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Privdir),

    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, {wrap,File,200,3}},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]}=ttb:p(all,[call,timestamp]),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([format]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   end_of_trace] = flush(),

    ok = file:set_cwd(Cwd),
    ok.

wrap_merge_fetch_format(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).

write_config1(suite) ->
    [];
write_config1(doc) ->
    ["Write config given commands"];
write_config1(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),

    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"write_config1"),
    ok = ttb:write_config(File,
				[{ttb,tracer,[[Node,OtherNode],
					      [{file, File},
					       {handler,{fun myhandler/4,S}}]]},
				 {ttb,p,[all,call]},
				 {ttb,tp,[?MODULE,foo,[]]}]),
    [_,_,_] = ttb:list_config(File),
    ok = ttb:run_config(File),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config1"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config1")]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,Other,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    case metatest(Other,OtherNode,Privdir,"-write_config1.ti") of
	{error,Reason} ->
	    timer:sleep(5000),
	    ok = ttb:format(
			 [filename:join(Privdir,
					atom_to_list(Node)++"-write_config1"),
			  filename:join(Privdir,
					atom_to_list(OtherNode)++
					"-write_config1")]),
	    io:format("\nTrying again: ~p\n",[flush()]),
	    ct:fail(Reason);
	ok ->
	    ok
    end,	    
    ok.


write_config1(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).

write_config2(suite) ->
    [];
write_config2(doc) ->
    ["Write config from history (all)"];
write_config2(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"write_config2"),
    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ok = ttb:write_config(File,all),
    ttb:stop(),
    [_,_,_] = ttb:list_config(File),
    ok = ttb:run_config(File),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config2"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config2")]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,Other,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    case metatest(Other,OtherNode,Privdir,"-write_config2.ti") of
	{error,Reason} ->
	    timer:sleep(5000),
	    ok = ttb:format(
			 [filename:join(Privdir,
					atom_to_list(Node)++"-write_config2"),
			  filename:join(Privdir,
					atom_to_list(OtherNode)++
					"-write_config2")]),
	    io:format("\nTrying again: ~p\n",[flush()]),
	    ct:fail(Reason);
	ok ->
	    ok
    end,
    ok.

write_config2(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).


write_config3(suite) ->
    [];
write_config3(doc) ->
    ["Write config from history (selected and append)"];
write_config3(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"write_config3"),
    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ok = ttb:write_config(File,[1,2]),
    ttb:stop([nofetch]),
    [_,_] = ttb:list_config(File),
    ok = ttb:run_config(File),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config3"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config3")]),
    [end_of_trace] = flush(), %foo is not traced

    ok = ttb:write_config(File,[{ttb,tp,[?MODULE,foo,[]]}],
			      [append]),
    [_,_,_] = ttb:list_config(File),
    ok = ttb:run_config(File),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config3"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config3")]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,Other,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    case metatest(Other,OtherNode,Privdir,"-write_config3.ti") of
	{error,Reason} ->
	    timer:sleep(5000),
	    ok = ttb:format(
			 [filename:join(Privdir,
					atom_to_list(Node)++"-write_config3"),
			  filename:join(Privdir,
					atom_to_list(OtherNode)++
					"-write_config3")]),
	    io:format("\nTrying again: ~p\n",[flush()]),
	    ct:fail(Reason);
	ok ->
	    ok
    end,
    ok.

write_config3(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).



history(suite) ->
    [];
history(doc) ->
    ["List history and execute entry from history"];
history(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),

    Nodes = [Node,OtherNode],
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"history"),
    StartOpts = [{file, File},
		       {handler,{fun myhandler/4, S}}],
    {ok,[_,_]} = ttb:tracer(Nodes,StartOpts),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:ctp(?MODULE,foo),
    [{1,{ttb,tracer,[Nodes,StartOpts]}},
	   {2,{ttb,p,[all,call]}},
	   {3,{ttb,tp,[?MODULE,foo,[]]}},
	   {4,{ttb,ctp,[?MODULE,foo]}}] = ttb:list_history(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ok = ttb:run_history(3),
    ?MODULE:foo(),
    ok = ttb:run_history([3,4]),
    ?MODULE:foo(),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-history"),
		  filename:join(Privdir,atom_to_list(OtherNode)++"-history")]),
    [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

history(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).


write_trace_info(suite) ->
    [];
write_trace_info(doc) ->
    ["Write trace info and give handler explicitly in format command"];
write_trace_info(Config) when is_list(Config) ->
    Node = node(),
    {ok,OtherNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"write_trace_info"),
    {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ok = ttb:write_trace_info(mytraceinfo,fun() -> node() end),
    ?MODULE:foo(),
    rpc:call(OtherNode,?MODULE,foo,[]),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-write_trace_info"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_trace_info")],
		 [{handler,{fun otherhandler/4,S}}]),
    [{{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},[Node]},
	   {{trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},[OtherNode]},
	   end_of_trace] = flush(),

    ok.

write_trace_info(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).


seq_trace(suite) ->
    [];
seq_trace(doc) ->
    ["Test sequential tracing"];
seq_trace(Config) when is_list(Config) ->
    S = self(),

    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"seq_trace"),
    {ok,[Node]} = ttb:tracer(node(),[{file,File},
				     {handler,{fun myhandler/4, S}}]),
    {ok,[{new,[{matched,Node,0}]}]} = ttb:p(new,call),
    {ok,[{matched,Node,1},{saved,1}]} =
	   ttb:tpl(?MODULE,seq,0,ttb:seq_trigger_ms(send)),

    Start = spawn(fun() -> seq() end),
    timer:sleep(300),
    ttb:stop([nofetch]),
    ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-seq_trace")]),
    [{trace_ts,StartProc,call,{?MODULE,seq,[]},{_,_,_}},
           {seq_trace,0,{send,{First, Seq0},StartProc,P1Proc,SpawnRequest1}},
           {seq_trace,0,{send,{Seq0, Seq1},P1Proc,StartProc,SpawnReply1}},
           {seq_trace,0,{send,{Seq2, Seq3},StartProc,P2Proc,SpawnRequest2}},
           {seq_trace,0,{send,{Seq3, Seq4},P2Proc,StartProc,SpawnReply2}},
	   {seq_trace,0,{send,{Seq5, Seq6},StartProc,P1Proc,{Start,P2}}},
	   {seq_trace,0,{send,{Seq6,  Seq7},P1Proc,P2Proc,{P1,Start}}},
	   {seq_trace,0,{send,{Seq7,  Last},P2Proc,StartProc,{P2,P1}}},
	   end_of_trace] = flush(),
    spawn_request = element(1, SpawnRequest1),
    SReq1 = element(2, SpawnRequest1),
    spawn_reply = element(1, SpawnReply1),
    SReq1 = element(2, SpawnReply1),
    spawn_request = element(1, SpawnRequest2),
    SReq2 = element(2, SpawnRequest2),
    spawn_reply = element(1, SpawnReply2),
    SReq2 = element(2, SpawnReply2),
    true = First < Seq0,
    true = Seq0 < Seq1,
    true = Seq1 < Seq2,
    true = Seq2 < Seq3,
    true = Seq4 < Seq5,
    true = Seq6 < Seq7,
    true = Seq7 < Last,
   %% Additional test for metatrace
    case StartProc of
	{Start,_,_} -> ok;
	Pid when is_pid(Pid) ->
	    io:format("\n\nProcinfo was pid: ~p.\n"
		      "Should have been {Pid,Name,Node}\n",
		      [Pid]),
	    io:format("Trace information file:\n~p\n",
		      [ttb:dump_ti(
			 filename:join(Privdir,
				       atom_to_list(Node)++"-seq_trace.ti"))]),
	    ct:fail("metatrace failed for startproc")
    end,
    case P1Proc of
	{P1,_,_} -> ok;
	P1 when is_pid(P1) ->
	    io:format("\n\nProcinfo was pid: ~p.\n"
		      "Should have been {Pid,Name,Node}\n",
		      [P1]),
	    io:format("Trace information file:\n~p\n",
		      [ttb:dump_ti(
			 filename:join(Privdir,
				       atom_to_list(Node)++"-seq_trace.ti"))]),
	    ct:fail("metatrace failed for P1")
    end,
    case P2Proc of
	{P2,_,_} -> ok;
	P2 when is_pid(P2) ->
	    io:format("\n\nProcinfo was pid: ~p.\n"
		      "Should have been {Pid,Name,Node}\n",
		      [P2]),
	    io:format("Trace information file:\n~p\n",
		      [ttb:dump_ti(
			 filename:join(Privdir,
				       atom_to_list(Node)++"-seq_trace.ti"))]),
	    ct:fail("metatrace failed for P2")
    end,
    ok.


diskless(suite) ->
    [];
diskless(doc) ->
    ["Start tracing on diskless remote node"];
diskless(Config) when is_list(Config) ->
    {ok,RemoteNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"diskless"),
    {ok,[RemoteNode]} =
	ttb:tracer([RemoteNode],[{file, {local, File}},
				 {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,RemoteNode,_}]}]} = ttb:p(all,call),
    {ok,[{matched,RemoteNode,1}]} = ttb:tp(?MODULE,foo,[]),

    rpc:call(RemoteNode,?MODULE,foo,[]),
    timer:sleep(5000), % needed for the IP port to flush
    ttb:stop([nofetch]),
    ok = ttb:format(filename:join(Privdir,
					atom_to_list(RemoteNode)++"-diskless")),

    [{trace_ts,{_,_,RemoteNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

diskless(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).

diskless_wrap(suite) ->
    [];
diskless_wrap(doc) ->
    ["Start tracing on diskless remote node, save to local wrapped file"];
diskless_wrap(Config) when is_list(Config) ->
    {ok,RemoteNode} = test_server:start_node(node2,slave,[]),
    c:nl(?MODULE),
    S = self(),
    Privdir=priv_dir(Config),
    File = filename:join(Privdir,"diskless"),
    {ok,[RemoteNode]} =
	ttb:tracer([RemoteNode],[{file, {local, {wrap,File,200,3}}},
				 {handler,{fun myhandler/4, S}}]),
    {ok,[{all,[{matched,RemoteNode,_}]}]} = ttb:p(all,call),
    {ok,[{matched,RemoteNode,1}]} = ttb:tp(?MODULE,foo,[]),

    rpc:call(RemoteNode,?MODULE,foo,[]),
    timer:sleep(5000), % needed for the IP port to flush
    ttb:stop([nofetch]),
    ok = ttb:format(filename:join(Privdir,
					atom_to_list(RemoteNode)++"-diskless.*.wrp")),

    [{trace_ts,{_,_,RemoteNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

diskless_wrap(cleanup,_Config) ->
    test_server:stop_node(ttb_helper:get_node(node2)).

otp_4967_1(suite) ->
    [];
otp_4967_1(doc) ->
    ["OTP-4967: clear flag"];
otp_4967_1(Config) when is_list(Config) ->
    {ok,[Node]} = ttb:tracer(),
    {ok,[{all,[{matched,Node,_}]}]} =  ttb:p(all,call),
    {ok,[{all,[{matched,Node,_}]}]} =  ttb:p(all,clear),
    stopped = ttb:stop(),
    ok.


otp_4967_2(suite) ->
    [];
otp_4967_2(doc) ->
    ["OTP-4967: Trace message sent to {Name, Node}"];
otp_4967_2(Config) when is_list(Config) ->
    io:format("1: ~p",[erlang:timestamp()]),
    Privdir = priv_dir(Config),
    io:format("2: ~p",[erlang:timestamp()]),
    File = filename:join(Privdir,"otp_4967"),
    io:format("3: ~p",[erlang:timestamp()]),
    S = self(),
    io:format("4: ~p",[erlang:timestamp()]),
    {ok,[Node]} =
	ttb:tracer(node(),[{file, File},
			   {handler,{fun myhandler/4, S}}]),

    io:format("5: ~p",[erlang:timestamp()]),
    %% Test that delayed registration of a process works.
    receive after 200 -> ok end,
    register(otp_4967,self()),
    io:format("6: ~p",[erlang:timestamp()]),
    {ok,[{S,[{matched,Node,1}]}]} =  ttb:p(self(),s),
    io:format("7: ~p",[erlang:timestamp()]),
    {otp_4967,node()} ! heihopp,
    io:format("8: ~p",[erlang:timestamp()]),
    stopped = ttb:stop([format]),
    io:format("9: ~p",[erlang:timestamp()]),
    Msgs = flush(),
    io:format("10: ~p",[erlang:timestamp()]),
    io:format("Messages received: \n~p\n",[Msgs]),
    io:format("11: ~p",[erlang:timestamp()]),
    true = lists:member(heihopp,Msgs), % the heihopp message itself
    io:format("13: ~p",[erlang:timestamp()]),
    {value,{trace_ts,_,send,heihopp,{_,otp_4967,Node},{_,_,_}}} =
	lists:keysearch(heihopp,4,Msgs), % trace trace of the heihopp message
    io:format("14: ~p",[erlang:timestamp()]),
    end_of_trace = lists:last(Msgs), % end of the trace
    ok.
    

myhandler(_Fd,Trace,_,Relay) ->
    Relay ! Trace,
    Relay.

simple_call_handler() ->
    {fun(A, {trace_ts, _, call, _, _} ,_,_) -> io:format(A, "ok.~n", []);
	(A, {drop, N}, _, _) -> io:format(A, "{drop, ~p}.", [N]);
	(_, end_of_trace, _, _) -> ok end, []}.

marking_call_handler() ->
    {fun(_, _, _, initial) -> file:write_file("HANDLER_OK", []);
	(_,_,_,_) -> ok end, initial}.

counter_call_handler() ->
    {fun(_, {trace_ts, _, call, _, _} ,_,State) -> State + 1;
	(A, end_of_trace, _, State) -> io:format(A,"~p.~n", [State]) end, 0}.

ret_caller_call_handler() ->
    {fun(A, {trace_ts, _, call, _, _, _} ,_,_) -> io:format(A, "ok.~n", []);
	(A, {trace_ts, _, return_from, _, _, _}, _, _) -> io:format(A, "ok.~n", []);
	(_, _, _, _) -> ok end, []}.

node_call_handler() ->
    {fun(A, {trace_ts, {_,_,Node}, call, _, _} ,_,_) -> io:format(A, "~p.~n", [Node]);
	(_, end_of_trace, _, _) -> ok end, []}.

otherhandler(_Fd,_,end_of_trace,Relay) ->
    Relay ! end_of_trace,
    Relay;
otherhandler(_Fd,Trace,TI,Relay) ->
    {value,{mytraceinfo,I}} = lists:keysearch(mytraceinfo,1,TI),
    Relay ! {Trace,I},
    Relay.

flush() ->
    flush([]).
flush(Acc) ->
    receive
	X ->
	    flush(Acc ++ [X])
    after 1000 ->
	    Acc
    end.

foo() ->
    %% Sync between nodes is not always exact, so here is a little timeout to 
    %% make sure traces come i correct sequence when merging.
    %% In the real world there is no way to avoid this kind of trouble
    timer:sleep(100),
    foo_called.


seq() ->
    Fun = fun() -> timer:sleep(100),
		   receive {From,To} -> To ! {self(),From} end 
	  end,
    P1 = spawn(Fun),
    P2 = spawn(Fun),
    P1 ! {self(),P2},
    receive {P2,P1} -> ok end,
    {P1,P2}.

%% Additional test for metatrace which might fail on OtherNode
metatest(Proc,Node,Privdir,Filename) ->
    case Proc of
	{_,_,Node} -> ok;
	Pid when is_pid(Pid) ->
	    io:format("\n\nProcinfo was pid: ~p.\n"
		      "Should have been {Pid,Name,Node}\n",
		      [Pid]),
	    io:format("Trace information file:\n~p\n",
		      [ttb:dump_ti(
			 filename:join(Privdir,atom_to_list(Node)++Filename))]),
%	    ct:fail("metatrace failed on "++atom_to_list(Node))
	    {error,"metatrace failed on "++atom_to_list(Node)}
    end.

check_gone(Dir,File) ->
    {ok,List} = file:list_dir(Dir),
    case lists:member(File,List) of
	      true -> 
		  timer:sleep(2000),
		  {ok,NewList} = file:list_dir(Dir),
		  case lists:member(File,NewList) of
		      true ->
			  io:format("~p: ~p~n",
				    [Dir,NewList]),
			  ct:fail(File ++ " not removed from original place");
		      false ->
			  io:format("gone after 2 sec....~n",[]),
			  ok
		  end;
	      false -> 
		  ok
	  end.

start_client_and_server() ->
    {ok,ClientNode} = test_server:start_node(client,slave,[]),
    ok = ttb_helper:c(code, add_paths, [code:get_path()]),
    {ok,ServerNode} = test_server:start_node(server,slave,[]),
    ok = ttb_helper:s(code, add_paths, [code:get_path()]),
    ttb_helper:clear(),
    {ServerNode, ClientNode}.

stop_client_and_server() ->
    ClientNode = ttb_helper:get_node(client),
    ServerNode = ttb_helper:get_node(server),
    erlang:monitor_node(ClientNode,true),
    erlang:monitor_node(ServerNode,true),
    test_server:stop_node(ClientNode),
    test_server:stop_node(ServerNode),
    wait_for_client_and_server_stop(ClientNode,ServerNode).

wait_for_client_and_server_stop(undefined,undefined) ->
    ok;
wait_for_client_and_server_stop(ClientNode,ServerNode) ->
    receive
	{nodedown,ClientNode} ->
	    erlang:monitor_node(ClientNode,false),
	    wait_for_client_and_server_stop(undefined,ServerNode);
	{nodedown,ServerNode} ->
	    erlang:monitor_node(ServerNode,false),
	    wait_for_client_and_server_stop(ClientNode,undefined)
    end.

begin_trace(ServerNode, ClientNode, Dest) ->
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, Dest}]),
    ttb:p(all, call),
    ttb:tp(server, received, []),
    ttb:tp(client, put, []),
    ttb:tp(client, get, []).

begin_trace_local(ServerNode, ClientNode, Dest) ->
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, Dest}]),
    ttb:p(all, call),
    ttb:tpl(server, received, []),
    ttb:tpl(client, put, []),
    ttb:tpl(client, get, []).

check_size(N, Dest, Output, ServerNode, ClientNode) ->
    begin_trace(ServerNode, ClientNode, Dest),
    case Dest of
        {local, _} ->
            ttb_helper:msgs_ip(N);
        _ ->
	    ttb_helper:msgs(N)
    end,
    {_, D} = ttb:stop([fetch, return_fetch_dir]),
    ttb:format(D, [{out, Output}, {handler, simple_call_handler()}]),
    {ok, Ret} = file:consult(Output),
    check_output(N+1, Ret).

check_output(Expected, Ret)
  when length(Ret) =:= Expected -> ok;
check_output(Expected, Ret) ->
    io:format("~p~n",[Ret]),
    io:format("Expected ~p got ~p ~n",[Expected, length(Ret)]),
    Expected = length(Ret).

fetch_when_no_option_given(suite) ->
    [];
fetch_when_no_option_given(doc) ->
    ["Fetch when no option given"];
fetch_when_no_option_given(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    {ok, Privdir} = file:get_cwd(),
    [] = filelib:wildcard(filename:join(Privdir,"ttb_upload_temptest*")),
    begin_trace(ServerNode, ClientNode, ?FNAME),
    ttb_helper:msgs(4),
    stopped = ttb:stop(),
    [_] = filelib:wildcard(filename:join(Privdir,"ttb_upload_temptest*")).

fetch_when_no_option_given(cleanup,_Config) ->
    stop_client_and_server().


basic_ttb_run_ip_port(suite) ->
    [];
basic_ttb_run_ip_port(doc) ->
    ["Basic ttb run ip port"];
basic_ttb_run_ip_port(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    check_size(1, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode),
    check_size(2, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode),
    check_size(10, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode).
basic_ttb_run_ip_port(cleanup,_Config) ->
    stop_client_and_server().

basic_ttb_run_file_port(suite) ->
    [];
basic_ttb_run_file_port(doc) ->
    ["Basic ttb run file port"];
basic_ttb_run_file_port(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    check_size(1, ?FNAME, ?OUTPUT, ServerNode, ClientNode),
    check_size(2, ?FNAME, ?OUTPUT, ServerNode, ClientNode),
    check_size(10, ?FNAME, ?OUTPUT, ServerNode, ClientNode).
basic_ttb_run_file_port(cleanup,_Config) ->
    stop_client_and_server().

return_fetch_dir_implies_fetch(suite) ->
    [];
return_fetch_dir_implies_fetch(doc) ->
    ["Return_fetch_dir implies fetch"];
return_fetch_dir_implies_fetch(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, ?FNAME),
    ttb_helper:msgs(2),
    {_,_} = ttb:stop([return_fetch_dir]).
return_fetch_dir_implies_fetch(cleanup,_Config) ->
    stop_client_and_server().

logfile_name_in_fetch_dir(suite) ->
    [];
logfile_name_in_fetch_dir(doc) ->
    ["Logfile name in fetch dir"];
logfile_name_in_fetch_dir(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, {local, ?FNAME}),
    {_,Dir} = ttb:stop([return_fetch_dir]),
    P1 = lists:nth(3, string:lexemes(filename:basename(Dir), "_")),
    P2 = hd(string:lexemes(P1, "-")),
    _File = P2.
logfile_name_in_fetch_dir(cleanup,_Config) ->
    stop_client_and_server().

upload_to_my_logdir(suite) ->
    [];
upload_to_my_logdir(doc) ->
    ["Upload to my logdir"];
upload_to_my_logdir(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    {stopped,_} = ttb:stop([return_fetch_dir, {fetch_dir, ?DIRNAME}]),
    true = filelib:is_file(?DIRNAME),
    [] = filelib:wildcard("ttb_upload_"++?FNAME).
upload_to_my_logdir(cleanup,_Config) ->
    stop_client_and_server().

upload_to_my_existing_logdir(suite) ->
    [];
upload_to_my_existing_logdir(doc) ->
    ["Upload to my existing logdir"];
upload_to_my_existing_logdir(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    ok = file:make_dir(?DIRNAME),
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    {error,_,_} = (catch ttb:stop([return_fetch_dir, {fetch_dir, ?DIRNAME}])),
    {stopped,_} = ttb:stop(return_fetch_dir).
upload_to_my_existing_logdir(cleanup,_Config) ->
    stop_client_and_server().

fetch_with_options_not_as_list(suite) ->
    [];
fetch_with_options_not_as_list(doc) ->
    ["Fetch with options not as list"];
fetch_with_options_not_as_list(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    {stopped, D} = ttb:stop(return_fetch_dir),
    false = filelib:is_file(?OUTPUT),
    ttb:format(D, {out, ?OUTPUT}),
    true = filelib:is_file(?OUTPUT).
fetch_with_options_not_as_list(cleanup,_Config) ->
    stop_client_and_server().

error_when_formatting_multiple_files_4393(suite) ->
    [];
error_when_formatting_multiple_files_4393(doc) ->
    ["Error when formatting multiple files"];
error_when_formatting_multiple_files_4393(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, ?FNAME),
    ttb_helper:msgs(2),
    {_, Dir} = ttb:stop(return_fetch_dir),
    Files = [filename:join(Dir, atom_to_list(ServerNode) ++ "-" ++ ?FNAME),
		   filename:join(Dir, atom_to_list(ClientNode) ++ "-" ++ ?FNAME)],
    ok = ttb:format(Files).
error_when_formatting_multiple_files_4393(cleanup,_Config) ->
    stop_client_and_server().

format_on_trace_stop(suite) ->
    [];
format_on_trace_stop(doc) ->
    ["Format on trace stop"];
format_on_trace_stop(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, {local, ?FNAME}),
    ttb_helper:msgs_ip(2),
    file:delete("HANDLER_OK"),
    {_,_} = ttb:stop([fetch, return_fetch_dir, {format, {handler, marking_call_handler()}}]),
    true = filelib:is_file("HANDLER_OK"),
    ok = file:delete("HANDLER_OK").
format_on_trace_stop(cleanup,_Config) ->
    stop_client_and_server().

%% The following three tests are for the issue "fixes fetch fail when nodes on the same host
%% have different cwd"
trace_to_remote_files_on_localhost_with_different_pwd(suite) ->
    [];
trace_to_remote_files_on_localhost_with_different_pwd(doc) ->
    ["Trace to remote files on localhost with different pwd"];
trace_to_remote_files_on_localhost_with_different_pwd(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd(".."),
    {ServerNode, ClientNode} = start_client_and_server(),
    check_size(2, ?FNAME, ?OUTPUT, ServerNode, ClientNode),
    ok = file:set_cwd(OldDir).
trace_to_remote_files_on_localhost_with_different_pwd(cleanup,_Config) ->
    stop_client_and_server().

trace_to_local_files_on_localhost_with_different_pwd(suite) ->
    [];
trace_to_local_files_on_localhost_with_different_pwd(doc) ->
    ["Trace to local files on localhost with different pwd"];
trace_to_local_files_on_localhost_with_different_pwd(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd(".."),
    {ServerNode, ClientNode} = start_client_and_server(),
    check_size(2, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode),
    ok = file:set_cwd(OldDir).
trace_to_local_files_on_localhost_with_different_pwd(cleanup,_Config) ->
    stop_client_and_server().

trace_to_remote_files_on_localhost_with_different_pwd_abs(suite) ->
    [];
trace_to_remote_files_on_localhost_with_different_pwd_abs(doc) ->
    ["Trace to remote files on localhost with different pwd abs"];
trace_to_remote_files_on_localhost_with_different_pwd_abs(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd(".."),
    {ok, Path} = file:get_cwd(),
    {ServerNode, ClientNode} = start_client_and_server(),
    File = filename:join(Path, ?FNAME),
    check_size(2, File, ?OUTPUT, ServerNode, ClientNode),
    ok = file:set_cwd(OldDir).
trace_to_remote_files_on_localhost_with_different_pwd_abs(cleanup,_Config) ->
    stop_client_and_server().

%% Trace is not affected by changes of cwd on control node or remote nodes during tracing
%% (three tests)
changing_cwd_on_control_node(suite) ->
    [];
changing_cwd_on_control_node(doc) ->
    ["Changing cwd on control node during tracing is safe"];
changing_cwd_on_control_node(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, ?FNAME),
    NumMsgs = 3,
    ttb_helper:msgs(NumMsgs),
    ok = file:set_cwd(".."),
    ttb_helper:msgs(NumMsgs),
    {_, D} = ttb:stop([fetch, return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    {ok, Ret} = file:consult(?OUTPUT),
    check_output(2*(NumMsgs + 1),Ret),
    ok = file:set_cwd(OldDir).
changing_cwd_on_control_node(cleanup,_Config) ->
    stop_client_and_server().

changing_cwd_on_control_node_with_local_trace(suite) ->
    [];
changing_cwd_on_control_node_with_local_trace(doc) ->
    ["Changing cwd on control node during local tracing is safe"];
changing_cwd_on_control_node_with_local_trace(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, {local, ?FNAME}),
    NumMsgs = 3,
    ttb_helper:msgs_ip(NumMsgs),
    ok = file:set_cwd(".."),
    ttb_helper:msgs_ip(NumMsgs),
    {_, D} = ttb:stop([fetch, return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    {ok, Ret} = file:consult(?OUTPUT),
    Expected = 2*(NumMsgs + 1),
    check_output(Expected, Ret),
    ok = file:set_cwd(OldDir).
changing_cwd_on_control_node_with_local_trace(cleanup,_Config) ->
    stop_client_and_server().

changing_cwd_on_remote_node(suite) ->
    [];
changing_cwd_on_remote_node(doc) ->
    ["Changing cwd on remote node during tracing is safe"];
changing_cwd_on_remote_node(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace(ServerNode, ClientNode, ?FNAME),
    NumMsgs = 2,
    ttb_helper:msgs(NumMsgs),
    ok = rpc:call(ClientNode, file, set_cwd, [".."]),
    ttb_helper:msgs(NumMsgs),
    {_, D} = ttb:stop([fetch, return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    {ok, Ret} = file:consult(?OUTPUT),
    check_output(2*(NumMsgs + 1),Ret).
changing_cwd_on_remote_node(cleanup,_Config) ->
    stop_client_and_server().

one_command_trace_setup(suite) ->
    [];
one_command_trace_setup(doc) ->
    ["One command trace setup"];
one_command_trace_setup(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    ttb:start_trace([ClientNode, ServerNode],
			  [{server, received, '_', []},
			   {client, put, 1, []},
			   {client, get, '_', []}],
			  {all, call},
			  [{file, ?FNAME}]),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop(return_fetch_dir),
    ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    {ok, Ret} = file:consult(?OUTPUT),
    5 = length(Ret).
one_command_trace_setup(cleanup,_Config) ->
    stop_client_and_server().

dbg_style_fetch(suite) ->
    [];
dbg_style_fetch(doc) ->
    ["Dbg style fetch"];
dbg_style_fetch(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    DirSize = length(element(2, file:list_dir("."))),
    ttb:start_trace([ClientNode, ServerNode],
			  [{server, received, '_', []},
			   {client, put, 1, []},
			   {client, get, '_', []}],
			  {all, call},
			  [{shell, only}]),
    DirSize = length(element(2, file:list_dir("."))),
    ttb_helper:msgs(2),
    DirSize = length(element(2, file:list_dir("."))),
    stopped, ttb:stop(format),
    %%+1 -> ttb_last_trace
    true = (DirSize + 1 == length(element(2, file:list_dir(".")))),
    {ok,[{all, [{matched,_,_}, {matched,_,_}]}]} =
	ttb:start_trace([ClientNode, ServerNode],
			[{server, received, '_', []},
			 {client, put, 1, []},
			 {client, get, '_', []}],
			{all, call},
			[{shell, only}]),
    ttb:stop().
dbg_style_fetch(cleanup,_Config) ->
    stop_client_and_server().

shell_tracing_init(suite) ->
    [];
shell_tracing_init(doc) ->
    ["Shell tracing init"];
shell_tracing_init(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    ttb:tracer([ClientNode, ServerNode], shell),
    ttb:stop(),
    ttb:tracer([ClientNode, ServerNode],
		     [{file, {local, ?FNAME}}, shell]),
    ttb:stop(),
    local_client_required_on_shell_tracing =
	try  ttb:tracer([ClientNode, ServerNode],[{file, ?FNAME}, shell])
	catch
	    exit:local_client_required_on_shell_tracing ->
		local_client_required_on_shell_tracing
	end.
shell_tracing_init(cleanup,_Config) ->
    stop_client_and_server().

only_one_state_for_format_handler(suite) ->
    [];
only_one_state_for_format_handler(doc) ->
    ["Only one state for format handler"];
only_one_state_for_format_handler(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_local(ServerNode, ClientNode, ?FNAME),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop([return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, counter_call_handler()}]),
    {ok, Ret} = file:consult(?OUTPUT),
    [5] = Ret.
only_one_state_for_format_handler(cleanup,_Config) ->
    stop_client_and_server().

only_one_state_with_default_format_handler(suite) ->
    [];
only_one_state_with_default_format_handler(doc) ->
    ["Only one state with default format handler"];
only_one_state_with_default_format_handler(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_local(ServerNode, ClientNode, ?FNAME),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop([return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}]),
    true = filelib:is_file(?OUTPUT).
only_one_state_with_default_format_handler(cleanup,_Config) ->
    stop_client_and_server().

only_one_state_with_initial_format_handler(suite) ->
    [];
only_one_state_with_initial_format_handler(doc) ->
    ["Only one state with initial format handler"];
only_one_state_with_initial_format_handler(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}, {handler, counter_call_handler()}]),
    ttb:p(all, call),
    ttb:tpl(server, received, []),
    ttb:tpl(client, put, []),
    ttb:tpl(client, get, []),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop([return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}]),
    {ok, Ret} = file:consult(?OUTPUT),
    [5] = Ret.
only_one_state_with_initial_format_handler(cleanup,_Config) ->
    stop_client_and_server().

run_trace_with_shortcut(Shortcut, Ret, F) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    ttb:p(all, call),
    ttb:F(client, put, Shortcut),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop([return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, ret_caller_call_handler()}]),
    {ok, Ret} =file:consult(?OUTPUT),
    stop_client_and_server().

fun_for(return) ->
    {codestr, "fun(_) -> return_trace() end"};
fun_for(msg_false) ->
    {codestr, "fun(_) -> message(false) end"}.

run_trace_with_shortcut1(suite) ->
    [];
run_trace_with_shortcut1(doc) ->
    ["Run trace with shortcut 1"];
run_trace_with_shortcut1(Config) when is_list(Config) ->
    run_trace_with_shortcut(caller, [ok,ok], tp),
    run_trace_with_shortcut(caller, [ok,ok], tpl).
run_trace_with_shortcut1(cleanup,_Config) ->
    stop_client_and_server().

run_trace_with_shortcut2(suite) ->
    [];
run_trace_with_shortcut2(doc) ->
    ["Run trace with shortcut 2"];
run_trace_with_shortcut2(Config) when is_list(Config) ->
    run_trace_with_shortcut(return, [ok,ok], tp),
    run_trace_with_shortcut(return, [ok,ok], tpl).
run_trace_with_shortcut2(cleanup,_Config) ->
    stop_client_and_server().

run_trace_with_shortcut3(suite) ->
    [];
run_trace_with_shortcut3(doc) ->
    ["Run trace with shortcut 3"];
run_trace_with_shortcut3(Config) when is_list(Config) ->
    run_trace_with_shortcut(fun_for(return), [ok,ok], tp),
    run_trace_with_shortcut(fun_for(return), [ok,ok], tpl).
run_trace_with_shortcut3(cleanup,_Config) ->
    stop_client_and_server().

run_trace_with_shortcut4(suite) ->
    [];
run_trace_with_shortcut4(doc) ->
    ["Run trace with shortcut 4"];
run_trace_with_shortcut4(Config) when is_list(Config) ->
    run_trace_with_shortcut(fun_for(msg_false), [], tp),
    run_trace_with_shortcut(fun_for(msg_false), [], tpl).
run_trace_with_shortcut4(cleanup,_Config) ->
    stop_client_and_server().

cant_specify_local_and_flush(suite) ->
    [];
cant_specify_local_and_flush(doc) ->
    ["Can't specify local and flush"];
cant_specify_local_and_flush(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    flush_unsupported_with_ip_trace_port =
	try ttb:tracer([ServerNode, ClientNode],
		       [{flush, 1000}, {file, {local, ?FNAME}}])
	catch
	    exit:flush_unsupported_with_ip_trace_port ->
		flush_unsupported_with_ip_trace_port
	end.
cant_specify_local_and_flush(cleanup,_Config) ->
    stop_client_and_server().

trace_sorted_by_default(suite) ->
    [];
trace_sorted_by_default(doc) ->
    ["Trace sorted by default"];
trace_sorted_by_default(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_local(ServerNode, ClientNode, ?FILE),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop([return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, node_call_handler()}, {disable_sort, false}]),
    {ok, Ret} = file:consult(?OUTPUT),
    [ClientNode,ServerNode,ClientNode,ServerNode,ServerNode] = Ret.
trace_sorted_by_default(cleanup,_Config) ->
    stop_client_and_server().

disable_sorting(suite) ->
    [];
disable_sorting(doc) ->
    ["Disable sorting"];
disable_sorting(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_local(ServerNode, ClientNode, ?FILE),
    ttb_helper:msgs(2),
    {_, D} = ttb:stop([return_fetch_dir]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, node_call_handler()}, {disable_sort, true}]),
    {ok, Ret} = file:consult(?OUTPUT),
    [ClientNode,ClientNode,ServerNode,ServerNode,ServerNode] = Ret.
disable_sorting(cleanup,_Config) ->
    stop_client_and_server().

%% -----------------------------------------------------------------------------
%% tests for autoresume of tracing
%% -----------------------------------------------------------------------------

trace_resumed_after_node_restart(suite) ->
    [];
trace_resumed_after_node_restart(doc) ->
    ["Test trace resumed after node restart, trace to files on remote node."];
trace_resumed_after_node_restart(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_with_resume(ServerNode, ClientNode, ?FNAME),
    logic(2,6,file).
trace_resumed_after_node_restart(cleanup,_Config) ->
    stop_client_and_server().

trace_resumed_after_node_restart_ip(suite) ->
    [];
trace_resumed_after_node_restart_ip(doc) ->
    ["Test trace resumed after node restart, trace via tcp/ip to local node."];
trace_resumed_after_node_restart_ip(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_with_resume(ServerNode, ClientNode, {local, ?FNAME}),
    logic(2,6,local).
trace_resumed_after_node_restart_ip(cleanup,_Config) ->
    stop_client_and_server().

trace_resumed_after_node_restart_wrap(suite) ->
    [];
trace_resumed_after_node_restart_wrap(doc) ->
    ["Test trace resumed after node restart, wrap option."];
trace_resumed_after_node_restart_wrap(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_with_resume(ServerNode, ClientNode, {wrap, ?FNAME, 10, 4}),
    logic(1,4,file).
trace_resumed_after_node_restart_wrap(cleanup,_Config) ->
    stop_client_and_server().

trace_resumed_after_node_restart_wrap_mult(suite) ->
    [];
trace_resumed_after_node_restart_wrap_mult(doc) ->
    ["Test trace resumed after node restart, wrap option, multiple files."];
trace_resumed_after_node_restart_wrap_mult(Config) when is_list(Config) ->
    {ServerNode, ClientNode} = start_client_and_server(),
    begin_trace_with_resume(ServerNode, ClientNode, {wrap, ?FNAME, 10, 4}),
    logic(20,8,file).
trace_resumed_after_node_restart_wrap_mult(cleanup,_Config) ->
    stop_client_and_server().

logic(N, M, TracingType) ->
    helper_msgs(N, TracingType),
    test_server:stop_node(ttb_helper:get_node(client)),
    timer:sleep(2500),
    {ok,_ClientNode} = test_server:start_node(client,slave,[]),
    ct:log("client started",[]),
    ok = ttb_helper:c(code, add_paths, [code:get_path()]),
    ct:log("paths added",[]),
    ttb_helper:c(client, init, []),
    ct:log("client initiated",[]),
    helper_msgs(N, TracingType),
    ct:log("helper msgs sent and flushed",[]),
    {_, D} = ttb:stop([return_fetch_dir]),
    ct:log("stopped ~p",[D]),
    ttb:format(D, [{out, ?OUTPUT}, {handler, ret_caller_call_handler2()}]),
    ct:log("formatted ~p",[{D,?OUTPUT}]),
    {ok, Ret} = file:consult(?OUTPUT),
    ct:log("consulted: ~p",[Ret]),
    check_output(M,Ret).

begin_trace_with_resume(ServerNode, ClientNode, Dest) ->
    {ok, _} = ttb:tracer([ServerNode,ClientNode], [{file, Dest}, resume]),
    ttb:p(all, [call, timestamp]),
    ttb:tp(server, received, []),
    ttb:tp(client, put, []),
    ttb:tp(client, get, []).

ret_caller_call_handler2() ->
    {fun(A, {trace_ts, _, call, _, _} ,_,_) -> io:format(A, "ok.~n", []);
	(_, _, _, _) -> ok end, []}.

helper_msgs(N, TracingType) ->
    case TracingType of
        local ->
            ttb_helper:msgs_ip(N);
        _ ->
            ttb_helper:msgs(N)
    end.

priv_dir(Conf) ->
    %% Due to problem with long paths on windows => creating a new
    %% priv_dir under data_dir
    Dir = filename:absname(filename:join(?config(data_dir, Conf),priv_dir)),
    filelib:ensure_dir(filename:join(Dir,"*")),
    Dir.

clean_priv_dir(Config) ->
    PrivDir = priv_dir(Config),
    case filelib:is_dir(PrivDir) of
        true -> rm(PrivDir);
        false -> ok
    end.

rm(This) ->
    case filelib:is_dir(This) of
        true ->
            {ok,Files} = file:list_dir(This),
            [rm(filename:join(This,F)) || F <- Files],
	    file:del_dir(This);
        false ->
	    file:delete(This)
    end.
