%%
%% %CopyrightBegin%
%%
%%
%% Copyright Ericsson AB 2002-2018. All Rights Reserved.
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

-define(default_timeout, ?t:minutes(1)).
-define(OUTPUT, "handler_output").
-define(FNAME, "temptest").
-define(DIRNAME, "ddtemp").

init_per_testcase(Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
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
    ?t:timetrap_cancel(Dog),
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
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"file"),
    ?line {ok,[Node]} =
	ttb:tracer(Node,[{file, File},
			 {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{S,[{matched,Node,_}]}]} = ttb:p(S,call),
    ?line {ok,[OtherNode]} = 
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[]} = ttb:tracer([Node,OtherNode],
				  [{file, File},
				   {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(filename:join(Privdir,atom_to_list(Node)++"-file")),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-file")),

    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace,
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

file(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).

file_no_pi(suite) ->
    [];
file_no_pi(doc) ->
    ["Start tracing on multiple nodes, single file, no process information"];
file_no_pi(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"file"),
    ?line {ok,[_,_]} =
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}},
				     {process_info,false}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(filename:join(Privdir,atom_to_list(Node)++"-file")),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-file")),

    ?line [{trace_ts,LocalProc,call,{?MODULE,foo,[]}, {_,_,_}},
	   end_of_trace,
	   {trace_ts,RemoteProc,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ?line true = is_pid(LocalProc),
    ?line true = is_pid(RemoteProc),
    ok.

file_no_pi(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).


file_fetch(suite) ->
    [];
file_fetch(doc) ->
    ["stop with the fetch option, i.e. collect all files when ttb is stopped"];
file_fetch(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line ThisDir = filename:join(Privdir,this),
    ?line ok = file:make_dir(ThisDir),
    ?line OtherDir = filename:join(Privdir,other),
    ?line ok = file:make_dir(OtherDir),
    ?line ThisFile = filename:join(ThisDir,"file_fetch"),
    ?line OtherFile = filename:join(OtherDir,"file_fetch"),

    %% I'm setting priv_dir as cwd, so ttb_upload directory is created there
    %% and not in any other strange place!
    ?line {ok,Cwd} = file:get_cwd(),
    ?line ok = file:set_cwd(Privdir),

    ?line {ok,[Node]} =
	ttb:tracer(Node,[{file, ThisFile},
			 {handler,{fun myhandler/4, S}}]),
    ?line {ok,[OtherNode]} = 
	ttb:tracer([OtherNode],[{file, OtherFile},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?t:capture_start(),
    ?line ttb:stop([return_fetch_dir]),
    ?line ?t:capture_stop(),
    ?line [StoreString] = ?t:capture_get(),
    ?line UploadDir =
	lists:last(string:lexemes(lists:flatten(StoreString),"$ \n")),

    %% check that files are no longer in original directories...
    ?line ok = check_gone(ThisDir,atom_to_list(Node)++"-file_fetch"),
    ?line ok = check_gone(ThisDir,atom_to_list(Node)++"-file_fetch.ti"),
%    ?line false = lists:member(TrcLog,ThisList),
%    ?line false = lists:member(TIFile,ThisList),
    
    ?line {ok,OtherList} = file:list_dir(OtherDir),
    ?line false = lists:member(atom_to_list(OtherNode)++"-file_fetch",OtherList),
    ?line false = lists:member(atom_to_list(OtherNode)++"-file_fetch.ti",
			       OtherList),
    
    %% but instead in ttb_upload directory, where they can be formatted
    ?line ok = ttb:format(filename:join(UploadDir,
					atom_to_list(Node)++"-file_fetch")),
    ?line ok = ttb:format(filename:join(UploadDir,
					atom_to_list(OtherNode)++"-file_fetch")),

    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace,
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    ?line ok = file:set_cwd(Cwd),
    ok.

file_fetch(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).


wrap(suite) ->
    [];
wrap(doc) ->
    ["Start tracing on multiple nodes, wrap files"];
wrap(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"wrap"),
    ?line {ok,[_,_]} = 
	ttb:tracer([Node,OtherNode],[{file, {wrap,File,200,3}},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(Node)++"-wrap.*.wrp")),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-wrap.*.wrp")),
    ?line [{trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    %% Check that merge does not crash even if the timestamp flag is not on.
    ?line ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-wrap.*.wrp"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-wrap.*.wrp")],[{disable_sort,true}]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

wrap(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).

wrap_merge(suite) ->
    [];
wrap_merge(doc) ->
    ["Start tracing on multiple nodes, wrap files, merge logs from both nodes"];
wrap_merge(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"wrap_merge"),
    ?line {ok,[_,_]} = 
	ttb:tracer([Node,OtherNode],[{file, {wrap,File,200,3}},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]}=ttb:p(all,[call,timestamp]),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-wrap_merge.*.wrp"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-wrap_merge.*.wrp")]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,_,call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,_,call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   end_of_trace] = flush(),
    ok.

wrap_merge(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).


wrap_merge_fetch_format(suite) ->
    [];
wrap_merge_fetch_format(doc) ->
    ["Start tracing on multiple nodes, wrap files, fetch and format at stop"];
wrap_merge_fetch_format(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"wrap_merge_fetch_format"),

    %% I'm setting priv_dir as cwd, so ttb_upload directory is created there
    %% and not in any other strange place!
    ?line {ok,Cwd} = file:get_cwd(),
    ?line ok = file:set_cwd(Privdir),

    ?line {ok,[_,_]} = 
	ttb:tracer([Node,OtherNode],[{file, {wrap,File,200,3}},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]}=ttb:p(all,[call,timestamp]),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([format]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   end_of_trace] = flush(),

    ?line ok = file:set_cwd(Cwd),
    ok.

wrap_merge_fetch_format(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).

write_config1(suite) ->
    [];
write_config1(doc) ->
    ["Write config given commands"];
write_config1(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),

    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"write_config1"),
    ?line ok = ttb:write_config(File,
				[{ttb,tracer,[[Node,OtherNode],
					      [{file, File},
					       {handler,{fun myhandler/4,S}}]]},
				 {ttb,p,[all,call]},
				 {ttb,tp,[?MODULE,foo,[]]}]),
    ?line [_,_,_] = ttb:list_config(File),
    ?line ok = ttb:run_config(File),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config1"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config1")]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,Other,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    case metatest(Other,OtherNode,Privdir,"-write_config1.ti") of
	{error,Reason} ->
	    timer:sleep(5000),
	    ?line ok = ttb:format(
			 [filename:join(Privdir,
					atom_to_list(Node)++"-write_config1"),
			  filename:join(Privdir,
					atom_to_list(OtherNode)++
					"-write_config1")]),
	    ?line io:format("\nTrying again: ~p\n",[flush()]),
	    ?line ?t:fail(Reason);
	ok ->
	    ok
    end,	    
    ok.


write_config1(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).

write_config2(suite) ->
    [];
write_config2(doc) ->
    ["Write config from history (all)"];
write_config2(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"write_config2"),
    ?line {ok,[_,_]} = 
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ok = ttb:write_config(File,all),
    ?line ttb:stop(),
    ?line [_,_,_] = ttb:list_config(File),
    ?line ok = ttb:run_config(File),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config2"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config2")]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,Other,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    case metatest(Other,OtherNode,Privdir,"-write_config2.ti") of
	{error,Reason} ->
	    timer:sleep(5000),
	    ?line ok = ttb:format(
			 [filename:join(Privdir,
					atom_to_list(Node)++"-write_config2"),
			  filename:join(Privdir,
					atom_to_list(OtherNode)++
					"-write_config2")]),
	    ?line io:format("\nTrying again: ~p\n",[flush()]),
	    ?line ?t:fail(Reason);
	ok ->
	    ok
    end,
    ok.

write_config2(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).


write_config3(suite) ->
    [];
write_config3(doc) ->
    ["Write config from history (selected and append)"];
write_config3(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"write_config3"),
    ?line {ok,[_,_]} = 
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ok = ttb:write_config(File,[1,2]),
    ?line ttb:stop([nofetch]),
    ?line [_,_] = ttb:list_config(File),
    ?line ok = ttb:run_config(File),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config3"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config3")]),
    ?line [end_of_trace] = flush(), %foo is not traced

    ?line ok = ttb:write_config(File,[{ttb,tp,[?MODULE,foo,[]]}],
			      [append]),
    ?line [_,_,_] = ttb:list_config(File),
    ?line ok = ttb:run_config(File),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,
				atom_to_list(Node)++"-write_config3"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_config3")]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   {trace_ts,Other,call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),

    case metatest(Other,OtherNode,Privdir,"-write_config3.ti") of
	{error,Reason} ->
	    timer:sleep(5000),
	    ?line ok = ttb:format(
			 [filename:join(Privdir,
					atom_to_list(Node)++"-write_config3"),
			  filename:join(Privdir,
					atom_to_list(OtherNode)++
					"-write_config3")]),
	    ?line io:format("\nTrying again: ~p\n",[flush()]),
	    ?line ?t:fail(Reason);
	ok ->
	    ok
    end,
    ok.

write_config3(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).



history(suite) ->
    [];
history(doc) ->
    ["List history and execute entry from history"];
history(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),

    ?line Nodes = [Node,OtherNode],
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"history"),
    ?line StartOpts = [{file, File},
		       {handler,{fun myhandler/4, S}}],
    ?line {ok,[_,_]} = ttb:tracer(Nodes,StartOpts),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:ctp(?MODULE,foo),
    ?line [{1,{ttb,tracer,[Nodes,StartOpts]}},
	   {2,{ttb,p,[all,call]}},
	   {3,{ttb,tp,[?MODULE,foo,[]]}},
	   {4,{ttb,ctp,[?MODULE,foo]}}] = ttb:list_history(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ok = ttb:run_history(3),
    ?line ?MODULE:foo(),
    ?line ok = ttb:run_history([3,4]),
    ?line ?MODULE:foo(),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-history"),
		  filename:join(Privdir,atom_to_list(OtherNode)++"-history")]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

history(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).


write_trace_info(suite) ->
    [];
write_trace_info(doc) ->
    ["Write trace info and give handler explicitly in format command"];
write_trace_info(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"write_trace_info"),
    ?line {ok,[_,_]} = 
	ttb:tracer([Node,OtherNode],[{file, File},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,_,_},{matched,_,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,_,1},{matched,_,1}]} = ttb:tp(?MODULE,foo,[]),
    ?line ok = ttb:write_trace_info(mytraceinfo,fun() -> node() end),
    ?line ?MODULE:foo(),
    ?line rpc:call(OtherNode,?MODULE,foo,[]),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-write_trace_info"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_trace_info")],
		 [{handler,{fun otherhandler/4,S}}]),
    ?line [{{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},[Node]},
	   {{trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},[OtherNode]},
	   end_of_trace] = flush(),

    ok.

write_trace_info(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).


seq_trace(suite) ->
    [];
seq_trace(doc) ->
    ["Test sequential tracing"];
seq_trace(Config) when is_list(Config) ->
    ?line S = self(),

    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"seq_trace"),
    ?line {ok,[Node]} = ttb:tracer(node(),[{file,File},
				     {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{new,[{matched,Node,0}]}]} = ttb:p(new,call),
    ?line {ok,[{matched,Node,1},{saved,1}]} = 
	   ttb:tpl(?MODULE,seq,0,ttb:seq_trigger_ms(send)),

    ?line Start = spawn(fun() -> seq() end),
    ?line timer:sleep(300),
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-seq_trace")]),
    ?line [{trace_ts,StartProc,call,{?MODULE,seq,[]},{_,_,_}},
	   {seq_trace,0,{send,{0,1},StartProc,P1Proc,{Start,P2}}},
	   {seq_trace,0,{send,{1,2},P1Proc,P2Proc,{P1,Start}}},
	   {seq_trace,0,{send,{2,3},P2Proc,StartProc,{P2,P1}}},
	   end_of_trace] = flush(),

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
	    ?t:fail("metatrace failed for startproc")
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
	    ?t:fail("metatrace failed for P1")
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
	    ?t:fail("metatrace failed for P2")
    end,
    ok.


diskless(suite) ->
    [];
diskless(doc) ->
    ["Start tracing on diskless remote node"];
diskless(Config) when is_list(Config) ->
    ?line {ok,RemoteNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"diskless"),
    ?line {ok,[RemoteNode]} = 
	ttb:tracer([RemoteNode],[{file, {local, File}},
				 {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,RemoteNode,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,RemoteNode,1}]} = ttb:tp(?MODULE,foo,[]),

    ?line rpc:call(RemoteNode,?MODULE,foo,[]),
    ?line timer:sleep(500), % needed for the IP port to flush
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(RemoteNode)++"-diskless")),

    ?line [{trace_ts,{_,_,RemoteNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

diskless(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).

diskless_wrap(suite) ->
    [];
diskless_wrap(doc) ->
    ["Start tracing on diskless remote node, save to local wrapped file"];
diskless_wrap(Config) when is_list(Config) ->
    ?line {ok,RemoteNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=priv_dir(Config),
    ?line File = filename:join(Privdir,"diskless"),
    ?line {ok,[RemoteNode]} =
	ttb:tracer([RemoteNode],[{file, {local, {wrap,File,200,3}}},
				 {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,RemoteNode,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,RemoteNode,1}]} = ttb:tp(?MODULE,foo,[]),

    ?line rpc:call(RemoteNode,?MODULE,foo,[]),
    ?line timer:sleep(500), % needed for the IP port to flush
    ?line ttb:stop([nofetch]),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(RemoteNode)++"-diskless.*.wrp")),

    ?line [{trace_ts,{_,_,RemoteNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

diskless_wrap(cleanup,_Config) ->
    ?line ?t:stop_node(ttb_helper:get_node(node2)).

otp_4967_1(suite) ->
    [];
otp_4967_1(doc) ->
    ["OTP-4967: clear flag"];
otp_4967_1(Config) when is_list(Config) ->
    ?line {ok,[Node]} = ttb:tracer(),
    ?line {ok,[{all,[{matched,Node,_}]}]} =  ttb:p(all,call),
    ?line {ok,[{all,[{matched,Node,_}]}]} =  ttb:p(all,clear),
    ?line stopped = ttb:stop(),
    ok.


otp_4967_2(suite) ->
    [];
otp_4967_2(doc) ->
    ["OTP-4967: Trace message sent to {Name, Node}"];
otp_4967_2(Config) when is_list(Config) ->
    io:format("1: ~p",[erlang:timestamp()]),
    ?line Privdir = priv_dir(Config),
    io:format("2: ~p",[erlang:timestamp()]),
    ?line File = filename:join(Privdir,"otp_4967"),
    io:format("3: ~p",[erlang:timestamp()]),
    ?line S = self(),
    io:format("4: ~p",[erlang:timestamp()]),
    ?line {ok,[Node]} =
	ttb:tracer(node(),[{file, File},
			   {handler,{fun myhandler/4, S}}]),

    io:format("5: ~p",[erlang:timestamp()]),
    %% Test that delayed registration of a process works.
    receive after 200 -> ok end,
    ?line register(otp_4967,self()),
    io:format("6: ~p",[erlang:timestamp()]),
    ?line {ok,[{S,[{matched,Node,1}]}]} =  ttb:p(self(),s),
    io:format("7: ~p",[erlang:timestamp()]),
    ?line {otp_4967,node()} ! heihopp,
    io:format("8: ~p",[erlang:timestamp()]),
    ?line stopped = ttb:stop([format]),
    io:format("9: ~p",[erlang:timestamp()]),
    ?line Msgs = flush(),
    io:format("10: ~p",[erlang:timestamp()]),
    ?line io:format("Messages received: \n~p\n",[Msgs]),
    io:format("11: ~p",[erlang:timestamp()]),
    ?line true = lists:member(heihopp,Msgs), % the heihopp message itself
    io:format("13: ~p",[erlang:timestamp()]),
    ?line {value,{trace_ts,_,send,heihopp,{_,otp_4967,Node},{_,_,_}}} =
	lists:keysearch(heihopp,4,Msgs), % trace trace of the heihopp message
    io:format("14: ~p",[erlang:timestamp()]),
    ?line end_of_trace = lists:last(Msgs), % end of the trace
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
    %% Sync between nodes is not always exact, so here is a litle timeout to 
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
%	    ?t:fail("metatrace failed on "++atom_to_list(Node))
	    {error,"metatrace failed on "++atom_to_list(Node)}
    end.

check_gone(Dir,File) ->
    ?line {ok,List} = file:list_dir(Dir),
    ?line case lists:member(File,List) of
	      true -> 
		  timer:sleep(2000),
		  {ok,NewList} = file:list_dir(Dir),
		  case lists:member(File,NewList) of
		      true ->
			  io:format("~p: ~p~n",
				    [Dir,NewList]),
			  ?t:fail(File ++ " not removed from original place");
		      false ->
			  io:format("gone after 2 sec....~n",[]),
			  ok
		  end;
	      false -> 
		  ok
	  end.

start_client_and_server() ->
    ?line {ok,ClientNode} = ?t:start_node(client,slave,[]),
    ?line ok = ttb_helper:c(code, add_paths, [code:get_path()]),
    ?line {ok,ServerNode} = ?t:start_node(server,slave,[]),
    ?line ok = ttb_helper:s(code, add_paths, [code:get_path()]),
    ?line ttb_helper:clear(),
    {ServerNode, ClientNode}.

stop_client_and_server() ->
    ClientNode = ttb_helper:get_node(client),
    ServerNode = ttb_helper:get_node(server),
    erlang:monitor_node(ClientNode,true),
    erlang:monitor_node(ServerNode,true),
    ?line ?t:stop_node(ClientNode),
    ?line ?t:stop_node(ServerNode),
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
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, Dest}]),
    ?line ttb:p(all, call),
    ?line ttb:tp(server, received, []),
    ?line ttb:tp(client, put, []),
    ?line ttb:tp(client, get, []).

begin_trace_local(ServerNode, ClientNode, Dest) ->
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, Dest}]),
    ?line ttb:p(all, call),
    ?line ttb:tpl(server, received, []),
    ?line ttb:tpl(client, put, []),
    ?line ttb:tpl(client, get, []).

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
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line {ok, Privdir} = file:get_cwd(),
    ?line [] = filelib:wildcard(filename:join(Privdir,"ttb_upload_temptest*")),
    begin_trace(ServerNode, ClientNode, ?FNAME),
    ?line ttb_helper:msgs(4),
    ?line stopped = ttb:stop(),
    ?line [_] = filelib:wildcard(filename:join(Privdir,"ttb_upload_temptest*")).

fetch_when_no_option_given(cleanup,_Config) ->
    ?line stop_client_and_server().


basic_ttb_run_ip_port(suite) ->
    [];
basic_ttb_run_ip_port(doc) ->
    ["Basic ttb run ip port"];
basic_ttb_run_ip_port(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line check_size(1, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode),
    ?line check_size(2, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode),
    ?line check_size(10, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode).
basic_ttb_run_ip_port(cleanup,_Config) ->
    ?line stop_client_and_server().

basic_ttb_run_file_port(suite) ->
    [];
basic_ttb_run_file_port(doc) ->
    ["Basic ttb run file port"];
basic_ttb_run_file_port(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line check_size(1, ?FNAME, ?OUTPUT, ServerNode, ClientNode),
    ?line check_size(2, ?FNAME, ?OUTPUT, ServerNode, ClientNode),
    ?line check_size(10, ?FNAME, ?OUTPUT, ServerNode, ClientNode).
basic_ttb_run_file_port(cleanup,_Config) ->
    ?line stop_client_and_server().

return_fetch_dir_implies_fetch(suite) ->
    [];
return_fetch_dir_implies_fetch(doc) ->
    ["Return_fetch_dir implies fetch"];
return_fetch_dir_implies_fetch(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace(ServerNode, ClientNode, ?FNAME),
    ?line ttb_helper:msgs(2),
    ?line {_,_} = ttb:stop([return_fetch_dir]).
return_fetch_dir_implies_fetch(cleanup,_Config) ->
    ?line stop_client_and_server().

logfile_name_in_fetch_dir(suite) ->
    [];
logfile_name_in_fetch_dir(doc) ->
    ["Logfile name in fetch dir"];
logfile_name_in_fetch_dir(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace(ServerNode, ClientNode, {local, ?FNAME}),
    ?line {_,Dir} = ttb:stop([return_fetch_dir]),
    ?line P1 = lists:nth(3, string:lexemes(filename:basename(Dir), "_")),
    ?line P2 = hd(string:lexemes(P1, "-")),
    ?line _File = P2.
logfile_name_in_fetch_dir(cleanup,_Config) ->
    ?line stop_client_and_server().

upload_to_my_logdir(suite) ->
    [];
upload_to_my_logdir(doc) ->
    ["Upload to my logdir"];
upload_to_my_logdir(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    ?line {stopped,_} = ttb:stop([return_fetch_dir, {fetch_dir, ?DIRNAME}]),
    ?line true = filelib:is_file(?DIRNAME),
    ?line [] = filelib:wildcard("ttb_upload_"++?FNAME).
upload_to_my_logdir(cleanup,_Config) ->
    ?line stop_client_and_server().

upload_to_my_existing_logdir(suite) ->
    [];
upload_to_my_existing_logdir(doc) ->
    ["Upload to my existing logdir"];
upload_to_my_existing_logdir(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line ok = file:make_dir(?DIRNAME),
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    ?line {error,_,_} = (catch ttb:stop([return_fetch_dir, {fetch_dir, ?DIRNAME}])),
    ?line {stopped,_} = ttb:stop(return_fetch_dir).
upload_to_my_existing_logdir(cleanup,_Config) ->
    ?line stop_client_and_server().

fetch_with_options_not_as_list(suite) ->
    [];
fetch_with_options_not_as_list(doc) ->
    ["Fetch with options not as list"];
fetch_with_options_not_as_list(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    ?line {stopped, D} = ttb:stop(return_fetch_dir),
    ?line false = filelib:is_file(?OUTPUT),
    ?line ttb:format(D, {out, ?OUTPUT}),
    ?line true = filelib:is_file(?OUTPUT).
fetch_with_options_not_as_list(cleanup,_Config) ->
    ?line stop_client_and_server().

error_when_formatting_multiple_files_4393(suite) ->
    [];
error_when_formatting_multiple_files_4393(doc) ->
    ["Error when formatting multiple files"];
error_when_formatting_multiple_files_4393(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace(ServerNode, ClientNode, ?FNAME),
    ?line ttb_helper:msgs(2),
    ?line {_, Dir} = ttb:stop(return_fetch_dir),
    ?line Files = [filename:join(Dir, atom_to_list(ServerNode) ++ "-" ++ ?FNAME),
		   filename:join(Dir, atom_to_list(ClientNode) ++ "-" ++ ?FNAME)],
    ?line ok = ttb:format(Files).
error_when_formatting_multiple_files_4393(cleanup,_Config) ->
    ?line stop_client_and_server().

format_on_trace_stop(suite) ->
    [];
format_on_trace_stop(doc) ->
    ["Format on trace stop"];
format_on_trace_stop(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace(ServerNode, ClientNode, {local, ?FNAME}),
    ?line ttb_helper:msgs_ip(2),
    ?line file:delete("HANDLER_OK"),
    ?line {_,_} = ttb:stop([fetch, return_fetch_dir, {format, {handler, marking_call_handler()}}]),
    ?line true = filelib:is_file("HANDLER_OK"),
    ?line ok = file:delete("HANDLER_OK").
format_on_trace_stop(cleanup,_Config) ->
    ?line stop_client_and_server().

%% The following three tests are for the issue "fixes fetch fail when nodes on the same host
%% have different cwd"
trace_to_remote_files_on_localhost_with_different_pwd(suite) ->
    [];
trace_to_remote_files_on_localhost_with_different_pwd(doc) ->
    ["Trace to remote files on localhost with different pwd"];
trace_to_remote_files_on_localhost_with_different_pwd(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line ok = file:set_cwd(".."),
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line check_size(2, ?FNAME, ?OUTPUT, ServerNode, ClientNode),
    ?line ok = file:set_cwd(OldDir).
trace_to_remote_files_on_localhost_with_different_pwd(cleanup,_Config) ->
    ?line stop_client_and_server().

trace_to_local_files_on_localhost_with_different_pwd(suite) ->
    [];
trace_to_local_files_on_localhost_with_different_pwd(doc) ->
    ["Trace to local files on localhost with different pwd"];
trace_to_local_files_on_localhost_with_different_pwd(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line ok = file:set_cwd(".."),
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line check_size(2, {local, ?FNAME}, ?OUTPUT, ServerNode, ClientNode),
    ?line ok = file:set_cwd(OldDir).
trace_to_local_files_on_localhost_with_different_pwd(cleanup,_Config) ->
    ?line stop_client_and_server().

trace_to_remote_files_on_localhost_with_different_pwd_abs(suite) ->
    [];
trace_to_remote_files_on_localhost_with_different_pwd_abs(doc) ->
    ["Trace to remote files on localhost with different pwd abs"];
trace_to_remote_files_on_localhost_with_different_pwd_abs(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line ok = file:set_cwd(".."),
    ?line {ok, Path} = file:get_cwd(),
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line File = filename:join(Path, ?FNAME),
    ?line check_size(2, File, ?OUTPUT, ServerNode, ClientNode),
    ?line ok = file:set_cwd(OldDir).
trace_to_remote_files_on_localhost_with_different_pwd_abs(cleanup,_Config) ->
    ?line stop_client_and_server().

%% Trace is not affected by changes of cwd on control node or remote nodes during tracing
%% (three tests)
changing_cwd_on_control_node(suite) ->
    [];
changing_cwd_on_control_node(doc) ->
    ["Changing cwd on control node during tracing is safe"];
changing_cwd_on_control_node(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace(ServerNode, ClientNode, ?FNAME),
    ?line NumMsgs = 3,
    ?line ttb_helper:msgs(NumMsgs),
    ?line ok = file:set_cwd(".."),
    ?line ttb_helper:msgs(NumMsgs),
    ?line {_, D} = ttb:stop([fetch, return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    ?line {ok, Ret} = file:consult(?OUTPUT),
    check_output(2*(NumMsgs + 1),Ret),
    ok = file:set_cwd(OldDir).
changing_cwd_on_control_node(cleanup,_Config) ->
    ?line stop_client_and_server().

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
    ?line stop_client_and_server().

changing_cwd_on_remote_node(suite) ->
    [];
changing_cwd_on_remote_node(doc) ->
    ["Changing cwd on remote node during tracing is safe"];
changing_cwd_on_remote_node(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace(ServerNode, ClientNode, ?FNAME),
    ?line NumMsgs = 2,
    ?line ttb_helper:msgs(NumMsgs),
    ?line ok = rpc:call(ClientNode, file, set_cwd, [".."]),
    ?line ttb_helper:msgs(NumMsgs),
    ?line {_, D} = ttb:stop([fetch, return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    ?line {ok, Ret} = file:consult(?OUTPUT),
    check_output(2*(NumMsgs + 1),Ret).
changing_cwd_on_remote_node(cleanup,_Config) ->
    ?line stop_client_and_server().

one_command_trace_setup(suite) ->
    [];
one_command_trace_setup(doc) ->
    ["One command trace setup"];
one_command_trace_setup(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line ttb:start_trace([ClientNode, ServerNode],
			  [{server, received, '_', []},
			   {client, put, 1, []},
			   {client, get, '_', []}],
			  {all, call},
			  [{file, ?FNAME}]),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop(return_fetch_dir),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, simple_call_handler()}]),
    ?line {ok, Ret} = file:consult(?OUTPUT),
    ?line 5 = length(Ret).
one_command_trace_setup(cleanup,_Config) ->
    ?line stop_client_and_server().

dbg_style_fetch(suite) ->
    [];
dbg_style_fetch(doc) ->
    ["Dbg style fetch"];
dbg_style_fetch(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line DirSize = length(element(2, file:list_dir("."))),
    ?line ttb:start_trace([ClientNode, ServerNode],
			  [{server, received, '_', []},
			   {client, put, 1, []},
			   {client, get, '_', []}],
			  {all, call},
			  [{shell, only}]),
    ?line DirSize = length(element(2, file:list_dir("."))),
    ?line ttb_helper:msgs(2),
    ?line DirSize = length(element(2, file:list_dir("."))),
    ?line stopped, ttb:stop(format),
    %%+1 -> ttb_last_trace
    ?line true = (DirSize + 1 == length(element(2, file:list_dir(".")))),
    ?line {ok,[{all, [{matched,_,_}, {matched,_,_}]}]} =
	ttb:start_trace([ClientNode, ServerNode],
			[{server, received, '_', []},
			 {client, put, 1, []},
			 {client, get, '_', []}],
			{all, call},
			[{shell, only}]),
    ?line ttb:stop().
dbg_style_fetch(cleanup,_Config) ->
    ?line stop_client_and_server().

shell_tracing_init(suite) ->
    [];
shell_tracing_init(doc) ->
    ["Shell tracing init"];
shell_tracing_init(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line ttb:tracer([ClientNode, ServerNode], shell),
    ?line ttb:stop(),
    ?line ttb:tracer([ClientNode, ServerNode],
		     [{file, {local, ?FNAME}}, shell]),
    ?line ttb:stop(),
    ?line local_client_required_on_shell_tracing =
	try  ttb:tracer([ClientNode, ServerNode],[{file, ?FNAME}, shell])
	catch
	    exit:local_client_required_on_shell_tracing ->
		local_client_required_on_shell_tracing
	end.
shell_tracing_init(cleanup,_Config) ->
    ?line stop_client_and_server().

only_one_state_for_format_handler(suite) ->
    [];
only_one_state_for_format_handler(doc) ->
    ["Only one state for format handler"];
only_one_state_for_format_handler(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_local(ServerNode, ClientNode, ?FNAME),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, counter_call_handler()}]),
    ?line {ok, Ret} = file:consult(?OUTPUT),
    ?line [5] = Ret.
only_one_state_for_format_handler(cleanup,_Config) ->
    ?line stop_client_and_server().

only_one_state_with_default_format_handler(suite) ->
    [];
only_one_state_with_default_format_handler(doc) ->
    ["Only one state with default format handler"];
only_one_state_with_default_format_handler(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_local(ServerNode, ClientNode, ?FNAME),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}]),
    ?line true = filelib:is_file(?OUTPUT).
only_one_state_with_default_format_handler(cleanup,_Config) ->
    ?line stop_client_and_server().

only_one_state_with_initial_format_handler(suite) ->
    [];
only_one_state_with_initial_format_handler(doc) ->
    ["Only one state with initial format handler"];
only_one_state_with_initial_format_handler(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}, {handler, counter_call_handler()}]),
    ?line ttb:p(all, call),
    ?line ttb:tpl(server, received, []),
    ?line ttb:tpl(client, put, []),
    ?line ttb:tpl(client, get, []),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}]),
    ?line {ok, Ret} = file:consult(?OUTPUT),
    ?line [5] = Ret.
only_one_state_with_initial_format_handler(cleanup,_Config) ->
    ?line stop_client_and_server().

run_trace_with_shortcut(Shortcut, Ret, F) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line {ok, _} =
	ttb:tracer([ServerNode,ClientNode],[{file, ?FNAME}]),
    ?line ttb:p(all, call),
    ?line ttb:F(client, put, Shortcut),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, ret_caller_call_handler()}]),
    ?line {ok, Ret} =file:consult(?OUTPUT),
    ?line stop_client_and_server().

fun_for(return) ->
    {codestr, "fun(_) -> return_trace() end"};
fun_for(msg_false) ->
    {codestr, "fun(_) -> message(false) end"}.

run_trace_with_shortcut1(suite) ->
    [];
run_trace_with_shortcut1(doc) ->
    ["Run trace with shortcut 1"];
run_trace_with_shortcut1(Config) when is_list(Config) ->
    ?line run_trace_with_shortcut(caller, [ok,ok], tp),
    ?line run_trace_with_shortcut(caller, [ok,ok], tpl).
run_trace_with_shortcut1(cleanup,_Config) ->
    ?line stop_client_and_server().

run_trace_with_shortcut2(suite) ->
    [];
run_trace_with_shortcut2(doc) ->
    ["Run trace with shortcut 2"];
run_trace_with_shortcut2(Config) when is_list(Config) ->
    ?line run_trace_with_shortcut(return, [ok,ok], tp),
    ?line run_trace_with_shortcut(return, [ok,ok], tpl).
run_trace_with_shortcut2(cleanup,_Config) ->
    ?line stop_client_and_server().

run_trace_with_shortcut3(suite) ->
    [];
run_trace_with_shortcut3(doc) ->
    ["Run trace with shortcut 3"];
run_trace_with_shortcut3(Config) when is_list(Config) ->
    ?line run_trace_with_shortcut(fun_for(return), [ok,ok], tp),
    ?line run_trace_with_shortcut(fun_for(return), [ok,ok], tpl).
run_trace_with_shortcut3(cleanup,_Config) ->
    ?line stop_client_and_server().

run_trace_with_shortcut4(suite) ->
    [];
run_trace_with_shortcut4(doc) ->
    ["Run trace with shortcut 4"];
run_trace_with_shortcut4(Config) when is_list(Config) ->
    ?line run_trace_with_shortcut(fun_for(msg_false), [], tp),
    ?line run_trace_with_shortcut(fun_for(msg_false), [], tpl).
run_trace_with_shortcut4(cleanup,_Config) ->
    ?line stop_client_and_server().

cant_specify_local_and_flush(suite) ->
    [];
cant_specify_local_and_flush(doc) ->
    ["Can't specify local and flush"];
cant_specify_local_and_flush(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line flush_unsupported_with_ip_trace_port =
	try ttb:tracer([ServerNode, ClientNode],
		       [{flush, 1000}, {file, {local, ?FNAME}}])
	catch
	    exit:flush_unsupported_with_ip_trace_port ->
		flush_unsupported_with_ip_trace_port
	end.
cant_specify_local_and_flush(cleanup,_Config) ->
    ?line stop_client_and_server().

trace_sorted_by_default(suite) ->
    [];
trace_sorted_by_default(doc) ->
    ["Trace sorted by default"];
trace_sorted_by_default(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_local(ServerNode, ClientNode, ?FILE),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, node_call_handler()}, {disable_sort, false}]),
    {ok, Ret} = file:consult(?OUTPUT),
    ?line [ClientNode,ServerNode,ClientNode,ServerNode,ServerNode] = Ret.
trace_sorted_by_default(cleanup,_Config) ->
    ?line stop_client_and_server().

disable_sorting(suite) ->
    [];
disable_sorting(doc) ->
    ["Disable sorting"];
disable_sorting(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_local(ServerNode, ClientNode, ?FILE),
    ?line ttb_helper:msgs(2),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, node_call_handler()}, {disable_sort, true}]),
    {ok, Ret} = file:consult(?OUTPUT),
    ?line [ClientNode,ClientNode,ServerNode,ServerNode,ServerNode] = Ret.
disable_sorting(cleanup,_Config) ->
    ?line stop_client_and_server().

%% -----------------------------------------------------------------------------
%% tests for autoresume of tracing
%% -----------------------------------------------------------------------------

trace_resumed_after_node_restart(suite) ->
    [];
trace_resumed_after_node_restart(doc) ->
    ["Test trace resumed after node restart, trace to files on remote node."];
trace_resumed_after_node_restart(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_with_resume(ServerNode, ClientNode, ?FNAME),
    ?line logic(2,6,file).
trace_resumed_after_node_restart(cleanup,_Config) ->
    ?line stop_client_and_server().

trace_resumed_after_node_restart_ip(suite) ->
    [];
trace_resumed_after_node_restart_ip(doc) ->
    ["Test trace resumed after node restart, trace via tcp/ip to local node."];
trace_resumed_after_node_restart_ip(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_with_resume(ServerNode, ClientNode, {local, ?FNAME}),
    ?line logic(2,6,local).
trace_resumed_after_node_restart_ip(cleanup,_Config) ->
    ?line stop_client_and_server().

trace_resumed_after_node_restart_wrap(suite) ->
    [];
trace_resumed_after_node_restart_wrap(doc) ->
    ["Test trace resumed after node restart, wrap option."];
trace_resumed_after_node_restart_wrap(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_with_resume(ServerNode, ClientNode, {wrap, ?FNAME, 10, 4}),
    ?line logic(1,4,file).
trace_resumed_after_node_restart_wrap(cleanup,_Config) ->
    ?line stop_client_and_server().

trace_resumed_after_node_restart_wrap_mult(suite) ->
    [];
trace_resumed_after_node_restart_wrap_mult(doc) ->
    ["Test trace resumed after node restart, wrap option, multiple files."];
trace_resumed_after_node_restart_wrap_mult(Config) when is_list(Config) ->
    ?line {ServerNode, ClientNode} = start_client_and_server(),
    ?line begin_trace_with_resume(ServerNode, ClientNode, {wrap, ?FNAME, 10, 4}),
    ?line logic(20,8,file).
trace_resumed_after_node_restart_wrap_mult(cleanup,_Config) ->
    ?line stop_client_and_server().

logic(N, M, TracingType) ->
    helper_msgs(N, TracingType),
    ?t:stop_node(ttb_helper:get_node(client)),
    timer:sleep(2500),
    ?line {ok,_ClientNode} = ?t:start_node(client,slave,[]),
    ct:log("client started",[]),
    ?line ok = ttb_helper:c(code, add_paths, [code:get_path()]),
    ct:log("paths added",[]),
    ?line ttb_helper:c(client, init, []),
    ct:log("client initiated",[]),
    ?line helper_msgs(N, TracingType),
    ct:log("helper msgs sent and flushed",[]),
    ?line {_, D} = ttb:stop([return_fetch_dir]),
    ct:log("stopped ~p",[D]),
    ?line ttb:format(D, [{out, ?OUTPUT}, {handler, ret_caller_call_handler2()}]),
    ct:log("formatted ~p",[{D,?OUTPUT}]),
    ?line {ok, Ret} = file:consult(?OUTPUT),
    ct:log("consulted: ~p",[Ret]),
    check_output(M,Ret).

begin_trace_with_resume(ServerNode, ClientNode, Dest) ->
    ?line {ok, _} = ttb:tracer([ServerNode,ClientNode], [{file, Dest}, resume]),
    ?line ttb:p(all, [call, timestamp]),
    ?line ttb:tp(server, received, []),
    ?line ttb:tp(client, put, []),
    ?line ttb:tp(client, get, []).

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
