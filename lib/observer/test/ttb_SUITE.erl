%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ttb:stop(),
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [file, file_no_pi, file_fetch, wrap, wrap_merge,
     wrap_merge_fetch_format, write_config1, write_config2,
     write_config3, history, write_trace_info, seq_trace,
     diskless, otp_4967_1, otp_4967_2].

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


file(suite) ->
    [];
file(doc) ->
    ["Start tracing on multiple nodes, single file"];
file(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
    ?line ok = ttb:format(filename:join(Privdir,atom_to_list(Node)++"-file")),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(OtherNode)++"-file")),

    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace,
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.

file_no_pi(suite) ->
    [];
file_no_pi(doc) ->
    ["Start tracing on multiple nodes, single file, no process information"];
file_no_pi(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
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

file_fetch(suite) ->
    [];
file_fetch(doc) ->
    ["stop with the fetch option, i.e. collect all files when ttb is stopped"];
file_fetch(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ttb:stop([return]),
    ?line ?t:capture_stop(),
    ?line [StoreString] = ?t:capture_get(),
    ?line UploadDir =
	lists:last(string:tokens(lists:flatten(StoreString),"$ \n")),
    ?line ?t:stop_node(OtherNode),

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

wrap(suite) ->
    [];
wrap(doc) ->
    ["Start tracing on multiple nodes, wrap files"];
wrap(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
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

wrap_merge(suite) ->
    [];
wrap_merge(doc) ->
    ["Start tracing on multiple nodes, wrap files, merge logs from both nodes"];
wrap_merge(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
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


wrap_merge_fetch_format(suite) ->
    [];
wrap_merge_fetch_format(doc) ->
    ["Start tracing on multiple nodes, wrap files, fetch and format at stop"];
wrap_merge_fetch_format(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   {trace_ts,{S,_,Node},call,{?MODULE,foo,[]},_},
	   {trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},_},
	   end_of_trace] = flush(),

    ?line ok = file:set_cwd(Cwd),
    ok.


write_config1(suite) ->
    [];
write_config1(doc) ->
    ["Write config given commands"];
write_config1(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),

    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
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

write_config2(suite) ->
    [];
write_config2(doc) ->
    ["Write config from history (all)"];
write_config2(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
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

write_config3(suite) ->
    [];
write_config3(doc) ->
    ["Write config from history (selected and append)"];
write_config3(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
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
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
    ?line ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-history"),
		  filename:join(Privdir,atom_to_list(OtherNode)++"-history")]),
    ?line [{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.
    


write_trace_info(suite) ->
    [];
write_trace_info(doc) ->
    ["Write trace info and give handler explicitly in format command"];
write_trace_info(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line {ok,OtherNode} = ?t:start_node(node2,slave,[]),
    ?line c:nl(?MODULE),
    ?line S = self(),
    ?line Privdir=?config(priv_dir, Config),
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
    ?line ?t:stop_node(OtherNode),
    ?line ok = ttb:format(
		 [filename:join(Privdir,atom_to_list(Node)++"-write_trace_info"),
		  filename:join(Privdir,
				atom_to_list(OtherNode)++"-write_trace_info")],
		 [{handler,{fun otherhandler/4,S}}]),
    ?line [{{trace_ts,{S,_,Node},call,{?MODULE,foo,[]},{_,_,_}},[Node]},
	   {{trace_ts,{_,_,OtherNode},call,{?MODULE,foo,[]},{_,_,_}},[OtherNode]},
	   end_of_trace] = flush(),

    ok.


seq_trace(suite) ->
    [];
seq_trace(doc) ->
    ["Test sequential tracing"];
seq_trace(Config) when is_list(Config) ->
    ?line S = self(),

    ?line Privdir=?config(priv_dir, Config),
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
    ?line Privdir=?config(priv_dir, Config),
    ?line File = filename:join(Privdir,"diskless"),
    ?line {ok,[RemoteNode]} = 
	ttb:tracer([RemoteNode],[{file, {local, File}},
				 {handler,{fun myhandler/4, S}}]),
    ?line {ok,[{all,[{matched,RemoteNode,_}]}]} = ttb:p(all,call),
    ?line {ok,[{matched,RemoteNode,1}]} = ttb:tp(?MODULE,foo,[]),

    ?line rpc:call(RemoteNode,?MODULE,foo,[]),
    ?line timer:sleep(500), % needed for the IP port to flush
    ?line ttb:stop([nofetch]),
    ?line ?t:stop_node(RemoteNode),
    ?line ok = ttb:format(filename:join(Privdir,
					atom_to_list(RemoteNode)++"-diskless")),

    ?line [{trace_ts,{_,_,RemoteNode},call,{?MODULE,foo,[]},{_,_,_}},
	   end_of_trace] = flush(),
    ok.


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
    io:format("1: ~p",[now()]),
    ?line Privdir = ?config(priv_dir,Config),
    io:format("2: ~p",[now()]),
    ?line File = filename:join(Privdir,"otp_4967"),
    io:format("3: ~p",[now()]),
    ?line S = self(),
    io:format("4: ~p",[now()]),
    ?line {ok,[Node]} =
	ttb:tracer(node(),[{file, File},
			   {handler,{fun myhandler/4, S}}]),

    io:format("5: ~p",[now()]),
    %% Test that delayed registration of a process works.
    receive after 200 -> ok end,
    ?line register(otp_4967,self()),
    io:format("6: ~p",[now()]),
    ?line {ok,[{S,[{matched,Node,1}]}]} =  ttb:p(self(),s),
    io:format("7: ~p",[now()]),
    ?line {otp_4967,node()} ! heihopp,
    io:format("8: ~p",[now()]),
    ?line stopped = ttb:stop([format]),
    io:format("9: ~p",[now()]),
    ?line Msgs = flush(),
    io:format("10: ~p",[now()]),
    ?line io:format("Messages received: \n~p\n",[Msgs]),
    io:format("11: ~p",[now()]),
    ?line true = lists:member(heihopp,Msgs), % the heihopp message itself
    io:format("13: ~p",[now()]),
    ?line {value,{trace_ts,_,send,heihopp,{_,otp_4967,Node},{_,_,_}}} =
	lists:keysearch(heihopp,4,Msgs), % trace trace of the heihopp message
    io:format("14: ~p",[now()]),
    ?line end_of_trace = lists:last(Msgs), % end of the trace
    ok.
    



myhandler(_Fd,Trace,_,Relay) ->
    Relay ! Trace,
    Relay.

otherhandler(_Fd,Trace,end_of_trace,Relay) ->
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
