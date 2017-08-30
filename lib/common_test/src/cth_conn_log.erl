%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% CT hook for logging of connections.
%%
%% HookOptions can be hardcoded in the test suite:
%%
%% suite() ->
%%    [{ct_hooks, [{cth_conn_log,
%%                  [{conn_mod(),hook_options()}]}]}].
%%
%% or specified in a configuration file:
%%
%% {ct_conn_log,[{conn_mod(),hook_options()}]}.
%%
%% The conn_mod() is the common test module implementing the protocol,
%% e.g. ct_netconfc, ct_telnet, etc. This module must log by calling
%%
%% error_logger:info_report(ConnLogInfo,Data).
%% ConnLogInfo = #conn_log{} | {ct_connection,Action,ConnName}
%% Action = open | close | send | recv | term()
%% ConnName = atom() - The 'KeyOrName' argument used when opening the connection
%%
%% ct_conn_log_h will print to html log or separate file (depending on
%% log_type() option). conn_mod() must implement and export
%%
%% format_data(log_type(), Data).
%%
%% If logging to separate file, ct_conn_log_h will also log error
%% reports which are witten like this:
%%
%% error_logger:error_report([{ct_connection,ConnName} | Report]).
%%
%%----------------------------------------------------------------------
-module(cth_conn_log).

-include_lib("common_test/include/ct.hrl").

-export([init/2,
	 pre_init_per_testcase/4,
	 post_end_per_testcase/5]).

%%----------------------------------------------------------------------
%% Type declarations
%%----------------------------------------------------------------------
-type hook_options() :: ct:conn_log_options().
-type log_type() :: ct:conn_log_type().
-type conn_mod() :: ct:conn_log_mod().
%%----------------------------------------------------------------------

-spec init(Id, HookOpts) -> Result when
      Id :: term(),
      HookOpts :: hook_options(),
      Result :: {ok,[{conn_mod(),{log_type(),[ct:key_or_name()]}}]}.
init(_Id, HookOpts) ->
    ConfOpts = ct:get_config(ct_conn_log,[]),
    {ok,merge_log_info(ConfOpts,HookOpts)}.

merge_log_info([{Mod,ConfOpts}|ConfList],HookList) ->
    {Opts,HookList1} =
	case lists:keytake(Mod,1,HookList) of
	    false ->
		{ConfOpts,HookList};
	    {value,{_,HookOpts},HL1} ->
		{ConfOpts ++ HookOpts, HL1} % ConfOpts overwrites HookOpts!
	end,
    [{Mod,get_log_opts(Mod,Opts)} | merge_log_info(ConfList,HookList1)];
merge_log_info([],HookList) ->
    [{Mod,get_log_opts(Mod,Opts)} || {Mod,Opts} <- HookList].

get_log_opts(Mod,Opts) ->
    DefaultLogType = if Mod == ct_telnet -> raw;
			true -> html
		     end,
    LogType = proplists:get_value(log_type,Opts,DefaultLogType),
    Hosts = proplists:get_value(hosts,Opts,[]),
    {LogType,Hosts}.

pre_init_per_testcase(_Suite,TestCase,Config,CthState) ->
    Logs =
	lists:map(
	  fun({ConnMod,{LogType,Hosts}}) ->		  
		  ct_util:set_testdata({{?MODULE,ConnMod},LogType}),
		  case LogType of
		      LogType when LogType==raw; LogType==pretty ->
			  Dir = ?config(priv_dir,Config),
			  TCStr = atom_to_list(TestCase),
			  ConnModStr = atom_to_list(ConnMod),
			  DefLogName = TCStr  ++ "-" ++ ConnModStr ++ ".txt",
			  DefLog = filename:join(Dir,DefLogName),
			  Ls = [{Host,
				 filename:join(Dir,TCStr ++ "-"++
						   atom_to_list(Host) ++ "-" ++
						   ConnModStr ++
						   ".txt")}
				|| Host <- Hosts]
			      ++[{default,DefLog}],
			  Str =
			      "<table borders=1>"
			      "<b>" ++ ConnModStr ++ " logs:</b>\n" ++
			      [io_lib:format(
				 "<tr><td>~tp</td><td><a href=\"~ts\">~ts</a>"
				 "</td></tr>",
				 [S,ct_logs:uri(L),filename:basename(L)])
			       || {S,L} <- Ls] ++
			      "</table>",
			  ct:log(Str,[],[no_css]),
			  {ConnMod,{LogType,Ls}};
		      _ ->
			  {ConnMod,{LogType,[]}}
		  end
	  end,
	  CthState),

    GL = group_leader(),
    Update =
	fun(Init) when Init == undefined; Init == [] ->
		error_logger:add_report_handler(ct_conn_log_h,{GL,Logs}),
		[TestCase];
	   (PrevUsers) ->
		error_logger:info_report(update,{GL,Logs}),
		receive
		    {updated,GL} ->
			[TestCase|PrevUsers]
		after
		    5000 ->
			{error,no_response}
		end
	end,
    ct_util:update_testdata(?MODULE, Update, [create]),
    {Config,CthState}.

post_end_per_testcase(_Suite,TestCase,_Config,Return,CthState) ->
    Update =
	fun(PrevUsers) ->
		case lists:delete(TestCase, PrevUsers) of
		    [] ->
			'$delete';
		    PrevUsers1 ->
			PrevUsers1
		end
	end,
    case ct_util:update_testdata(?MODULE, Update) of
	deleted ->
	    _ = [ct_util:delete_testdata({?MODULE,ConnMod}) ||
		{ConnMod,_} <- CthState],
	    error_logger:delete_report_handler(ct_conn_log_h);
	{error,no_response} ->
	    exit({?MODULE,no_response_from_logger});
	_PrevUsers ->
	    ok
    end,
    {Return,CthState}.

