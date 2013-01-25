%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% CT hook for logging of connections.
%%
%% HookOptions can be hardcoded in the test suite:
%%
%% suite() ->
%%    [{ct_hooks, [{cth_conn_log,
%%                  [{ct_netconfc:conn_mod(),ct_netconfc:hook_options()}]}]}].
%%
%% or specified in a configuration file:
%%
%% {ct_conn_log,[{ct_netconfc:conn_mod(),ct_netconfc:hook_options()}]}.
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
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4]).

-spec init(Id, HookOpts) -> Result when
      Id :: term(),
      HookOpts :: ct_netconfc:hook_options(),
      Result :: {ok,[{ct_netconfc:conn_mod(),
		      {ct_netconfc:log_type(),[ct_netconfc:key_or_name()]}}]}.
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
    [{Mod,get_log_opts(Opts)} | merge_log_info(ConfList,HookList1)];
merge_log_info([],HookList) ->
    [{Mod,get_log_opts(Opts)} || {Mod,Opts} <- HookList].

get_log_opts(Opts) ->
    LogType = proplists:get_value(log_type,Opts,html),
    Hosts = proplists:get_value(hosts,Opts,[]),
    {LogType,Hosts}.


pre_init_per_testcase(TestCase,Config,CthState) ->
    Logs =
	lists:map(
	  fun({ConnMod,{LogType,Hosts}}) ->
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
				 "<tr><td>~p</td><td><a href=\"~ts\">~ts</a>"
				 "</td></tr>",
				 [S,ct_logs:uri(L),filename:basename(L)])
			       || {S,L} <- Ls] ++
			      "</table>",
			  io:format(Str,[]),
			  {ConnMod,{LogType,Ls}};
		      _ ->
			  {ConnMod,{LogType,[]}}
		  end
	  end,
	  CthState),
    error_logger:add_report_handler(ct_conn_log_h,{group_leader(),Logs}),
    {Config,CthState}.

post_end_per_testcase(_TestCase,_Config,Return,CthState) ->
    error_logger:delete_report_handler(ct_conn_log_h),
    {Return,CthState}.
