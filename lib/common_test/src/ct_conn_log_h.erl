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
-module(ct_conn_log_h).

%%%
%%% A handler that can be connected to the error_logger event
%%% handler. Writes all ct connection events. See comments in
%%% cth_conn_log for more information.
%%%

-include("ct_util.hrl").

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-record(state, {group_leader,logs=[]}).

-define(WIDTH,80).

%%%-----------------------------------------------------------------
%%% Callbacks
init({GL,Logs}) ->
    open_files(Logs,#state{group_leader=GL}).

open_files([{ConnMod,{LogType,Logs}}|T],State) ->
    case do_open_files(Logs,[]) of
	{ok,Fds} ->
	    open_files(T,State#state{logs=[{ConnMod,{LogType,Fds}} |
					   State#state.logs]});
	Error ->
	    Error
    end;
open_files([],State) ->
    {ok,State}.


do_open_files([{Tag,File}|Logs],Acc) ->
    case file:open(File, [write,{encoding,utf8}]) of
	{ok,Fd} ->
	    do_open_files(Logs,[{Tag,Fd}|Acc]);
	{error,Reason} ->
	    {error,{could_not_open_log,File,Reason}}
    end;
do_open_files([],Acc) ->
    {ok,lists:reverse(Acc)}.

handle_event({_Type, GL, _Msg}, State) when node(GL) /= node() ->
    {ok, State};
handle_event({_Type,_GL,{Pid,{ct_connection,Action,ConnName},Report}},State) ->
    %% NOTE: if the format of this event is changed
    %% ({ct_connection,Action,ConnName}) then remember to change
    %% test_server_h:report_receiver as well!!!
    Info = conn_info(Pid,#conn_log{name=ConnName,action=Action}),
    write_report(now(),Info,Report,State),
    {ok, State};
handle_event({_Type,_GL,{Pid,Info=#conn_log{},Report}},State) ->
    %% NOTE: if the format of this event is changed
    %% (Info=#conn_log{}) then remember to change
    %% test_server_h:report_receiver as well!!!
    write_report(now(),conn_info(Pid,Info),Report,State),
    {ok, State};
handle_event({error_report,_,{Pid,_,[{ct_connection,ConnName}|R]}},State) ->
    %% Error reports from connection
    write_error(now(),conn_info(Pid,#conn_log{name=ConnName}),R,State),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) ->
    {ok, {error, bad_query}, State}.

terminate(_,#state{logs=Logs}) ->
    [file:close(Fd) || {_,{_,Fds}} <- Logs, {_,Fd} <- Fds],
    ok.


%%%-----------------------------------------------------------------
%%% Writing reports
write_report(Time,#conn_log{module=ConnMod}=Info,Data,State) ->
    {LogType,Fd} = get_log(Info,State),
    io:format(Fd,"~n~ts~ts~ts",[format_head(ConnMod,LogType,Time),
				format_title(LogType,Info),
				format_data(ConnMod,LogType,Data)]).

write_error(Time,#conn_log{module=ConnMod}=Info,Report,State) ->
    case get_log(Info,State) of
	{html,_} ->
	    %% The error will anyway be written in the html log by the
	    %% sasl error handler, so don't write it again.
	    ok;
	{LogType,Fd} ->
	    io:format(Fd,"~n~ts~ts~ts",
		      [format_head(ConnMod,LogType,Time," ERROR"),
		       format_title(LogType,Info),
		       format_error(LogType,Report)])
    end.

get_log(Info,State) ->
    case proplists:get_value(Info#conn_log.module,State#state.logs) of
	{html,_} ->
	    {html,State#state.group_leader};
	{LogType,Fds} ->
	    {LogType,get_fd(Info,Fds)};
	undefined ->
	    {html,State#state.group_leader}
    end.

get_fd(#conn_log{name=undefined},Fds) ->
    proplists:get_value(default,Fds);
get_fd(#conn_log{name=ConnName},Fds) ->
    case proplists:get_value(ConnName,Fds) of
	undefined ->
	    proplists:get_value(default,Fds);
	Fd ->
	    Fd
    end.

%%%-----------------------------------------------------------------
%%% Formatting
format_head(ConnMod,LogType,Time) ->
    format_head(ConnMod,LogType,Time,"").

format_head(ConnMod,raw,Time,Text) ->
    io_lib:format("~n~w, ~w~ts, ",[now_to_time(Time),ConnMod,Text]);
format_head(ConnMod,_,Time,Text) ->
    Head = pad_char_end(?WIDTH,pretty_head(now_to_time(Time),ConnMod,Text),$=),
    io_lib:format("~n~ts",[Head]).

format_title(raw,#conn_log{client=Client}=Info) ->
    io_lib:format("Client ~w ~s ~ts",[Client,actionstr(Info),serverstr(Info)]);
format_title(_,Info) ->
    Title = pad_char_end(?WIDTH,pretty_title(Info),$=),
    io_lib:format("~n~ts", [Title]).

format_data(_,_,NoData) when NoData == ""; NoData == <<>> ->
    "";
format_data(ConnMod,LogType,Data) ->
    ConnMod:format_data(LogType,Data).

format_error(raw,Report) ->
    io_lib:format("~n~p~n",[Report]);
format_error(pretty,Report) ->
    [io_lib:format("~n    ~p: ~p",[K,V]) || {K,V} <- Report].


%%%-----------------------------------------------------------------
%%% Helpers
conn_info(LoggingProc, #conn_log{client=undefined} = ConnInfo) ->
    conn_info(ConnInfo#conn_log{client=LoggingProc});
conn_info(_, ConnInfo) ->
    conn_info(ConnInfo).

conn_info(#conn_log{client=Client, module=undefined} = ConnInfo) ->
    case ets:lookup(ct_connections,Client) of
	[#conn{address=Address,callback=Callback}] ->
	    ConnInfo#conn_log{address=Address,module=Callback};
	[] ->
	    ConnInfo
    end;
conn_info(ConnInfo) ->
    ConnInfo.


now_to_time({_,_,MicroS}=Now) ->
    {calendar:now_to_local_time(Now),MicroS}.

pretty_head({{{Y,Mo,D},{H,Mi,S}},MicroS},ConnMod,Text0) ->
    Text = string:to_upper(atom_to_list(ConnMod) ++ Text0),
    io_lib:format("= ~s ==== ~s-~s-~w::~s:~s:~s,~s ",
		  [Text,t(D),month(Mo),Y,t(H),t(Mi),t(S),
		   micro2milli(MicroS)]).

pretty_title(#conn_log{client=Client}=Info) ->
    io_lib:format("= Client ~w  ~s  Server ~ts ",
		  [Client,actionstr(Info),serverstr(Info)]).

actionstr(#conn_log{action=send}) -> "----->";
actionstr(#conn_log{action=recv}) -> "<-----";
actionstr(#conn_log{action=open}) -> "opened session to";
actionstr(#conn_log{action=close}) -> "closed session to";
actionstr(_) -> "<---->".

serverstr(#conn_log{name=undefined,address=Address}) ->
    io_lib:format("~p",[Address]);
serverstr(#conn_log{name=Alias,address=Address}) ->
    io_lib:format("~w(~p)",[Alias,Address]).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

micro2milli(X) ->
    pad0(3,integer_to_list(X div 1000)).

t(X) ->
    pad0(2,integer_to_list(X)).

pad0(N,Str) ->
    M = length(Str),
    lists:duplicate(N-M,$0) ++ Str.

pad_char_end(N,Str,Char) ->
    case length(lists:flatten(Str)) of
	M when M<N -> Str ++ lists:duplicate(N-M,Char);
	_ -> Str
    end.
