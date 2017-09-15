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

-record(state, {logs=[], default_gl}).

-define(WIDTH,80).

-define(now, os:timestamp()).

%%%-----------------------------------------------------------------
%%% Callbacks
init({GL,ConnLogs}) ->
    open_files(GL,ConnLogs,#state{default_gl=GL}).

open_files(GL,[{ConnMod,{LogType,LogFiles}}|T],State=#state{logs=Logs}) ->
    case do_open_files(LogFiles,[]) of
	{ok,Fds} ->
	    ConnInfo = proplists:get_value(GL,Logs,[]),
	    Logs1 = [{GL,[{ConnMod,{LogType,Fds}}|ConnInfo]} | 
		     proplists:delete(GL,Logs)],
	    open_files(GL,T,State#state{logs=Logs1});
	Error ->
	    Error
    end;
open_files(_GL,[],State) ->
    {ok,State}.

do_open_files([{Tag,File}|LogFiles],Acc) ->
    case file:open(File, [write,append,{encoding,utf8}]) of
	{ok,Fd} ->
	    do_open_files(LogFiles,[{Tag,Fd}|Acc]);
	{error,Reason} ->
	    {error,{could_not_open_log,File,Reason}}
    end;
do_open_files([],Acc) ->
    {ok,lists:reverse(Acc)}.

handle_event({info_report,_,{From,update,{GL,ConnLogs}}},
	     State) when node(GL) == node() ->
    Result = open_files(GL,ConnLogs,State),
    From ! {updated,GL},
    Result;
handle_event({_Type, GL, _Msg}, State) when node(GL) /= node() ->
    {ok, State};
handle_event({_Type,GL,{Pid,{ct_connection,Mod,Action,ConnName},Report}},
	     State) ->
    Info = conn_info(Pid,#conn_log{name=ConnName,action=Action,module=Mod}),
    write_report(?now,Info,Report,GL,State),
    {ok, State};
handle_event({_Type,GL,{Pid,Info=#conn_log{},Report}}, State) ->
    write_report(?now,conn_info(Pid,Info),Report,GL,State),
    {ok, State};
handle_event({error_report,GL,{Pid,_,[{ct_connection,ConnName}|R]}}, State) ->
    %% Error reports from connection
    write_error(?now,conn_info(Pid,#conn_log{name=ConnName}),R,GL,State),
    {ok, State};
handle_event(_What, State) ->
    {ok, State}.

handle_info(_What, State) ->
    {ok, State}.

handle_call(_Query, State) ->
    {ok, {error, bad_query}, State}.

terminate(_,#state{logs=Logs}) ->
    lists:foreach(
      fun({_GL,ConnLogs}) ->
	      [file:close(Fd) || {_,{_,Fds}} <- ConnLogs, {_,Fd} <- Fds]
      end, Logs),
    ok.


%%%-----------------------------------------------------------------
%%% Writing reports
write_report(_Time,#conn_log{header=false,module=ConnMod}=Info,Data,GL,State) ->
    case get_log(Info,GL,State) of
	{silent,_,_} ->
	    ok;
	{LogType,Dest,Fd} ->
	    Str = if LogType == html, Dest == gl -> ["$tc_html","~n~ts"];
		     true                        -> "~n~ts"
		  end,
	    io:format(Fd,Str,[format_data(ConnMod,LogType,Data)])
    end;

write_report(Time,#conn_log{module=ConnMod}=Info,Data,GL,State) ->
    case get_log(Info,GL,State) of
	{silent,_,_} ->
	    ok;
	{LogType,Dest,Fd} ->
	    case format_data(ConnMod,LogType,Data) of
		[] when Info#conn_log.action==send; Info#conn_log.action==recv ->
		    ok;
		FormattedData ->
		    Str = if LogType == html, Dest == gl ->
				  ["$tc_html","~n~ts~ts~ts"];
			     true ->
				  "~n~ts~ts~ts"
			  end,
		    io:format(Fd,Str,[format_head(ConnMod,LogType,Time),
				      format_title(LogType,Info),
				      FormattedData])
	    end
    end.

write_error(Time,#conn_log{module=ConnMod}=Info,Report,GL,State) ->
    case get_log(Info,GL,State) of
	{LogType,_,_} when LogType==html; LogType==silent ->
	    %% The error will anyway be written in the html log by the
	    %% sasl error handler, so don't write it again.
	    ok;
	{LogType,Dest,Fd} ->
	    Str = if LogType == html, Dest == gl -> ["$tc_html","~n~ts~ts~ts"];
		     true                        -> "~n~ts~ts~ts"
		  end,
	    io:format(Fd,Str,[format_head(ConnMod,LogType,Time," ERROR"),
			      format_title(LogType,Info),
			      format_error(LogType,Report)])
    end.

get_log(Info,GL,State) ->
    case proplists:get_value(GL,State#state.logs) of
	undefined ->
	    {html,gl,State#state.default_gl};
	ConnLogs ->
	    case proplists:get_value(Info#conn_log.module,ConnLogs) of
		{html,_} ->
		    {html,gl,GL};
		{LogType,Fds} ->
		    {LogType,file,get_fd(Info,Fds)};
		undefined ->
		    {html,gl,GL}
	    end
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
    io_lib:format("Client ~tw ~s ~ts",[Client,actionstr(Info),serverstr(Info)]);
format_title(_,Info) ->
    Title = pad_char_end(?WIDTH,pretty_title(Info),$=),
    io_lib:format("~n~ts", [Title]).

format_data(_,_,NoData) when NoData == ""; NoData == <<>> ->
    "";
format_data(ConnMod,LogType,Data) ->
    ConnMod:format_data(LogType,Data).

format_error(raw,Report) ->
    io_lib:format("~n~tp~n",[Report]);
format_error(pretty,Report) ->
    [io_lib:format("~n    ~tp: ~tp",[K,V]) || {K,V} <- Report].


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
    Text = string:uppercase(atom_to_list(ConnMod) ++ Text0),
    io_lib:format("= ~s ==== ~s-~s-~w::~s:~s:~s,~s ",
		  [Text,t(D),month(Mo),Y,t(H),t(Mi),t(S),
		   micro2milli(MicroS)]).

pretty_title(#conn_log{client=Client}=Info) ->
    io_lib:format("= Client ~tw ~s ~ts ",
		  [Client,actionstr(Info),serverstr(Info)]).

actionstr(#conn_log{action=send}) -> "----->";
actionstr(#conn_log{action=cmd}) -> "----->";
actionstr(#conn_log{action=recv}) -> "<-----";
actionstr(#conn_log{action=open}) -> "opened session to";
actionstr(#conn_log{action=close}) -> "closed session to";
actionstr(#conn_log{action=connect}) -> "connected to";
actionstr(#conn_log{action=disconnect}) -> "disconnected from";
actionstr(_) -> "<---->".

serverstr(#conn_log{name=undefined,address={undefined,_}}) ->
    io_lib:format("server",[]);
serverstr(#conn_log{name=undefined,address=Address}) ->
    io_lib:format("~tp",[Address]);
serverstr(#conn_log{name=Alias,address={undefined,_}}) ->
    io_lib:format("~tw",[Alias]);
serverstr(#conn_log{name=Alias,address=Address}) ->
    io_lib:format("~tw(~tp)",[Alias,Address]).

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
    case string:length(Str) of
	M when M<N -> Str ++ lists:duplicate(N-M,Char);
	_ -> Str
    end.
