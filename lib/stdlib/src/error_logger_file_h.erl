%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(error_logger_file_h).

-behaviour(gen_event).


%%%
%%% A handler that can be connected to the error_logger
%%% event handler. Writes all events formatted to file.
%%%
%%% It can only be started from error_logger:swap_handler({logfile, File})
%%% or error_logger:logfile(File).
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(st,
	{fd,
	 filename,
	 prev_handler,
	 depth=unlimited :: 'unlimited' | non_neg_integer()}).

%% This one is used when we takeover from the simple error_logger.
init({File, {error_logger, Buf}}) ->
    case init(File, error_logger) of
	{ok, State} ->
	    write_events(State, Buf),
	    {ok, State};
	Error ->
	    Error
    end;
%% This one is used when we are started directly.
init(File) ->
    init(File, []).

init(File, PrevHandler) ->
    process_flag(trap_exit, true),
    case file:open(File, [write,{encoding,utf8}]) of
	{ok,Fd} ->
            Depth = error_logger:get_format_depth(),
	    State = #st{fd=Fd,filename=File,prev_handler=PrevHandler,
			depth=Depth},
	    {ok, State};
	Error ->
	    Error
    end.

handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event(Event, State) ->
    write_event(State, Event),
    {ok, State}.

handle_info({'EXIT', Fd, _Reason}, #st{fd=Fd,prev_handler=PrevHandler}) ->
    case PrevHandler of
	[] ->
	    remove_handler;
	_ -> 
	    {swap_handler, install_prev, [], PrevHandler, go_back}
    end;
handle_info(_, State) ->
    {ok, State}.

handle_call(filename, #st{filename=File}=State) ->
    {ok, File, State};
handle_call(_Query, State) ->
    {ok, {error, bad_query}, State}.

terminate(_Reason, #st{fd=Fd}) ->
    file:close(Fd).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ------------------------------------------------------
%%% Misc. functions.
%%% ------------------------------------------------------

write_events(State, [Ev|Es]) ->
    %% Write the events in reversed order.
    write_events(State, Es),
    write_event(State, Ev);
write_events(_State, []) ->
    ok.

write_event(#st{fd=Fd}=State, Event) ->
    case parse_event(Event) of
	ignore ->
	    ok;
	{Head,Pid,FormatList} ->
	    Time = erlang:universaltime(),
	    Header = header(Time, Head),
	    Body = format_body(State, FormatList),
	    AtNode = if
			 node(Pid) =/= node() ->
			     ["** at node ",atom_to_list(node(Pid))," **\n"];
			 true ->
			     []
		     end,
	    io:put_chars(Fd, [Header,AtNode,Body])
    end.

format_body(State, [{Format,Args}|T]) ->
    S = try format(State, Format, Args) of
	    S0 ->
		S0
	catch
	    _:_ ->
		format(State, "ERROR: ~tp - ~tp\n", [Format,Args])
	end,
    [S|format_body(State, T)];
format_body(_State, []) ->
    [].

format(#st{depth=unlimited}, Format, Args) ->
    io_lib:format(Format, Args);
format(#st{depth=Depth}, Format0, Args) ->
    Format1 = io_lib:scan_format(Format0, Args),
    Format = limit_format(Format1, Depth),
    io_lib:build_text(Format).

limit_format([#{control_char:=C0}=M0|T], Depth) when C0 =:= $p;
						     C0 =:= $w ->
    C = C0 - ($a - $A),				%To uppercase.
    #{args:=Args} = M0,
    M = M0#{control_char:=C,args:=Args++[Depth]},
    [M|limit_format(T, Depth)];
limit_format([H|T], Depth) ->
    [H|limit_format(T, Depth)];
limit_format([], _) ->
    [].

parse_event({error, _GL, {Pid, Format, Args}}) ->
    {"ERROR REPORT",Pid,[{Format,Args}]};
parse_event({info_msg, _GL, {Pid, Format, Args}}) ->
    {"INFO REPORT",Pid,[{Format, Args}]};
parse_event({warning_msg, _GL, {Pid, Format, Args}}) ->
    {"WARNING REPORT",Pid,[{Format,Args}]};
parse_event({error_report, _GL, {Pid, std_error, Args}}) ->
    {"ERROR REPORT",Pid,format_term(Args)};
parse_event({info_report, _GL, {Pid, std_info, Args}}) ->
    {"INFO REPORT",Pid,format_term(Args)};
parse_event({warning_report, _GL, {Pid, std_warning, Args}}) ->
    {"WARNING REPORT",Pid,format_term(Args)};
parse_event(_) -> ignore.

format_term(Term) when is_list(Term) ->
    case string_p(lists:flatten(Term)) of
	true ->
	    [{"~ts\n",[Term]}];
	false ->
	    format_term_list(Term)
    end;
format_term(Term) ->
    [{"~tp\n",[Term]}].

format_term_list([{Tag,Data}|T]) ->
    [{"    ~tp: ~tp\n",[Tag,Data]}|format_term_list(T)];
format_term_list([Data|T]) ->
    [{"    ~tp\n",[Data]}|format_term_list(T)];
format_term_list([]) ->
    [].

string_p([]) ->
    false;
string_p(FlatList) ->
    io_lib:printable_list(FlatList).

get_utc_config() ->
    %% SASL utc_log configuration overrides stdlib config
    %% in order to have uniform timestamps in log messages
    case application:get_env(sasl, utc_log) of
        {ok, Val} -> Val;
        undefined ->
            case application:get_env(stdlib, utc_log) of
                {ok, Val} -> Val;
                undefined -> false
            end
    end.

header(Time, Title) ->
    case get_utc_config() of
        true ->
            header(Time, Title, "UTC ");
        _ ->
            header(calendar:universal_time_to_local_time(Time), Title, "")
    end.

header({{Y,Mo,D},{H,Mi,S}}, Title, UTC) ->
    io_lib:format("~n=~ts==== ~p-~s-~p::~s:~s:~s ~s===~n",
                  [Title,D,month(Mo),Y,t(H),t(Mi),t(S),UTC]).

t(X) when is_integer(X) ->
    t1(integer_to_list(X)).

t1([X]) -> [$0,X];
t1(X)   -> X.

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
