%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(error_logger_file_h).

-behaviour(gen_event).


%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all events formatted to file.
%%%   Handles events tagged error, emulator and info.
%%%
%%% It can only be started from error_logger:swap_handler({logfile, File})
%%% or error_logger:logfile(File)
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

%% This one is used when we takeover from the simple error_logger.
init({File, {error_logger, Buf}}) ->
    case init(File, error_logger) of
	{ok, {Fd, File, PrevHandler}} ->
	    write_events(Fd, Buf),
	    {ok, {Fd, File, PrevHandler}};
	Error ->
	    Error
    end;
%% This one is used when we are started directly.
init(File) ->
    init(File, []).

init(File, PrevHandler) ->
    process_flag(trap_exit, true),
    case file:open(File, [write]) of
	{ok,Fd} ->
	    {ok, {Fd, File, PrevHandler}};
	Error ->
	    Error
    end.
    
handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event(Event, {Fd, File, PrevHandler}) ->
    write_event(Fd, tag_event(Event)),
    {ok, {Fd, File, PrevHandler}};
handle_event(_, State) ->
    {ok, State}.

handle_info({'EXIT', Fd, _Reason}, {Fd, _File, PrevHandler}) ->
    case PrevHandler of
	[] ->
	    remove_handler;
	_ -> 
	    {swap_handler, install_prev, [], PrevHandler, go_back}
    end;
handle_info({emulator, GL, Chars}, {Fd, File, PrevHandler})
  when node(GL) == node() ->
    write_event(Fd, tag_event({emulator, GL, Chars})),
    {ok, {Fd, File, PrevHandler}};
handle_info({emulator, noproc, Chars}, {Fd, File, PrevHandler}) ->
    write_event(Fd, tag_event({emulator, noproc, Chars})),
    {ok, {Fd, File, PrevHandler}};
handle_info(_, State) ->
    {ok, State}.

handle_call(filename, {Fd, File, Prev}) ->
    {ok, File, {Fd, File, Prev}};
handle_call(_Query, State) ->
    {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    case State of
        {Fd, _File, _Prev} ->
            ok = file:close(Fd);
        _ ->
            ok
    end,
    [].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ------------------------------------------------------
%%% Misc. functions.
%%% ------------------------------------------------------

tag_event(Event) ->    
    {erlang:localtime(), Event}.

write_events(Fd, Events) -> write_events1(Fd, lists:reverse(Events)).

write_events1(Fd, [Event|Es]) ->
    write_event(Fd, Event),
    write_events1(Fd, Es);
write_events1(_, []) ->
    ok.

write_event(Fd, {Time, {error, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    io:format(Fd, T ++ F, [Format,Args])
    end;
write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    io:format(Fd, T ++ "ERROR: ~p ~n", [Chars])
    end;
write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(maybe_utc(Time)),
    io:format(Fd, T ++ add_node("~p~n",Pid),[Info]);
write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(maybe_utc(Time)),
    S = format_report(Rep),
    io:format(Fd, T ++ S ++ add_node("", Pid), []);
write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    S = format_report(Rep),
    io:format(Fd, T ++ S ++ add_node("", Pid), []);
write_event(Fd, {Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    io:format(Fd, T ++ F, [Format,Args])
    end;
write_event(Fd, {Time, {warning_report, _GL, {Pid, std_warning, Rep}}}) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    S = format_report(Rep),
    io:format(Fd, T ++ S ++ add_node("", Pid), []);
write_event(Fd, {Time, {warning_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    io:format(Fd, T ++ F, [Format,Args])
    end;
write_event(_, _) ->
    ok.

maybe_utc(Time) ->
    UTC = case application:get_env(sasl, utc_log) of
              {ok, Val} ->
                  Val;
              undefined ->
                  %% Backwards compatible:
                  case application:get_env(stdlib, utc_log) of
                      {ok, Val} ->
                          Val;
                      undefined ->
                          false
                  end
          end,
    if
        UTC =:= true ->
            {utc, calendar:local_time_to_universal_time(Time)};
        true -> 
            Time
    end.

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
	true ->
	    io_lib:format("~s~n",[Rep]);
	_ ->
	    format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) =/= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").

write_time({utc,{{Y,Mo,D},{H,Mi,S}}}, Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]).

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".
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


