%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
-module(error_logger_tty_h).

-behaviour(gen_event).

%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all events formatted to stdout.
%%%   Handles events tagged error, emulator and info.
%%%
%%% It can only be started from error_logger:swap_handler(tty)
%%% or error_logger:tty(true)
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([write_event/2]).

%% This one is used when we takeover from the simple error_logger.
init({[], {error_logger, Buf}}) ->
    User = set_group_leader(),
    write_events(Buf,io),
    {ok, {User, error_logger}};
%% This one is used if someone took over from us, and now wants to
%% go back.
init({[], {error_logger_tty_h, PrevHandler}}) ->
    User = set_group_leader(),
    {ok, {User, PrevHandler}};
%% This one is used when we are started directly.
init([]) ->
    User = set_group_leader(),
    {ok, {User, []}}.
    
handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event(Event, State) ->
    ok = write_event(tag_event(Event),io),
    {ok, State}.

handle_info({'EXIT', User, _Reason}, {User, PrevHandler}) ->
    case PrevHandler of
	[] ->
	    remove_handler;
	_ -> 
	    {swap_handler, install_prev, {User, PrevHandler}, 
	     PrevHandler, go_back}
    end;
handle_info({emulator, GL, Chars}, State) when node(GL) == node() ->
    ok = write_event(tag_event({emulator, GL, Chars}),io),
    {ok, State};
handle_info({emulator, noproc, Chars}, State) ->
    ok = write_event(tag_event({emulator, noproc, Chars}),io),
    {ok, State};
handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

% unfortunately, we can't unlink from User - links are not counted!
%    if pid(User) -> unlink(User); true -> ok end,
terminate(install_prev, _State) ->
    [];
terminate(_Reason, {_User, PrevHandler}) ->
    {error_logger_tty_h, PrevHandler}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ------------------------------------------------------
%%% Misc. functions.
%%% ------------------------------------------------------

set_group_leader() ->
    case whereis(user) of
	User when is_pid(User) -> link(User), group_leader(User,self()), User;
	_                      -> false
    end.

tag_event(Event) ->    
    {erlang:universaltime(), Event}.

%% IOMOd is always 'io'
write_events(Events,IOMod) -> write_events1(lists:reverse(Events),IOMod).

write_events1([Event|Es],IOMod) ->
    ok = write_event(Event,IOMod),
    write_events1(Es,IOMod);
write_events1([],_IOMod) ->
    ok.

write_event({Time, {error, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F, [Format,Args])
    end;
write_event({Time, {emulator, _GL, Chars}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    format(IOMod, T ++ S);
	_ ->
	    format(IOMod, T ++ "ERROR: ~p ~n", [Chars])
    end;
write_event({Time, {info, _GL, {Pid, Info, _}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    format(IOMod, T ++ add_node("~p~n",Pid),[Info]);
write_event({Time, {error_report, _GL, {Pid, std_error, Rep}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid));
write_event({Time, {info_report, _GL, {Pid, std_info, Rep}}},IOMod) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid));
write_event({Time, {info_msg, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F, [Format,Args])
    end;
write_event({Time, {warning_report, _GL, {Pid, std_warning, Rep}}},IOMod) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid));
write_event({Time, {warning_msg, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F, [Format,Args])
    end;
write_event({_Time, _Error},_IOMod) ->
    ok.

maybe_utc(Time) ->
    UTC = case application:get_env(sasl, utc_log) of
              {ok, Val} -> Val;
              undefined ->
                  %% Backwards compatible:
                  case application:get_env(stdlib, utc_log) of
                      {ok, Val} -> Val;
                      undefined -> false
                  end
          end,
    maybe_utc(Time, UTC).

maybe_utc(Time, true) -> {utc, Time};
maybe_utc(Time, _) -> {local, calendar:universal_time_to_local_time(Time)}.

format(IOMod, String)       -> format(IOMod, String, []).
format(io_lib, String, Args) -> io_lib:format(String, Args);
format(io, String, Args) -> io:format(user, String, Args).

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
write_time({utc,{{Y,Mo,D},{H,Mi,S}}},Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_time({local, {{Y,Mo,D},{H,Mi,S}}},Type) ->
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





