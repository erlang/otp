%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(error_logger_tty_h).

-behaviour(gen_event).

%%%
%%% A handler that can be connected to the error_logger
%%% event handler. Writes all events formatted to stdout.
%%%
%%% It can only be started from error_logger:swap_handler(tty)
%%% or error_logger:tty(true).
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([write_event/2,write_event/3]).

-record(st,
	{user,
	 prev_handler,
	 io_mod=io,
	 depth=unlimited}).

%% This one is used when we takeover from the simple error_logger.
init({[], {error_logger, Buf}}) ->
    User = set_group_leader(),
    Depth = get_depth(),
    State = #st{user=User,prev_handler=error_logger,depth=Depth},
    write_events(State, Buf),
    {ok, State};
%% This one is used if someone took over from us, and now wants to
%% go back.
init({[], {error_logger_tty_h, PrevHandler}}) ->
    User = set_group_leader(),
    {ok, #st{user=User,prev_handler=PrevHandler}};
%% This one is used when we are started directly.
init([]) ->
    User = set_group_leader(),
    Depth = get_depth(),
    {ok, #st{user=User,prev_handler=[],depth=Depth}}.

get_depth() ->
    case application:get_env(kernel, error_logger_format_depth) of
	{ok, Depth} when is_integer(Depth) ->
	    max(10, Depth);
	undefined ->
	    unlimited
    end.
    
handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event(Event, State) ->
    ok = do_write_event(State, tag_event(Event)),
    {ok, State}.

handle_info({'EXIT', User, _Reason},
	    #st{user=User,prev_handler=PrevHandler}=State) ->
    case PrevHandler of
	[] ->
	    remove_handler;
	_ -> 
	    {swap_handler, install_prev, State,
	     PrevHandler, go_back}
    end;
handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(install_prev, _State) ->
    [];
terminate(_Reason, #st{prev_handler=PrevHandler}) ->
    {error_logger_tty_h, PrevHandler}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Exported (but unoffical) API.
write_event(Event, IoMod) ->
    do_write_event(#st{io_mod=IoMod}, Event).

write_event(Event, IoMod, Depth) ->
    do_write_event(#st{io_mod=IoMod,depth=Depth}, Event).


%%% ------------------------------------------------------
%%% Misc. functions.
%%% ------------------------------------------------------

set_group_leader() ->
    case whereis(user) of
	User when is_pid(User) ->
	    link(User),
	    group_leader(User,self()),
	    User;
	_ ->
	    false
    end.

tag_event(Event) ->    
    {erlang:universaltime(), Event}.

write_events(State, [Ev|Es]) ->
    %% Write the events in reverse order.
    _ = write_events(State, Es),
    _ = do_write_event(State, Ev),
    ok;
write_events(_State, []) ->
    ok.

do_write_event(State, {Time0, Event}) ->
    case parse_event(Event) of
	ignore ->
	    ok;
	{Head,Pid,FormatList} ->
	    Time = maybe_utc(Time0),
	    Header = write_time(Time, Head),
	    Body = format_body(State, FormatList),
	    AtNode = if
			 node(Pid) =/= node() ->
			     ["** at node ",atom_to_list(node(Pid))," **\n"];
			 true ->
			     []
		     end,
	    Str = [Header,Body,AtNode],
	    case State#st.io_mod of
		io_lib ->
		    Str;
		io ->
		    io:put_chars(user, Str)
	    end
    end;
do_write_event(_, _) ->
    ok.

format_body(State, [{Format,Args}|T]) ->
    S = try format(State, Format, Args) of
	    S0 ->
		S0
	catch
	    _:_ ->
		format(State, "ERROR: ~p - ~p\n", [Format,Args])
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

format_term(Term) when is_list(Term) ->
    case string_p(Term) of
	true ->
	    [{"~s\n",[Term]}];
	false ->
	    format_term_list(Term)
    end;
format_term(Term) ->
    [{"~p\n",[Term]}].

format_term_list([{Tag,Data}|T]) ->
    [{"    ~p: ~p\n",[Tag,Data]}|format_term_list(T)];
format_term_list([Data|T]) ->
    [{"    ~p\n",[Data]}|format_term_list(T)];
format_term_list([]) ->
    [];
format_term_list(_) ->
    %% Continue to allow non-proper lists for now.
    %% FIXME: Remove this clause in OTP 19.
    [].

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





