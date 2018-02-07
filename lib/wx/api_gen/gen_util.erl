%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
%% Some utilities

-module(gen_util).
-compile(export_all).

lowercase([F|R]) when F >= $A, F =< $Z ->   [F+($a-$A)|R];
lowercase(Str) when is_list(Str) ->   Str.

lowercase_all([F|R]) when F >= $A, F =< $Z -> [F+($a-$A)|lowercase_all(R)];
lowercase_all([F|R])  ->               [F|lowercase_all(R)];
lowercase_all([]) ->                   [].

uppercase([F|R]) when F >= $a, F =< $z ->    [F+($A-$a)|R];
uppercase(Str) when is_list(Str) ->   Str.

uppercase_all([F|R]) when F >= $a, F =< $z -> [F+($A-$a)|uppercase_all(R)];
uppercase_all([A|R]) ->             [A|uppercase_all(R)];
uppercase_all([]) ->                [].


strip_name([H|R1],[H|R2]) ->
    strip_name(R1,R2);
strip_name(String,[]) -> String.


get_hook(_Type, undefined) -> ignore;
get_hook(Type, List) -> proplists:get_value(Type, List, ignore).

get_taylor_made(Str, Name) ->
    re:run(Str, "<<"++Name++"(.*)"++Name++">>",
	   [dotall, {capture, all_but_first, list}]).

open_write(File) ->
    open_write(File, []).
open_write(File, Opts) ->
    %% io:format("Generating ~s~n",[File]),
    {ok, Fd} = file:open(File++".temp", [write|Opts]),
    put(current_file, {Fd,File}).


close() ->
    case get(current_file) of
	undefined ->
	    ok;
	{closed, File} ->
	    io:format("Closing twice ~s~n",[File]);
	{Fd,File} ->
	    file:close(Fd),
	    case os:cmd("diff " ++ File ++ " " ++ File ++ ".temp" ++ "| head -30") of
		[] ->
		    ok = file:delete(File ++ ".temp"),
		    %% So that make understands that we have made this
		    %% case os:getenv("CLEARCASE_ROOT") of
		    %% 	false -> os:cmd("touch " ++ File);
		    %% 	_ ->  ignore
		    %% end,
		    ok;
		Diff ->
		    case check_diff(Diff) of
			copyright -> %% We ignore copyright changes only
			    ok = file:delete(File ++ ".temp");
			_ ->
			    io:format("Diff in ~s~n~s ~n", [File, Diff]),
			    case file:rename(File ++ ".temp", File) of
				ok -> ok;
				_ ->
				    io:format("*****  Failed to save file ~p ~n",[File])
			    end
		    end
	    end,
	    put(current_file, {closed, File})
    end.


check_diff(Diff) ->
    try
	[_,D1,_,D2|Tail] = re:split(Diff, "\n"),
	case Tail of
	    [] -> ok;
	    [<<>>] -> ok;
	    _ -> throw(diff)
	end,
	copyright(D1),
	copyright(D2),
	copyright
    catch
	throw:_ ->  diff;
	error:{badmatch,_} ->
	    diff;
	_:What:Stacktrace ->
	    io:format("~p:~p: ~p ~p~n", [?MODULE,?LINE, What, Stacktrace]),
	    diff
    end.

copyright(<<_, _, "%% Copyright", _/binary>>) -> ok;
copyright(<<_, _, " * Copyright", _/binary>>) -> ok;
copyright(_) -> throw(diff).

w(Str) ->
    w(Str, []).
w(Str,Args) ->
    {Fd,_} = get(current_file),
    io:format(Fd, Str, Args).

args(Fun, Limit, List) ->
    args(Fun, Limit, List, infinity, 0).

args(Fun, Limit, List, Max) ->
    args(Fun, Limit, List, Max, 0).

args(_Fun, _Limit, [], _Max, _) -> "";  %% No args
args(Fun, _Limit, [Last], _Max, _Pos) ->
    case Fun(Last) of
	skip -> ""; %% FIXME bug if last skips
	Str  -> Str
    end;
args(Fun, Limit, [H|R], Max, Pos) ->
    case Fun(H) of
	skip -> args(Fun,Limit,R, Max, Pos);
	Str  ->
	    {NL, NewPos} =
		case length(Str) + Pos of
		    Curr when Curr > Max ->
			{"\n  ", 0};
		    Curr ->
			{"", Curr}
		end,
	    case args(Fun,Limit,R, Max, NewPos) of
		"" -> Str;
		End -> Str ++ Limit ++ NL ++ End
	    end
    end.




tokens(S) ->
    tokens1(S, " \t\r\n(){}*;,@", []).
tokens(S,Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [C|Toks]);
        false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    replace_and_remove(Toks, []).

tokens2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [C, lists:reverse(Cs) |Toks]);
        false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    replace_and_remove([lists:reverse(Cs)|Toks], []).

replace_and_remove([E|R], Acc) when is_list(E) -> %% Keep everything that is a word
    replace_and_remove(R, [E|Acc]);
replace_and_remove([$\n | R], Acc) ->   %% It is semi line oriented so keep eol
    replace_and_remove(R, [eol|Acc]);
replace_and_remove([$( | R], Acc) ->
    replace_and_remove(R, ["("|Acc]);
replace_and_remove([$) | R], Acc) ->
    replace_and_remove(R, [")"|Acc]);
replace_and_remove([${ | R], Acc) ->
    replace_and_remove(R, ["{"|Acc]);
replace_and_remove([$} | R], Acc) ->
    replace_and_remove(R, ["}"|Acc]);
replace_and_remove([$| | R], Acc) ->
    replace_and_remove(R, ["|"|Acc]);
replace_and_remove([$* | R], Acc) ->
    replace_and_remove(R, ["*"|Acc]);
replace_and_remove([$+ | R], Acc) ->
    replace_and_remove(R, ["+"|Acc]);
replace_and_remove([$& | R], Acc) ->
    replace_and_remove(R, [$&|Acc]);
replace_and_remove([$<,$< | R], Acc) ->
    replace_and_remove(R, ["<<"|Acc]);
replace_and_remove([$, | R], Acc) ->
    replace_and_remove(R, [cont|Acc]);
replace_and_remove([$; | R], Acc) ->
    replace_and_remove(R, [eoe|Acc]);
replace_and_remove([$@ | R], Acc) ->
    replace_and_remove(R, [directive|Acc]);

replace_and_remove([_E|R], Acc) ->       %% Ignore everything else
    replace_and_remove(R, Acc);
replace_and_remove([], Acc) ->
    Acc.

halt(Reason) ->
    case process_info(group_leader(), status) of
	{_,waiting} ->
	    %% Now all output data is down in the driver.
	    %% Give the driver some extra time before halting.
	    receive after 10 -> ok end,
	    erlang:halt(Reason);
	_ ->
	    %% Probably still processing I/O requests.
	    receive after 20 -> ok end,
	    gen_util:halt(Reason)
    end.

erl_copyright() ->
    StartYear = start_year(get(current_class)),
    {CurrentYear,_,_}   = erlang:date(),
    w("%%~n",[]),
    w("%% %CopyrightBegin%~n",[]),
    w("%%~n",[]),
    w("%% Copyright Ericsson AB ~p-~p. All Rights Reserved.~n",
      [StartYear, CurrentYear]),
    w("%%~n",[]),
    w("%% Licensed under the Apache License, Version 2.0 (the \"License\");~n",[]),
    w("%% you may not use this file except in compliance with the License.~n",[]),
    w("%% You may obtain a copy of the License at~n",[]),
    w("%%~n",[]),
    w("%%     http://www.apache.org/licenses/LICENSE-2.0~n",[]),
    w("%%~n",[]),
    w("%% Unless required by applicable law or agreed to in writing, software~n",[]),
    w("%% distributed under the License is distributed on an \"AS IS\" BASIS,~n",[]),
    w("%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.~n",[]),
    w("%% See the License for the specific language governing permissions and~n",[]),
    w("%% limitations under the License.~n",[]),
    w("%%~n",[]),
    w("%% %CopyrightEnd%~n",[]).

c_copyright() ->
    {CurrentYear,_,_}   = erlang:date(),
    w("/*~n",[]),
    w(" * %CopyrightBegin%~n",[]),
    w(" *~n",[]),
    w(" * Copyright Ericsson AB 2008-~p. All Rights Reserved.~n",[CurrentYear]),
    w(" *~n",[]),
    w(" * Licensed under the Apache License, Version 2.0 (the \"License\");~n",[]),
    w(" * you may not use this file except in compliance with the License.~n",[]),
    w(" * You may obtain a copy of the License at~n",[]),
    w(" *~n",[]),
    w(" *     http://www.apache.org/licenses/LICENSE-2.0~n",[]),
    w(" *~n",[]),
    w(" * Unless required by applicable law or agreed to in writing, software~n",[]),
    w(" * distributed under the License is distributed on an \"AS IS\" BASIS,~n",[]),
    w(" * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.~n",[]),
    w(" * See the License for the specific language governing permissions and~n",[]),
    w(" * limitations under the License.~n",[]),
    w(" *~n",[]),
    w(" * %CopyrightEnd% ~n",[]),
    w("*/~n",[]).

start_year("wxAuiManagerEvent") -> 2009;
start_year("wxAuiNotebookEvent") -> 2009;
start_year("wxChoicebook") -> 2009;
start_year("wxGridCellBoolEditor") -> 2009;
start_year("wxGridCellBoolRenderer") -> 2009;
start_year("wxGridCellChoiceEditor") -> 2009;
start_year("wxGridCellFloatEditor") -> 2009;
start_year("wxGridCellFloatRenderer") -> 2009;
start_year("wxGridCellNumberEditor") -> 2009;
start_year("wxGridCellNumberRenderer") -> 2009;
start_year("wxGridCellStringRenderer") -> 2009;
start_year("wxGridCellTextEditor") -> 2009;
start_year("wxHtmlLinkEvent") -> 2009;
start_year("wxHtmlWindow") -> 2009;
start_year("wxListbook") -> 2009;
start_year("wxLogNull") -> 2009;
start_year("wxSpinEvent") -> 2009;
start_year("wxSplitterEvent") -> 2009;
start_year("wxSplitterWindow") -> 2009;
start_year("wxToolbook") -> 2009;
start_year("wxTreebook") -> 2009;
start_year(_) -> 2008.
