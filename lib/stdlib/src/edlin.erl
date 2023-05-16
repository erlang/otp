%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
-module(edlin).

%% A simple Emacs-like line editor.
%% About Latin-1 characters: see the beginning of erl_scan.erl.

-export([init/0,init/1,start/1,start/2,edit_line/2,prefix_arg/1]).
-export([erase_line/0,erase_inp/1,redraw_line/1]).
-export([length_before/1,length_after/1,prompt/1]).
-export([current_line/1, current_chars/1]).

-export([edit_line1/2]).

-import(lists, [reverse/1, reverse/2]).

-export([over_word/3]).


%% A Continuation has the structure:
%%	{line,Prompt,CurrentLine,EditPrefix}

%% init()
%%  Initialise the line editor. This must be done once per process using
%%  the editor.

init() ->
    put(kill_buffer, []).

init(Pid) ->
    %% copy the kill_buffer from the process Pid
    CopiedKillBuf =
	case erlang:process_info(Pid, dictionary) of
	    {dictionary,Dict} ->
		case proplists:get_value(kill_buffer, Dict) of
		    undefined -> [];
		    Buf       -> Buf
		end;
	    undefined ->
		[]
	end,
    put(kill_buffer, CopiedKillBuf).

%% start(Prompt)
%% edit(Characters, Continuation)
%%  Return
%%	{done,Line,Rest,Requests}
%%	{more_chars,Cont,Requests}
%%	{blink,Cont,Requests}
%%	{undefined,Char,Rest,Cont,Requests}

start(Pbs) ->
    start(Pbs, none).

%% Only two modes used: 'none' and 'search'. Other modes can be
%% handled inline through specific character handling.
start(Pbs, {_,{_,_},_}=Cont) ->
    Rs1 = erase_line(),
    Rs2 = redraw(Pbs, Cont, Rs1),
    Rs3 = reverse(Rs2),
    {more_chars,{line,Pbs,Cont,none},Rs3};

start(Pbs, Mode) ->
    {more_chars,{line,Pbs,{[],{[],[]},[]},Mode},[new_prompt, {put_chars,unicode,Pbs}]}.

edit_line(Cs, {line,P,L,{blink,N_Rs}}) ->
    edit(Cs, P, L, none, N_Rs);
edit_line(Cs, {line,P,L,M}) ->
    edit(Cs, P, L, M, []).

edit_line1(Cs, {line,P,L,{blink,N_Rs}}) ->
    edit(Cs, P, L, none, N_Rs);
edit_line1(Cs, {line,P,{B,{[],[]},A},none}) ->
    [CurrentLine|Lines] = [string:to_graphemes(Line) || Line <- reverse(string:split(Cs, "\n",all))],
    Cont = {Lines ++ B,{reverse(CurrentLine),[]},A},
    Rs = reverse(redraw(P, Cont, [])),
    %erlang:display({P, Cont, Cs, CurrentLine}),
    {more_chars, {line,P,Cont,none},[delete_line|Rs]};
edit_line1(Cs, {line,P,L,M}) ->
    edit(Cs, P, L, M, []).

edit([C|Cs], P, Line, {blink,_}, [_|Rs]) ->	%Remove blink here
    edit([C|Cs], P, Line, none, Rs);
edit([C|Cs], P, {LB, {Bef,Aft}, LA}=MultiLine, Prefix, Rs0) ->
    case key_map(C, Prefix) of
        meta ->
            edit(Cs, P, MultiLine, meta, Rs0);
        meta_o ->
            edit(Cs, P, MultiLine, meta_o, Rs0);
        meta_csi ->
            edit(Cs, P, MultiLine, meta_csi, Rs0);
        meta_meta ->
            edit(Cs, P, MultiLine, meta_meta, Rs0);
        {csi, _} = Csi ->
            edit(Cs, P, MultiLine, Csi, Rs0);
        meta_left_sq_bracket ->
            edit(Cs, P, MultiLine, meta_left_sq_bracket, Rs0);
        search_meta ->
            edit(Cs, P, MultiLine, search_meta, Rs0);
        search_meta_left_sq_bracket ->
            edit(Cs, P, MultiLine, search_meta_left_sq_bracket, Rs0);
        ctlx ->
            edit(Cs, P, MultiLine, ctlx, Rs0);
        new_line ->
            case Bef of
                [] -> edit(Cs, P, MultiLine, none, Rs0);
                _ -> MultiLine1 = {[lists:reverse(Bef)|LB],{[],Aft},LA},
                    edit(Cs, P, MultiLine1, none, redraw(P, MultiLine1, Rs0))
                end;
        new_line_finish ->
            [Last|LAR]=LA1 = lists:reverse([lists:reverse(Bef,Aft)|LA]),
            MultiLine1 = {LA1 ++ LB,{[],[]},[]},
            % Move to end and redraw
            Rs1 = redraw(P, {LAR ++ LB, {lists:reverse(Last), []},[]}, Rs0),
            {done, MultiLine1, Cs, reverse(Rs1, [{insert_chars, unicode, "\n"}])};
        redraw_line ->
            Rs1 = erase_line(Rs0),
            Rs = redraw(P, MultiLine, Rs1),
            edit(Cs, P, MultiLine, none, Rs);
        clear ->
            Rs = redraw(P, MultiLine, [clear|Rs0]),
            edit(Cs, P, MultiLine, none, Rs);
        tab_expand ->
            {expand, chars_before(MultiLine), Cs,
            {line, P, MultiLine, tab_expand},
            reverse(Rs0)};
        tab_expand_full ->
            {expand_full, chars_before(MultiLine), Cs,
	        {line, P, MultiLine, tab_expand},
	        reverse(Rs0)};
        {undefined,C} ->
            {undefined,{none,Prefix,C},Cs,{line,P,MultiLine,none},
            reverse(Rs0)};
        Op ->
            case do_op(Op, MultiLine, Rs0) of
                {blink,N,MultiLine1,Rs} ->
                    edit(Cs, P, MultiLine1, {blink,N}, Rs);
                {redraw, MultiLine1, Rs} ->
                    edit(Cs, P, MultiLine1, none, redraw(P, MultiLine1, Rs));
                {MultiLine1, Rs, Mode} -> % allow custom modes from do_op
                    edit(Cs, P, MultiLine1, Mode, Rs);
                {MultiLine1,Rs} ->
                    edit(Cs, P, MultiLine1, none, Rs)
            end
    end;
edit([], P, L, {blink,N}, Rs) ->
    {blink,{line,P,L, {blink,N}},reverse(Rs)};
edit([], P, L, Prefix, Rs) ->
    {more_chars,{line,P,L,Prefix},reverse(Rs)};
edit(eof, _, {_,{Bef,Aft0},LA} = L, _, Rs) ->
    Aft1 = case LA of
        [Last|_] -> Last;
        _ -> Aft0
    end,
    {done,L,[],reverse(Rs, [{move_combo,-cp_len(Bef), length(LA), cp_len(Aft1)}])}.

%% %% Assumes that arg is a string
%% %% Horizontal whitespace only.
%% whitespace_only([]) ->
%%     true;
%% whitespace_only([C|Rest]) ->
%%     case C of
%% 	$\s ->
%% 	    whitespace_only(Rest);
%% 	$\t ->
%% 	    whitespace_only(Rest);
%% 	_ ->
%% 	    false
%%     end.

%% prefix_arg(Argument)
%%  Take a prefix argument and return its numeric value.

prefix_arg(none) -> 1;
prefix_arg({ctlu,N}) -> N;
prefix_arg(N) -> N.

%% key_map(Char, Prefix)
%%  Map a character and a prefix to an action.

key_map(A, _) when is_atom(A) -> A;             % so we can push keywords
key_map($\^A, none) -> beginning_of_line;
key_map($\^B, none) -> backward_char;
key_map($\^D, none) -> forward_delete_char;
key_map($\^E, none) -> end_of_line;
key_map($\^F, none) -> forward_char;
key_map($\^H, none) -> backward_delete_char;
key_map($\t, none) -> tab_expand;
key_map($\t, tab_expand) -> tab_expand_full;
key_map(C, tab_expand) -> key_map(C, none);
key_map($\^K, none) -> kill_line;
key_map($\^L, none) -> clear;
key_map($\n, none) -> new_line_finish;
key_map($\r, none) -> new_line_finish;
key_map($\^T, none) -> transpose_char;
key_map($\^U, none) -> ctlu;
key_map($\^], none) -> auto_blink;
key_map($\^X, none) -> ctlx;
key_map($\^Y, none) -> yank;
key_map($\^W, none) -> backward_kill_word;
key_map($\e, none) -> meta;
key_map($), Prefix) when Prefix =/= meta,
                         Prefix =/= search,
                         Prefix =/= search_meta -> {blink,$),$(};
key_map($}, Prefix) when Prefix =/= meta,
                         Prefix =/= search,
                         Prefix =/= search_meta -> {blink,$},${};
key_map($], Prefix) when Prefix =/= meta,
                         Prefix =/= search,
                         Prefix =/= search_meta -> {blink,$],$[};
key_map($B, meta) -> backward_word;
key_map($D, meta) -> kill_word;
key_map($F, meta) -> forward_word;
key_map($L, meta) -> redraw_line;
key_map($T, meta) -> transpose_word;
key_map($Y, meta) -> yank_pop;
key_map($b, meta) -> backward_word;
key_map($c, meta) -> clear_line;
key_map($d, meta) -> kill_word;
key_map($f, meta) -> forward_word;
key_map($l, meta) -> redraw_line;
key_map($t, meta) -> transpose_word;
key_map($y, meta) -> yank_pop;
key_map($<, meta) -> beginning_of_expression;
key_map($>, meta) -> end_of_expression;
key_map($\n, meta) -> new_line;
key_map($\r, meta) -> new_line;
key_map($O, meta) -> meta_o;
key_map($H, meta_o) -> beginning_of_line;
key_map($F, meta_o) -> end_of_line;
key_map($\177, none) -> backward_delete_char;
key_map($\177, meta) -> backward_kill_word;
key_map($[, meta) -> meta_left_sq_bracket;
key_map($H, meta_left_sq_bracket) -> beginning_of_line;
key_map($F, meta_left_sq_bracket) -> end_of_line;
key_map($D, meta_left_sq_bracket) -> backward_char;
key_map($C, meta_left_sq_bracket) -> forward_char;
% support a few <CTRL/ALT>+<CURSOR> combinations...
%  - forward:  \e\e[C, \e[5C, \e[1;5C
%  - backward: \e\e[D, \e[5D, \e[1;5D
key_map($\e, meta) -> meta_meta;
key_map($[, meta_meta) -> meta_csi;
key_map($C, meta_csi) -> forward_word;
key_map($D, meta_csi) -> backward_word;
key_map($1, meta_left_sq_bracket) -> {csi, "1"};
key_map($3, meta_left_sq_bracket) -> {csi, "3"};
key_map($C, {csi, "3"}) -> forward_word;
key_map($D, {csi, "3"})  -> backward_word;
key_map($~, {csi, "3"}) -> forward_delete_char;
key_map($5, meta_left_sq_bracket) -> {csi, "5"};
key_map($C, {csi, "5"}) -> forward_word;
key_map($D, {csi, "5"})  -> backward_word;
key_map($;, {csi, "1"}) -> {csi, "1;"};
key_map($3, {csi, "1;"}) -> {csi, "1;3"};
key_map($C, {csi, "1;3"}) -> forward_word;
key_map($D, {csi, "1;3"}) -> backward_word;
key_map($A, {csi, "1;3"}) -> backward_line;
key_map($B, {csi, "1;3"}) -> forward_line;
key_map($4, {csi, "1;"}) -> {csi, "1;4"};
key_map($A, {csi, "1;4"}) -> beginning_of_expression;
key_map($B, {csi, "1;4"}) -> end_of_expression;
key_map($5, {csi, "1;"}) -> {csi, "1;5"};
key_map($C, {csi, "1;5"}) -> forward_word;
key_map($D, {csi, "1;5"}) -> backward_word;
key_map($A, {csi, "1;5"}) -> backward_line;
key_map($B, {csi, "1;5"}) -> forward_line;





key_map(C, none) when C >= $\s ->
    {insert,C};
%% for search, we need smarter line handling and so
%% we cheat a bit on the dispatching, and allow to
%% return a mode.
key_map($\^H, search) -> {search, backward_delete_char};
key_map($\177, search) -> {search, backward_delete_char};
key_map($\^R, search) -> {search, skip_up};
key_map($\^S, search) -> {search, skip_down};
key_map($\n, search) -> {search, search_found};
key_map($\r, search) -> {search, search_found};
key_map($\^A, search) -> {search, search_quit};
key_map($\^B, search) -> {search, search_quit};
key_map($\^D, search) -> {search, search_quit};
key_map($\^E, search) -> {search, search_quit};
key_map($\^F, search) -> {search, search_quit};
key_map($\t,  search) -> {search, search_quit};
key_map($\^L, search) -> {search, search_quit};
key_map($\^T, search) -> {search, search_quit};
key_map($\^U, search) -> {search, search_quit};
key_map($\^], search) -> {search, search_quit};
key_map($\^X, search) -> {search, search_quit};
key_map($\^Y, search) -> {search, search_quit};
key_map($\e,  search) -> search_meta;
key_map($c, search_meta) -> {search, search_cancel};
key_map($C, search_meta) -> {search, search_cancel};
key_map($[,  search_meta) -> search_meta_left_sq_bracket;
key_map(_, search_meta) -> {search, search_quit};
key_map(_C, search_meta_left_sq_bracket) -> {search, search_quit};
key_map(C, search) -> {insert_search,C};
key_map(C, _) -> {undefined,C}.

%% do_op(Action, Before, After, Requests)
%% Before and After are of lists of type string:grapheme_cluster()
do_op({insert,C}, {LB,{[],[]},LA}, Rs) ->
    {{LB,{[C],[]},LA},[{insert_chars, unicode,[C]}|Rs]};
do_op({insert,C}, {LB,{[Bef|Bef0], []},LA}, Rs) ->
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{LB,{[GC|Bef0],[]},LA},[{insert_chars, unicode,[C]}|Rs]};
        _ -> {{LB,{[C,Bef|Bef0],[]},LA},[{insert_chars, unicode,[C]}|Rs]}
    end;
do_op({insert,C}, {LB,{[], Aft},LA}, Rs) ->
    {{LB,{[C],Aft},LA},[{insert_chars, unicode,[C]}|Rs]};
do_op({insert,C}, {LB,{[Bef|Bef0], Aft},LA}, Rs) ->
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{LB,{[GC|Bef0],Aft},LA},[{insert_chars, unicode,[C]}|Rs]};
        _ -> {{LB,{[C,Bef|Bef0],Aft},LA},[{insert_chars, unicode,[C]}|Rs]}
    end;
%% Search mode prompt always looks like (search)`$TERMS': $RESULT.
%% the {insert_search, _} handlings allow to share this implementation
%% correctly with group.erl. This module provides $TERMS, and group.erl
%% is in charge of providing $RESULT.
%% This require a bit of trickery. Because search disables moving around
%% on the line (left/right arrow keys and other shortcuts that just exit
%% search mode), we can use the Bef and Aft variables to hold each
%% part of the line. Bef takes charge of "(search)`$TERMS" and Aft
%% takes charge of "': $RESULT".
%% 
%% Since multiline support the search mode prompt always looks like:
%% search: $TERMS
%%   $ResultLine1
%%   $ResultLine2
do_op({insert_search, C}, {LB,{Bef, []},LA}, Rs) ->
    {{LB, {[C|Bef],[]}, LA},
     [{insert_chars, unicode, [C]}, delete_after_cursor | Rs], search};
do_op({insert_search, C}, {LB,{Bef, _Aft},LA}, Rs) ->
    {{LB, {[C|Bef],[]}, LA},
     [{insert_chars, unicode, [C]}, delete_after_cursor | Rs],
     search};
do_op({search, backward_delete_char}, {LB,{[_|Bef], Aft},LA}, Rs) ->
    Offset= cp_len(Aft)+1,
    {{LB, {Bef,Aft}, LA},
     [{insert_chars, unicode, Aft}, {delete_chars,-Offset}|Rs],
     search};
do_op({search, backward_delete_char}, {LB,{[], Aft},LA}, Rs) ->
    {{LB, {[],Aft}, LA}, [{insert_chars, unicode, Aft}, {delete_chars,-cp_len(Aft)}|Rs], search};
do_op({search, skip_up}, {_,{Bef, Aft},_}, Rs) ->
    Offset= cp_len(Aft),
    {{[],{[$\^R|Bef],Aft},[]}, % we insert ^R as a flag to whoever called us
     [{insert_chars, unicode, Aft}, {delete_chars,-Offset}|Rs],
     search};
do_op({search, skip_down}, {_,{Bef, Aft},_LA}, Rs) ->
    Offset= cp_len(Aft),
    {{[],{[$\^S|Bef],Aft},[]}, % we insert ^S as a flag to whoever called us
     [{insert_chars, unicode, Aft}, {delete_chars,-Offset}|Rs],
     search};
do_op({search, search_found}, {_,{_Bef, Aft},LA}, Rs) ->
    {{[],{[],Aft},LA}, Rs, search_found};
do_op({search, search_quit}, {_,{_Bef, Aft},LA}, Rs) ->
    {{[],{[],Aft},LA}, Rs, search_quit};
do_op({search, search_cancel}, _, Rs) ->
    {{[],{[],[]},[]}, Rs, search_cancel};
%% do blink after $$
do_op({blink,C,M}, {_,{[$$,$$|_], _},_} = MultiLine, Rs) ->
    blink(over_paren(chars_before(MultiLine), C, M), C, MultiLine, Rs);
%% don't blink after a $
do_op({blink,C,_}, {_,{[$$|_], _},_} = MultiLine, Rs) ->
    do_op({insert,C}, MultiLine, Rs);
do_op({blink,C,M}, MultiLine, Rs) ->
    blink(over_paren(chars_before(MultiLine), C, M), C, MultiLine, Rs);
do_op(auto_blink, MultiLine, Rs) ->
    blink(over_paren_auto(chars_before(MultiLine)), MultiLine, Rs);
do_op(forward_delete_char, {LB,{Bef, []},[NextLine|LA]}, Rs) ->
    NewLine = {LB, {Bef, NextLine}, LA},
    {redraw, NewLine, Rs};
do_op(forward_delete_char, {LB,{Bef, [GC|Aft]},LA}, Rs) ->
    {{LB, {Bef,Aft}, LA},[{delete_chars,gc_len(GC)}|Rs]};
do_op(backward_delete_char, {[PrevLine|LB],{[], Aft},LA}, Rs) ->
    NewLine = {LB, {lists:reverse(PrevLine), Aft}, LA},
    {redraw, NewLine,Rs};
do_op(backward_delete_char, {LB,{[GC|Bef], Aft},LA}, Rs) ->
    {{LB, {Bef,Aft}, LA},[{delete_chars,-gc_len(GC)}|Rs]};
do_op(transpose_char, {LB,{[C1,C2|Bef], []},LA}, Rs) ->
    Len = gc_len(C1)+gc_len(C2),
    {{LB, {[C2,C1|Bef],[]}, LA},[{put_chars, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(transpose_char, {LB,{[C2|Bef], [C1|Aft]},LA}, Rs) ->
    Len = gc_len(C2),
    {{LB, {[C2,C1|Bef],Aft}, LA},[{put_chars, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(kill_word, {LB,{Bef, Aft0},LA}, Rs) ->
    {Aft1,Kill0,N0} = over_non_word(Aft0, [], 0),
    {Aft,Kill,N} = over_word(Aft1, Kill0, N0),
    put(kill_buffer, reverse(Kill)),
    {{LB, {Bef,Aft}, LA},[{delete_chars,N}|Rs]};
do_op(backward_kill_word, {LB,{Bef0, Aft},LA}, Rs) ->
    {Bef1,Kill0,N0} = over_non_word(Bef0, [], 0),
    {Bef,Kill,N} = over_word(Bef1, Kill0, N0),
    put(kill_buffer, Kill),
    {{LB,{Bef,Aft},LA},[{delete_chars,-N}|Rs]};
do_op(kill_line, {LB, {Bef, Aft}, LA}, Rs) ->
    put(kill_buffer, Aft),
    {{LB, {Bef,[]}, LA},[{delete_chars,cp_len(Aft)}|Rs]};
do_op(clear_line, _, Rs) ->
    {redraw, {[], {[],[]},[]}, Rs};
do_op(yank, {LB,{Bef, []},LA}, Rs) ->
    Kill = get(kill_buffer),
    {{LB, {reverse(Kill, Bef),[]}, LA},[{put_chars, unicode,Kill}|Rs]};
do_op(yank, {LB,{Bef, Aft},LA}, Rs) ->
    Kill = get(kill_buffer),
    {{LB, {reverse(Kill, Bef),Aft}, LA},[{insert_chars, unicode,Kill}|Rs]};
do_op(forward_line, {_,_,[]} = MultiLine, Rs) ->
    {MultiLine, Rs};
do_op(forward_line, {LB,{Bef, Aft},[AL|LA]}, Rs) ->
    CL = lists:reverse(Bef, Aft),
    CursorPos = min(length(Bef), length(AL)),
    {Bef1, Aft1} = lists:split(CursorPos, AL),
    {{[CL|LB], {lists:reverse(Bef1), Aft1}, LA}, [{move_combo, -cp_len(Bef), 1, cp_len(Bef1)}|Rs]};
do_op(backward_line, {[], _, _} = MultiLine, Rs) ->
    {MultiLine, Rs};
do_op(backward_line, {[BL|LB],{Bef, Aft},LA}, Rs) ->
    CL = lists:reverse(Bef, Aft),
    CursorPos = min(length(Bef), length(BL)),
    {Bef1, Aft1} = lists:split(CursorPos, BL),
    {{LB, {lists:reverse(Bef1), Aft1}, [CL|LA]},[{move_combo, -cp_len(Bef), -1, cp_len(Bef1)}|Rs]};
do_op(forward_char, {LB,{Bef, []}, [AL|LA]}, Rs) ->
    {{[lists:reverse(Bef)|LB],{[], string:to_graphemes(AL)}, LA}, [{move_combo, -cp_len(Bef), 1, 0}|Rs]};
do_op(forward_char, {LB,{Bef, [C|Aft]},LA}, Rs) ->
    {{LB,{[C|Bef],Aft},LA},[{move_rel,gc_len(C)}|Rs]};
do_op(backward_char, {[BL|LB],{[], Aft},LA}, Rs) ->
    {{LB,{lists:reverse(string:to_graphemes(BL)), []}, [Aft|LA]}, [{move_combo, 0, -1, cp_len(BL)}|Rs]};
do_op(backward_char, {LB,{[C|Bef], Aft},LA}, Rs) ->
    {{LB, {Bef,[C|Aft]}, LA},[{move_rel,-gc_len(C)}|Rs]};
do_op(forward_word, {LB,{Bef0, []},[NextLine|LA]}, Rs) ->
    {{[reverse(Bef0)|LB], {[], NextLine}, LA},[{move_combo, -cp_len(Bef0), 1, 0}|Rs]};
do_op(forward_word, {LB,{Bef0, Aft0},LA}, Rs) ->
    {Aft1,Bef1,N0} = over_non_word(Aft0, Bef0, 0),
    {Aft, Bef, N} = over_word(Aft1, Bef1, N0),
    {{LB, {Bef,Aft}, LA},[{move_rel,N}|Rs]};
do_op(backward_word, {[PrevLine|LB],{[], Aft0},LA}, Rs) ->
    {{LB, {reverse(PrevLine), []}, [Aft0|LA]},[{move_combo, 0, -1, cp_len(PrevLine)}|Rs]};
do_op(backward_word, {LB,{Bef0, Aft0},LA}, Rs) ->
    {Bef1,Aft1,N0} = over_non_word(Bef0, Aft0, 0),
    {Bef,Aft,N} = over_word(Bef1, Aft1, N0),
    {{LB, {Bef,Aft}, LA},[{move_rel,-N}|Rs]};
do_op(beginning_of_expression, {[],{[], Aft},LA}, Rs) ->
    {{[], {[],Aft}, LA},Rs};
do_op(beginning_of_expression, {LB,{Bef, Aft},LA}, Rs) ->
    [First|Rest] = lists:reverse(LB) ++ [lists:reverse(Bef, Aft)],
    {{[], {[],First}, Rest ++ LA},[{move_combo, -cp_len(Bef), -length(LB), 0}|Rs]};
do_op(end_of_expression, {LB,{Bef, []},[]}, Rs) ->
    {{LB, {Bef,[]}, []},Rs};
do_op(end_of_expression, {LB,{Bef, Aft},LA}, Rs) ->
    [Last|Rest] = lists:reverse(LA) ++ [lists:reverse(Bef, Aft)],
    {{LB ++ Rest, {lists:reverse(Last),[]}, []},[{move_combo, -cp_len(Bef), length(LA), cp_len(Last)}|Rs]};
do_op(beginning_of_line, {LB,{[_|_]=Bef, Aft},LA}, Rs) ->
    {{LB, {[],reverse(Bef, Aft)}, LA},[{move_rel,-(cp_len(Bef))}|Rs]};
do_op(beginning_of_line, {LB,{[], Aft},LA}, Rs) ->
    {{LB, {[],Aft}, LA},Rs};
do_op(end_of_line, {LB,{Bef, [_|_]=Aft},LA}, Rs) ->
    {{LB, {reverse(Aft, Bef),[]}, LA},[{move_rel,cp_len(Aft)}|Rs]};
do_op(end_of_line, {LB,{Bef, []},LA}, Rs) ->
    {{LB, {Bef,[]}, LA},Rs};
do_op(ctlu, {LB,{Bef, Aft},LA}, Rs) ->
    put(kill_buffer, reverse(Bef)),
    {{LB, {[], Aft}, LA}, [{delete_chars, -cp_len(Bef)} | Rs]};
do_op(beep, {LB,{Bef, Aft},LA}, Rs) ->
    {{LB,{Bef,Aft},LA},[beep|Rs]};
do_op(_, {LB,{Bef, Aft},LA}, Rs) ->
    {{LB,{Bef,Aft},LA},[beep|Rs]}.

blink(beep, C, {LB, {Bef, Aft}, LA}, Rs) ->
    {{LB,{[C|Bef], Aft},LA}, [beep,{insert_chars, unicode, [C]}|Rs]};
blink({N, R}, C, MultiLine, Rs) ->
    blink({N, R, C}, MultiLine, Rs).
%% same line
blink(beep, {LB,{Bef, Aft},LA}, Rs) ->
    {{LB,{Bef, Aft},LA}, [beep|Rs]};
blink({N, 0, Paren}, {LB, {Bef, Aft}, LA}, Rs) ->
    MoveBackToParen = {move_rel,-N-1},
    MoveForwardToParen = {move_rel, N+1},
    {blink,[MoveForwardToParen],{LB,{[Paren|Bef],Aft},LA},
        [MoveBackToParen,{insert_chars, unicode,[Paren]}|Rs]};
%% multiline
blink({N, R, Paren}, {LB,{Bef, Aft},LA}, Rs) ->
    LengthToClosingParen = cp_len([Paren|Bef]),
    LengthOpeningParen = cp_len(lists:nth(R,LB)) - N - 1,
    MoveToOpeningParen = {move_combo, -LengthToClosingParen, -R, LengthOpeningParen},
    MoveToClosingParen = {move_combo, -LengthOpeningParen, R, LengthToClosingParen+1},
    {blink,[MoveToClosingParen],{LB,{[Paren|Bef],Aft},LA},
        [MoveToOpeningParen,{insert_chars, unicode,[Paren]}|Rs]}.

%% over_word(Chars, InitialStack, InitialCount) ->
%%	{RemainingChars,CharStack,Count}
%% over_non_word(Chars, InitialStack, InitialCount) ->
%%	{RemainingChars,CharStack,Count}
%%  Step over word/non-word characters pushing the stepped over ones on
%%  the stack.
over_word(Cs, Stack, N) ->
    L = length([1 || $\' <- Cs]),
    case L rem 2 of
	0 ->
	    over_word1(Cs, Stack, N);
	1 ->
	    until_quote(Cs, Stack, N)
    end.

until_quote([$\'|Cs], Stack, N) ->
    {Cs, [$\'|Stack], N+1};
until_quote([C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+gc_len(C)).

over_word1([$\'=C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+1);
over_word1(Cs, Stack, N) ->
    over_word2(Cs, Stack, N).

over_word2([C|Cs], Stack, N) ->
    case word_char(C) of
	true -> over_word2(Cs, [C|Stack], N+gc_len(C));
	false -> {[C|Cs],Stack,N}
    end;
over_word2([], Stack, N) when is_integer(N) ->
    {[],Stack,N}.

over_non_word([C|Cs], Stack, N) ->
    case word_char(C) of
	true -> {[C|Cs],Stack,N};
	false -> over_non_word(Cs, [C|Stack], N+gc_len(C))
    end;
over_non_word([], Stack, N) ->
    {[],Stack,N}.

word_char(C) when C >= $A, C =< $Z -> true;
word_char(C) when C >= $À, C =< $Þ, C =/= $× -> true;
word_char(C) when C >= $a, C =< $z -> true;
word_char(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
word_char(C) when C >= $0, C =< $9 -> true;
word_char(C) when C =:= $_ -> true;
word_char([_|_]) -> true; %% Is grapheme
word_char(_) -> false.

%% over_white(Chars, InitialStack, InitialCount) ->
%%	{RemainingChars,CharStack,Count}

%% over_white([$\s|Cs], Stack, N) ->
%%     over_white(Cs, [$\s|Stack], N+1);
%% over_white([$\t|Cs], Stack, N) ->
%%     over_white(Cs, [$\t|Stack], N+1);
%% over_white(Cs, Stack, N) ->
%%     {Cs,Stack,N}.

%% over_paren(Chars, Paren, Match)
%% over_paren(Chars, Paren, Match, Depth, N)
%%  Step over parentheses until matching Paren is found at depth 0. Don't
%%  do proper parentheses matching check. Paren has NOT been added.

over_paren(Chars, Paren, Match) ->
    over_paren(Chars, Paren, Match, 1, 1, 0, []).


over_paren([C,$$,$$|Cs], Paren, Match, D, N, R, L)  ->
    over_paren([C|Cs], Paren, Match, D, N+2, R, L);
over_paren([GC,$$|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1+gc_len(GC), R, L);
over_paren([$\n|Cs], Paren, Match, D, _N, R, L) ->
    over_paren(Cs, Paren, Match, D, 0, R+1, L);
over_paren([Match|_], _Paren, Match, 1, N, R, _) ->
    {N, R};
over_paren([Match|Cs], Paren, Match, D, N, R, [Match|L]) ->
    over_paren(Cs, Paren, Match, D-1, N+1, R, L);
over_paren([Paren|Cs], Paren, Match, D, N, R, L) ->
    over_paren(Cs, Paren, Match, D+1, N+1, R, [Match|L]);

over_paren([$)|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, R, [$(|L]);
over_paren([$]|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, R, [$[|L]);
over_paren([$}|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, R, [${|L]);

over_paren([$(|Cs], Paren, Match, D, N, R, [$(|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, R, L);
over_paren([$[|Cs], Paren, Match, D, N, R, [$[|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, R, L);
over_paren([${|Cs], Paren, Match, D, N, R, [${|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, R, L);

over_paren([$(|_], _, _, _, _, _, _)  ->
    beep;
over_paren([$[|_], _, _, _, _, _, _)  ->
    beep;
over_paren([${|_], _, _, _, _, _, _)  ->
    beep;

over_paren([GC|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+gc_len(GC), R, L);
over_paren([], _, _, _, _, _, _) ->
    beep.

over_paren_auto(Chars) ->
    over_paren_auto(Chars, 1, 1, 0, []).


over_paren_auto([C,$$,$$|Cs], D, N, R, L)  ->
    over_paren_auto([C|Cs], D, N+2, R, L);
over_paren_auto([GC,$$|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1+gc_len(GC), R, L);
over_paren_auto([$\n|Cs], D, _N, R, L) ->
    over_paren_auto(Cs, D, 0, R+1, L);

over_paren_auto([$(|_], _, N, R, [])  ->
    {N, R, $)};
over_paren_auto([$[|_], _, N, R, [])  ->
    {N, R, $]};
over_paren_auto([${|_], _, N, R, [])  ->
    {N, R, $}};

over_paren_auto([$)|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1, R, [$(|L]);
over_paren_auto([$]|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1, R, [$[|L]);
over_paren_auto([$}|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1, R, [${|L]);

over_paren_auto([$(|Cs], D, N, R, [$(|L])  ->
    over_paren_auto(Cs, D, N+1, R, L);
over_paren_auto([$[|Cs], D, N, R, [$[|L])  ->
    over_paren_auto(Cs, D, N+1, R, L);
over_paren_auto([${|Cs], D, N, R, [${|L])  ->
    over_paren_auto(Cs, D, N+1, R, L);

over_paren_auto([GC|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+gc_len(GC), R, L);
over_paren_auto([], _, _, _, _) ->
    beep.

%% erase_line(Line)
%% erase_inp(Line)
%% redraw_line(Line)
%% length_before(Line)
%% length_after(Line)
%% prompt(Line)
%% current_line(Line)
%% current_chars(Line)
%%  Various functions for accessing bits of a line.

erase_line() ->
    [delete_line].

erase_inp({line,_, L,_}) ->
    reverse(erase([], L, [])).

erase_line(Rs) ->
    [delete_line|Rs].

erase(Pbs, {_,{Bef, Aft},_}, Rs) ->
    [{delete_chars,-cp_len(Pbs)-cp_len(Bef)},{delete_chars,cp_len(Aft)}|Rs].

redraw_line({line, Pbs, L,_}) ->
    redraw(Pbs, L, []).

multi_line_prompt(Pbs) ->
    lists:duplicate(max(0,prim_tty:npwcwidthstring(Pbs)-3), $ )++".. ".

redraw(Pbs, {_,{_,_},_}=L, Rs) ->
    [{redraw_prompt, Pbs, multi_line_prompt(Pbs), L} |Rs].

chars_before({[],{Bef,_},_}) ->
    Bef;
chars_before({LB,{Bef,_},_}) ->
    lists:flatten(lists:join($\n, [Bef| [reverse(Line)|| Line <- LB]])).

length_before({line,Pbs,{_,{Bef,_Aft},_},_}) ->
    cp_len(Pbs) + cp_len(Bef).

length_after({line,_,{_,{_Bef,Aft},_},_}) ->
    cp_len(Aft).

prompt({line,Pbs,_,_}) ->
    Pbs.

current_chars({line,_,MultiLine,_}) ->
    current_line(MultiLine).
current_line({line,_,MultiLine,_}) ->
    current_line(MultiLine) ++ "\n";
%% Convert a multiline tuple into a string with new lines
current_line({LinesBefore, {Before, After}, LinesAfter}) ->
    CurrentLine = lists:reverse(Before, After),
    unicode:characters_to_list(lists:flatten(
        lists:filter(
            fun (X) ->
                    X /= []
                end,
                lists:join($\n, lists:reverse(LinesBefore) ++ [CurrentLine] ++ LinesAfter)))).

%% Grapheme length in codepoints
gc_len(CP) when is_integer(CP) -> 1;
gc_len(CPs) when is_list(CPs) -> length(CPs).

%% String length in codepoints
cp_len(Str) ->
    cp_len(Str, 0).

cp_len([GC|R], Len) ->
    cp_len(R, Len + gc_len(GC));
cp_len([], Len) -> Len.
