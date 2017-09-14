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
-module(edlin).

%% A simple Emacs-like line editor.
%% About Latin-1 characters: see the beginning of erl_scan.erl.

-export([init/0,init/1,start/1,start/2,edit_line/2,prefix_arg/1]).
-export([erase_line/1,erase_inp/1,redraw_line/1]).
-export([length_before/1,length_after/1,prompt/1]).
-export([current_line/1, current_chars/1]).
%%-export([expand/1]).

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
start(Pbs, Mode) ->
    {more_chars,{line,Pbs,{[],[]},Mode},[{put_chars,unicode,Pbs}]}.

edit_line(Cs, {line,P,L,{blink,N}}) ->
    edit(Cs, P, L, none, [{move_rel,N}]);
edit_line(Cs, {line,P,L,M}) ->
    edit(Cs, P, L, M, []).

edit_line1(Cs, {line,P,L,{blink,N}}) ->
    edit(Cs, P, L, none, [{move_rel,N}]);
edit_line1(Cs, {line,P,{[],[]},none}) ->
    {more_chars, {line,P,{string:reverse(Cs),[]},none},[{put_chars, unicode, Cs}]};
edit_line1(Cs, {line,P,L,M}) ->
    edit(Cs, P, L, M, []).

edit([C|Cs], P, Line, {blink,_}, [_|Rs]) ->	%Remove blink here
    edit([C|Cs], P, Line, none, Rs);
edit([C|Cs], P, {Bef,Aft}, Prefix, Rs0) ->
    case key_map(C, Prefix) of
	meta ->
	    edit(Cs, P, {Bef,Aft}, meta, Rs0);
        meta_o ->
            edit(Cs, P, {Bef,Aft}, meta_o, Rs0);
        meta_csi ->
            edit(Cs, P, {Bef,Aft}, meta_csi, Rs0);
        meta_meta ->
            edit(Cs, P, {Bef,Aft}, meta_meta, Rs0);
        {csi, _} = Csi ->
            edit(Cs, P, {Bef,Aft}, Csi, Rs0);
	meta_left_sq_bracket ->
	    edit(Cs, P, {Bef,Aft}, meta_left_sq_bracket, Rs0);
	search_meta ->
	    edit(Cs, P, {Bef,Aft}, search_meta, Rs0);
	search_meta_left_sq_bracket ->
	    edit(Cs, P, {Bef,Aft}, search_meta_left_sq_bracket, Rs0);
	ctlx ->
	    edit(Cs, P, {Bef,Aft}, ctlx, Rs0);
	new_line ->
	    {done, get_line(Bef, Aft ++ "\n"), Cs,
	     reverse(Rs0, [{move_rel,cp_len(Aft)},{put_chars,unicode,"\n"}])};
	redraw_line ->
	    Rs1 = erase(P, Bef, Aft, Rs0),
	    Rs = redraw(P, Bef, Aft, Rs1),
	    edit(Cs, P, {Bef,Aft}, none, Rs);
	tab_expand ->
	    {expand, Bef, Cs,
	     {line, P, {Bef, Aft}, none},
	     reverse(Rs0)};

%% 	tab ->
%% 	    %% Always redraw the line since expand/1 might have printed
%% 	    %% possible expansions.
%% 	    case expand(Bef) of
%% 		{yes,Str} ->
%% 		    edit([redraw_line|
%% 			  (Str ++ Cs)], P, {Bef,Aft}, none, Rs0);
%% 		no ->
%% 		    %% don't beep if there's only whitespace before
%% 		    %% us - user may have pasted in a lot of indented stuff.
%% 		    case whitespace_only(Bef) of
%% 			false ->
%% 			    edit([redraw_line|Cs], P, {Bef,Aft}, none,
%% 				 [beep|Rs0]);
%% 			true ->
%% 			    edit([redraw_line|Cs], P, {Bef,Aft}, none, [Rs0])
%% 		    end
%% 	    end;
	{undefined,C} ->
	    {undefined,{none,Prefix,C},Cs,{line,P,{Bef,Aft},none},
	     reverse(Rs0)};
	Op ->
	    case do_op(Op, Bef, Aft, Rs0) of
		{blink,N,Line,Rs} ->
		    edit(Cs, P, Line, {blink,N}, Rs);
		{Line, Rs, Mode} -> % allow custom modes from do_op
		    edit(Cs, P, Line, Mode, Rs);
		{Line,Rs} ->
		    edit(Cs, P, Line, none, Rs)
	    end
    end;
edit([], P, L, {blink,N}, Rs) ->
    {blink,{line,P,L,{blink,N}},reverse(Rs)};
edit([], P, L, Prefix, Rs) ->
    {more_chars,{line,P,L,Prefix},reverse(Rs)};
edit(eof, _, {Bef,Aft}, _, Rs) ->
    {done,get_line(Bef, Aft),[],reverse(Rs, [{move_rel,cp_len(Aft)}])}.

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

key_map(A, _) when is_atom(A) -> A;		% so we can push keywords
key_map($\^A, none) -> beginning_of_line;
key_map($\^B, none) -> backward_char;
key_map($\^D, none) -> forward_delete_char;
key_map($\^E, none) -> end_of_line;
key_map($\^F, none) -> forward_char;
key_map($\^H, none) -> backward_delete_char;
key_map($\t, none) -> tab_expand;
key_map($\^L, none) -> redraw_line;
key_map($\n, none) -> new_line;
key_map($\^K, none) -> kill_line;
key_map($\r, none) -> new_line;
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
key_map($T, meta) -> transpose_word;
key_map($Y, meta) -> yank_pop;
key_map($b, meta) -> backward_word;
key_map($d, meta) -> kill_word;
key_map($f, meta) -> forward_word;
key_map($t, meta) -> transpose_word;
key_map($y, meta) -> yank_pop;
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
% support a few <CTRL>+<CURSOR LEFT|RIGHT> combinations...
%  - forward:  \e\e[C, \e[5C, \e[1;5C
%  - backward: \e\e[D, \e[5D, \e[1;5D
key_map($\e, meta) -> meta_meta;
key_map($[, meta_meta) -> meta_csi;
key_map($C, meta_csi) -> forward_word;
key_map($D, meta_csi) -> backward_word;
key_map($1, meta_left_sq_bracket) -> {csi, "1"};
key_map($3, meta_left_sq_bracket) -> {csi, "3"};
key_map($5, meta_left_sq_bracket) -> {csi, "5"};
key_map($5, {csi, "1;"}) -> {csi, "1;5"};
key_map($~, {csi, "3"}) -> forward_delete_char;
key_map($C, {csi, "5"}) -> forward_word;
key_map($C, {csi, "1;5"}) -> forward_word;
key_map($D, {csi, "5"})  -> backward_word;
key_map($D, {csi, "1;5"}) -> backward_word;
key_map($;, {csi, "1"}) -> {csi, "1;"};
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
key_map($[,  search_meta) -> search_meta_left_sq_bracket;
key_map(_, search_meta) -> {search, search_quit};
key_map(_C, search_meta_left_sq_bracket) -> {search, search_quit};
key_map(C, search) -> {insert_search,C};
key_map(C, _) -> {undefined,C}.

%% do_op(Action, Before, After, Requests)
%% Before and After are of lists of type string:grapheme_cluster()
do_op({insert,C}, [], [], Rs) ->
    {{[C],[]},[{put_chars, unicode,[C]}|Rs]};
do_op({insert,C}, [Bef|Bef0], [], Rs) ->
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{[GC|Bef0],[]},[{put_chars, unicode,[C]}|Rs]};
        _ -> {{[C,Bef|Bef0],[]},[{put_chars, unicode,[C]}|Rs]}
    end;
do_op({insert,C}, [], Aft, Rs) ->
    {{[C],Aft},[{insert_chars, unicode,[C]}|Rs]};
do_op({insert,C}, [Bef|Bef0], Aft, Rs) ->
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{[GC|Bef0],Aft},[{insert_chars, unicode,[C]}|Rs]};
        _ -> {{[C,Bef|Bef0],Aft},[{insert_chars, unicode,[C]}|Rs]}
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
do_op({insert_search, C}, Bef, [], Rs) ->
    Aft="': ",
    {{[C|Bef],Aft},
     [{insert_chars, unicode, [C]++Aft}, {delete_chars,-3} | Rs],
     search};
do_op({insert_search, C}, Bef, Aft, Rs) ->
    Offset= cp_len(Aft),
    NAft = "': ",
    {{[C|Bef],NAft},
     [{insert_chars, unicode, [C]++NAft}, {delete_chars,-Offset} | Rs],
     search};
do_op({search, backward_delete_char}, [_|Bef], Aft, Rs) ->
    Offset= cp_len(Aft)+1,
    NAft = "': ",
    {{Bef,NAft},
     [{insert_chars, unicode, NAft}, {delete_chars,-Offset}|Rs],
     search};
do_op({search, backward_delete_char}, [], _Aft, Rs) ->
    Aft="': ",
    {{[],Aft}, Rs, search};
do_op({search, skip_up}, Bef, Aft, Rs) ->
    Offset= cp_len(Aft),
    NAft = "': ",
    {{[$\^R|Bef],NAft}, % we insert ^R as a flag to whoever called us
     [{insert_chars, unicode, NAft}, {delete_chars,-Offset}|Rs],
     search};
do_op({search, skip_down}, Bef, Aft, Rs) ->
    Offset= cp_len(Aft),
    NAft = "': ",
    {{[$\^S|Bef],NAft}, % we insert ^S as a flag to whoever called us
     [{insert_chars, unicode, NAft}, {delete_chars,-Offset}|Rs],
     search};
do_op({search, search_found}, _Bef, Aft, Rs) ->
    "': "++NAft = Aft,
    {{[],NAft},
     [{put_chars, unicode, "\n"}, {move_rel,-cp_len(Aft)} | Rs],
     search_found};
do_op({search, search_quit}, _Bef, Aft, Rs) ->
    "': "++NAft = Aft,
    {{[],NAft},
     [{put_chars, unicode, "\n"}, {move_rel,-cp_len(Aft)} | Rs],
     search_quit};
%% do blink after $$
do_op({blink,C,M}, Bef=[$$,$$|_], Aft, Rs) ->
    N = over_paren(Bef, C, M),
    {blink,N+1,{[C|Bef],Aft},[{move_rel,-(N+1)},{insert_chars, unicode,[C]}|Rs]};
%% don't blink after a $
do_op({blink,C,_}, Bef=[$$|_], Aft, Rs) ->
    do_op({insert,C}, Bef, Aft, Rs);
%do_op({blink,C,M}, Bef, [], Rs) ->
%    N = over_paren(Bef, C, M),
%    {blink,N+1,{[C|Bef],[]},[{move_rel,-(N+1)},{put_chars,[C]}|Rs]};
do_op({blink,C,M}, Bef, Aft, Rs) ->
    case over_paren(Bef, C, M) of
	beep ->
	    {{[C|Bef], Aft}, [beep,{insert_chars, unicode, [C]}|Rs]};
	N -> {blink,N+1,{[C|Bef],Aft},
	      [{move_rel,-(N+1)},{insert_chars, unicode,[C]}|Rs]}
    end;
do_op(auto_blink, Bef, Aft, Rs) ->
    case over_paren_auto(Bef) of
	{N, Paren} ->
	    {blink,N+1,
	     {[Paren|Bef], Aft},[{move_rel,-(N+1)},{insert_chars, unicode,[Paren]}|Rs]};
	% N is likely 0
	N -> {blink,N+1,{Bef,Aft},
	      [{move_rel,-(N+1)}|Rs]}
    end;
do_op(forward_delete_char, Bef, [GC|Aft], Rs) ->
    {{Bef,Aft},[{delete_chars,gc_len(GC)}|Rs]};
do_op(backward_delete_char, [GC|Bef], Aft, Rs) ->
    {{Bef,Aft},[{delete_chars,-gc_len(GC)}|Rs]};
do_op(transpose_char, [C1,C2|Bef], [], Rs) ->
    Len = gc_len(C1)+gc_len(C2),
    {{[C2,C1|Bef],[]},[{put_chars, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(transpose_char, [C2|Bef], [C1|Aft], Rs) ->
    Len = gc_len(C2),
    {{[C2,C1|Bef],Aft},[{put_chars, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(kill_word, Bef, Aft0, Rs) ->
    {Aft1,Kill0,N0} = over_non_word(Aft0, [], 0),
    {Aft,Kill,N} = over_word(Aft1, Kill0, N0),
    put(kill_buffer, reverse(Kill)),
    {{Bef,Aft},[{delete_chars,N}|Rs]};
do_op(backward_kill_word, Bef0, Aft, Rs) ->
    {Bef1,Kill0,N0} = over_non_word(Bef0, [], 0),
    {Bef,Kill,N} = over_word(Bef1, Kill0, N0),
    put(kill_buffer, Kill),
    {{Bef,Aft},[{delete_chars,-N}|Rs]};
do_op(kill_line, Bef, Aft, Rs) ->
    put(kill_buffer, Aft),
    {{Bef,[]},[{delete_chars,cp_len(Aft)}|Rs]};
do_op(yank, Bef, [], Rs) ->
    Kill = get(kill_buffer),
    {{reverse(Kill, Bef),[]},[{put_chars, unicode,Kill}|Rs]};
do_op(yank, Bef, Aft, Rs) ->
    Kill = get(kill_buffer),
    {{reverse(Kill, Bef),Aft},[{insert_chars, unicode,Kill}|Rs]};
do_op(forward_char, Bef, [C|Aft], Rs) ->
    {{[C|Bef],Aft},[{move_rel,gc_len(C)}|Rs]};
do_op(backward_char, [C|Bef], Aft, Rs) ->
    {{Bef,[C|Aft]},[{move_rel,-gc_len(C)}|Rs]};
do_op(forward_word, Bef0, Aft0, Rs) ->
    {Aft1,Bef1,N0} = over_non_word(Aft0, Bef0, 0),
    {Aft,Bef,N} = over_word(Aft1, Bef1, N0),
    {{Bef,Aft},[{move_rel,N}|Rs]};
do_op(backward_word, Bef0, Aft0, Rs) ->
    {Bef1,Aft1,N0} = over_non_word(Bef0, Aft0, 0),
    {Bef,Aft,N} = over_word(Bef1, Aft1, N0),
    {{Bef,Aft},[{move_rel,-N}|Rs]};
do_op(beginning_of_line, [_|_]=Bef, Aft, Rs) ->
    {{[],reverse(Bef, Aft)},[{move_rel,-(cp_len(Bef))}|Rs]};
do_op(beginning_of_line, [], Aft, Rs) ->
    {{[],Aft},Rs};
do_op(end_of_line, Bef, [_|_]=Aft, Rs) ->
    {{reverse(Aft, Bef),[]},[{move_rel,cp_len(Aft)}|Rs]};
do_op(end_of_line, Bef, [], Rs) ->
    {{Bef,[]},Rs};
do_op(ctlu, Bef, Aft, Rs) ->
    put(kill_buffer, reverse(Bef)),
    {{[], Aft}, [{delete_chars, -cp_len(Bef)} | Rs]};
do_op(beep, Bef, Aft, Rs) ->
    {{Bef,Aft},[beep|Rs]};
do_op(_, Bef, Aft, Rs) ->
    {{Bef,Aft},[beep|Rs]}.

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
    over_paren(Chars, Paren, Match, 1, 1, []).


over_paren([C,$$,$$|Cs], Paren, Match, D, N, L)  ->
    over_paren([C|Cs], Paren, Match, D, N+2, L);
over_paren([GC,$$|Cs], Paren, Match, D, N, L)  ->
    over_paren(Cs, Paren, Match, D, N+1+gc_len(GC), L);
over_paren([Match|_], _Paren, Match, 1, N, _) ->
    N;
over_paren([Match|Cs], Paren, Match, D, N, [Match|L]) ->
    over_paren(Cs, Paren, Match, D-1, N+1, L);
over_paren([Paren|Cs], Paren, Match, D, N, L) ->
    over_paren(Cs, Paren, Match, D+1, N+1, [Match|L]);

over_paren([$)|Cs], Paren, Match, D, N, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, [$(|L]);
over_paren([$]|Cs], Paren, Match, D, N, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, [$[|L]);
over_paren([$}|Cs], Paren, Match, D, N, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, [${|L]);

over_paren([$(|Cs], Paren, Match, D, N, [$(|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, L);
over_paren([$[|Cs], Paren, Match, D, N, [$[|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, L);
over_paren([${|Cs], Paren, Match, D, N, [${|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, L);

over_paren([$(|_], _, _, _, _, _)  ->
    beep;
over_paren([$[|_], _, _, _, _, _)  ->
    beep;
over_paren([${|_], _, _, _, _, _)  ->
    beep;

over_paren([GC|Cs], Paren, Match, D, N, L)  ->
    over_paren(Cs, Paren, Match, D, N+gc_len(GC), L);
over_paren([], _, _, _, _, _) ->
    0.

over_paren_auto(Chars) ->
    over_paren_auto(Chars, 1, 1, []).


over_paren_auto([C,$$,$$|Cs], D, N, L)  ->
    over_paren_auto([C|Cs], D, N+2, L);
over_paren_auto([GC,$$|Cs], D, N, L)  ->
    over_paren_auto(Cs, D, N+1+gc_len(GC), L);

over_paren_auto([$(|_], _, N, [])  ->
    {N, $)};
over_paren_auto([$[|_], _, N, [])  ->
    {N, $]};
over_paren_auto([${|_], _, N, [])  ->
    {N, $}};

over_paren_auto([$)|Cs], D, N, L)  ->
    over_paren_auto(Cs, D, N+1, [$(|L]);
over_paren_auto([$]|Cs], D, N, L)  ->
    over_paren_auto(Cs, D, N+1, [$[|L]);
over_paren_auto([$}|Cs], D, N, L)  ->
    over_paren_auto(Cs, D, N+1, [${|L]);

over_paren_auto([$(|Cs], D, N, [$(|L])  ->
    over_paren_auto(Cs, D, N+1, L);
over_paren_auto([$[|Cs], D, N, [$[|L])  ->
    over_paren_auto(Cs, D, N+1, L);
over_paren_auto([${|Cs], D, N, [${|L])  ->
    over_paren_auto(Cs, D, N+1, L);

over_paren_auto([GC|Cs], D, N, L)  ->
    over_paren_auto(Cs, D, N+gc_len(GC), L);
over_paren_auto([], _, _, _) ->
    0.

%% erase_line(Line)
%% erase_inp(Line)
%% redraw_line(Line)
%% length_before(Line)
%% length_after(Line)
%% prompt(Line)
%% current_line(Line)
%%  Various functions for accessing bits of a line.

erase_line({line,Pbs,{Bef,Aft},_}) ->
    reverse(erase(Pbs, Bef, Aft, [])).

erase_inp({line,_,{Bef,Aft},_}) ->
    reverse(erase([], Bef, Aft, [])).

erase(Pbs, Bef, Aft, Rs) ->
    [{delete_chars,-cp_len(Pbs)-cp_len(Bef)},{delete_chars,cp_len(Aft)}|Rs].

redraw_line({line,Pbs,{Bef,Aft},_}) ->
    reverse(redraw(Pbs, Bef, Aft, [])).

redraw(Pbs, Bef, Aft, Rs) ->
    [{move_rel,-cp_len(Aft)},{put_chars, unicode,reverse(Bef, Aft)},{put_chars, unicode,Pbs}|Rs].

length_before({line,Pbs,{Bef,_Aft},_}) ->
    cp_len(Pbs) + cp_len(Bef).

length_after({line,_,{_Bef,Aft},_}) ->
    cp_len(Aft).

prompt({line,Pbs,_,_}) ->
    Pbs.

current_line({line,_,{Bef, Aft},_}) ->
    get_line(Bef, Aft ++ "\n").

current_chars({line,_,{Bef,Aft},_}) ->
    get_line(Bef, Aft).

get_line(Bef, Aft) ->
    unicode:characters_to_list(reverse(Bef, Aft)).

%% Grapheme length in codepoints
gc_len(CP) when is_integer(CP) -> 1;
gc_len(CPs) when is_list(CPs) -> length(CPs).

%% String length in codepoints
cp_len(Str) ->
    cp_len(Str, 0).

cp_len([GC|R], Len) ->
    cp_len(R, Len + gc_len(GC));
cp_len([], Len) -> Len.

%% %% expand(CurrentBefore) ->
%% %%	{yes,Expansion} | no
%% %%  Try to expand the word before as either a module name or a function
%% %%  name. We can handle white space around the seperating ':' but the
%% %%  function name must be on the same line. CurrentBefore is reversed
%% %%  and over_word/3 reverses the characters it finds. In certain cases
%% %%  possible expansions are printed.

%% expand(Bef0) ->
%%     {Bef1,Word,_} = over_word(Bef0, [], 0),
%%     case over_white(Bef1, [], 0) of
%% 	{[$:|Bef2],_White,_Nwh} ->
%% 	    {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
%% 	    {_,Mod,_Nm} = over_word(Bef3, [], 0),
%% 	    expand_function_name(Mod, Word);
%% 	{_,_,_} ->
%% 	    expand_module_name(Word)
%%     end.

%% expand_module_name(Prefix) ->
%%     match(Prefix, code:all_loaded(), ":").

%% expand_function_name(ModStr, FuncPrefix) ->
%%     Mod = list_to_atom(ModStr),
%%     case erlang:module_loaded(Mod) of
%% 	true ->
%% 	    L = apply(Mod, module_info, []),
%% 	    case lists:keyfind(exports, 1, L) of
%% 		{_, Exports} ->
%% 		    match(FuncPrefix, Exports, "(");
%% 		_ ->
%% 		    no
%% 	    end;
%% 	false ->
%% 	    no
%%     end.

%% match(Prefix, Alts, Extra) ->
%%     Matches = match1(Prefix, Alts),
%%     case longest_common_head([N || {N,_} <- Matches]) of
%% 	{partial, []} ->
%% 	    print_matches(Matches),
%% 	    no;
%% 	{partial, Str} ->
%% 	    case lists:nthtail(length(Prefix), Str) of
%% 		[] ->
%% 		    print_matches(Matches),
%% 		    {yes, []};
%% 		Remain ->
%% 		    {yes, Remain}
%% 	    end;
%% 	{complete, Str} ->
%% 	    {yes, lists:nthtail(length(Prefix), Str) ++ Extra};
%% 	no ->
%% 	    no
%%     end.

%% %% Print the list of names L in multiple columns.
%% print_matches(L) ->
%%     io:nl(),
%%     col_print(lists:sort(L)),
%%     ok.

%% col_print([]) -> ok;
%% col_print(L)  -> col_print(L, field_width(L), 0).

%% col_print(X, Width, Len) when Width + Len > 79 ->
%%     io:nl(),
%%     col_print(X, Width, 0);
%% col_print([{H0,A}|T], Width, Len) ->
%%     H = if
%% 	    %% If the second element is an integer, we assume it's an
%% 	    %% arity, and meant to be printed.
%% 	    integer(A) ->
%% 		H0 ++ "/" ++ integer_to_list(A);
%% 	    true ->
%% 		H0
%% 	end,
%%     io:format("~-*s",[Width,H]),
%%     col_print(T, Width, Len+Width);
%% col_print([], _, _) ->
%%     io:nl().

%% field_width([{H,_}|T]) -> field_width(T, length(H)).

%% field_width([{H,_}|T], W) ->
%%     case length(H) of
%% 	L when L > W -> field_width(T, L);
%% 	_ -> field_width(T, W)
%%     end;
%% field_width([], W) when W < 40 ->
%%     W + 4;
%% field_width([], _) ->
%%     40.

%% match1(Prefix, Alts) ->
%%     match1(Prefix, Alts, []).

%% match1(Prefix, [{H,A}|T], L) ->
%%     case prefix(Prefix, Str = atom_to_list(H)) of
%% 	true ->
%% 	    match1(Prefix, T, [{Str,A}|L]);
%% 	false ->
%% 	    match1(Prefix, T, L)
%%     end;
%% match1(_, [], L) ->
%%     L.

%% longest_common_head([]) ->
%%     no;
%% longest_common_head(LL) ->
%%     longest_common_head(LL, []).

%% longest_common_head([[]|_], L) ->
%%     {partial, reverse(L)};
%% longest_common_head(LL, L) ->
%%     case same_head(LL) of
%% 	true ->
%% 	    [[H|_]|_] = LL,
%% 	    LL1 = all_tails(LL),
%% 	    case all_nil(LL1) of
%% 		false ->
%% 		    longest_common_head(LL1, [H|L]);
%% 		true ->
%% 		    {complete, reverse([H|L])}
%% 	    end;
%% 	false ->
%% 	    {partial, reverse(L)}
%%     end.

%% same_head([[H|_]|T1]) -> same_head(H, T1).

%% same_head(H, [[H|_]|T]) -> same_head(H, T);
%% same_head(_, [])        -> true;
%% same_head(_, _)         -> false.

%% all_tails(LL) -> all_tails(LL, []).

%% all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
%% all_tails([], L)         -> L.

%% all_nil([]) -> true;
%% all_nil([[] | Rest]) -> all_nil(Rest);
%% all_nil(_) -> false.
