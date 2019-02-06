%%%----------------------------------------------------------------------
%%% File    : eed.erl
%%% Author  : Bjorn Gustavsson <bjorn@strider>
%%% Purpose : Unix `ed' look-alike.
%%% Created : 24 Aug 1997 by Bjorn Gustavsson <bjorn@strider>
%%%----------------------------------------------------------------------

-module(eed).
-author('bjorn@strider').

-export([edit/0, edit/1, file/1, cmd_line/1]).

-compile({no_auto_import,[error/1]}).

-record(state, {dot = 0,			% Line number of dot.
		upto_dot = [],			% Lines up to dot (reversed).
		after_dot = [],			% Lines after dot.
		lines = 0,			% Total number of lines.
		print=false,			% Print after command.
		filename=[],			% Current file.
		pattern,			% Current pattern.
		in_global=false,		% True if executing global command.
		input=[],			% Global input stream.
		undo,				% Last undo state.
		marks=[],			% List of marks.
		modified=false,			% Buffer is modified.
		opts=[{prompt, ''}],		% Options.
		last_error,			% The last error encountered.
		input_fd			% Input file descriptor.
	       }).

-record(line, {contents,			% Contents of line.
	       mark=false			% Marked (for global prefix).
	      }).

cmd_line([Script]) ->
    file(Script),
    halt().

file(Script) ->
    case file:open(Script, [read]) of
	{ok,Fd} ->
	    loop(#state{input_fd=Fd}),
	    ok;
	{error,E} ->
	    {error,E}
    end.

edit() ->
    loop(#state{input_fd=group_leader()}).

edit(Name) ->
    loop(command([$e|Name], #state{input_fd=group_leader()})).

loop(St0) ->
    {ok, St1, Cmd} = get_line(St0),
    case catch command(nonl(Cmd), St1) of
	{'EXIT', Reason} ->
	    %% XXX Should clear outstanding global command here.
	    loop(print_error({'EXIT', Reason}, St1));
	quit ->
	    ok;
	{error, Reason} ->
	    loop(print_error(Reason, St1));
	St2 when is_record(St2, state) ->
	    loop(St2)
    end.

nonl([$\n]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].

command(Cmd, St) ->
    case parse_command(Cmd, St) of
	quit ->
	    quit;
	St1 when is_function(St1#state.print) ->
	    if
		St1#state.dot /= 0 ->
		    print_current(St1);
		true ->
		    ok
	    end,
	    St1#state{print=false};
	St1 when is_record(St1, state) ->
	    St1
    end.

get_line(St) ->
    Opts = St#state.opts,
    {value, {prompt, Prompt}} = lists:keysearch(prompt, 1, Opts),
    get_line(Prompt, St).

get_line(Prompt, St) when St#state.input == [] ->
    Line = get_line1(St#state.input_fd, Prompt, []),
    {ok, St, Line};
get_line(_, St) ->
    get_input(St#state.input, St, []).

get_input([eof], St, []) ->
    {ok, St, eof};
get_input([eof], St, Result) ->
    {ok, St#state{input=[eof]}, lists:reverse(Result)};
get_input([$\n|Rest], St, Result) ->
    {ok, St#state{input=Rest}, lists:reverse(Result)};
get_input([C|Rest], St, Result) ->
    get_input(Rest, St, [C|Result]).

get_line1(Io, Prompt, Result) ->
    get_line2(Io, io:get_line(Io, Prompt), Result).

get_line2(_Io, eof, []) ->
    eof;
get_line2(_Io, eof, Result) ->
    lists:reverse(Result);
get_line2(Io, [$\\, $\n], Result) ->
    get_line1(Io, '', [$\n|Result]);
get_line2(_Io, [$\n], Result) ->
    lists:reverse(Result, [$\n]);
get_line2(Io, [C|Rest], Result) ->
    get_line2(Io, Rest, [C|Result]).

print_error(Reason, St0) ->
    St1 = St0#state{last_error=Reason},
    io:put_chars("?\n"),
    case lists:member(help_always, St1#state.opts) of
	true ->
	    help_command([], [], St1),
	    St1;
	false ->
	    St1
    end.

format_error(bad_command) -> "unknown command";
format_error(bad_filename) -> "illegal or missing filename";
format_error(bad_file) -> "cannot open input file";
format_error(bad_linenum) -> "line out of range";
format_error(bad_delimiter) -> "illegal or missing delimiter";
format_error(bad_undo) -> "nothing to undo";
format_error(bad_mark) -> "mark not lower case ascii";
format_error(bad_pattern) -> "invalid regular expression";
format_error(buffer_modified) -> "warning: expecting `w'";
format_error(nested_globals) -> "multiple globals not allowed";
format_error(nomatch) -> "search string not found";
format_error(missing_space) -> "no space after command";
format_error(garbage_after_command) -> "illegal suffix";
format_error(not_implemented) -> "not implemented yet";
format_error({'EXIT', {Code, {Mod, Func, Args}}}) ->
    lists:flatten(io_lib:format("aborted due to bug (~p)",
				[{Code, {Mod, Func, length(Args)}}]));
format_error(A) -> atom_to_list(A).



%%% Parsing commands.

parse_command(Cmd, St) ->
    parse_command(Cmd, St, []).

parse_command(Cmd, State, Nums) ->
    case get_one(Cmd, State) of
	{ok, Num, Rest, NewState} ->
	    parse_next_address(Rest, NewState, [Num|Nums]);
	false ->
	    parse_command1(Cmd, State, Nums)
    end.

parse_next_address([$,|Rest], State, Nums) ->
    parse_command(Rest, State, Nums);
parse_next_address([$;|Rest], State, [Num|Nums]) ->
    parse_command(Rest, move_to(Num, State), [Num|Nums]);
parse_next_address(Rest, State, Nums) ->
    parse_command1(Rest, State, Nums).

parse_command1([Letter|Rest], State, Nums) ->
    Cont = fun(Fun, NumLines, Def) ->
		   execute_command(Fun, NumLines, Def, State, Nums, Rest) end,
    parse_cmd_char(Letter, Cont);
parse_command1([], State, Nums) ->
    execute_command(fun print_command/3, 1, next, State, Nums, []).

get_one(Cmd, St) ->
    case get_address(Cmd, St) of
	{ok, Addr, Cmd1, St1} ->
	    get_one1(Cmd1, Addr, St1);
	false ->
	    get_one1(Cmd, false, St)
    end.

get_one1([D|Rest], false, St) when $0 =< D, D =< $9 ->
    get_one2(get_number([D|Rest]), 1, 0, St);
get_one1([D|Rest], Sum, St) when $0 =< D, D =< $9 ->
    get_one2(get_number([D|Rest]), 1, Sum, St);
get_one1([$+, D|Rest], Sum, St) when $0 =< D, D =< $9 ->
    get_one2(get_number([D|Rest]), 1, Sum, St);
get_one1([$-, D|Rest], Sum, St) when $0 =< D, D =< $9 ->
    get_one2(get_number([D|Rest]), -1, Sum, St);
get_one1([$+|Rest], Sum, St) ->
    get_one2({ok, 1, Rest}, 1, Sum, St);
get_one1([$-|Rest], Sum, St) ->
    get_one2({ok, 1, Rest}, -1, Sum, St);
get_one1(_Cmd, false, _St) ->
    false;
get_one1(Cmd, Sum, St) ->
    {ok, Sum, Cmd, St}.

get_one2({ok, Number, Rest}, Mul, false, St) ->
    get_one1(Rest, St#state.dot+Mul*Number, St);
get_one2({ok, Number, Rest}, Mul, Sum, St) ->
    get_one1(Rest, Sum+Mul*Number, St).

get_number(Cmd) ->
    get_number(Cmd, 0).

get_number([D|Rest], Result) when $0 =< D, D =< $9 ->
    get_number(Rest, Result*10+D-$0);
get_number(Rest, Result) ->
    {ok, Result, Rest}.

get_address([$.|Rest], State) ->
    {ok, State#state.dot, Rest, State};
get_address([$$|Rest], State) ->
    {ok, State#state.lines, Rest, State};
get_address([$', Mark|Rest], St) when $a =< Mark, Mark =< $z ->
    case lists:keysearch(Mark, 2, St#state.marks) of
	{value, {Line, Mark}} ->
	    {ok, Line, Rest, St};
	false ->
	    {ok, 0, Rest, St}
    end;
get_address([$'|_Rest], _State) ->
    error(bad_mark);
get_address([$/|Rest], State) ->
    scan_forward($/, Rest, State);
get_address([$?|_Rest], _State) ->
    error(not_implemented);
get_address(_Cmd, _St) ->
    false.

scan_forward(End, Patt0, State) ->
    {ok, Rest, NewState} = get_pattern(End, Patt0, State),
    Dot = NewState#state.dot,
    After = NewState#state.after_dot,
    scan_forward1(Dot+1, After, NewState, Rest).

scan_forward1(Linenum, [Line|Rest], State, RestCmd) ->
    case re:run(Line#line.contents, State#state.pattern, [{capture, none}]) of
	match ->
	    {ok, Linenum, RestCmd, State};
	nomatch ->
	    scan_forward1(Linenum+1, Rest, State, RestCmd)
    end;
scan_forward1(_, [], State, RestCmd) ->
    Dot = State#state.dot,
    Upto = State#state.upto_dot,
    case scan_forward2(Dot, Upto, State, RestCmd) of
	false ->
	    error(bad_linenum);
	Other ->
	    Other
    end.

scan_forward2(0, [], _State, _RestCmd) ->
    false;
scan_forward2(Linenum, [Line|Rest], State, RestCmd) ->
    case scan_forward2(Linenum-1, Rest, State, RestCmd) of
	false ->
	    case re:run(Line#line.contents, State#state.pattern,
			[{capture, none}]) of
		match ->
		    {ok, Linenum, RestCmd, State};
		nomatch ->
		    false
	    end;
	Other ->
	    Other
    end.

parse_cmd_char($S, Cont) -> Cont(fun quest_command/3, 0, none);
parse_cmd_char($T, Cont) -> Cont(fun time_command/3, 0, none);
parse_cmd_char($=, Cont) -> Cont(fun print_linenum/3, 1, last);
parse_cmd_char($a, Cont) -> Cont(fun append_command/3, 1, dot);
parse_cmd_char($c, Cont) -> Cont(fun change_command/3, 2, dot);
parse_cmd_char($d, Cont) -> Cont(fun delete_command/3, 2, dot);
parse_cmd_char($e, Cont) -> Cont(fun enter_command/3, 0, none);
parse_cmd_char($E, Cont) -> Cont(fun enter_always_command/3, 0, none);
parse_cmd_char($f, Cont) -> Cont(fun file_command/3, 0, none);
parse_cmd_char($g, Cont) -> Cont(fun global_command/3, 2, all);
parse_cmd_char($h, Cont) -> Cont(fun help_command/3, 0, none);
parse_cmd_char($H, Cont) -> Cont(fun help_always_command/3, 0, none);
parse_cmd_char($i, Cont) -> Cont(fun insert_command/3, 1, dot);
parse_cmd_char($k, Cont) -> Cont(fun mark_command/3, 1, dot);
parse_cmd_char($l, Cont) -> Cont(fun list_command/3, 2, dot);
parse_cmd_char($m, Cont) -> Cont(fun move_command/3, 2, dot);
parse_cmd_char($n, Cont) -> Cont(fun number_command/3, 2, dot);
parse_cmd_char($p, Cont) -> Cont(fun print_command/3, 2, dot);
parse_cmd_char($P, Cont) -> Cont(fun prompt_command/3, 0, none);
parse_cmd_char($q, Cont) -> Cont(fun quit_command/3, 0, none);
parse_cmd_char($Q, Cont) -> Cont(fun quit_always_command/3, 0, none);
parse_cmd_char($r, Cont) -> Cont(fun read_command/3, 1, last);
parse_cmd_char($s, Cont) -> Cont(fun subst_command/3, 2, dot);
parse_cmd_char($t, Cont) -> Cont(fun transpose_command/3, 2, dot);
parse_cmd_char($u, Cont) -> Cont(fun undo_command/3, 0, none);
parse_cmd_char($v, Cont) -> Cont(fun vglobal_command/3, 2, all);
parse_cmd_char($w, Cont) -> Cont(fun write_command/3, 2, all);
parse_cmd_char(_, _Cont)  -> error(bad_command).

execute_command(Fun, NumLines, Def, State, Nums, Rest) ->
    Lines = check_lines(NumLines, Def, Nums, State),
    Fun(Rest, Lines, State).

check_lines(0, _, [], _State) ->
    [];
check_lines(1, dot, [], #state{dot=Dot}) ->
    [Dot];
check_lines(1, next, [], State) when State#state.dot < State#state.lines ->
    [State#state.dot+1];
check_lines(1, last, [], State) ->
    [State#state.lines];
check_lines(1, _, [Num|_], State) when 0 =< Num, Num =< State#state.lines ->
    [Num];
check_lines(2, dot, [], #state{dot=Dot}) ->
    [Dot, Dot];
check_lines(2, all, [], #state{lines=Lines}) ->
    [1, Lines];
check_lines(2, _, [Num], State) when 0 =< Num, Num =< State#state.lines ->
    [Num, Num];
check_lines(2, _, [Num2, Num1|_], State)
when 0 =< Num1, Num1 =< Num2, Num2 =< State#state.lines ->
    [Num1, Num2];
check_lines(_, _, _, _) ->
    error(bad_linenum).


%%% Executing commands.

%% ($)= - print line number

print_linenum(Rest, [Line], State) ->
    NewState = check_trailing_p(Rest, State),
    io:format("~w\n", [Line]),
    NewState.

%% ? - print state (for debugging)

quest_command([], [], State) ->
    io:format("~p\n", [State]),
    State.

%% Tcmd - time command

time_command(Cmd, [], St) ->
    Fun = fun parse_command/2,
    erlang:garbage_collect(),
    {Elapsed, Val} = timer:tc(erlang, apply, [Fun, [Cmd, St]]),
    io:format("Time used: ~p s~n", [Elapsed/1000000.0]),
    case Val of
	{error, Reason} ->
	    throw({error, Reason});
	Other ->
	    Other
    end.

%% (.)a - append text

append_command(Rest, [Line], St0) ->
    St1 = save_for_undo(St0),
    append(move_to(Line, check_trailing_p(Rest, St1))).

append(St0) ->
    {ok, St1, Line0} = get_line('', St0),
    case Line0 of
	eof ->
	    St1;
	".\n" ->
	    St1;
	Line ->
	    append(insert_line(Line, St1))
    end.

%% (.,.)c

change_command(Rest, Lines, St0) ->
    St1 = delete_command(Rest, Lines, St0),
    St2 = append_command([], [St1#state.dot-1], St1),
    save_for_undo(St2, St0).

%% (.,.)d - delete lines

delete_command(_Rest, [0, _Last], _St) ->
    error(bad_linenum);
delete_command(Rest, [First, Last], St0) ->
    St1 = check_trailing_p(Rest, save_for_undo(St0)),
    delete(Last-First+1, move_to(Last, St1)).

delete(0, St) when St#state.dot == St#state.lines ->
    St;
delete(0, St) ->
    next_line(St);
delete(Left, St0) ->
    St1 = delete_current_line(St0),
    delete(Left-1, St1).

%% e file - replace buffer with new file

enter_command(_Name, [], St) when St#state.modified == true ->
    error(buffer_modified);
enter_command(Name, [], St0) ->
    enter_always_command(Name, [], St0).

%% E file - replace buffer with new file

enter_always_command(Name, [], St0) ->
    St1 = read_command(Name, [0], #state{filename=St0#state.filename,
					 opts=St0#state.opts}),
    St1#state{modified=false}.

%% f file - print filename; set filename

file_command([], [], St) ->
    io:format("~s~n", [St#state.filename]),
    St;
file_command([$_|Name0], [], St) ->
    Name = skip_blanks(Name0),
    file_command([], [], St#state{filename=Name});
file_command(_, _, _) ->
    error(missing_space).

%% (1,$)g/RE/commands - execute commands on all matching lines.
%% (1,$)v/RE/commands - execute commands on all non-matching lines.

global_command(Cmd, Lines, St) ->
    check_global0(true, Cmd, Lines, St).

vglobal_command(Cmd, Lines, St) ->
    check_global0(false, Cmd, Lines, St).

check_global0(_, _, _, St) when St#state.in_global == true ->
    error(nested_globals);
check_global0(Sense, [Sep|Pattern], Lines, St0) ->
    {ok, Cmd, St1} = get_pattern(Sep, Pattern, St0),
    St2 = mark(Sense, Lines, St1),
    do_global_command(Cmd, St2#state{in_global=true}, 0).
    
mark(Sense, [First, Last], St0) ->
    St1 = move_to(Last, St0),
    mark1(Sense, First-1, St1).

mark1(_Sense, First, St) when St#state.dot == First ->
    St;
mark1(Sense, First, St) ->
    [Line|Prev] = St#state.upto_dot,
    NewLine = case match(St) of
		  true  -> Line#line{mark=Sense};
		  false -> Line#line{mark=not(Sense)}
	      end,
    mark1(Sense, First, prev_line(St#state{upto_dot=[NewLine|Prev]})).

do_global_command(Cmd, St0, Matches) ->
    case find_mark(St0) of
	{ok, St1} ->
	    St2 = St1#state{input=Cmd++[eof]},
	    {ok, St3, Cmd1} = get_line(St2),
	    St4 = command(Cmd1, St3),
	    %% XXX There might be several commands.
	    do_global_command(Cmd, St4, Matches+1);
	false when Matches == 0 ->
	    error(nomatch);
	false ->
	    St0#state{in_global=false, input=[]}
    end.

find_mark(State) ->
    find_mark(State#state.lines, State).

find_mark(0, _State) ->
    false;
find_mark(Limit, State) when State#state.dot == 0 ->
    find_mark(Limit, next_line(State));
find_mark(Limit, State) ->
    case State#state.upto_dot of
	[Line|Prev] when Line#line.mark == true ->
	    NewLine = Line#line{mark=false},
	    {ok, State#state{upto_dot=[NewLine|Prev]}};
	_Other ->
	    find_mark(Limit-1, wrap_next_line(State))
    end.

%% h - print info about last error

help_command([], [], St) ->
    case St#state.last_error of
	undefined ->
	    St;
	Reason ->
	    io:put_chars(format_error(Reason)),
	    io:nl(),
	    St
    end;
help_command(_, _, _) ->
    error(garbage_after_command).

%% H - toggle automatic help mode on/off

help_always_command([], [], St) ->
    Opts = St#state.opts,
    case lists:member(help_always, Opts) of
	true ->
	    St#state{opts=Opts--[help_always]};
	false ->
	    help_command([], [], St),
	    St#state{opts=[help_always|Opts]}
    end.

%% (.)i - insert text

insert_command(_Rest, [0], _State) ->
    error(bad_linenum);
insert_command(Rest, [Line], State) ->
    append_command(Rest, [Line-1], State).

%% (.)kx - mark line

mark_command(_, [0], _St) ->
    error(bad_linenum);
mark_command([Mark|_Rest], [_Line], _St) when $a =< Mark, Mark =< $z ->
    error(not_implemented);
mark_command(_, _, _) ->
    error(bad_mark).
    
%% (.,.)l - list lines

list_command(Rest, Lines, St) ->
    print([$l|Rest], Lines, St).

%% (.,.)m - move lines

move_command(_Cmd, [_First, _Last], _St) ->
    error(not_implemented).

%% (.,.)t - copy lines

transpose_command(_Cmd, [_First, _Last], _St) ->
    error(not_implemented).

%% (.,.)n - print lines with line numbers

number_command(Rest, Lines, St) ->
    print([$n|Rest], Lines, St).

%% (.,.)p - print lines

print_command(Rest, Lines, St) ->
    print([$p|Rest], Lines, St).

%% P - toggle prompt

prompt_command([], [], St) ->
    Opts = St#state.opts,
    case lists:keysearch(prompt, 1, Opts) of
	{value, {prompt, ''}} ->
	    St#state{opts=[{prompt, '*'}|Opts]};
	{value, Value} ->
	    St#state{opts=[{prompt, ''} | Opts--[Value]]}
    end;
prompt_command(_, _, _) ->
    error(garbage_after_command).

%% q - quit editor

quit_command([], [], _) ->
    quit;
quit_command(_, _, _) ->
    error(garbage_after_command).

%% Q - quit editor

quit_always_command([], [], _) ->
    quit;
quit_always_command(_, _, _) ->
    error(garbage_after_command).

%% ($)r file - read file

read_command([], _, St) when St#state.filename == [] ->
    error(bad_filename);
read_command([], [After], St) ->
    read(After, St#state.filename, St);
read_command([$ |Name0], [After], St) when St#state.filename == [] ->
    Name = skip_blanks(Name0),
    read(After, Name, St#state{filename=Name});
read_command([$ |Name0], [After], St) ->
    Name = skip_blanks(Name0),
    read(After, Name, St);
read_command(_, _, _) ->
    error(missing_space).

read(After, Name, St0) ->
    case file:read_file(Name) of
	{ok, Bin} ->
	    Chars = size(Bin),
	    St1 = move_to(After, St0),
	    St2 = insert_line(binary_to_list(Bin), St1),
	    io:format("~w~n", [Chars]),
	    St2;
	{error, _} ->
	    error(bad_file)
    end.

%% s/pattern/replacement/gp

subst_command(_, [0, _], _) ->
    error(bad_linenum);
subst_command([$ |_Cmd0], [_First, _Last], _St0) ->
    error(bad_delimiter);
subst_command([$\n|_Cmd0], [_First, _Last], _St0) ->
    error(bad_delimiter);
subst_command([Sep|Cmd0], [First, Last], St0) ->
    St1 = save_for_undo(St0),
    {ok, Cmd1, St2} = get_pattern(Sep, Cmd0, St1),
    {ok, Replacement, Cmd2} = get_replacement(Sep, Cmd1),
    {ok, Opts, Cmd3} = subst_check_gflag(Cmd2),
    St3 = check_trailing_p(Cmd3, St2),
    subst_command(Last-First+1, Opts, Replacement,
		  move_to(First-1, St3), nomatch);
subst_command([], _, _) ->
    error(bad_delimiter).
    
subst_command(0, _, _, _, nomatch) ->
    error(nomatch);
subst_command(0, _, _, _, StLast) when is_record(StLast, state) ->
    StLast;
subst_command(Left, Opts, Repl, St0, LastMatch) ->
    St1 = next_line(St0),
    [Line|_] = St1#state.upto_dot,
    Contents = Line#line.contents,
    case re:replace(Contents, St1#state.pattern, Repl, Opts) of
	Contents ->
	    subst_command(Left-1, Opts, Repl, St1, LastMatch);
	NewContents ->
	    %% XXX This doesn't work with marks.
	    St2 = delete_current_line(St1),
	    St3 = insert_line(NewContents, St2),
	    subst_command(Left-1, Opts, Repl, St3, St3)
    end.

subst_check_gflag([$g|Cmd]) -> {ok, [global,{return,list}], Cmd};
subst_check_gflag(Cmd)      -> {ok, [{return,list}], Cmd}.

%% u - undo

undo_command([], [], St) when St#state.undo == undefined ->
    error(bad_undo);
undo_command([], [], #state{undo=Undo}) ->
    Undo;
undo_command(_, _, _) ->
    error(garbage_after_command).

%% (1,$)w - write buffer to file

write_command(_Cmd, [_First, _Last], _St) ->
    error(not_implemented).
    

%%% Primitive buffer operations.

print_current(St) ->
    [Line|_] = St#state.upto_dot,
    Printer = St#state.print,
    Printer(Line#line.contents, St).

delete_current_line(St) when St#state.dot == 0 ->
    error(bad_linenum);
delete_current_line(St) ->
    Lines = St#state.lines,
    [_|Prev] = St#state.upto_dot,
    St#state{dot=St#state.dot-1, upto_dot=Prev, lines=Lines-1, modified=true}.
    
insert_line(Line, State) ->
    insert_line1(Line, State, []).

insert_line1([$\n|Rest], State, Result) ->
    NewState = insert_single_line(lists:reverse(Result, [$\n]), State),
    insert_line1(Rest, NewState, []);
insert_line1([C|Rest], State, Result) ->
    insert_line1(Rest, State, [C|Result]);
insert_line1([], State, []) ->
    State;
insert_line1([], State, Result) ->
    insert_single_line(lists:reverse(Result, [$\n]), State).

insert_single_line(Line0, State) ->
    Line = #line{contents=Line0},
    Dot = State#state.dot,
    Before = State#state.upto_dot,
    Lines = State#state.lines,
    %% XXX Avoid updating the record every time.
    State#state{dot=Dot+1, upto_dot=[Line|Before], lines=Lines+1, modified=true}.

move_to(Line, State) when Line < State#state.dot ->
    move_to(Line, prev_line(State));
move_to(Line, State) when State#state.dot < Line ->
    move_to(Line, next_line(State));
move_to(Line, State) when Line == State#state.dot ->
    State.

prev_line(State) ->
    Dot = State#state.dot,
    Before = State#state.upto_dot,
    After = State#state.after_dot,
    State#state{dot=Dot-1, upto_dot=tl(Before), after_dot=[hd(Before)|After]}.

next_line(State) ->
    Dot = State#state.dot,
    Before = State#state.upto_dot,
    After = State#state.after_dot,
    State#state{dot=Dot+1, upto_dot=[hd(After)|Before], after_dot=tl(After)}.

wrap_next_line(State) when State#state.dot == State#state.lines ->
    move_to(1, State);
wrap_next_line(State) ->
    next_line(State).


%%% Utilities.

get_pattern(End, Cmd, State) ->
    get_pattern(End, Cmd, State, []).

get_pattern(End, [End|Rest], State, []) when State#state.pattern /= undefined ->
    {ok, Rest, State};
get_pattern(End, [End|Rest], State, Result) ->
    case re:compile(lists:reverse(Result)) of
	{error, _} ->
	    error(bad_pattern);
	{ok, Re} ->
	    {ok, Rest, State#state{pattern=Re}}
    end;
get_pattern(End, [C|Rest], State, Result) ->
    get_pattern(End, Rest, State, [C|Result]);
get_pattern(End, [], State, Result) ->
    get_pattern(End, [End], State, Result).

get_replacement(End, Cmd) ->
    get_replacement(End, Cmd, []).

get_replacement(End, [End|Rest], Result) ->
    {ok, lists:reverse(Result), Rest};
get_replacement(End, [$\\, $&|Rest], Result) ->
    get_replacement(End, Rest, [$&, $\\|Result]);
get_replacement(End, [$\\, C|Rest], Result) ->
    get_replacement(End, Rest, [C|Result]);
get_replacement(End, [C|Rest], Result) ->
    get_replacement(End, Rest, [C|Result]);
get_replacement(End, [], Result) ->
    get_replacement(End, [End], Result).

check_trailing_p([$l], St) ->
    St#state{print=fun(Line, _) -> lister(Line, 0) end};
check_trailing_p([$n], St) ->
    St#state{print=fun numberer/2};
check_trailing_p([$p], St) ->
    St#state{print=fun(Line, _) -> io:put_chars(Line) end};
check_trailing_p([], State) ->
    State;
check_trailing_p(_Other, _State) ->
    error(garbage_after_command).

error(Reason) ->
    throw({error, Reason}).

match(State) when State#state.dot == 0 ->
    false;
match(State) ->
    [Line|_] = State#state.upto_dot,
    Re = State#state.pattern,
    case re:run(Line#line.contents, Re, [{capture, none}]) of
	match   -> true;
	nomatch -> false
    end.

skip_blanks([$ |Rest]) ->
    skip_blanks(Rest);
skip_blanks(Rest) ->
    Rest.

print(Rest, [Line], St0) when Line > 0 ->
    St1 = check_trailing_p(Rest, St0),
    print(Line, move_to(Line-1, St1));
print(Rest, [First, Last], St0) when First > 0 ->
    St1 = check_trailing_p(Rest, St0),
    print(Last, move_to(First-1, St1)).

print(Last, St) when St#state.dot == Last ->
    St#state{print=false};
print(Last, St0) ->
    St1 = next_line(St0),
    print_current(St1),
    print(Last, St1).

lister(Rest, 64) ->
    io:put_chars("\\\n"),
    lister(Rest, 0);
lister([C|Rest], Num) ->
    list_char(C),
    lister(Rest, Num+1);
lister([], _) ->
    ok.

list_char($\t) ->
    io:put_chars("\\t");
list_char($\n) ->
    io:put_chars("$\n");
list_char(C) ->
    io:put_chars([C]).

numberer(Line, St) ->
    io:format("~w\t~s", [St#state.dot, Line]).

save_for_undo(St) ->
    St#state{undo=St#state{undo=undefined, print=false}}.

save_for_undo(St, OldSt) ->
    St#state{undo=OldSt#state{undo=undefined, print=false}}.
