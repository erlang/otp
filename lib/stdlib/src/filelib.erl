%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

-module(filelib).

%% File utilities.
-export([wildcard/1, wildcard/2, is_dir/1, is_file/1, is_regular/1]).
-export([fold_files/5, last_modified/1, file_size/1, ensure_dir/1]).
-export([wildcard/3, is_dir/2, is_file/2, is_regular/2]).
-export([fold_files/6, last_modified/2, file_size/2]).
-export([find_file/2, find_file/3, find_source/1, find_source/2, find_source/3]).

%% For debugging/testing.
-export([compile_wildcard/1]).

-include_lib("kernel/include/file.hrl").

-define(HANDLE_ERROR(Expr),
	try
	    Expr
	catch
	    error:{badpattern,_}=UnUsUalVaRiAbLeNaMe ->
		%% Get the stack backtrace correct.
		error(UnUsUalVaRiAbLeNaMe)
	end).

-type filename() :: file:name().
-type dirname() :: filename().

-type filename_all() :: file:name_all().
-type dirname_all() :: filename_all().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec wildcard(Wildcard) -> [file:filename()] when
      Wildcard :: filename() | dirname().
wildcard(Pattern) when is_list(Pattern) ->
    ?HANDLE_ERROR(do_wildcard(Pattern, ".", file)).

-spec wildcard(Wildcard, Cwd) -> [file:filename()] when
      Wildcard :: filename() | dirname(),
      Cwd :: dirname().
wildcard(Pattern, Cwd) when is_list(Pattern), is_list(Cwd) ->
    ?HANDLE_ERROR(do_wildcard(Pattern, Cwd, file));
wildcard(Pattern, Mod) when is_list(Pattern), is_atom(Mod) ->
    ?HANDLE_ERROR(do_wildcard(Pattern, ".", Mod)).

-spec wildcard(file:name(), file:name(), atom()) -> [file:filename()].
wildcard(Pattern, Cwd, Mod)
  when is_list(Pattern), is_list(Cwd), is_atom(Mod) ->
    ?HANDLE_ERROR(do_wildcard(Pattern, Cwd, Mod)).

-spec is_dir(Name) -> boolean() when
      Name :: filename_all() | dirname_all().
is_dir(Dir) ->
    do_is_dir(Dir, file).

-spec is_dir(file:name_all(), atom()) -> boolean().
is_dir(Dir, Mod) when is_atom(Mod) ->
    do_is_dir(Dir, Mod).

-spec is_file(Name) -> boolean() when
      Name :: filename_all() | dirname_all().
is_file(File) ->
    do_is_file(File, file).

-spec is_file(file:name_all(), atom()) -> boolean().
is_file(File, Mod) when is_atom(Mod) ->
    do_is_file(File, Mod).

-spec is_regular(Name) -> boolean() when
      Name :: filename_all().
is_regular(File) ->
    do_is_regular(File, file).
    
-spec is_regular(file:name_all(), atom()) -> boolean().
is_regular(File, Mod) when is_atom(Mod) ->
    do_is_regular(File, Mod).
    
-spec fold_files(Dir, RegExp, Recursive, Fun, AccIn) -> AccOut when
      Dir :: dirname(),
      RegExp :: string(),
      Recursive :: boolean(),
      Fun :: fun((F :: file:filename(), AccIn) -> AccOut),
      AccIn :: term(),
      AccOut :: term().
fold_files(Dir, RegExp, Recursive, Fun, Acc) ->
    do_fold_files(Dir, RegExp, Recursive, Fun, Acc, file).

-spec fold_files(file:name(), string(), boolean(), fun((_,_) -> _), _, atom()) -> _.
fold_files(Dir, RegExp, Recursive, Fun, Acc, Mod) when is_atom(Mod) ->
    do_fold_files(Dir, RegExp, Recursive, Fun, Acc, Mod).

-spec last_modified(Name) -> file:date_time() | 0 when
      Name :: filename_all() | dirname_all().
last_modified(File) ->
    do_last_modified(File, file).

-spec last_modified(file:name_all(), atom()) -> file:date_time() | 0.
last_modified(File, Mod) when is_atom(Mod) ->
    do_last_modified(File, Mod).

-spec file_size(Filename) -> non_neg_integer() when
      Filename :: filename_all().
file_size(File) ->
    do_file_size(File, file).

-spec file_size(file:name(), atom()) -> non_neg_integer().
file_size(File, Mod) when is_atom(Mod) ->
    do_file_size(File, Mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_is_dir(Dir, Mod) ->
    case eval_read_file_info(Dir, Mod) of
	{ok, #file_info{type=directory}} ->
	    true;
	_ ->
	    false
    end.

do_is_file(File, Mod) ->
    case eval_read_file_info(File, Mod) of
	{ok, #file_info{type=regular}} ->
	    true;
	{ok, #file_info{type=directory}} ->
	    true;
        _ ->
            false
    end.

do_is_regular(File, Mod) ->
    case eval_read_file_info(File, Mod) of
	{ok, #file_info{type=regular}} ->
	    true;
        _ ->
            false
    end.

%% fold_files(Dir, RegExp, Recursive, Fun, AccIn).

%% folds the function Fun(F, Acc) -> Acc1 over
%%   all files <F> in <Dir> that match the regular expression <RegExp>
%%   If <Recursive> is true all sub-directories to <Dir> are processed

do_fold_files(Dir, RegExp, Recursive, Fun, Acc, Mod) ->
    {ok, Re1} = re:compile(RegExp,[unicode]),
    do_fold_files1(Dir, Re1, RegExp, Recursive, Fun, Acc, Mod).

do_fold_files1(Dir, RegExp, OrigRE, Recursive, Fun, Acc, Mod) ->
    case eval_list_dir(Dir, Mod) of
	{ok, Files} -> do_fold_files2(Files, Dir, RegExp, OrigRE,
				      Recursive, Fun, Acc, Mod);
	{error, _}  -> Acc
    end.

%% OrigRE is not to be compiled as it's for non conforming filenames,
%% i.e. for filenames that does not comply to the current encoding, which should
%% be very rare. We use it only in those cases and do not want to precompile.
do_fold_files2([], _Dir, _RegExp, _OrigRE, _Recursive, _Fun, Acc, _Mod) -> 
    Acc;
do_fold_files2([File|T], Dir, RegExp, OrigRE, Recursive, Fun, Acc0, Mod) ->
    FullName = filename:join(Dir, File),
    case do_is_regular(FullName, Mod) of
	true  ->
	    case (catch re:run(File, if is_binary(File) -> OrigRE; 
					true -> RegExp end, 
			       [{capture,none}])) of
		match  -> 
		    Acc = Fun(FullName, Acc0),
		    do_fold_files2(T, Dir, RegExp, OrigRE, Recursive, Fun, Acc, Mod);
		{'EXIT',_} ->
		    do_fold_files2(T, Dir, RegExp, OrigRE, Recursive, Fun, Acc0, Mod);
		nomatch ->
		    do_fold_files2(T, Dir, RegExp, OrigRE, Recursive, Fun, Acc0, Mod)
	    end;
	false ->
	    case Recursive andalso do_is_dir(FullName, Mod) of
		true ->
		    Acc1 = do_fold_files1(FullName, RegExp, OrigRE, Recursive,
					  Fun, Acc0, Mod),
		    do_fold_files2(T, Dir, RegExp, OrigRE, Recursive, Fun, Acc1, Mod);
		false ->
		    do_fold_files2(T, Dir, RegExp, OrigRE, Recursive, Fun, Acc0, Mod)
	    end
    end.

do_last_modified(File, Mod) ->
    case eval_read_file_info(File, Mod) of
	{ok, Info} ->
	    Info#file_info.mtime;
	_ ->
	    0
    end.

do_file_size(File, Mod) ->
    case eval_read_file_info(File, Mod) of
	{ok, Info} ->
	    Info#file_info.size;
	_ ->
	    0
    end.

%%----------------------------------------------------------------------
%% +type ensure_dir(X) -> ok | {error, Reason}.
%% +type X = filename() | dirname()
%% ensures that the directory name required to create D exists

-spec ensure_dir(Name) -> 'ok' | {'error', Reason} when
      Name :: filename_all() | dirname_all(),
      Reason :: file:posix().
ensure_dir("/") ->
    ok;
ensure_dir(F) ->
    Dir = filename:dirname(F),
    case do_is_dir(Dir, file) of
	true ->
	    ok;
	false when Dir =:= F ->
	    %% Protect against infinite loop
	    {error,einval};
	false ->
	    _ = ensure_dir(Dir),
	    case file:make_dir(Dir) of
		{error,eexist}=EExist ->
		    case do_is_dir(Dir, file) of
			true ->
			    ok;
			false ->
			    EExist
		    end;
		Err ->
		    Err
	    end
    end.


%%%
%%% Pattern matching using a compiled wildcard.
%%%

do_wildcard(Pattern, Cwd, Mod) ->
    {Compiled,PrefixLen} = compile_wildcard(Pattern, Cwd),
    Files0 = do_wildcard_1(Compiled, Mod),
    Files = if
		PrefixLen =:= 0 ->
		    Files0;
		true ->
		    [lists:nthtail(PrefixLen, File) || File <- Files0]
	    end,
    lists:sort(Files).

do_wildcard_1({exists,File}, Mod) ->
    case eval_read_link_info(File, Mod) of
	{ok,_} -> [File];
	_ -> []
    end;
do_wildcard_1([Base|Rest], Mod) ->
    do_wildcard_2([Base], Rest, [], Mod).

do_wildcard_2([File|Rest], Pattern, Result, Mod) ->
    do_wildcard_2(Rest, Pattern, do_wildcard_3(File, Pattern, Result, Mod), Mod);
do_wildcard_2([], _, Result, _Mod) ->
    Result.

do_wildcard_3(Base, [[double_star]|Rest], Result, Mod) ->
    do_double_star(".", [Base], Rest, Result, Mod, true);
do_wildcard_3(Base0, [Pattern|Rest], Result, Mod) ->
    case do_list_dir(Base0, Mod) of
	{ok, Files} ->
	    Base = prepare_base(Base0),
	    Matches = do_wildcard_4(Pattern, Base, Files),
	    do_wildcard_2(Matches, Rest, Result, Mod);
	_ ->
	    Result
    end;
do_wildcard_3(Base, [], Result, _Mod) ->
    [Base|Result].

do_wildcard_4(Pattern, Base, Files) ->
    case will_always_match(Pattern) of
	false ->
	    [Base++F || F <- Files, match_part(Pattern, F)];
	true ->
	    [Base++F || F <- Files]
    end.

match_part([question|Rest1], [_|Rest2]) ->
    match_part(Rest1, Rest2);
match_part([accept], _) ->
    true;
match_part([double_star], _) ->
    true;
match_part([star|Rest], File) ->
    do_star(Rest, File);
match_part([{one_of, Ordset}|Rest], [C|File]) ->
    gb_sets:is_element(C, Ordset) andalso match_part(Rest, File);
match_part([{alt, Alts}], File) ->
    do_alt(Alts, File);
match_part([C|Rest1], [C|Rest2]) when is_integer(C) ->
    match_part(Rest1, Rest2);
match_part([X|_], [Y|_]) when is_integer(X), is_integer(Y) ->
    false;
match_part([], []) ->
    true;
match_part([], [_|_]) ->
    false;
match_part([_|_], []) ->
    false.

will_always_match([accept]) -> true;
will_always_match(_) -> false.

prepare_base(Base0) ->
    Base1 = filename:join(Base0, "x"),
    "x"++Base2 = lists:reverse(Base1),
    lists:reverse(Base2).

do_double_star(Base, [H|T], Rest, Result, Mod, Root) ->
    Full = case Root of
	       false -> filename:join(Base, H);
	       true -> H
	   end,
    Result1 = case do_list_dir(Full, Mod) of
        {ok, Files} ->
            do_double_star(Full, Files, Rest, Result, Mod, false);
        _ -> Result
    end,
    Result2 = case Root andalso Rest == [] of
        true  -> Result1;
        false -> do_wildcard_3(Full, Rest, Result1, Mod)
    end,
    do_double_star(Base, T, Rest, Result2, Mod, Root);
do_double_star(_Base, [], _Rest, Result, _Mod, _Root) ->
    Result.

do_star(Pattern, [_|Rest]=File) ->
    match_part(Pattern, File) orelse do_star(Pattern, Rest);
do_star(Pattern, []) ->
    match_part(Pattern, []).

do_alt([Alt|Rest], File) ->
    match_part(Alt, File) orelse do_alt(Rest, File);
do_alt([], _File) ->
    false.

do_list_dir(Dir, Mod) ->     eval_list_dir(Dir, Mod).

	    
%%% Compiling a wildcard.


%% Define characters used for escaping a \.
-define(ESCAPE_PREFIX, $@).
-define(ESCAPE_CHARACTER, [?ESCAPE_PREFIX,$e]).
-define(ESCAPED_ESCAPE_PREFIX, [?ESCAPE_PREFIX,?ESCAPE_PREFIX]).

%% Only for debugging.
compile_wildcard(Pattern) when is_list(Pattern) ->
    {compiled_wildcard,?HANDLE_ERROR(compile_wildcard(Pattern, "."))}.

compile_wildcard(Pattern0, Cwd0) ->
    Pattern = convert_escapes(Pattern0),
    [Root|Rest] = filename:split(Pattern),
    case filename:pathtype(Root) of
	relative ->
	    Cwd = prepare_base(Cwd0),
	    compile_wildcard_2([Root|Rest], {cwd,Cwd});
	_ ->
	    compile_wildcard_2(Rest, {root,0,Root})
    end.

compile_wildcard_2([Part|Rest], Root) ->
    case compile_part(Part) of
	Part ->
	    compile_wildcard_2(Rest, compile_join(Root, Part));
	Pattern ->
	    compile_wildcard_3(Rest, [Pattern,Root])
    end;
compile_wildcard_2([], {root,PrefixLen,Root}) ->
    {{exists,Root},PrefixLen}.

compile_wildcard_3([Part|Rest], Result) ->
    compile_wildcard_3(Rest, [compile_part(Part)|Result]);
compile_wildcard_3([], Result) ->
    case lists:reverse(Result) of
	[{root,PrefixLen,Root}|Compiled] ->
	    {[Root|Compiled],PrefixLen};
	[{cwd,Root}|Compiled] ->
	    {[Root|Compiled],length(filename:join(Root, "x"))-1}
    end.

compile_join({cwd,"."}, File) ->
    {root,0,File};
compile_join({cwd,Cwd}, File0) ->
    File = filename:join([File0]),
    Root = filename:join(Cwd, File),
    PrefixLen = length(Root) - length(File),
    {root,PrefixLen,Root};
compile_join({root,PrefixLen,Root}, File) ->
    {root,PrefixLen,filename:join(Root, File)}.

compile_part(Part0) ->
    Part = wrap_escapes(Part0),
    compile_part(Part, false, []).

compile_part_to_sep(Part) ->
    compile_part(Part, true, []).

compile_part([], true, _) ->
    badpattern(missing_delimiter);
compile_part([$,|Rest], true, Result) ->
    {ok, $,, lists:reverse(Result), Rest};
compile_part([$}|Rest], true, Result) ->
    {ok, $}, lists:reverse(Result), Rest};
compile_part([$?|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [question|Result]);
compile_part([$*,$*], Upto, Result) ->
    compile_part([], Upto, [double_star|Result]);
compile_part([$*,$*|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [star|Result]);
compile_part([$*], Upto, Result) ->
    compile_part([], Upto, [accept|Result]);
compile_part([$*|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [star|Result]);
compile_part([$[|Rest], Upto, Result) ->
    case compile_charset(Rest, ordsets:new()) of
	{ok, Charset, Rest1} ->
	    compile_part(Rest1, Upto, [Charset|Result]);
	error ->
	    compile_part(Rest, Upto, [$[|Result])
    end;
compile_part([${|Rest], Upto, Result) ->
    case compile_alt(Rest) of
	{ok, Alt} ->
	    lists:reverse(Result, [Alt]);
	error ->
	    compile_part(Rest, Upto, [${|Result])
    end;
compile_part([{escaped,X}|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [X|Result]);
compile_part([X|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [X|Result]);
compile_part([], _Upto, Result) ->
    lists:reverse(Result).

compile_charset([$]|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element($], Ordset));
compile_charset([], _Ordset) ->
    error;
compile_charset(List, Ordset) ->
    compile_charset1(List, Ordset).

compile_charset1([Lower, $-, Upper|Rest], Ordset) when Lower =< Upper ->
    compile_charset1(Rest, compile_range(Lower, Upper, Ordset));
compile_charset1([$]|Rest], Ordset) ->
    {ok, {one_of, gb_sets:from_ordset(Ordset)}, Rest};
compile_charset1([{escaped,X}|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element(X, Ordset));
compile_charset1([X|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element(X, Ordset));
compile_charset1([], _Ordset) ->
    error.
    
compile_range(Lower, Current, Ordset) when Lower =< Current ->
    compile_range(Lower, Current-1, ordsets:add_element(Current, Ordset));
compile_range(_, _, Ordset) ->
    Ordset.

compile_alt(Pattern) ->
    compile_alt(Pattern, []).

compile_alt(Pattern, Result) ->
    case compile_part_to_sep(Pattern) of
	{ok, $,, AltPattern, Rest} ->
	    compile_alt(Rest, [AltPattern|Result]);
	{ok, $}, AltPattern, Rest} ->
	    NewResult = [AltPattern|Result],
	    RestPattern = compile_part(Rest),
	    {ok, {alt, [Alt++RestPattern || Alt <- NewResult]}};
	Pattern ->
	    error
    end.

%% Convert backslashes to an illegal Unicode character to
%% protect in from filename:split/1.

convert_escapes([?ESCAPE_PREFIX|T]) ->
    ?ESCAPED_ESCAPE_PREFIX ++ convert_escapes(T);
convert_escapes([$\\|T]) ->
    ?ESCAPE_CHARACTER ++ convert_escapes(T);
convert_escapes([H|T]) ->
    [H|convert_escapes(T)];
convert_escapes([]) ->
    [].

%% Wrap each escape in a tuple to remove the special meaning for
%% the character that follows.

wrap_escapes(?ESCAPED_ESCAPE_PREFIX ++ T) ->
    [?ESCAPE_PREFIX|wrap_escapes(T)];
wrap_escapes(?ESCAPE_CHARACTER ++ [C|T]) ->
    [{escaped,C}|wrap_escapes(T)];
wrap_escapes(?ESCAPE_CHARACTER) ->
    [];
wrap_escapes([H|T]) ->
    [H|wrap_escapes(T)];
wrap_escapes([]) ->
    [].

badpattern(Reason) ->
    error({badpattern,Reason}).

eval_read_file_info(File, file) ->
    file:read_file_info(File);
eval_read_file_info(File, erl_prim_loader) ->
    case erl_prim_loader:read_file_info(File) of
	error -> {error, erl_prim_loader};
	Res-> Res
    end;
eval_read_file_info(File, Mod) ->
    Mod:read_file_info(File).

eval_read_link_info(File, file) ->
    file:read_link_info(File);
eval_read_link_info(File, erl_prim_loader) ->
    case erl_prim_loader:read_link_info(File) of
        error -> {error, erl_prim_loader};
        Res-> Res
    end;
eval_read_link_info(File, Mod) ->
    Mod:read_link_info(File).

eval_list_dir(Dir, file) ->
    file:list_dir(Dir);
eval_list_dir(Dir, erl_prim_loader) ->
    case erl_prim_loader:list_dir(Dir) of
	error -> {error, erl_prim_loader};
	Res-> Res
    end;
eval_list_dir(Dir, Mod) ->
    Mod:list_dir(Dir).

%% Getting the rules to use for file search

keep_dir_search_rules(Rules) ->
    [T || {_,_}=T <- Rules].

keep_suffix_search_rules(Rules) ->
    [T || {_,_,_}=T <- Rules].

get_search_rules() ->
    case application:get_env(kernel, source_search_rules) of
        undefined -> default_search_rules();
        {ok, []}  -> default_search_rules();
        {ok, R} when is_list(R) -> R
    end.

default_search_rules() ->
    [%% suffix-speficic rules for source search
     {".beam", ".erl", erl_source_search_rules()},
     {".erl", ".yrl", []},
     {"", ".src", erl_source_search_rules()},
     {".so", ".c", c_source_search_rules()},
     {".o", ".c", c_source_search_rules()},
     {"", ".c", c_source_search_rules()},
     {"", ".in", basic_source_search_rules()},
     %% plain old directory rules, backwards compatible
     {"", ""}] ++ erl_source_search_rules().

basic_source_search_rules() ->
    (erl_source_search_rules()
     ++ c_source_search_rules()).

erl_source_search_rules() ->
    [{"ebin","src"}, {"ebin","esrc"},
     {"ebin",filename:join("src", "*")},
     {"ebin",filename:join("esrc", "*")}].

c_source_search_rules() ->
    [{"priv","c_src"}, {"priv","src"}, {"bin","c_src"}, {"bin","src"}, {"", "src"}].

%% Looks for a file relative to a given directory

-type find_file_rule() :: {ObjDirSuffix::string(), SrcDirSuffix::string()}.

-spec find_file(filename(), filename()) ->
        {ok, filename()} | {error, not_found}.
find_file(Filename, Dir) ->
    find_file(Filename, Dir, []).

-spec find_file(filename(), filename(), [find_file_rule()]) ->
        {ok, filename()} | {error, not_found}.
find_file(Filename, Dir, []) ->
    find_file(Filename, Dir, get_search_rules());
find_file(Filename, Dir, Rules) ->
    try_dir_rules(keep_dir_search_rules(Rules), Filename, Dir).

%% Looks for a source file relative to the object file name and directory

-type find_source_rule() :: {ObjExtension::string(), SrcExtension::string(),
                             [find_file_rule()]}.

-spec find_source(filename()) ->
        {ok, filename()} | {error, not_found}.
find_source(FilePath) ->
    find_source(filename:basename(FilePath), filename:dirname(FilePath)).

-spec find_source(filename(), filename()) ->
        {ok, filename()} | {error, not_found}.
find_source(Filename, Dir) ->
    find_source(Filename, Dir, []).

-spec find_source(filename(), filename(), [find_source_rule()]) ->
        {ok, filename()} | {error, not_found}.
find_source(Filename, Dir, []) ->
    find_source(Filename, Dir, get_search_rules());
find_source(Filename, Dir, Rules) ->
    try_suffix_rules(keep_suffix_search_rules(Rules), Filename, Dir).

try_suffix_rules(Rules, Filename, Dir) ->
    Ext = filename:extension(Filename),
    try_suffix_rules(Rules, filename:rootname(Filename, Ext), Dir, Ext).

try_suffix_rules([{Ext,Src,Rules}|Rest], Root, Dir, Ext)
  when is_list(Src), is_list(Rules) ->
    case try_dir_rules(add_local_search(Rules), Root ++ Src, Dir) of
        {ok, File} -> {ok, File};
        _Other ->
            try_suffix_rules(Rest, Root, Dir, Ext)
    end;
try_suffix_rules([_|Rest], Root, Dir, Ext) ->
    try_suffix_rules(Rest, Root, Dir, Ext);
try_suffix_rules([], _Root, _Dir, _Ext) ->
    {error, not_found}.

%% ensuring we check the directory of the object file before any other directory
add_local_search(Rules) ->
    Local = {"",""},
    [Local] ++ lists:filter(fun (X) -> X =/= Local end, Rules).

try_dir_rules([{From, To}|Rest], Filename, Dir)
  when is_list(From), is_list(To) ->
    case try_dir_rule(Dir, Filename, From, To) of
	{ok, File} -> {ok, File};
	error      -> try_dir_rules(Rest, Filename, Dir)
    end;
try_dir_rules([], _Filename, _Dir) ->
    {error, not_found}.

try_dir_rule(Dir, Filename, From, To) ->
    case lists:suffix(From, Dir) of
	true ->
	    NewDir = lists:sublist(Dir, 1, length(Dir)-length(From))++To,
	    Src = filename:join(NewDir, Filename),
	    case is_regular(Src) of
		true -> {ok, Src};
		false -> find_regular_file(wildcard(Src))
	    end;
	false ->
	    error
    end.

find_regular_file([]) ->
    error;
find_regular_file([File|Files]) ->
    case is_regular(File) of
        true -> {ok, File};
        false -> find_regular_file(Files)
    end.
