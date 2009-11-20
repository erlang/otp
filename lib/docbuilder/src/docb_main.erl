%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_main).

-export([process/2,
	 parse/2, parse1/2,
	 pp/2,
	 insert_after/3,
	 transform/5, pp/5,
	 include_file/2, include/3,
	 eval_str/1,
	 validate_html/1
	]).
-export([do_parse_sgmls/1]).

%%----------------------------------------------------------------------

%% process(File, Opts) -> errors | ok
%% Parses the source file File and transforms the result to html,
%% latex and/or man page format.
process(File, Opts) ->

    File1 = File ++ ".tmpconv",
    os:cmd("sed -e 's/xi:include[ \t]*href/include file/g' -e 's/xmlns:xi=\"http:\\/\\/www.w3.org\\/2001\\/XInclude\"//g' < " ++ 
	   File ++ ".xml > " ++ File1 ++ ".xml"), %LATH
    
    case parse1(File1, Opts) of
	errors ->
	    file:delete(File1 ++ ".xml"),
	    errors;
	{ok, Tree} ->
	    From = element(1, Tree),
	    Tos0 =
		lists:foldl(
		  fun(latex, Acc) -> [latex|Acc];
		     (html, Acc)  -> [html|Acc];
		     ({man, _Section}, Acc) -> [man|Acc];
		     (_, Acc) -> Acc
		  end, [], Opts),

	    %% If no target format is specified, assume HTML:
	    Tos = if
		      Tos0==[] -> [html];
		      true -> Tos0
		  end,

	    Result = [transform(From, To, Opts, File, Tree)||To <- Tos], 
	    case lists:member(transformation_error,Result) of 
		true -> 	
		    file:delete(File1 ++ ".xml"),
		    errors;
		_ -> 
		    file:delete(File1 ++ ".xml"),
		    ok
	    end
    
    end.

%%----------------------------------------------------------------------

%% parse(File, Opts) -> {ok, Tree} | errors
%% Parses the source file File, resulting in a tree structure.
parse(File, Opts) ->
    case docb_util:lookup_option(src_type, Opts) of
	".xml" ->
	    parse_xml(File++".xml", Opts);
	".sgml" ->
	    parse_sgml(File, Opts)
    end.

%% parse1(File, Opts) -> {ok, Tree} | errors
%% Like parse/2, but in the SGML case also prints the parse errors
%% (in File.html.sgmls_errs) information to stdout.
parse1(File, Opts) ->
    parse(File, [{print_parse_errs, true}|Opts]).


validate_html(InFile) ->
    ScanOpts = [{validation,true}, {fetch_fun, fun fetch_dtd/2}],
    case xmerl_scan:file(InFile, ScanOpts) of
	{_XMLTuple,[]} -> % ok
	    {InFile,ok};
	{'EXIT',Reason} ->
	    {InFile,Reason}
    end.

fetch_dtd({public,_,"http://www.w3.org/TR/xhtml1/DTD/"++ Rest},GlobalState) ->
    Filename = filename:join(docb_util:dtd_dir(),Rest),
    {ok,{file,Filename},GlobalState};
fetch_dtd({public,_,Str},GlobalState) ->
    {ok,{file,filename:join(docb_util:dtd_dir(),Str)},GlobalState}.
    
    

parse_xml(InFile, Opts) ->
    DtdDir = docb_util:dtd_dir(),
    ScanOpts = [{validation,true}, {fetch_path, [DtdDir]}],
    PrintP = docb_util:lookup_option(print_parse_errs, Opts),
    case catch xmerl_scan:file(InFile, ScanOpts) of
	{'EXIT', Error} when PrintP ->
	    docb_util:message(error,
			      "XML validation error:~n~p", [Error]),
	    errors;
	{'EXIT', _Error} ->
	    errors;
	{error, Reason} -> % probably file error
	    docb_util:message(error, "XML scan error: ~p", [Reason]),
	    errors;
	{Doc, []} ->
	    case catch xmerl:export([Doc], docb_xmerl_tree_cb) of
		[Tree] ->
		    verify(Tree),
		    {ok, Tree};
		{'EXIT', Error} ->
		    docb_util:message(error,
				      "XML export error:~n~p", [Error]),
		    errors
	    end
    end.

parse_sgml(InFile, Opts) ->

    Pfx = tmp_file_prefix(InFile, Opts),
    OutFile = Pfx ++ "sgmls_output",
    ErrFile = Pfx ++ "sgmls_errs",

    EntVals = lists:usort(docb_util:lookup_options(ent, Opts)),
    Ents = lists:flatten([" -ent " ++ Val || Val <- EntVals]),
    Cmd = docb_util:old_docb_dir() ++ "/bin/docb_sgmls_run " ++
	Ents ++ " " ++ InFile ++ ".sgml " ++ OutFile ++ " " ++ ErrFile,

    case os:cmd(Cmd) of
	[] ->
	    PrintP = docb_util:lookup_option(print_parse_errs, Opts),
	    case filelib:file_size(ErrFile) of
		0 -> % implies no errors
		    parse_sgmls(InFile, OutFile);
		_ when PrintP ->
		    cat(ErrFile),
		    errors;
		_ ->
		    errors
	    end;
	Msg ->
	    docb_util:message(error, "~p", [Msg]),
	    errors
    end.

tmp_file_prefix(File, Opts) ->
    lists:concat(
      [File, "." | lists:foldl(
		     fun(latex, Acc) -> ["latex."|Acc];
			(html, Acc)  -> ["html."|Acc];
			({man, Section}, Acc) -> ["man", Section, "."|Acc];
			(_, Acc) -> Acc
		     end, [], Opts)]).

parse_sgmls(InFile, SgmlsFile) ->
    case file:open(SgmlsFile, [read]) of
	{ok, Fd} ->
	    Res = case (catch do_parse_sgmls(Fd)) of
		      {ok, Tree} ->
			  {ok, Tree};
		      {'EXIT', Reason} ->
			  docb_util:message(
			    error,
			    "Cannot parse sgmls output file "
			    "~s, obtained from parsing ~s, "
			    "reason: ~w",
			    [SgmlsFile, InFile, Reason]),
			  errors;
		      {error, Reason} ->
			  docb_util:message(
			    error,
			    "Cannot parse sgmls output file "
			    "~s, obtained from parsing ~s, "
			    "reason: ~w",
			    [SgmlsFile, InFile, Reason]),
			  errors
		  end,
	    file:close(Fd),
	    case Res of
		{ok, Tree0} ->
		    verify(Tree0),
		    {ok, Tree0};
		_Other ->
		    errors
	    end;
	{error, Reason} ->
	    docb_util:message(error,
			      "Cannot open sgmls output file ~s, "
			      "obtained from parsing ~s, reason: ~w",
			      [SgmlsFile, InFile, Reason]),
	    errors
    end.

do_parse_sgmls(Fd) ->
    do_parse_sgmls(Fd, []).

do_parse_sgmls(Fd, Attrs) ->
    case get_line(Fd) of
	{attrs, A} ->
	    do_parse_sgmls(Fd, [A|Attrs]);
	{startTag, Tag} ->
	    {ok, {Tag, Attrs, get_args(Fd)}};
	Other ->
	    {error, Other}
    end.

get_args(Fd) ->
    case get_line(Fd) of
	{startTag, Tag} ->
	    H = {Tag, [], get_args(Fd)},
	    [H|get_args(Fd)];
	{dataTag, Str} ->
	    [{pcdata, [], Str}|get_args(Fd)];
	{attrs, A} ->
	    get_args_attr(Fd, [A]);
	close ->
	    [];
	ok ->
	    []
    end.

get_args_attr(Fd, Attrs) ->
    case get_line(Fd) of
	{startTag, Tag} ->
	    H = {Tag, lists:reverse(Attrs), get_args(Fd)},
	    [H|get_args(Fd)];
	{dataTag, Str} ->
	    [{pcdata, lists:reverse(Attrs), Str}|get_args(Fd)];
	{attrs, A} ->
	    get_args_attr(Fd, [A|Attrs]);
	close ->
	    [];
	ok ->
	    []
    end.

get_line(Fd) ->
    Str = io:get_line(Fd, ''),
    case Str of
	[$(|T] ->
	    {startTag, tag_name(T)};
	[$-|T] ->
	    {dataTag, T};
	[$)|_T] ->
	    close;
	[$A|T] ->
	    {attrs, attrs(remove_nl(T))};
	[$?|_T] ->
	    get_line(Fd);
	[$C|_] ->
	    ok
    end.

remove_nl([$\n|_]) -> [];
remove_nl([H|T])    -> [H|remove_nl(T)];
remove_nl([])       -> [].

%% attrs
%% splits a string like
%% AAAAA BBBBB ......
%% into {"AAA", "BBB", Rest}

attrs(T) ->
    {X, T1} = get_item(T),
    {Y, T2} = get_item(T1),
    T3 = skip_blanks(T2),
    {X, Y, T3}.

get_item(T) -> get_item(skip_blanks(T), []).

get_item([$ |T], L) -> {lists:reverse(L), [$ |T]};
get_item([H|T], L)  -> get_item(T, [H|L]);
get_item([], L)     -> {lists:reverse(L), []}.

skip_blanks([$ |T]) -> skip_blanks(T);
skip_blanks(T)      -> T.

tag_name(Str) -> tag_name(Str, []).

tag_name([H|T], L) when $A =< H, H =< $Z ->
    tag_name(T, [H-$A+$a|L]);
tag_name([$\n], L) ->
    list_to_atom(lists:reverse(L));
tag_name([H|T], L) ->
    tag_name(T, [H|L]).

cat(File) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    cat1(Fd),
	    file:close(Fd);
	Other ->
	    Other
    end.

cat1(Fd) ->
    case io:get_line(Fd, '') of
	eof ->
	    eof;
	Str ->
	    io:format("~s", [Str]),
	    cat1(Fd)
    end.

%%----------------------------------------------------------------------

verify(Tree) -> verify(Tree, [], 1).

verify({pcdata, Optional, _}, Path, Level) ->
    verify_optional(Optional, Path, Level);
verify({Tag, Optional, Args}, Path, Level) when is_list(Args) ->
    case verify_optional(Optional, Path, Level) of
	true ->
	    verify_list(Args, [Tag|Path], Level);
	false ->
	    false
    end;
verify(Other, Path, Level) ->
    verify_error(Other, Path, Level).

verify_error(X, Path, Level) ->
    docb_util:message(error, "Invalid object found at: ~p level:~w~n~s",
		      [Path, Level, docb_pretty_format:term(X)]),
    false.

verify_list([H|T], Path, Level) ->
    case verify(H, Path, Level) of
	true ->
	    verify_list(T, Path, Level +1);
	false ->
	    false
    end;
verify_list([], _, _) ->
    true.

verify_optional([{_, _, _}|T], Path, Level) ->
    verify_optional(T, Path, Level);
verify_optional([], _Path, _Level) ->
    true;
verify_optional(X, Path, Level) ->
    verify_error(X, Path, Level).

%%----------------------------------------------------------------------

%% pp(File, Opts) -> {ok, OutFile} | errors
%% Parses the source file and, if successful, prints the resulting tree
%% structure to a file with the extension ".pp".
pp(File, Opts) ->
    case parse(File, Opts) of
	{ok, Tree} ->
	    OutFile = File ++ ".pp",
	    dump(OutFile, Tree),
	    {ok, OutFile};
	errors ->
	    errors
    end.

dump(File, Struct) ->
    {ok, Stream} = file:open(File, [write]),
    io:format("Info: Dump on ~p ...", [File]),
    io:format(Stream, "~n~s~n", [docb_pretty_format:term(Struct)]),
    io:format(" done.\n"),
    file:close(Stream).

%%----------------------------------------------------------------------

%% insert_after(Tag, Tree, Obj) -> Tree | {'EXIT', Reason}
%% Insert an element in a tree structure
insert_after(Tag, Tree, Obj) ->
    edit(Tag, Tree, {insert_after, Obj}).

%% edit Op = delete, insert_before, insert_after
edit(Tag, Tree, Op) ->
    case catch edit1(Tag, Tree, Op) of
	error ->
	    docb_util:message(error, "Cannot do ~p to ~w", [Op, Tag]),
	    Tree;
	Other ->
	    Other
    end.

edit1(Tag, {Tag, _O, _A}, _Op) ->
    throw(error);
edit1(Tag, {Tag1, O, A}, Op) ->
    {Tag1, O, edit1_list(Tag, A, Op)};
edit1(_, _, _) ->
    throw(error).

edit1_list(Tag, [{pcdata, Str}|T], Op) ->
    [{pcdata, Str}|edit1_list(Tag, T, Op)];
edit1_list(Tag, [{Tag, O, A}|T], {insert_after, Obj}) ->
    [{Tag, O, A}, Obj|T];
edit1_list(Tag, [H|T], Op) ->
    [H|edit1_list(Tag, T, Op)];
edit1_list(_Tag, [], _Op) ->
    [].

%%______________________________________________________________________

%% transform(From, To, Opts, File, Tree) -> void()
%% Actual transformation of tree structure to desired format.
transform(From, To, Opts, File, Tree) ->
    Filter = if
		 To==html; To==kwic ->
		     list_to_atom("docb_tr_" ++ atom_to_list(From) ++
				  [$2|atom_to_list(To)]);
		 true ->
		     list_to_atom("sgml_tr_" ++ atom_to_list(From) ++
				  [$2|atom_to_list(To)])
	     end,

    case catch apply(Filter, transform, [File, Tree, Opts]) of

	%% R5C
	{'EXIT', {undef, [{Filter, transform, [File, Tree, Opts]}|_]}}->
	    %% No transformation defined
	    finish_transform(Tree, File, Opts, Filter);
	
	{'EXIT', {undef, {Filter, transform, [File, Tree, Opts]}}} ->
	    %% No transformation defined
	    finish_transform(Tree, File, Opts, Filter);

	{'EXIT', What} ->
	    docb_util:message(error,
			      "Transformation trouble in ~P", [What, 9]),
	    transformation_error;

	{error, Reason} ->
	    docb_util:message(error, Reason),
	    transformation_error;
	
	{Tree1, Opts1} ->
	    %% transformation returning both new parse and new options
	    finish_transform(Tree1, File, Opts1, Filter);
	
	Tree1 ->
	    %% transformation returning only new parse
	    finish_transform(Tree1, File, Opts, Filter)
    end.

finish_transform(Tree, File, Opts, Filter) ->
    {Str, NewOpts} = pp(Tree, [], 1, Filter, Opts),
    Extension =
	case catch apply(Filter, extension, [NewOpts]) of
	    {'EXIT', _} ->
		apply(Filter, extension, []);
	    Others ->
		Others
	end,
    {ok, Out} =
	file:open(docb_util:outfile(File, Extension, NewOpts), [write]),
    put_chars(Out, Str),
    file:close(Out).

put_chars(Out, Str) -> put_chars(Out, Str, 0).

put_chars(Out, [$\n|Cs], _Pos) ->
    io:put_chars(Out, [$\n]),
    put_chars(Out, Cs, 0);

put_chars(Out, [$\011|Cs], Pos) -> % tab
    TabbedPos = 8*((Pos div 8)+1),
    Nblanks = TabbedPos - Pos,
    io:put_chars(Out, lists:duplicate(Nblanks, $ )),
    put_chars(Out, Cs, Pos+Nblanks);

put_chars(Out, [C|Cs], Pos) when is_integer(C) ->
    io:put_chars(Out, [C]),
    put_chars(Out, Cs, Pos+1);

put_chars(Out, [L|Cs], Pos) when is_list(L) ->
    put_chars(Out, Cs, put_chars(Out, L, Pos));

put_chars(_Out, [], Pos) ->
    Pos.

pp({Tag, Optional, Args}, TagPath, Level, Filter, Opts) ->
    TagPath1  = [Tag|TagPath],
    Optional1 = reduce_optional(Optional),

    %% First try Filter:rule/3. It returns {Return, NewOpts}
    %%                          where Return is as from rule/2:
    Rule_3_result =
	case catch Filter:rule(TagPath1, {Level,Optional1,Args},Opts) of
	    %% R5C
	    {'EXIT', {undef, [{_, rule, _}|_]}} -> % No rule/3 defined
		failed;

	    {'EXIT', {undef, {_, rule, _}}} -> % No rule/3 defined
		failed;
	    %% R5C
	    {'EXIT', {function_clause, [{_, rule, _}|_]}} -> % No MATCHING rule/3
		failed;

	    {'EXIT', {function_clause, {_, rule, _}}} -> % No MATCHING rule/3
		failed;

	    {'EXIT', What} ->
		docb_util:message(error,
				  "Serious Error: ~P", [What, 9]);
	    Others ->
		Others
	end,
    handle_rule_call_result({r3, Rule_3_result}, Filter, TagPath1, Tag,
			    Level, Optional1, Args, Opts).

handle_rule_call_result({r3, failed}, Filter, TagPath1, Tag, Level, Optional1,
			Args, Opts) ->
    %% Hmmm, try Filter:rule/2
    Rule_2_result = (catch Filter:rule(TagPath1, {Level, Optional1, Args})),
    handle_rule_call_result({r2, Rule_2_result}, Filter, TagPath1, Tag,
			    Level, Optional1, Args, Opts);
handle_rule_call_result({r3, {Result, NewOpts}}, Filter, TagPath1, Tag, Level,
			Optional1, Args, _Opts) ->
    handle_rule_call_result({r2, Result}, Filter, TagPath1, Tag, Level,
			    Optional1, Args, NewOpts);
handle_rule_call_result({_, {func, F}}, _Filter, _TagPath1, _Tag, _Level,
			_Optional1, Args, Opts) ->
    {F(Args), Opts};
handle_rule_call_result({_, {'EXIT', Why}}, _Filter, TagPath1, _Tag, Level,
			Optional1, Args, Opts) ->
    report_error(TagPath1, Why, {Level, Optional1, Args}),
    {[], Opts};
handle_rule_call_result({_, {drop, Str}}, _Filter, _TagPath1, _Tag, _Level,
			_Optional1, _Args, Opts) ->
    {[Str], Opts};
handle_rule_call_result({_, {newargs, NewArgs}}, Filter, TagPath1, _Tag, _Level,
			_Optional1, _Args, Opts) ->
    {List, NewOpts} = pp_list(NewArgs, TagPath1, 1, Filter, Opts),
    {[List], NewOpts};
handle_rule_call_result({_, {newargs, Before, NewArgs, After}}, Filter, TagPath1, _Tag, _Level,
			_Optional1, _Args, Opts) ->
    {List, NewOpts} = pp_list(NewArgs, TagPath1, 1, Filter, Opts),
    {[Before, List, After], NewOpts};
handle_rule_call_result({_, {Before, After}}, Filter, TagPath1, _Tag, _Level,
			_Optional1, Args, Opts) when is_list(Before) ->
    {List, NewOpts} = pp_list(Args, TagPath1, 1, Filter, Opts),
    {[Before, List, After], NewOpts}.

pp_list([H|T], TagPath, Level, Rules, Opts) ->
    {Hpp, Hopts} = pp(H, TagPath, Level, Rules, Opts),
    {Tpp, Tops} = pp_list(T, TagPath, Level + 1, Rules, Hopts),
    {[Hpp|Tpp], Tops};
pp_list([], _, _, _, Opts) ->
    {[], Opts}.

reduce_optional([{_, _, H}|T]) -> [H|reduce_optional(T)];
reduce_optional([])          -> [].

report_error(Arg1, Cause, Arg2) ->
    [Tag|_] = Arg1,
    docb_util:message(error,
		      "Formatting trouble in ~p: ~p", [Tag, Cause]),
    docb_util:message(error, "Failure in rule(~p, ~p)", [Arg1, Arg2]).

%%----------------------------------------------------------------------

%% include_file(File, Tag) -> {ok, String} | error
include_file(File, Tag) ->
    include(File, "%S" ++ Tag, "%E" ++ Tag).

%% include(File, StartTag, StopTag) -> {ok, String} | error
include(File, "", "") ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    String = include_all(Fd),
	    file:close(Fd),
	    {ok, String};
	_ ->
	    docb_util:message(error,
			      "Include file ~s not found", [File]),
	    error
    end;
include(File, StartTag, StopTag) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    String = extract(File, Fd, StartTag, StopTag, searching),
	    file:close(Fd),
	    {ok, lists:flatten(String)};
	_ ->
	    docb_util:message(error,
			      "Include file ~s not found", [File]),
	    error
    end.

include_all(Fd) ->
    case io:get_line(Fd, '') of
	eof ->
	    [];
	ListOfChars ->
	    lists:append(ListOfChars, include_all(Fd))
    end.

extract(File, Fd, StartTag, StopTag, State) ->
    Line=io:get_line(Fd, ''),
    extract(File, Fd, StartTag, StopTag, State, Line).

extract(File, _, _, _, _, eof) ->
    docb_util:message(error,
		      "Premature end of file in include file ~p",
		      [File]),
    [];
extract(File, Fd, StartTag, StopTag, searching, Line) ->
    case regexp:match(Line, "^" ++ StartTag) of
	{match, _Start, _Length} ->
	    extract(File, Fd, StartTag, StopTag, copying);
	nomatch ->
	    extract(File, Fd, StartTag, StopTag, searching);
	{error, _Error} ->
	    docb_util:message(error, "Bad syntax in ~s", [File]),
	    []
    end;
extract(File, Fd, StartTag, StopTag, copying, Line) ->
    case regexp:match(Line, "^" ++ StopTag) of
	{match, _Start, _Length} ->
	    [];
	nomatch ->
	    [Line|extract(File, Fd, StartTag, StopTag, copying)];
	{error, _Error} ->
	    docb_util:message(error, "Bad syntax in ~s", [File]),
	    []
    end.

%%----------------------------------------------------------------------

eval_str(Str) ->
    case lib:eval_str(Str) of
	{error, Report} ->
	    docb_util:message(error,
			      "ErlEval failed: ~s (~s)", [Str, Report]);
	{ok, S} ->
	    io_lib:format("~p~n", [S])
    end.
