%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2015. All Rights Reserved.
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

-module(demo_html_tagger).

%% You will notice that this program has very few type declarations
%% That's because this program uses some pretty dodgy techniques to
%% get at the data it requires.

%% I use epp_dodger to parse the file and the new imporved erl_scan
%% find the exact values of the tokens

%% epp_dodger returns an objects of type erl_syntax which are pretty
%% nasty buggers. We could write the types out but it would hardly
%% help.

%% to test run

%%-compile(export_all).


-export([erl2htmltext/1, erl2htmlfile/1]).

erl2htmltext(File) ->
    try
	erl2html0(File)
    catch
	What:Why ->
	    io:format("error in:~s ~p ~p~n",[File,What,Why])
    end.

erl2htmlfile(File) ->
    try
	Text = erl2html0(File),
	Root = filename:basename(filename:rootname(File)),
	Out = "./html/" ++ Root ++ ".html",
	file:write_file(Out, [Text])
    catch
	What:Why ->
	    io:format("error in:~s ~p ~p~n",[File,What,Why])
    end.


splitErlang(File) ->
    {ok, Forms} = dodge_file(File),
    {Anchors, Patches} = analyse(Forms),
    Raw = read_raw_forms(File),
    Raw1 = merge_anchors(Anchors, Raw),
    Raw2 = merge_forms(Raw1, Patches, []),
    Rtf = [final(I) || I <- Raw2],
    {taggedBlocks, Rtf}.

erl2html0(File) ->
    Tb = splitErlang(File),
    Html = to_html(Tb),
    prelude(Html).

merge_forms([{Tag,L1}|T], Patches, L) ->
    {L2, Patches1} = apply_patches(L1, Patches),
    merge_forms(T, Patches1, [{Tag,L2}|L]);
merge_forms([], _, L) ->
    lists:reverse(L).

apply_patches(Toks, [])      ->
    %% we've run out of patches but we must still simplify 
    %% every term
    {[simplify(I) || I <- Toks], []};
apply_patches(Toks, Patches) ->
    apply_patches(Toks, Patches, []).

apply_patches([{atom,Ln,Val}=A|T], Patches, L) ->
    case do_patch(Ln, Patches)  of
	{yes, New, Patches1} ->
	    New1 = reformat(New, Val),
	    apply_patches(T, Patches1, [New1|L]);
	{no, Patches1} ->
	    apply_patches(T, Patches1, [simplify(A)|L])
    end;
apply_patches([H|T], Patches, L) ->
    apply_patches(T, Patches, [simplify(H)|L]);
apply_patches([], Patches, L) ->
    {lists:reverse(L), Patches}.


simplify({atom,_,Str}) ->
    case (catch list_to_existing_atom(Str)) of
	{'EXIT', _} ->
	    {atom, Str};
	A ->
	    case is_keyword(A) of
		true -> {keyword, Str};
		false ->
		    {atom, Str}
	    end
    end;
simplify({dot,_,Str}) ->
    {terminal, Str};
simplify({Tag,_,Str}) ->
    case is_keyword(Tag) of
	true ->
	    {keyword, Str};
	false ->
	    case is_terminal(Tag) of
		true ->
		    {terminal, Str};
		false ->
		    {Tag, Str}
	    end
    end;
simplify(X) ->
    io:format("simplify wtfit:~p~n",[X]),
    X.

do_patch(Ln, [{Ln,Tag}|P])                  -> {yes, Tag, P};
do_patch(Ln, [{Ln1,_}|_] = P) when Ln1 > Ln -> {no, P};
do_patch(Ln, [_|T])                         -> do_patch(Ln, T);
do_patch(_, [])                             -> {no, []}.

reformat({local,{F,A}}, Str) -> {local,F,A,Str};
reformat({remote,M,F,A}, Str) -> {remote,M,F,A,Str};
reformat({remote,{M,F,A}}, Str) -> {remote,M,F,A,Str};
reformat({bif,{F,A}}, Str) -> {bif,F,A,Str};
reformat(Tag, Str) ->
    io:format("reformat*:~p ~p~n",[Tag,Str]),
    {Tag,Str}.

to_html({taggedBlocks, L}) ->
    [[anchor1(Anchor),to_html(Body)] || {Anchor,Body} <- L];
to_html({taggedToks, L}) ->
    [to_html1(I) || I <- L].

anchor1({func, F, A}) ->
    ["<a name='",linkname(F,A),"'></a>"];
anchor1({specification, F, A}) ->
    ["<a name='",linkname(F,A),"'></a>"];
anchor1(_X) ->
    "".

linkname(F, A) when is_atom(F) ->
    a2s(F) ++ "-" ++ integer_to_list(A);
linkname(F, A) when is_list(F) ->
    F ++ "-" ++ integer_to_list(A).

a2s(A) ->
    atom_to_list(A).

font(C, S) ->
    ["<font color=\"", C, "\">", htmlquote(S), "</font>"].

htmlquote("<" ++ T) -> ["&lt;"|htmlquote(T)];
htmlquote([H|T]) -> [H|htmlquote(T)];
htmlquote([]) -> [].

to_html1({white_space,V}) -> V;
to_html1({comment, V})    -> font("#B22222", V);
to_html1({var,V})         -> font("orange", V);
to_html1({string,V})      -> font("#FA8072", V);
to_html1({integer,V})     -> font("#1111AA", V);
to_html1({bif,_F,_A,Str}) -> font("#FF00FF", Str);
to_html1({keyword, V})    -> font("#FF00FF", V);
to_html1({atom, V})       -> V;
to_html1({terminal,V})    -> V;
to_html1({char,V})        -> V;
to_html1({float,V})       -> V;
to_html1({anchor,F,A}) ->
    ["<a name='",linkname(F,A),"'></a>"];
to_html1({local,F,A,Str}) ->  
    ["<a href='#",linkname(F,A),"'>", 
     htmlquote(Str),"</a>"];
to_html1({remote,_M,_F,_A,Str}) ->  
    %%["<a href='",htmlname(M), "#",linkname(F,A),"'>",htmlquote(Str),"</a>"],
    Str.

%% merge the anchors
%% there should be one block per anchor
%% we check the containing form (for safety)

%% merge_anchors([{_,{file,_}}|A], B) -> 
%%     merge_anchors(A, B);
merge_anchors([{Tag,Val}=H|A], [B|T])  ->
    case contains(Tag, B) of
	true ->
	    [{Val,B}|merge_anchors(A, T)];
	false ->
	    io:format("Logic error: H=~p B=~p~n",[H,B]),
	    exit(1)
    end;
merge_anchors([], []) -> [];
merge_anchors([], [X]) ->
    %% this is the last block -
    %% trailing white space and comments have no tag
    %% because eos is not a tag ...
    [{eof, X}];
merge_anchors(X, Y) -> 
    io:format("ops:~p~n",[{X,Y}]),
    [].

contains(Loc, [{_,Loc,_}|_]) -> true;
contains(Loc, [_|T]) ->  contains(Loc, T);
contains(_, []) -> false.


dodge_file(File) ->   
    case file:open(File, [read]) of
	{ok, Handle} ->
	    {ok, F} = epp_dodger:parse(Handle, {1,1}),
	    file:close(Handle),
	    L = [revert_forms(I) || I <- F],
	    {ok, L};
	Error ->
	    Error
    end.

revert_forms(F) ->
    case erl_syntax:is_form(F) of
	true ->
	    %% revert fails on ifdef ... etc
	    case (catch erl_syntax:revert(F)) of
		{'EXIT', _Why} ->
		    io:format("error reverting:~p=~p~n",[F,_Why]),
		    F;
		Other ->
		    Other
	    end;
	false ->
	    io:format("uugh:~p~n",[F])
    end.

%% read up to dot
%% read_raw_forms(File) -> [form()]
%% form() = [tok()]
%% tok() = {Type,{Line::int,Col::int},string}
%% Type = atom | int | var | string ...

read_raw_forms(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    loop(erl_scan:tokens([], Str, {1,1}, [return,text]), []).

loop({done, {eof,_}, eof}, L) ->
    lists:reverse(L);
loop({done, {ok, Toks, _}, eof}, L) ->
    lists:reverse([normalize_toks(Toks)|L]);
loop({done, {ok, Toks, Ln}, Str1}, L) ->
    loop(erl_scan:tokens([], Str1, Ln, [return,text]), 
	 [normalize_toks(Toks)|L]);
loop({more, X}, L) ->
    loop(erl_scan:tokens(X, eof, {1,1}, [return,text]), L).

normalize_toks(Toks) ->
    [normalize_tok(I) || I <- Toks].

normalize_tok(Tok) ->
    %% this is the portable way ...
    Type = erl_scan:category(Tok),
    Line = erl_scan:line(Tok),
    Col = erl_scan:column(Tok),
    Txt = erl_scan:text(Tok),
    Val  = {Type,{Line,Col},Txt},
    %% io:format("here:X=~p ~p~n",[Tok,Val]),
    Val.


%% analse the result of dodge_file

analyse(Forms) ->
    Calls = calls(Forms),
    Anchors = compute_anchors(Forms),
    Imports = [{{F,A},Mod} ||
		  {attribute,_,import,{Mod,L}} <- Forms, {F,A} <- L],
    D = dict:from_list(Imports),
    Patches = [{Loc, resolve(X, D)} || {Loc, X} <- Calls],
    {Anchors, Patches}.

%% An anchor is something that is placed at the start of every form
%% The anchor is named after the first item in the form
%% compute_anchors(Forms) -> [{{Line,Col}, anchor()}]
%%   {Line,Col} is the line and column of where the
%%   form starts - this is not the same as the first token in 
%%   the form since we might have skipped comments and white space
%%   at the start of the form.
%%   anchor() is a term decscribing the anchor
%%   anchor(() = {func,Name,Aritry} (for functions)
%%             | 
%%             | {Type,{Line,Col}} anythis else    

compute_anchors(Forms) ->
    A1 = [anchor0(I) || I <- Forms],
    merge_specs(A1).

%% If a specification is immediately followed by
%% a function when we promote the function anchor to point
%% at the specification.
%% We change the second tag to func2 - because we still want a
%% tag for every block 

merge_specs([{_Ln1,{specification,F,A}}=H,{Ln2, {func,F,A}}|T]) ->
    [H,{Ln2,{func1,F,A}}|merge_specs(T)];
merge_specs([H|T]) ->
    [H|merge_specs(T)];
merge_specs([]) ->
    [].

anchor0(I) ->
    case  anchor(I) of
	{{Line,Col,_,_}, Val} ->
	    {{Line,Col}, Val};
	{{_,_}, _} = X ->
	    X
    end.

anchor({function, Ln, F, A, _}) -> {Ln, {func, F, A}};
anchor({attribute,Ln,'spec', {{F,A},_}}) ->
    {Ln, {specification,F,A}};
anchor({attribute,Ln,module, M}) ->
    {Ln, {module,M}};
anchor({attribute,Ln,Type,_}) -> {Ln, {Type, Ln}};
anchor({eof,Ln}) -> {Ln, eof};
anchor({error,{Ln,_,_}}) -> 
    %% Ln is in a different format in errors (sigh)
    {Line, Col} = Ln,
    Ln1 = {Line,Col,0,""},
    {Ln1, {error, Ln}};
anchor({tree,attribute,{attr,{_,_,_,Type}=Ln,_,_},_}) -> 
    {Ln, {attribute,Type,Ln}};
anchor({tree,attribute,_,
	{attribute, {atom,Ln,Type}, _}}) ->
    {Ln, {attribute,Type,Ln}};
anchor({tree,attribute,
	{attr,Ln,[],none},
	_}=X) ->
    io:format("FIX ME this is a bug????:~p~n",[X]),
    {Ln, {other, Ln}};
anchor(X) ->
    %% this is some syntactic form that I don't know
    %% about yet ...
    io:format("FIX ME this is a bug????:~p~n",[X]),
    exit(1).
    
resolve({F,A}=Tup, D) ->
    case dict:find({F,A}, D) of
	{ok, Mod} ->
	    {remote,Mod,F,A};
	error ->
	    case erlang:is_builtin(erlang, F, A) of
		true  -> {bif, {F,A}};
		false -> {local,Tup}
	    end
    end;
resolve({erlang,F,A}, _) ->
    {bif,{F,A}};
resolve({anchor,_,_}=A, _) ->
    A;
resolve(X, _D) ->
    {remote, X}.

calls(X) -> lists:reverse(calls(X, [])).

calls({call,_,{atom,Ln,Func},Args}, L) ->
    calls(Args, [{normalise(Ln),{Func,length(Args)}}|L]);
calls({call,_,{remote,_,{atom,Ln1,Mod},{atom,_Ln2,Func}}, Args}, L) ->
    calls(Args, [{normalise(Ln1),{Mod,Func,length(Args)}}|L]);
calls(T, L) when is_tuple(T) ->
    calls(tuple_to_list(T), L);
calls([], L) ->
    L;
calls(T, L) when is_list(T) ->
    lists:foldl(fun calls/2, L, T);
calls(_, L) ->
    L.

normalise({_Line,_Col}=X) ->
    X;
normalise({Line,Col,_Len,_Text}) ->
    {Line, Col}.


prelude(L) ->
    ["<html>\n"
     "<head>\n"
     "</head>\n"
     "<body>\n"
     "<ul><pre>\n",L,"\n</pre></ul></body>"].


final({Tag, Toks}) ->
    {Tag, {taggedToks, final1(Tag, Toks)}}.

final1({Tag,_,_}, Toks) when Tag =:= func; Tag =:= func1 ->
    %% io:format("fix_remote:~p~n",[Toks]),
    fix_remote(Toks);
final1({export,_}, Toks) ->
    fix_exports(Toks);
final1({import,_}, Toks) ->
    fix_imports(Toks);
final1(_, Toks) ->
    %% io:format("final:~p~n",[X]),
    Toks.


fix_imports(Toks) ->
    %% io:format("fix imports:~p~n",[Toks]),
    Mod = find_imported_module(Toks),
    %% io:format("Mod =~p~n",[Mod]),
    fix_imports(Toks, Mod).

fix_imports([{atom,A},{terminal,"/"},{integer,N}|T], Mod) ->
    [{remote, Mod,A,list_to_integer(N),A++"/"++N}|
     fix_imports(T, Mod)];
fix_imports([H|T], Mod) ->
    [H|fix_imports(T, Mod)];
fix_imports([], _) ->
    [].

%% skip to the atom import, then take the first atom after import
find_imported_module([{atom,"import"}|T]) -> find_imported_module1(T);
find_imported_module([_|T])               -> find_imported_module(T).

find_imported_module1([{atom,M}|_]) -> list_to_atom(M);
find_imported_module1([_|T])        -> find_imported_module1(T).

%% won't work if there is white space between the symbols
%% fix later

fix_exports([{atom,A},{terminal,"/"},{integer,N}|T]) ->
    [{local,A,list_to_integer(N),A++"/"++N}|fix_exports(T)];
fix_exports([H|T]) ->
    [H|fix_exports(T)];
fix_exports([]) ->
    [].

%% fix_remote merges Mod : Func into a single string
%%  the problem is that
%%  we only tag the first atom in a remote call mod:func(...)
%%  mod is tagged as remote - but we want to
%%  extend the tagging to include the entire mod:func
%%  call ...

fix_remote([{remote,M,F,A,Str},{terminal,":"},{atom,Str1}|T]) ->
    [{remote,M,F,A,Str ++ ":" ++ Str1}|fix_remote(T)];
fix_remote([{remote,M,F,A,Str},{white_space,S1},{terminal,":"},{atom,Str1}|T]) ->
    [{remote,M,F,A,Str ++ S1 ++ ":" ++ Str1}|fix_remote(T)];
fix_remote([{remote,M,F,A,Str},{white_space,S1},{terminal,":"},{white_space,S2},{atom,Str1}|T]) ->
    [{remote,M,F,A,Str ++ S1 ++ ":" ++ S2 ++ Str1}|fix_remote(T)];
fix_remote([{remote,M,F,A,Str},{terminal,":"},{white_space,S2},{atom,Str1}|T]) ->
    [{remote,M,F,A,Str ++ ":" ++ S2 ++ Str1}|fix_remote(T)];
fix_remote([H|T]) ->
    [H|fix_remote(T)];
fix_remote([]) ->
    [].

-spec is_keyword(atom()) -> boolean().

is_keyword('after' ) -> true;
is_keyword('and') -> true;
is_keyword('andalso' ) -> true;
is_keyword('band' ) -> true;
is_keyword('begin' ) -> true;
is_keyword('bnot' ) -> true;
is_keyword('bor' ) -> true;
is_keyword('bsl' ) -> true;
is_keyword('bsr' ) -> true;
is_keyword('bxor' ) -> true;
is_keyword('case' ) -> true;
is_keyword('catch' ) -> true;
is_keyword('cond') -> true;
is_keyword('div' ) -> true;
is_keyword('end' ) -> true;
is_keyword('fun' ) -> true;
is_keyword('if' ) -> true;
is_keyword('not') -> true;
is_keyword('of' ) -> true;
is_keyword('or' ) -> true;
is_keyword('orelse' ) -> true;
is_keyword('receive' ) -> true;
is_keyword('rem' ) -> true;
is_keyword('spec') -> true;
is_keyword('try' ) -> true;
is_keyword('when') -> true;
is_keyword('xor') -> true;
is_keyword(_) -> false.

is_terminal('!')  -> true;
is_terminal('#')  -> true;
is_terminal('(')  -> true;
is_terminal(')')  -> true;
is_terminal('*')  -> true;
is_terminal('+')  -> true;
is_terminal('++') -> true;
is_terminal(',')  -> true;
is_terminal('-')  -> true;
is_terminal('--') -> true;
is_terminal('->') -> true;
is_terminal('.')  -> true;
is_terminal('/')  -> true;
is_terminal('/=') -> true;
is_terminal(':')  -> true;
is_terminal(':-') -> true;
is_terminal('::') -> true;
is_terminal(';')  -> true;
is_terminal('<')  -> true;
is_terminal('<-') -> true;
is_terminal('<<') -> true;
is_terminal('<=') -> true;
is_terminal('=')  -> true;
is_terminal('=/=') -> true;
is_terminal('=:=') -> true;
is_terminal('=<') -> true;
is_terminal('==') -> true;
is_terminal('>')  -> true;
is_terminal('>=') -> true;
is_terminal('>>') -> true;
is_terminal('?')  -> true;
is_terminal('[')  -> true;
is_terminal(']')  -> true;
is_terminal('{')  -> true;
is_terminal('|')  -> true;
is_terminal('||') -> true;
is_terminal('}')  -> true;
is_terminal(_) -> false.
