%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(verify).

-export([dir/0, dir/1]).

%% usage verify:dir()
%% or    verify:dir(Dir)
%% 
%% runs tests on all files with the extension ".t1"
%% creates an error log file verify.log in the directory where the
%% tests were run

-import(lists, [reverse/1, foldl/3, map/2]).

dir() ->
    dir(".").

dir(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    VFiles = collect_vers(Files, []),
	    VFiles1 = map(fun(F) -> Dir ++ "/" ++ F end, VFiles),
	    Nerrs = foldl(fun(F, Sum) -> 
				  case file(F) of
				      {file,_,had,N,errors} ->
					  Sum + N;
				      no_errors ->
					  Sum;
				      Other ->
					  Sum + 1
				  end
			  end, 0, VFiles1),
	    case Nerrs of
		0 -> no_errors;
		_ -> {dir,Dir,had,Nerrs,errors}
	    end;
	_ ->
	    {error, cannot,list_dir, Dir}
    end.

collect_vers([H|T], L) ->
    case reverse(H) of
	[$1,$t,$.|T1] -> collect_vers(T, [reverse(T1)|L]);
	_             -> collect_vers(T, L)
    end;
collect_vers([], L) ->
    L.

file(File) ->
    case file:open(File ++ ".t1", read) of
	{ok, S} ->
	    io:format("Verifying: ~s\n", [File]),
	    ErrFile = File ++ ".errs",
	    {ok, E} = file:open(ErrFile, write),
	    Bind0 = erl_eval:new_bindings(),
	    NErrs = do(S, {E, File, Bind0, 0}, 1),
	    file:close(S),
	    file:close(E),
	    case NErrs of
		0 ->
		    file:delete(ErrFile),
		    no_errors;
		_ ->
		    {file,File,had,NErrs,errors}
	    end;
	_ ->
	    error_in_opening_file
    end.

do(S, Env, Line) ->
    R = io:scan_erl_exprs(S, '', Line),
    do1(R, S, Env).

do1({eof,_}, _, {_,_,_,NErrs}) ->
    NErrs;
do1({ok,Toks,Next}, S, Env0) ->
    E1 = handle_toks(Toks, Next, Env0),
    do(S, E1, Next);
do1({error, {Line,Mod,Args}, Next}, S, E) ->
    io:format("*** ~w ~p~n", [Line,Mod:format_error(Args)]),
    E1 = add_error(E),
    do(S, E1, Next).

add_error({Stream, File, Bindings, N}) -> {Stream, File, Bindings, N+1}.

handle_toks(Toks, Line, Env0) ->
    %% io:format("Toks:~p\n", [Toks]).
    case erl_parse:parse_exprs(Toks) of
	{ok, Exprs} ->
	    %% io:format("Got:~p\n", [Exprs]),
	    eval(Exprs, Line, Env0);
	{error, {LineNo, Mod, What}} ->
	    Str = apply(Mod, format_error, [What]),
	    io:format("*** Line:~w  ***~s\n", [LineNo, Str]),
	    add_error(Env0);
	Parse_error ->
	    io:format("Parse Error:~p\n",[Parse_error]),
 	    add_error(Env0)
    end.

forget([{var,_,Name}], B0) -> erl_eval:del_binding(Name, B0);
forget([], _)              -> erl_eval:new_bindings().

eval([{call,_,{atom,_,f}, Args}], _, {Stream, Bind0, Errs}) ->
    Bind1 = forget(Args, Bind0),
    {Stream, Bind1, Errs};
eval(Exprs, Line, {Stream, File, Bind0, NErrs}) ->
    %% io:format("Bindings >> ~p\n", [Bind0]),
    %% io:format("Exprs >> ~p\n", [Exprs]),
    case catch erl_eval:exprs(Exprs, Bind0) of
	{'EXIT', Reason} ->
	    out_both(Stream, "----------------------------------~n", []),
	    out_both(Stream, "File:~s Error in:~s~n", [File, pp(Exprs)]),
	    print_bindings(Stream, Exprs, Bind0),
	    print_lhs(Stream, Exprs),
	    out_both(Stream, '*** Rhs evaluated to:~p~n',[rhs(Exprs, Bind0)]),
	    {Stream, File, Bind0, NErrs+1};
	{value, _, Bind1} ->
	    {Stream, File, Bind1, NErrs}
    end.

pp([H])   -> erl_pp:expr(H);
pp([H|T]) -> [erl_pp:expr(H),$,|pp(T)];
pp([])    -> [].

print_bindings(E, Form, Bindings) ->
    case varsin(Form) of
	[] ->
	    true;
	Vars ->
	    print_vars(E, Vars, Bindings)
    end.

print_vars(E, [Var|T], Bindings) ->
    case erl_eval:binding(Var, Bindings) of
	{value, Val} ->
	    out_both(E, '~s = ~p\n',[Var, Val]);
	unbound ->
	    out_both(E, '~s *is unbound*\n', [Var])
    end,
    print_vars(E, T, Bindings);
print_vars(_, [], _) ->
    true.


out_both(E, Format, Data) ->
    io:format(Format, Data),
    io:format(E, Format, Data).

print_lhs(E, [{match, _, Lhs, Rhs}]) ->
    %% io:format(">>>> here:~w\n",[Lhs]),
    out_both(E, '*** Lhs was:~s\n',[erl_pp:expr(Lhs)]);
print_lhs(E, _) ->
    out_both(E, '** UNDEFINED **', []).


rhs([{match, _, Lhs, Rhs}], Bindings) ->
    case catch erl_eval:exprs([Rhs], Bindings) of
	{value, Val, _} -> Val;
	Other           ->  undefined()
    end;
rhs(_, _) ->
    undefined().

varsin(X) -> varsin(X, []).

varsin({var,_,'_'}, L) ->
    L;
varsin({var,_,V}, L) ->
    case lists:member(V, L) of
	true  -> L;
	false -> [V|L]
    end;
varsin([H|T], L) ->
    varsin(T, varsin(H, L));
varsin(T, L) when tuple(T) ->
    varsin(tuple_to_list(T), L);
varsin(_, L) ->
    L.
    
undefined() ->
    '** UNDEFINED **'.
