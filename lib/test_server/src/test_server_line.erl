%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
-module(test_server_line).

%% User interface
-export([get_lines/0]).
-export([clear/0]).

%% Parse transform functions
-export([parse_transform/2]).
-export(['$test_server_line'/3]).
-export(['$test_server_lineQ'/3]).
-export([trace_line/3]).

-define(TEST_SERVER_LINE_SIZE, 10).
%-define(STORAGE_FUNCTION, '$test_server_line').
-define(STORAGE_FUNCTION, '$test_server_lineQ').

-include("test_server.hrl").

-record(vars, {module,                      % atom() Module name
	       function,                    % atom() Function name
	       arity,                       % int()  Function arity
	       lines,                       % [int()]  seen lines
	       is_guard=false,              % boolean()
	       no_lines=[],                 % [{atom(),integer()}] 
					    %    Functions to exclude
	       line_trace=false
	      }).




%% Process dictionary littering variant
%%

'$test_server_line'(Mod, Func, Line) ->
    {Prev,Next} = 
	case get('$test_server_line') of
	    I when is_integer(I) ->
		if  1 =< I, I < ?TEST_SERVER_LINE_SIZE -> {I,I+1};
		    true -> {?TEST_SERVER_LINE_SIZE,1}
		end;
	    _ -> {?TEST_SERVER_LINE_SIZE,1}
	end,
    PrevTag = {'$test_server_line',Prev},
    case get(PrevTag) of
	{Mod,Func,_} -> put(PrevTag, {Mod,Func,Line});
	_ -> 
	    put({'$test_server_line',Next}, {Mod,Func,Line}),
	    put('$test_server_line', Next)
    end, ok.

test_server_line_get() ->
    case get('$test_server_line') of
	I when is_integer(I), 1 =< I, I =< ?TEST_SERVER_LINE_SIZE ->
	    test_server_line_get_1(?TEST_SERVER_LINE_SIZE, I, []);
	_ -> []
    end.

test_server_line_get_1(0, _I, R) ->
    R;
test_server_line_get_1(Cnt, I, R) ->
    J = if  I < ?TEST_SERVER_LINE_SIZE -> I+1;
	    true -> 1 end,
    case get({'$test_server_line',J}) of
	undefined -> 
	    %% Less than ?TEST_SERVER_LINE_SIZE number of lines stored
	    %% Start from line 1 and stop at actutual number of lines
	    case get({'$test_server_line',1}) of 
		undefined -> R; % no lines at all stored
		E -> test_server_line_get_1(I-1,1,[E|R])
	    end;
	E -> 
	    test_server_line_get_1(Cnt-1, J, [E|R])
    end.

test_server_line_clear() ->
    Is = lists:seq(1,?TEST_SERVER_LINE_SIZE),
    lists:foreach(fun (I) -> erase({'$test_server_line',I}) end, Is),
    erase('$test_server_line'),
    ok.


%% Queue variant, uses just one process dictionary entry
%%

'$test_server_lineQ'(Mod, Func, Line) ->		
    case get('$test_server_lineQ') of
	{I,Q} when is_integer(I), 1 =< I, I =< ?TEST_SERVER_LINE_SIZE -> 
	    case queue:head(Q) of
		{Mod,Func,_} ->
		    %% Replace queue head
		    put('$test_server_lineQ',
			{I,queue:cons({Mod,Func,Line}, queue:tail(Q))});
		_ when I < ?TEST_SERVER_LINE_SIZE ->
		    put('$test_server_lineQ',
			{I+1,queue:cons({Mod,Func,Line}, Q)});
		_ -> 
		    %% Waste last in queue
		    put('$test_server_lineQ',
			{I,queue:cons({Mod,Func,Line}, queue:lait(Q))})
	    end;
	_ -> 
	    Q = queue:new(),
	    put('$test_server_lineQ', {1,queue:cons({Mod,Func,Line}, Q)})
    end, ok.

%test_server_lineQ_get() ->
%    case get('$test_server_lineQ') of
%	{I,Q} when integer(I), 1 =< I, I =< ?TEST_SERVER_LINE_SIZE -> 
%	    queue:to_list(Q);
%	_ -> []
%    end.

test_server_lineQ_clear() ->
    erase('$test_server_lineQ'),
    ok.


%% Get line - check if queue or dictionary is used, then get the lines
%%

get_lines() ->
    case get('$test_server_lineQ') of
	{I,Q} when is_integer(I), 1 =< I, I =< ?TEST_SERVER_LINE_SIZE -> 
	    queue:to_list(Q);
	_ -> 
	    test_server_line_get()
    end.
	
%% Clear all dictionary entries
%%
clear() ->
    test_server_line_clear(),
    test_server_lineQ_clear().


trace_line(Mod,Func,Line) ->
    io:format(lists:concat([Mod,":",Func,",",integer_to_list(Line),": ~p"]),
	      [erlang:now()]).


%%%=================================================================
%%%========= ****   PARSE TRANSFORM    **** ========================
%%%=================================================================
parse_transform(Forms, _Options) ->
    transform(Forms, _Options).

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

transform(Forms, _Options)->
    Vars0 = #vars{},
    {ok, MungedForms, _Vars} = transform(Forms, [], Vars0),
    MungedForms.
    

transform([Form|Forms], MungedForms, Vars) ->
    case munge(Form, Vars) of
	ignore ->
	    transform(Forms, MungedForms, Vars);
	{MungedForm, Vars2} ->
	    transform(Forms, [MungedForm|MungedForms], Vars2)
    end;
transform([], MungedForms, Vars) ->
    {ok, lists:reverse(MungedForms), Vars}.

%% This code traverses the abstract code, stored as the abstract_code
%% chunk in the BEAM file, as described in absform(3) for Erlang/OTP R8B
%% (Vsn=abstract_v2).
%% The abstract format after preprocessing differs slightly from the abstract
%% format given eg using epp:parse_form, this has been noted in comments.
munge(Form={attribute,_,module,Module}, Vars) ->
    Vars2 = Vars#vars{module=Module},
    {Form, Vars2};

munge(Form={attribute,_,no_lines,Funcs}, Vars) ->
    Vars2 = Vars#vars{no_lines=Funcs},
    {Form, Vars2};

munge(Form={attribute,_,line_trace,_}, Vars) ->
    Vars2 = Vars#vars{line_trace=true},
    {Form, Vars2};

munge({function,0,module_info,_Arity,_Clauses}, _Vars) ->
    ignore; % module_info will be added again when the forms are recompiled
munge(Form = {function,Line,Function,Arity,Clauses}, Vars) ->
    case lists:member({Function,Arity},Vars#vars.no_lines) of
	true ->
	    %% Line numbers in this function shall not be stored
	    {Form,Vars};
	false ->
	    Vars2 = Vars#vars{function=Function,
			      arity=Arity,
			      lines=[]},
	    {MungedClauses, Vars3} = munge_clauses(Clauses, Vars2, []),
	    {{function,Line,Function,Arity,MungedClauses}, Vars3}
    end;
munge(Form, Vars) -> % attributes
    {Form, Vars}.

munge_clauses([{clause,Line,Pattern,Guards,Body}|Clauses], Vars, MClauses) ->
    {MungedGuards, _Vars} = munge_exprs(Guards, Vars#vars{is_guard=true},[]),
    {MungedBody, Vars2} = munge_body(Body, Vars, []),
    munge_clauses(Clauses, Vars2,
		  [{clause,Line,Pattern,MungedGuards,MungedBody}|
		   MClauses]);
munge_clauses([], Vars, MungedClauses) -> 
    {lists:reverse(MungedClauses), Vars}.

munge_body([Expr|Body], Vars, MungedBody) ->
    %% Here is the place to add a call to storage function!
    Line = element(2, Expr),
    Lines = Vars#vars.lines,
    case lists:member(Line,Lines) of
	true -> % already a bump at this line!
	    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
	    munge_body(Body, Vars2, [MungedExpr|MungedBody]);
	false ->
	    Bump = {call, 0, {remote,0,
			      {atom,0,?MODULE},
			      {atom,0,?STORAGE_FUNCTION}},
		    [{atom,0,Vars#vars.module},
		     {atom, 0, Vars#vars.function},
		     {integer, 0, Line}]},
	    Lines2 = [Line|Lines],
	    
	    {MungedExpr, Vars2} = munge_expr(Expr, Vars#vars{lines=Lines2}),
	    MungedBody2 = 
		if Vars#vars.line_trace ->
			LineTrace = {call, 0, {remote,0,
					       {atom,0,?MODULE},
					       {atom,0,trace_line}},
				     [{atom,0,Vars#vars.module},
				      {atom, 0, Vars#vars.function},
				      {integer, 0, Line}]},
			[MungedExpr,LineTrace,Bump|MungedBody];
		   true ->
			[MungedExpr,Bump|MungedBody]
		end,
	    munge_body(Body, Vars2, MungedBody2)
    end;
munge_body([], Vars, MungedBody) ->
    {lists:reverse(MungedBody), Vars}.

munge_expr({match,Line,ExprL,ExprR}, Vars) ->
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{match,Line,MungedExprL,MungedExprR}, Vars3};
munge_expr({tuple,Line,Exprs}, Vars) ->
    {MungedExprs, Vars2} = munge_exprs(Exprs, Vars, []),
    {{tuple,Line,MungedExprs}, Vars2};
munge_expr({record,Line,Expr,Exprs}, Vars) ->
    %% Only for Vsn=raw_abstract_v1
    {MungedExprName, Vars2} = munge_expr(Expr, Vars),
    {MungedExprFields, Vars3} = munge_exprs(Exprs, Vars2, []),
    {{record,Line,MungedExprName,MungedExprFields}, Vars3};
munge_expr({record_field,Line,ExprL,ExprR}, Vars) ->
    %% Only for Vsn=raw_abstract_v1
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{record_field,Line,MungedExprL,MungedExprR}, Vars3};
munge_expr({cons,Line,ExprH,ExprT}, Vars) ->
    {MungedExprH, Vars2} = munge_expr(ExprH, Vars),
    {MungedExprT, Vars3} = munge_expr(ExprT, Vars2),
    {{cons,Line,MungedExprH,MungedExprT}, Vars3};
munge_expr({op,Line,Op,ExprL,ExprR}, Vars) ->
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{op,Line,Op,MungedExprL,MungedExprR}, Vars3};
munge_expr({op,Line,Op,Expr}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {{op,Line,Op,MungedExpr}, Vars2};
munge_expr({'catch',Line,Expr}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {{'catch',Line,MungedExpr}, Vars2};
munge_expr({call,Line1,{remote,Line2,ExprM,ExprF},Exprs},
	   Vars) when Vars#vars.is_guard==false->
    {MungedExprM, Vars2} = munge_expr(ExprM, Vars),
    {MungedExprF, Vars3} = munge_expr(ExprF, Vars2),
    {MungedExprs, Vars4} = munge_exprs(Exprs, Vars3, []),
    {{call,Line1,{remote,Line2,MungedExprM,MungedExprF},MungedExprs}, Vars4};
munge_expr({call,Line1,{remote,_Line2,_ExprM,ExprF},Exprs},
	   Vars) when Vars#vars.is_guard==true ->
    %% Difference in abstract format after preprocessing: BIF calls in guards
    %% are translated to {remote,...} (which is not allowed as source form)
    %% NOT NECESSARY FOR Vsn=raw_abstract_v1
    munge_expr({call,Line1,ExprF,Exprs}, Vars);
munge_expr({call,Line,Expr,Exprs}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {MungedExprs, Vars3} = munge_exprs(Exprs, Vars2, []),
    {{call,Line,MungedExpr,MungedExprs}, Vars3};
munge_expr({lc,Line,Expr,LC}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {MungedLC, Vars3} = munge_lc(LC, Vars2, []),
    {{lc,Line,MungedExpr,MungedLC}, Vars3};
munge_expr({block,Line,Body}, Vars) ->
    {MungedBody, Vars2} = munge_body(Body, Vars, []),
    {{block,Line,MungedBody}, Vars2};
munge_expr({'if',Line,Clauses}, Vars) -> 
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars, []),
    {{'if',Line,MungedClauses}, Vars2};
munge_expr({'case',Line,Expr,Clauses}, Vars) ->
    {MungedExpr,Vars2} = munge_expr(Expr,Vars),
    {MungedClauses,Vars3} = munge_clauses(Clauses, Vars2, []),
    {{'case',Line,MungedExpr,MungedClauses}, Vars3};
munge_expr({'receive',Line,Clauses}, Vars) -> 
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars, []),
    {{'receive',Line,MungedClauses}, Vars2};
munge_expr({'receive',Line,Clauses,Expr,Body}, Vars) ->
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars, []),
    {MungedExpr, Vars3} = munge_expr(Expr, Vars2),
    {MungedBody, Vars4} = munge_body(Body, Vars3, []),
    {{'receive',Line,MungedClauses,MungedExpr,MungedBody}, Vars4};
munge_expr({'try',Line,Exprs,Clauses,CatchClauses,After}, Vars) ->
    {MungedExprs, Vars1} = munge_exprs(Exprs, Vars, []),
    {MungedClauses, Vars2} = munge_clauses(Clauses, Vars1, []),
    {MungedCatchClauses, Vars3} = munge_clauses(CatchClauses, Vars2, []),
    {MungedAfter, Vars4} = munge_body(After, Vars3, []),
    {{'try',Line,MungedExprs,MungedClauses,MungedCatchClauses,MungedAfter}, 
     Vars4};
%% Difference in abstract format after preprocessing: Funs get an extra
%% element Extra.
%% NOT NECESSARY FOR Vsn=raw_abstract_v1
munge_expr({'fun',Line,{function,Name,Arity},_Extra}, Vars) ->
    {{'fun',Line,{function,Name,Arity}}, Vars};
munge_expr({'fun',Line,{clauses,Clauses},_Extra}, Vars) ->
    {MungedClauses,Vars2}=munge_clauses(Clauses, Vars, []),
    {{'fun',Line,{clauses,MungedClauses}}, Vars2};
munge_expr({'fun',Line,{clauses,Clauses}}, Vars) ->
    %% Only for Vsn=raw_abstract_v1
    {MungedClauses,Vars2}=munge_clauses(Clauses, Vars, []),
    {{'fun',Line,{clauses,MungedClauses}}, Vars2};
munge_expr({bc,Line,Expr,LC}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {MungedLC, Vars3} = munge_lc(LC, Vars2, []),
    {{bc,Line,MungedExpr,MungedLC}, Vars3};
munge_expr(Form, Vars) -> % var|char|integer|float|string|atom|nil|bin|eof
    {Form, Vars}.

munge_exprs([Expr|Exprs], Vars, MungedExprs) when Vars#vars.is_guard==true,
						  is_list(Expr) ->
    {MungedExpr, _Vars} = munge_exprs(Expr, Vars, []),
    munge_exprs(Exprs, Vars, [MungedExpr|MungedExprs]);
munge_exprs([Expr|Exprs], Vars, MungedExprs) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_exprs(Exprs, Vars2, [MungedExpr|MungedExprs]);
munge_exprs([], Vars, MungedExprs) ->
    {lists:reverse(MungedExprs), Vars}.

munge_lc([{generate,Line,Pattern,Expr}|LC], Vars, MungedLC) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_lc(LC, Vars2, [{generate,Line,Pattern,MungedExpr}|MungedLC]);
munge_lc([{b_generate,Line,Pattern,Expr}|LC], Vars, MungedLC) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_lc(LC, Vars2, [{b_generate,Line,Pattern,MungedExpr}|MungedLC]);
munge_lc([Expr|LC], Vars, MungedLC) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_lc(LC, Vars2, [MungedExpr|MungedLC]);
munge_lc([], Vars, MungedLC) ->
    {lists:reverse(MungedLC), Vars}.










