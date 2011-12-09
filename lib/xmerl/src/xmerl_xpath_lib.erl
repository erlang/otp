%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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

%%
-module(xmerl_xpath_lib).

-include("xmerl.hrl").

-export([eval/3]).

-define(string(X), #xmlObj{type = string,
			   value = X}).
-define(number(X), #xmlObj{type = number,
			   value = X}).


eval(primary_expr,PrimExpr,C) ->
    primary_expr(PrimExpr, C);
eval(predicate,Pred,C) ->
    xmerl_xpath_pred:eval(Pred,C).

primary_expr({number, N}, _C) ->
    ?number(N);
primary_expr({literal, S}, _C) ->
    ?string(S);
primary_expr({function_call, F, Args}, C) ->
    case xmerl_xpath_pred:core_function(F) of
	{true, F1} ->
	    xmerl_xpath_pred:F1(C, Args);
	true ->
	    xmerl_xpath_pred:F(C, Args);
	false ->
	    %% here, we should look up the function in the context provided 
	    %% by the caller, but we haven't figured this out yet.
	    exit({not_a_core_function, F})
    end;
primary_expr(PrimExpr, _C) ->
    exit({primary_expression, {not_implemented, PrimExpr}}).

