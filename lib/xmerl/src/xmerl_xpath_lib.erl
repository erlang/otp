%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

