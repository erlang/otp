%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(float_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 pending/1,bif_calls/1,math_functions/1,mixed_float_and_int/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [pending, bif_calls, math_functions,
     mixed_float_and_int].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Thanks to Tobias Lindahl <tobias.lindahl@it.uu.se>
%% Shows the effect of pending exceptions on the x86.

pending(Config) when is_list(Config) ->
    case catch float_mul(1, 1.1e300, 3.14e300) of
	{'EXIT',{badarith,_}} -> ok;
	Other -> ct:fail({expected_exception,Other})
    end,
    0.0 = float_sub(2.0).

float_sub(A)->
    catch A - 2.0.

float_mul(0, _, _)->
    ok;
float_mul(Iter, A, B) when is_float(A), is_float(B) ->
    _ = A*B,
    float_mul(Iter-1, A, B).

%% Thanks to Mikael Pettersson and Tobias Lindahl (HiPE).

bif_calls(Config) when is_list(Config) ->
    {'EXIT',{badarith,_}} = (catch bad_arith(2.0, 1.7)),
    {'EXIT',{badarith,_}} = (catch bad_arith_again(2.0, [])),
    {'EXIT',{badarith,_}} = (catch bad_arith_xor(2.0, [])),
    {'EXIT',{badarith,_}} = (catch bad_arith_hd(2.0, [])),
    {'EXIT',{badarith,_}} = (catch bad_negate(2.0, 1.7)),
    ok.

bad_arith(X, Y) when is_float(X) ->
    X1 = X * 1.7e+308,
    X2 = X1 + 1.0,
    Y1 = Y * 2,					%Calls erts_mixed_times/2.
						%(A BIF call.)
    {X2, Y1}.

bad_arith_xor(X, Y) when is_float(X) ->
    X1 = X * 1.7e+308,
    Y1 = Y xor true,				%A failing BIF call.
    {X1 + 1.0, Y1}.

bad_arith_hd(X, Y) when is_float(X) ->
    X1 = X * 1.7e+308,
    Y1 = hd(Y),					%A failing BIF call.
    {X1 + 1.0, Y1}.

bad_arith_again(X, Y) when is_float(X) ->
    X1 = X * 1.7e+308,
    Y1 = element(1, Y),				%A failing BIF call.
    {X1 + 1.0, Y1}.

bad_negate(X, Y) when is_float(X) ->
    X1 = X * 1.7e+308,
    X2 = X1 + 1.0,
    Y1 = -Y,					%BIF call.
    {X2, Y1}.

%% Some math functions are not implemented on all platforms.
-define(OPTIONAL(Expected, Expr),
	try
	    Expected = Expr
	catch
	    error:undef -> ok
	end).

math_functions(Config) when is_list(Config) ->
    %% Mostly silly coverage.
    0.0 = math:tan(0),
    0.0 = math:atan2(0, 1),
    0.0 = math:sinh(0),
    1.0 = math:cosh(0),
    0.0 = math:tanh(0),
    1.0 = math:log2(2),
    1.0 = math:log10(10),
    -1.0 = math:cos(math:pi()),
    1.0 = math:exp(0),
    1.0 = math:pow(math:pi(), 0),
    0.0 = math:log(1),
    0.0 = math:asin(0),
    0.0 = math:acos(1),
    ?OPTIONAL(0.0, math:asinh(0)),
    ?OPTIONAL(0.0, math:acosh(1)),
    ?OPTIONAL(0.0, math:atanh(0)),
    ?OPTIONAL(0.0, math:erf(0)),
    ?OPTIONAL(1.0, math:erfc(0)),

    0.0 = math:tan(id(0)),
    0.0 = math:atan2(id(0), 1),
    0.0 = math:sinh(id(0)),
    1.0 = math:cosh(id(0)),
    0.0 = math:tanh(id(0)),
    1.0 = math:log2(id(2)),
    1.0 = math:log10(id(10)),
    1.0 = math:exp(id(0)),
    0.0 = math:log(id(1)),
    0.0 = math:asin(id(0)),
    0.0 = math:acos(id(1)),
    ?OPTIONAL(0.0, math:asinh(id(0))),
    ?OPTIONAL(0.0, math:acosh(id(1))),
    ?OPTIONAL(0.0, math:atanh(id(0))),
    ?OPTIONAL(0.0, math:erf(id(0))),
    ?OPTIONAL(1.0, math:erfc(id(0))),

    %% Only for coverage (of beam_type.erl).
    {'EXIT',{undef,_}} = (catch math:fnurfla(0)),
    {'EXIT',{undef,_}} = (catch math:fnurfla(0, 0)),
    {'EXIT',{badarg,_}} = (catch float(kalle)),
    {'EXIT',{badarith,_}} = (catch name/1),
    ok.

mixed_float_and_int(Config) when is_list(Config) ->
    129.0 = pc(77, 23, 5),
    ok.

pc(Cov, NotCov, X) ->
    round(Cov/(Cov+NotCov)*100) + 42 + 2.0*X.

id(I) -> I.

