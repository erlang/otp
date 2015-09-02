%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
-module(nested_tuples_in_case_expr).
-export([nested_tuples_in_case_expr/0,t/2]).

nested_tuples_in_case_expr() ->
    ok.

t(A, B) ->
    case {{element(1, A),element(2, B)},{element(2, A),element(2, B)}} of
	{Same,Same} -> ok;
	{{0,1},{up,X}} -> bar(X);
	{_,{X,_}} -> bar(X)
    end.

bar(X) -> X.

    
    
	    
