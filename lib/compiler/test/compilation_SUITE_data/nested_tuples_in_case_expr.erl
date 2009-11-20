%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

    
    
	    
