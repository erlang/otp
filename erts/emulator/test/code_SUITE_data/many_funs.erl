%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

-module(many_funs).

-export([make_fun/1,many_funs/0]).

make_fun(A) ->
    fun(X) -> A + X end.

%% Force dynamic allocation of lambda table.
many_funs() ->
    [fun(_) -> 1 end,
     fun(_) -> 2 end,
     fun(_) -> 3 end,
     fun(_) -> 4 end,
     fun(_) -> 5 end,
     fun(_) -> 6 end,
     fun(_) -> 7 end,
     fun(_) -> 8 end,
     fun(_) -> 9 end,
     fun(_) -> 10 end,
     fun(_) -> 11 end,
     fun(_) -> 12 end,
     fun(_) -> 13 end,
     fun(_) -> 14 end,
     fun(_) -> 15 end,
     fun(_) -> 16 end].
	     

	    
