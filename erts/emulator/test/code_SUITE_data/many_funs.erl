%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
	     

	    
