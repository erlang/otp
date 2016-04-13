%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Utility module for megaco_config 
%%----------------------------------------------------------------------
%% 

-module(megaco_config_misc).

%% Application internal exports
-export([
         verify_bool/1, 

	 verify_int/1, verify_int/2, verify_int/3, 
	 verify_strict_int/1, verify_strict_int/2, verify_strict_int/3, 

	 verify_uint/1, verify_uint/2, 
	 verify_strict_uint/1, verify_strict_uint/2
        ]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

verify_bool(true)  -> true;
verify_bool(false) -> true;
verify_bool(_)     -> false.


%% verify_int(Val) -> boolean()
verify_int(infinity) -> true;
verify_int(Val)      -> verify_strict_int(Val).

%% verify_int(Val, Max) -> boolean()
verify_int(Int, infinity) ->
    verify_int(Int);
verify_int(infinity, _Max) ->
    true;
verify_int(Int, Max) ->
    verify_strict_int(Int) andalso verify_strict_int(Max) andalso (Int =< Max).

%% verify_int(Val, Min, Max) -> boolean()
verify_int(infinity, Min, infinity) ->
    verify_strict_int(Min);
verify_int(Val, Min, infinity) ->
    verify_strict_int(Val) andalso 
	verify_strict_int(Min) andalso (Val >= Min);
verify_int(Int, Min, Max) ->
    verify_strict_int(Int, Min, Max).

%% verify_strict_int(Val) -> boolean()
verify_strict_int(Int) when is_integer(Int) -> true;
verify_strict_int(_)                        -> false.

%% verify_strict_int(Val, Max) -> boolean()
verify_strict_int(Int, infinity) ->
    verify_strict_int(Int);
verify_strict_int(Int, Max) ->
    verify_strict_int(Int) andalso verify_strict_int(Max) andalso (Int =< Max).

%% verify_strict_int(Val, Min, Max) -> boolean()
verify_strict_int(Val, Min, Max) 
  when (is_integer(Val) andalso 
	is_integer(Min) andalso 
	is_integer(Max) andalso 
	(Val >= Min)    andalso 
	(Val =< Max)) ->
    true;
verify_strict_int(_Val, _Min, _Max) ->
    false.
    

%% verify_uint(Val) -> boolean()
verify_uint(infinity) -> true;
verify_uint(Val)      -> verify_strict_uint(Val).

%% verify_uint(Val, Max) -> boolean()
verify_uint(Int, infinity) ->
    verify_uint(Int);
verify_uint(infinity, _Max) ->
    true;
verify_uint(Int, Max) ->
    verify_strict_int(Int, 0, Max).

%% verify_strict_uint(Val) -> boolean()
verify_strict_uint(Int) when is_integer(Int) andalso (Int >= 0) -> true;
verify_strict_uint(_)                                           -> false.

%% verify_strict_uint(Val, Max) -> boolean()
verify_strict_uint(Int, infinity) ->
    verify_strict_uint(Int);
verify_strict_uint(Int, Max) ->
    verify_strict_int(Int, 0, Max).

