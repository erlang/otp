%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%% Purpose : Test OTP-2163

-module(otp2163).


-export([apply_test/0, list_to_atom_test/0, error/1]).
-export([test/0]).

apply_test() ->
    M = {},
    apply(M,dummy,[]).

list_to_atom_test() ->
    list_to_atom(id({1,2})).

id(I) -> I.

%% OTP-4845 OTP 4859
-record(sune, {a,sd,g,s}).
-record(error, {a,sd,g,s}).

test() ->
    sune = error(#sune{}),
    {false,false} = error(false),
    {true,true}  = error(true),
    error(#error{}).

error(X) ->
    if
	is_record(X, sune) ->
	    sune;
	X ->
	    {true, X};
	not X ->
	    {false, X};
	not is_record(X, error) ->
	    error;
	true ->
	    ok
    end.
