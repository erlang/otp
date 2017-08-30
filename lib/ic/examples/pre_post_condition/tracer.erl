%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: tracer.erl
%% 
%% Description:
%%    This file contains an example of pre and post conditions for 
%%    the corba backend.
%%
%%-----------------------------------------------------------------
-module(tracer).
-include("m.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([pre/3, post/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
pre(M, F, [State, I]) when is_integer(I) ->
    io:format("Precond called in process ~p: ~s:~s() ~p\n", [self(), M, F,  [State, I]]),
    ok;
pre(_M, _F, _A) -> %% Just an silly example to get an exception case
    corba:raise(#'m_NotAnInteger'{}).

post(M, F, A, R) ->
    io:format("Postcond called in process ~p: ~s:~s() ~p ~p\n", [self(), M, F, A, R]),
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
