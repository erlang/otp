%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

-module(run).

-compile(export_all).

host() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(H, []) -> [].

start() ->
    net_kernel:start([fideridum,shortnames]),
    {ok, Node} = slave:start(host(), heppel),
    P = spawn(Node, a, b, []),
    B1 = term_to_binary(P),
    N1 = node(P),
    ok = net_kernel:stop(),
    N2 = node(P),
    io:format("~w~n", [N1 == N2]),
    if
	N1 == N2 -> 
	    init:stop();
	true ->
	    %% Make sure that the io:format/2 output is really written
	    %% (especially on Windows).
	    erlang:yield(),
	    init:stop()
    end.
