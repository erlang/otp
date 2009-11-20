%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
