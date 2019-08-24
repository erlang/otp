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

-module(run).

-compile(export_all).

host() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(H, []) -> [].

start() ->
    Result = do_it(),

    %% Do GCs and node_and_dist_references
    %% in an attempt to crash the VM (without OTP-13076 fix)
    lists:foreach(fun(P) -> erlang:garbage_collect(P) end,
		  processes()),
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:get_internal_state(node_and_dist_references),

    io:format("~w~n", [Result]),

    if
	Result ->
	    init:stop();
	true ->
	    %% Make sure that the io:format/2 output is really written
	    %% (especially on Windows).
	    erlang:yield(),
	    init:stop()
    end.


do_it() ->
    {ok, _} = net_kernel:start([fideridum,shortnames]),
    {ok, Node} = slave:start(host(), heppel),
    P = spawn(Node, net_kernel, stop, []),
    B1 = term_to_binary(P),
    N1 = node(P),
    ok = net_kernel:stop(),
    N2 = node(P),

    %% OTP-13076
    %% Restart distribution with same node name as previous remote node
    %% Repeat to wrap around creation
    Result = lists:foldl(fun(_, Acc) ->
				 timer:sleep(2),  % give net_kernel:stop() time to take effect :-(
				 {ok, _} = net_kernel:start([heppel,shortnames]),
				 N3 = node(P),
				 ok = net_kernel:stop(),
				 N4 = node(P),
				 Acc and (N3 =:= N1) and (N4 =:= N1)
			 end,
			 (N2 =:= N1),
			 lists:seq(1,3)),

    Result.
