%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_node_sets).

-export([new/0,
	 spilled/1,
	 colored/1,
	 add_spilled/2,
	 add_colored/2
	]).

-record(node_sets, 
	{spilled,    % Nodes marked for spilling
	 colored     % Nodes successfully colored
	}).

spilled(Node_sets) -> Node_sets#node_sets.spilled.
colored(Node_sets) -> Node_sets#node_sets.colored.
    
set_spilled(Spilled, Node_sets) -> Node_sets#node_sets{spilled = Spilled}.
set_colored(Colored, Node_sets) -> Node_sets#node_sets{colored = Colored}.

new() ->
  #node_sets{spilled = [], colored = []}.

add_spilled(Node, Node_sets) ->
  set_spilled([Node | spilled(Node_sets)], Node_sets).

add_colored(Node, Node_sets) ->
  set_colored([Node | colored(Node_sets)], Node_sets).
