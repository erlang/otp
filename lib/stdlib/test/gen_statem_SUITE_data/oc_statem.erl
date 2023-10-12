%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2022. All Rights Reserved.
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
-module(oc_statem).

-behaviour(gen_statem).

%% API
-export([start/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4]).

start(Opts) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [], Opts).

init([]) ->
    %% Supervise state machine parent i.e the test case, and if it dies
    %% (fails due to some reason), kill the state machine,
    %% just to not leak resources (process, name, ETS table, etc...)
    %%
    Parent = gen:get_parent(),
    Statem = self(),
    _Supervisor =
        spawn(
          fun () ->
                  StatemRef = monitor(process, Statem),
                  ParentRef = monitor(process, Parent),
                  receive
                      {'DOWN', StatemRef, _, _, Reason} ->
                          exit(Reason);
                      {'DOWN', ParentRef, _, _, _} ->
                          exit(Statem, kill)
                  end
          end),
    {ok, state_2, [undefined]}.

callback_mode() ->
    [handle_event_function, state_enter].

handle_event(enter, _OldState, state_2, [undefined|Data]) ->
    {keep_state, [enter|Data]};
handle_event({call,From}, get_data, state_2, Data) ->
    {keep_state_and_data, [{reply,From,hd(Data)}]};
handle_event(
  {call,From}, {push_callback_module,_NewModule} = Action,
  state_2, [enter|Data]) ->
    {next_state, state_1, [undefined|Data],
     [Action,
      {reply,From,ok}]};
handle_event(
  {call,From}, pop_callback_module = Action,
  state_2, [enter|_Data]) ->
    {keep_state_and_data,
     [Action,
      {reply,From,ok}]}.
