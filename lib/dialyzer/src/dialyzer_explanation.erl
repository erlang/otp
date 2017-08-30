%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_explanation.erl
%%% Author  : Elli Fragkaki <ellifrag@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------

-module(dialyzer_explanation).

-export([expl_loop/3]).

-include("dialyzer.hrl").

-spec expl_loop(pid(), dialyzer_codeserver:codeserver(), dialyzer_plt:plt()) ->
	no_return().

expl_loop(Parent, CServer, Plt) ->
  receive
    {Parent, warning, _Warning} ->
      send_explanation(Parent, none),
      expl_loop(Parent, CServer, Plt);
    {Parent, further, _Explanation} -> 
      Parent ! {self(), further, none},
      expl_loop(Parent, CServer, Plt);
    Other ->
      io:format("Unknown message: ~p\n", [Other]),
      expl_loop(Parent, CServer, Plt)
  end.

send_explanation(Parent, Expl) ->
  Parent ! {self(), explanation, Expl},
  ok.

