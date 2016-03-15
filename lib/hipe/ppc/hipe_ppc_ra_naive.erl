%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(hipe_ppc_ra_naive).
-export([ra/3]).

-include("hipe_ppc.hrl").

ra(Defun, _Coloring_fp, _Options) ->	% -> {Defun, Coloring}
  {NewDefun,_DidSpill} =
    hipe_ppc_ra_postconditions:check_and_rewrite2(Defun, [], 'naive'),
  {NewDefun, []}.
