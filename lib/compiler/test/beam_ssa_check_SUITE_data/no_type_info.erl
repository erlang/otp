%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% This module tests functions which have previously crashed the
%% compiler when the `no_type_opt` option was used.
%%

-module(no_type_info).
-export([bug/0]).

-compile(no_type_opt).

bug() ->
%ssa% () when post_ssa_opt ->
%ssa% X = bif:hd(L) { unique => [L] },
%ssa% _ = succeeded:body(X) { aliased => [X] }.
    begin
        + <<42 ||
              $s <-
                  try
                      something
                  catch
                      error:false ->
                          []
                  end
          >>
    end:(hd(not girl))(
      try home of
          _ when 34 ->
              8
      catch
          _:_ ->
              whatever
      after
          ok
      end).
