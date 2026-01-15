%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2024 Jonatan Kłosko <jonatanklosko@gmail.com>
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
-module(source_annotations).

-export([main/1,
         bar/0,
         no_slogan/1,
         spec_slogan/1,
         spec_slogan/2,
         no_doc_slogan/1,
         spec_no_doc_slogan/1,
         spec_multiclause_slogan_ignored/1,
         connect/2
        ]).

-spec main(X :: integer()) -> ok.
main(_X) ->
    ok.

bar() ->
    ok.
