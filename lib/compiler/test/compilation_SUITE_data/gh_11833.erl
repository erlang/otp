%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(gh_11833).
-export([?MODULE/0]).
-compile({inline, [is_byte/1, is_word/1]}).

?MODULE() ->
    {false,42} = f(<<42>>),
    ok = try
             f(<<1,2>>)
         catch
             error:{badmatch,<<1,2>>} ->
                 ok
         end.
    
is_byte(<<_:8>>) -> true;
is_byte(_) -> false.

is_word(<<_:16>>) -> true;
is_word(_) -> false.

f(B) ->
    A = is_byte(B) andalso is_word(B),
    <<Tag:8>> = B,
    {A,Tag}.
