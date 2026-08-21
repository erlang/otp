%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Stavros Aronis 2011. All Rights Reserved.
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
%%-----------------------------------------------------------------------
%% Program that gave erroneous warnings due to missing information about
%% the {encoding, latin1 | unicode | utf8 | ...} option of file:open/3.
%%-----------------------------------------------------------------------
-module(file_open_encoding).

-compile([{nowarn_possibly_unsafe_function, {erlang, binary_to_atom, 2}}]).

-export([parse/1]).

-spec parse(string()) -> proplists:proplist().
parse(FileName) ->
  {ok, IoDevice} = file:open(FileName, [read, binary, {encoding, utf8}]),
  do_parse(IoDevice, []).

do_parse(IoDevice, ResultSoFar) ->
  case io:get_line(IoDevice, "") of
    eof ->
      file:close(IoDevice),
      ResultSoFar;
    <<"--"/utf8, _Comment/binary>> ->
      do_parse(IoDevice, ResultSoFar);
    Line ->
      [Key, Value] = binary:split(Line, [<<": ">>, <<"\n">>], [global, trim]),
      do_parse(IoDevice, [{binary_to_atom(Key, utf8), Value} | ResultSoFar])
 end.
