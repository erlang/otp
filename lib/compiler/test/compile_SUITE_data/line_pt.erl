%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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

-module(line_pt).
-export([parse_transform/2, parse_transform_info/0]).

parse_transform_info() ->
    #{error_location => line}.

parse_transform(Forms,_Options) ->
    lists:map(
      fun
          ({eof,Location}) ->
              Anno = erl_anno:new(Location),
              false = erl_anno:column(Anno) =/= undefined,
              {eof,Location};
          (Form) ->
              erl_parse:map_anno(
                fun(Anno) ->
                        false = erl_anno:column(Anno) =/= undefined,
                        Anno
                end, Form)
      end, Forms).
