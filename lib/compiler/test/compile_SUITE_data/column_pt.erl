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

-module(column_pt).
-export([parse_transform/2, parse_transform_info/0, format_error/1]).

parse_transform_info() ->
    %% Will default to {error_location,column}.
    #{}.

parse_transform(Forms, Options) ->

    HasColumn =
        case proplists:get_value(error_location, Options, column) of
            column ->
                true;
            line ->
                false
        end,

    AddColumns = proplists:get_value(add_columns, Options, false),
    AddError = proplists:get_value(add_error, Options, false),

    {eof,LastLocation} = lists:keyfind(eof, 1, Forms),
    LastLine = erl_anno:line(erl_anno:new(LastLocation)),
    ExtraFormsAnno =
        if HasColumn ->
                erl_anno:new({LastLine,1});
           not HasColumn ->
                erl_anno:new(LastLine)
        end,

    ExtraForms = [{warning,{ExtraFormsAnno,?MODULE,"injected warning"}}] ++
        [{error,{ExtraFormsAnno,?MODULE,"injected error"}} || AddError],

    lists:map(
      fun
          ({eof,Location}) ->
              Anno = erl_anno:new(Location),
              HasColumn = erl_anno:column(Anno) =/= undefined,
              if AddColumns ->
                      {eof,{erl_anno:line(Anno), 1}};
                  not AddColumns ->
                      {eof,Location}
              end;
          ({ErrorOrWarning,{Location,Module,Text}})
            when ErrorOrWarning =:= error;
                 ErrorOrWarning =:= warning ->
              Anno = erl_anno:new(Location),
              HasColumn = erl_anno:column(Anno) =/= undefined,
              if AddColumns ->
                      {ErrorOrWarning,{{erl_anno:line(Anno), 1}, Module, Text}};
                  not AddColumns ->
                      {ErrorOrWarning, {Location, Module, Text}}
              end;
          (Form) ->
              erl_parse:map_anno(
                fun(Anno) ->
                        HasColumn = erl_anno:column(Anno) =/= undefined,
                        if AddColumns ->
                                erl_anno:set_location({erl_anno:line(Anno), 1}, Anno);
                           not AddColumns ->
                                Anno
                        end
                end, Form)
      end, Forms ++ ExtraForms).

format_error(Error) ->
    Error.
