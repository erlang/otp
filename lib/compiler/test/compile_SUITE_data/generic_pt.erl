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

-module(generic_pt).
-export([core_transform/2, parse_transform/2, format_error/1]).

core_transform(Forms, Options) ->
    Source = not_used,
    actions(Source, Forms, Options).

parse_transform(Forms, Options) ->
    Source = get_source(Forms),
    actions(Source, Forms, Options).

actions(Source, Forms, Options) ->
    Ws = [{Source, [{{1,1}, ?MODULE, bad_moon_phase}]}],
    Es = [{Source, [{{1,1}, ?MODULE, fire_on_mother_board}]}],
    case proplists:get_value(action, Options, none) of
        none ->
            Forms;
        warning ->
            {warning, Forms, Es};
        error ->
            {error, Es, Ws};
        undefined_error ->
            {error, [{Source, [{{1,1}, ?MODULE, unknown_error}]}], []};
        throw ->
            throw(thrown);
        exit ->
            exit(exited)
    end.

format_error(bad_moon_phase) ->
    "bad phase of the moon";
format_error(fire_on_mother_board) ->
    "the mother board is on fire".

get_source([{attribute,_,file,{Source,_}}|_]) ->
    Source.
