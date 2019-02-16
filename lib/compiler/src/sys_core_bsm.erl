%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
%% Purpose : Optimize bit syntax matching.


-module(sys_core_bsm).
-export([module/2,format_error/1]).

-include("core_parse.hrl").

-spec module(cerl:c_module(), [compile:option()]) -> {'ok', cerl:c_module()}.

module(#c_module{defs=Ds}=Mod, _Opts) ->
    {ok,Mod#c_module{defs=function(Ds)}}.

function([{#c_var{name={F,Arity}}=Name,B0}|Fs]) ->
    try cerl_trees:map(fun bsm_reorder/1, B0) of
        B -> [{Name,B} | function(Fs)]
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [F,Arity]),
            erlang:raise(Class, Error, Stack)
    end;
function([]) ->
    [].

-type error() :: atom().
-spec format_error(error()) -> nonempty_string().

format_error(_) -> error(badarg).

%%% Reorder bit syntax matching to faciliate optimization in further passes.

bsm_reorder(#c_case{arg=#c_var{}=V}=Case) ->
    bsm_reorder_1([V], Case);
bsm_reorder(#c_case{arg=#c_values{es=Es}}=Case) ->
    bsm_reorder_1(Es, Case);
bsm_reorder(Core) ->
    Core.

bsm_reorder_1(Vs0, #c_case{clauses=Cs0}=Case) ->
    case bsm_leftmost(Cs0) of
        Pos when Pos > 0, Pos =/= none ->
            Vs = core_lib:make_values(move_from_col(Pos, Vs0)),
            Cs = [C#c_clause{pats=move_from_col(Pos, Ps)}
                  || #c_clause{pats=Ps}=C <- Cs0],
            Case#c_case{arg=Vs,clauses=Cs};
        _ ->
            Case
    end.

move_from_col(Pos, L) ->
    {First,[Col|Rest]} = lists:split(Pos - 1, L),
    [Col|First] ++ Rest.

%% bsm_leftmost(Cs) -> none | ArgumentNumber
%%  Find the leftmost argument that matches a nonempty binary.
%%  Return either 'none' or the argument number (1-N).

bsm_leftmost(Cs) ->
    bsm_leftmost_1(Cs, none).

bsm_leftmost_1([_|_], 1) ->
    1;
bsm_leftmost_1([#c_clause{pats=Ps}|Cs], Pos) ->
    bsm_leftmost_2(Ps, Cs, 1, Pos);
bsm_leftmost_1([], Pos) -> Pos.

bsm_leftmost_2(_, Cs, Pos, Pos) ->
    bsm_leftmost_1(Cs, Pos);
bsm_leftmost_2([#c_binary{segments=[_|_]}|_], Cs, N, _) ->
    bsm_leftmost_1(Cs, N);
bsm_leftmost_2([_|Ps], Cs, N, Pos) ->
    bsm_leftmost_2(Ps, Cs, N+1, Pos);
bsm_leftmost_2([], Cs, _, Pos) ->
    bsm_leftmost_1(Cs, Pos).
