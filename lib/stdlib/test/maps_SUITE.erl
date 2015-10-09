%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'maps' module.
%%%-----------------------------------------------------------------

-module(maps_SUITE).

-include_lib("test_server/include/test_server.hrl").

-define(default_timeout, ?t:minutes(1)).

% Test server specific exports
-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([t_get_3/1, t_filter_2/1,
         t_fold_3/1,t_map_2/1,t_size_1/1,
         t_with_2/1,t_without_2/1]).

%-define(badmap(V,F,Args), {'EXIT', {{badmap,V}, [{maps,F,Args,_}|_]}}).
%-define(badarg(F,Args), {'EXIT', {badarg, [{maps,F,Args,_}|_]}}).
% silly broken hipe
-define(badmap(V,F,_Args), {'EXIT', {{badmap,V}, [{maps,F,_,_}|_]}}).
-define(badarg(F,_Args), {'EXIT', {badarg, [{maps,F,_,_}|_]}}).

suite() ->
    [{ct_hooks, [ts_install_cth]}].

all() ->
    [t_get_3,t_filter_2,
     t_fold_3,t_map_2,t_size_1,
     t_with_2,t_without_2].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

t_get_3(Config) when is_list(Config) ->
    Map = #{ key1 => value1, key2 => value2 },
    DefaultValue = "Default value",
    value1 = maps:get(key1, Map, DefaultValue),
    value2 = maps:get(key2, Map, DefaultValue),
    DefaultValue = maps:get(key3, Map, DefaultValue),

    %% error case
    ?badmap(a,get,[[a,b],a,def]) = (catch maps:get([a,b],id(a),def)),
    ok.

t_without_2(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100) -- Ki]),
    M1 = maps:without([{k,I}||I <- Ki],M0),

    %% error case
    ?badmap(a,without,[[a,b],a]) = (catch maps:without([a,b],id(a))),
    ?badmap(a,without,[{a,b},a]) = (catch maps:without({a,b},id(a))),
    ?badarg(without,[a,#{}]) = (catch maps:without(a,#{})),
    ok.

t_with_2(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-Ki]),
    M1 = maps:with([{k,I}||I <- Ki],M0),

    %% error case
    ?badmap(a,with,[[a,b],a]) = (catch maps:with([a,b],id(a))),
    ?badmap(a,with,[{a,b},a]) = (catch maps:with({a,b},id(a))),
    ?badarg(with,[a,#{}]) = (catch maps:with(a,#{})),
    ok.

t_filter_2(Config) when is_list(Config) ->
    M = #{a => 2, b => 3, c=> 4, "a" => 1, "b" => 2, "c" => 4},
    Pred1 = fun(K,V) -> is_atom(K) andalso (V rem 2) =:= 0 end,
    Pred2 = fun(K,V) -> is_list(K) andalso (V rem 2) =:= 0 end,
    #{a := 2,c := 4} = maps:filter(Pred1,M),
    #{"b" := 2,"c" := 4} = maps:filter(Pred2,M),
    %% error case
    ?badmap(a,filter,[_,a]) = (catch maps:filter(fun(_,_) -> ok end,id(a))),
    ?badarg(filter,[<<>>,#{}]) = (catch maps:filter(id(<<>>),#{})),
    ok.

t_fold_3(Config) when is_list(Config) ->
    Vs = lists:seq(1,200),
    M0 = maps:from_list([{{k,I},I}||I<-Vs]),
    #{ {k,1} := 1, {k,200} := 200} = M0,
    Tot0 = lists:sum(Vs),
    Tot1 = maps:fold(fun({k,_},V,A) -> A + V end, 0, M0),
    true = Tot0 =:= Tot1,

    %% error case
    ?badmap(a,fold,[_,0,a]) = (catch maps:fold(fun(_,_,_) -> ok end,0,id(a))),
    ?badarg(fold,[<<>>,0,#{}]) = (catch maps:fold(id(<<>>),0,#{})),
    ok.

t_map_2(Config) when is_list(Config) ->
    Vs = lists:seq(1,200),
    M0 = maps:from_list([{{k,I},I}||I<-Vs]),
    #{ {k,1} := 1, {k,200} := 200} = M0,
    M1 = maps:map(fun({k,_},V) -> V + 42 end, M0),
    #{ {k,1} := 43, {k,200} := 242} = M1,

    %% error case
    ?badmap(a,map,[_,a]) = (catch maps:map(fun(_,_) -> ok end, id(a))),
    ?badarg(map,[<<>>,#{}]) = (catch maps:map(id(<<>>),#{})),
    ok.


t_size_1(Config) when is_list(Config) ->
      0 = maps:size(#{}),
     10 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,10)])),
     20 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,20)])),
     30 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,30)])),
     40 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,40)])),
     50 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,50)])),
     60 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,60)])),
    600 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,600)])),

    %% error case
    ?badmap(a,size,[a]) = (catch maps:size(id(a))),
    ?badmap(<<>>,size,[<<>>]) = (catch maps:size(id(<<>>))),
    ok.

id(I) -> I.
