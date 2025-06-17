%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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

%%
%% Test service and transport config. In particular, of the detection
%% of config errors.
%%

-module(diameter_config_SUITE).

%% testscases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         %% The test cases
         start_service/1,
         add_transport/1]).

-include("diameter_util.hrl").

%% Lists of {Key, GoodConfigList, BadConfigList} with which to
%% configure.

-define(SERVICE_CONFIG,
        [{application,
          [[[{dictionary, diameter_gen_base_rfc6733},
             {module, ?MODULE}]]
           | [[[{dictionary, D},
                {module, M},
                {alias, A},
                {state, S},
                {answer_errors, AE},
                {request_errors, RE},
                {call_mutates_state, C}]]
              || D <- [diameter_gen_base_rfc3588, diameter_gen_base_rfc6733],
                 M <- [?MODULE, [?MODULE, diameter_lib:now()]],
                 A <- [0, common, make_ref()],
                 S <- [[], make_ref()],
                 AE <- [report, callback, discard],
                 RE <- [answer_3xxx, answer, callback],
                 C <- [true, false]]],
          [[x],
           [[]],
           [[{dictionary, diameter_gen_base_rfc3588}]],
           [[{module, ?MODULE}]]
           | [[[{dictionary, diameter_gen_base_rfc6733},
                {module, ?MODULE},
                {K,x}]]
              || K <- [answer_errors,
                       request_errors,
                       call_mutates_state]]]},
         {restrict_connections,
          [[false], [node], [nodes], [[node(), node()]]],
          []},
         {sequence,
          [[{0,32}], [{1,31}]],
          [[{2,31}]]},
         {share_peers,
          [[true],
           [false],
           [[node()]]],
          [[x]]},
         {use_shared_peers,
          [[true],
           [false],
           [[node(), node()]]],
          [[x]]},
         {string_decode,
          [[true], [false]],
          [[0], [x]]},
         {incoming_maxlen,
          [[0], [65536], [16#FFFFFF]],
          [[-1], [1 bsl 24], [infinity], [false]]},
         {spawn_opt,
          [[[]], [[monitor, link]]],
          [[false]]},
         {invalid_option,  %% invalid service options are rejected
          [],
          [[x],
           [x,x]]}]).

-define(TRANSPORT_CONFIG,
        [{transport_module,
          [[?MODULE]],
          [[[?MODULE]]]},
         {transport_config,
          [[{}, 3000],
           [{}, infinity]],
          [[{}, x]]},
         {applications,
          [[[1, a, [x]]]],
          [[x]]},
         {capabilities,
          [[[{'Origin-Host', "diameter.erlang.org"}]],
           [[{'Origin-Realm', "erlang.org"}]]]
          ++ [[[{'Host-IP-Address', L}]]
              || L <- [[{127,0,0,1}],
                       ["127.0.0.1"],
                       ["127.0.0.1", "FFFF::1", "::1", {1,2,3,4,5,6,7,8}]]]
          ++ [[[{'Product-Name', N}]]
              || N <- [["Product", $-, ["Name"]],
                       "Norðurálfa",
                       "ᚠᚢᚦᚨᚱᚲ"]]
          ++ [[[{K,V}]]
              || K <- ['Vendor-Id',
                       'Origin-State-Id',
                       'Firmware-Revision'],
                 V <- [0, 256, 16#FFFF]]
          ++ [[[{K,V}]]
              || K <- ['Supported-Vendor-Id',
                       'Auth-Application-Id',
                       'Acct-Application-Id',
                       'Inband-Security-Id'],
                 V <- [[17], [0, 256, 16#FFFF]]]
          ++ [[[{'Vendor-Specific-Application-Id',
                 [[{'Vendor-Id', V},
                   {'Auth-Application-Id', [0]},
                   {'Acct-Application-Id', [4]}]]}]]
              || V <- [1, [1]]],
          [[x], [[{'Origin-Host', "ᚠᚢᚦᚨᚱᚲ"}]]]
          ++ [[[{'Host-IP-Address', A}]]
              || A <- [{127,0,0,1}]]
          ++ [[[{'Product-Name', N}]]
              || N <- [x, 1]]
          ++ [[[{K,V}]]
              || K <- ['Vendor-Id',
                       'Origin-State-Id',
                       'Firmware-Revision'],
                 V <- [x, [0], -1, 1 bsl 32]]
          ++ [[[{K,V}]]
              || K <- ['Supported-Vendor-Id',
                       'Auth-Application-Id',
                       'Acct-Application-Id',
                       'Inband-Security-Id'],
                 V <- [x, 17, [-1], [1 bsl 32]]]
          ++ [[[{'Vendor-Specific-Application-Id', V}]]
              || V <- [x,
                       [[{'Vendor-Id', 1 bsl 32}]],
                       [[{'Auth-Application-Id', 1}]]]]},
         {capabilities_cb,
          [[x]],
          []},
         {capx_timeout,
          [[3000]],
          [[{?MODULE, tmo, []}]]},
         {disconnect_cb,
          [[x]],
          []},
         {length_errors,
          [[exit], [handle], [discard]],
          [[x]]},
         {dpr_timeout,
          [[0], [3000], [16#FFFFFFFF]],
          [[infinity], [-1], [1 bsl 32], [x]]},
         {dpa_timeout,
          [[0], [3000], [16#FFFFFFFF]],
          [[infinity], [-1], [1 bsl 32], [x]]},
         {connect_timer,
          [[3000]],
          [[infinity]]},
         {watchdog_timer,
          [[3000],
           [{?MODULE, tmo, []}]],
          [[infinity],
           [-1]]},
         {watchdog_config,
          [[[{okay, 0}, {suspect, 0}]],
           [[{okay, 1}]],
           [[{suspect, 2}]]],
          [[x],
           [[{open, 0}]]]},
         {pool_size,
          [[1], [100]],
          [[0], [infinity], [-1], [x]]},
         {private,
          [[x]],
          []},
         {spawn_opt,
          [[[]], [[monitor, link]]],
          [[false]]},
         {invalid_option,  %% invalid transport options are silently ignored
          [[x],
           [x,x]],
          []}]).


-define(CL(F),    ?CL(F, [])).
-define(CL(F, A), ?LOG("DCONF", F, A)).


%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 15}}].

all() ->
    [
     start_service,
     add_transport
    ].


init_per_suite(Config) ->
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?DUTIL:end_per_suite(Config).


start_service(Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    put(dia_factor, dia_factor(Config)),
    Res = run([?FUNCTION_NAME]),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.

add_transport(Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    put(dia_factor, dia_factor(Config)),
    Res = run([?FUNCTION_NAME]),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.

dia_factor(Config) ->
    {value, {?FUNCTION_NAME, DiaFactor}} =
        lists:keysearch(?FUNCTION_NAME, 1, Config),
    DiaFactor.

%% ===========================================================================

%% Factor: >= 1
to(Base, Factor) when (Factor >= 0) ->
    round(Base * (((Factor-1) + 10) / 10)).

run() ->
    run(all()).

run(List)
  when is_list(List) ->
    BaseTo = 5000,
    To = case get(dia_factor) of
             undefined ->
                 BaseTo;
             DF when is_integer(DF) ->
                 to(BaseTo, DF)
         end,
    ?CL("~w -> timeout calculated to ~w", [?FUNCTION_NAME, To]),
    try
        ?RUN([[[fun run/1, {F, To}] || F <- List]])
    after
        dbg:stop(),
        diameter:stop()
    end;

run({F, Tmo}) ->
    ?CL("~w -> entry - try start diameter", [?FUNCTION_NAME]),
    ok = diameter:start(),
    try
        ?CL("~w -> try - run ~p", [?FUNCTION_NAME, F]),
        ?RUN([{[fun run/1, F], Tmo}])
    after
        ?CL("~w -> after - try stop diameter", [?FUNCTION_NAME]),
        ok = diameter:stop()
    end;

run(start_service = Case) ->
    ?CL("~w(~w) -> entry", [?FUNCTION_NAME, Case]),
    ?RUN([[fun start/1, T]
               || T <- [lists:keyfind(capabilities, 1, ?TRANSPORT_CONFIG)
                       | ?SERVICE_CONFIG]]);

run(add_transport = Case) ->
    ?CL("~w(~w) -> entry", [?FUNCTION_NAME, Case]),
    ?RUN([[fun add/1, T] || T <- ?TRANSPORT_CONFIG]).

start(T) ->
    ?CL("~w -> entry with"
        "~n   T: ~p", [?FUNCTION_NAME, T]),
    do(fun start/3, T).

add(T) ->
    ?CL("~w -> entry with"
        "~n   T: ~p", [?FUNCTION_NAME, T]),
    do(fun add/3, T).


%% ===========================================================================

%% do/2

do(F, {Key, Good, Bad}) ->
    ?CL("~w -> entry with"
        "~n   Key:  ~p"
        "~n   Good: ~p"
        "~n   Bad:  ~p", [?FUNCTION_NAME, Key, Good, Bad]),
    F(Key, Good, Bad).

%% add/3

add(Key, Good, Bad) ->
    ?CL("~w -> entry with"
        "~n   Key:  ~p"
        "~n   Good: ~p"
        "~n   Bad:  ~p", [?FUNCTION_NAME, Key, Good, Bad]),
    {[],[]} = {[{Vs,T} || Vs <- Good,
                          T <- [add(Key, Vs)],
                          [T] /= [T || {ok,_} <- [T]]],
               [{Vs,T} || Vs <- Bad,
                          T <- [add(Key, Vs)],
                          [T] /= [T || {error,_} <- [T]]]}.

add(Key, Vs) ->
    ?CL("~w -> entry with"
        "~n   Key: ~p"
        "~n   Vs:  ~p", [?FUNCTION_NAME, Key, Vs]),
    T = list_to_tuple([Key | Vs]),
    diameter:add_transport(make_ref(), {connect, [T]}).

%% start/3

start(Key, Good, Bad) ->
    ?CL("~w -> entry with"
        "~n   Key:  ~p"
        "~n   Good: ~p"
        "~n   Bad:  ~p", [?FUNCTION_NAME, Key, Good, Bad]),
    {[],[]} = {[{Vs,T} || Vs <- Good,
                          T <- [start(Key, Vs)],
                          T /= ok],
               [{Vs,T} || Vs <- Bad,
                          T <- [start(Key, Vs)],
                          [T] /= [T || {error,_} <- [T]]]}.

start(capabilities = K, [Vs]) ->
    ?CL("~w -> entry with"
        "~n   K:  ~p"
        "~n   Vs: ~p", [?FUNCTION_NAME, K, Vs]),
    if is_list(Vs) ->
            start(make_ref(), Vs ++ apps(K));
       true ->
            {error, Vs}
    end;

start(Key, Vs)
  when is_atom(Key) ->
    ?CL("~w -> entry with"
        "~n   Key: ~p"
        "~n   Vs:  ~p", [?FUNCTION_NAME, Key, Vs]),
    start(make_ref(), [list_to_tuple([Key | Vs]) | apps(Key)]);

start(SvcName, Opts) ->
    try
        ?CL("~w -> [try] - start service: "
            "~n   SvcName: ~p"
            "~n   Opts:    ~p", [?FUNCTION_NAME, SvcName, Opts]),
        Res1 = diameter:start_service(SvcName, Opts),
        ?CL("~w -> [try] - start service result: "
            "~n   Res: ~p", [?FUNCTION_NAME, Res1]),
        Res1
    after
        ?CL("~w -> [after] - try stop service: "
            "~n   SvcName: ~p", [?FUNCTION_NAME, SvcName]),
        Res2 = diameter:stop_service(SvcName),
        ?CL("~w -> [after] - stop service result: "
            "~n   Res: ~p", [?FUNCTION_NAME, Res2]),
        Res2
    end.

apps(application) ->
    [];
apps(_) ->
    [{application, [{dictionary, diameter_gen_base_rfc6733},
                    {module, ?MODULE}]}].
