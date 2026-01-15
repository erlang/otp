%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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
%% Test encode/decode of dictionary-related modules. Each test case
%% runs multiple tests in parallel since many of the tests are just
%% the same code with different in-data: implementing each test as a
%% single testcase would make for much duplication with ct's current
%% requirement of one function per testcase.
%%

-module(diameter_codec_SUITE).

%% testcases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,

         %% The test cases
         base/1,
         gen/1,
         lib/1,
         unknown/1,
         recode/1
        ]).

-include("diameter.hrl").

-include("diameter_util.hrl").


-define(CL(F),    ?CL(F, [])).
-define(CL(F, A), ?LOG("DCS", F, A)).
-define(L,        atom_to_list).


%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [
     base,
     gen,
     lib,
     unknown,
     recode
    ].


init_per_suite(Config) ->
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?DUTIL:end_per_suite(Config).


%% This test case can take a *long* time, so if the machine is too slow, skip
init_per_testcase(Case, Config) when is_list(Config) ->
    ?CL("init_per_testcase(~w) -> check factor", [Case]),
    Key = dia_factor,
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Factor}} when (Factor > 10) ->
            ?CL("init_per_testcase(~w) -> Too slow (~w) => SKIP",
                [Case, Factor]),
            {skip, {machine_too_slow, Factor}};
        _ ->
            ?CL("init_per_testcase(~w) -> run test", [Case]),
            Config
    end;
init_per_testcase(Case, Config) ->
    ?CL("init_per_testcase(~w) -> entry", [Case]),
    Config.


end_per_testcase(Case, Config) when is_list(Config) ->
    ?CL("end_per_testcase(~w) -> entry", [Case]),
    Config.


%% ===========================================================================

base(_Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    Res = run(base),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.

gen(_Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    Res = run(gen),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.

lib(_Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    Res = run(lib),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.

unknown(Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    Res  = unknown(Priv, Data),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.

recode(_Config) ->
    ?CL("~w -> entry", [?FUNCTION_NAME]),
    Res = run(recode),
    ?CL("~w -> done when"
        "~n   Res: ~p", [?FUNCTION_NAME, Res]),
    Res.


%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run(base) ->
    diameter_codec_test:base();

run(gen) ->
    [{application, diameter, App}] = diameter_util:consult(diameter, app),
    {modules, Ms} = lists:keyfind(modules, 1, App),
    [_|_] = Gs = lists:filter(fun(M) ->
                                      lists:prefix("diameter_gen_", ?L(M))
                              end,
                              Ms),
    lists:foreach(fun(G) ->
                          diameter_codec_test:gen(G, 6733),
                          diameter_codec_test:gen(G, unknown)
                  end, Gs);

run(lib) ->
    diameter_codec_test:lib();

%% Have a separate AVP dictionary just to exercise more code.
run(unknown) ->
    PD = ?MKTEMP("diameter_codec"),
    DD = filename:join([code:lib_dir(diameter),
                        "test",
                        "diameter_codec_SUITE_data"]),
    try
        unknown(PD, DD)
    after
        file:del_dir_r(PD)
    end;

run(success) ->
    success();

run(grouped_error) ->
    grouped_error();

run(failed_error) ->
    failed_error();

run(recode) ->
    ok = diameter:start(),
    try
        ?RUN([{?MODULE, run, [F]} || F  <- [success,
                                            grouped_error,
                                            failed_error]])
    after
        ok = diameter:stop()
    end;

run(List) ->
    ?RUN([{{?MODULE, run, [F]}, 10000} || F <- List]).


%% ===========================================================================

unknown(Priv, Data) ->
    ?CL("~w -> entry with"
        "~n   Priv dir: ~p"
        "~n   Data dir: ~p"
        "~n", [?FUNCTION_NAME, Priv, Data]),
    ok = make(Data, "recv.dia", Priv),
    ok = make(Data, "avps.dia", Priv),
    {ok, _, _} = compile(Priv, "diameter_test_avps.erl"),
    ok = make(Data, "send.dia", Priv),
    {ok, _, _} = compile(Priv, "diameter_test_send.erl"),
    {ok, _, _} = compile(Priv, "diameter_test_recv.erl"),
    {ok, _, _} = compile(Priv,
                         filename:join([Data, "diameter_test_unknown.erl"]),
                         [{i, Priv}]),
    diameter_test_unknown:run().

make(Dir, File, Out) ->
    ?CL("~w -> entry with"
        "~n   File: ~p"
        "~n", [?FUNCTION_NAME, File]),
    pcall(fun() ->
                  diameter_make:codec(filename:join(Dir, File), [{outdir, Out}])
          end, 5000).

compile(Dir, File) ->
    compile(Dir, File, []).

compile(Dir, File, Opts) ->
    ?CL("~w -> entry with"
        "~n   File: ~p"
        "~n   Opts: ~p"
        "~n", [?FUNCTION_NAME, File, Opts]),
    pcall(fun() ->
                  compile:file(filename:join(Dir, File), [return | Opts])
          end).


pcall(F) when is_function(F) ->
    pcall(F, infinity, ?SECS(1)).

pcall(F, Timeout)
  when is_function(F) andalso is_integer(Timeout) andalso (Timeout > 0) ->
    TMP = Timeout div 4,
    PollTimeout =
        if
            (TMP > 1000) ->
                1000;
            true ->
                TMP
        end,
    pcall(F, Timeout, PollTimeout).


pcall(F, Timeout, PollTimeout)
  when is_function(F) andalso
       is_integer(Timeout) andalso
       ((PollTimeout =:= infinity) orelse
        (is_integer(PollTimeout) andalso (Timeout > PollTimeout))) ->
    ?PCALL(F, Timeout, PollTimeout);
pcall(F, Timeout, PollTimeout) 
  when is_function(F) andalso
       (Timeout =:= infinity) andalso
       ((PollTimeout =:= infinity) orelse
        (is_integer(PollTimeout) andalso (PollTimeout > 0))) ->
    ?PCALL(F, Timeout, PollTimeout).






%% ===========================================================================

%% Ensure a Grouped AVP is represented by a list in the avps field.
success() ->
    Avps = [{295, <<1:32>>},  %% Termination-Cause
            {284, [{280, "Proxy-Host"}, %% Proxy-Info
                   {33, "Proxy-State"}, %%
                   {295, <<2:32>>}]}],  %% Termination-Cause
    #diameter_packet{avps = [#diameter_avp{code = 295,
                                           value = 1,
                                           data = <<1:32>>},
                             [#diameter_avp{code = 284},
                              #diameter_avp{code = 280},
                              #diameter_avp{code = 33},
                              #diameter_avp{code = 295,
                                            value = 2,
                                            data = <<2:32>>}]],
                     errors = []}
        = str(repkg(str(Avps))).

%% ===========================================================================

%% Ensure a Grouped AVP is represented by a list in the avps field
%% even in the case of a decode error on a component AVP.
grouped_error() ->
    Avps = [{295, <<1:32>>},  %% Termination-Cause
            {284, [{295, <<0:32>>},      %% Proxy-Info, Termination-Cause
                   {280, "Proxy-Host"},
                   {33, "Proxy-State"}]}],
    #diameter_packet{avps = [#diameter_avp{code = 295,
                                           value = 1,
                                           data = <<1:32>>},
                             [#diameter_avp{code = 284},
                              #diameter_avp{code = 295,
                                            value = undefined,
                                            data = <<0:32>>},
                              #diameter_avp{code = 280},
                              #diameter_avp{code = 33}]],
                     errors = [{5004, #diameter_avp{code = 284}}]}
        = str(repkg(str(Avps))).

%% ===========================================================================

%% Ensure that a failed decode in Failed-AVP is acceptable, and that
%% the component AVPs are decoded if possible.
failed_error() ->
    Avps = [{279, [{295, <<0:32>>},    %% Failed-AVP, Termination-Cause
                   {258, <<1:32>>},             %% Auth-Application-Id
                   {284, [{280, "Proxy-Host"},  %% Proxy-Info
                          {33, "Proxy-State"},
                          {295, <<0:32>>},      %% Termination-Cause, invalid
                          {258, <<2:32>>}]}]}], %% Auth-Application-Id
    #diameter_packet{avps = [[#diameter_avp{code = 279},
                              #diameter_avp{code = 295,
                                            value = undefined,
                                            data = <<0:32>>},
                              #diameter_avp{code = 258,
                                            value = 1,
                                            data = <<1:32>>},
                              [#diameter_avp{code = 284},
                               #diameter_avp{code = 280},
                               #diameter_avp{code = 33},
                               #diameter_avp{code = 295,
                                             value = undefined},
                               #diameter_avp{code = 258,
                                             value = 2,
                                             data = <<2:32>>}]]],
                     errors = []}
        = sta(repkg(sta(Avps))).

%% ===========================================================================

%% str/1

str(#diameter_packet{avps = [#diameter_avp{code = 263},
                             #diameter_avp{code = 264},
                             #diameter_avp{code = 296},
                             #diameter_avp{code = 283},
                             #diameter_avp{code = 258,
                                           value = 0}
                             | T]}
    = Pkt) ->
    Pkt#diameter_packet{avps = T};

str(Avps) ->
    OH = "diameter.erlang.org",
    OR = "erlang.org",
    DR = "example.com",
    Sid = "diameter.erlang.org;123;456",

    [#diameter_header{version = 1,
                      cmd_code = 275,  %% STR
                      is_request = true,
                      application_id = 0,
                      hop_by_hop_id = 17,
                      end_to_end_id = 42,
                      is_proxiable = false,
                      is_error = false,
                      is_retransmitted = false}
     | avp([{263, Sid},  %% Session-Id
            {264, OH},   %% Origin-Host
            {296, OR},   %% Origin-Realm
            {283, DR},   %% Destination-Realm
            {258, <<0:32>>}]  %% Auth-Application-Id
           ++ Avps)].

%% sta/1

sta(#diameter_packet{avps = [#diameter_avp{code = 263},
                             #diameter_avp{code = 268},
                             #diameter_avp{code = 264},
                             #diameter_avp{code = 296},
                             #diameter_avp{code = 278,
                                           value = 4}
                             | T]}
    = Pkt) ->
    Pkt#diameter_packet{avps = T};

sta(Avps) ->
    OH = "diameter.erlang.org",
    OR = "erlang.org",
    Sid = "diameter.erlang.org;123;456",

    [#diameter_header{version = 1,
                      cmd_code = 275,  %% STA
                      is_request = false,
                      application_id = 0,
                      hop_by_hop_id = 17,
                      end_to_end_id = 42,
                      is_proxiable = false,
                      is_error = false,
                      is_retransmitted = false}
     | avp([{263, Sid},         %% Session-Id
            {268, <<2002:32>>}, %% Result-Code
            {264, OH},          %% Origin-Host
            {296, OR},          %% Origin-Realm
            {278, <<4:32>>}]    %% Origin-State-Id
           ++ Avps)].

avp({Code, Data}) ->
    #diameter_avp{code = Code,
                  data = avp(Data)};

avp(#diameter_avp{} = A) ->
    A;

avp([{_,_} | _] = Avps) ->
    lists:map(fun avp/1, Avps);

avp(V) ->
    V.

%% repkg/1

repkg(Msg) ->
    recode(Msg, diameter_gen_base_rfc6733).

recode(#diameter_packet{} = Pkt, Dict) ->
    diameter_codec:decode(Dict, opts(Dict), diameter_codec:encode(Dict, Pkt));

recode(Msg, Dict) ->
    recode(#diameter_packet{msg = Msg}, Dict).

opts(Mod) ->
    #{app_dictionary => Mod,
      decode_format => record,
      string_decode => false,
      strict_mbit => true,
      rfc => 6733,
      failed_avp => false}.
