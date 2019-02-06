%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([base/1,
         gen/1,
         lib/1,
         unknown/1,
         success/1,
         grouped_error/1,
         failed_error/1]).

-include("diameter_ct.hrl").
-include("diameter.hrl").

-define(L, atom_to_list).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [base, gen, lib, unknown, {group, recode}].

groups() ->
    [{recode, [], [success,
                   grouped_error,
                   failed_error]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(recode, Config) ->
    ok = diameter:start(),
    Config.

end_per_group(_, _) ->
    ok =  diameter:stop().

init_per_testcase(gen, Config) ->
    [{application, ?APP, App}] = diameter_util:consult(?APP, app),
    {modules, Ms} = lists:keyfind(modules, 1, App),
    [_|_] = Gs = lists:filter(fun(M) ->
                                      lists:prefix("diameter_gen_", ?L(M))
                              end,
                              Ms),
    [{dicts, Gs} | Config];

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

%% ===========================================================================

base(_Config) ->
    diameter_codec_test:base().

gen([{dicts, Ms} | _]) ->
    lists:foreach(fun diameter_codec_test:gen/1, Ms).

lib(_Config) ->
    diameter_codec_test:lib().

%% Have a separate AVP dictionary just to exercise more code.
unknown(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    ok = make(Data, "recv.dia"),
    ok = make(Data, "avps.dia"),
    {ok, _, _} = compile("diameter_test_avps.erl"),
    ok = make(Data, "send.dia"),
    {ok, _, _} = compile("diameter_test_send.erl"),
    {ok, _, _} = compile("diameter_test_recv.erl"),
    {ok, _, _} = compile(filename:join([Data, "diameter_test_unknown.erl"]),
                         [{i, Priv}]),
    diameter_test_unknown:run().

make(Dir, File) ->
    diameter_make:codec(filename:join([Dir, File])).

compile(File) ->
    compile(File, []).

compile(File, Opts) ->
    compile:file(File, [return | Opts]).

%% ===========================================================================

%% Ensure a Grouped AVP is represented by a list in the avps field.
success(_) ->
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
        = str(recode(str(Avps))).

%% ===========================================================================

%% Ensure a Grouped AVP is represented by a list in the avps field
%% even in the case of a decode error on a component AVP.
grouped_error(_) ->
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
        = str(recode(str(Avps))).

%% ===========================================================================

%% Ensure that a failed decode in Failed-AVP is acceptable, and that
%% the component AVPs are decoded if possible.
failed_error(_) ->
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
        = sta(recode(sta(Avps))).

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

%% recode/1

recode(Msg) ->
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
