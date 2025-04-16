#!/usr/bin/env escript

%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

%% This script takes a json string as argument and checks that all the compiler flags defined by the OSSF
%% are used.

main([CompilerFlagsJson]) ->
    io:format(standard_error,"~p",[os:env()]),
    CFLAGS = proplists:get_value(cflags, erlang:system_info(compile_info)) ++ " " ++ os:getenv("SKIPPED_OSSF_CFLAGS"),
    LDFLAGS = proplists:get_value(ldflags, erlang:system_info(compile_info)) ++ " " ++ os:getenv("SKIPPED_OSSF_LDFLAGS"),
    {gnuc, {Vsn, _, _} } = erlang:system_info(c_compiler_used),
    #{ ~"options" := #{ ~"recommended" := Opts } } = json:decode(unicode:characters_to_binary(CompilerFlagsJson)),
    io:format(standard_error, ~s'CFLAGS="~ts"~nLDFLAGS="~ts"~n',[CFLAGS, LDFLAGS]),
    Missing = [Opt || Opt <- Opts, check_option(Opt, string:split(CFLAGS, " ", all), string:split(LDFLAGS, " ", all), Vsn)],
    io:format("~ts~n",[sarif(Missing)]),
    ok.
check_option(#{ ~"requires" := #{ ~"gcc" := GccVsn }, ~"opt" := Opt }, CFLAGS, _LDFLAGS, CurrentGccVsn) ->
    io:format(standard_error, "Looking for ~ts...",[Opt]),
    case binary_to_integer(hd(string:split(GccVsn, "."))) > CurrentGccVsn of
        true -> io:format(standard_error, "skipped!~n",[]), false;
        false ->
            check_for_flags(Opt, CFLAGS)
    end;
check_option(#{ ~"requires" := #{ ~"binutils" := _ }, ~"opt" := Opt }, _CFLAGS, LDFLAGS, _CurrentGccVsn) ->
    io:format(standard_error, "Looking for ~ts...",[Opt]),
    check_for_flags(Opt, LDFLAGS);
check_option(#{ ~"requires" := #{ ~"libstdc++" := _ }, ~"opt" := Opt }, _CFLAGS, LDFLAGS, _CurrentGccVsn) ->
    io:format(standard_error, "Looking for ~ts...",[Opt]),
    check_for_flags(Opt, LDFLAGS);
check_option(#{ ~"requires" := Tool, ~"opt" := Opt }, _CFLAGS, _LDFLAGS, _CurrentGccVsn) ->
    io:format(standard_error, "~ts not implemented yet using ~p!~n",[Opt, Tool]),
    true.

check_for_flags(Flag, Flags) ->
    case lists:any(fun(O) -> lists:search(fun(A) -> string:equal(string:trim(O), string:trim(A)) end, Flags) =:= false end, string:split(Flag, " ", all) ) of
        true -> io:format(standard_error, "missing!~n",[]), true;
        false -> io:format(standard_error, "found!~n",[]), false
    end.

sarif(Missing) ->
    Zip = lists:zip(lists:seq(1,length(Missing)), Missing),
    json:encode(
      #{ ~"version" => ~"2.1.0",
         ~"$schema" => ~"https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json",
         ~"runs" =>
             [ #{
                 ~"tool" =>
                     #{ ~"driver" =>
                            #{ ~"informationUri" => ~"https://github.com/erlang/otp/.github/workflow/ossf-scanner",
                               ~"name" => ~"ossf-scanner",
                               ~"rules" =>
                                   [ #{ ~"id" => base64:encode(erlang:md5(Opt)),
                                        ~"name" => ~"MissingCompilerFlag",
                                        ~"shortDescription" =>
                                            #{ ~"text" => <<"Missing CFLAGS ", Opt/binary>> },
                                        ~"helpUri" => ~"https://best.openssf.org/Compiler-Hardening-Guides/Compiler-Options-Hardening-Guide-for-C-and-C++",
                                        ~"fullDescription" =>
                                            #{
                                              ~"text" => <<Desc/binary,"\nA OSSF C/C++ compiler hardening flag is missing from the tests. "
                                                           "Please check https://best.openssf.org/Compiler-Hardening-Guides/Compiler-Options-Hardening-Guide-for-C-and-C++ for details.">>
                                             }
                                      }
                                     || {_Id, #{ ~"desc" := Desc, ~"opt" := Opt }} <- Zip],
                               ~"version" => ~"1.0"
                             }
                      },
                 ~"artifacts" =>
                     [ #{
                         ~"location" => #{
                                          ~"uri" => ~".github/docker/Dockerfile.64-bit"
                                         },
                         ~"length" => -1
                        }
                     ],
                 ~"results" =>
                     [ #{
                         ~"ruleId" => base64:encode(erlang:md5(Opt)),
                         ~"ruleIndex" => Id,
                         ~"level" => ~"warning",
                         ~"message" => #{ ~"text" => <<"Missing CFLAGS ", Opt/binary>> },
                         ~"locations" =>
                             [ #{ ~"physicalLocation" =>
                                      #{ ~"artifactLocation" =>
                                             #{ ~"uri" => ~".github/docker/Dockerfile.64-bit" }
                                       }
                                } ]
                        } || {Id, #{ ~"opt" := Opt }} <- Zip]
                } ]
       }).