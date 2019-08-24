%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(testMegaco).

-export([compile/3,main/2]).

-include_lib("common_test/include/ct.hrl").

compile(Config, Erule, Options) ->
    asn1_test_lib:compile("MEDIA-GATEWAY-CONTROL.asn", Config, [Erule|Options]),
    asn1_test_lib:compile("OLD-MEDIA-GATEWAY-CONTROL.asn", Config, [Erule|Options]),
    {ok,'OLD-MEDIA-GATEWAY-CONTROL','MEDIA-GATEWAY-CONTROL'}.

main(no_module,_) -> ok;
main('OLD-MEDIA-GATEWAY-CONTROL',Config) ->
    CaseDir = proplists:get_value(case_dir, Config),
    {ok,Msg} = asn1ct:value('OLD-MEDIA-GATEWAY-CONTROL','MegacoMessage',
                            [{i, CaseDir}]),
    asn1_test_lib:roundtrip('OLD-MEDIA-GATEWAY-CONTROL', 'MegacoMessage', Msg),
    ok;
main('MEDIA-GATEWAY-CONTROL'=Mod, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = filelib:wildcard(filename:join([DataDir,megacomessages,"*.val"])),
    lists:foreach(fun(File) ->
			  {ok,Bin} = file:read_file(File),
			  V = binary_to_term(Bin),
			  T = element(1, V),
			  asn1_test_lib:roundtrip(Mod, T, V)
		  end, Files).
