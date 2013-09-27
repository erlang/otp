%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(testMegaco).

-export([compile/3,main/2]).

-include_lib("test_server/include/test_server.hrl").

compile(Config, Erule, Options) ->
    asn1_test_lib:compile("MEDIA-GATEWAY-CONTROL.asn", Config, [Erule|Options]),
    asn1_test_lib:compile("OLD-MEDIA-GATEWAY-CONTROL.asn", Config, [Erule|Options]),
    {ok,'OLD-MEDIA-GATEWAY-CONTROL','MEDIA-GATEWAY-CONTROL'}.

main(no_module,_) -> ok;
main('OLD-MEDIA-GATEWAY-CONTROL',Config) ->
    CaseDir = ?config(case_dir, Config),
    {ok,Msg} = asn1ct:value('OLD-MEDIA-GATEWAY-CONTROL','MegacoMessage',
                            [{i, CaseDir}]),
    asn1_test_lib:roundtrip('OLD-MEDIA-GATEWAY-CONTROL', 'MegacoMessage', Msg),
    ok;
main('MEDIA-GATEWAY-CONTROL'=Mod, Config) ->
    DataDir = ?config(data_dir, Config),
    Files = filelib:wildcard(filename:join([DataDir,megacomessages,"*.val"])),
    lists:foreach(fun(File) ->
			  {ok,Bin} = file:read_file(File),
			  V = binary_to_term(Bin),
			  T = element(1, V),
			  asn1_test_lib:roundtrip(Mod, T, V)
		  end, Files).
