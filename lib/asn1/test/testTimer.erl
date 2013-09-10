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
-module(testTimer).
-export([go/2]).

-include_lib("test_server/include/test_server.hrl").

-define(times, 5000).

val() ->
    _Value = {'H323-UserInformation',{'H323-UU-PDU',
                            {callProceeding,
                                {'CallProceeding-UUIE',
                                    {0,8,222},
                                    {'EndpointType',
                                        {'NonStandardParameter',
                                            {object,{0,9,237}},
                                            "O"},
                                        {'VendorIdentifier',
                                            {'H221NonStandard',62,63,16282},
                                            "OC",
                                            "OC"},
                                        {'GatekeeperInfo',
                                            {'NonStandardParameter',
                                                {object,{0,10,260}},
                                                "O"}},
                                        {'GatewayInfo',
                                            [{h320,
                                                 {'H320Caps',
                                                     {'NonStandardParameter',
                                                         {object,{0,11,282}},
                                                         "O"},
                                                     [{'DataRate',
                                                          {'NonStandardParameter',
                                                              {object,
                                                                  {0,11,295}},
                                                              "O"},
                                                          1290470518,
                                                          78}],
                                                     [{'SupportedPrefix',
                                                          {'NonStandardParameter',
                                                              {object,
                                                                  {0,12,312}},
                                                              "O"},
                                                          {'h323-ID',"BM"}}]}}], 
                                            {'NonStandardParameter',
                                                {object,{0,13,326}},
                                                "O"}},
                                        {'McuInfo',
                                            {'NonStandardParameter',
                                                {object,{1,13,340,340}},
                                                "OC"}},
                                        {'TerminalInfo',
                                            {'NonStandardParameter',
                                                {object,{1,14,353,354}},
                                                "OC"}},
                                        true,
                                        true},
                                    {ipxAddress,
                                        {'TransportAddress_ipxAddress',
                                            "OCTET ",
                                            "OCTE",
                                            "OC"}},
                                    {'CallIdentifier',"OCTET STRINGOCTE"},
                                    {noSecurity,'NULL'},
                                    [{'ClearToken',
                                         1667517741,
                                         "BM",
                                         {'DHset',[1],[1],[1]},
                                         "OCTET STR",
                                         -26430296,
                                         {'TypedCertificate',
                                             {1,16,405,406},
                                             "OC"},
                                         "BMP",
                                         {'NonStandardParameter',
                                             {1,16,414,415},
                                             "OC"}},
                                     {'ClearToken',
                                         1817656756,
                                         "BMP",
                                         {'DHset',[1],[1],[1]},
                                         "OCTET STRI",
                                         -16356110,
                                         {'TypedCertificate',
                                             {1,17,442,443},
                                             "OC"},
                                         "BMP",
                                         {'NonStandardParameter',
                                             {1,18,452,452},
                                             "OC"}}],
                                    [{cryptoGKPwdEncr,
                                         {'CryptoH323Token_cryptoGKPwdEncr',
                                             {1,18,467,467},
                                             {'Params',-7477016,"OCTET ST"},
                                             "OC"}},
                                     {cryptoGKPwdEncr,
                                         {'CryptoH323Token_cryptoGKPwdEncr',
                                             {1,19,486,486},
                                             {'Params',-2404513,"OCTET ST"},
                                             []}}],
                                    []}},
                            {'NonStandardParameter',{object,{0,3,84}},[]},
                            [],
                            true,
                            [],
                            []},
                        {'H323-UserInformation_user-data',24,"O"}}.
    

go(Config, _Enc) ->
    ?line true = code:add_patha(?config(priv_dir,Config)),

    Module = 'H323-MESSAGES',
    Type = 'H323-UserInformation',
    Value = val(),
    {ok,Bytes} = asn1rt:encode(Module,Type,Value),

    CompileOptions = compile_options(),
    
    {ValWr,done} = timer:tc(fun() -> encode(?times, Module, Type, Value) end),
    ?line io:format("ASN1 encode ~p: ~p micro~n", [CompileOptions, ValWr / ?times]),

    done = decode(2, Module, Type, Bytes),

    {ValRead,done} = timer:tc(fun() -> decode(?times, Module, Type, Bytes) end),
    ?line io:format("ASN1 decode ~p: ~p micro~n", [CompileOptions, ValRead /?times]),


    ?line Comment = "encode: "++integer_to_list(round(ValWr/?times))++
	" micro, decode: "++integer_to_list(round(ValRead /?times))++
	" micro.  " ++ CompileOptions,
    {comment,Comment}.

encode(0, _Module,_Type,_Value) ->
    done;
encode(N, Module,Type,Value) ->
    ?line {ok,B} = asn1rt:encode(Module,Type,Value),
    _B2 = if 
             is_list(B) -> list_to_binary(B);
             true -> B
         end,
    encode(N-1, Module,Type,Value).

decode(0, _Module, _Type, _Value) ->
    done;
decode(N, Module, Type, Value) ->
    {ok,_B} = asn1rt:decode(Module, Type, Value),
    decode(N-1, Module, Type, Value).

compile_options() ->
    {ok,Info} = asn1rt:info('H323-MESSAGES'),
    case lists:keyfind(options, 1, Info) of
	{_,Opts0} ->
	    Opts1 = [X || X <- Opts0,
			  (X =:= ber orelse
			   X =:= per orelse
			   X =:= uper)],
	    lists:flatten(io_lib:format("~p", [Opts1]));
	_ ->
	    "[]"
    end.
    
