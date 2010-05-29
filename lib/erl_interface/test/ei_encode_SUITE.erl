%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(ei_encode_SUITE).

-include("test_server.hrl").
-include("ei_encode_SUITE_data/ei_encode_test_cases.hrl").

-export(
   [
    all/1,
    test_ei_encode_long/1,
    test_ei_encode_ulong/1,
    test_ei_encode_longlong/1,
    test_ei_encode_ulonglong/1,
    test_ei_encode_char/1,
    test_ei_encode_misc/1,
    test_ei_encode_fails/1
   ]).

all(suite) ->
    [
     test_ei_encode_long,
     test_ei_encode_ulong,
     test_ei_encode_longlong,
     test_ei_encode_ulonglong,
     test_ei_encode_char,
     test_ei_encode_misc,
     test_ei_encode_fails
    ].

%% ---------------------------------------------------------------------------

% NOTE: for historical reasons we don't pach as tight as we can,
%       we only fill 27 bits in 32 bit INTEGER_EXT 


%% ######################################################################## %%

test_ei_encode_long(suite) -> [];
test_ei_encode_long(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_encode_long),

    ?line {<<97,0>>                        ,0}   = get_buf_and_term(P),
    ?line {<<97,255>>                      ,255} = get_buf_and_term(P),
    ?line {<<98,256:32/big-signed-integer>>,256} = get_buf_and_term(P),
    ?line {<<98,-1:32/big-signed-integer>> ,-1}  = get_buf_and_term(P),

    ?line {<<98, 16#07ffffff:32/big-signed-integer>>, 16#07ffffff} = get_buf_and_term(P),
    ?line {<<98,-16#08000000:32/big-signed-integer>>,-16#08000000} = get_buf_and_term(P),
    ?line {<<110,4,0, 0,0,0,8>>                     , 16#08000000} = get_buf_and_term(P),
    ?line {<<110,4,1, 1,0,0,8>>                     ,-16#08000001} = get_buf_and_term(P),

    ?line {<<110,4,0, 255,255,255,127>>             , 16#7fffffff} = get_buf_and_term(P),
    ?line {<<110,4,1, 0,0,0,128>>                   ,-16#80000000} = get_buf_and_term(P),

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_encode_ulong(suite) -> [];
test_ei_encode_ulong(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_encode_ulong),

    ?line {<<97,0>>                          ,0}   = get_buf_and_term(P),
    ?line {<<97,255>>                        ,255} = get_buf_and_term(P),
    ?line {<<98,256:32/big-unsigned-integer>>,256} = get_buf_and_term(P),

    ?line {<<98, 16#07ffffff:32/big-signed-integer>>,16#07ffffff} = get_buf_and_term(P),
    ?line {<<110,4,0, 0,0,0,8>>                     ,16#08000000} = get_buf_and_term(P),

    ?line {<<110,4,0, 255,255,255,127>>             ,16#7fffffff} = get_buf_and_term(P),
    ?line {<<110,4,0, 0,0,0,128>>                   ,16#80000000} = get_buf_and_term(P),
    ?line {<<110,4,0, 255,255,255,255>>             ,16#ffffffff} = get_buf_and_term(P),

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_encode_longlong(suite) -> [];
test_ei_encode_longlong(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {skip,"Skipped on VxWorks"};
	_ ->
	    ?line P = runner:start(?test_ei_encode_longlong),

	    ?line {<<97,0>>                        ,0}   = get_buf_and_term(P),
	    ?line {<<97,255>>                      ,255} = get_buf_and_term(P),
	    ?line {<<98,256:32/big-signed-integer>>,256} = get_buf_and_term(P),
	    ?line {<<98,-1:32/big-signed-integer>> ,-1}  = get_buf_and_term(P),
	    
	    ?line {<<98, 16#07ffffff:32/big-signed-integer>>, 16#07ffffff} = get_buf_and_term(P),
	    ?line {<<98,-16#08000000:32/big-signed-integer>>,-16#08000000} = get_buf_and_term(P),
	    ?line {<<110,4,0, 0,0,0,8>>                     , 16#08000000} = get_buf_and_term(P),
	    ?line {<<110,4,1, 1,0,0,8>>                     ,-16#08000001} = get_buf_and_term(P),

	    ?line {<<110,4,0, 255,255,255,127>>             , 16#7fffffff} = get_buf_and_term(P),
	    ?line {<<110,4,1, 0,0,0,128>>                   ,-16#80000000} = get_buf_and_term(P),
	    ?line {<<110,6,0, 255,255,255,255,255,127>>     , 16#7fffffffffff} = get_buf_and_term(P),
	    ?line {<<110,6,1, 0,0,0,0,0,128>>               ,-16#800000000000} = get_buf_and_term(P),
	    ?line {<<110,8,0, 255,255,255,255,255,255,255,127>>,16#7fffffffffffffff} = get_buf_and_term(P),
	    ?line {<<110,8,1, 0,0,0,0,0,0,0,128>>           ,-16#8000000000000000} = get_buf_and_term(P),

	    ?line runner:recv_eot(P),
	    ok
    end.


%% ######################################################################## %%

test_ei_encode_ulonglong(suite) -> [];
test_ei_encode_ulonglong(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {skip,"Skipped on VxWorks"};
	_ ->
	    ?line P = runner:start(?test_ei_encode_ulonglong),

	    ?line {<<97,0>>                          ,0} = get_buf_and_term(P),
	    ?line {<<97,255>>                        ,255} = get_buf_and_term(P),
	    ?line {<<98,256:32/big-unsigned-integer>>,256} = get_buf_and_term(P),

	    ?line {<<98, 16#07ffffff:32/big-signed-integer>>,16#07ffffff} = get_buf_and_term(P),
	    ?line {<<110,4,0, 0,0,0,8>>              ,16#08000000} = get_buf_and_term(P),

	    ?line {<<110,4,0, 255,255,255,127>>      ,16#7fffffff} = get_buf_and_term(P),
	    ?line {<<110,4,0, 0,0,0,128>>            ,16#80000000} = get_buf_and_term(P),
	    ?line {<<110,4,0, 255,255,255,255>>      ,16#ffffffff} = get_buf_and_term(P),
	    ?line {<<110,6,0, 255,255,255,255,255,255>>,16#ffffffffffff} = get_buf_and_term(P),
	    ?line {<<110,8,0, 255,255,255,255,255,255,255,255>>,16#ffffffffffffffff} = get_buf_and_term(P),

	    ?line runner:recv_eot(P),
	    ok
    end.


%% ######################################################################## %%
%% A "character" for us is an 8 bit integer, alwasy positive, i.e.
%% it is unsigned.
%% FIXME maybe the API should change to use "unsigned char" to be clear?!

test_ei_encode_char(suite) -> [];
test_ei_encode_char(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_encode_char),

    ?line {<<97,  0>>,0} = get_buf_and_term(P),
    ?line {<<97,127>>,16#7f} = get_buf_and_term(P),
    ?line {<<97,255>>,16#ff} = get_buf_and_term(P),

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_encode_misc(suite) -> [];
test_ei_encode_misc(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_encode_misc),

    ?line <<131>>  = get_binaries(P),

    ?line {<<70,_:8/binary>>,F0} = get_buf_and_term(P),
    ?line true = match_float(F0, 0.0),

    ?line {<<70,_:8/binary>>,Fn1} = get_buf_and_term(P),
    ?line true = match_float(Fn1, -1.0),

    ?line {<<70,_:8/binary>>,Fp1} = get_buf_and_term(P),
    ?line true = match_float(Fp1, 1.0),

    ?line {<<100,0,5,"false">>,false}  = get_buf_and_term(P),
    ?line {<<100,0,4,"true">> ,true}   = get_buf_and_term(P),
    ?line {<<100,0,4,"true">> ,true}   = get_buf_and_term(P),
    ?line {<<100,0,4,"true">> ,true}   = get_buf_and_term(P),

    ?line {<<100,0,3,"foo">>,foo}         = get_buf_and_term(P),
    ?line {<<100,0,3,"foo">>,foo}         = get_buf_and_term(P),
    ?line {<<100,0,0,"">>,''}             = get_buf_and_term(P),
    ?line {<<100,0,0,"">>,''}             = get_buf_and_term(P),
    ?line {<<100,0,6,"ÅÄÖåäö">>,'ÅÄÖåäö'} = get_buf_and_term(P),
    ?line {<<100,0,6,"ÅÄÖåäö">>,'ÅÄÖåäö'} = get_buf_and_term(P),

    ?line {<<107,0,3,"foo">>,"foo"}       = get_buf_and_term(P),
    ?line {<<107,0,3,"foo">>,"foo"}       = get_buf_and_term(P),
    ?line {<<106>>,""}                    = get_buf_and_term(P),
    ?line {<<106>>,""}                    = get_buf_and_term(P),
    ?line {<<107,0,6,"ÅÄÖåäö">>,"ÅÄÖåäö"} = get_buf_and_term(P),
    ?line {<<107,0,6,"ÅÄÖåäö">>,"ÅÄÖåäö"} = get_buf_and_term(P),

    ?line {<<109,0,0,0,3,"foo">>,<<"foo">>}       = get_buf_and_term(P),
    ?line {<<109,0,0,0,0,"">>,<<>>}               = get_buf_and_term(P),
    ?line {<<109,0,0,0,6,"ÅÄÖåäö">>,<<"ÅÄÖåäö">>} = get_buf_and_term(P),

    ?line {<<104,0>>,{}}       = get_buf_and_term(P),	% Tuple header for {}
    ?line {<<106>>,[]}         = get_buf_and_term(P),	% Empty list []

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_encode_fails(suite) -> [];
test_ei_encode_fails(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_encode_fails),

    ?line XAtom = list_to_atom(lists:duplicate(255, $x)),
    ?line YAtom = list_to_atom(lists:duplicate(255, $y)),

    ?line XAtom = get_term(P),
    ?line XAtom = get_term(P),
    ?line YAtom = get_term(P),
    ?line YAtom = get_term(P),

    ?line {{{{}}}} = get_term(P),

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

% We read two packets for each test, the ei_encode and ei_x_encode version....

get_buf_and_term(P) ->
    B = get_binaries(P),
    case B of
	<<131>> ->
	    io:format("(got single magic, no content)\n",[]),
	    {B,'$$magic$$'};
	<<131,_>> ->
	    T = binary_to_term(B),
	    io:format("~w\n~w\n(got magic)\n",[B,T]),
	    {B,T};
	_ ->
	    B1 = list_to_binary([131,B]),	% No magic, add
	    T = binary_to_term(B1),
	    io:format("~w\n~w\n(got no magic)\n",[B,T]),
	    {B,T}
    end.
    

get_binaries(P) ->
    B1 = get_binary(P),
    B2 = get_binary(P),
    B1 = B2.

get_binary(P) ->
    case runner:get_term(P) of
	{bytes,L} ->
	    B = list_to_binary(L),
	    io:format("~w\n",[L]),
% For strange reasons <<131>> show up as <>....
%	    io:format("~w\n",[B]),
	    B;
	Other ->
	    Other
    end.

%%

% We use our own get_term()

get_term(P) ->
    case runner:get_term(P) of
	{bytes,[131]} ->
	    io:format("(got single magic, no content)\n",[]),
	    '$$magic$$';
	{bytes,[131,L]} ->
	    B = list_to_binary(L),
	    T = binary_to_term(B),
	    io:format("~w\n~w\n(got magic)\n",[L,T]),
	    T;
	{bytes,L} ->
	    B = list_to_binary([131,L]),
	    T = binary_to_term(B),
	    io:format("~w\n~w\n(got no magic)\n",[L,T]),
	    T;
	Other ->
	    Other
    end.
 
%%

match_float(F, Match) when is_float(F), is_float(Match), F == Match ->
    true;
match_float(F, Match) when is_float(F), F > Match*0.99, F < Match*1.01 ->
    true.
    
