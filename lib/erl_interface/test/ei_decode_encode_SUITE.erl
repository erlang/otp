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
-module(ei_decode_encode_SUITE).

-include("test_server.hrl").
-include("ei_decode_encode_SUITE_data/ei_decode_encode_test_cases.hrl").

-export(
   [
    all/1,
    test_ei_decode_encode/1
   ]).

all(suite) ->
    [
     test_ei_decode_encode
    ].

%% ---------------------------------------------------------------------------

% NOTE: these types have no meaning on the C side so we pass them
%       to C and back just to see they are the same.


%% ######################################################################## %%

test_ei_decode_encode(suite) -> [];
test_ei_decode_encode(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_decode_encode),

    Fun   = fun (X) -> {X,true} end,
    Pid   = self(),
    Port  = case os:type() of
		{win32,_} ->
		    open_port({spawn,"sort"},[]);
		_ ->
		    open_port({spawn,"/bin/true"},[])
	    end,
    Ref   = make_ref(),
    Trace = {1,2,3,self(),4},			% FIXME how to construct?!


    BigSmallA = 1696192905348584855517250509684275447603964214606878827319923580493120589769459602596313014087329389174229999430092223701630077631205171572331191216670754029016160388576759960413039261647653627052707047,
    BigSmallB = 43581177444506616087519351724629421082877485633442736512567383077022781906420535744195118099822189576169114064491200598595995538299156626345938812352676950427869649947439032133573270227067833308153431095,
    BigSmallC = 52751775381034251994634567029696659541685100826881826508158083211003576763074162948462801435204697796532659535818017760528684167216110865807581759669824808936751316879636014972704885388116861127856231,

    BigLargeA = 1 bsl 11111 + BigSmallA,
    BigLargeB = 1 bsl 11112 + BigSmallB,
    BigLargeC = BigSmallA * BigSmallB * BigSmallC * BigSmallA,

    ?line send_rec(P, Fun),
    ?line send_rec(P, Pid),
    ?line send_rec(P, Port),
    ?line send_rec(P, Ref),
    ?line send_rec(P, Trace),

    % bigs

    ?line send_rec(P, BigSmallA),
    ?line send_rec(P, BigSmallB),
    ?line send_rec(P, BigSmallC),
    
    ?line send_rec(P, BigLargeA),
    ?line send_rec(P, BigLargeB),
    ?line send_rec(P, BigLargeC),

    %% Test large node containers...

    ?line ThisNode = {node(), erlang:system_info(creation)},
    ?line TXPid = mk_pid(ThisNode, 32767, 8191),
    ?line TXPort = mk_port(ThisNode, 268435455),
    ?line TXRef = mk_ref(ThisNode, [262143, 4294967295, 4294967295]),

    ?line OtherNode = {gurka@sallad, 2},
    ?line OXPid = mk_pid(OtherNode, 32767, 8191),
    ?line OXPort = mk_port(OtherNode, 268435455),
    ?line OXRef = mk_ref(OtherNode, [262143, 4294967295, 4294967295]),

    ?line send_rec(P, TXPid),
    ?line send_rec(P, TXPort),
    ?line send_rec(P, TXRef),
    ?line send_rec(P, OXPid),
    ?line send_rec(P, OXPort),
    ?line send_rec(P, OXRef),

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

% We read two packets for each test, the ei_decode_encode and ei_x_decode_encode  version....

send_rec(P, Term) when is_port(P) ->
    ?t:format("Testing: ~p~n", [Term]),
    P ! {self(), {command, term_to_binary(Term)}},
    {_B,Term} = get_buf_and_term(P).
    


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
%% Node container constructor functions
%%

-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).


uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).



mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    mk_pid({atom_to_list(NodeName), Creation}, Number, Serial);
mk_pid({NodeName, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					?PID_EXT,
					?ATOM_EXT,
					uint16_be(length(NodeName)),
					NodeName,
					uint32_be(Number),
					uint32_be(Serial),
					uint8(Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeName, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    mk_port({atom_to_list(NodeName), Creation}, Number);
mk_port({NodeName, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PORT_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeName, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    mk_ref({atom_to_list(NodeName), Creation}, Numbers);
mk_ref({NodeName, Creation}, [Number]) when is_list(NodeName),
					    is_integer(Creation),
					    is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?REFERENCE_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, [Number]]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end;
mk_ref({NodeName, Creation}, Numbers) when is_list(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?NEW_REFERENCE_EXT,
					      uint16_be(length(Numbers)),
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint8(Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

