%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(ssh_eqc_encode_decode).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
%%-define(PROPER,true).
%%-define(TRIQ,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.


%%% Properties:

prop_ssh_decode() ->
    ?FORALL(Msg, ssh_msg(),
	    try ssh_message:decode(Msg)
	    of
		_ -> true
	    catch
		C:E -> io:format('~p:~p~n',[C,E]),
		       false
	    end
	   ).


%%% This fails because ssh_message is not symmetric in encode and decode regarding data types
prop_ssh_decode_encode() ->
    ?FORALL(Msg, ssh_msg(),
	    Msg == ssh_message:encode(ssh_message:decode(Msg))
	   ).


%%%================================================================
%%%
%%% Scripts to generate message generators
%%%

%% awk '/^( |\t)+byte( |\t)+SSH/,/^( |\t)*$/{print}' rfc425?.txt | sed 's/^\( \|\\t\)*//' > msgs.txt

%% awk '/^byte( |\t)+SSH/{print $2","}' < msgs.txt

%% awk 'BEGIN{print "%%%---- BEGIN GENERATED";prev=0} END{print "    >>.\n%%%---- END GENERATED"}  /^byte( |\t)+SSH/{if (prev==1) print "    >>.\n"; prev=1; printf "%c%s%c",39,$2,39; print "()->\n   <<?"$2;next}  /^string( |\t)+\"/{print "    ,"$2;next} /^string( |\t)+.*address/{print "    ,(ssh_string_address())/binary %%",$2,$3,$4,$5,$6;next}/^string( |\t)+.*US-ASCII/{print "    ,(ssh_string_US_ASCII())/binary %%",$2,$3,$4,$5,$6;next} /^string( |\t)+.*UTF-8/{print "    ,(ssh_string_UTF_8())/binary %% ",$2,$3,$4,$5,$6;next}    /^[a-z0-9]+( |\t)/{print "    ,(ssh_"$1"())/binary %%",$2,$3,$4,$5,$6;next}  /^byte\[16\]( |\t)+/{print"    ,(ssh_byte_16())/binary %%",$2,$3,$4,$5,$6;next} /^name-list( |\t)+/{print"    ,(ssh_name_list())/binary %%",$2,$3,$4,$5,$6;next} /./{print "?? %%",$0}' < msgs.txt > gen.txt

%%%================================================================
%%%
%%% Generators
%%% 

ssh_msg() -> ?LET(M,oneof(
[[msg_code('SSH_MSG_CHANNEL_CLOSE'),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_DATA'),gen_uint32(),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_EOF'),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_EXTENDED_DATA'),gen_uint32(),gen_uint32(),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_FAILURE'),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("direct-tcpip"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32(),gen_string( ),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("forwarded-tcpip"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32(),gen_string( ),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("session"),gen_uint32(),gen_uint32(),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("x11"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string( ),gen_uint32(),gen_uint32(),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN_CONFIRMATION'),gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_OPEN_FAILURE'),gen_uint32(),gen_uint32(),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("env"),gen_boolean(),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exec"),gen_boolean(),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exit-signal"),0,gen_string( ),gen_boolean(),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exit-status"),0,gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("pty-req"),gen_boolean(),gen_string( ),gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("shell"),gen_boolean()],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("signal"),0,gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("subsystem"),gen_boolean(),gen_string( )],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("window-change"),0,gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("x11-req"),gen_boolean(),gen_boolean(),gen_string( ),gen_string( ),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("xon-xoff"),0,gen_boolean()],
    [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string( ),gen_boolean()],
    [msg_code('SSH_MSG_CHANNEL_SUCCESS'),gen_uint32()],
    [msg_code('SSH_MSG_CHANNEL_WINDOW_ADJUST'),gen_uint32(),gen_uint32()],
%%Assym    [msg_code('SSH_MSG_DEBUG'),gen_boolean(),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_DISCONNECT'),gen_uint32(),gen_string( ),gen_string( )],
%%Assym    [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string("cancel-tcpip-forward"),gen_boolean(),gen_string( ),gen_uint32()],
%%Assym    [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string("tcpip-forward"),gen_boolean(),gen_string( ),gen_uint32()],
%%Assym    [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string( ),gen_boolean()],
    [msg_code('SSH_MSG_IGNORE'),gen_string( )],
    %% [msg_code('SSH_MSG_KEXDH_INIT'),gen_mpint()],
    %% [msg_code('SSH_MSG_KEXDH_REPLY'),gen_string( ),gen_mpint(),gen_string( )],
    %% [msg_code('SSH_MSG_KEXINIT'),gen_byte(16),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_boolean(),gen_uint32()],
    [msg_code('SSH_MSG_KEX_DH_GEX_GROUP'),gen_mpint(),gen_mpint()],
    [msg_code('SSH_MSG_NEWKEYS')],
    [msg_code('SSH_MSG_REQUEST_FAILURE')],
    [msg_code('SSH_MSG_REQUEST_SUCCESS')],
    [msg_code('SSH_MSG_REQUEST_SUCCESS'),gen_uint32()],
    [msg_code('SSH_MSG_SERVICE_ACCEPT'),gen_string( )],
    [msg_code('SSH_MSG_SERVICE_REQUEST'),gen_string( )],
    [msg_code('SSH_MSG_UNIMPLEMENTED'),gen_uint32()],
    [msg_code('SSH_MSG_USERAUTH_BANNER'),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_USERAUTH_FAILURE'),gen_name_list(),gen_boolean()],
    [msg_code('SSH_MSG_USERAUTH_PASSWD_CHANGEREQ'),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_USERAUTH_PK_OK'),gen_string( ),gen_string( )],
    [msg_code('SSH_MSG_USERAUTH_SUCCESS')]
]

), list_to_binary(M)).


%%%================================================================
%%%
%%% Generator
%%% 

do() -> 
    io_lib:format('[~s~n]',
		  [write_gen(
		     files(["rfc4254.txt", 
			    "rfc4253.txt", 
			    "rfc4419.txt",
			    "rfc4252.txt",
			    "rfc4256.txt"]))]).
    

write_gen(L) when is_list(L) -> 
    string:join(lists:map(fun write_gen/1, L), ",\n    ");
write_gen({MsgName,Args}) -> 
    lists:flatten(["[",generate_args([MsgName|Args]),"]"]).
     
generate_args(As) -> string:join([generate_arg(A) || A <- As], ",").

generate_arg({<<"string">>, <<"\"",B/binary>>}) -> 
    S = get_string($",B),
    ["gen_string(\"",S,"\")"];
generate_arg({<<"string">>, _}) -> "gen_string( )";
generate_arg({<<"byte[",B/binary>>, _}) -> 
    io_lib:format("gen_byte(~p)",[list_to_integer(get_string($],B))]);
generate_arg({<<"byte">>  ,_}) ->    "gen_byte()";
generate_arg({<<"uint16">>,_}) ->    "gen_uint16()";
generate_arg({<<"uint32">>,_}) ->    "gen_uint32()";
generate_arg({<<"uint64">>,_}) ->    "gen_uint64()";
generate_arg({<<"mpint">>,_}) ->     "gen_mpint()";
generate_arg({<<"name-list">>,_}) -> "gen_name_list()";
generate_arg({<<"boolean">>,<<"FALSE">>}) -> "0";
generate_arg({<<"boolean">>,<<"TRUE">>}) ->  "1";
generate_arg({<<"boolean">>,_}) -> "gen_boolean()";
generate_arg({<<"....">>,_}) -> "";  %% FIXME
generate_arg(Name) when is_binary(Name) -> 
    lists:flatten(["msg_code('",binary_to_list(Name),"')"]).


gen_boolean() -> choose(0,1).

gen_byte() -> choose(0,255).

gen_uint16() -> gen_byte(2).

gen_uint32() -> gen_byte(4).

gen_uint64() -> gen_byte(8).

gen_byte(N) when N>0 -> [gen_byte() || _ <- lists:seq(1,N)].
    
gen_char() -> choose($a,$z).

gen_mpint() -> ?LET(Size, choose(1,20), 
	       ?LET(Str, vector(Size, gen_byte()),
		    gen_string( strip_0s(Str) )
		   )).

strip_0s([0|T]) -> strip_0s(T);
strip_0s(X) -> X.
    

gen_string() -> 
    ?LET(Size, choose(0,10), 
	 ?LET(Vector,vector(Size, gen_char()),
	      gen_string(Vector) 
	     )).

gen_string(S) when is_binary(S) -> gen_string(binary_to_list(S));
gen_string(S) when is_list(S) -> uint32_to_list(length(S)) ++ S.
    
gen_name_list() ->
    ?LET(NumNames, choose(0,10),
	 ?LET(L, [gen_name() || _ <- lists:seq(1,NumNames)],
	      gen_string( string:join(L,"," ) )
	)).

gen_name() -> gen_string().

uint32_to_list(I) ->  binary_to_list(<<I:32/unsigned-big-integer>>).
    
%%%----
get_string(Delim, B) -> 
    binary_to_list( element(1, split_binary(B, count_string_chars(Delim,B,0))) ).

count_string_chars(Delim, <<Delim,_/binary>>, Acc) -> Acc;
count_string_chars(Delim, <<_,B/binary>>, Acc) -> count_string_chars(Delim, B, Acc+1).


-define(MSG_CODE(Name,Num),
msg_code(Name) -> Num;
msg_code(Num) -> Name
).

?MSG_CODE('SSH_MSG_USERAUTH_REQUEST',   50);
?MSG_CODE('SSH_MSG_USERAUTH_FAILURE',   51);
?MSG_CODE('SSH_MSG_USERAUTH_SUCCESS',   52);
?MSG_CODE('SSH_MSG_USERAUTH_BANNER',   53);
?MSG_CODE('SSH_MSG_USERAUTH_PK_OK',   60);
?MSG_CODE('SSH_MSG_USERAUTH_PASSWD_CHANGEREQ',   60);
?MSG_CODE('SSH_MSG_DISCONNECT',   1);
?MSG_CODE('SSH_MSG_IGNORE',   2);
?MSG_CODE('SSH_MSG_UNIMPLEMENTED',   3);
?MSG_CODE('SSH_MSG_DEBUG',   4);
?MSG_CODE('SSH_MSG_SERVICE_REQUEST',   5);
?MSG_CODE('SSH_MSG_SERVICE_ACCEPT',   6);
?MSG_CODE('SSH_MSG_KEXINIT',   20);
?MSG_CODE('SSH_MSG_NEWKEYS',   21);
?MSG_CODE('SSH_MSG_GLOBAL_REQUEST',   80);
?MSG_CODE('SSH_MSG_REQUEST_SUCCESS',   81);
?MSG_CODE('SSH_MSG_REQUEST_FAILURE',   82);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN',   90);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN_CONFIRMATION',   91);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN_FAILURE',   92);
?MSG_CODE('SSH_MSG_CHANNEL_WINDOW_ADJUST',   93);
?MSG_CODE('SSH_MSG_CHANNEL_DATA',   94);
?MSG_CODE('SSH_MSG_CHANNEL_EXTENDED_DATA',   95);
?MSG_CODE('SSH_MSG_CHANNEL_EOF',   96);
?MSG_CODE('SSH_MSG_CHANNEL_CLOSE',   97);
?MSG_CODE('SSH_MSG_CHANNEL_REQUEST',   98);
?MSG_CODE('SSH_MSG_CHANNEL_SUCCESS',   99);
?MSG_CODE('SSH_MSG_CHANNEL_FAILURE',   100);
?MSG_CODE('SSH_MSG_USERAUTH_INFO_REQUEST',   60);
?MSG_CODE('SSH_MSG_USERAUTH_INFO_RESPONSE',   61);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REQUEST_OLD',   30);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REQUEST',   34);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_GROUP',   31);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_INIT',   32);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REPLY', 33).

%%%=============================================================================
%%%=============================================================================
%%%=============================================================================

files(Fs) ->
    Defs = lists:usort(lists:flatten(lists:map(fun file/1, Fs))),
    DefinedIDs = lists:usort([binary_to_list(element(1,D)) || D <- Defs]),
    WantedIDs = lists:usort(wanted_messages()),
    Missing = WantedIDs -- DefinedIDs,
    case Missing of
	[] -> ok;
	_ -> io:format('%% Warning: missing ~p~n', [Missing])
    end,
    Defs.
	    

file(F) ->
    {ok,B} = file:read_file(F),
    hunt_msg_def(B).


hunt_msg_def(<<"\n",B/binary>>) -> some_hope(skip_blanks(B));
hunt_msg_def(<<_, B/binary>>) -> hunt_msg_def(B);
hunt_msg_def(<<>>) -> [].
    
some_hope(<<"byte ", B/binary>>) -> try_message(skip_blanks(B));
some_hope(B) -> hunt_msg_def(B).
    
try_message(B = <<"SSH_MSG_",_/binary>>) ->
    {ID,Rest} = get_id(B),
    case lists:member(binary_to_list(ID), wanted_messages()) of
	true ->
	    {Lines,More} = get_def_lines(skip_blanks(Rest), []),
	    [{ID,lists:reverse(Lines)} | hunt_msg_def(More)];
	false ->
	    hunt_msg_def(Rest)
    end;
try_message(B) -> hunt_msg_def(B).
    

skip_blanks(<<32, B/binary>>) -> skip_blanks(B);
skip_blanks(<< 9, B/binary>>) -> skip_blanks(B);
skip_blanks(B) -> B.

get_def_lines(B0 = <<"\n",B/binary>>, Acc) ->
    {ID,Rest} = get_id(skip_blanks(B)),
    case {size(ID), skip_blanks(Rest)} of
	{0,<<"....",More/binary>>} ->
	    {Text,LineEnd} = get_to_eol(skip_blanks(More)),
	    get_def_lines(LineEnd, [{<<"....">>,Text}|Acc]);
	{0,_} ->
	    {Acc,B0};
	{_,Rest1} -> 
	    {Text,LineEnd} = get_to_eol(Rest1),
	    get_def_lines(LineEnd, [{ID,Text}|Acc])
    end;
get_def_lines(B, Acc) -> 
    {Acc,B}.
    

get_to_eol(B) -> split_binary(B, count_to_eol(B,0)).

count_to_eol(<<"\n",_/binary>>, Acc) -> Acc;
count_to_eol(<<>>, Acc) -> Acc;
count_to_eol(<<_,B/binary>>, Acc) -> count_to_eol(B,Acc+1).
    

get_id(B) -> split_binary(B, count_id_chars(B,0)).
    
count_id_chars(<<C,B/binary>>, Acc) when $A=<C,C=<$Z -> count_id_chars(B,Acc+1);
count_id_chars(<<C,B/binary>>, Acc) when $a=<C,C=<$z -> count_id_chars(B,Acc+1);
count_id_chars(<<C,B/binary>>, Acc) when $0=<C,C=<$9 -> count_id_chars(B,Acc+1);
count_id_chars(<<"_",B/binary>>, Acc)  ->  count_id_chars(B,Acc+1);
count_id_chars(<<"-",B/binary>>, Acc)  ->  count_id_chars(B,Acc+1); %% e.g name-list
count_id_chars(<<"[",B/binary>>, Acc)  ->  count_id_chars(B,Acc+1); %% e.g byte[16]
count_id_chars(<<"]",B/binary>>, Acc)  ->  count_id_chars(B,Acc+1); %% e.g byte[16]
count_id_chars(_, Acc) -> Acc.

wanted_messages() ->
    ["SSH_MSG_CHANNEL_CLOSE",
     "SSH_MSG_CHANNEL_DATA",
     "SSH_MSG_CHANNEL_EOF",
     "SSH_MSG_CHANNEL_EXTENDED_DATA",
     "SSH_MSG_CHANNEL_FAILURE",
     "SSH_MSG_CHANNEL_OPEN",
     "SSH_MSG_CHANNEL_OPEN_CONFIRMATION",
     "SSH_MSG_CHANNEL_OPEN_FAILURE",
     "SSH_MSG_CHANNEL_REQUEST",
     "SSH_MSG_CHANNEL_SUCCESS",
     "SSH_MSG_CHANNEL_WINDOW_ADJUST",
     "SSH_MSG_DEBUG",
     "SSH_MSG_DISCONNECT",
     "SSH_MSG_GLOBAL_REQUEST",
     "SSH_MSG_IGNORE",
     "SSH_MSG_KEXDH_INIT",
     "SSH_MSG_KEXDH_REPLY",
     "SSH_MSG_KEXINIT",
     "SSH_MSG_KEX_DH_GEX_GROUP",
     "SSH_MSG_KEX_DH_GEX_REQUEST",
     "SSH_MSG_KEX_DH_GEX_REQUEST_OLD",
     "SSH_MSG_NEWKEYS",
     "SSH_MSG_REQUEST_FAILURE",
     "SSH_MSG_REQUEST_SUCCESS",
     "SSH_MSG_SERVICE_ACCEPT",
     "SSH_MSG_SERVICE_REQUEST",
     "SSH_MSG_UNIMPLEMENTED",
     "SSH_MSG_USERAUTH_BANNER",
     "SSH_MSG_USERAUTH_FAILURE",
%% hard args    "SSH_MSG_USERAUTH_INFO_REQUEST",
%%     "SSH_MSG_USERAUTH_INFO_RESPONSE",
     "SSH_MSG_USERAUTH_PASSWD_CHANGEREQ",
     "SSH_MSG_USERAUTH_PK_OK",
%%rfc4252 p12 error      "SSH_MSG_USERAUTH_REQUEST",
     "SSH_MSG_USERAUTH_SUCCESS"].

