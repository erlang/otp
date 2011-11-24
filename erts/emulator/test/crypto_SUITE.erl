%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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

-module(crypto_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 t_md5/1,t_md5_update/1,error/1,unaligned_context/1,random_lists/1,
	 misc_errors/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [t_md5, t_md5_update, error, unaligned_context,
     random_lists, misc_errors].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



misc_errors(doc) ->
    ["Test crc32, adler32 and md5 error cases not covered by other tests"];
misc_errors(suite) ->
    [];
misc_errors(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),
    ?line 1 = erlang:adler32([]),
    ?line L = lists:duplicate(600,3),
    ?line 1135871753 = erlang:adler32(L),
    ?line L2 = lists:duplicate(22000,3),
    ?line 1100939744 = erlang:adler32(L2),
    ?line {'EXIT', {badarg,_}} = (catch erlang:adler32(L++[a])),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32(L++[a])),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32([1,2,3|<<25:7>>])),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32([1,2,3|4])),
    ?line Big = 111111111111111111111111111111,
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32(Big,<<"hej">>)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32(25,[1,2,3|4])),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32_combine(Big,3,3)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32_combine(3,Big,3)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:crc32_combine(3,3,Big)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:adler32(Big,<<"hej">>)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:adler32(25,[1,2,3|4])),
    ?line {'EXIT', {badarg,_}} = (catch erlang:adler32_combine(Big,3,3)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:adler32_combine(3,Big,3)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:adler32_combine(3,3,Big)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:md5_update(<<"hej">>,<<"hej">>)),
    ?line {'EXIT', {badarg,_}} = (catch erlang:md5_final(<<"hej">>)),
    ?line test_server:timetrap_cancel(Dog),
    ok.


%%
%% Most of the real code for these test cases are in 
%% the modules crypto_reference and random_iolist.
%%
-define(REF,crypto_reference).

nicesplit(N,L) ->
    nicesplit(N,L,[]).
nicesplit(0,Tail,Acc) ->
    {lists:reverse(Acc),Tail};
nicesplit(_,[],Acc) ->
     {lists:reverse(Acc),[]};
nicesplit(N,[H|Tail],Acc) ->
    nicesplit(N-1,Tail,[H|Acc]).

run_in_para([],_) ->
    true;
run_in_para(FunList,Schedulers) ->
    {ThisTime,NextTime} = nicesplit(Schedulers,FunList),
    case length(ThisTime) of 
	1 ->
	    [{L,Fun}] = ThisTime,
	    try
		Fun()
            catch
		_:Reason ->
		  exit({error_at_line,L,Reason})
	    end;
        _ ->
	    These = [ {L,erlang:spawn_monitor(F)} || {L,F} <- ThisTime ],
	    collect_workers(These)
    end,
    run_in_para(NextTime,Schedulers).

collect_workers([]) ->
    ok;
collect_workers([{L,{Pid,Ref}}|T]) ->
    receive
	{'DOWN',Ref,process,Pid,normal} ->
	    collect_workers(T);
	{'DOWN',Ref,process,Pid,Other} ->
	    exit({error_at_line,L,Other})
    end.

random_lists(doc) ->
    ["Test crc32, adler32 and md5 on a number of pseudo-randomly generated "
     "lists."];
random_lists(suite) ->
    [];
random_lists(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(5)),
    ?line Num = erlang:system_info(schedulers_online),
    ?line B = list_to_binary(
		lists:duplicate(
		  (erlang:system_info(context_reductions)*10) - 50,$!)),
    ?line CRC32_1 = fun(L) -> erlang:crc32(L) end,
    ?line CRC32_2 = fun(L) -> ?REF:crc32(L) end,
    ?line ADLER32_1 = fun(L) -> erlang:adler32(L) end,
    ?line ADLER32_2 = fun(L) -> ?REF:adler32(L) end,
    ?line MD5_1 = fun(L) -> erlang:md5(L) end,
    ?line MD5_2 = fun(L) -> ?REF:md5_final(
			       ?REF:md5_update(?REF:md5_init(),L)) end,
    ?line MD5_3 =  fun(L) -> erlang:md5_final(
			       erlang:md5_update(erlang:md5_init(),L)) end,
    ?line CRC32_1_L = fun(L) -> erlang:crc32([B|L]) end,
    ?line CRC32_2_L = fun(L) -> ?REF:crc32([B|L]) end,
    ?line ADLER32_1_L = fun(L) -> erlang:adler32([B|L]) end,
    ?line ADLER32_2_L = fun(L) -> ?REF:adler32([B|L]) end,
    ?line MD5_1_L = fun(L) -> erlang:md5([B|L]) end,
    ?line MD5_2_L = fun(L) -> ?REF:md5_final(
				 ?REF:md5_update(?REF:md5_init(),[B|L])) end,
    ?line MD5_3_L =  fun(L) -> erlang:md5_final(
				 erlang:md5_update(
				   erlang:md5_init(),[B|L])) end,
    ?line Wlist0 = 
	[{?LINE, fun() -> random_iolist:run(150, CRC32_1, CRC32_2) end},
	 {?LINE, fun() -> random_iolist:run(150, ADLER32_1, ADLER32_2) end},
	 {?LINE, fun() -> random_iolist:run(150,MD5_1,MD5_2) end},
	 {?LINE, fun() -> random_iolist:run(150,MD5_1,MD5_3) end},
	 {?LINE, fun() -> random_iolist:run(150, CRC32_1_L, CRC32_2_L) end},
	 {?LINE, 
	  fun() -> random_iolist:run(150, ADLER32_1_L, ADLER32_2_L) end},
	 {?LINE, fun() -> random_iolist:run(150,MD5_1_L,MD5_2_L) end},
	 {?LINE, fun() -> random_iolist:run(150,MD5_1_L,MD5_3_L) end}],
    ?line run_in_para(Wlist0,Num),
    ?line CRC32_1_2 = fun(L1,L2) -> erlang:crc32([L1,L2]) end,
    ?line CRC32_2_2 = fun(L1,L2) -> erlang:crc32(erlang:crc32(L1),L2) end,
    ?line CRC32_3_2 = fun(L1,L2) -> erlang:crc32_combine(
				      erlang:crc32(L1),
				      erlang:crc32(L2),
				      erlang:iolist_size(L2)) 
		      end,
    ?line ADLER32_1_2 = fun(L1,L2) -> erlang:adler32([L1,L2]) end,
    ?line ADLER32_2_2 = fun(L1,L2) -> erlang:adler32(
					erlang:adler32(L1),L2) end,
    ?line ADLER32_3_2 = fun(L1,L2) -> erlang:adler32_combine(
				      erlang:adler32(L1),
				      erlang:adler32(L2),
				      erlang:iolist_size(L2)) 
			end,
    ?line MD5_1_2 = fun(L1,L2) -> erlang:md5([L1,L2]) end,
    ?line MD5_2_2 = fun(L1,L2) -> 
			    erlang:md5_final(
			      erlang:md5_update(
				erlang:md5_update(
				  erlang:md5_init(),
				  L1),
				L2)) 
		    end,
    ?line CRC32_1_L_2 = fun(L1,L2) -> erlang:crc32([[B|L1],[B|L2]]) end,
    ?line CRC32_2_L_2 = fun(L1,L2) -> erlang:crc32(
					erlang:crc32([B|L1]),[B|L2]) end,
    ?line CRC32_3_L_2 = fun(L1,L2) -> erlang:crc32_combine(
					erlang:crc32([B|L1]),
					erlang:crc32([B|L2]),
					erlang:iolist_size([B|L2])) 
			end,
    ?line ADLER32_1_L_2 = fun(L1,L2) -> erlang:adler32([[B|L1],[B|L2]]) end,
    ?line ADLER32_2_L_2 = fun(L1,L2) -> erlang:adler32(
					  erlang:adler32([B|L1]),
					  [B|L2]) 
			  end,
    ?line ADLER32_3_L_2 = fun(L1,L2) -> erlang:adler32_combine(
					  erlang:adler32([B|L1]),
					  erlang:adler32([B|L2]),
					  erlang:iolist_size([B|L2])) 
			  end,
    ?line MD5_1_L_2 = fun(L1,L2) -> erlang:md5([[B|L1],[B|L2]]) end,
    ?line MD5_2_L_2 = fun(L1,L2) -> 
			      erlang:md5_final(
				erlang:md5_update(
				  erlang:md5_update(
				    erlang:md5_init(),
				    [B|L1]),
				  [B|L2])) 
		      end,
    ?line Wlist1 = 
	[{?LINE, fun() -> random_iolist:run2(150,CRC32_1_2,CRC32_2_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,CRC32_1_2,CRC32_3_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,ADLER32_1_2,ADLER32_2_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,ADLER32_1_2,ADLER32_3_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,MD5_1_2,MD5_2_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,CRC32_1_L_2,CRC32_2_L_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,CRC32_1_L_2,CRC32_3_L_2) end},
	 {?LINE, 
	  fun() -> random_iolist:run2(150,ADLER32_1_L_2,ADLER32_2_L_2) end},
	 {?LINE, 
	  fun() -> random_iolist:run2(150,ADLER32_1_L_2,ADLER32_3_L_2) end},
	 {?LINE, fun() -> random_iolist:run2(150,MD5_1_L_2,MD5_2_L_2) end}],
    ?line run_in_para(Wlist1,Num),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%%
%%
t_md5(doc) ->
    ["Generate MD5 message digests and check the result. Examples are "
     "from RFC-1321."];
t_md5(Config) when is_list(Config) ->
    ?line t_md5_test("", "d41d8cd98f00b204e9800998ecf8427e"),
    ?line t_md5_test("a", "0cc175b9c0f1b6a831c399e269772661"),
    ?line t_md5_test("abc", "900150983cd24fb0d6963f7d28e17f72"),
    ?line t_md5_test(["message ","digest"], "f96b697d7cb7938d525a2f31aaf161d0"),
    ?line t_md5_test(["message ",unaligned_sub_bin(<<"digest">>)],
		   "f96b697d7cb7938d525a2f31aaf161d0"),
    ?line t_md5_test("abcdefghijklmnopqrstuvwxyz",
		   "c3fcd3d76192e4007dfb496cca67e13b"),
    ?line t_md5_test("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		       "0123456789",
	    "d174ab98d277d9f5a5611c2c9f419d9f"),
    ?line t_md5_test("12345678901234567890123456789012345678901234567890"
		       "123456789012345678901234567890",
	    "57edf4a22be3c955ac49da2e2107b67a"),
    ok.

%%
%%
t_md5_update(doc) ->
    ["Generate MD5 message using md5_init, md5_update, and md5_final, and"
     "check the result. Examples are from RFC-1321."];
t_md5_update(Config) when is_list(Config) ->
    ?line t_md5_update_1(fun(Str) -> Str end),
    ?line t_md5_update_1(fun(Str) -> list_to_binary(Str) end),
    ?line t_md5_update_1(fun(Str) -> unaligned_sub_bin(list_to_binary(Str)) end),
    ok.

t_md5_update_1(Tr) when is_function(Tr, 1) ->
    Ctx = erlang:md5_init(),
    Ctx1 = erlang:md5_update(Ctx, Tr("ABCDEFGHIJKLMNOPQRSTUVWXYZ")),
    Ctx2 = erlang:md5_update(Ctx1, Tr("abcdefghijklmnopqrstuvwxyz"
				      "0123456789")),
    m(erlang:md5_final(Ctx2),
      hexstr2bin("d174ab98d277d9f5a5611c2c9f419d9f")),
    ok.

%%
%%
error(Config) when is_list(Config) ->
    ?line {'EXIT',{badarg,_}} = (catch erlang:md5(bit_sized_binary(<<"abc">>))),
    ?line Ctx0 = erlang:md5_init(),
    ?line {'EXIT',{badarg,_}} =
	(catch erlang:md5_update(Ctx0, bit_sized_binary(<<"abcfjldjd">>))),
    ?line {'EXIT',{badarg,_}} =
	(catch erlang:md5_update(Ctx0, ["something",bit_sized_binary(<<"abcfjldjd">>)])),
    ?line {'EXIT',{badarg,_}} =
	(catch erlang:md5_update(bit_sized_binary(Ctx0), "something")),
    ?line {'EXIT',{badarg,_}} = (catch erlang:md5_final(bit_sized_binary(Ctx0))),
    ?line m(erlang:md5_final(Ctx0), hexstr2bin("d41d8cd98f00b204e9800998ecf8427e")),
    ok.


%%
%%
unaligned_context(Config) when is_list(Config) ->
    ?line Ctx0 = erlang:md5_init(),
    ?line Ctx1 = erlang:md5_update(unaligned_sub_bin(Ctx0), "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    ?line Ctx = erlang:md5_update(unaligned_sub_bin(Ctx1),
				  "abcdefghijklmnopqrstuvwxyz0123456789"),
    ?line m(erlang:md5_final(unaligned_sub_bin(Ctx)),
	    hexstr2bin("d174ab98d277d9f5a5611c2c9f419d9f")),
    ok.

%%
%% Help functions
%%

t_md5_test(Str, ResultStr) ->
    ResultBin = hexstr2bin(ResultStr),
    m(erlang:md5(Str), ResultBin),
    Bin = list_to_binary(Str),
    m(erlang:md5(Bin), ResultBin),
    UnalignedSubBin = unaligned_sub_bin(Bin),
    m(erlang:md5(UnalignedSubBin), ResultBin).

m(X, X) -> true.

hexstr2bin(S) ->
    list_to_binary(hexstr2list(S)).

hexstr2list([X,Y|T]) ->
    [mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
    [].

mkint(C) when $0 =< C, C =< $9 ->
    C - $0;
mkint(C) when $A =< C, C =< $F ->
    C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
    C - $a + 10.

unaligned_sub_bin(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

%% Add 1 bit to the size of the binary.
bit_sized_binary(Bin0) ->
    Bin = <<Bin0/binary,1:1>>,
    BitSize = bit_size(Bin),
    BitSize = 8*size(Bin) + 1,
    Bin.

id(I) -> I.
    

