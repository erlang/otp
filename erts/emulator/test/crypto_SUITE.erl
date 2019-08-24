%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(crypto_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
         t_md5/1,t_md5_update/1,error/1,unaligned_context/1,random_lists/1,
         misc_errors/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [t_md5, t_md5_update, error, unaligned_context,
     random_lists, misc_errors].

%% Test crc32, adler32 and md5 error cases not covered by other tests"
misc_errors(Config) when is_list(Config) ->
    ct:timetrap({minutes, 2}),
    1 = erlang:adler32([]),
    L = lists:duplicate(600,3),
    1135871753 = erlang:adler32(L),
    L2 = lists:duplicate(22000,3),
    1100939744 = erlang:adler32(L2),
    {'EXIT', {badarg,_}} = (catch erlang:adler32(L++[a])),
    {'EXIT', {badarg,_}} = (catch erlang:crc32(L++[a])),
    {'EXIT', {badarg,_}} = (catch erlang:crc32([1,2,3|<<25:7>>])),
    {'EXIT', {badarg,_}} = (catch erlang:crc32([1,2,3|4])),
    Big = 111111111111111111111111111111,
    {'EXIT', {badarg,_}} = (catch erlang:crc32(Big,<<"hej">>)),
    {'EXIT', {badarg,_}} = (catch erlang:crc32(25,[1,2,3|4])),
    {'EXIT', {badarg,_}} = (catch erlang:crc32_combine(Big,3,3)),
    {'EXIT', {badarg,_}} = (catch erlang:crc32_combine(3,Big,3)),
    {'EXIT', {badarg,_}} = (catch erlang:crc32_combine(3,3,Big)),
    {'EXIT', {badarg,_}} = (catch erlang:adler32(Big,<<"hej">>)),
    {'EXIT', {badarg,_}} = (catch erlang:adler32(25,[1,2,3|4])),
    {'EXIT', {badarg,_}} = (catch erlang:adler32_combine(Big,3,3)),
    {'EXIT', {badarg,_}} = (catch erlang:adler32_combine(3,Big,3)),
    {'EXIT', {badarg,_}} = (catch erlang:adler32_combine(3,3,Big)),
    {'EXIT', {badarg,_}} = (catch erlang:md5_update(<<"hej">>,<<"hej">>)),
    {'EXIT', {badarg,_}} = (catch erlang:md5_final(<<"hej">>)),
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

%% Test crc32, adler32 and md5 on a number of pseudo-randomly generated lists.
random_lists(Config) when is_list(Config) ->
    ct:timetrap({minutes, 5}),
    Num = erlang:system_info(schedulers_online),
    B = list_to_binary(
          lists:duplicate(
            (erlang:system_info(context_reductions)*10) - 50,$!)),
    CRC32_1 = fun(L) -> erlang:crc32(L) end,
    CRC32_2 = fun(L) -> ?REF:crc32(L) end,
    ADLER32_1 = fun(L) -> erlang:adler32(L) end,
    ADLER32_2 = fun(L) -> ?REF:adler32(L) end,
    MD5_1 = fun(L) -> erlang:md5(L) end,
    MD5_2 = fun(L) -> ?REF:md5_final(
                         ?REF:md5_update(?REF:md5_init(),L)) end,
    MD5_3 =  fun(L) -> erlang:md5_final(
                         erlang:md5_update(erlang:md5_init(),L)) end,
    CRC32_1_L = fun(L) -> erlang:crc32([B|L]) end,
    CRC32_2_L = fun(L) -> ?REF:crc32([B|L]) end,
    ADLER32_1_L = fun(L) -> erlang:adler32([B|L]) end,
    ADLER32_2_L = fun(L) -> ?REF:adler32([B|L]) end,
    MD5_1_L = fun(L) -> erlang:md5([B|L]) end,
    MD5_2_L = fun(L) -> ?REF:md5_final(
                           ?REF:md5_update(?REF:md5_init(),[B|L])) end,
    MD5_3_L =  fun(L) -> erlang:md5_final(
                           erlang:md5_update(
                             erlang:md5_init(),[B|L])) end,
    Wlist0 = 
    [{?LINE, fun() -> random_iolist:run(150, CRC32_1, CRC32_2) end},
     {?LINE, fun() -> random_iolist:run(150, ADLER32_1, ADLER32_2) end},
     {?LINE, fun() -> random_iolist:run(150,MD5_1,MD5_2) end},
     {?LINE, fun() -> random_iolist:run(150,MD5_1,MD5_3) end},
     {?LINE, fun() -> random_iolist:run(150, CRC32_1_L, CRC32_2_L) end},
     {?LINE, 
      fun() -> random_iolist:run(150, ADLER32_1_L, ADLER32_2_L) end},
     {?LINE, fun() -> random_iolist:run(150,MD5_1_L,MD5_2_L) end},
     {?LINE, fun() -> random_iolist:run(150,MD5_1_L,MD5_3_L) end}],
    run_in_para(Wlist0,Num),
    CRC32_1_2 = fun(L1,L2) -> erlang:crc32([L1,L2]) end,
    CRC32_2_2 = fun(L1,L2) -> erlang:crc32(erlang:crc32(L1),L2) end,
    CRC32_3_2 = fun(L1,L2) -> erlang:crc32_combine(
                                erlang:crc32(L1),
                                erlang:crc32(L2),
                                erlang:iolist_size(L2)) 
                end,
    ADLER32_1_2 = fun(L1,L2) -> erlang:adler32([L1,L2]) end,
    ADLER32_2_2 = fun(L1,L2) -> erlang:adler32(
                                  erlang:adler32(L1),L2) end,
    ADLER32_3_2 = fun(L1,L2) -> erlang:adler32_combine(
                                  erlang:adler32(L1),
                                  erlang:adler32(L2),
                                  erlang:iolist_size(L2)) 
                  end,
    MD5_1_2 = fun(L1,L2) -> erlang:md5([L1,L2]) end,
    MD5_2_2 = fun(L1,L2) -> 
                      erlang:md5_final(
                        erlang:md5_update(
                          erlang:md5_update(
                            erlang:md5_init(),
                            L1),
                          L2)) 
              end,
    CRC32_1_L_2 = fun(L1,L2) -> erlang:crc32([[B|L1],[B|L2]]) end,
    CRC32_2_L_2 = fun(L1,L2) -> erlang:crc32(
                                  erlang:crc32([B|L1]),[B|L2]) end,
    CRC32_3_L_2 = fun(L1,L2) -> erlang:crc32_combine(
                                  erlang:crc32([B|L1]),
                                  erlang:crc32([B|L2]),
                                  erlang:iolist_size([B|L2])) 
                  end,
    ADLER32_1_L_2 = fun(L1,L2) -> erlang:adler32([[B|L1],[B|L2]]) end,
    ADLER32_2_L_2 = fun(L1,L2) -> erlang:adler32(
                                    erlang:adler32([B|L1]),
                                    [B|L2]) 
                    end,
    ADLER32_3_L_2 = fun(L1,L2) -> erlang:adler32_combine(
                                    erlang:adler32([B|L1]),
                                    erlang:adler32([B|L2]),
                                    erlang:iolist_size([B|L2])) 
                    end,
    MD5_1_L_2 = fun(L1,L2) -> erlang:md5([[B|L1],[B|L2]]) end,
    MD5_2_L_2 = fun(L1,L2) -> 
                        erlang:md5_final(
                          erlang:md5_update(
                            erlang:md5_update(
                              erlang:md5_init(),
                              [B|L1]),
                            [B|L2])) 
                end,
    Wlist1 = 
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
    run_in_para(Wlist1,Num),
    ok.

%% Generate MD5 message digests and check the result. Examples are from RFC-1321.
t_md5(Config) when is_list(Config) ->
    t_md5_test("", "d41d8cd98f00b204e9800998ecf8427e"),
    t_md5_test("a", "0cc175b9c0f1b6a831c399e269772661"),
    t_md5_test("abc", "900150983cd24fb0d6963f7d28e17f72"),
    t_md5_test(["message ","digest"], "f96b697d7cb7938d525a2f31aaf161d0"),
    t_md5_test(["message ",unaligned_sub_bin(<<"digest">>)],
               "f96b697d7cb7938d525a2f31aaf161d0"),
    t_md5_test("abcdefghijklmnopqrstuvwxyz",
               "c3fcd3d76192e4007dfb496cca67e13b"),
    t_md5_test("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
               "0123456789",
               "d174ab98d277d9f5a5611c2c9f419d9f"),
    t_md5_test("12345678901234567890123456789012345678901234567890"
               "123456789012345678901234567890",
               "57edf4a22be3c955ac49da2e2107b67a"),
    ok.

%% Generate MD5 message using md5_init, md5_update, and md5_final, and
%% check the result. Examples are from RFC-1321.
t_md5_update(Config) when is_list(Config) ->
    t_md5_update_1(fun(Str) -> Str end),
    t_md5_update_1(fun(Str) -> list_to_binary(Str) end),
    t_md5_update_1(fun(Str) -> unaligned_sub_bin(list_to_binary(Str)) end),
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
    {'EXIT',{badarg,_}} = (catch erlang:md5(bit_sized_binary(<<"abc">>))),
    Ctx0 = erlang:md5_init(),
    {'EXIT',{badarg,_}} =
    (catch erlang:md5_update(Ctx0, bit_sized_binary(<<"abcfjldjd">>))),
    {'EXIT',{badarg,_}} =
    (catch erlang:md5_update(Ctx0, ["something",bit_sized_binary(<<"abcfjldjd">>)])),
    {'EXIT',{badarg,_}} =
    (catch erlang:md5_update(bit_sized_binary(Ctx0), "something")),
    {'EXIT',{badarg,_}} = (catch erlang:md5_final(bit_sized_binary(Ctx0))),
    m(erlang:md5_final(Ctx0), hexstr2bin("d41d8cd98f00b204e9800998ecf8427e")),
    ok.


%%
%%
unaligned_context(Config) when is_list(Config) ->
    Ctx0 = erlang:md5_init(),
    Ctx1 = erlang:md5_update(unaligned_sub_bin(Ctx0), "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    Ctx = erlang:md5_update(unaligned_sub_bin(Ctx1),
                            "abcdefghijklmnopqrstuvwxyz0123456789"),
    m(erlang:md5_final(unaligned_sub_bin(Ctx)),
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
