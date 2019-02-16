%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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
-module(crypto_bench_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("common_test/include/ct.hrl").

suite() -> [%%{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]},
	    {timetrap,{minutes,2}}
	   ].

all() ->
    [
     {group, textblock_256}
    ].

groups() ->
    [
     {textblock_256, [], [
                          {group, ciphers_128},
                          {group, ciphers_256}
                         ]},

     {ciphers_128, [{repeat, 5}], [
                                   block,
                                   stream
                                  ]},

     {ciphers_256, [{repeat, 5}], [
                                   block,
                                   stream,
                                   chacha
                                  ]}
    ].

%%%----------------------------------------------------------------
%%%
init_per_suite(Config0) ->
    try crypto:start() of
        _ ->
            [{_,_,Info}] = crypto:info_lib(),
            ct:comment("~s",[Info]),
            ct:pal("Crypto version: ~p~n~n~p",[Info,crypto:supports()]),
            Config1 = measure_openssl_aes_cbc([128,256], Config0),
            calibrate([{sec_goal,10} | Config1])

    catch _:_ ->
	    {fail, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

%%%----------------------------------------------------------------
%%%
init_per_group(Group, Config) ->
    case atom_to_list(Group) of
        "ciphers_"++KeySizeStr ->
            KeySize = list_to_integer(KeySizeStr),
            [{key_size,KeySize} | Config];

        "textblock_"++BlockSizeStr ->
            BlockSize = list_to_integer(BlockSizeStr),
            [{block_size,BlockSize} | Config];

        _ ->
            Config
    end.

end_per_group(_Group, Config) ->
    Config.


measure_openssl_aes_cbc(KeySizes, Config) ->
    BLno_acc = [baseline(aes_cbc, KeySize, false) || KeySize <- KeySizes],
    ct:pal("Non-accelerated baseline encryption time [µs/block]:~n~p", [BLno_acc]),
    BLacc = [baseline(aes_cbc, KeySize, true) || KeySize <- KeySizes],
    ct:pal("Possibly accelerated baseline encryption time [µs/block]:~n~p", [BLacc]),
    [{acc,BLacc},
     {no_acc,BLno_acc} | Config].

calibrate(Config) ->
    Secs = proplists:get_value(sec_goal, Config, 10),
    {_,Empty} = data(empty, 0, 0),
    {Ne,Te} = run1(Secs*3000, Empty),
    report(["Overhead"], Te/Ne),
    [{overhead,Te/Ne} | Config].

%%%================================================================
%%%
%%%
block(Config) ->
    run_cryptos([aes_cbc, aes_gcm, aes_ccm],
                Config).

stream(Config) ->
    run_cryptos([aes_ctr],
                Config).
    
chacha(Config) ->
    run_cryptos([chacha20, chacha20_poly1305],
                Config).
    

%%%================================================================
%%%
%%%

run_cryptos(Cryptos, Config) ->
    KeySize = proplists:get_value(key_size, Config),
    BlockSize = proplists:get_value(block_size, Config),
    MilliSecGoal = 1000*proplists:get_value(sec_goal,Config),
    OverHead = proplists:get_value(overhead, Config, 0),
    [try
         TimePerOpBrutto = run(Crypto,KeySize,BlockSize,MilliSecGoal),
         %% ct:pal("Brutto: ~p Overhead: ~p (~.2f %) Netto: ~p",
         %%        [TimePerOpBrutto, OverHead, 100*OverHead/TimePerOpBrutto,TimePerOpBrutto - OverHead]),
         TimePerOpBrutto - OverHead
     of
         TimePerOp -> % µs
             %% First, Report speed of encrypting blocks of 1000. [blocks/sec]
             ReportUnit = 1000,
             Label = [fmt(Crypto)," key:",KeySize," block:",BlockSize],
             report(Label,
                    (BlockSize/ReportUnit)*1000000/TimePerOp
                   ),

             EffCrypto = case Crypto of
                             X -> X
                         end,
             %% Percent of accelerated speed
             case find_value([acc,{EffCrypto,KeySize},BlockSize], Config) of
                 undefined ->
                     ok;
                 TimePerOpBaseAcc ->
                     report(["Percent of acc OpenSSL "|Label],
                            100*TimePerOpBaseAcc/TimePerOp % Percent of base *speed*
                           )
             end,

             %% Percent of non-accelerated speed
             case find_value([no_acc,{EffCrypto,KeySize},BlockSize], Config) of
                 undefined ->
                     ok;
                 TimePerOpBaseNoAcc ->
                     report(["Percent of noacc OpenSSL "|Label],
                            100*TimePerOpBaseNoAcc/TimePerOp % Percent of base *speed*
                           )
             end
     catch
         _:_ ->
             ct:pal("~p unsupported",[{Crypto,KeySize,BlockSize}])
     end
     || Crypto <- Cryptos,
        supported(Crypto)
    ].


run(Crypto, KeySize, BlockSize, MilliSecGoal) ->
    {_Type, Funs} = data(Crypto, KeySize, BlockSize),
    {Nc,Tc} = run1(MilliSecGoal, Funs),
    Tc/Nc.

fmt(X) -> X.


find_value(KeyPath, PropList, Default) ->
    try find_value(KeyPath, PropList)
    of
        undefined -> Default
    catch
        error:function_clause -> Default
    end.

find_value(KeyPath, PropList) ->
    lists:foldl(fun(K, L) when is_list(L) -> proplists:get_value(K,L);
                   (_, _) -> undefined
                end, PropList, KeyPath).

%%%================================================================
%%%
%%%
funs({block, {Type, Key, IV, Block}}) ->
    {fun() -> ok end,
     fun(_) -> crypto:block_encrypt(Type, Key, IV, Block) end,
     fun(_) -> ok end};

funs({stream, {Type, Key, IV, Block}}) ->
    {fun() -> {crypto:stream_init(Type, Key, IV),ok} end,
     fun({Ctx,_}) -> crypto:stream_encrypt(Ctx, Block) end,
     fun(_) -> ok end}.


data(aes_cbc, KeySize, BlockSize) ->
    Type = case KeySize of
               128 -> aes_cbc128;
               256 -> aes_cbc256
           end,
    Key = mk_bin(KeySize div 8),
    IV = mk_bin(16),
    Block = mk_bin(BlockSize),
    {Type, funs({block, {Type, Key, IV, Block}})};

data(aes_gcm, KeySize, BlockSize) ->
    Type = aes_gcm,
    Key = mk_bin(KeySize div 8),
    IV = mk_bin(12),
    Block = mk_bin(BlockSize),
    AAD = <<01,02,03,04>>,
    {Type, funs({block, {Type, Key, IV, {AAD,Block,16}}})};

data(aes_ccm, KeySize, BlockSize) ->
    Type = aes_ccm,
    Key = mk_bin(KeySize div 8),
    IV = mk_bin(12),
    Block = mk_bin(BlockSize),
    AAD = <<01,02,03,04>>,
    {Type, funs({block, {Type, Key, IV, {AAD,Block,12}}})};

data(aes_ctr, KeySize, BlockSize) ->
    Type = aes_ctr,
    Key = mk_bin(KeySize div 8),
    IV = mk_bin(16),
    Block = mk_bin(BlockSize),
    {Type, funs({stream, {Type, Key, IV, Block}})};

data(chacha20_poly1305, 256=KeySize, BlockSize) ->
    Type = chacha20_poly1305,
    Key = mk_bin(KeySize div 8),
    IV = mk_bin(16),
    AAD = <<01,02,03,04>>,
    Block = mk_bin(BlockSize),
    {Type, funs({block, {Type, Key, IV, {AAD,Block}}})};

data(chacha20, 256=KeySize, BlockSize) ->
    Type = chacha20,
    Key = mk_bin(KeySize div 8),
    IV = mk_bin(16),
    Block = mk_bin(BlockSize),
    {Type, funs({stream, {Type, Key, IV, Block}})};

data(empty, 0, 0) ->
    {undefined,
     {fun() -> ok end,
      fun(X) -> X end,
      fun(_) -> ok end}}.

%%%================================================================
%%%
%%%
run1(MilliSecGoal, Funs) ->
    Parent = self(),
    Pid = spawn(fun() ->
                        {Fi,Fu,Ff} = Funs,
                        Ctx0 = Fi(),
                        erlang:garbage_collect(),
                        T0 = start_time(),
                        {N,Ctx} = loop(Fu, Ctx0, 0),
                        T = elapsed_time(T0),
                        Ff(Ctx),
                        Parent ! {result,N,microseconds(T)}
                end),
    Pid ! go,
    receive
    after MilliSecGoal ->
            Pid ! stop
    end,
    receive
        {result,N,MicroSecs} ->
            {N,MicroSecs}
    end.


loop(F, Ctx, N) ->
    receive
        stop ->
            {N, Ctx}
    after 0 ->
            loop(F, F(Ctx), N+1)
    end.

%%%----------------------------------------------------------------
report(LabelList, Value) ->
    Label = report_chars(lists:concat(LabelList)),
    ct:pal("ct_event:notify ~p: ~p", [Label, Value]),
    ct_event:notify(
      #event{name = benchmark_data,
             data = [{name, Label},
                     {value,Value}]}).
    
report_chars(Cs) ->
    [case C of
         $- -> $_;
         _ -> C
     end || C <- Cs].

%%%----------------------------------------------------------------
supported(Algorithm) ->
    lists:member(Algorithm,
                 [A || {_,As} <- crypto:supports(), A <- As]
                ).

%%%----------------------------------------------------------------
start_time() ->
    erlang:system_time().

elapsed_time(StartTime) ->
    erlang:system_time() - StartTime.

microseconds(Time) ->
    erlang:convert_time_unit(Time, native, microsecond).

%%%----------------------------------------------------------------

%% Example output:
%% +DT:aes-128-cbc:3:16
%% +R:135704772:aes-128-cbc:2.980000
%% +DT:aes-128-cbc:3:64
%% +R:36835089:aes-128-cbc:3.000000
%% +DT:aes-128-cbc:3:256
%% +R:9398616:aes-128-cbc:3.000000
%% +DT:aes-128-cbc:3:1024
%% +R:2355683:aes-128-cbc:2.990000
%% +DT:aes-128-cbc:3:8192
%% +R:294508:aes-128-cbc:2.990000
%% +H:16:64:256:1024:8192
%% +F:22:aes-128-cbc:728616225.50:785815232.00:802015232.00:806762338.46:806892821.40

baseline(Crypto, KeySize, EVP) ->
    Spec=
        case {Crypto,KeySize} of
            {aes_cbc, 128} -> "aes-128-cbc";
            {aes_cbc, 256} -> "aes-256-cbc"
        end,
    {{Crypto,KeySize}, baseline(Spec, EVP)}.

baseline(Spec, EVP) ->
    Cmd =
        case EVP of
            true -> "openssl speed -mr -evp " ++ Spec;
            false-> "openssl speed -mr      " ++ Spec
        end,
    get_base_values(string:tokens(os:cmd(Cmd),"\n"), Spec, []).


get_base_values(["+DT:"++Sdt,
                 "+R:"++Sr
                 |T], Crypto, Acc) ->
    [Crypto0,_GoalSecs0,BlockSize0] = string:tokens(Sdt, ":"),
    [Nblocks0,Crypto0,RealSecs0] = string:tokens(Sr, ":"),
    Crypto = fix_possible_space_bug(Crypto0),
    RealSecs = list_to_float(RealSecs0),
    BlockSize = list_to_integer(BlockSize0),
    Nblocks = list_to_integer(Nblocks0),
    get_base_values(T, Crypto, [{BlockSize, 1000000*RealSecs/Nblocks} | Acc]);

get_base_values([_|T], Crypto, Acc) ->
    get_base_values(T, Crypto, Acc);

get_base_values([], _, Acc) ->
    lists:sort(Acc).
    
fix_possible_space_bug(S) -> lists:concat(lists:join("-",string:tokens(S,"- "))).

%%%----------------------------------------------------------------
mk_bin(Size) when Size =< 256 ->
    list_to_binary(lists:seq(0,Size-1));

mk_bin(Size) when 1024 =< Size ->
    B = mk_bin(Size div 4),
    Brest = mk_bin(Size rem 4),
    <<B/binary, B/binary, B/binary, B/binary, Brest/binary>>;

mk_bin(Size) when 256 < Size ->
    B = mk_bin(Size div 2),
    Brest = mk_bin(Size rem 2),
    <<B/binary, B/binary, Brest/binary>>.

