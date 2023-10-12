%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2022. All Rights Reserved.
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

-module(crypto_ng_api).

-compile(export_all).

-include_lib("common_test/include/ct_property_test.hrl").

-include("crypto_prop_generators.hrl").

%%%================================================================
%%% Properties:

prop__crypto_one_time() ->
    numtests(10000,
             ?FORALL({TextPlain, Cipher, Key, IV, Padding}, ?LET(Ciph,cipher(),
                                                                 {text_plain(), Ciph, key(Ciph), iv(Ciph), padding()}),
                     begin
                         R = equal(TextPlain,
                                   full_blocks(TextPlain, Cipher, Padding),
                                   decrypt_encrypt_one_time(Cipher, Key, IV, TextPlain, Padding)),
                         prt_inf(Cipher, TextPlain, R)
                     end
                    )
            ).

prop__crypto_init_update_final() ->
    numtests(10000,
             ?FORALL({TextPlain, Cipher, Key, IV, Padding}, ?LET(Ciph,cipher(),
                                                                 {text_plain(), Ciph, key(Ciph), iv(Ciph), padding()}),
                     begin
                         R = equal(TextPlain,
                                   full_blocks(TextPlain, Cipher, Padding),
                                   decrypt_encrypt_init_update_final(Cipher, Key, IV, TextPlain, Padding)),
                         prt_inf(Cipher, TextPlain, R)
                     end
                    )
            ).

prt_inf(Cipher, TextPlain, R) ->
    aggregate(ct_property_test:title("text lengths",
                                     ct_property_test:print_frequency_ranges(),
                                     fun ct:pal/2),
              [iolist_size(TextPlain)],
    aggregate(ct_property_test:title("ciphers",
                                     ct_property_test:print_frequency(),
                                     fun ct:pal/2),
              [Cipher],
   R)).

%%%================================================================
%%% Lib

equal(_, _,  correct_exception) -> true;
equal(_, T, T) -> true;
equal(F, Tp, Td) ->
    ct:pal("Full:  ~p~n"
           "Block: ~p~n"
           "Decr:  ~p~n",
           [F, Tp, Td]),
    false.


decrypt_encrypt_one_time(Cipher, Key, IV, TextPlain, Padding) ->
    io:format("~p:~p Cipher: ~p, BlockSize: ~p, Key: ~p, IV: ~p, TextPlain: ~p (~p chunks), Padding: ~p",
              [?MODULE,?LINE, Cipher, block_size(Cipher), size(Key), size(IV), size(iolist_to_binary(TextPlain)),
               num_chunks(TextPlain), Padding]),
    Sblock = block_size(Cipher),
    ExcessBytesLastBlock = size(iolist_to_binary(TextPlain)) rem Sblock,
    PadSize = if
                  (Padding == zero) ; (Padding == random) ->
                      (Sblock - ExcessBytesLastBlock) rem Sblock;
                  true ->
                      0
              end,
    try
        crypto:crypto_one_time(Cipher, Key, IV, TextPlain, [{encrypt,true},{padding,Padding}])
    of
        TextCrypto ->
            io:format("~p:~p PadSize: ~p, TextCrypto: ~p",
                      [?MODULE,?LINE, PadSize, size(TextCrypto)]),
            TextDecryptPadded =
                crypto:crypto_one_time(Cipher, Key, IV, TextCrypto, [{encrypt,false},{padding,Padding}]),
            io:format("~p:~p TextDecryptPadded: ~p",
                      [?MODULE,?LINE, size(TextDecryptPadded)]),
            element(1, split_binary(TextDecryptPadded, size(TextDecryptPadded) - PadSize))
    catch
        error:{error,{"api_ng.c",Line},Msg} when ExcessBytesLastBlock>0,
                                                 Padding == none ->
            case Msg of
                "Padding 'none' but unfilled last block"++_ ->
                    io:format("~p:~p Correct exception: ~p",
                              [?MODULE,?LINE, {error,{"api_ng.c",Line},Msg}]),
                    correct_exception;
                _ ->
                    error({error,{"api_ng.c",Line},Msg})
            end
    end.
    

decrypt_encrypt_init_update_final(Cipher, Key, IV, TextPlain, Padding) when is_binary(TextPlain) ->
    decrypt_encrypt_init_update_final(Cipher, Key, IV, [TextPlain], Padding);

decrypt_encrypt_init_update_final(Cipher, Key, IV, TextPlain, Padding) ->
    io:format("~p:~p Cipher: ~p, BlockSize: ~p, Key: ~p, IV: ~p, TextPlain: ~p (~p chunks), Padding: ~p",
              [?MODULE,?LINE, Cipher, block_size(Cipher), size(Key), size(IV), size(iolist_to_binary(TextPlain)),
               num_chunks(TextPlain), Padding]),
    Sblock = block_size(Cipher),
    ExcessBytesLastBlock = size(iolist_to_binary(TextPlain)) rem Sblock,

    Cenc = crypto:crypto_init(Cipher, Key, IV, [{encrypt,true},{padding,Padding}]),
    TextOut = lists:foldl(fun(TextIn, TextOutAcc) ->
                                  [crypto:crypto_update(Cenc,TextIn) | TextOutAcc]
                          end, [], TextPlain),
    try
        Rf = crypto:crypto_final(Cenc),
        Rps = maps:get(padding_size,crypto:crypto_get_data(Cenc)),
        {Rps,Rf}
    of
        {PadSize0,LastTextOut} ->
            TextCrypto = lists:reverse([LastTextOut|TextOut]),
            io:format("~p:~p PadSize0: ~p, TextCrypto: ~p",
                      [?MODULE,?LINE, PadSize0, size(iolist_to_binary(TextCrypto))]),
            PadSize = case Padding of
                          pkcs_padding -> 0;
                          none -> 0;
                          undefined -> 0;
                          _ -> PadSize0
                      end,
    
            Cdec = crypto:crypto_init(Cipher, Key, IV, [{encrypt,false},{padding,Padding}]),
            TextDec = lists:foldl(fun(TextC, TextDecAcc) ->
                                          [crypto:crypto_update(Cdec,TextC) | TextDecAcc]
                                  end, [], TextCrypto),
            LastDecOut = crypto:crypto_final(Cdec),
            TextDecryptPadded = iolist_to_binary(lists:reverse([LastDecOut|TextDec])),
            io:format("~p:~p TextDecryptPadded: ~p",
                      [?MODULE,?LINE, size(TextDecryptPadded)]),
            element(1, split_binary(TextDecryptPadded, size(TextDecryptPadded) - PadSize))
    catch
        error:{error,{"api_ng.c",Line},Msg} when ExcessBytesLastBlock>0,
                                                 Padding == none ->
            io:format("~p:~p Correct exception: ~p",
                      [?MODULE,?LINE, {error,{"api_ng.c",Line},Msg}]),
            correct_exception
    end.


full_blocks(TextPlain, Cipher, Pad) when Pad == undefined ; Pad == none ->
    TextPlainBin = iolist_to_binary(TextPlain),
    {Head,_Tail} = split_binary(TextPlainBin, (size(TextPlainBin) - num_rest_bytes(TextPlainBin,Cipher))),
    Head;
full_blocks(TextPlain, _Cipher, _) ->
    iolist_to_binary(TextPlain).


num_chunks(B) when is_binary(B) -> 1;
num_chunks(L) when is_list(L) -> length(L).

num_rest_bytes(Bin, Cipher) -> size(Bin) rem block_size(Cipher).

