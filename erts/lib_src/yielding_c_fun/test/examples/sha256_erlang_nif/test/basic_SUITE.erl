-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test1/1, test2/1]).

all() ->
    [test1, test2].

bin_to_hex_string(Bin)->
    lists:flatten(io_lib:format("~s", [lists:flatten([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bin ])])).

foreach_up_to_helper(UpTo, UpTo, Fun) ->
    ok;
foreach_up_to_helper(UpTo, Current, Fun) ->
    Fun(Current),
    foreach_up_to_helper(UpTo, Current + 1, Fun).

foreach_up_to(UpTo, Fun) ->
    foreach_up_to_helper(UpTo, 1, Fun).

test1(_Config) ->
    foreach_up_to(15, fun(V) ->
                              In = erlang:list_to_binary(lists:flatten(lists:duplicate(1024 bsl V, "h"))),
                              {CryptoHashTime, Expect} = timer:tc(crypto, hash, [sha256,In]),
                              {MyTime, Expect} = timer:tc(sha256_nif, sha256, [In]),
                              io:format("Size: ~p My Time: ~p Crypto Hash Time: ~p", [1024 bsl V, MyTime/1000000, CryptoHashTime/1000000])
                       end),
    ok.

test2(_Config) ->
    Expect = crypto:hash(sha256, "hej"),
    Expect = sha256_nif:sha256(<<"hej">>).
