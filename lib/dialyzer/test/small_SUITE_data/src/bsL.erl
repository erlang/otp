-module(bsL).

-export([t/0]).

%% Found in lib/observer/test/crashdump_helper.erl.

t() ->
    Size = 60,
    <<H:16/unit:8>> = erlang:md5(<<Size:32>>),
    true = H < 20,
    true = H > 2,
    Data = ((H bsl (8*150)) div (H+7919)),
    <<Data:Size/unit:8>>.
