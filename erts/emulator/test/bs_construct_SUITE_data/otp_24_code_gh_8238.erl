-module(otp_24_code_gh_8238).
-export([?MODULE/0]).

%% Produce otp_24_code_gh_8238.S using Erlang/OTP 24 like this:
%%     erlc -S +no_copt +no_ssa_opt otp_24_code_gh_8238.erl

?MODULE() ->
    Bin = integer_to_binary(1000),
    io:format("~p\n", [<<"Example: ", Bin/binary>>]).

