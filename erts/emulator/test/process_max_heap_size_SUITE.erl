%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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

-module(process_max_heap_size_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
         immediate_termination/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() ->
    [immediate_termination].

groups() ->
    [].

init_per_suite(Config) ->
    _ = id(Config),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    %% Restore max_heap_size to default value.
    erlang:system_flag(max_heap_size,
                       #{size => 0,
                         kill => true,
                         include_shared_binaries => false,
                         error_logger => true}),
    ok.

%% Make sure that when maximum allowed heap size is exceeded, the
%% process will actually terminate.
%%
%% Despite the timetrap and limit of number of iterations, bugs
%% provoked by the test case can cause the runtime system to hang in
%% this test case.
immediate_termination(_Config) ->
    ct:timetrap({minutes,1}),
    do_more_spawn_opt_max_heap_size(fun(F) -> F end),

    %% save_calls will cause all calls to BIFs to go through their
    %% exports entries, which will test a different code path.
    Wrapper1 = fun(F) ->
                       fun() ->
                               process_flag(save_calls, 10),
                               F()
                       end
               end,
    do_more_spawn_opt_max_heap_size(Wrapper1),

    %% Make sure that the kill signal can't be caught.
    Wrapper2 = fun(F) ->
                       fun() ->
                               process_flag(save_calls, 10),
                               catch F()
                       end
               end,
    do_more_spawn_opt_max_heap_size(Wrapper2),

    ok.

do_more_spawn_opt_max_heap_size(Wrap) ->
    Funs = [fun build_and_bif/0,
            fun build_bin_and_bif/0,
            fun build_bin_on_heap_known_size/0,
            fun build_bin_on_heap_unknown_small_size/0,
            fun build_bin_on_heap_unknown_size/0,
            fun build_bin_append_known_binary/0,
            fun build_bin_append_unknown_binary/0,
            fun build_bin_append_unknown_binary_guard/0,
            fun build_bin_append_writable_binary_extra/0,
            fun build_bin_append_unknown_binary_extra/0,
            fun build_and_recv_timeout/0,
            fun build_and_recv_msg/0,
            fun bif_and_recv_timeout/0,
            fun bif_and_recv_msg/0,
            fun bs_match_integer_known_size/0,
            fun bs_match_integer_unknown_size/0
           ],
    _ = [begin
             {name,Name} = erlang:fun_info(F0, name),
             F = Wrap(F0),
             {Pid,Ref} = spawn_opt(F, [{max_heap_size,
                                        #{size => 233, kill => true,
                                          error_logger => false}},
                                       monitor]),
             io:format("~p ~p ~p/0\n", [Pid,F,Name]),
             receive
                 {'DOWN',Ref,process,Pid,Reason} ->
                     killed = Reason
             end
         end || F0 <- Funs],
    ok.

%% This number should be greater than the default heap size.
-define(MANY_ITERATIONS, 10_000).

build_and_bif() ->
    build_and_bif(?MANY_ITERATIONS, []).

build_and_bif(0, Acc0) ->
    Acc0;
build_and_bif(N, Acc0) ->
    Acc = [0|Acc0],
    _ = erlang:crc32(Acc),
    build_and_bif(N-1, Acc).

build_bin_on_heap_known_size() ->
    Data = id(1),
    A = erlang:make_tuple(233 - 3 * 8, a),

    %% Each created binary will be a heap binary.
    B = <<Data:64/unit:8>>,
    C = <<Data:64/unit:8>>,
    D = <<Data:64/unit:8>>,
    E = <<Data:64/unit:8>>,

    %% This code should never be reached.
    exit({A,B,C,D,E}).

build_bin_on_heap_unknown_small_size() ->
    Size = id(64),

    %% Make it known to the JIT that the maximum size is 64 bytes,
    %% so that the JIT will know that each of binaries below are
    %% heap binaries.
    true = is_integer(Size) andalso Size =< 64,

    A = erlang:make_tuple(233 - 3 * 8, a),
    B = <<0:Size/unit:8>>,
    C = <<0:Size/unit:8>>,
    D = <<0:Size/unit:8>>,
    E = <<0:Size/unit:8>>,
    exit({A,B,C,D,E}).

build_bin_on_heap_unknown_size() ->
    Size = id(64),
    A = erlang:make_tuple(233 - 3 * 8, a),

    %% Each created binary will be a heap binary, but the JIT will
    %% not know that beforehand.
    B = <<0:Size/unit:8>>,
    C = <<0:Size/unit:8>>,
    D = <<0:Size/unit:8>>,
    E = <<0:Size/unit:8>>,
    exit({A,B,C,D,E}).

build_bin_append_known_binary() ->
    A = erlang:make_tuple(233 - 3 * 8, a),
    Bin = id(<<>>),
    true = is_binary(Bin),

    B = <<Bin/binary,"abc">>,
    C = <<B/binary,"abc">>,
    D = <<C/binary,"abc">>,
    E = <<D/binary,"abc">>,
    exit({A,B,C,D,E}).

build_bin_append_unknown_binary() ->
    A = erlang:make_tuple(233 - 3 * 8, a),
    Bin = id(<<>>),

    B = <<Bin/binary,"abc">>,
    C = <<(id(B))/binary,"abc">>,
    D = <<(id(C))/binary,"abc">>,
    E = <<(id(D))/binary,"abc">>,
    exit({A,B,C,D,E}).

build_bin_append_unknown_binary_guard() ->
    A = erlang:make_tuple(233 - 3 * 8, a),
    Bin = id(<<>>),

    if
        byte_size(<<Bin/binary,"abc">>) =:= 0 ->
            ok;
        byte_size(<<Bin/binary,"abc">>) =:= 0 ->
            ok;
        byte_size(<<Bin/binary,"abc">>) =:= 0 ->
            ok;
        byte_size(<<Bin/binary,"abc">>) =:= 0 ->
            ok;
        true ->
            exit(A)
    end.

build_bin_append_writable_binary_extra() ->
    A = erlang:make_tuple(233 - 3 * 8, a),
    Bin = id(<<>>),
    Size = id(0),

    B = <<Bin/binary,1:8>>,

    %% Cover handling of special case for append of zero bits. The heap space
    %% needed for the tuple will cause a GC.
    C = <<B/binary,0:Size>>,
    exit({A,B,C,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24}).

build_bin_append_unknown_binary_extra() ->
    A = erlang:make_tuple(233 - 3 * 8, a),
    Bin = id(<<>>),

    %% Try appending zero bits.
    B = <<Bin/binary,0:0>>,
    exit({A,B,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24}).

build_bin_and_bif() ->
    build_bin_and_bif(?MANY_ITERATIONS, <<>>).

build_bin_and_bif(0, Acc0) ->
    Acc0;
build_bin_and_bif(N, Acc0) ->
    Acc = <<0, Acc0/binary>>,
    _ = erlang:crc32(Acc),
    build_bin_and_bif(N-1, Acc).

build_and_recv_timeout() ->
    build_and_recv_timeout(?MANY_ITERATIONS, []).

build_and_recv_timeout(0, Acc0) ->
    Acc0;
build_and_recv_timeout(N, Acc0) ->
    Acc = [0|Acc0],
    receive
    after 1 ->
            ok
    end,
    build_and_recv_timeout(N-1, Acc).

build_and_recv_msg() ->
    build_and_recv_msg(?MANY_ITERATIONS, []).

build_and_recv_msg(0, Acc0) ->
    Acc0;
build_and_recv_msg(N, Acc0) ->
    Acc = [0|Acc0],
    receive
        _ ->
            ok
    after 0 ->
            ok
    end,
    build_and_recv_msg(N-1, Acc).

bif_and_recv_timeout() ->
    Bin = <<0:?MANY_ITERATIONS/unit:8>>,
    bif_and_recv_timeout(Bin).

bif_and_recv_timeout(Bin) ->
    List = binary_to_list(Bin),
    receive
    after 1 ->
            ok
    end,
    List.

bif_and_recv_msg() ->
    Bin = <<0:?MANY_ITERATIONS/unit:8>>,
    bif_and_recv_msg(Bin).

bif_and_recv_msg(Bin) ->
    List = binary_to_list(Bin),
    receive
        _ ->
            ok
    after 0 ->
            ok
    end,
    List.

bs_match_integer_known_size() ->
    Size = 233 * 8,
    Bin = id(<<255:Size/little-unit:8>>),
    <<N:Size/big-unit:8>> = Bin,
    exit(N).

bs_match_integer_unknown_size() ->
    Size = id(233 * 8),
    Bin = id(<<255:Size/little-unit:8>>),
    <<N:Size/big-unit:8>> = Bin,
    exit(N).


%%%
%%% Common utilities.
%%%

id(I) ->
    I.
