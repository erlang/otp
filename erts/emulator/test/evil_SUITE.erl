%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

-module(evil_SUITE).

-export([all/0, suite/0,
         heap_frag/1,
         encode_decode_ext/1,
         decode_integer_ext/1,
         decode_small_big_ext/1,
         decode_large_big_ext/1,
         decode_small_big_ext_neg/1,
         decode_large_big_ext_neg/1,
         decode_too_small/1,
         decode_pos_neg_zero/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() -> 
    [heap_frag, encode_decode_ext, decode_integer_ext,
     decode_small_big_ext, decode_large_big_ext,
     decode_small_big_ext_neg, decode_large_big_ext_neg,
     decode_too_small, decode_pos_neg_zero].

heap_frag(Config) when is_list(Config) ->
    N = 512,
    Self = self(),
    Pid = spawn_link(fun() -> appender(Self, N) end),
    receive
        {Pid,Res} ->
            Res = my_appender(N);
        Garbage ->
            io:format("Garbage: ~p\n", [Garbage]),
            ct:fail(got_garbage)
    end.


%% ######################################################################## %%

%% "Interesting" integers taken from erl_interface ei_decode_SUITE.erl
%% These test cases are not "evil" but the next test case is....

encode_decode_ext(Config) when is_list(Config) ->
    enc_dec( 2,   0),               % SMALL_INTEGER_EXT smallest
    enc_dec( 2, 255),           	  % SMALL_INTEGER_EXT largest
    enc_dec( 5, 256),           	  % INTEGER_EXT smallest pos (*)
    enc_dec( 5,  -1),            	  % INTEGER_EXT largest  neg

    enc_dec( 5, 16#07ffffff),  	  % INTEGER_EXT largest (28 bits)
    enc_dec( 5,-16#08000000),  	  % INTEGER_EXT smallest
    enc_dec( 7, 16#08000000),  	  % SMALL_BIG_EXT smallest pos(*)
    enc_dec( 7,-16#08000001),  	  % SMALL_BIG_EXT largest neg (*)

    enc_dec( 7, 16#7fffffff),  	  % SMALL_BIG_EXT largest  i32
    enc_dec( 7,-16#80000000),  	  % SMALL_BIG_EXT smallest i32

    enc_dec( 7, 16#80000000),  	  % SMALL_BIG_EXT u32
    enc_dec( 7, 16#ffffffff),  	  % SMALL_BIG_EXT largest u32

    enc_dec( 9, 16#7fffffffffff),     % largest  i48
    enc_dec( 9,-16#800000000000),     % smallest i48
    enc_dec( 9, 16#ffffffffffff),     % largest  u48
    enc_dec(11, 16#7fffffffffffffff), % largest  i64
    enc_dec(11,-16#8000000000000000), % smallest i64
    enc_dec(11, 16#ffffffffffffffff), % largest  u64
    ok.


%% ######################################################################## %%

%% "Interesting" integers taken from erl_interface ei_decode_SUITE.erl
%% These test the decoding "unusual", i.e. integers packed according
%% to "erts/emulator/internal_doc/erl_ext_dist.txt" but not the way
%% the emulator or erl_interface encode them.
%%
%% NOTE!!!! The comments below after a decode line is how it currently
%% is encoded in the external format by the emulator and
%% erl_interface, i.e. not how it is encoded in the test case below.

decode_integer_ext(Config) when is_list(Config) ->
    decode(  0, <<131,98,  0:32>>),	% SMALL_INTEGER_EXT
    decode( 42, <<131,98, 42:32>>),	% SMALL_INTEGER_EXT
    decode(255, <<131,98,255:32>>),	% SMALL_INTEGER_EXT
    decode( 16#08000000, <<131,98, 16#08000000:32>>), % SMALL_BIG_EXT
    decode(-16#08000001, <<131,98,-16#08000001:32>>), % SMALL_BIG_EXT
    decode( 16#7fffffff, <<131,98, 16#7fffffff:32>>), % SMALL_BIG_EXT
    decode(-16#80000000, <<131,98,-16#80000000:32>>), % SMALL_BIG_EXT
    ok.

decode_small_big_ext(Config) when is_list(Config) ->
    decode(256,<<131,110,2,0,0,1>>),	% INTEGER_EXT
    decode(16#07ffffff,<<131,110,4,0,255,255,255,7>>), % INTEGER_EXT
    decode(16#7fffffff,<<131,110,4,0,255,255,255,127>>), % SMALL_BIG_EXT

    decode(42,<<131,110,1,0,42>>),	% SMALL_INTEGER_EXT
    decode(42,<<131,110,2,0,42,0>>),	% Redundant zeros from now on
    decode(42,<<131,110,3,0,42,0,0>>),
    decode(42,<<131,110,4,0,42,0,0,0>>),
    decode(42,<<131,110,5,0,42,0,0,0,0>>),
    decode(42,<<131,110,6,0,42,0,0,0,0,0>>),
    decode(42,<<131,110,7,0,42,0,0,0,0,0,0>>),
    decode(42,<<131,110,8,0,42,0,0,0,0,0,0,0>>),
    ok.

decode_large_big_ext(Config) when is_list(Config) ->
    decode(256,<<131,111,2:32,0,0,1>>), % INTEGER_EXT
    decode(16#07ffffff,<<131,111,4:32,0,255,255,255,7>>), % INTEG_EXT
    decode(16#7fffffff,<<131,111,4:32,0,255,255,255,127>>), % SMA_BIG
    decode(16#ffffffff,<<131,111,4:32,0,255,255,255,255>>), % SMA_BIG

    N = largest_small_big(),
    decode(N,<<131,111,255:32,0,N:2040/little>>), % SMALL_BIG_EXT

    decode(42,<<131,111,1:32,0,42>>),
    decode(42,<<131,111,2:32,0,42,0>>), % Redundant zeros from now on
    decode(42,<<131,111,3:32,0,42,0,0>>),
    decode(42,<<131,111,4:32,0,42,0,0,0>>),
    decode(42,<<131,111,5:32,0,42,0,0,0,0>>),
    decode(42,<<131,111,6:32,0,42,0,0,0,0,0>>),
    decode(42,<<131,111,7:32,0,42,0,0,0,0,0,0>>),
    decode(42,<<131,111,8:32,0,42,0,0,0,0,0,0,0>>),
    ok.

decode_small_big_ext_neg(Config) when is_list(Config) ->
    decode(-1,<<131,110,1,1,1>>),		% INTEGER_EXT
    decode(-16#08000000,<<131,110,4,1,0,0,0,8>>), % INTEGER_EXT
    decode(-16#80000000,<<131,110,4,1,0,0,0,128>>), % SMALL_BIG_EXT
    decode(-16#ffffffff,<<131,110,4,1,255,255,255,255>>), % SMALL_BIG_EXT

    N = largest_small_big(),
    decode(-N,<<131,111,255:32,1,N:2040/little>>), % SMALL_BIG_EXT

    decode(-42,<<131,110,1,1,42>>),
    decode(-42,<<131,110,2,1,42,0>>),	% Redundant zeros from now on
    decode(-42,<<131,110,3,1,42,0,0>>),
    decode(-42,<<131,110,4,1,42,0,0,0>>),
    decode(-42,<<131,110,5,1,42,0,0,0,0>>),
    decode(-42,<<131,110,6,1,42,0,0,0,0,0>>),
    decode(-42,<<131,110,7,1,42,0,0,0,0,0,0>>),
    decode(-42,<<131,110,8,1,42,0,0,0,0,0,0,0>>),
    ok.

decode_large_big_ext_neg(Config) when is_list(Config) ->
    decode(-1,<<131,111,1:32,1,1>>),	% INTEGER_EXT
    decode(-16#08000000,<<131,111,4:32,1,0,0,0,8>>), % INTEGER_EXT
    decode(-16#80000000,<<131,111,4:32,1,0,0,0,128>>), % SMALL_BIG_EXT

    decode(-42,<<131,111,1:32,1,42>>),
    decode(-42,<<131,111,2:32,1,42,0>>), % Redundant zeros from now on
    decode(-42,<<131,111,3:32,1,42,0,0>>),
    decode(-42,<<131,111,4:32,1,42,0,0,0>>),
    decode(-42,<<131,111,5:32,1,42,0,0,0,0>>),
    decode(-42,<<131,111,6:32,1,42,0,0,0,0,0>>),
    decode(-42,<<131,111,7:32,1,42,0,0,0,0,0,0>>),
    decode(-42,<<131,111,8:32,1,42,0,0,0,0,0,0,0>>),
    ok.

decode_pos_neg_zero(Config) when is_list(Config) ->
    decode( 0, <<131,110,0,0>>),		% SMALL_BIG_EXT (positive zero)
    decode( 0, <<131,110,1,0,0>>),	% SMALL_BIG_EXT (positive zero)
    decode( 0, <<131,110,0,1>>),		% SMALL_BIG_EXT (negative zero)
    decode( 0, <<131,110,1,1,0>>),	% SMALL_BIG_EXT (negative zero)

    decode( 0, <<131,111,0:32,0>>),	% SMALL_BIG_EXT (positive zero)
    decode( 0, <<131,111,1:32,0,0>>),	% SMALL_BIG_EXT (positive zero)
    decode( 0, <<131,111,0:32,1>>),	% SMALL_BIG_EXT (negative zero)
    decode( 0, <<131,111,1:32,1,0>>),	% SMALL_BIG_EXT (negative zero)

    N = largest_small_big(),
    decode( N,<<131,110,255,0,N:2040/little>>), % largest SMALL_BIG_EXT
    decode(-N,<<131,110,255,1,N:2040/little>>), % largest SMALL_BIG_EXT

    ok.

%% Test to decode uncompleted encodings for all in "erl_ext_dist.txt"

decode_too_small(Config) when is_list(Config) ->
    decode_badarg(<<131, 97>>),
    decode_badarg(<<131, 98>>),
    decode_badarg(<<131, 98, 0>>),
    decode_badarg(<<131, 98, 0, 0>>),
    decode_badarg(<<131, 98, 0, 0, 0>>),
    decode_badarg(<<131, 99>>),
    decode_badarg(<<131, 99, 0>>),
    decode_badarg(<<131, 99, 0:240>>),

    decode_badarg(<<131,100>>),
    decode_badarg(<<131,100, 1:16/big>>),
    decode_badarg(<<131,100, 2:16/big>>),
    decode_badarg(<<131,100, 2:16/big, "A">>),

    % FIXME node name "A" seem ok, should it be?
    %    decode_badarg(<<131,101,100,1:16/big,"A",42:32/big,0>>),

    decode_badarg(<<131,101>>),
    decode_badarg(<<131,101,106>>),
    decode_badarg(<<131,101,255>>),
    decode_badarg(<<131,101,106,42:8/big>>),
    decode_badarg(<<131,101,106,42:16/big>>),
    decode_badarg(<<131,101,255,42:24/big>>),
    decode_badarg(<<131,101,255,42:32/big,0>>),
    decode_badarg(<<131,101,100,1:16/big,"A">>),
    decode_badarg(<<131,101,100,1:16/big,"A",42:32/big>>),

    decode_badarg(<<131,102>>),
    decode_badarg(<<131,102,106,42:32/big,0>>),
    decode_badarg(<<131,102,255,42:32/big,0>>),
    decode_badarg(<<131,102,100,1:16/big,"A">>),
    decode_badarg(<<131,102,100,1:16/big,"A",42:32/big>>),

    decode_badarg(<<131,103>>),
    decode_badarg(<<131,103,106,42:32/big,0>>),
    decode_badarg(<<131,103,255,42:32/big,0>>),
    decode_badarg(<<131,103,100,1:16/big,"A">>),
    decode_badarg(<<131,103,100,1:16/big,"A",42:32/big>>),
    decode_badarg(<<131,103,100,1:16/big,"A",4:32/big,2:32/big>>),

    decode_badarg(<<131,104>>),
    decode_badarg(<<131,104, 1>>),
    decode_badarg(<<131,104, 2, 106>>),
    decode_badarg(<<131,105, 1:32/big>>),
    decode_badarg(<<131,105, 2:32/big, 106>>),

    decode_badarg(<<131,107>>),
    decode_badarg(<<131,107, 1:16/big>>),
    decode_badarg(<<131,107, 2:16/big>>),
    decode_badarg(<<131,107, 2:16/big, "A">>),

    decode_badarg(<<131,108>>),
    decode_badarg(<<131,108, 1:32/big>>),
    decode_badarg(<<131,108, 2:32/big>>),
    decode_badarg(<<131,108, 2:32/big, 106>>), % FIXME don't use NIL

    decode_badarg(<<131,109>>),
    decode_badarg(<<131,109, 1:32/big>>),
    decode_badarg(<<131,109, 2:32/big>>),
    decode_badarg(<<131,109, 2:32/big, 42>>),

    N = largest_small_big(),

    decode_badarg(<<131,110>>),
    decode_badarg(<<131,110,1>>),
    decode_badarg(<<131,110,1,0>>),
    decode_badarg(<<131,110,1,1>>),
    decode_badarg(<<131,110,2,0,42>>),
    decode_badarg(<<131,110,2,1,42>>),
    decode_badarg(<<131,110,255,0,N:2032/little>>),
    decode_badarg(<<131,110,255,1,N:2032/little>>),

    decode_badarg(<<131,111>>),
    decode_badarg(<<131,111,  1:32/big>>),
    decode_badarg(<<131,111,  1:32/big,0>>),
    decode_badarg(<<131,111,  1:32/big,1>>),
    decode_badarg(<<131,111,  2:32/big,0,42>>),
    decode_badarg(<<131,111,  2:32/big,1,42>>),
    decode_badarg(<<131,111,256:32/big,0,N:2032/little>>),
    decode_badarg(<<131,111,256:32/big,1,N:2032/little>>),
    decode_badarg(<<131,111,256:32/big,0,N:2040/little>>),
    decode_badarg(<<131,111,256:32/big,1,N:2040/little>>),
    decode_badarg(<<131,111,257:32/big,0,N:2048/little>>),
    decode_badarg(<<131,111,257:32/big,1,N:2048/little>>),

    % Emulator dies if trying to create large bignum....
    %    decode_badarg(<<131,111,16#ffffffff:32/big,0>>),
    %    decode_badarg(<<131,111,16#ffffffff:32/big,1>>),

    decode_badarg(<<131, 78>>),
    decode_badarg(<<131, 78, 42>>),
    decode_badarg(<<131, 78, 42, 1>>),
    decode_badarg(<<131, 78, 42, 1:16/big>>),
    decode_badarg(<<131, 78, 42, 2:16/big>>),
    decode_badarg(<<131, 78, 42, 2:16/big, "A">>),

    decode_badarg(<<131, 67>>),

    decode_badarg(<<131,114>>),
    decode_badarg(<<131,114,0>>),
    decode_badarg(<<131,114,1:16/big>>),
    decode_badarg(<<131,114,1:16/big,100>>),
    decode_badarg(<<131,114,1:16/big,100,1:16/big>>),
    decode_badarg(<<131,114,1:16/big,100,1:16/big,"A">>),
    decode_badarg(<<131,114,1:16/big,100,1:16/big,"A",0>>),
    decode_badarg(<<131,114,1:16/big,100,1:16/big,"A",0,42:8>>),
    decode_badarg(<<131,114,1:16/big,100,1:16/big,"A",0,42:16>>),
    decode_badarg(<<131,114,1:16/big,100,1:16/big,"A",0,42:24>>),

    decode_badarg(<<131,117>>),		% FIXME needs more tests

    ok.

%% ######################################################################## %%

decode_badarg(Bin) ->
    io:format("Trying ~w\n",[Bin]),
    {'EXIT',{badarg,_}} = (catch binary_to_term(Bin)).

enc_dec(_Size, Term) ->
    Bin = term_to_binary(Term),
    Term = binary_to_term(Bin),
    ok.

decode(Term, Binary) ->
    io:format("Encoding ~w to ~w ... ",[Binary,Term]),
    NewTerm = binary_to_term(Binary),
    io:format("got ~w\n",[NewTerm]),
    Term = NewTerm.

largest_small_big() ->
    List  = lists:duplicate(255,255),
    Limbs = list_to_binary(List),
    binary_to_term(<<131,110,255,0,Limbs/binary>>).

%% ######################################################################## %%

appender(Parent, N) ->
    seed(),
    Res = appender_1(N, {}),
    Parent ! {self(),Res}.

appender_1(0, T) -> T;
appender_1(N, T0) ->
    U = rnd_term(),
    T = erlang:append_element(T0, U),
    appender_1(N-1, T).

my_appender(N) ->
    seed(),
    my_appender_1(N, []).

my_appender_1(0, T) ->
    list_to_tuple(lists:reverse(T));
my_appender_1(N, T0) ->
    U = rnd_term(),
    T = [U|T0],
    my_appender_1(N-1, T).

seed() ->
    rand:seed(exsplus, {3172,9815,20129}).

rnd_term() ->
    U0 = rand:uniform(),
    B = <<U0/float>>,
    {U0,U0 * 2.5 + 3.14,[U0*2.3,B]}.
