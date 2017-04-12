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
-module(ei_decode_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ei_decode_SUITE_data/ei_decode_test_cases.hrl").

-export([all/0, suite/0,
         test_ei_decode_long/1,
         test_ei_decode_ulong/1,
         test_ei_decode_longlong/1,
         test_ei_decode_ulonglong/1,
         test_ei_decode_char/1,
         test_ei_decode_nonoptimal/1,
         test_ei_decode_misc/1,
         test_ei_decode_utf8_atom/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [test_ei_decode_long, test_ei_decode_ulong,
     test_ei_decode_longlong, test_ei_decode_ulonglong,
     test_ei_decode_char, test_ei_decode_nonoptimal,
     test_ei_decode_misc, test_ei_decode_utf8_atom].

%% ---------------------------------------------------------------------------

% NOTE: for historical reasons we don't pach as tight as we can,
%       we only fill 27 bits in 32 bit INTEGER_EXT 


%% ######################################################################## %%

test_ei_decode_long(Config) when is_list(Config) ->
    P = runner:start(?test_ei_decode_long),
    send_integers(P),
    runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_decode_ulong(Config) when is_list(Config) ->
    P = runner:start(?test_ei_decode_ulong),
    send_integers(P),
    runner:recv_eot(P),
    ok.


% (*) In practical terms, other values may fit into the ext format
% i32 is signed 32 bit on C side
% u32 is unsigned 32 bit on C side

%% ######################################################################## %%

test_ei_decode_longlong(Config) when is_list(Config) ->
    case os:type() of
        vxworks ->
            {skip,"Skipped on VxWorks"};
        _ ->
            P = runner:start(?test_ei_decode_longlong),
            send_integers2(P),
            runner:recv_eot(P),
            ok
    end.


%% ######################################################################## %%

test_ei_decode_ulonglong(Config) when is_list(Config) ->
    case os:type() of
        vxworks ->
            {skip,"Skipped on VxWorks"};
        _ ->
            P = runner:start(?test_ei_decode_ulonglong),
            send_integers2(P),
            runner:recv_eot(P),
            ok
    end.


%% ######################################################################## %%
%% A "character" for us is an 8 bit integer, always positive, i.e.
%% it is unsigned.
%% FIXME maybe the API should change to use "unsigned char" to be clear?!

test_ei_decode_char(Config) when is_list(Config) ->
    P = runner:start(?test_ei_decode_char),

    send_term_as_binary(P,0),
    send_term_as_binary(P,16#7f),
    send_term_as_binary(P,16#ff),

    send_term_as_binary(P, []), % illegal type

    runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_decode_nonoptimal(Config) when is_list(Config) ->
    P = runner:start(?test_ei_decode_nonoptimal),

    send_non_optimal_pos(P),			% decode_char
    send_non_optimal(P),			% decode_long
    send_non_optimal_pos(P),			% decode_ulong
    case os:type() of
        vxworks ->
            ok;
        _ ->
            send_non_optimal(P),			% decode_longlong
            send_non_optimal_pos(P)			% decode_ulonglong
    end,

    runner:recv_eot(P),
    ok.


send_non_optimal(P) ->
    send_non_optimal_pos(P),
    send_non_optimal_neg(P).

send_non_optimal_pos(P) ->
    send_raw(P, <<131,97,42>>),
    send_raw(P, <<131,98,42:32>>),
    send_raw(P, <<131,110,1,0,42>>),
    send_raw(P, <<131,110,2,0,42,0>>),
    send_raw(P, <<131,110,4,0,42,0,0,0>>),
    send_raw(P, <<131,111,0,0,0,1,0,42>>),
    send_raw(P, <<131,111,0,0,0,2,0,42,0>>),
    send_raw(P, <<131,111,0,0,0,3,0,42,0,0>>),
    send_raw(P, <<131,111,0,0,0,6,0,42,0,0,0,0,0>>),
    ok.

send_non_optimal_neg(P) ->
    %   send_raw(P, <<131,97,-42>>),
    send_raw(P, <<131,98,-42:32>>),
    send_raw(P, <<131,110,1,1,42>>),
    send_raw(P, <<131,110,2,1,42,0>>),
    send_raw(P, <<131,110,4,1,42,0,0,0>>),
    send_raw(P, <<131,111,0,0,0,1,1,42>>),
    send_raw(P, <<131,111,0,0,0,2,1,42,0>>),
    send_raw(P, <<131,111,0,0,0,3,1,42,0,0>>),
    send_raw(P, <<131,111,0,0,0,6,1,42,0,0,0,0,0>>),
    ok.


%% ######################################################################## %%

test_ei_decode_misc(Config) when is_list(Config) ->
    P = runner:start(?test_ei_decode_misc),

    send_term_as_binary(P,0.0),
    send_term_as_binary(P,-1.0),
    send_term_as_binary(P,1.0),

    send_term_as_binary(P,false),
    send_term_as_binary(P,true),

    send_term_as_binary(P,foo),
    send_term_as_binary(P,''),
    %%send_term_as_binary(P,'ÅÄÖåäö'),
    send_latin1_atom_as_binary(P, "ÅÄÖåäö"),

    send_term_as_binary(P,"foo"),
    send_term_as_binary(P,""),
    send_term_as_binary(P,"ÅÄÖåäö"),

    send_term_as_binary(P,<<"foo">>),
    send_term_as_binary(P,<<>>),
    send_term_as_binary(P,<<"ÅÄÖåäö">>),

    %    send_term_as_binary(P,{}),
    %    send_term_as_binary(P,[]),

    runner:recv_eot(P),
    ok.

%% ######################################################################## %%

test_ei_decode_utf8_atom(Config) ->
    P = runner:start(?test_ei_decode_utf8_atom),

    send_latin1_atom_as_binary(P,"å"),
    send_latin1_atom_as_binary(P,"ä"),
    send_latin1_atom_as_binary(P,"ö"),
    send_latin1_atom_as_binary(P,"õ"),

    send_utf8_atom_as_binary(P,[1758]),
    send_utf8_atom_as_binary(P,[1758,1758]),
    send_utf8_atom_as_binary(P,[1758,1758,1758]),
    send_utf8_atom_as_binary(P,[1758,1758,1758,1758]),

    send_latin1_atom_as_binary(P,"a"),
    send_latin1_atom_as_binary(P,"b"),

    send_term_as_binary(P,'c'),
    send_term_as_binary(P,'d'),

    runner:recv_eot(P),
    ok.


%% ######################################################################## %%

send_term_as_binary(Port, Term) when is_port(Port) ->
    Port ! {self(), {command, term_to_binary(Term)}}.

send_raw(Port, Bin) when is_port(Port) ->
    Port ! {self(), {command, Bin}}.

send_utf8_atom_as_binary(Port, String) ->
    Port ! {self(), {command, term_to_binary(uc_atup(String))}}.

send_latin1_atom_as_binary(Port, String) ->
    Port ! {self(), {command, encode_latin1_atom(String)}}.

send_integers(P) ->
    send_term_as_binary(P,0),		% SMALL_INTEGER_EXT smallest
    send_term_as_binary(P,255),		% SMALL_INTEGER_EXT largest
    send_term_as_binary(P,256),		% INTEGER_EXT smallest pos (*)
    send_term_as_binary(P,-1),		% INTEGER_EXT largest  neg

    send_term_as_binary(P, 16#07ffffff),	% INTEGER_EXT old largest (28 bits)
    send_term_as_binary(P,-16#08000000),	% INTEGER_EXT old smallest
    send_term_as_binary(P, 16#08000000),  % SMALL_BIG_EXT old smallest pos(*)
    send_term_as_binary(P,-16#08000001),	% SMALL_BIG_EXT old largest neg (*)

    send_term_as_binary(P, 16#7fffffff),	% INTEGER_EXT new largest (32 bits)
    send_term_as_binary(P,-16#80000000),	% INTEGER_EXT new smallest (32 bis)
    send_term_as_binary(P, 16#80000000),  % SMALL_BIG_EXT new smallest pos(*)
    send_term_as_binary(P,-16#80000001),	% SMALL_BIG_EXT new largest neg (*)

    case erlang:system_info({wordsize,external}) of
        4 ->
            send_term_as_binary(P, 16#80000000),% SMALL_BIG_EXT u32
            send_term_as_binary(P, 16#ffffffff),% SMALL_BIG_EXT largest u32

            send_term_as_binary(P, 16#7fffffffffff), % largest  i48
            send_term_as_binary(P,-16#800000000000), % smallest i48
            send_term_as_binary(P, 16#ffffffffffff), % largest  u48
            send_term_as_binary(P, 16#7fffffffffffffff), % largest  i64
            send_term_as_binary(P,-16#8000000000000000), % smallest i64
            send_term_as_binary(P, 16#ffffffffffffffff); % largest  u64
        8 ->
            send_term_as_binary(P, 16#8000000000000000),% SMALL_BIG_EXT u64
            % SMALL_BIG_EXT largest u64
            send_term_as_binary(P, 16#ffffffffffffffff),
            % largest  i96
            send_term_as_binary(P, 16#7fffffffffffffffffffffff),
            % smallest i96
            send_term_as_binary(P,-16#800000000000000000000000),
            % largest  u96
            send_term_as_binary(P, 16#ffffffffffffffffffffffff),
            % largest  i128
            send_term_as_binary(P, 16#7fffffffffffffffffffffffffffffff),
            % smallest i128
            send_term_as_binary(P,-16#80000000000000000000000000000000),
            % largest  u128
            send_term_as_binary(P, 16#ffffffffffffffffffffffffffffffff)
    end,
    send_term_as_binary(P, []), % illegal type
    ok.

send_integers2(P) ->
    send_term_as_binary(P,0),           % SMALL_INTEGER_EXT smallest
    send_term_as_binary(P,255),	        % SMALL_INTEGER_EXT largest
    send_term_as_binary(P,256),	        % INTEGER_EXT smallest pos (*)
    send_term_as_binary(P,-1),          % INTEGER_EXT largest  neg

    send_term_as_binary(P, 16#07ffffff),     % INTEGER_EXT old largest (28 bits)
    send_term_as_binary(P,-16#08000000),     % INTEGER_EXT old smallest
    send_term_as_binary(P, 16#08000000),     % SMALL_BIG_EXT old smallest pos(*)
    send_term_as_binary(P,-16#08000001),     % SMALL_BIG_EXT old largest neg (*)

    send_term_as_binary(P, 16#7fffffff),     % INTEGER_EXT new largest (32 bits)
    send_term_as_binary(P,-16#80000000),     % INTEGER_EXT new smallest
    send_term_as_binary(P, 16#80000000),     % SMALL_BIG_EXT new smallest pos(*)
    send_term_as_binary(P,-16#80000001),     % SMALL_BIG_EXT new largest neg (*)

    send_term_as_binary(P, 16#ffffffff),     % SMALL_BIG_EXT largest u32

    send_term_as_binary(P, 16#7fffffffffff),     % largest  i48
    send_term_as_binary(P,-16#800000000000),     % smallest i48
    send_term_as_binary(P, 16#ffffffffffff),     % largest  u48
    send_term_as_binary(P, 16#7fffffffffffffff), % largest  i64
    send_term_as_binary(P,-16#8000000000000000), % smallest i64
    send_term_as_binary(P, 16#ffffffffffffffff), % largest  u64
    send_term_as_binary(P, []), % illegal type
    ok.

encode_latin1_atom(String) ->
    Len = length(String),
    %% Use ATOM_EXT (not SMALL_*) to simulate old term_to_binary
    TagLen = [$d, Len bsr 8, Len band 16#ff],
    list_to_binary([131, TagLen, String]).

uc_atup(ATxt) ->
    string_to_atom(ATxt).

string_to_atom(String) ->
    Utf8List = string_to_utf8_list(String),
    Len = length(Utf8List),
    TagLen = case Len < 256 of
                 true -> [119, Len];
                 false -> [118, Len bsr 8, Len band 16#ff]
             end,
    binary_to_term(list_to_binary([131, TagLen, Utf8List])).

string_to_utf8_list([]) ->
    [];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   0 =< CP,
                                   CP =< 16#7F ->
    [CP | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   16#80 =< CP,
                                   CP =< 16#7FF ->
    [16#C0 bor (CP bsr 6),
     16#80 bor (16#3F band CP) | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   16#800 =< CP,
                                   CP =< 16#FFFF ->
    [16#E0 bor (CP bsr 12),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP) | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   16#10000 =< CP,
                                   CP =< 16#10FFFF ->
    [16#F0 bor (CP bsr 18),
     16#80 bor (16#3F band (CP bsr 12)),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP) | string_to_utf8_list(CPs)].
