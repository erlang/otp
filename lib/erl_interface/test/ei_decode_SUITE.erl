%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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

%%
-module(ei_decode_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("ei_decode_SUITE_data/ei_decode_test_cases.hrl").

-export(
   [
    all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
    init_per_group/2,end_per_group/2, init_per_testcase/2,
    end_per_testcase/2,
    test_ei_decode_long/1,
    test_ei_decode_ulong/1,
    test_ei_decode_longlong/1,
    test_ei_decode_ulonglong/1,
    test_ei_decode_char/1,
    test_ei_decode_nonoptimal/1,
    test_ei_decode_misc/1,
    test_ei_decode_utf8_atom/1
   ]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [test_ei_decode_long, test_ei_decode_ulong,
     test_ei_decode_longlong, test_ei_decode_ulonglong,
     test_ei_decode_char, test_ei_decode_nonoptimal,
     test_ei_decode_misc, test_ei_decode_utf8_atom].

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

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_RC, Config) ->
    Config.

%% ---------------------------------------------------------------------------

% NOTE: for historical reasons we don't pach as tight as we can,
%       we only fill 27 bits in 32 bit INTEGER_EXT 


%% ######################################################################## %%

test_ei_decode_long(suite) -> [];
test_ei_decode_long(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_decode_long),
    send_integers(P),
    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_decode_ulong(suite) -> [];
test_ei_decode_ulong(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_decode_ulong),
    send_integers(P),
    ?line runner:recv_eot(P),
    ok.


% (*) In practical terms, other values may fit into the ext format
% i32 is signed 32 bit on C side
% u32 is unsigned 32 bit on C side
 
%% ######################################################################## %%

test_ei_decode_longlong(suite) -> [];
test_ei_decode_longlong(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {skip,"Skipped on VxWorks"};
	_ ->
	    ?line P = runner:start(?test_ei_decode_longlong),
	    send_integers2(P),
	    ?line runner:recv_eot(P),
	    ok
    end.


%% ######################################################################## %%

test_ei_decode_ulonglong(suite) -> [];
test_ei_decode_ulonglong(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {skip,"Skipped on VxWorks"};
	_ ->
	    ?line P = runner:start(?test_ei_decode_ulonglong),
	    send_integers2(P),
	    ?line runner:recv_eot(P),
	    ok
    end.


%% ######################################################################## %%
%% A "character" for us is an 8 bit integer, alwasy positive, i.e.
%% it is unsigned.
%% FIXME maybe the API should change to use "unsigned char" to be clear?!

test_ei_decode_char(suite) -> [];
test_ei_decode_char(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_decode_char),

    ?line send_term_as_binary(P,0),
    ?line send_term_as_binary(P,16#7f),
    ?line send_term_as_binary(P,16#ff),

    ?line send_term_as_binary(P, []), % illegal type

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

test_ei_decode_nonoptimal(suite) -> [];
test_ei_decode_nonoptimal(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_decode_nonoptimal),

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

    ?line runner:recv_eot(P),
    ok.


send_non_optimal(P) ->
    send_non_optimal_pos(P),
    send_non_optimal_neg(P).

send_non_optimal_pos(P) ->
    ?line send_raw(P, <<131,97,42>>),
    ?line send_raw(P, <<131,98,42:32>>),
    ?line send_raw(P, <<131,110,1,0,42>>),
    ?line send_raw(P, <<131,110,2,0,42,0>>),
    ?line send_raw(P, <<131,110,4,0,42,0,0,0>>),
    ?line send_raw(P, <<131,111,0,0,0,1,0,42>>),
    ?line send_raw(P, <<131,111,0,0,0,2,0,42,0>>),
    ?line send_raw(P, <<131,111,0,0,0,3,0,42,0,0>>),
    ?line send_raw(P, <<131,111,0,0,0,6,0,42,0,0,0,0,0>>),
    ok.

send_non_optimal_neg(P) ->
%   ?line send_raw(P, <<131,97,-42>>),
    ?line send_raw(P, <<131,98,-42:32>>),
    ?line send_raw(P, <<131,110,1,1,42>>),
    ?line send_raw(P, <<131,110,2,1,42,0>>),
    ?line send_raw(P, <<131,110,4,1,42,0,0,0>>),
    ?line send_raw(P, <<131,111,0,0,0,1,1,42>>),
    ?line send_raw(P, <<131,111,0,0,0,2,1,42,0>>),
    ?line send_raw(P, <<131,111,0,0,0,3,1,42,0,0>>),
    ?line send_raw(P, <<131,111,0,0,0,6,1,42,0,0,0,0,0>>),
    ok.


%% ######################################################################## %%

test_ei_decode_misc(suite) -> [];
test_ei_decode_misc(Config) when is_list(Config) ->
    ?line P = runner:start(?test_ei_decode_misc),

    ?line send_term_as_binary(P,0.0),
    ?line send_term_as_binary(P,-1.0),
    ?line send_term_as_binary(P,1.0),

    ?line send_term_as_binary(P,false),
    ?line send_term_as_binary(P,true),

    ?line send_term_as_binary(P,foo),
    ?line send_term_as_binary(P,''),
    ?line send_term_as_binary(P,'ÅÄÖåäö'),

    ?line send_term_as_binary(P,"foo"),
    ?line send_term_as_binary(P,""),
    ?line send_term_as_binary(P,"ÅÄÖåäö"),

    ?line send_term_as_binary(P,<<"foo">>),
    ?line send_term_as_binary(P,<<>>),
    ?line send_term_as_binary(P,<<"ÅÄÖåäö">>),

%    ?line send_term_as_binary(P,{}),
%    ?line send_term_as_binary(P,[]),

    ?line runner:recv_eot(P),
    ok.

%% ######################################################################## %%

test_ei_decode_utf8_atom(Config) ->
    ?line P = runner:start(?test_ei_decode_utf8_atom),

    send_utf8_atom_as_binary(P,"å"),
    send_utf8_atom_as_binary(P,"ä"),
    send_term_as_binary(P,'ö'),
    send_term_as_binary(P,'õ'),
    
    ?line send_utf8_atom_as_binary(P,[1758]),
    ?line send_utf8_atom_as_binary(P,[1758,1758]),
    ?line send_utf8_atom_as_binary(P,[1758,1758,1758]),
    ?line send_utf8_atom_as_binary(P,[1758,1758,1758,1758]),

    send_utf8_atom_as_binary(P,"a"),
    send_utf8_atom_as_binary(P,"b"),
    send_term_as_binary(P,'c'),
    send_term_as_binary(P,'d'),

    ?line runner:recv_eot(P),
    ok.


%% ######################################################################## %%

send_term_as_binary(Port, Term) when is_port(Port) ->
    Port ! {self(), {command, term_to_binary(Term)}}.

send_raw(Port, Bin) when is_port(Port) ->
    Port ! {self(), {command, Bin}}.

send_utf8_atom_as_binary(Port, String) ->
    Port ! {self(), {command, term_to_binary(uc_atup(String))}}.

send_integers(P) ->
    ?line send_term_as_binary(P,0),		% SMALL_INTEGER_EXT smallest
    ?line send_term_as_binary(P,255),		% SMALL_INTEGER_EXT largest
    ?line send_term_as_binary(P,256),		% INTEGER_EXT smallest pos (*)
    ?line send_term_as_binary(P,-1),		% INTEGER_EXT largest  neg 

    ?line send_term_as_binary(P, 16#07ffffff),	% INTEGER_EXT old largest (28 bits)
    ?line send_term_as_binary(P,-16#08000000),	% INTEGER_EXT old smallest
    ?line send_term_as_binary(P, 16#08000000),  % SMALL_BIG_EXT old smallest pos(*)
    ?line send_term_as_binary(P,-16#08000001),	% SMALL_BIG_EXT old largest neg (*)

    ?line send_term_as_binary(P, 16#7fffffff),	% INTEGER_EXT new largest (32 bits)
    ?line send_term_as_binary(P,-16#80000000),	% INTEGER_EXT new smallest (32 bis)
    ?line send_term_as_binary(P, 16#80000000),  % SMALL_BIG_EXT new smallest pos(*)
    ?line send_term_as_binary(P,-16#80000001),	% SMALL_BIG_EXT new largest neg (*)
 
    case erlang:system_info({wordsize,external}) of
	4 ->	 
          ?line send_term_as_binary(P, 16#80000000),% SMALL_BIG_EXT u32
          ?line send_term_as_binary(P, 16#ffffffff),% SMALL_BIG_EXT largest u32

          ?line send_term_as_binary(P, 16#7fffffffffff), % largest  i48
          ?line send_term_as_binary(P,-16#800000000000), % smallest i48
          ?line send_term_as_binary(P, 16#ffffffffffff), % largest  u48
          ?line send_term_as_binary(P, 16#7fffffffffffffff), % largest  i64
          ?line send_term_as_binary(P,-16#8000000000000000), % smallest i64
          ?line send_term_as_binary(P, 16#ffffffffffffffff); % largest  u64
	8 ->
          ?line send_term_as_binary(P, 16#8000000000000000),% SMALL_BIG_EXT u64
	  % SMALL_BIG_EXT largest u64
          ?line send_term_as_binary(P, 16#ffffffffffffffff),
	  % largest  i96
          ?line send_term_as_binary(P, 16#7fffffffffffffffffffffff), 
	  % smallest i96
          ?line send_term_as_binary(P,-16#800000000000000000000000), 
	  % largest  u96
          ?line send_term_as_binary(P, 16#ffffffffffffffffffffffff), 
	  % largest  i128
          ?line send_term_as_binary(P, 16#7fffffffffffffffffffffffffffffff), 
	  % smallest i128
          ?line send_term_as_binary(P,-16#80000000000000000000000000000000),
	  % largest  u128 
          ?line send_term_as_binary(P, 16#ffffffffffffffffffffffffffffffff) 
    end,
    ?line send_term_as_binary(P, []), % illegal type
    ok.

send_integers2(P) ->
    ?line send_term_as_binary(P,0),		% SMALL_INTEGER_EXT smallest
    ?line send_term_as_binary(P,255),		% SMALL_INTEGER_EXT largest
    ?line send_term_as_binary(P,256),		% INTEGER_EXT smallest pos (*)
    ?line send_term_as_binary(P,-1),		% INTEGER_EXT largest  neg 
    
    ?line send_term_as_binary(P, 16#07ffffff),	% INTEGER_EXT old largest (28 bits)
    ?line send_term_as_binary(P,-16#08000000),	% INTEGER_EXT old smallest 
    ?line send_term_as_binary(P, 16#08000000),  % SMALL_BIG_EXT old smallest pos(*)
    ?line send_term_as_binary(P,-16#08000001),	% SMALL_BIG_EXT old largest neg (*)

    ?line send_term_as_binary(P, 16#7fffffff),	% INTEGER_EXT new largest (32 bits)
    ?line send_term_as_binary(P,-16#80000000),	% INTEGER_EXT new smallest
    ?line send_term_as_binary(P, 16#80000000),  % SMALL_BIG_EXT new smallest pos(*)
    ?line send_term_as_binary(P,-16#80000001),	% SMALL_BIG_EXT new largest neg (*)

    ?line send_term_as_binary(P, 16#ffffffff),% SMALL_BIG_EXT largest u32

    ?line send_term_as_binary(P, 16#7fffffffffff), % largest  i48
    ?line send_term_as_binary(P,-16#800000000000), % smallest i48
    ?line send_term_as_binary(P, 16#ffffffffffff), % largest  u48
    ?line send_term_as_binary(P, 16#7fffffffffffffff), % largest  i64
    ?line send_term_as_binary(P,-16#8000000000000000), % smallest i64
    ?line send_term_as_binary(P, 16#ffffffffffffffff), % largest  u64
    ?line send_term_as_binary(P, []), % illegal type
    ok.

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
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
				   16#800 =< CP,
				   CP =< 16#FFFF ->
    [16#E0 bor (CP bsr 12),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
				   16#10000 =< CP,
				   CP =< 16#10FFFF ->
    [16#F0 bor (CP bsr 18),
     16#80 bor (16#3F band (CP bsr 12)),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)].
