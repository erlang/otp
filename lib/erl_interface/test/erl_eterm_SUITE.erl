%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(erl_eterm_SUITE).

-include("test_server.hrl").
-include("erl_eterm_SUITE_data/eterm_test_cases.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The tests are organised as follows:
%%%
%%% 1. Basic tests (encoding, decoding, memory allocation).
%%% 2. Constructing terms (the erl_mk_xxx() functions and erl_copy_term()).
%%% 3. Extracting & info functions (erl_hd(), erl_length() etc).
%%% 4. I/O list functions.
%%% 5. Miscellanous functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([all/1, build_terms/1, round_trip_conversion/1,
	 decode_terms/1, decode_float/1,
	 t_erl_mk_int/1, t_erl_mk_list/1,
	 basic_copy/1,
	 t_erl_cons/1,
	 t_erl_mk_atom/1,
	 t_erl_mk_binary/1,
	 t_erl_mk_empty_list/1,
	 t_erl_mk_float/1,
	 t_erl_mk_pid/1,
	 t_erl_mk_xpid/1,
	 t_erl_mk_port/1,
	 t_erl_mk_xport/1,
	 t_erl_mk_ref/1,
	 t_erl_mk_long_ref/1,
	 t_erl_mk_string/1,
	 t_erl_mk_estring/1,
	 t_erl_mk_tuple/1,
	 t_erl_mk_uint/1,
	 t_erl_mk_var/1,
	 t_erl_size/1,
	 t_erl_var_content/1,
	 t_erl_element/1,
	 t_erl_length/1, t_erl_hd/1, t_erl_tl/1,
	 type_checks/1, extractor_macros/1,
	 t_erl_iolist_length/1, t_erl_iolist_to_binary/1,
	 t_erl_iolist_to_string/1,
	 erl_print_term/1, print_string/1,
	 t_erl_free_compound/1,
	 high_chaparal/1,
	 broken_data/1,
	 cnode_1/1]).

-export([start_cnode/1]).

-import(runner, [get_term/1]).

%% This test suite controls the running of the C language functions
%% in eterm_test.c and print_term.c.

all(suite) -> [build_terms, round_trip_conversion,
	       decode_terms, decode_float,
	       t_erl_mk_int, t_erl_mk_list,
	       basic_copy,
	       t_erl_mk_atom,
	       t_erl_mk_binary,
	       t_erl_mk_empty_list,
	       t_erl_mk_float,
	       t_erl_mk_pid,
	       t_erl_mk_xpid,
	       t_erl_mk_port,
	       t_erl_mk_xport,
	       t_erl_mk_ref,
	       t_erl_mk_long_ref,
	       t_erl_mk_string,
	       t_erl_mk_estring,
	       t_erl_mk_tuple,
	       t_erl_mk_uint,
	       t_erl_mk_var,
	       t_erl_size,
	       t_erl_var_content,
	       t_erl_element,
	       t_erl_cons,
	       t_erl_length, t_erl_hd, t_erl_tl,
	       type_checks, extractor_macros,
	       t_erl_iolist_length, t_erl_iolist_to_binary,
	       t_erl_iolist_to_string,
	       erl_print_term, print_string,
	       t_erl_free_compound,
	       high_chaparal,
	       broken_data,
	       cnode_1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	1.   B a s i c    t e s t s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test asks the C function to construct all data types in
%% a list and verifies that the result is as expected.

build_terms(suite) -> [];
build_terms(Config) when is_list(Config) ->
    ?line P = runner:start(?build_terms),
    ?line {term, Term} = get_term(P),
    ?line io:format("Received: ~p", [Term]),
    ?line [ARefLN, ARef, APortLN, APort, APidLN, APid,
	   {element1, 42, 767}, "A string",
	   1, -1, 0, 3.0, ABin, 'I am an atom'] = Term,
    ?line "A binary" = binary_to_list(ABin),
    ?line case ARef of
	      R when is_reference(R), node(R) == kalle@localhost -> ok
	  end,
    ?line case ARefLN of
	      R1 when is_reference(R1), node(R1) == abcdefghijabcdefghij@localhost -> ok
	  end,
    ?line case APort of
	      Port when is_port(Port), node(Port) == kalle@localhost -> ok
	  end,
    ?line case APortLN of
	      Port1 when is_port(Port1), node(Port1) == abcdefghijabcdefghij@localhost -> ok
	  end,
    ?line case APid of
	      Pid when is_pid(Pid), node(Pid) == kalle@localhost -> ok
	  end,
    ?line case APidLN of
	      Pid1 when is_pid(Pid1), node(Pid1) == abcdefghijabcdefghij@localhost -> ok
	  end,

    ?line runner:recv_eot(P),
    ok.

%% This test is run entirely in C code.

round_trip_conversion(suite) -> [];
round_trip_conversion(Config) when is_list(Config) ->
    ?line runner:test(?round_trip_conversion),
    ok.

%% This test sends a list of all data types to the C code function,
%% which decodes it and verifies it.

decode_terms(suite) -> [];
decode_terms(Config) when is_list(Config) ->
    ?line Dummy1 = list_to_atom(filename:join(?config(priv_dir, Config),
					      dummy_file1)),
    ?line Dummy2 = list_to_atom(filename:join(?config(priv_dir, Config),
					      dummy_file2)),
    ?line Port1 = open_port(Dummy1, [out]),
    ?line Port2 = open_port(Dummy2, [out]),
    ?line ABinary = list_to_binary("A binary"),
    ?line Terms = [make_ref(), make_ref(),
		   Port1, Port2,
		   self(), self(),
		   {element1, 42, 767}, "A string",
		   1, -1, 0, 3.0, ABinary, 'I am an atom'],

    ?line P = runner:start(?decode_terms),
    ?line runner:send_term(P, Terms),
    ?line runner:recv_eot(P),

    ok.

%% Decodes the floating point number 3.1415.

decode_float(suite) -> [];
decode_float(Config) when is_list(Config) ->
    ?line P = runner:start(?decode_float),
    ?line runner:send_term(P, 3.1415),
    ?line runner:recv_eot(P),
    ok.

%% Tests the erl_free_compound() function.

t_erl_free_compound(suite) -> [];
t_erl_free_compound(Config) when is_list(Config) ->
    ?line runner:test(?t_erl_free_compound),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	2.   C o n s t r u c t i n g   t e r m s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This tests the erl_mk_list() function.

t_erl_mk_list(suite) -> [];
t_erl_mk_list(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_list),

    ?line {term, []} = get_term(P),
    ?line {term, [abc]} = get_term(P),
    ?line {term, [abcdef, 42]} = get_term(P),
    ?line {term, [0.0, 23, [], 3.1415]} = get_term(P),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_int() function.

t_erl_mk_int(suite) -> [];
t_erl_mk_int(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_int),
    
    ?line {term, 0} = get_term(P),
    ?line {term, 127} = get_term(P),
    ?line {term, 128} = get_term(P),
    ?line {term, 255} = get_term(P),
    ?line {term, 256} = get_term(P),

    ?line {term, 16#FFFF} = get_term(P),
    ?line {term, 16#10000} = get_term(P),

    ?line {term, 16#07FFFFFF} = get_term(P),
    ?line {term, 16#0FFFFFFF} = get_term(P),
    ?line {term, 16#1FFFFFFF} = get_term(P),
    ?line {term, 16#3FFFFFFF} = get_term(P),
    ?line {term, 16#7FFFFFFF} = get_term(P),

    ?line {term, 16#08000000} = get_term(P),
    ?line {term, 16#10000000} = get_term(P),
    ?line {term, 16#20000000} = get_term(P),
    ?line {term, 16#40000000} = get_term(P),


    ?line {term, -16#07FFFFFF} = get_term(P),
    ?line {term, -16#0FFFFFFF} = get_term(P),
    ?line {term, -16#1FFFFFFF} = get_term(P),
    ?line {term, -16#3FFFFFFF} = get_term(P),
    ?line {term, -16#7FFFFFFF} = get_term(P),

    ?line {term, -16#08000000} = get_term(P),
    ?line {term, -16#10000000} = get_term(P),
    ?line {term, -16#20000000} = get_term(P),
    ?line {term, -16#40000000} = get_term(P),

    ?line {term, -16#08000001} = get_term(P),
    ?line {term, -16#10000001} = get_term(P),
    ?line {term, -16#20000001} = get_term(P),
    ?line {term, -16#40000001} = get_term(P),

    ?line {term, -16#08000002} = get_term(P),
    ?line {term, -16#10000002} = get_term(P),
    ?line {term, -16#20000002} = get_term(P),
    ?line {term, -16#40000002} = get_term(P),

    ?line {term, -1999999999} = get_term(P),
    ?line {term, -2000000000} = get_term(P),
    ?line {term, -2000000001} = get_term(P),

    ?line runner:recv_eot(P),
    ok.


%% Basic test of erl_copy_term().

basic_copy(suite) -> [];
basic_copy(Config) when is_list(Config) ->
    ?line runner:test(?basic_copy),
    ok.


%% This tests the erl_mk_tuple() function.

t_erl_mk_tuple(suite) -> [];
t_erl_mk_tuple(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_tuple),

    ?line {term, {madonna, 21, 'mad donna', 12}} = get_term(P),
    ?line {term, {'Madonna',21,{children,{"Isabella",2}},
		  {'home page',"http://www.madonna.com/"}}} = get_term(P),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_atom() function.

t_erl_mk_atom(suite) -> [];
t_erl_mk_atom(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_atom),

    ?line {term, madonna} = (get_term(P)),
    ?line {term, 'Madonna'} = (get_term(P)),
    ?line {term, 'mad donna'} = (get_term(P)),
    ?line {term, '_madonna_'} = (get_term(P)),
    ?line {term, '/home/madonna/tour_plan'} = (get_term(P)),
    ?line {term, 'http://www.madonna.com/tour_plan'} = (get_term(P)),
    ?line {term, '\'madonna\''} = (get_term(P)),
    ?line {term, '\"madonna\"'} = (get_term(P)),
    ?line {term, '\\madonna\\'} = (get_term(P)),
    ?line {term, '{madonna,21,\'mad donna\',12}'} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_binary() function.

t_erl_mk_binary(suite) -> [];
t_erl_mk_binary(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_binary),

    ?line {term, Bin} = (get_term(P)),
    ?line "{madonna,21,'mad donna',1234.567.890, !#$%&/()=?+-@, \" \\}" = 
	binary_to_list(Bin),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_empty_list() function.

t_erl_mk_empty_list(suite) -> [];
t_erl_mk_empty_list(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_empty_list),

    ?line {term, []} = get_term(P),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_float() function.

t_erl_mk_float(suite) -> [];
t_erl_mk_float(Config) when is_list(Config) ->
    case os:type() of 
	vxworks ->
	    {skipped, "Floating point numbers never compare equal on PPC"};
	_ ->
	    ?line P = runner:start(?t_erl_mk_float),
	    ?line {term, {3.1415, 1.999999, 2.000000, 2.000001, 
			  2.000002, 12345.67890}} = 
		get_term(P),
	    ?line runner:recv_eot(P),
	    ok
    end.


%% This tests the erl_mk_pid() function.

t_erl_mk_pid(suite) -> [];
t_erl_mk_pid(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_pid),

    ?line {term, A_pid} = (get_term(P)),
    ?line {pid, kalle@localhost, 3, 2} = nc2vinfo(A_pid),

    ?line runner:recv_eot(P),
    ok.

t_erl_mk_xpid(suite) -> [];
t_erl_mk_xpid(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_xpid),

    ?line {term, A_pid} = (get_term(P)),
    ?line {pid, kalle@localhost, 32767, 8191} = nc2vinfo(A_pid),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_port() function.

t_erl_mk_port(suite) -> [];
t_erl_mk_port(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_port),

    ?line {term, A_port} = (get_term(P)),
    ?line {port, kalle@localhost, 4} = nc2vinfo(A_port),

    ?line runner:recv_eot(P),
    ok.

t_erl_mk_xport(suite) -> [];
t_erl_mk_xport(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_xport),

    ?line {term, A_port} = (get_term(P)),
    ?line {port, kalle@localhost, 268435455} = nc2vinfo(A_port),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_ref() function.

t_erl_mk_ref(suite) -> [];
t_erl_mk_ref(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_ref),

    ?line {term, A_ref} = (get_term(P)),
    ?line {ref, kalle@localhost, _Length, [6]} = nc2vinfo(A_ref),

    ?line runner:recv_eot(P),
    ok.

t_erl_mk_long_ref(suite) -> [];
t_erl_mk_long_ref(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_long_ref),

    ?line {term, A_ref} = (get_term(P)),
    ?line {ref, kalle@localhost, _Length, [4294967295,4294967295,262143]}
	= nc2vinfo(A_ref),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_string() function.

t_erl_mk_string(suite) -> [];
t_erl_mk_string(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_string),

    ?line {term, "madonna"} = (get_term(P)),
    ?line {term, "Madonna"} = (get_term(P)),
    ?line {term, "mad donna"} = (get_term(P)),
    ?line {term, "_madonna_"} = (get_term(P)),
    ?line {term, "/home/madonna/tour_plan"} = (get_term(P)),
    ?line {term, "http://www.madonna.com/tour_plan"} = (get_term(P)),
    ?line {term, "\'madonna\'"} = (get_term(P)),
    ?line {term, "\"madonna\""} = (get_term(P)),
    ?line {term, "\\madonna\\"} = (get_term(P)),
    ?line {term, "{madonna,21,'mad donna',12}"} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_estring() function.

t_erl_mk_estring(suite) -> [];
t_erl_mk_estring(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_estring),

    ?line {term, "madonna"} = (get_term(P)),
    ?line {term, "Madonna"} = (get_term(P)),
    ?line {term, "mad donna"} = (get_term(P)),
    ?line {term, "_madonna_"} = (get_term(P)),
    ?line {term, "/home/madonna/tour_plan"} = (get_term(P)),
    ?line {term, "http://www.madonna.com/tour_plan"} = (get_term(P)),
    ?line {term, "\'madonna\'"} = (get_term(P)),
    ?line {term, "\"madonna\""} = (get_term(P)),
    ?line {term, "\\madonna\\"} = (get_term(P)),
    ?line {term, "{madonna,21,'mad donna',12}"} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_uint() function.

t_erl_mk_uint(suite) -> [];
t_erl_mk_uint(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_uint),

    ?line {term, 54321} = (get_term(P)),
    ?line {term, 2147483647} = (get_term(P)),
    ?line {term, 2147483648} = (get_term(P)),
    ?line {term, 2147483649} = (get_term(P)),
    ?line {term, 2147483650} = (get_term(P)),
    ?line {term, 4294967295} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_mk_var() function.

t_erl_mk_var(suite) -> [];
t_erl_mk_var(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_mk_var),

    ?line {term, 1} = (get_term(P)),
    ?line {term, 0} = (get_term(P)),
    ?line {term, 1} = (get_term(P)),
    ?line {term, 0} = (get_term(P)),
    ?line {term, 1} = (get_term(P)),
    ?line {term, 0} = (get_term(P)),
    ?line {term, 1} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_cons() function.

t_erl_cons(suite) -> [];
t_erl_cons(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_cons),

    ?line {term, [madonna, 21]} = get_term(P),

    ?line runner:recv_eot(P),
    ok.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	3.   E x t r a c t i n g  &   i n f o    f u n c t i o n s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the erl_length() function.

t_erl_length(suite) -> [];
t_erl_length(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_length),

    ?line 0 = erl_length(P, []),
    ?line 1 = erl_length(P, [a]),
    ?line 2 = erl_length(P, [a, b]),
    ?line 3 = erl_length(P, [a, b, c]),

    ?line 4 = erl_length(P, [a, [x, y], c, []]),

    ?line -1 = erl_length(P, [a|b]),
    ?line -1 = erl_length(P, a),

    ?line runner:finish(P),
    ok.

%% Invokes the erl_length() function.

erl_length(Port, List) ->
    call_erl_function(Port, List).

%% Tests the erl_hd() function.

t_erl_hd(suite) -> [];
t_erl_hd(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_hd),
    
    ?line 'NULL' = erl_hd(P, 42),
    ?line 'NULL' = erl_hd(P, abc),
    ?line 'NULL' = erl_hd(P, []),

    ?line [] = erl_hd(P, [[], a]),
    ?line a = erl_hd(P, [a]),
    ?line a = erl_hd(P, [a, b]),
    ?line a = erl_hd(P, [a, b, c]),
    ?line a = erl_hd(P, [a|b]),

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

%% Invokes the erl_hd() function.

erl_hd(Port, List) ->
    call_erl_function(Port, List).

%% Tests the erl_tail() function.

t_erl_tl(suite) -> [];
t_erl_tl(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_tl),

    ?line 'NULL' = erl_tl(P, 42),
    ?line 'NULL' = erl_tl(P, abc),
    ?line 'NULL' = erl_tl(P, []),

    ?line [] = erl_tl(P, [a]),
    ?line [b] = erl_tl(P, [a, b]),
    ?line [b, c] = erl_tl(P, [a, b, c]),

    ?line b = erl_tl(P, [a|b]),

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

%% Invokes the erl_tail() function in erl_interface.

erl_tl(Port, List) ->
    call_erl_function(Port, List).

%% Tests the type checking macros (done in the C program).

type_checks(suite) -> [];
type_checks(Config) when is_list(Config) ->
    ?line runner:test(?type_checks),
    ok.

%% Tests the extractor macros (done in the C program).

extractor_macros(suite) -> [];
extractor_macros(Config) when is_list(Config) ->
    ?line runner:test(?extractor_macros),
    ok.


%% This tests the erl_size() function.

t_erl_size(suite) -> [];
t_erl_size(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_size),

    ?line {term, 0} = (get_term(P)),
    ?line {term, 4} = (get_term(P)),

    ?line {term, 0} = (get_term(P)),
    ?line {term, 27} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_var_content() function.

t_erl_var_content(suite) -> [];
t_erl_var_content(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_var_content),

    ?line {term, 17} = (get_term(P)),
    ?line {term, "http://www.madonna.com"} = (get_term(P)),
    ?line {term, 2} = (get_term(P)),
    ?line {term, "http://www.madonna.com"} = (get_term(P)),
    ?line {term, 2} = (get_term(P)),

    ?line runner:recv_eot(P),
    ok.


%% This tests the erl_element() function.

t_erl_element(suite) -> [];
t_erl_element(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_element),

    ?line {term, madonna} = get_term(P),
    ?line {term, 21} = get_term(P),
    ?line {term, 'mad donna'} = get_term(P),
    ?line {term, 12} = get_term(P),

    ?line {term, 'Madonna'} = get_term(P),
    ?line {term, 21} = get_term(P),
    ?line {term, {children,{"Isabella",2}}} = get_term(P),
    ?line {term, {'home page',"http://www.madonna.com/"}} = get_term(P),

    ?line runner:recv_eot(P),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	4.   I / O   l i s t   f u n c t i o n s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the erl_iolist_length() function.

t_erl_iolist_length(suite) -> [];
t_erl_iolist_length(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_iolist_length),

    %% Flat lists.

    ?line 0 = erl_iolist_length(P, []),
    ?line 1 = erl_iolist_length(P, [10]),
    ?line 2 = erl_iolist_length(P, [10, 20]),
    ?line 3 = erl_iolist_length(P, [10, 20, 30]),
    ?line 256 = erl_iolist_length(P, lists:seq(0, 255)),

    %% Deep lists.

    ?line 0 = erl_iolist_length(P, [[]]),
    ?line 1 = erl_iolist_length(P, [[], 42]),
    ?line 1 = erl_iolist_length(P, [42, []]),
    ?line 2 = erl_iolist_length(P, [42, [], 45]),

    ?line 3 = erl_iolist_length(P, [42, [90], 45]),
    ?line 3 = erl_iolist_length(P, [[42, [90]], 45]),
    ?line 3 = erl_iolist_length(P, [[42, [90]], 45]),

    %% List with binaries.

    ?line 0 = erl_iolist_length(P, [list_to_binary([])]),
    ?line 0 = erl_iolist_length(P, [[], list_to_binary([])]),
    ?line 1 = erl_iolist_length(P, [[1], list_to_binary([])]),
    ?line 1 = erl_iolist_length(P, [[], list_to_binary([2])]),
    ?line 2 = erl_iolist_length(P, [[42], list_to_binary([2])]),
    ?line 4 = erl_iolist_length(P, [[42], list_to_binary([2, 3, 4])]),

    %% Binaries as tail.

    ?line 0 = erl_iolist_length(P, [[]| list_to_binary([])]),
    ?line 1 = erl_iolist_length(P, [[1]| list_to_binary([])]),
    ?line 1 = erl_iolist_length(P, [[]| list_to_binary([2])]),
    ?line 2 = erl_iolist_length(P, [[42]| list_to_binary([2])]),

    %% Binaries only.

    ?line 0 = erl_iolist_length(P, list_to_binary("")),
    ?line 1 = erl_iolist_length(P, list_to_binary([1])),
    ?line 2 = erl_iolist_length(P, list_to_binary([1, 2])),

    %% Illegal cases.

    ?line -1 = erl_iolist_length(P, [42|43]),
    ?line -1 = erl_iolist_length(P, a),

    ?line -1 = erl_iolist_length(P, [a]),
    ?line -1 = erl_iolist_length(P, [256]),
    ?line -1 = erl_iolist_length(P, [257]),
    ?line -1 = erl_iolist_length(P, [-1]),
    ?line -1 = erl_iolist_length(P, [-2]),
    ?line -1 = erl_iolist_length(P, [-127]),
    ?line -1 = erl_iolist_length(P, [-128]),

    ?line runner:finish(P),
    ok.

%% Invokes the erl_iolist_length() function.

erl_iolist_length(Port, List) ->
    call_erl_function(Port, List).

%% Tests the erl_iolist_to_binary() function.

t_erl_iolist_to_binary(suite) -> [];
t_erl_iolist_to_binary(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_iolist_to_binary),

    %% Flat lists.

    ?line [] = iolist_to_list(P, []),
    ?line [10] = iolist_to_list(P, [10]),
    ?line [10, 20] = iolist_to_list(P, [10, 20]),
    ?line [10, 20, 30] = iolist_to_list(P, [10, 20, 30]),
    ?line AllBytes = lists:seq(0, 255),
    ?line AllBytes = iolist_to_list(P, AllBytes),

    %% Deep lists.

    ?line [] = iolist_to_list(P, [[]]),
    ?line [42] = iolist_to_list(P, [[], 42]),
    ?line [42] = iolist_to_list(P, [42, []]),
    ?line [42, 45] = iolist_to_list(P, [42, [], 45]),

    ?line [42, 90, 45] = iolist_to_list(P, [42, [90], 45]),
    ?line [42, 90, 45] = iolist_to_list(P, [[42, [90]], 45]),
    ?line [42, 90, 45] = iolist_to_list(P, [[42, [90]], 45]),

    %% List with binaries.

    ?line [] = iolist_to_list(P, [list_to_binary([])]),
    ?line [] = iolist_to_list(P, [[], list_to_binary([])]),
    ?line [1] = iolist_to_list(P, [[1], list_to_binary([])]),
    ?line [2] = iolist_to_list(P, [[], list_to_binary([2])]),
    ?line [42, 2] = iolist_to_list(P, [[42], list_to_binary([2])]),
    ?line [42, 2, 3, 4] = iolist_to_list(P, [[42], list_to_binary([2, 3, 4])]),

    %% Binaries as tail.

    ?line [] = iolist_to_list(P, [[]| list_to_binary([])]),
    ?line [1] = iolist_to_list(P, [[1]| list_to_binary([])]),
    ?line [2] = iolist_to_list(P, [[]| list_to_binary([2])]),
    ?line [42, 2] = iolist_to_list(P, [[42]| list_to_binary([2])]),

    %% Binaries only.

    ?line [] = iolist_to_list(P, list_to_binary("")),
    ?line [1] = iolist_to_list(P, list_to_binary([1])),
    ?line [1, 2] = iolist_to_list(P, list_to_binary([1, 2])),

    %% Illegal cases.

    ?line 'NULL' = iolist_to_list(P, [42|43]),
    ?line 'NULL' = iolist_to_list(P, a),

    ?line 'NULL' = iolist_to_list(P, [a]),
    ?line 'NULL' = iolist_to_list(P, [256]),
    ?line 'NULL' = iolist_to_list(P, [257]),
    ?line 'NULL' = iolist_to_list(P, [-1]),
    ?line 'NULL' = iolist_to_list(P, [-2]),
    ?line 'NULL' = iolist_to_list(P, [-127]),
    ?line 'NULL' = iolist_to_list(P, [-128]),

    ?line runner:finish(P),
    ok.

iolist_to_list(Port, Term) ->
    case call_erl_function(Port, Term) of
	'NULL' ->
	    'NULL';
	Bin when is_binary(Bin) ->
	    binary_to_list(Bin)
    end.

%% Tests the erl_iolist_to_string() function.

t_erl_iolist_to_string(suite) -> [];
t_erl_iolist_to_string(Config) when is_list(Config) ->
    ?line P = runner:start(?t_erl_iolist_to_string),

    %% Flat lists.

    ?line [0] = iolist_to_string(P, []),
    ?line [10, 0] = iolist_to_string(P, [10]),
    ?line [10, 20, 0] = iolist_to_string(P, [10, 20]),
    ?line [10, 20, 30, 0] = iolist_to_string(P, [10, 20, 30]),
    ?line AllBytes = lists:seq(1, 255)++[0],
    ?line AllBytes = iolist_to_string(P, lists:seq(1, 255)),

    %% Deep lists.

    ?line [0] = iolist_to_string(P, [[]]),
    ?line [42, 0] = iolist_to_string(P, [[], 42]),
    ?line [42, 0] = iolist_to_string(P, [42, []]),
    ?line [42, 45, 0] = iolist_to_string(P, [42, [], 45]),

    ?line [42, 90, 45, 0] = iolist_to_string(P, [42, [90], 45]),
    ?line [42, 90, 45, 0] = iolist_to_string(P, [[42, [90]], 45]),
    ?line [42, 90, 45, 0] = iolist_to_string(P, [[42, [90]], 45]),

    %% List with binaries.

    ?line [0] = iolist_to_string(P, [list_to_binary([])]),
    ?line [0] = iolist_to_string(P, [[], list_to_binary([])]),
    ?line [1, 0] = iolist_to_string(P, [[1], list_to_binary([])]),
    ?line [2, 0] = iolist_to_string(P, [[], list_to_binary([2])]),
    ?line [42, 2, 0] = iolist_to_string(P, [[42], list_to_binary([2])]),
    ?line [42, 2, 3, 4, 0] = iolist_to_string(P, [[42],
						  list_to_binary([2, 3, 4])]),

    %% Binaries as tail.

    ?line [0] = iolist_to_string(P, [[]| list_to_binary([])]),
    ?line [1, 0] = iolist_to_string(P, [[1]| list_to_binary([])]),
    ?line [2, 0] = iolist_to_string(P, [[]| list_to_binary([2])]),
    ?line [42, 2, 0] = iolist_to_string(P, [[42]| list_to_binary([2])]),

    %% Binaries only.

    ?line [0] = iolist_to_string(P, list_to_binary("")),
    ?line [1, 0] = iolist_to_string(P, list_to_binary([1])),
    ?line [1, 2, 0] = iolist_to_string(P, list_to_binary([1, 2])),

    %% Illegal cases.

    ?line 'NULL' = iolist_to_string(P, [0]),
    ?line 'NULL' = iolist_to_string(P, [65, 0, 66]),
    ?line 'NULL' = iolist_to_string(P, [65, 66, 67, 0]),

    ?line 'NULL' = iolist_to_string(P, [42|43]),
    ?line 'NULL' = iolist_to_string(P, a),

    ?line 'NULL' = iolist_to_string(P, [a]),
    ?line 'NULL' = iolist_to_string(P, [256]),
    ?line 'NULL' = iolist_to_string(P, [257]),
    ?line 'NULL' = iolist_to_string(P, [-1]),
    ?line 'NULL' = iolist_to_string(P, [-2]),
    ?line 'NULL' = iolist_to_string(P, [-127]),
    ?line 'NULL' = iolist_to_string(P, [-128]),

    ?line runner:finish(P),
    ok.

%% Invokes the erl_iolist_to_string() function.

iolist_to_string(Port, Term) ->
    runner:send_term(Port, Term),
    case get_term(Port) of
	{bytes, Result} -> Result;
	'NULL'       -> 'NULL'
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	5.   M i s c e l l a n o u s   T e s t s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

erl_print_term(suite) -> [];
erl_print_term(doc) -> "Tests the erl_print_term() function";
erl_print_term(Config) when is_list(Config) ->
    ?line PrintTerm = print_term(Config),
    ?line P = open_port({spawn, PrintTerm}, [stream]),

    %% Lists.

    ?line print(P, "[]", []),
    ?line print(P, "[a]", [a]),
    ?line print(P, "[[a]]", [[a]]),
    ?line print(P, "[[]]", [[]]),
    ?line print(P, "[a,b,c]", [a,b,c]),
    ?line print(P, "[a,b|c]", [a,b|c]),
    ?line print(P, "[a,[],c]", [a,[],c]),
    ?line print(P, "[a,[1000,1],c]", [a,[1000,1],c]),

    %% Tuples.

    ?line print(P, "{}", {}),
    ?line print(P, "{ok}", {ok}),
    ?line print(P, "{1,2,3}", {1, 2, 3}),

    %% Pids.

    ?line {_X, Y, Z} = split_pid(self()),
    ?line PidString = lists:flatten(io_lib:format("<~s.~w.~w>",
						  [node(), Y, Z])),
    ?line print(P, PidString, self()),

    ?line unlink(P),
    ?line exit(P, die),
    ok.

split_pid(Pid) when is_pid(Pid) ->
    split_pid(pid_to_list(Pid), 0, []).

split_pid([$<|Rest], Cur, Result) ->
    split_pid(Rest, Cur, Result);
split_pid([Digit|Rest], Cur, Result) when $0 =< Digit, Digit =< $9 ->
    split_pid(Rest, 10*Cur+Digit-$0, Result);
split_pid([$.|Rest], Cur, Result) ->
    split_pid(Rest, 0, Result++[Cur]);
split_pid([$>], Cur, Result) ->
    list_to_tuple(Result++[Cur]).

print_string(suite) -> [];
print_string(doc) -> "Test printing a string with erl_print_term()";
print_string(Config) when is_list(Config) ->
    ?line PrintTerm = print_term(Config),
    ?line P = open_port({spawn, PrintTerm}, [stream]),

    %% Strings.

    ?line print(P, "\"ABC\"", "ABC"),
    ?line {11, "\"\\tABC\\r\\n\""} = print(P, "\tABC\r\n"),

    %% Not strings.

    ?line print(P, "[65,66,67,0]", "ABC\000"),

    ?line unlink(P),
    ?line exit(P, die),
    ok.

print(Port, TermString, Term) ->
    Length = length(TermString),
    {Length, TermString} = print(Port, Term).

%% This function uses the erl_print_term() function in erl_interface
%% to print a term.
%% Returns: {NumChars, Chars}

print(Port, Term) ->
    Bin = term_to_binary(Term),
    Size = size(Bin),
    Port ! {self(), {command, [Size div 256, Size rem 256, Bin]}},
    collect_line(Port, []).

collect_line(Port, Result) ->
    receive
	{Port, {data, Data}} ->
	    case lists:reverse(Data) of
		[$\n|Rest] ->
		    collect_line1(Rest++Result, []);
		Chars ->
		    collect_line(Port, Chars++Result)
	    end
    after test_server:seconds(5) ->
	    test_server:fail("No response from C program")
    end.

collect_line1([$\r|Rest], Result) ->
    {list_to_integer(Result), lists:reverse(Rest)};
collect_line1([C|Rest], Result) ->
    collect_line1(Rest, [C|Result]).

%% Test case submitted by Per Lundgren, ERV.

high_chaparal(suite) -> [];
high_chaparal(Config) when is_list(Config) ->
    ?line P = runner:start(?high_chaparal),
    ?line {term, [hello, world]} = get_term(P),
    ?line runner:recv_eot(P),
    ok.

%% OTP-7448
broken_data(suite) -> [];
broken_data(Config) when is_list(Config) ->
    ?line P = runner:start(?broken_data),
    ?line runner:recv_eot(P),
    ok.

%% This calls a C function with one parameter and returns the result.

call_erl_function(Port, Term) ->
    runner:send_term(Port, Term),
    case get_term(Port) of
	{term, Result} -> Result;
	'NULL'       -> 'NULL'
    end.

print_term(Config) when is_list(Config) ->
    filename:join(?config(data_dir, Config), "print_term").



%%% We receive a ref from the cnode, and expect it to be a long ref.
%%% We also send a ref we created ourselves, and expect to get it
%%% back, without having been mutated into short form. We must take
%%% care then to check the actual returned ref, and not the original
%%% one, which is equal to it.
cnode_1(suite) -> [];
cnode_1(doc) -> "Tests involving cnode: sends a long ref from a cnode to us";
cnode_1(Config) when is_list(Config) ->
    ?line Cnode = filename:join(?config(data_dir, Config), "cnode"),
    ?line register(mip, self()),
    ?line spawn_link(?MODULE, start_cnode, [Cnode]),
    ?line Ref1 = get_ref(),
    io:format("Ref1 ~p~n", [Ref1]),
    ?line check_ref(Ref1),
    ?line Ref2 = make_ref(),
    ?line receive
	      Pid -> Pid
	  end,
    ?line Fun1 = fun(X) -> {Pid, X} end,	% sneak in a fun test here
    %?line Fun1 = {wait_with_funs, new_dist_format},
    ?line Term = {Ref2, Fun1, {1,2,3,4,5,6,7,8,9,10}},
    %% A term which will overflow the original buffer used in 'cnode'.
    ?line Pid ! Term,
    ?line receive
	      Term2 ->
		  io:format("received ~p~n", [Term2]),
		  case Term2 of
		      Term ->
			  {Ref22,_,_} = Term2,
			  ?line check_ref(Ref22);
		      X ->
		      test_server:fail({receive1,X})
		  end
	  after 5000 ->
		  test_server:fail(receive1)
	  end,
    ?line receive
	      Pid ->
		  ok;
	      Y ->
		  test_server:fail({receive1,Y})
	  after 5000 ->
		  test_server:fail(receive2)
	  end,
    ?line io:format("ref = ~p~n", [Ref1]),
    ?line check_ref(Ref1),
    ok.

check_ref(Ref) ->
    case bin_ext_type(Ref) of
	101 ->
	    test_server:fail(oldref);
	114 ->
	    ok;
	Type ->
	    test_server:fail({type, Type})
    end.

bin_ext_type(T) ->
    [131, Type | _] = binary_to_list(term_to_binary(T)),
    Type.

get_ref() ->
    receive
	X when is_reference(X) ->
	    X
    after 5000 ->
	    test_server:fail({cnode, timeout})
    end.

start_cnode(Cnode) ->
    open_port({spawn, Cnode ++ " " ++ atom_to_list(erlang:get_cookie())}, []),
    rec_cnode().

rec_cnode() ->
    receive
	X ->
	    io:format("from cnode: ~p~n", [X]),
	    rec_cnode()
    end.

nc2vinfo(Pid) when is_pid(Pid) ->
    ?line [_NodeStr, NumberStr, SerialStr]
	= string:tokens(pid_to_list(Pid), "<.>"),
    ?line Number = list_to_integer(NumberStr),
    ?line Serial = list_to_integer(SerialStr),
    ?line {pid, node(Pid), Number, Serial};
nc2vinfo(Port) when is_port(Port) ->
    ?line ["#Port", _NodeStr, NumberStr]
	= string:tokens(erlang:port_to_list(Port), "<.>"),
    ?line Number = list_to_integer(NumberStr),
    ?line {port, node(Port), Number};    
nc2vinfo(Ref) when is_reference(Ref) ->
    ?line ["#Ref", _NodeStr | NumStrList]
	= string:tokens(erlang:ref_to_list(Ref), "<.>"),
    ?line {Len, RevNumList} = lists:foldl(fun ("0", {N, []}) ->
						  {N+1, []};
					      (IStr, {N, Is}) ->
						  {N+1,
						   [list_to_integer(IStr)|Is]}
					  end,
					  {0, []},
					  NumStrList),
    ?line {ref, node(Ref), Len, lists:reverse(RevNumList)};
nc2vinfo(Other) ->
    ?line {badarg, Other}.


