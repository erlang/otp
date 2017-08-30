%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(erl_eterm_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erl_eterm_SUITE_data/eterm_test_cases.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The tests are organised as follows:
%%%
%%% 1. Basic tests (encoding, decoding, memory allocation).
%%% 2. Constructing terms (the erl_mk_xxx() functions and erl_copy_term()).
%%% 3. Extracting & info functions (erl_hd(), erl_length() etc).
%%% 4. I/O list functions.
%%% 5. Miscellaneous functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([all/0, suite/0,
         build_terms/1, round_trip_conversion/1,
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

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [build_terms, round_trip_conversion, decode_terms,
     decode_float, t_erl_mk_int, t_erl_mk_list, basic_copy,
     t_erl_mk_atom, t_erl_mk_binary, t_erl_mk_empty_list,
     t_erl_mk_float, t_erl_mk_pid, t_erl_mk_xpid,
     t_erl_mk_port, t_erl_mk_xport, t_erl_mk_ref,
     t_erl_mk_long_ref, t_erl_mk_string, t_erl_mk_estring,
     t_erl_mk_tuple, t_erl_mk_uint, t_erl_mk_var, t_erl_size,
     t_erl_var_content, t_erl_element, t_erl_cons,
     t_erl_length, t_erl_hd, t_erl_tl, type_checks,
     extractor_macros, t_erl_iolist_length,
     t_erl_iolist_to_binary, t_erl_iolist_to_string,
     erl_print_term, print_string, t_erl_free_compound,
     high_chaparal, broken_data, cnode_1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	1.   B a s i c    t e s t s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test asks the C function to construct all data types in
%% a list and verifies that the result is as expected.

build_terms(Config) when is_list(Config) ->
    P = runner:start(?build_terms),
    {term, Term} = get_term(P),
    io:format("Received: ~p", [Term]),
    [ARefLN, ARef, APortLN, APort, APidLN, APid,
     {element1, 42, 767}, "A string",
     1, -1, 0, 3.0, ABin, 'I am an atom'] = Term,
    "A binary" = binary_to_list(ABin),
    case ARef of
        R when is_reference(R), node(R) == kalle@localhost -> ok
    end,
    case ARefLN of
        R1 when is_reference(R1), node(R1) == abcdefghijabcdefghij@localhost -> ok
    end,
    case APort of
        Port when is_port(Port), node(Port) == kalle@localhost -> ok
    end,
    case APortLN of
        Port1 when is_port(Port1), node(Port1) == abcdefghijabcdefghij@localhost -> ok
    end,
    case APid of
        Pid when is_pid(Pid), node(Pid) == kalle@localhost -> ok
    end,
    case APidLN of
        Pid1 when is_pid(Pid1), node(Pid1) == abcdefghijabcdefghij@localhost -> ok
    end,

    runner:recv_eot(P),
    ok.

%% This test is run entirely in C code.

round_trip_conversion(Config) when is_list(Config) ->
    runner:test(?round_trip_conversion),
    ok.

%% This test sends a list of all data types to the C code function,
%% which decodes it and verifies it.

decode_terms(Config) when is_list(Config) ->
    Dummy1 = list_to_atom(filename:join(proplists:get_value(priv_dir, Config),
                                        dummy_file1)),
    Dummy2 = list_to_atom(filename:join(proplists:get_value(priv_dir, Config),
                                        dummy_file2)),
    Port1 = open_port(Dummy1, [out]),
    Port2 = open_port(Dummy2, [out]),
    ABinary = list_to_binary("A binary"),
    Terms = [make_ref(), make_ref(),
             Port1, Port2,
             self(), self(),
             {element1, 42, 767}, "A string",
             1, -1, 0, 3.0, ABinary, 'I am an atom'],

    P = runner:start(?decode_terms),
    runner:send_term(P, Terms),
    runner:recv_eot(P),

    ok.

%% Decodes the floating point number 3.1415.

decode_float(Config) when is_list(Config) ->
    P = runner:start(?decode_float),
    runner:send_term(P, 3.1415),
    runner:recv_eot(P),
    ok.

%% Tests the erl_free_compound() function.

t_erl_free_compound(Config) when is_list(Config) ->
    runner:test(?t_erl_free_compound),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	2.   C o n s t r u c t i n g   t e r m s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This tests the erl_mk_list() function.

t_erl_mk_list(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_list),

    {term, []} = get_term(P),
    {term, [abc]} = get_term(P),
    {term, [abcdef, 42]} = get_term(P),
    {term, [0.0, 23, [], 3.1415]} = get_term(P),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_int() function.

t_erl_mk_int(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_int),

    {term, 0} = get_term(P),
    {term, 127} = get_term(P),
    {term, 128} = get_term(P),
    {term, 255} = get_term(P),
    {term, 256} = get_term(P),

    {term, 16#FFFF} = get_term(P),
    {term, 16#10000} = get_term(P),

    {term, 16#07FFFFFF} = get_term(P),
    {term, 16#0FFFFFFF} = get_term(P),
    {term, 16#1FFFFFFF} = get_term(P),
    {term, 16#3FFFFFFF} = get_term(P),
    {term, 16#7FFFFFFF} = get_term(P),

    {term, 16#08000000} = get_term(P),
    {term, 16#10000000} = get_term(P),
    {term, 16#20000000} = get_term(P),
    {term, 16#40000000} = get_term(P),


    {term, -16#07FFFFFF} = get_term(P),
    {term, -16#0FFFFFFF} = get_term(P),
    {term, -16#1FFFFFFF} = get_term(P),
    {term, -16#3FFFFFFF} = get_term(P),
    {term, -16#7FFFFFFF} = get_term(P),

    {term, -16#08000000} = get_term(P),
    {term, -16#10000000} = get_term(P),
    {term, -16#20000000} = get_term(P),
    {term, -16#40000000} = get_term(P),

    {term, -16#08000001} = get_term(P),
    {term, -16#10000001} = get_term(P),
    {term, -16#20000001} = get_term(P),
    {term, -16#40000001} = get_term(P),

    {term, -16#08000002} = get_term(P),
    {term, -16#10000002} = get_term(P),
    {term, -16#20000002} = get_term(P),
    {term, -16#40000002} = get_term(P),

    {term, -1999999999} = get_term(P),
    {term, -2000000000} = get_term(P),
    {term, -2000000001} = get_term(P),

    runner:recv_eot(P),
    ok.


%% Basic test of erl_copy_term().

basic_copy(Config) when is_list(Config) ->
    runner:test(?basic_copy),
    ok.


%% This tests the erl_mk_tuple() function.

t_erl_mk_tuple(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_tuple),

    {term, {madonna, 21, 'mad donna', 12}} = get_term(P),
    {term, {'Madonna',21,{children,{"Isabella",2}},
            {'home page',"http://www.madonna.com/"}}} = get_term(P),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_atom() function.

t_erl_mk_atom(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_atom),

    {term, madonna} = (get_term(P)),
    {term, 'Madonna'} = (get_term(P)),
    {term, 'mad donna'} = (get_term(P)),
    {term, '_madonna_'} = (get_term(P)),
    {term, '/home/madonna/tour_plan'} = (get_term(P)),
    {term, 'http://www.madonna.com/tour_plan'} = (get_term(P)),
    {term, '\'madonna\''} = (get_term(P)),
    {term, '\"madonna\"'} = (get_term(P)),
    {term, '\\madonna\\'} = (get_term(P)),
    {term, '{madonna,21,\'mad donna\',12}'} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_binary() function.

t_erl_mk_binary(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_binary),

    {term, Bin} = (get_term(P)),
    "{madonna,21,'mad donna',1234.567.890, !#$%&/()=?+-@, \" \\}" = binary_to_list(Bin),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_empty_list() function.

t_erl_mk_empty_list(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_empty_list),

    {term, []} = get_term(P),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_float() function.

t_erl_mk_float(Config) when is_list(Config) ->
    case os:type() of 
        vxworks ->
            {skipped, "Floating point numbers never compare equal on PPC"};
        _ ->
            P = runner:start(?t_erl_mk_float),
            {term, {3.1415, 1.999999, 2.000000, 2.000001,
                    2.000002, 12345.67890}} = get_term(P),
            runner:recv_eot(P),
            ok
    end.


%% This tests the erl_mk_pid() function.

t_erl_mk_pid(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_pid),

    {term, A_pid} = (get_term(P)),
    {pid, kalle@localhost, 3, 2} = nc2vinfo(A_pid),

    runner:recv_eot(P),
    ok.

t_erl_mk_xpid(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_xpid),

    {term, A_pid} = (get_term(P)),
    {pid, kalle@localhost, 32767, 8191} = nc2vinfo(A_pid),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_port() function.

t_erl_mk_port(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_port),

    {term, A_port} = (get_term(P)),
    {port, kalle@localhost, 4} = nc2vinfo(A_port),

    runner:recv_eot(P),
    ok.

t_erl_mk_xport(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_xport),

    {term, A_port} = (get_term(P)),
    {port, kalle@localhost, 268435455} = nc2vinfo(A_port),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_ref() function.

t_erl_mk_ref(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_ref),

    {term, A_ref} = (get_term(P)),
    {ref, kalle@localhost, _Length, [6]} = nc2vinfo(A_ref),

    runner:recv_eot(P),
    ok.

t_erl_mk_long_ref(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_long_ref),

    {term, A_ref} = (get_term(P)),
    {ref, kalle@localhost, _Length, [4294967295,4294967295,262143]}
    = nc2vinfo(A_ref),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_string() function.

t_erl_mk_string(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_string),

    {term, "madonna"} = (get_term(P)),
    {term, "Madonna"} = (get_term(P)),
    {term, "mad donna"} = (get_term(P)),
    {term, "_madonna_"} = (get_term(P)),
    {term, "/home/madonna/tour_plan"} = (get_term(P)),
    {term, "http://www.madonna.com/tour_plan"} = (get_term(P)),
    {term, "\'madonna\'"} = (get_term(P)),
    {term, "\"madonna\""} = (get_term(P)),
    {term, "\\madonna\\"} = (get_term(P)),
    {term, "{madonna,21,'mad donna',12}"} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_estring() function.

t_erl_mk_estring(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_estring),

    {term, "madonna"} = (get_term(P)),
    {term, "Madonna"} = (get_term(P)),
    {term, "mad donna"} = (get_term(P)),
    {term, "_madonna_"} = (get_term(P)),
    {term, "/home/madonna/tour_plan"} = (get_term(P)),
    {term, "http://www.madonna.com/tour_plan"} = (get_term(P)),
    {term, "\'madonna\'"} = (get_term(P)),
    {term, "\"madonna\""} = (get_term(P)),
    {term, "\\madonna\\"} = (get_term(P)),
    {term, "{madonna,21,'mad donna',12}"} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_uint() function.

t_erl_mk_uint(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_uint),

    {term, 54321} = (get_term(P)),
    {term, 2147483647} = (get_term(P)),
    {term, 2147483648} = (get_term(P)),
    {term, 2147483649} = (get_term(P)),
    {term, 2147483650} = (get_term(P)),
    {term, 4294967295} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_mk_var() function.

t_erl_mk_var(Config) when is_list(Config) ->
    P = runner:start(?t_erl_mk_var),

    {term, 1} = (get_term(P)),
    {term, 0} = (get_term(P)),
    {term, 1} = (get_term(P)),
    {term, 0} = (get_term(P)),
    {term, 1} = (get_term(P)),
    {term, 0} = (get_term(P)),
    {term, 1} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_cons() function.

t_erl_cons(Config) when is_list(Config) ->
    P = runner:start(?t_erl_cons),

    {term, [madonna, 21]} = get_term(P),

    runner:recv_eot(P),
    ok.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	3.   E x t r a c t i n g  &   i n f o    f u n c t i o n s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the erl_length() function.

t_erl_length(Config) when is_list(Config) ->
    P = runner:start(?t_erl_length),

    0 = erl_length(P, []),
    1 = erl_length(P, [a]),
    2 = erl_length(P, [a, b]),
    3 = erl_length(P, [a, b, c]),

    4 = erl_length(P, [a, [x, y], c, []]),

    -1 = erl_length(P, [a|b]),
    -1 = erl_length(P, a),

    runner:finish(P),
    ok.

%% Invokes the erl_length() function.

erl_length(Port, List) ->
    call_erl_function(Port, List).

%% Tests the erl_hd() function.

t_erl_hd(Config) when is_list(Config) ->
    P = runner:start(?t_erl_hd),

    'NULL' = erl_hd(P, 42),
    'NULL' = erl_hd(P, abc),
    'NULL' = erl_hd(P, []),

    [] = erl_hd(P, [[], a]),
    a = erl_hd(P, [a]),
    a = erl_hd(P, [a, b]),
    a = erl_hd(P, [a, b, c]),
    a = erl_hd(P, [a|b]),

    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

%% Invokes the erl_hd() function.

erl_hd(Port, List) ->
    call_erl_function(Port, List).

%% Tests the erl_tail() function.

t_erl_tl(Config) when is_list(Config) ->
    P = runner:start(?t_erl_tl),

    'NULL' = erl_tl(P, 42),
    'NULL' = erl_tl(P, abc),
    'NULL' = erl_tl(P, []),

    [] = erl_tl(P, [a]),
    [b] = erl_tl(P, [a, b]),
    [b, c] = erl_tl(P, [a, b, c]),

    b = erl_tl(P, [a|b]),

    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

%% Invokes the erl_tail() function in erl_interface.

erl_tl(Port, List) ->
    call_erl_function(Port, List).

%% Tests the type checking macros (done in the C program).

type_checks(Config) when is_list(Config) ->
    runner:test(?type_checks),
    ok.

%% Tests the extractor macros (done in the C program).

extractor_macros(Config) when is_list(Config) ->
    runner:test(?extractor_macros),
    ok.


%% This tests the erl_size() function.

t_erl_size(Config) when is_list(Config) ->
    P = runner:start(?t_erl_size),

    {term, 0} = (get_term(P)),
    {term, 4} = (get_term(P)),

    {term, 0} = (get_term(P)),
    {term, 27} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_var_content() function.

t_erl_var_content(Config) when is_list(Config) ->
    P = runner:start(?t_erl_var_content),

    {term, 17} = (get_term(P)),
    {term, "http://www.madonna.com"} = (get_term(P)),
    {term, 2} = (get_term(P)),
    {term, "http://www.madonna.com"} = (get_term(P)),
    {term, 2} = (get_term(P)),

    runner:recv_eot(P),
    ok.


%% This tests the erl_element() function.

t_erl_element(Config) when is_list(Config) ->
    P = runner:start(?t_erl_element),

    {term, madonna} = get_term(P),
    {term, 21} = get_term(P),
    {term, 'mad donna'} = get_term(P),
    {term, 12} = get_term(P),

    {term, 'Madonna'} = get_term(P),
    {term, 21} = get_term(P),
    {term, {children,{"Isabella",2}}} = get_term(P),
    {term, {'home page',"http://www.madonna.com/"}} = get_term(P),

    runner:recv_eot(P),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	4.   I / O   l i s t   f u n c t i o n s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the erl_iolist_length() function.

t_erl_iolist_length(Config) when is_list(Config) ->
    P = runner:start(?t_erl_iolist_length),

    %% Flat lists.

    0 = erl_iolist_length(P, []),
    1 = erl_iolist_length(P, [10]),
    2 = erl_iolist_length(P, [10, 20]),
    3 = erl_iolist_length(P, [10, 20, 30]),
    256 = erl_iolist_length(P, lists:seq(0, 255)),

    %% Deep lists.

    0 = erl_iolist_length(P, [[]]),
    1 = erl_iolist_length(P, [[], 42]),
    1 = erl_iolist_length(P, [42, []]),
    2 = erl_iolist_length(P, [42, [], 45]),

    3 = erl_iolist_length(P, [42, [90], 45]),
    3 = erl_iolist_length(P, [[42, [90]], 45]),
    3 = erl_iolist_length(P, [[42, [90]], 45]),

    %% List with binaries.

    0 = erl_iolist_length(P, [list_to_binary([])]),
    0 = erl_iolist_length(P, [[], list_to_binary([])]),
    1 = erl_iolist_length(P, [[1], list_to_binary([])]),
    1 = erl_iolist_length(P, [[], list_to_binary([2])]),
    2 = erl_iolist_length(P, [[42], list_to_binary([2])]),
    4 = erl_iolist_length(P, [[42], list_to_binary([2, 3, 4])]),

    %% Binaries as tail.

    0 = erl_iolist_length(P, [[]| list_to_binary([])]),
    1 = erl_iolist_length(P, [[1]| list_to_binary([])]),
    1 = erl_iolist_length(P, [[]| list_to_binary([2])]),
    2 = erl_iolist_length(P, [[42]| list_to_binary([2])]),

    %% Binaries only.

    0 = erl_iolist_length(P, list_to_binary("")),
    1 = erl_iolist_length(P, list_to_binary([1])),
    2 = erl_iolist_length(P, list_to_binary([1, 2])),

    %% Illegal cases.

    -1 = erl_iolist_length(P, [42|43]),
    -1 = erl_iolist_length(P, a),

    -1 = erl_iolist_length(P, [a]),
    -1 = erl_iolist_length(P, [256]),
    -1 = erl_iolist_length(P, [257]),
    -1 = erl_iolist_length(P, [-1]),
    -1 = erl_iolist_length(P, [-2]),
    -1 = erl_iolist_length(P, [-127]),
    -1 = erl_iolist_length(P, [-128]),

    runner:finish(P),
    ok.

%% Invokes the erl_iolist_length() function.

erl_iolist_length(Port, List) ->
    call_erl_function(Port, List).

%% Tests the erl_iolist_to_binary() function.

t_erl_iolist_to_binary(Config) when is_list(Config) ->
    P = runner:start(?t_erl_iolist_to_binary),

    %% Flat lists.

    [] = iolist_to_list(P, []),
    [10] = iolist_to_list(P, [10]),
    [10, 20] = iolist_to_list(P, [10, 20]),
    [10, 20, 30] = iolist_to_list(P, [10, 20, 30]),
    AllBytes = lists:seq(0, 255),
    AllBytes = iolist_to_list(P, AllBytes),

    %% Deep lists.

    [] = iolist_to_list(P, [[]]),
    [42] = iolist_to_list(P, [[], 42]),
    [42] = iolist_to_list(P, [42, []]),
    [42, 45] = iolist_to_list(P, [42, [], 45]),

    [42, 90, 45] = iolist_to_list(P, [42, [90], 45]),
    [42, 90, 45] = iolist_to_list(P, [[42, [90]], 45]),
    [42, 90, 45] = iolist_to_list(P, [[42, [90]], 45]),

    %% List with binaries.

    [] = iolist_to_list(P, [list_to_binary([])]),
    [] = iolist_to_list(P, [[], list_to_binary([])]),
    [1] = iolist_to_list(P, [[1], list_to_binary([])]),
    [2] = iolist_to_list(P, [[], list_to_binary([2])]),
    [42, 2] = iolist_to_list(P, [[42], list_to_binary([2])]),
    [42, 2, 3, 4] = iolist_to_list(P, [[42], list_to_binary([2, 3, 4])]),

    %% Binaries as tail.

    [] = iolist_to_list(P, [[]| list_to_binary([])]),
    [1] = iolist_to_list(P, [[1]| list_to_binary([])]),
    [2] = iolist_to_list(P, [[]| list_to_binary([2])]),
    [42, 2] = iolist_to_list(P, [[42]| list_to_binary([2])]),

    %% Binaries only.

    [] = iolist_to_list(P, list_to_binary("")),
    [1] = iolist_to_list(P, list_to_binary([1])),
    [1, 2] = iolist_to_list(P, list_to_binary([1, 2])),

    %% Illegal cases.

    'NULL' = iolist_to_list(P, [42|43]),
    'NULL' = iolist_to_list(P, a),

    'NULL' = iolist_to_list(P, [a]),
    'NULL' = iolist_to_list(P, [256]),
    'NULL' = iolist_to_list(P, [257]),
    'NULL' = iolist_to_list(P, [-1]),
    'NULL' = iolist_to_list(P, [-2]),
    'NULL' = iolist_to_list(P, [-127]),
    'NULL' = iolist_to_list(P, [-128]),

    runner:finish(P),
    ok.

iolist_to_list(Port, Term) ->
    case call_erl_function(Port, Term) of
        'NULL' ->
            'NULL';
        Bin when is_binary(Bin) ->
            binary_to_list(Bin)
    end.

%% Tests the erl_iolist_to_string() function.

t_erl_iolist_to_string(Config) when is_list(Config) ->
    P = runner:start(?t_erl_iolist_to_string),

    %% Flat lists.

    [0] = iolist_to_string(P, []),
    [10, 0] = iolist_to_string(P, [10]),
    [10, 20, 0] = iolist_to_string(P, [10, 20]),
    [10, 20, 30, 0] = iolist_to_string(P, [10, 20, 30]),
    AllBytes = lists:seq(1, 255)++[0],
    AllBytes = iolist_to_string(P, lists:seq(1, 255)),

    %% Deep lists.

    [0] = iolist_to_string(P, [[]]),
    [42, 0] = iolist_to_string(P, [[], 42]),
    [42, 0] = iolist_to_string(P, [42, []]),
    [42, 45, 0] = iolist_to_string(P, [42, [], 45]),

    [42, 90, 45, 0] = iolist_to_string(P, [42, [90], 45]),
    [42, 90, 45, 0] = iolist_to_string(P, [[42, [90]], 45]),
    [42, 90, 45, 0] = iolist_to_string(P, [[42, [90]], 45]),

    %% List with binaries.

    [0] = iolist_to_string(P, [list_to_binary([])]),
    [0] = iolist_to_string(P, [[], list_to_binary([])]),
    [1, 0] = iolist_to_string(P, [[1], list_to_binary([])]),
    [2, 0] = iolist_to_string(P, [[], list_to_binary([2])]),
    [42, 2, 0] = iolist_to_string(P, [[42], list_to_binary([2])]),
    [42, 2, 3, 4, 0] = iolist_to_string(P, [[42],
                                            list_to_binary([2, 3, 4])]),

    %% Binaries as tail.

    [0] = iolist_to_string(P, [[]| list_to_binary([])]),
    [1, 0] = iolist_to_string(P, [[1]| list_to_binary([])]),
    [2, 0] = iolist_to_string(P, [[]| list_to_binary([2])]),
    [42, 2, 0] = iolist_to_string(P, [[42]| list_to_binary([2])]),

    %% Binaries only.

    [0] = iolist_to_string(P, list_to_binary("")),
    [1, 0] = iolist_to_string(P, list_to_binary([1])),
    [1, 2, 0] = iolist_to_string(P, list_to_binary([1, 2])),

    %% Illegal cases.

    'NULL' = iolist_to_string(P, [0]),
    'NULL' = iolist_to_string(P, [65, 0, 66]),
    'NULL' = iolist_to_string(P, [65, 66, 67, 0]),

    'NULL' = iolist_to_string(P, [42|43]),
    'NULL' = iolist_to_string(P, a),

    'NULL' = iolist_to_string(P, [a]),
    'NULL' = iolist_to_string(P, [256]),
    'NULL' = iolist_to_string(P, [257]),
    'NULL' = iolist_to_string(P, [-1]),
    'NULL' = iolist_to_string(P, [-2]),
    'NULL' = iolist_to_string(P, [-127]),
    'NULL' = iolist_to_string(P, [-128]),

    runner:finish(P),
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

%% Tests the erl_print_term() function
erl_print_term(Config) when is_list(Config) ->
    PrintTerm = print_term(Config),
    P = open_port({spawn, PrintTerm}, [stream]),

    %% Lists.

    print(P, "[]", []),
    print(P, "[a]", [a]),
    print(P, "[[a]]", [[a]]),
    print(P, "[[]]", [[]]),
    print(P, "[a,b,c]", [a,b,c]),
    print(P, "[a,b|c]", [a,b|c]),
    print(P, "[a,[],c]", [a,[],c]),
    print(P, "[a,[1000,1],c]", [a,[1000,1],c]),

    %% Tuples.

    print(P, "{}", {}),
    print(P, "{ok}", {ok}),
    print(P, "{1,2,3}", {1, 2, 3}),

    %% Pids.

    {_X, Y, Z} = split_pid(self()),
    PidString = lists:flatten(io_lib:format("<~s.~w.~w>",
                                            [node(), Y, Z])),
    print(P, PidString, self()),

    unlink(P),
    exit(P, die),
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

%% Test printing a string with erl_print_term()
print_string(Config) when is_list(Config) ->
    PrintTerm = print_term(Config),
    P = open_port({spawn, PrintTerm}, [stream]),

    %% Strings.

    print(P, "\"ABC\"", "ABC"),
    {11, "\"\\tABC\\r\\n\""} = print(P, "\tABC\r\n"),

    %% Not strings.

    print(P, "[65,66,67,0]", "ABC\000"),

    unlink(P),
    exit(P, die),
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
    after 5000 ->
              ct:fail("No response from C program")
    end.

collect_line1([$\r|Rest], Result) ->
    {list_to_integer(Result), lists:reverse(Rest)};
collect_line1([C|Rest], Result) ->
    collect_line1(Rest, [C|Result]).

%% Test case submitted by Per Lundgren, ERV.

high_chaparal(Config) when is_list(Config) ->
    P = runner:start(?high_chaparal),
    {term, [hello, world]} = get_term(P),
    runner:recv_eot(P),
    ok.

%% OTP-7448
broken_data(Config) when is_list(Config) ->
    P = runner:start(?broken_data),
    runner:recv_eot(P),
    ok.

%% This calls a C function with one parameter and returns the result.

call_erl_function(Port, Term) ->
    runner:send_term(Port, Term),
    case get_term(Port) of
        {term, Result} -> Result;
        'NULL'       -> 'NULL'
    end.

print_term(Config) when is_list(Config) ->
    filename:join(proplists:get_value(data_dir, Config), "print_term").



%%% We receive a ref from the cnode, and expect it to be a long ref.
%%% We also send a ref we created ourselves, and expect to get it
%%% back, without having been mutated into short form. We must take
%%% care then to check the actual returned ref, and not the original
%%% one, which is equal to it.

%% Tests involving cnode: sends a long ref from a cnode to us
cnode_1(Config) when is_list(Config) ->
    Cnode = filename:join(proplists:get_value(data_dir, Config), "cnode"),
    register(mip, self()),
    spawn_link(?MODULE, start_cnode, [Cnode]),
    Ref1 = get_ref(),
    io:format("Ref1 ~p~n", [Ref1]),
    check_ref(Ref1),
    Ref2 = make_ref(),
    Pid = receive
              Msg -> Msg %% pid
          end,
    Fun1 = fun(X) -> {Pid, X} end,	% sneak in a fun test here
    %Fun1 = {wait_with_funs, new_dist_format},
    Term = {Ref2, Fun1, {1,2,3,4,5,6,7,8,9,10}},
    %% A term which will overflow the original buffer used in 'cnode'.
    Pid ! Term,
    receive
        Term2 ->
            io:format("received ~p~n", [Term2]),
            case Term2 of
                Term ->
                    {Ref22,_,_} = Term2,
                    check_ref(Ref22);
                X ->
                    ct:fail({receive1,X})
            end
    after 5000 ->
              ct:fail(receive1)
    end,
    receive
        Pid ->
            ok;
        Y ->
            ct:fail({receive1,Y})
    after 5000 ->
              ct:fail(receive2)
    end,
    io:format("ref = ~p~n", [Ref1]),
    check_ref(Ref1),
    ok.

check_ref(Ref) ->
    case bin_ext_type(Ref) of
        101 ->
            ct:fail(oldref);
        114 ->
            ok;
        Type ->
            ct:fail({type, Type})
    end.

bin_ext_type(T) ->
    [131, Type | _] = binary_to_list(term_to_binary(T)),
    Type.

get_ref() ->
    receive
        X when is_reference(X) ->
            X
    after 5000 ->
              ct:fail({cnode, timeout})
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
    [_NodeStr, NumberStr, SerialStr]
    = string:tokens(pid_to_list(Pid), "<.>"),
    Number = list_to_integer(NumberStr),
    Serial = list_to_integer(SerialStr),
    {pid, node(Pid), Number, Serial};
nc2vinfo(Port) when is_port(Port) ->
    ["#Port", _NodeStr, NumberStr]
    = string:tokens(erlang:port_to_list(Port), "<.>"),
    Number = list_to_integer(NumberStr),
    {port, node(Port), Number};
nc2vinfo(Ref) when is_reference(Ref) ->
    ["#Ref", _NodeStr | NumStrList]
    = string:tokens(erlang:ref_to_list(Ref), "<.>"),
    {Len, RevNumList} = lists:foldl(fun ("0", {N, []}) ->
                                            {N+1, []};
                                        (IStr, {N, Is}) ->
                                            {N+1,
                                             [list_to_integer(IStr)|Is]}
                                    end,
                                    {0, []},
                                    NumStrList),
    {ref, node(Ref), Len, lists:reverse(RevNumList)};
nc2vinfo(Other) ->
    {badarg, Other}.
