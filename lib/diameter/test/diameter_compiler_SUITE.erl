%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
%% Tests of the dictionary file compiler.
%%

-module(diameter_compiler_SUITE).

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([format/1,    format/2,
         replace/1,   replace/2,
         generate/1,  generate/4,
         flatten1/1,  flatten1/3,
         flatten2/1]).

-export([dict/0]).  %% fake dictionary module

%% dictionary callbacks for flatten2/1
-export(['A1'/4, 'Unsigned32'/4]).

-define(base, "base_rfc3588.dia").
-define(util, diameter_util).
-define(S, atom_to_list).
-define(L, integer_to_list).

%% ===========================================================================

%% RE/Replacement (in the sense of re:replace/4) pairs for morphing
%% base_rfc3588.dia. The key is 'ok' or the the expected error as
%% returned in the first element of the error tuple returned by
%% diameter_make:codec/2.
-define(REPLACE,
        [{ok,
          "",
          ""},
         {scan,
          "@id 0",
          "@id \\&"},
         {scan,
          "@name ",
          "&'"},
         {parse,
          "@id 0",
          "@id @id"},
         {avp_code_already_defined,
          "480",
          "485"},
         {uint32_out_of_range,
          "@id 0",
          "@id 4294967296"},
         {uint32_out_of_range,
          "@vendor 0",
          "@vendor 4294967296"},
         {uint32_out_of_range,
          [{"^ *Failed-AVP .*$", "&V"},
           {"@avp_types", "@avp_vendor_id 4294967296 Failed-AVP\n&"}]},
         {imported_avp_already_defined,
          "@avp_types",
          "@inherits diameter_gen_base_rfc3588 &"},
         {duplicate_import,
          [{"@avp_types", "@inherits diameter_gen_base_rfc3588 Class\n&"},
           {"@avp_types", "@inherits diameter_gen_base_rfc3588\n&"},
           {"^@avp_types[^@]*", ""},
           {"^@enum[^&]*", ""}]},
         {duplicate_section,
          "@prefix",
          "@name"},
         {already_declared,
          "@enum Termination-Cause",
          "& XXX 0\n &"},
         {already_declared,
          "@define Result-Code",
          "& XXX 1000 &"},
         {inherited_avp_already_defined,
          "@id",
          "@inherits nomod Origin-Host &"},
         {avp_already_defined,
          "@avp_types",
          "@inherits m XXX\nXXX\n&"},
         {avp_already_defined,
          "@avp_types",
          "@inherits mod1 XXX\n@inherits mod2 XXX\n&"},
         {key_already_defined,
          "DIAMETER_SUCCESS",
          "& 2001\n&"},
         {messages_without_id,
          "@id 0",
          ""},
         {avp_name_already_defined,
          "Class",
          "& 666 Time M\n&"},
         {avp_has_unknown_type,
          "Enumerated",
          "Enum"},
         {avp_has_invalid_flag,
          " -",
          " X"},
         {avp_has_duplicate_flag,
          " -",
          " MM"},
         {ok,
          "@vendor 0",
          "@vendor 10415"},
         {ok,
          [{"@vendor 0", "@vendor 10415"},
           {"Proxy-Info .*M$", "&V"},
           {"Proxy-Info ::= [^>]*", "& 10415 "}]},
         {grouped_vendor_id_without_flag,
          [{"@vendor 0", "@vendor 10415"},
           {"Proxy-Info ::= [^>]*", "& 10415 "}]},
         {avp_has_vendor_id,
          "@avp_types",
          "@avp_vendor_id 667 Class\n&"},
         {avp_has_no_vendor,
          [{"^ *Class .*$", "&V"},
           {"@vendor .*", ""}]},
         {group_already_defined,
          "@grouped",
          "& Failed-AVP ::= < AVP Header: 279 > " "{AVP}\n&"},
         {grouped_avp_code_mismatch,
          "(Failed-AVP ::= [^0-9]*27)9",
          "&8"},
         {grouped_avp_has_wrong_type,
          "(Failed-AVP *279 *)Grouped",
          "\\1Time"},
         {grouped_avp_not_defined,
          "Failed-AVP *.*",
          ""},
         {grouped_avp_not_grouped,
          "Failed-AVP ::=.*\n.*}",
          ""},
         {grouped_vendor_id_without_flag,
          "(Failed-AVP .*)>",
          "\\1 668>"},
         {grouped_vendor_id_mismatch,
          [{"(Failed-AVP .*)>", "\\1 17>"},
           {"^ *Failed-AVP .*$", "&V"},
           {"@avp_types", "@avp_vendor_id 18 Failed-AVP\n&"}]},
         {ok,
          [{"(Failed-AVP .*)>", "\\1 17>"},
           {"^ *Failed-AVP .*$", "&V"}]},
         {message_name_already_defined,
          "CEA ::= .*:",
          "& 257 > {Result-Code}\n&"},
         {message_code_already_defined,
          "CEA( ::= .*)",
          "XXX\\1 {Result-Code}\n&"},
         {message_has_duplicate_flag,
          "(CER ::=.*)>",
          "\\1, REQ>"},
         {message_application_id_mismatch,
         "(CER ::=.*)>",
          "\\1 1>"},
         {invalid_avp_order,
          "CEA ::=",
          "{Result-Code} &"},
         {ok,
          "{ Product-Name",
          "* &"},
         {required_avp_has_zero_max_arity,
          "{ Product-Name",
          "*0 &"},
         {required_avp_has_zero_min_arity,
          "{ Product-Name",
          "0* &"},
         {required_avp_has_zero_min_arity,
          "{ Product-Name",
          "0*0 &"},
         {ok,
          "{ Product-Name",
          "*1 &"},
         {ok,
          "{ Product-Name",
          "1* &"},
         {ok,
          "{ Product-Name",
          "1*1 &"},
         {ok,
          "{ Product-Name",
          "2* &"},
         {ok,
          "{ Product-Name",
          "*2 &"},
         {ok,
          "{ Product-Name",
          "2*2 &"},
         {ok,
          "{ Product-Name",
          "2*3 &"},
         {qualifier_has_min_greater_than_max,
          "{ Product-Name",
          "3*2 &"},
         {ok,
          "\\[ Origin-State-Id",
          "* &"},
         {ok,
          "\\[ Origin-State-Id",
          "0* &"},
         {ok,
          "\\[ Origin-State-Id",
          "*0 &"},
         {ok,
          "\\[ Origin-State-Id",
          "0*0 &"},
         {ok,
          "\\[ Origin-State-Id",
          "0*1 &"},
         {ok,
          "\\[ Origin-State-Id",
          "0*2 &"},
         {ok,
          "\\[ Origin-State-Id",
          "*1 &"},
         {optional_avp_has_nonzero_min_arity,
          "\\[ Origin-State-Id",
          "1* &"},
         {optional_avp_has_nonzero_min_arity,
          "\\[ Origin-State-Id",
          "1*1 &"},
         {ok,
          "\\[ Origin-State-Id",
          "*2 &"},
         {optional_avp_has_nonzero_min_arity,
          "\\[ Origin-State-Id",
          "2* &"},
         {optional_avp_has_nonzero_min_arity,
          "\\[ Origin-State-Id",
          "2*2 &"},
         {optional_avp_has_nonzero_min_arity,
          "\\[ Origin-State-Id",
          "2*3 &"},
         {optional_avp_has_nonzero_min_arity,
          "\\[ Origin-State-Id",
          "3*2 &"},
         {ok,
          "^ *< Session-Id",
          "* &"},
         {ok,
          "^ *< Session-Id",
          "*0 &"},
         {ok,
          "^ *< Session-Id",
          "0* &"},
         {ok,
          "^ *< Session-Id",
          "0*0 &"},
         {ok,
          "^ *< Session-Id",
          "0*1 &"},
         {ok,
          "^ *< Session-Id",
          "0*2 &"},
         {ok,
          "^ *< Session-Id",
          "*1 &"},
         {ok,
          "^ *< Session-Id",
          "1* &"},
         {ok,
          "^ *< Session-Id",
          "1*1 &"},
         {ok,
          "^ *< Session-Id",
          "*2 &"},
         {ok,
          "^ *< Session-Id",
          "2* &"},
         {ok,
          "^ *< Session-Id",
          "2*2 &"},
         {ok,
          "^ *< Session-Id",
          "2*3 &"},
         {qualifier_has_min_greater_than_max,
          "^ *< Session-Id",
          "3*2 &"},
         {avp_already_referenced,
          "CER ::=.*",
          "& {Origin-Host}"},
         {message_missing,
          "CER ::=",
          "XXR ::= < Diameter-Header: 666, REQ > {Origin-Host} &"},
         {requested_avp_not_found,
          [{"@id", "@inherits diameter_gen_base_rfc3588 XXX &"},
           {"CEA ::=", "<XXX> &"}]},
         {requested_avp_not_found,
          [{"@id", "@inherits diameter_gen_base_rfc3588 'X X X' &"},
           {"CEA ::=", "<'X X X'> &"}]},
         {enumerated_avp_has_wrong_local_type,
          "Enumerated",
          "Time"},
         {enumerated_avp_not_defined,
         [{"{ Disconnect-Cause }", ""},
          {"^ *Disconnect-Cause .*", ""}]},
         {avp_not_defined,
          "CEA ::=",
          "<XXX> &"},
         {ok,
          "@avp_types",
          "@codecs tmod Session-Id &"},
         {ok,
          "@avp_types",
          "@custom_types tmod Session-Id &"},
         {avp_not_defined,
          "@avp_types",
          "@codecs tmod OctetString &"},
         {avp_not_defined,
          "@avp_types",
          "@custom_types tmod OctetString &"},
         {avp_already_defined,
          "@avp_types",
          "@codecs tmod Session-Id @custom_types tmod Session-Id &"},
         {not_loaded,
          [{"@avp_types", "@inherits nomod XXX &"},
           {"CEA ::=", "<XXX> &"}]},
         {recompile,
          [{"@avp_types", "@inherits " ++ ?S(?MODULE) ++ " XXX &"},
           {"CEA ::=", "<XXX> &"}]},
         {no_dict,
          [{"@avp_types", "@inherits diameter XXX &"},
           {"CEA ::=", "<XXX> &"}]},
         {ok,
          "@avp_types",
          "@end & bad syntax"},
         {parse,
          "@avp_types",
          "& bad syntax"},
         {ok,
          [{"@avp_types", "& 3XXX 666 Time M 'X X X' 667 Time -"},
           {"^ *Class .*", "@avp_types"},
           {"^ *Failed-AVP ", "@avp_types &"},
           {"@grouped", "&&"},
           {"^ *Failed-AVP ::=", "@grouped &"},
           {"CEA ::=", "<'Class'> &"},
           {"@avp_types", "@inherits diameter_gen_base_rfc3588 Class\n&"},
           {"@avp_types", "@custom_types mymod "
                              "Product-Name Firmware-Revision\n"
                          "@codecs mymod "
                              "Origin-Host Origin-Realm\n&"}]}]).

%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [format,
     replace,
     generate,
     flatten1,
     flatten2].

%% Error handling testcases will make an erroneous dictionary out of
%% the base dictionary and check that the expected error results.
%% ?REPLACE encodes the modifications and expected error.
init_per_suite(Config) ->
    Path = filename:join([code:lib_dir(diameter, src), "dict", ?base]),
    {ok, Bin} = file:read_file(Path),
    [{base, Bin} | Config].

end_per_suite(_Config) ->
    ok.

%% ===========================================================================
%% format/1
%%
%% Ensure that parse o format is the identity map.

format(Config) ->
    Bin = proplists:get_value(base, Config),
    [] = ?util:run([{?MODULE, [format, M, Bin]}
                    || E <- ?REPLACE,
                       {ok, M} <- [norm(E)]]).

format(Mods, Bin) ->
    B = modify(Bin, Mods),
    {ok, Dict} = parse(B, []),
    {ok, D} = parse(diameter_make:format(Dict), []),
    {Dict, Dict} = {Dict, D}.

parse(File, Opts) ->
    case diameter_make:codec(File, [parse, hrl, return | Opts]) of
        {ok, [Dict, _]} ->
            {ok, Dict};
        {error, _} = E ->
            E
    end.

%% ===========================================================================
%% replace/1
%%
%% Ensure the expected success/error when parsing a morphed common
%% dictionary.

replace(Config) ->
    Bin = proplists:get_value(base, Config),
    [] = ?util:run([{?MODULE, [replace, N, Bin]}
                    || E <- ?REPLACE,
                       N <- [norm(E)]]).

replace({E, Mods}, Bin) ->
    B = modify(Bin, Mods),
    case {E, parse(B, [{include, here()}]), Mods} of
        {ok, {ok, Dict}, _} ->
            Dict;
        {_, {error, {E,_} = T}, _} when E /= ok ->
            diameter_make:format_error(T)
    end.

re({RE, Repl}, Bin) ->
    re:replace(Bin, RE, Repl, [multiline]).

%% ===========================================================================
%% generate/1
%%
%% Ensure success when generating code and compiling.

generate(Config) ->
    Bin = proplists:get_value(base, Config),
    Rs  = lists:zip(?REPLACE, lists:seq(1, length(?REPLACE))),
    [] = ?util:run([{?MODULE, [generate, M, Bin, N, T]}
                    || {E,N} <- Rs,
                       {ok, M} <- [norm(E)],
                       T <- [erl, hrl, parse, forms]]).

generate(Mods, Bin, N, Mode) ->
    B = modify(Bin, Mods ++ [{"@name .*", "@name dict" ++ ?L(N)}]),
    {ok, Dict} = parse(B, []),
    File = "dict" ++ integer_to_list(N),
    {_, ok} = {Dict, diameter_make:codec(Dict,
                                         [{name, File},
                                          {prefix, "base"},
                                          Mode])},
    generate(Mode, File, Dict).

generate(erl, File, _) ->
    {ok, _} = compile:file(File ++ ".erl", [return_errors]);

generate(forms, File, _) ->
    {ok, [_]} = file:consult(File ++ ".F");

generate(parse, File, Dict) ->
    {ok, [Dict]} = file:consult(File ++ ".D"),  %% assert
    {ok, [F]} = diameter_make:codec(Dict, [forms, return]),
    {ok, _, _, _} = compile:forms(F, [return]);

generate(hrl, _, _) ->
    ok.

%% ===========================================================================
%% flatten1/1

flatten1(_Config) ->
    [Vsn | BaseD] = diameter_gen_base_rfc6733:dict(),
    {ok, I} = parse("@inherits diameter_gen_base_rfc6733\n", []),
    [Vsn | FlatD] = diameter_make:flatten(I),
    [] = ?util:run([{?MODULE, [flatten1, K, BaseD, FlatD]}
                    || K <- [avp_types, grouped, enum]]).

flatten1(Key, BaseD, FlatD) ->
    Vs = orddict:fetch(Key, BaseD),
    Vs = orddict:fetch(Key, FlatD).

%% ===========================================================================
%% flatten2/1

flatten2(_Config) ->
    Dict1 =
        "@name diameter_test1\n"
        "@prefix diameter_test1\n"
        "@vendor 666 test\n"
        "@avp_vendor_id 111 A1 A3\n"
        "@avp_vendor_id 222 A4 A6\n"
        "@custom_types " ++ ?S(?MODULE) ++ " A1 A4\n"
        "@codecs " ++ ?S(?MODULE) ++ " A3 A6\n"
        "@avp_types\n"
        "A1 1001 Unsigned32 V\n"
        "A2 1002 Unsigned32 V\n"
        "A3 1003 Unsigned32 V\n"
        "A4 1004 Unsigned32 V\n"
        "A5 1005 Unsigned32 V\n"
        "A6 1006 Unsigned32 V\n"
        "@end ignored\n",
    Dict2 =
        "@name diameter_test2\n"
        "@prefix diameter_test2\n"
        "@vendor 777 test\n"
        "@inherits diameter_test1 A1 A2 A3\n"
        "@inherits diameter_gen_base_rfc6733\n"
        "@avp_vendor_id 333 A1\n",

    {ok, [E1, F1]}
        = diameter_make:codec(Dict1, [erl, forms, return]),
    ct:pal("~s", [E1]),
    diameter_test1 = M1 = load_forms(F1),

    {ok, [D2, E2, F2]}
        = diameter_make:codec(Dict2, [parse, erl, forms, return]),
    ct:pal("~s", [E2]),
    diameter_test2 = M2 = load_forms(F2),

    Flat = lists:flatten(diameter_make:format(diameter_make:flatten(D2))),
    ct:pal("~s", [Flat]),
    {ok, [E3, F3]}
        = diameter_make:codec(Flat, [erl, forms, return,
                                     {name, "diameter_test3"}]),
    ct:pal("~s", [E3]),
    diameter_test3 = M3 = load_forms(F3),

    [{1001, 111, M1, 'A1'},  %% @avp_vendor_id
     {1002, 666, M1, 'A2'},  %% @vendor
     {1003, 111, M1, 'A3'},  %% @avp_vendor_id
     {1004, 222, M1, 'A4'},  %% @avp_vendor_id
     {1005, 666, M1, 'A5'},  %% @vendor
     {1006, 222, M1, 'A6'},  %% @avp_vendor_id
     {1001, 333, M2, 'A1'},  %% M2 @avp_vendor_id
     {1002, 666, M2, 'A2'},  %% M1 @vendor
     {1003, 666, M2, 'A3'},  %% M1 @vendor
     {1001, 333, M3, 'A1'},  %% (as for M2)
     {1002, 666, M3, 'A2'},  %%   "
     {1003, 666, M3, 'A3'}]  %%   "
        = [{Code, Vid, Mod, Name}
           || Mod <- [M1, M2, M3],
              Code <- lists:seq(1001, 1006),
              Vid <- [666, 111, 222, 777, 333],
              {Name, 'Unsigned32'} <- [Mod:avp_name(Code, Vid)]],

    [] = [{A,T,M,RC} || A <- ['A1', 'A3'],
                        T <- [encode, decode],
                        M <- [M2, M3],
                        Ref <- [make_ref()],
                        RC <- [M:avp(T, Ref, A, #{module => M})],
                        RC /= {T, Ref}].

'A1'(T, 'Unsigned32', Ref, _Opts) ->
    {T, Ref}.

'Unsigned32'(T, 'A3', Ref, _Opts) ->
    {T, Ref}.

load_forms(Forms) ->
    {ok, Mod, Bin, _} = compile:forms(Forms, [return]),
    {module, Mod} = code:load_binary(Mod, ?S(Mod), Bin),
    Mod.

%% ===========================================================================

modify(Bin, Mods) ->
    lists:foldl(fun re/2, Bin, Mods).

norm({E, RE, Repl}) ->
    {E, [{RE, Repl}]};
norm({_,_} = T) ->
    T.

here() ->
    filename:dirname(code:which(?MODULE)).

dict() ->
    [0 | orddict:new()].
