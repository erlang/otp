%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Tests of the dictionary file compiler.
%%

-module(diameter_compiler_SUITE).
-compile({no_auto_import, [error/2]}).

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([format/1,
         replace/1, replace/2]).

-export([dict/0]).  %% fake dictionary module

-define(base, "base_rfc3588.dia").
-define(util, diameter_util).
-define(S, atom_to_list).

%% ===========================================================================

%% RE/Replacement (in the sense of re:replace/4) pairs for morphing
%% base_rfc3588.dia. The key is 'ok' or the the expected error as
%% returned in the first element of the error tuple returned by
%% diameter_dict_util:parse/2.
-define(REPLACE,
        [{scan,
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
         {grouped_vendor_id_without_flag,
          "(Failed-AVP .*)>",
          "\\1 668>"},
         {grouped_vendor_id_mismatch,
          [{"(Failed-AVP .*)>", "\\1 17>"},
           {"^ *Failed-AVP .*$", "&V"},
           {"@avp_types", "@avp_vendor_id 18 Failed-AVP\n&"}]},
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
         {invalid_qualifier,
          "CEA ::=.*",
          "& 3*2"},
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
    [{timetrap, {seconds, 5}}].

all() ->
    [format,
     replace].

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
%% testcases

%% Ensure that parse o format is the identity map.
format(Config) ->
    Bin = proplists:get_value(base, Config),
    {ok, Dict} = diameter_dict_util:parse(Bin, []),
    {ok, D} = diameter_dict_util:parse(diameter_dict_util:format(Dict), []),
    {Dict, Dict} = {Dict, D}.

%% replace/1

replace(Config) ->
    Bin = proplists:get_value(base, Config),
    [] = ?util:run([{?MODULE, [replace, E, Bin]} || E <- ?REPLACE]).

replace({E, RE, Repl}, Bin) ->
    replace({E, [{RE, Repl}]}, Bin);

replace({E, Mods}, Bin) ->
    B = iolist_to_binary(lists:foldl(fun re/2, Bin, Mods)),
    case diameter_dict_util:parse(B, [{include, here()}]) of
        {ok, Dict} when E == ok ->
            Dict;
        {error, {E,_} = T} ->
            S = diameter_dict_util:format_error(T),
            true = nochar($", S, E),
            true = nochar($', S, E),
            S
    end.

re({RE, Repl}, Bin) ->
    re:replace(Bin, RE, Repl, [multiline]).

%% ===========================================================================

nochar(Char, Str, Err) ->
    Err == parse orelse not lists:member(Char, Str) orelse Str.

here() ->
    filename:dirname(code:which(?MODULE)).

dict() ->
    [0 | orddict:new()].
