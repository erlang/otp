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
%% This module turns a .dia (aka spec) file into the orddict that
%% diameter_codegen.erl in turn morphs into .erl and .hrl files for
%% encode and decode of Diameter messages and AVPs.
%%

-module(diameter_spec_util).

-export([parse/2]).

-define(ERROR(T), erlang:error({T, ?MODULE, ?LINE})).
-define(ATOM, list_to_atom).

%% parse/1
%%
%% Output: orddict()

parse(Path, Options) ->
    put({?MODULE, verbose}, lists:member(verbose, Options)),
    {ok, B} = file:read_file(Path),
    Chunks = chunk(B),
    Spec = make_spec(Chunks),
    true = groups_defined(Spec),  %% sanity checks
    true = customs_defined(Spec), %%
    Full = import_enums(import_groups(import_avps(insert_codes(Spec),
                                                  Options))),
    true = enums_defined(Full),   %% sanity checks
    true = v_flags_set(Spec),
    Full.

%% Optional reports when running verbosely.
report(What, Data) ->
    report(get({?MODULE, verbose}), What, Data).

report(true, Tag, Data) ->
    io:format("##~n## ~p ~p~n", [Tag, Data]);
report(false, _, _) ->
    ok.

%% chunk/1

chunk(B) ->
    chunkify(normalize(binary_to_list(B))).

%% normalize/1
%%
%% Replace CR NL by NL, multiple NL by one, tab by space, and strip
%% comments and leading/trailing space from each line. Precludes
%% semicolons being used for any other purpose than comments.

normalize(Str) ->
    nh(Str, []).

nh([], Acc) ->
    lists:reverse(Acc);

%% Trim leading whitespace.
nh(Str, Acc) ->
    nb(trim(Str), Acc).

%% tab -> space
nb([$\t|Rest], Acc) ->
    nb(Rest, [$\s|Acc]);

%% CR NL -> NL
nb([$\r,$\n|Rest], Acc) ->
    nt(Rest, Acc);

%% Gobble multiple newlines before starting over again.
nb([$\n|Rest], Acc) ->
    nt(Rest, Acc);

%% Comment.
nb([$;|Rest], Acc) ->
    nb(lists:dropwhile(fun(C) -> C /= $\n end, Rest), Acc);

%% Just an ordinary character. Boring ...
nb([C|Rest], Acc) ->
    nb(Rest, [C|Acc]);

nb([] = Str, Acc) ->
    nt(Str, Acc).

%% Discard a subsequent newline.
nt(T, [$\n|_] = Acc) ->
    nh(T, trim(Acc));

%% Trim whitespace from the end of the line before continuing.
nt(T, Acc) ->
    nh(T, [$\n|trim(Acc)]).

trim(S) ->
    lists:dropwhile(fun(C) -> lists:member(C, "\s\t") end, S).

%% chunkify/1
%%
%% Split the spec file into pieces delimited by lines starting with
%% @Tag. Returns a list of {Tag, Args, Chunk} where Chunk is the
%% string extending to the next delimiter. Note that leading
%% whitespace has already been stripped.

chunkify(Str) ->
    %% Drop characters to the start of the first chunk.
    {_, Rest} = split_chunk([$\n|Str]),
    chunkify(Rest, []).

chunkify([], Acc) ->
    lists:reverse(Acc);

chunkify(Rest, Acc) ->
    {H,T} = split_chunk(Rest),
    chunkify(T, [split_tag(H) | Acc]).

split_chunk(Str) ->
    split_chunk(Str, []).

split_chunk([] = Rest, Acc) ->
    {lists:reverse(Acc), Rest};
split_chunk([$@|Rest], [$\n|_] = Acc) ->
    {lists:reverse(Acc), Rest};
split_chunk([C|Rest], Acc) ->
    split_chunk(Rest, [C|Acc]).

%% Expect a tag and its arguments on a single line.
split_tag(Str) ->
    {L, Rest} = get_until($\n, Str),
    [{tag, Tag} | Toks] = diameter_spec_scan:parse(L),
    {Tag, Toks, trim(Rest)}.

get_until(EndT, L) ->
    {H, [EndT | T]} = lists:splitwith(fun(C) -> C =/= EndT end, L),
    {H,T}.

%% ------------------------------------------------------------------------
%% make_spec/1
%%
%% Turn chunks into spec.

make_spec(Chunks) ->
    lists:foldl(fun(T,A) -> report(chunk, T), chunk(T,A) end,
                orddict:new(),
                Chunks).

chunk({T, [X], []}, Dict)
  when T == name;
       T == prefix ->
    store(T, atomize(X), Dict);

chunk({id = T, [{number, I}], []}, Dict) ->
    store(T, I, Dict);

chunk({vendor = T, [{number, I}, N], []}, Dict) ->
    store(T, {I, atomize(N)}, Dict);

%% inherits -> [{Mod, [AvpName, ...]}, ...]
chunk({inherits = T, [_,_|_] = Args, []}, Acc) ->
    Mods = [atomize(A) || A <- Args],
    append_list(T, [{M,[]} || M <- Mods], Acc);
chunk({inherits = T, [Mod], Body}, Acc) ->
    append(T, {atomize(Mod), parse_avp_names(Body)}, Acc);

%% avp_types -> [{AvpName, Code, Type, Flags, Encr}, ...]
chunk({avp_types = T, [], Body}, Acc) ->
    store(T, parse_avp_types(Body), Acc);

%% custom_types -> [{Mod, [AvpName, ...]}, ...]
chunk({custom_types = T, [Mod], Body}, Dict) ->
    [_|_] = Avps = parse_avp_names(Body),
    append(T, {atomize(Mod), Avps}, Dict);

%% messages -> [{MsgName, Code, Type, Appl, Avps}, ...]
chunk({messages = T, [], Body}, Acc) ->
    store(T, parse_messages(Body), Acc);

%% grouped -> [{AvpName, Code, Vendor, Avps}, ...]
chunk({grouped = T, [], Body}, Acc) ->
    store(T, parse_groups(Body), Acc);

%% avp_vendor_id -> [{Id, [AvpName, ...]}, ...]
chunk({avp_vendor_id = T, [{number, I}], Body}, Dict) ->
    [_|_] = Names = parse_avp_names(Body),
    append(T, {I, Names}, Dict);

%% enums -> [{AvpName, [{Value, Name}, ...]}, ...]
chunk({enum, [N], Str}, Dict) ->
    append(enums, {atomize(N), parse_enums(Str)}, Dict);

%% result_codes -> [{ResultName, [{Value, Name}, ...]}, ...]
chunk({result_code, [N], Str}, Dict) ->
    append(result_codes, {atomize(N), parse_enums(Str)}, Dict);

%% commands -> [{Name, Abbrev}, ...]
chunk({commands = T, [], Body}, Dict) ->
    store(T, parse_commands(Body), Dict);

chunk(T, _) ->
    ?ERROR({unknown_tag, T}).

store(Key, Value, Dict) ->
    error == orddict:find(Key, Dict) orelse ?ERROR({duplicate, Key}),
    orddict:store(Key, Value, Dict).
append(Key, Value, Dict) ->
    orddict:append(Key, Value, Dict).
append_list(Key, Values, Dict) ->
    orddict:append_list(Key, Values, Dict).

atomize({tag, T}) ->
    T;
atomize({name, T}) ->
    ?ATOM(T).

get_value(Keys, Spec)
  when is_list(Keys) ->
    [get_value(K, Spec) || K <- Keys];
get_value(Key, Spec) ->
    proplists:get_value(Key, Spec, []).

%% ------------------------------------------------------------------------
%% enums_defined/1
%% groups_defined/1
%% customs_defined/1
%%
%% Ensure that every local enum/grouped/custom is defined as an avp
%% with an appropriate type.

enums_defined(Spec) ->
    Avps = get_value(avp_types, Spec),
    Import = get_value(import_enums, Spec),
    lists:all(fun({N,_}) ->
                      true = enum_defined(N, Avps, Import)
              end,
              get_value(enums, Spec)).

enum_defined(Name, Avps, Import) ->
    case lists:keyfind(Name, 1, Avps) of
        {Name, _, 'Enumerated', _, _} ->
            true;
        {Name, _, T, _, _} ->
            ?ERROR({avp_has_wrong_type, Name, 'Enumerated', T});
        false ->
            lists:any(fun({_,Is}) -> lists:keymember(Name, 1, Is) end, Import)
                orelse ?ERROR({avp_not_defined, Name, 'Enumerated'})
    end.
%% Note that an AVP is imported only if referenced by a message or
%% grouped AVP, so the final branch will fail if an enum definition is
%% extended without this being the case.

groups_defined(Spec) ->
    Avps = get_value(avp_types, Spec),
    lists:all(fun({N,_,_,_}) -> true = group_defined(N, Avps) end,
              get_value(grouped, Spec)).

group_defined(Name, Avps) ->
    case lists:keyfind(Name, 1, Avps) of
        {Name, _, 'Grouped', _, _} ->
            true;
        {Name, _, T, _, _} ->
            ?ERROR({avp_has_wrong_type, Name, 'Grouped', T});
        false ->
            ?ERROR({avp_not_defined, Name, 'Grouped'})
    end.

customs_defined(Spec) ->
    Avps = get_value(avp_types, Spec),
    lists:all(fun(A) -> true = custom_defined(A, Avps) end,
              lists:flatmap(fun last/1, get_value(custom_types, Spec))).

custom_defined(Name, Avps) ->
    case lists:keyfind(Name, 1, Avps) of
        {Name, _, T, _, _} when T == 'Grouped';
                                T == 'Enumerated' ->
            ?ERROR({avp_has_invalid_custom_type, Name, T});
        {Name, _, _, _, _} ->
            true;
        false ->
            ?ERROR({avp_not_defined, Name})
    end.

last({_,Xs}) -> Xs.

%% ------------------------------------------------------------------------
%% v_flags_set/1

v_flags_set(Spec) ->
    Avps = get_value(avp_types, Spec)
        ++ lists:flatmap(fun last/1, get_value(import_avps, Spec)),
    Vs = lists:flatmap(fun last/1, get_value(avp_vendor_id, Spec)),

    lists:all(fun(N) -> vset(N, Avps) end, Vs).

vset(Name, Avps) ->
    A = lists:keyfind(Name, 1, Avps),
    false == A andalso ?ERROR({avp_not_defined, Name}),
    {Name, _Code, _Type, Flags, _Encr} = A,
    lists:member('V', Flags) orelse ?ERROR({v_flag_not_set, A}).

%% ------------------------------------------------------------------------
%% insert_codes/1

insert_codes(Spec) ->
    [Msgs, Cmds] = get_value([messages, commands], Spec),

    %% Code -> [{Name, Flags}, ...]
    Dict = lists:foldl(fun({N,C,Fs,_,_}, D) -> dict:append(C,{N,Fs},D) end,
                       dict:new(),
                       Msgs),

    %% list() of {Code, {ReqName, ReqAbbr}, {AnsName, AnsAbbr}}
    %% If the name and abbreviation are the same then the 2-tuples
    %% are replaced by the common atom()-valued name.
    Codes = dict:fold(fun(C,Ns,A) -> [make_code(C, Ns, Cmds) | A] end,
                      [],
                      dict:erase(-1, Dict)),  %% answer-message

    orddict:store(command_codes, Codes, Spec).

make_code(Code, [_,_] = Ns, Cmds) ->
    {Req, Ans} = make_names(Ns, lists:map(fun({_,Fs}) ->
                                                  lists:member('REQ', Fs)
                                          end,
                                          Ns)),
    {Code, abbrev(Req, Cmds), abbrev(Ans, Cmds)};

make_code(Code, Cs, _) ->
    ?ERROR({missing_request_or_answer, Code, Cs}).

%% 3.3.  Diameter Command Naming Conventions
%%
%%    Diameter command names typically includes one or more English words
%%    followed by the verb Request or Answer.  Each English word is
%%    delimited by a hyphen.  A three-letter acronym for both the request
%%    and answer is also normally provided.

make_names([{Rname,_},{Aname,_}], [true, false]) ->
    {Rname, Aname};
make_names([{Aname,_},{Rname,_}], [false, true]) ->
    {Rname, Aname};
make_names([_,_] = Names, _) ->
    ?ERROR({inconsistent_command_flags, Names}).

abbrev(Name, Cmds) ->
    case abbr(Name, get_value(Name, Cmds)) of
        Name ->
            Name;
        Abbr ->
            {Name, Abbr}
    end.

%% No explicit abbreviation: construct.
abbr(Name, []) ->
    ?ATOM(abbr(string:tokens(atom_to_list(Name), "-")));

%% Abbreviation was specified.
abbr(_Name, Abbr) ->
    Abbr.

%% No hyphens: already abbreviated.
abbr([Abbr]) ->
    Abbr;

%% XX-Request/Answer ==> XXR/XXA
abbr([[_,_] = P, T])
  when T == "Request";
       T == "Answer" ->
    P ++ [hd(T)];

%% XXX-...-YYY-Request/Answer ==> X...YR/X...YA
abbr([_,_|_] = L) ->
    lists:map(fun erlang:hd/1, L).

%% ------------------------------------------------------------------------
%% import_avps/2

import_avps(Spec, Options) ->
    Msgs = get_value(messages, Spec),
    Groups = get_value(grouped, Spec),

    %% Messages and groups require AVP's referenced by them.
    NeededAvps
        = ordsets:from_list(lists:flatmap(fun({_,_,_,_,As}) ->
                                                  [avp_name(A) || A <- As]
                                          end,
                                          Msgs)
                            ++ lists:flatmap(fun({_,_,_,As}) ->
                                                     [avp_name(A) || A <- As]
                                             end,
                                             Groups)),
    MissingAvps = missing_avps(NeededAvps, Spec),

    report(needed, NeededAvps),
    report(missing, MissingAvps),

    Import = inherit(get_value(inherits, Spec), Options),

    report(imported, Import),

    ImportedAvps = lists:map(fun({N,_,_,_,_}) -> N end,
                             lists:flatmap(fun last/1, Import)),

    Unknown = MissingAvps -- ImportedAvps,

    [] == Unknown orelse ?ERROR({undefined_avps, Unknown}),

    orddict:store(import_avps, Import, orddict:erase(inherits, Spec)).

%% missing_avps/2
%%
%% Given a list of AVP names and parsed spec, return the list of
%% AVP's that aren't defined in this spec.

missing_avps(NeededNames, Spec) ->
    Avps = get_value(avp_types, Spec),
    Groups = lists:map(fun({N,_,_,As}) ->
                               {N, [avp_name(A) || A <- As]}
                       end,
                       get_value(grouped, Spec)),
    Names = ordsets:from_list(['AVP' | lists:map(fun({N,_,_,_,_}) -> N end,
                                                 Avps)]),
    missing_avps(NeededNames, [], {Names, Groups}).

avp_name({'<',A,'>'}) -> A;
avp_name({A}) -> A;
avp_name([A]) -> A;
avp_name({_, A}) -> avp_name(A).

missing_avps(NeededNames, MissingNames, {Names, _} = T) ->
    missing(ordsets:filter(fun(N) -> lists:member(N, NeededNames) end, Names),
            ordsets:union(NeededNames, MissingNames),
            T).

%% Nothing found locally.
missing([], MissingNames, _) ->
    MissingNames;

%% Or not. Keep looking for for the AVP's needed by the found AVP's of
%% type Grouped.
missing(FoundNames, MissingNames, {_, Groups} = T) ->
    NeededNames = lists:flatmap(fun({N,As}) ->
                                  choose(lists:member(N, FoundNames), As, [])
                          end,
                          Groups),
    missing_avps(ordsets:from_list(NeededNames),
                 ordsets:subtract(MissingNames, FoundNames),
                 T).

%% inherit/2

inherit(Inherits, Options) ->
    Dirs = [D || {include, D} <- Options] ++ ["."],
    lists:foldl(fun(T,A) -> find_avps(T, A, Dirs) end, [], Inherits).

find_avps({Mod, AvpNames}, Acc, Path) ->
    report(inherit_from, Mod),
    Avps = avps_from_beam(find_beam(Mod, Path), Mod),  %% could be empty
    [{Mod, lists:sort(find_avps(AvpNames, Avps))} | Acc].

find_avps([], Avps) ->
    Avps;
find_avps(Names, Avps) ->
    lists:filter(fun({N,_,_,_,_}) -> lists:member(N, Names) end, Avps).

%% find_beam/2

find_beam(Mod, Dirs)
  when is_atom(Mod) ->
    find_beam(atom_to_list(Mod), Dirs);
find_beam(Mod, Dirs) ->
    Beam = Mod ++ code:objfile_extension(),
    case try_path(Dirs, Beam) of
        {value, Path} ->
            Path;
        false ->
            ?ERROR({beam_not_on_path, Beam, Dirs})
    end.

try_path([D|Ds], Fname) ->
    Path = filename:join(D, Fname),
    case file:read_file_info(Path) of
        {ok, _} ->
            {value, Path};
        _ ->
            try_path(Ds, Fname)
    end;
try_path([], _) ->
    false.

%% avps_from_beam/2

avps_from_beam(Path, Mod) ->
    report(beam, Path),
    ok = load_module(code:is_loaded(Mod), Mod, Path),
    orddict:fetch(avp_types, Mod:dict()).

load_module(false, Mod, Path) ->
    R = filename:rootname(Path, code:objfile_extension()),
    {module, Mod} = code:load_abs(R),
    ok;
load_module({file, _}, _, _) ->
    ok.

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

%% ------------------------------------------------------------------------
%% import_groups/1
%% import_enums/1
%%
%% For each inherited module, store the content of imported AVP's of
%% type grouped/enumerated in a new key.

import_groups(Spec) ->
    orddict:store(import_groups, import(grouped, Spec), Spec).

import_enums(Spec) ->
    orddict:store(import_enums, import(enums, Spec), Spec).

import(Key, Spec) ->
    lists:flatmap(fun(T) -> import_key(Key, T) end,
                  get_value(import_avps, Spec)).

import_key(Key, {Mod, Avps}) ->
    Imports = lists:flatmap(fun(T) ->
                                    choose(lists:keymember(element(1,T),
                                                           1,
                                                           Avps),
                                           [T],
                                           [])
                            end,
                            get_value(Key, Mod:dict())),
    if Imports == [] ->
            [];
       true ->
            [{Mod, Imports}]
    end.

%% ------------------------------------------------------------------------
%% parse_enums/1
%%
%% Enums are specified either as the integer value followed by the
%% name or vice-versa. In the former case the name of the enum is
%% taken to be the string up to the end of line, which may contain
%% whitespace. In the latter case the integer may be parenthesized,
%% specified in hex and followed by an inline comment. This is
%% historical and will likely be changed to require a precise input
%% format.
%%
%% Output: list() of {integer(), atom()}

parse_enums(Str) ->
    lists:flatmap(fun(L) -> parse_enum(trim(L)) end, string:tokens(Str, "\n")).

parse_enum([]) ->
    [];

parse_enum(Str) ->
    REs = [{"^(0[xX][0-9A-Fa-f]+|[0-9]+)\s+(.*?)\s*$", 1, 2},
           {"^(.+?)\s+(0[xX][0-9A-Fa-f]+|[0-9]+)(\s+.*)?$", 2, 1},
           {"^(.+?)\s+\\((0[xX][0-9A-Fa-f]+|[0-9]+)\\)(\s+.*)?$", 2, 1}],
    parse_enum(Str, REs).

parse_enum(Str, REs) ->
    try lists:foreach(fun(R) -> enum(Str, R) end, REs) of
        ok ->
            ?ERROR({bad_enum, Str})
    catch
        throw: {enum, T} ->
            [T]
    end.

enum(Str, {Re, I, N}) ->
    case re:run(Str, Re, [{capture, all_but_first, list}]) of
        {match, Vs} ->
            T = list_to_tuple(Vs),
            throw({enum, {to_int(element(I,T)), ?ATOM(element(N,T))}});
        nomatch ->
            ok
    end.

to_int([$0,X|Hex])
  when X == $x;
       X == $X ->
    {ok, [I], _} = io_lib:fread("~#", "16#" ++ Hex),
    I;
to_int(I) ->
    list_to_integer(I).

%% ------------------------------------------------------------------------
%% parse_messages/1
%%
%% Parse according to the ABNF for message specifications in 3.2 of
%% RFC 3588 (shown below). We require all message and AVP names to
%% start with a digit or uppercase character, except for the base
%% answer-message, which is treated as a special case. Allowing names
%% that start with a digit is more than the RFC specifies but the name
%% doesn't affect what's sent over the wire. (Certains 3GPP standards
%% use names starting with a digit. eg 3GPP-Charging-Id in TS32.299.)

%%
%% Sadly, not even the RFC follows this grammar. In particular, except
%% in the example in 3.2, it wraps each command-name in angle brackets
%% ('<' '>') which makes parsing a sequence of specifications require
%% lookahead: after 'optional' avps have been parsed, it's not clear
%% whether a '<' is a 'fixed' or whether it's the start of a
%% subsequent message until we see whether or not '::=' follows the
%% closing '>'. Require the grammar as specified.
%%
%% Output: list of {Name, Code, Flags, ApplId, Avps}
%%
%%         Name   = atom()
%%         Code   = integer()
%%         Flags  = integer()
%%         ApplId = [] | [integer()]
%%         Avps   = see parse_avps/1

parse_messages(Str) ->
    p_cmd(trim(Str), []).

%%   command-def      = command-name "::=" diameter-message
%%
%%   command-name     = diameter-name
%%
%%   diameter-name    = ALPHA *(ALPHA / DIGIT / "-")
%%
%%   diameter-message = header  [ *fixed] [ *required] [ *optional]
%%                      [ *fixed]
%%
%%   header           = "<" Diameter-Header:" command-id
%%                      [r-bit] [p-bit] [e-bit] [application-id]">"
%%
%% The header spec (and example that follows it) is slightly mangled
%% and, given the examples in the RFC should as follows:
%%
%%   header           = "<" "Diameter Header:" command-id
%%                      [r-bit] [p-bit] [e-bit] [application-id]">"
%%
%% This is what's required/parsed below, modulo whitespace. This is
%% also what's specified in the current draft standard at
%% http://ftp.ietf.org/drafts/wg/dime.
%%
%% Note that the grammar specifies the order fixed, required,
%% optional. In practise there seems to be little difference between
%% the latter two since qualifiers can be used to change the
%% semantics. For example 1*[XXX] and *1{YYY} specify 1 or more of the
%% optional avp XXX and 0 or 1 of the required avp YYY, making the
%% iotional avp required and the required avp optional. The current
%% draft addresses this somewhat by requiring that min for a qualifier
%% on an optional avp must be 0 if present. It doesn't say anything
%% about required avps however, so specifying a min of 0 would still
%% be possible. The draft also does away with the trailing *fixed.
%%
%% What will be parsed here will treat required and optional
%% interchangeably. That is. only require that required/optional
%% follow and preceed fixed, not that optional avps must follow
%% required ones. We already have several specs for which this parsing
%% is necessary and there seems to be no harm in accepting it.

p_cmd("", Acc) ->
    lists:reverse(Acc);

p_cmd(Str, Acc) ->
    {Next, Rest} = split_def(Str),
    report(command, Next),
    p_cmd(Rest, [p_cmd(Next) | Acc]).

p_cmd("answer-message" ++ Str) ->
    p_header([{name, 'answer-message'} | diameter_spec_scan:parse(Str)]);

p_cmd(Str) ->
    p_header(diameter_spec_scan:parse(Str)).

%% p_header/1

p_header(['<', {name, _} = N, '>' | Toks]) ->
    p_header([N | Toks]);

p_header([{name, 'answer-message' = N}, '::=',
          '<', {name, "Diameter"}, {name, "Header"}, ':', {tag, code},
          ',', {name, "ERR"}, '[', {name, "PXY"}, ']', '>'
          | Toks]) ->
    {N, -1, ['ERR', 'PXY'], [], parse_avps(Toks)};

p_header([{name, Name}, '::=',
          '<', {name, "Diameter"}, {name, "Header"}, ':', {number, Code}
          | Toks]) ->
    {Flags, Rest} = p_flags(Toks),
    {ApplId, [C|_] = R} = p_appl(Rest),
    '>' == C orelse ?ERROR({invalid_flag, {Name, Code, Flags, ApplId}, R}),
    {?ATOM(Name), Code, Flags, ApplId, parse_avps(tl(R))};

p_header(Toks) ->
    ?ERROR({invalid_header, Toks}).

%%   application-id   = 1*DIGIT
%%
%%   command-id       = 1*DIGIT
%%                      ; The Command Code assigned to the command
%%
%%   r-bit            = ", REQ"
%%                      ; If present, the 'R' bit in the Command
%%                      ; Flags is set, indicating that the message
%%                      ; is a request, as opposed to an answer.
%%
%%   p-bit            = ", PXY"
%%                      ; If present, the 'P' bit in the Command
%%                      ; Flags is set, indicating that the message
%%                      ; is proxiable.
%%
%%   e-bit            = ", ERR"
%%                      ; If present, the 'E' bit in the Command
%%                      ; Flags is set, indicating that the answer
%%                      ; message contains a Result-Code AVP in
%%                      ; the "protocol error" class.

p_flags(Toks) ->
    lists:foldl(fun p_flags/2, {[], Toks}, ["REQ", "PXY", "ERR"]).

p_flags(N, {Acc, [',', {name, N} | Toks]}) ->
    {[?ATOM(N) | Acc], Toks};

p_flags(_, T) ->
    T.

%% The RFC doesn't specify ',' before application-id but this seems a
%% bit inconsistent. Accept a comma if it exists.
p_appl([',', {number, I} | Toks]) ->
    {[I], Toks};
p_appl([{number, I} | Toks]) ->
    {[I], Toks};
p_appl(Toks) ->
    {[], Toks}.

%% parse_avps/1
%%
%% Output: list() of Avp | {Qual, Avp}
%%
%%         Qual = '*' | {Min, '*'} | {'*', Max} | {Min, Max}
%%         Avp  = {'<', Name, '>'} | {Name} | [Name]
%%
%%         Min, Max = integer() >= 0

parse_avps(Toks) ->
    p_avps(Toks, ['<', '|', '<'], []).
%% The list corresponds to the delimiters expected at the front, middle
%% and back of the avp specification, '|' representing '{' and '['.

%%   fixed            = [qual] "<" avp-spec ">"
%%                      ; Defines the fixed position of an AVP
%%
%%   required         = [qual] "{" avp-spec "}"
%%                      ; The AVP MUST be present and can appear
%%                      ; anywhere in the message.
%%
%%   optional         = [qual] "[" avp-name "]"
%%                      ; The avp-name in the 'optional' rule cannot
%%                      ; evaluate to any AVP Name which is included
%%                      ; in a fixed or required rule.  The AVP can
%%                      ; appear anywhere in the message.
%%
%%   qual             = [min] "*" [max]
%%                      ; See ABNF conventions, RFC 2234 Section 6.6.
%%                      ; The absence of any qualifiers depends on whether
%%                      ; it precedes a fixed, required, or optional
%%                      ; rule.  If a fixed or required rule has no
%%                      ; qualifier, then exactly one such AVP MUST
%%                      ; be present.  If an optional rule has no
%%                      ; qualifier, then 0 or 1 such AVP may be
%%                      ; present.
%%                      ;
%%                      ; NOTE:  "[" and "]" have a different meaning
%%                      ; than in ABNF (see the optional rule, above).
%%                      ; These braces cannot be used to express
%%                      ; optional fixed rules (such as an optional
%%                      ; ICV at the end).  To do this, the convention
%%                      ; is '0*1fixed'.
%%
%%   min              = 1*DIGIT
%%                      ; The minimum number of times the element may
%%                      ; be present.  The default value is zero.
%%
%%   max              = 1*DIGIT
%%                      ; The maximum number of times the element may
%%                      ; be present.  The default value is infinity.  A
%%                      ; value of zero implies the AVP MUST NOT be
%%                      ; present.
%%
%%   avp-spec         = diameter-name
%%                      ; The avp-spec has to be an AVP Name, defined
%%                      ; in the base or extended Diameter
%%                      ; specifications.
%%
%%   avp-name         = avp-spec / "AVP"
%%                      ; The string "AVP" stands for *any* arbitrary
%%                      ; AVP Name, which does not conflict with the
%%                      ; required or fixed position AVPs defined in
%%                      ; the command code definition.
%%

p_avps([], _, Acc) ->
    lists:reverse(Acc);

p_avps(Toks, Delim, Acc) ->
    {Qual, Rest} = p_qual(Toks),
    {Avp, R, D} = p_avp(Rest, Delim),
    T = if Qual == false ->
                Avp;
           true ->
                {Qual, Avp}
        end,
    p_avps(R, D, [T | Acc]).

p_qual([{number, Min}, '*', {number, Max} | Toks]) ->
    {{Min, Max}, Toks};
p_qual([{number, Min}, '*' = Max | Toks]) ->
    {{Min, Max}, Toks};
p_qual(['*' = Min, {number, Max} | Toks]) ->
    {{Min, Max}, Toks};
p_qual(['*' = Q | Toks]) ->
    {Q, Toks};
p_qual(Toks) ->
    {false, Toks}.

p_avp([B, {name, Name}, E | Toks], [_|_] = Delim) ->
    {avp(B, ?ATOM(Name), E),
     Toks,
     delim(choose(B == '<', B, '|'), Delim)};
p_avp(Toks, Delim) ->
    ?ERROR({invalid_avp, Toks, Delim}).

avp('<' = B, Name, '>' = E) ->
    {B, Name, E};
avp('{', Name, '}') ->
    {Name};
avp('[', Name, ']') ->
    [Name];
avp(B, Name, E) ->
    ?ERROR({invalid_avp, B, Name, E}).

delim(B, D) ->
    if B == hd(D) -> D; true -> tl(D) end.

%% split_def/1
%%
%% Strip one command definition off head of a string.

split_def(Str) ->
    sdh(Str, []).

%% Look for the "::=" starting off the definition.
sdh("", _) ->
    ?ERROR({missing, '::='});
sdh("::=" ++ Rest, Acc) ->
    sdb(Rest, [$=,$:,$:|Acc]);
sdh([C|Rest], Acc) ->
    sdh(Rest, [C|Acc]).

%% Look for the "::=" starting off the following definition.
sdb("::=" ++ _ = Rest, Acc) ->
    sdt(trim(Acc), Rest);
sdb("" = Rest, Acc) ->
    sd(Acc, Rest);
sdb([C|Rest], Acc) ->
    sdb(Rest, [C|Acc]).

%% Put name characters of the subsequent specification back into Rest.
sdt([C|Acc], Rest)
  when C /= $\n, C /= $\s ->
    sdt(Acc, [C|Rest]);

sdt(Acc, Rest) ->
    sd(Acc, Rest).

sd(Acc, Rest) ->
    {trim(lists:reverse(Acc)), Rest}.
%% Note that Rest is already trimmed of leading space.

%% ------------------------------------------------------------------------
%% parse_groups/1
%%
%% Parse according to the ABNF for message specifications in 4.4 of
%% RFC 3588 (shown below). Again, allow names starting with a digit
%% and also require "AVP Header" without "-" since this is what
%% the RFC uses in all examples.
%%
%% Output: list of {Name, Code, Vendor, Avps}
%%
%%         Name   = atom()
%%         Code   = integer()
%%         Vendor = [] | [integer()]
%%         Avps   = see parse_avps/1

parse_groups(Str) ->
    p_group(trim(Str), []).

%%      grouped-avp-def  = name "::=" avp
%%
%%      name-fmt         = ALPHA *(ALPHA / DIGIT / "-")
%%
%%      name             = name-fmt
%%                         ; The name has to be the name of an AVP,
%%                         ; defined in the base or extended Diameter
%%                         ; specifications.
%%
%%      avp              = header  [ *fixed] [ *required] [ *optional]
%%                         [ *fixed]
%%
%%      header           = "<" "AVP-Header:" avpcode [vendor] ">"
%%
%%      avpcode          = 1*DIGIT
%%                         ; The AVP Code assigned to the Grouped AVP
%%
%%      vendor           = 1*DIGIT
%%                         ; The Vendor-ID assigned to the Grouped AVP.
%%                         ; If absent, the default value of zero is
%%                         ; used.

p_group("", Acc) ->
    lists:reverse(Acc);

p_group(Str, Acc) ->
    {Next, Rest} = split_def(Str),
    report(group, Next),
    p_group(Rest, [p_group(diameter_spec_scan:parse(Next)) | Acc]).

p_group([{name, Name}, '::=', '<', {name, "AVP"}, {name, "Header"},
         ':', {number, Code}
         | Toks]) ->
    {Id, [C|_] = R} = p_vendor(Toks),
    C == '>' orelse ?ERROR({invalid_group_header, R}),
    {?ATOM(Name), Code, Id, parse_avps(tl(R))};

p_group(Toks) ->
    ?ERROR({invalid_group, Toks}).

p_vendor([{number, I} | Toks]) ->
    {[I], Toks};
p_vendor(Toks) ->
    {[], Toks}.

%% ------------------------------------------------------------------------
%% parse_avp_names/1

parse_avp_names(Str) ->
    [p_name(N) || N <- diameter_spec_scan:parse(Str)].

p_name({name, N}) ->
    ?ATOM(N);
p_name(T) ->
    ?ERROR({invalid_avp_name, T}).

%% ------------------------------------------------------------------------
%% parse_avp_types/1
%%
%% Output: list() of {Name, Code, Type, Flags, Encr}

parse_avp_types(Str) ->
    p_avp_types(Str, []).

p_avp_types(Str, Acc) ->
    p_type(diameter_spec_scan:split(Str, 3), Acc).

p_type({[],[]}, Acc) ->
    lists:reverse(Acc);

p_type({[{name, Name}, {number, Code}, {name, Type}], Str}, Acc) ->
    {Flags, Encr, Rest} = try
                              p_avp_flags(trim(Str), [])
                          catch
                              throw: {?MODULE, Reason} ->
                                  ?ERROR({invalid_avp_type, Reason})
                          end,
    p_avp_types(Rest, [{?ATOM(Name), Code, ?ATOM(type(Type)), Flags, Encr}
                       | Acc]);

p_type(T, _) ->
    ?ERROR({invalid_avp_type, T}).

p_avp_flags([C|Str], Acc)
  when C == $M;
       C == $P;
       C == $V ->
    p_avp_flags(Str, [?ATOM([C]) | Acc]);
%% Could support lowercase here if there's a use for distinguishing
%% between Must and Should in the future in deciding whether or not
%% to set a flag.

p_avp_flags([$-|Str], Acc) ->
    %% Require encr on same line as flags if specified.
    {H,T} = lists:splitwith(fun(C) -> C /= $\n end, Str),

    {[{name, [$X|X]} | Toks], Rest} = diameter_spec_scan:split([$X|H], 2),

    "" == X orelse throw({?MODULE, {invalid_avp_flag, Str}}),

    Encr = case Toks of
               [] ->
                   "-";
               [{_, E}] ->
                   (E == "Y" orelse E == "N")
                       orelse throw({?MODULE, {invalid_encr, E}}),
                   E
           end,

    Flags = ordsets:from_list(lists:reverse(Acc)),

    {Flags, ?ATOM(Encr), Rest ++ T};

p_avp_flags(Str, Acc) ->
    p_avp_flags([$-|Str], Acc).

type("DiamIdent")   -> "DiameterIdentity";  %% RFC 3588
type("DiamURI")     -> "DiameterURI";       %% RFC 3588
type("IPFltrRule")  -> "IPFilterRule";      %% RFC 4005
type("QoSFltrRule") -> "QoSFilterRule";     %% RFC 4005
type(N)
  when N == "OctetString";
       N == "Integer32";
       N == "Integer64";
       N == "Unsigned32";
       N == "Unsigned64";
       N == "Float32";
       N == "Float64";
       N == "Grouped";
       N == "Enumerated";
       N == "Address";
       N == "Time";
       N == "UTF8String";
       N == "DiameterIdentity";
       N == "DiameterURI";
       N == "IPFilterRule";
       N == "QoSFilterRule" ->
    N;
type(N) ->
    ?ERROR({invalid_avp_type, N}).

%% ------------------------------------------------------------------------
%% parse_commands/1

parse_commands(Str) ->
    p_abbr(diameter_spec_scan:parse(Str), []).

 p_abbr([{name, Name}, {name, Abbrev} | Toks], Acc)
  when length(Abbrev) < length(Name) ->
    p_abbr(Toks, [{?ATOM(Name), ?ATOM(Abbrev)} | Acc]);

p_abbr([], Acc) ->
    lists:reverse(Acc);

p_abbr(T, _) ->
    ?ERROR({invalid_command, T}).
