%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% This module turns a dictionary file into the orddict that
%% diameter_codegen.erl in turn morphs into .erl and .hrl files for
%% encode and decode of Diameter messages and AVPs.
%%

-module(diameter_dict_util).

-export([parse/2,
         format_error/1,
         format/1]).

-include("diameter_vsn.hrl").

-define(RETURN(T), throw({T, ?MODULE, ?LINE})).
-define(RETURN(T, Args), ?RETURN({T, Args})).

-define(A, list_to_atom).
-define(L, atom_to_list).
-define(I, integer_to_list).
-define(F, io_lib:format).

%% ===========================================================================
%% parse/2
%% ===========================================================================

-spec parse(File, Opts)
   -> {ok, orddict:orddict()}
    | {error, term()}
 when File :: {path, file:name_all()}
            | iolist()
            | binary(),
      Opts :: list().

parse(File, Opts) ->
    putr(verbose, lists:member(verbose, Opts)),
    try
        {ok, do_parse(File, Opts)}
    catch
        {Reason, ?MODULE, _Line} ->
            {error, Reason}
    after
        eraser(verbose)
    end.

%% ===========================================================================
%% format_error/1
%% ===========================================================================

format_error({read, Reason}) ->
    file:format_error(Reason);
format_error({scan, Reason}) ->
    diameter_dict_scanner:format_error(Reason);
format_error({parse, {Line, _Mod, Reason}}) ->
    lists:flatten(["Line ", ?I(Line), ", ", Reason]);

format_error(T) ->
    {Fmt, As} = fmt(T),
    lists:flatten(io_lib:format(Fmt, As)).

fmt({avp_code_already_defined = E, [Code, false, Name, Line, L]}) ->
    {fmt(E), [Code, "", Name, Line, L]};
fmt({avp_code_already_defined = E, [Code, Vid, Name, Line, L]}) ->
    {fmt(E), [Code, ?F("/~p", [Vid]), Name, Line, L]};

fmt({uint32_out_of_range = E, [id | T]}) ->
    {fmt(E), ["@id", "application identifier" | T]};
fmt({uint32_out_of_range = E, [K | T]})
  when K == vendor;
       K == avp_vendor_id ->
    {fmt(E), [?F("@~p", [K]), "vendor id" | T]};
fmt({uint32_out_of_range = E, [K, Name | T]})
  when K == enum;
       K == define ->
    {fmt(E), [?F("@~p ~s", [K, Name]), "value" | T]};
fmt({uint32_out_of_range = E, [avp_types, Name | T]}) ->
    {fmt(E), ["AVP " ++ Name, "AVP code" | T]};
fmt({uint32_out_of_range = E, [grouped, Name | T]}) ->
    {fmt(E), ["Grouped AVP " ++ Name | T]};
fmt({uint32_out_of_range = E, [messages, Name | T]}) ->
    {fmt(E), ["Message " ++ Name, "command code" | T]};

fmt({Reason, As}) ->
    {fmt(Reason), As};

fmt(avp_code_already_defined) ->
    "AVP ~p~s (~s) at line ~p already defined at line ~p";

fmt(uint32_out_of_range) ->
    "~s specifies ~s ~p at line ~p that is out of range for a value of "
    "Diameter type Unsigned32";

fmt(imported_avp_already_defined) ->
    "AVP ~s imported by @inherits ~p at line ~p defined at line ~p";
fmt(duplicate_import) ->
    "AVP ~s is imported by more than one @inherits, both at line ~p "
    "and at line ~p";

fmt(duplicate_section) ->
    "Section @~s at line ~p already declared at line ~p";

fmt(already_declared) ->
    "Section @~p ~s at line ~p already declared at line ~p";

fmt(inherited_avp_already_defined) ->
    "AVP ~s inherited at line ~p defined in @avp_types at line ~p";
fmt(avp_already_defined) ->
    "AVP ~s at line ~p already in @~p at line ~p";
fmt(key_already_defined) ->
    "Value for ~s:~s in @~p at line ~p already provided at line ~p";

fmt(messages_without_id) ->
    "@messages at line ~p but @id not declared";

fmt(avp_name_already_defined) ->
    "AVP ~s at line ~p already defined at line ~p";
fmt(avp_has_unknown_type) ->
    "AVP ~s at line ~p defined with unknown type ~s";
fmt(avp_has_invalid_flag) ->
    "AVP ~s at line ~p specifies invalid flag ~c";
fmt(avp_has_duplicate_flag) ->
    "AVP ~s at line ~p specifies duplicate flag ~c";
fmt(avp_has_vendor_id) ->
    "AVP ~s at line ~p does not specify V flag "
    "but is assigned vendor id ~p at line ~p";
fmt(avp_has_no_vendor) ->
    "AVP ~s at line ~p specifies V flag "
    "but neither @vendor_avp_id nor @vendor supplies a value";

fmt(group_already_defined) ->
    "Group ~s at line ~p already defined at line ~p";
fmt(grouped_avp_code_mismatch) ->
    "AVP ~s at line ~p has with code ~p "
    "but @avp_types specifies ~p at line ~p";
fmt(grouped_avp_has_wrong_type) ->
    "Grouped AVP ~s at line ~p defined with type ~s at line ~p";
fmt(grouped_avp_not_defined) ->
    "Grouped AVP ~s on line ~p not defined in @avp_types";
fmt(grouped_avp_not_grouped) ->
    "Grouped AVP ~s on line ~p not defined in @grouped";
fmt(grouped_vendor_id_without_flag) ->
    "Grouped AVP ~s at line ~p has vendor id "
    "but definition at line ~p does not specify V flag";
fmt(grouped_vendor_id_mismatch) ->
    "Grouped AVP ~s at line ~p has vendor id ~p "
    "but ~p specified at line ~p";

fmt(message_name_already_defined) ->
    "Message ~s at line ~p already defined at line ~p";
fmt(message_code_already_defined) ->
    "~s message with code ~p at line ~p already defined at line ~p";
fmt(message_has_duplicate_flag) ->
    "Message ~s has duplicate flag ~s at line ~p";
fmt(message_application_id_mismatch) ->
    "Message ~s has application id ~p at line ~p "
    "but @id specifies ~p at line ~p";

fmt(invalid_avp_order) ->
    "AVP reference ~c~s~c at line ~p breaks fixed/required/optional order";
fmt(required_avp_has_zero_max_arity) ->
    "Required AVP has maximum arity 0 at line ~p";
fmt(required_avp_has_zero_min_arity) ->
    "Required AVP has minimum arity 0 at line ~p";
fmt(optional_avp_has_nonzero_min_arity) ->
    "Optional AVP has non-zero minimum arity at line ~p";
fmt(qualifier_has_min_greater_than_max) ->
    "Qualifier ~p*~p at line ~p has Min > Max";
fmt(avp_already_referenced) ->
    "AVP ~s at line ~p already referenced at line ~p";

fmt(message_missing) ->
    "~s message at line ~p but no ~s message is defined";

fmt(requested_avp_not_found) ->
    "@inherit ~s at line ~p requests AVP ~s at line ~p "
    "but module does not define that AVP";

fmt(enumerated_avp_has_wrong_local_type) ->
    "Enumerated AVP ~s in @enum at line ~p defined with type ~s at line ~p";
fmt(enumerated_avp_has_wrong_inherited_type) ->
    "Enumerated AVP ~s in @enum at line ~p "
    "inherited with type ~s from module ~s at line ~p";
fmt(enumerated_avp_not_defined) ->
    "Enumerated AVP ~s in @enum at line ~p neither defined nor inherited";

fmt(avp_not_defined) ->
    "AVP ~s referenced at line ~p neither defined nor inherited";

fmt(recompile) ->
    "Module ~p appears to have been compiler with an incompatible "
    "version of the dictionary compiler and must be recompiled";
fmt(not_loaded) ->
    "Module ~p is not on the code path or could not be loaded";
fmt(no_dict) ->
    "Module ~p does not appear to be a diameter dictionary".

%% ===========================================================================
%% format/1
%%
%% Turn dict/0 output back into a dictionary file (with line ending = $\n).

-spec format(Dict)
   -> iolist()
 when Dict :: orddict:orddict().

-define(KEYS, [id, name, prefix, vendor,
               inherits, codecs, custom_types,
               avp_types,
               messages,
               grouped,
               enum, define]).

format(Dict) ->
    Io = orddict:fold(fun io/3, [], Dict),
    [S || {_,S} <- lists:sort(fun keysort/2, Io)].

keysort({A,_}, {B,_}) ->
    [HA, HB] = [H || K <- [A,B],
                     H <- [lists:takewhile(fun(X) -> X /= K end, ?KEYS)]],
    HA < HB.

%% ===========================================================================

-define(INDENT, "    ").
-define(SP, " ").
-define(NL, $\n).

%% io/3

io(K, _, Acc)
  when K == command_codes;
       K == import_avps;
       K == import_groups;
       K == import_enums ->
    Acc;

io(Key, Body, Acc) ->
    [{Key, io(Key, Body)} | Acc].

%% io/2

io(K, Id)
  when K == id;
       K == name;
       K == prefix ->
    [?NL, section(K), ?SP, tok(Id)];

io(vendor = K, {Id, Name}) ->
    [?NL, section(K) | [[?SP, tok(X)] || X <- [Id, Name]]];

io(_, []) ->
    [];

io(avp_types = K, Body) ->
    [?NL, ?NL, section(K), ?NL, [body(K,A) || A <- Body]];

io(K, Body)
  when K == messages;
       K == grouped ->
    [?NL, ?NL, section(K), [body(K,A) || A <- Body]];

io(K, Body)
  when K == avp_vendor_id;
       K == inherits;
       K == custom_types;
       K == codecs;
       K == enum;
       K == define ->
    [[?NL, pairs(K, T)] || T <- Body].

pairs(K, {Id, Avps}) ->
    [?NL, section(K), ?SP, tok(Id), ?NL, [[?NL, body(K, A)] || A <- Avps]].

body(K, AvpName)
  when K == avp_vendor_id;
       K == inherits;
       K == custom_types;
       K == codecs ->
    [?INDENT, word(AvpName)];

body(K, {Name, N})
  when K == enum;
       K == define ->
    [?INDENT, word(Name), ?SP, ?I(N)];

body(avp_types = K, {Name, Code, Type, ""}) ->
    body(K, {Name, Code, Type, "-"});
body(avp_types, {Name, Code, Type, Flags}) ->
    [?NL, ?INDENT, word(Name),
     [[?SP, ?SP, S] || S <- [?I(Code), Type, Flags]]];

body(messages, {"answer-message", _, _, [], Avps}) ->
    [?NL, ?NL, ?INDENT,
     "answer-message ::= < Diameter Header: code, ERR [PXY] >",
     f_avps(Avps)];
body(messages, {Name, Code, Flags, ApplId, Avps}) ->
    [?NL, ?NL, ?INDENT, word(Name), " ::= ", header(Code, Flags, ApplId),
     f_avps(Avps)];

body(grouped, {Name, Code, Vid, Avps}) ->
    [?NL, ?NL, ?INDENT, word(Name), " ::= ", avp_header(Code, Vid),
     f_avps(Avps)].

header(Code, Flags, ApplId) ->
    ["< Diameter Header: ",
     ?I(Code),
     [[", ", ?L(F)] || F <- Flags],
     [[" ", ?I(N)] || N <- ApplId],
     " >"].

avp_header(Code, Vid) ->
    ["< AVP Header: ",
     ?I(Code),
     [[" ", ?I(V)] || V <- Vid],
     " >"].

f_avps(L) ->
    [[?NL, ?INDENT, ?INDENT, f_avp(A)] || A <- L].

f_avp({Q, A}) ->
    [D | _] = Avp = f_delim(A),
    f_avp(f_qual(D, Q), Avp);
f_avp(A) ->
    f_avp("", f_delim(A)).

f_delim({{A}}) ->
    [$<, word(A), $>];
f_delim({A}) ->
    [${, word(A), $}];
f_delim([A]) ->
    [$[, word(A), $]].

f_avp(Q, [L, Avp, R]) ->
    Len = length(lists:flatten([Q])),
    [io_lib:format("~*s", [-1*max(Len+1, 6) , Q]), L, " ", Avp, " ", R].

f_qual(${, '*') ->
    "1*";  %% Equivalent to "*" but the more common/obvious rendition
f_qual(_, '*') ->
    "*";
f_qual(_, {'*', N}) ->
    [$*, ?I(N)];
f_qual(_, {N, '*'}) ->
    [?I(N), $*];
f_qual(_, {M,N}) ->
    [?I(M), $*, ?I(N)].

section(Key) ->
    ["@", ?L(Key)].

tok(N)
  when is_integer(N) ->
    ?I(N);
tok(N) ->
    word(N).

word(Str) ->
    word(diameter_dict_scanner:is_name(Str), Str).

word(true, Str) ->
    Str;
word(false, Str) ->
    [$', Str, $'].

%% ===========================================================================

do_parse(File, Opts) ->
    Bin  = do([fun read/1, File], read),
    Toks = do([fun diameter_dict_scanner:scan/1, Bin], scan),
    Tree = do([fun diameter_dict_parser:parse/1, Toks], parse),
    make_dict(Tree, Opts).

do([F|A], E) ->
    case apply(F,A) of
        {ok, T} ->
            T;
        {error, Reason} ->
            ?RETURN({E, Reason})
    end.

read({path, Path}) ->
    file:read_file(Path);
read(File) ->
    {ok, iolist_to_binary([File])}.

make_dict(Parse, Opts) ->
    Dict = pass3(pass2(pass1(reset(make_dict(Parse), Opts))), Opts),
    ok = examine(Dict),
    make_orddict(Dict).

%% make_orddict/1

make_orddict(Dict) ->
    dict:fold(fun mo/3,
              orddict:from_list([{K,[]} || K <- [avp_types,
                                                 messages,
                                                 grouped,
                                                 inherits,
                                                 custom_types,
                                                 codecs,
                                                 avp_vendor_id,
                                                 enum,
                                                 define]]),
              Dict).

mo(K, Sects, Dict)
  when is_atom(K) ->
    orddict:store(K, make(K, Sects), Dict);

mo(_, _, Dict) ->
    Dict.

make(K, [[_Line, {_, _, X}]])
  when K == id;
       K == name;
       K == prefix ->
    X;

make(vendor, [[_Line, {_, _, Id}, {_, _, Name}]]) ->
    {Id, Name};

make(K, T)
  when K == command_codes;
       K == import_avps;
       K == import_groups;
       K == import_enums ->
    T;

make(K, Sects) ->
    post(K, foldl(fun([_L|B], A) -> make(K,B,A) end,
                  [],
                  Sects)).

post(avp_types, L) ->
    lists:sort(L);

post(K, L)
  when K == grouped;
       K == messages;
       K == enum;
       K == define ->
    lists:reverse(L);

post(_, L) ->
    L.

make(K, [{_,_,Name} | Body], Acc)
  when K == enum;
       K == define;
       K == avp_vendor_id;
       K == custom_types;
       K == inherits;
       K == codecs ->
    [{Name, mk(K, Body)} | Acc];

make(K, Body, Acc) ->
    foldl(fun(T,A) -> [mk(K, T) | A] end, Acc, Body).

mk(avp_types, [{_,_,Name}, {_,_,Code}, {_,_,Type}, {_,_,Flags}]) ->
    {Name, Code, type(Type), Flags};

mk(messages, [{'answer-message' = A, _}, false | Avps]) ->
    {?L(A), -1, ['ERR', 'PXY'], [], make_body(Avps)};

mk(messages, [{_,_,Name}, [{_,_,Code}, Flags, ApplId] | Avps]) ->
    {Name,
     Code,
     lists:map(fun({F,_}) -> F end, Flags),
     opt(ApplId),
     make_body(Avps)};

mk(grouped, [{_,_,Name}, [{_,_,Code}, Vid] | Avps]) ->
    {Name, Code, opt(Vid), make_body(Avps)};

mk(K, Body)
  when K == enum;
       K == define ->
    lists:map(fun([{_,_,Name}, {_,_,Value}]) -> {Name, Value} end, Body);

mk(K, Avps)
  when K == avp_vendor_id;
       K == custom_types;
       K == inherits;
       K == codecs ->
    lists:map(fun({_,_,N}) -> N end, Avps).

opt(false) ->
    [];
opt({_,_,X}) ->
    [X].

make_body(Avps) ->
    lists:map(fun avp/1, Avps).

avp([false, D, Avp]) ->
    avp(D, Avp);
avp([Q, D, Avp]) ->
    case {qual(D, Q), avp(D, Avp)} of
        {{0,1}, A} when D == $[ ->
            A;
        {{1,1}, A} ->
            A;
        T ->
            T
    end.
%% Could just store the qualifier as a pair in all cases but the more
%% compact form is easier to parse visually so live with a bit of
%% mapping. Ditto the use of '*'.

avp(D, {'AVP', _}) ->
    delim(D, "AVP");
avp(D, {_, _, Name}) ->
    delim(D, Name).

delim($<, N) ->
    {{N}};
delim(${, N) ->
    {N};
delim($[, N) ->
    [N].

%% There's a difference between max = 0 and not specifying an AVP:
%% reception of an AVP with max = 0 will always be an error, otherwise
%% it depends on the existence of 'AVP' and the M flag.

qual(${, {{_,L,0}, _}) ->
    ?RETURN(required_avp_has_zero_min_arity, [L]);
qual(${, {_, {_,L,0}}) ->
    ?RETURN(required_avp_has_zero_max_arity, [L]);

qual($[, {{_,L,N}, _})
  when 0 < N ->
    ?RETURN(optional_avp_has_nonzero_min_arity, [L]);

qual(_, {{_,L,Min}, {_,_,Max}})
  when Min > Max ->
    ?RETURN(qualifier_has_min_greater_than_max, [Min, Max, L]);

qual(_, true) ->
    '*';

qual(${, {true, {_,_,N}}) ->
    {1, N};
qual(_, {true, {_,_,N}}) ->
    {0, N};

qual(D, {{_,_,N}, true})
  when D == ${, N == 1;
       D /= ${, N == 0 ->
    '*';
qual(_, {{_,_,N}, true}) ->
    {N, '*'};

qual(_, {{_,_,Min}, {_,_,Max}}) ->
    {Min, Max}.

%% Optional reports when running verbosely.
report(What, [F | A])
  when is_function(F) ->
    report(What, apply(F, A));
report(What, Data) ->
    report(getr(verbose), What, Data).

report(true, Tag, Data) ->
    io:format("##~n## ~p ~p~n", [Tag, Data]);
report(false, _, _) ->
    ok.

%% ------------------------------------------------------------------------
%% make_dict/1
%%
%% Turn a parsed dictionary into an dict.

make_dict(Parse) ->
    foldl(fun(T,A) ->
                  report(section, T),
                  section(T,A)
          end,
          dict:new(),
          Parse).

section([{T, L} | Rest], Dict)
  when T == name;
       T == prefix;
       T == id;
       T == vendor ->
    case find(T, Dict) of
        [] ->
            dict:store(T, [[L | Rest]], Dict);
        [[Line | _]] ->
            ?RETURN(duplicate_section, [T, L, Line])
    end;

section([{T, L} | Rest], Dict)
  when T == avp_types;
       T == messages;
       T == grouped;
       T == inherits;
       T == custom_types;
       T == codecs;
       T == avp_vendor_id;
       T == enum;
       T == define ->
    dict:append(T, [L | Rest], Dict).

%% ===========================================================================
%% reset/2
%%
%% Reset sections from options.

reset(Dict, Opts) ->
    foldl([fun reset/3, Opts], Dict, [name, prefix, inherits]).

reset(K, Dict, Opts) ->
    foldl(fun opt/2, Dict, [T || {A,_} = T <- Opts, A == K]).

opt({inherits = Key, "-"}, Dict) ->
    dict:erase(Key, Dict);

opt({inherits = Key, Mod}, Dict) ->
    case lists:splitwith(fun(C) -> C /= $/ end, Mod) of
        {Mod, ""} ->
            dict:append(Key, [0, {word, 0, Mod}], Dict);
        {From, [$/|To]} ->
            dict:store(Key,
                       [reinherit(From, To, M) || M <- find(Key, Dict)],
                       Dict)
    end;

opt({Key, Val}, Dict) ->
    dict:store(Key, [[0, {word, 0, Val}]], Dict);

opt(_, Dict) ->
    Dict.

reinherit(From, To, [L, {word, _, From} = T | Avps]) ->
    [L, setelement(3, T, To) | Avps];
reinherit(_, _, T) ->
    T.

%% ===========================================================================
%% pass1/1
%%
%% Explode sections into additional dictionary entries plus semantic
%% checks.

pass1(Dict) ->
    true = no_messages_without_id(Dict),

    foldl(fun(K,D) -> foldl([fun p1/3, K], D, find(K,D)) end,
          Dict,
          [id,
           vendor,
           avp_types,  %% must precede inherits, grouped, enum
           avp_vendor_id,
           custom_types,
           codecs,
           inherits,
           grouped,
           messages,
           enum,
           define]).

%% Multiple sections are allowed as long as their bodies don't
%% overlap. (Except enum/define.)

p1([_Line, N], Dict, id = K) ->
    true = is_uint32(N, [K]),
    Dict;

p1([_Line, Id, _Name], Dict, vendor = K) ->
    true = is_uint32(Id, [K]),
    Dict;

p1([_Line, X | Body], Dict, K)
  when K == avp_vendor_id;
       K == custom_types;
       K == codecs;
       K == inherits ->
    foldl([fun explode/4, X, K], Dict, Body);

p1([_Line, X | Body], Dict, K)
  when K == define;
       K == enum ->
    {_, L, Name} = X,
    foldl([fun explode2/4, X, K],
          store_new({K, Name},
                    [L, Body],
                    Dict,
                    [K, Name, L],
                    already_declared),
          Body);

p1([_Line | Body], Dict, K)
  when K == avp_types;
       K == grouped;
       K == messages ->
    foldl([fun explode/3, K], Dict, Body).

no_messages_without_id(Dict) ->
    case find(messages, Dict) of
        [] ->
            true;
        [[Line | _] | _] ->
            [] /= find(id, Dict) orelse ?RETURN(messages_without_id, [Line])
    end.

%% Note that the AVP's in avp_vendor_id, custom_types, codecs and
%% enum can all be inherited, as can the AVP content of messages and
%% grouped AVP's. Check that the referenced AVP's exist after
%% importing definitions.

%% explode/4
%%
%% {avp_vendor_id, AvpName}    -> [Lineno, Id::integer()]
%% {custom|inherits,  AvpName} -> [Lineno, Mod::string()]

explode({_, Line, AvpName}, Dict, {_, _, X} = T, K) ->
    true = K /= avp_vendor_id orelse is_uint32(T, [K]),
    true = K /= inherits orelse avp_not_local(AvpName, Line, Dict),

    store_new({key(K), AvpName},
              [Line, X],
              Dict,
              [AvpName, Line, K],
              avp_already_defined).

%% explode2/4

%% {define, {Name, Key}} -> [Lineno, Value::integer(), enum|define]

explode2([{_, Line, Key}, {_, _, Value} = T], Dict, {_, _, Name}, K) ->
    true = is_uint32(T, [K, Name]),

    store_new({key(K), {Name, Key}},
              [Line, Value, K],
              Dict,
              [Name, Key, K, Line],
              key_already_defined).

%% key/1
%%
%% Conflate keys that are equivalent as far as uniqueness of
%% definition goes.

key(K)
  when K == enum;
       K == define ->
    define;
key(K)
  when K == custom_types;
       K == codecs ->
    custom;
key(K) ->
    K.

%% explode/3

%% {avp_types, AvpName}       -> [Line | Toks]
%% {avp_types, {Code, IsReq}} -> [Line, AvpName]
%%
%% where AvpName = string()
%%       Code    = integer()
%%       IsReq   = boolean()

explode([{_, Line, Name} | Toks], Dict0, avp_types = K) ->
    %% Each AVP can be defined only once.
    Dict = store_new({K, Name},
                     [Line | Toks],
                     Dict0,
                     [Name, Line],
                     avp_name_already_defined),

    [{number, _, _Code} = C, {word, _, Type}, {word, _, _Flags}] = Toks,

    true = avp_type_known(Type, Name, Line),
    true = is_uint32(C, [K, Name]),

    Dict;

%% {grouped, Name}            -> [Line, HeaderTok | AvpToks]
%% {grouped, {Name, AvpName}} -> [Line, Qual, Delim]
%%
%% where Name  = string()
%%       AvpName = string()
%%       Qual  = {Q, Q} | boolean()
%%       Q     = true | NumberTok
%%       Delim = $< | ${ | $[

explode([{_, Line, Name}, Header | Avps], Dict0, grouped = K) ->
    Dict = store_new({K, Name},
                     [Line, Header | Avps],
                     Dict0,
                     [Name, Line],
                     group_already_defined),

    [{_,_, Code} = C, Vid] = Header,
    {DefLine, {_, _, Flags}} = grouped_flags(Name, Code, Dict0, Line),
    V = lists:member($V, Flags),

    true = is_uint32(C, [K, Name, "AVP code"]),
    true = is_uint32(Vid, [K, Name, "vendor id"]),
    false = vendor_id_mismatch(Vid, V, Name, Dict0, Line, DefLine),

    explode_avps(Avps, Dict, K, Name);

%% {messages, Name}            -> [Line, HeaderTok | AvpToks]
%% {messages, {Code, IsReq}}   -> [Line, NameTok]
%% {messages, Code}            -> [[Line, NameTok, IsReq]]
%% {messages, {Name, Flag}}    -> [Line]
%% {messages, {Name, AvpName}} -> [Line, Qual, Delim]
%%
%% where Name  = string()
%%       Code  = integer()
%%       IsReq = boolean()
%%       Flag  = 'REQ' | 'PXY'
%%       AvpName = string()
%%       Qual  = true | {Q,Q}
%%       Q     = true | NumberTok
%%       Delim = $< | ${ | ${

explode([{'answer-message' = A, Line}, false = H | Avps],
        Dict0,
        messages = K) ->
    Name = ?L(A),
    Dict1 = store_new({K, Name},
                      [Line, H, Avps],
                      Dict0,
                      [Name, Line],
                      message_name_already_defined),

    explode_avps(Avps, Dict1, K, Name);

explode([{_, Line, MsgName} = M, Header | Avps],
        Dict0,
        messages = K) ->
    %% There can be at most one message with a given name.
    Dict1 = store_new({K, MsgName},
                      [Line, Header | Avps],
                      Dict0,
                      [MsgName, Line],
                      message_name_already_defined),

    [{_, _, Code} = C, Bits, ApplId] = Header,

    %% Don't check any application id since it's required to be
    %% the same as @id.
    true = is_uint32(C, [K, MsgName]),

    %% An application id specified as part of the message definition
    %% has to agree with @id. The former is parsed just because RFC
    %% 3588 specifies it.
    false = application_id_mismatch(ApplId, Dict1, MsgName),

    IsReq = lists:keymember('REQ', 1, Bits),

    %% For each command code, there can be at most one request and
    %% one answer.
    Dict2 = store_new({K, {Code, IsReq}},
                      [Line, M],
                      Dict1,
                      [choose(IsReq, "Request", "Answer"), Code, Line],
                      message_code_already_defined),

    %% For each message, each flag can occur at most once.
    Dict3 = foldl(fun({F,L},D) ->
                          store_new({K, {MsgName, F}},
                                    [L],
                                    D,
                                    [MsgName, ?L(F)],
                                    message_has_duplicate_flag)
                  end,
                  Dict2,
                  Bits),

    dict:append({K, Code},
                [Line, M, IsReq],
                explode_avps(Avps, Dict3, K, MsgName)).

%% explode_avps/4
%%
%% Ensure required AVP order and sane qualifiers. Can't check for AVP
%% names until after they've been imported.
%%
%% RFC 3588 allows a trailing fixed while 3588bis doesn't. Parse the
%% former.

explode_avps(Avps, Dict, Key, Name) ->
    xa("<{[<", Avps, Dict, Key, Name).

xa(_, [], Dict, _, _) ->
    Dict;

xa(Ds, [[Qual, D, {'AVP', Line}] | Avps], Dict, Key, Name) ->
    xa(Ds, [[Qual, D, {word, Line, "AVP"}] | Avps], Dict, Key, Name);

xa([], [[_Qual, D, {_, Line, Name}] | _], _, _, _) ->
    ?RETURN(invalid_avp_order, [D, Name, close(D), Line]);

xa([D|_] = Ds, [[Qual, D, {_, Line, AvpName}] | Avps], Dict, Key, Name) ->
    xa(Ds,
       Avps,
       store_new({Key, {Name, AvpName}},
                 [Line, Qual, D],
                 Dict,
                 [Name, Line],
                 avp_already_referenced),
       Key,
       Name);

xa([_|Ds], Avps, Dict, Key, Name) ->
    xa(Ds, Avps, Dict, Key, Name).

close($<) -> $>;
close(${) -> $};
close($[) -> $].

%% is_uint32/2

is_uint32(false, _) ->
    true;
is_uint32({Line, _, N}, Args) ->
    N < 1 bsl 32 orelse ?RETURN(uint32_out_of_range, Args ++ [N, Line]).
%% Can't call diameter_types here since it may not exist yet.

%% application_id_mismatch/3

application_id_mismatch({number, Line, Id}, Dict, MsgName) ->
    [[_, {_, L, I}]] = dict:fetch(id, Dict),

    I /= Id andalso ?RETURN(message_application_id_mismatch,
                            [MsgName, Id, Line, I, L]);

application_id_mismatch(false = No, _, _) ->
    No.

%% avp_not_local/3

avp_not_local(Name, Line, Dict) ->
    A = find({avp_types, Name}, Dict),

    [] == A orelse ?RETURN(inherited_avp_already_defined,
                           [Name, Line, hd(A)]).

%% avp_type_known/3

avp_type_known(Type, Name, Line) ->
    false /= type(Type)
        orelse ?RETURN(avp_has_unknown_type, [Name, Line, Type]).

%% vendor_id_mismatch/6
%%
%% Require a vendor id specified on a group to match any specified
%% in @avp_vendor_id. Note that both locations for the value are
%% equivalent, both in the value being attributed to a locally
%% defined AVP and ignored when imported from another dictionary.

vendor_id_mismatch({_,_,_}, false, Name, _, Line, DefLine) ->
    ?RETURN(grouped_vendor_id_without_flag, [Name, Line, DefLine]);

vendor_id_mismatch({_, _, I}, true, Name, Dict, Line, _) ->
    case vendor_id(Name, Dict) of
        {avp_vendor_id, L, N} ->
            I /= N andalso
                ?RETURN(grouped_vendor_id_mismatch, [Name, Line, I, N, L]);
        _ ->
            false
    end;

vendor_id_mismatch(_, _, _, _, _, _) ->
    false.

%% grouped_flags/4

grouped_flags(Name, Code, Dict, Line) ->
    case find({avp_types, Name}, Dict) of
        [L, {_, _, Code}, {_, _, "Grouped"}, Flags] ->
            {L, Flags};
        [_, {_, L, C}, {_, _, "Grouped"}, _Flags] ->
            ?RETURN(grouped_avp_code_mismatch, [Name, Line, Code, C, L]);
        [_, _Code, {_, L, T}, _] ->
            ?RETURN(grouped_avp_has_wrong_type, [Name, Line, T, L]);
        [] ->
            ?RETURN(grouped_avp_not_defined, [Name, Line])
    end.

%% vendor_id/2

%% Look for a vendor id in @avp_vendor_id, then @vendor.
vendor_id(Name, Dict) ->
    case find({avp_vendor_id, Name}, Dict) of
        [Line, Id] when is_integer(Id) ->
            {avp_vendor_id, Line, Id};
        [] ->
            vendor(Dict)
    end.

vendor(Dict) ->
    case find(vendor, Dict) of
        [[_Line, {_, _, Id}, {_, _, _}]] ->
            {vendor, Id};
        [] ->
            false
    end.

%% find/2

find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, L} when is_list(L) ->
            L;
        error ->
            []
    end.

%% store_new/5

store_new(Key, Value, Dict, Args, Err) ->
    case dict:find(Key, Dict) of
        {ok, [L | _]} ->
            ?RETURN(Err, Args ++ [L]);
        error ->
            dict:store(Key, Value, Dict)
    end.

%% type/1

type("DiamIdent") ->
    "DiameterIdentity";
type("DiamURI") ->
    "DiameterURI";
type(T)
  when T == "OctetString";
       T == "Integer32";
       T == "Integer64";
       T == "Unsigned32";
       T == "Unsigned64";
       T == "Float32";
       T == "Float64";
       T == "Grouped";
       T == "Enumerated";
       T == "Address";
       T == "Time";
       T == "UTF8String";
       T == "DiameterIdentity";
       T == "DiameterURI";
       T == "IPFilterRule";
       T == "QoSFilterRule" ->
    T;
type(_) ->
    false.

%% ===========================================================================
%% pass2/1
%%
%% More explosion, but that requires the previous pass to write its
%% entries.

pass2(Dict) ->
    foldl(fun(K,D) -> foldl([fun p2/3, K], D, find(K,D)) end,
          Dict,
          [avp_types]).

p2([_Line | Body], Dict, avp_types) ->
    foldl(fun explode_avps/2, Dict, Body);

p2([], Dict, _) ->
    Dict.

explode_avps([{_, Line, Name} | Toks], Dict) ->
    [{number, _, Code}, {word, _, _Type}, {word, _, Flags}] = Toks,

    true = avp_flags_valid(Flags, Name, Line),

    Vid = avp_vendor_id(Flags, Name, Line, Dict),

    %% An AVP is uniquely defined by its AVP code and vendor id (if any).
    %% Ensure there are no duplicates.
    store_new({avp_types, {Code, Vid}},
              [Line, Name],
              Dict,
              [Code, Vid, Name, Line],
              avp_code_already_defined).

%% avp_flags_valid/3

avp_flags_valid(Flags, Name, Line) ->
    Bad = lists:filter(fun(C) -> not lists:member(C, "MVP") end, Flags),
    [] == Bad
        orelse ?RETURN(avp_has_invalid_flag, [Name, Line, hd(Bad)]),

    Dup = Flags -- "MVP",
    [] == Dup
        orelse ?RETURN(avp_has_duplicate_flag, [Name, Line, hd(Dup)]).

%% avp_vendor_id/4

avp_vendor_id(Flags, Name, Line, Dict) ->
    V = lists:member($V, Flags),

    case vendor_id(Name, Dict) of
        {avp_vendor_id, _, I} when V ->
            I;
        {avp_vendor_id, L, I} ->
            ?RETURN(avp_has_vendor_id, [Name, Line, I, L]);
        {vendor, I} when V ->
            I;
        false when V ->
            ?RETURN(avp_has_no_vendor, [Name, Line]);
        _ ->
            false
    end.

%% ===========================================================================
%% pass3/2
%%
%% Import AVPs.

pass3(Dict, Opts) ->
    import_enums(import_groups(import_avps(insert_codes(Dict), Opts))).

%% insert_codes/1
%%
%% command_codes -> [{Code, ReqNameTok, AnsNameTok}]

insert_codes(Dict) ->
    dict:store(command_codes,
               dict:fold(fun make_code/3, [], Dict),
               Dict).

make_code({messages, Code}, Names, Acc)
  when is_integer(Code) ->
    [mk_code(Code, Names) | Acc];
make_code(_, _, Acc) ->
    Acc.

mk_code(Code, [[_, _, false] = Ans, [_, _, true] = Req]) ->
    mk_code(Code, [Req, Ans]);

mk_code(Code, [[_, {_,_,Req}, true], [_, {_,_,Ans}, false]]) ->
    {Code, Req, Ans};

mk_code(_Code, [[Line, _Name, IsReq]]) ->
    ?RETURN(message_missing, [choose(IsReq, "Request", "Answer"),
                              Line,
                              choose(IsReq, "answer", "request")]).

%% import_avps/2

import_avps(Dict, Opts) ->
    Import = inherit(Dict, Opts),
    report(imported, Import),

    %% examine/1 tests that all referenced AVP's are either defined
    %% or imported.

    dict:store(import_avps,
               lists:map(fun({M, _, As}) -> {M, [A || {_,A} <- As]} end,
                         lists:reverse(Import)),
               foldl(fun explode_imports/2, Dict, Import)).

explode_imports({Mod, Line, Avps}, Dict) ->
    foldl([fun xi/4, Mod, Line], Dict, Avps).

xi({L, {Name, _Code, _Type, _Flags} = A}, Dict, Mod, Line) ->
    store_new({avp_types, Name},
              [0, Mod, Line, L, A],
              store_new({import, Name},
                        [Line],
                        Dict,
                        [Name, Line],
                        duplicate_import),
              [Name, Mod, Line],
              imported_avp_already_defined).

%% import_groups/1
%% import_enums/1
%%
%% For each inherited module, store the content of imported AVP's of
%% type grouped/enumerated in a new key.

import_groups(Dict) ->
    dict:store(import_groups, import(grouped, Dict), Dict).

import_enums(Dict) ->
    dict:store(import_enums, import(enum, Dict), Dict).

import(Key, Dict) ->
    flatmap([fun import_key/2, Key], dict:fetch(import_avps, Dict)).

import_key({Mod, Avps}, Key) ->
    As = lists:flatmap(fun(T) ->
                               N = element(1,T),
                               choose(lists:keymember(N, 1, Avps), [T], [])
                       end,
                       orddict:fetch(Key, dict(Mod))),
    if As == [] ->
            [];
       true ->
            [{Mod, As}]
    end.

%% ------------------------------------------------------------------------
%% inherit/2
%%
%% Return a {Mod, Line, [{Lineno, Avp}]} list, where Mod is a module
%% name, Line points to the corresponding @inherit and each Avp is
%% from Mod:dict(). Lineno is 0 if the import is implicit.

inherit(Dict, Opts) ->
    code:add_pathsa([D || {include, D} <- Opts]),
    foldl(fun inherit_avps/2, [], find(inherits, Dict)).
%% Note that the module order of the returned lists is reversed
%% relative to @inherits.

inherit_avps([Line, {_,_,M} | Names], Acc) ->
    Mod = ?A(M),
    report(inherit_from, Mod),
    case find_avps(Names, avps_from_module(Mod)) of
        {_, [{_, L, N} | _]} ->
            ?RETURN(requested_avp_not_found, [Mod, Line, N, L]);
        {Found, []} ->
            [{Mod, Line, lists:sort(Found)} | Acc]
    end.

%% Import everything not defined locally ...
find_avps([], Avps) ->
    {[{0, A} || A <- Avps], []};

%% ... or specified AVPs.
find_avps(Names, Avps) ->
    foldl(fun acc_avp/2, {[], Names}, Avps).

acc_avp({Name, _Code, _Type, _Flags} = A, {Found, Not} = Acc) ->
    case lists:keyfind(Name, 3, Not) of
        {_, Line, Name} ->
            {[{Line, A} | Found], lists:keydelete(Name, 3, Not)};
        false ->
            Acc
    end.

%% avps_from_module/2

avps_from_module(Mod) ->
    orddict:fetch(avp_types, dict(Mod)).

dict(Mod) ->
    try Mod:dict() of
        [?VERSION | Dict] ->
            Dict;
        _ ->
            ?RETURN(recompile, [Mod])
    catch
        error: _ ->
            ?RETURN(choose(false == code:is_loaded(Mod),
                           not_loaded,
                           no_dict),
                    [Mod])
    end.

%% ===========================================================================
%% examine/1
%%
%% Sanity checks.

examine(Dict) ->
    dict:fold(fun(K, V, _) -> x(K, V, Dict) end, ok, Dict),
    ok.

%% Ensure enum AVP's have type Enumerated.
x({enum, Name}, [Line | _], Dict)
  when is_list(Name) ->
    true = is_enumerated_avp(Name, Dict, Line);

%% Ensure all referenced AVP's are either defined locally or imported.
x({K, {Name, AvpName}}, [Line | _], Dict)
  when (K == grouped orelse K == messages),
       is_list(Name),
       is_list(AvpName),
       AvpName /= "AVP" ->
    true = avp_is_defined(AvpName, Dict, Line);

%% Ditto.
x({K, AvpName}, [Line | _], Dict)
  when K == avp_vendor_id;
       K == custom ->
    true = avp_is_defined(AvpName, Dict, Line);

%% Ensure that all local AVP's of type Grouped are also present in @grouped.
x({avp_types, Name}, [Line | Toks], Dict)
  when 0 < Line, is_list(Name) ->
    [{number, _, _Code}, {word, _, Type}, {word, _, _Flags}] = Toks,
    "Grouped" == Type
        andalso error == dict:find({grouped, Name}, Dict)
        andalso ?RETURN(grouped_avp_not_grouped, [Name, Line]),
    ok;

x(_, _, _) ->
    ok.

%% has_enumerated_type/3

is_enumerated_avp(Name, Dict, Line) ->
    case find({avp_types, Name}, Dict) of
        [_Line, _Code, {_, _, "Enumerated"}, _Flags] ->   %% local
            true;
        [_Line, _Code, {_, L, T}, _] ->
            ?RETURN(enumerated_avp_has_wrong_local_type,
                    [Name, Line, T, L]);
        [0, _, _, _, {_Name, _Code, "Enumerated", _Flags}] ->   %% inherited
            true;
        [0, Mod, LM, LA, {_Name, _Code, Type, _Flags}] ->
            ?RETURN(enumerated_avp_has_wrong_inherited_type,
                    [Name, Line, Type, Mod, choose(0 == LA, LM, LA)]);
        [] ->
            ?RETURN(enumerated_avp_not_defined, [Name, Line])
    end.

avp_is_defined(Name, Dict, Line) ->
    case find({avp_types, Name}, Dict) of
        [_Line, _Code, _Type, _Flags] ->   %% local
            true;
        [0, _, _, _, {Name, _Code, _Type, _Flags}] ->  %% inherited
            true;
        [] ->
            ?RETURN(avp_not_defined, [Name, Line])
    end.

%% ===========================================================================

putr(Key, Value) ->
    put({?MODULE, Key}, Value).

getr(Key) ->
    get({?MODULE, Key}).

eraser(Key) ->
    erase({?MODULE, Key}).

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

foldl(F, Acc, List) ->
    lists:foldl(fun(T,A) -> eval([F,T,A]) end, Acc, List).

flatmap(F, List) ->
    lists:flatmap(fun(T) -> eval([F,T]) end, List).

eval([[F|X] | A]) ->
    eval([F | A ++ X]);
eval([F|A]) ->
    apply(F,A).
