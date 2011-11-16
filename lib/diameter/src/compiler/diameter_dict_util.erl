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
%% This module turns a dictionary file into the orddict that
%% diameter_codegen.erl in turn morphs into .erl and .hrl files for
%% encode and decode of Diameter messages and AVPs.
%%

-module(diameter_dict_util).

-export([parse/2,
         format_error/1]).

-include("diameter_vsn.hrl").

-define(RETURN(T), throw({T, ?MODULE, ?LINE})).
-define(RETURN(T, Args), ?RETURN({T, Args})).

-define(A, list_to_atom).
-define(L, atom_to_list).

%% ===========================================================================
%% parse/2
%% ===========================================================================

-spec parse(File, Opts)
   -> {ok, orddict:orddict()}
    | {error, term()}
 when File :: {path, string()}
            | string()
            | binary(),
      Opts :: list().

parse(File, Opts) ->
    put({?MODULE, verbose}, lists:member(verbose, Opts)),
    try
        {ok, do_parse(File, Opts)}
    catch
        {Reason, ?MODULE, _Line} ->
            {error, Reason}
    after
        erase({?MODULE, verbose})
    end.

%% ===========================================================================
%% format_error/1
%% ===========================================================================

format_error({read, Reason}) ->
    file:format_error(Reason);
format_error({scan, Reason}) ->
    diameter_dict_scanner:format_error(Reason);
format_error({parse, {Line, _Mod, Reason}}) ->
    lists:flatten(["Line ", integer_to_list(Line), ", ", Reason]);

format_error(T) ->
    {Fmt, As} = fmt(T),
    lists:flatten(io_lib:format(Fmt, As)).

fmt({avp_code_already_defined, [Code, false, Name, Line, L]}) ->
    {"AVP ~p (~s) at line ~p already defined at line ~p",
     [Code, Name, Line, L]};

fmt({Reason, As}) ->
    {fmt(Reason), As};

fmt(avp_code_already_defined) ->
    "AVP ~p/~p (~s) at line ~p already defined at line ~p";

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
fmt(invalid_qualifier) ->
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

fmt(beam_not_on_path) ->
    "Module ~s not found";

fmt(recompile) ->
    "Module ~p appears to have been compiler with an incompatible "
    "version of the dictionary compiler and must be recompiled";
fmt(no_dict) ->
    "Module ~p does not appear to be a diameter dictionary".

%% ===========================================================================
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
    {ok, File}.

make_dict(Parse, Opts) ->
    make_orddict(pass4(pass3(pass2(pass1(reset(make_dict(Parse),
                                               Opts))),
                             Opts))).

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
    {qual(Q), avp(D, Avp)}.

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

qual({true, {_,_,N}}) ->
    {'*', N};
qual({{_,_,N}, true}) ->
    {N, '*'};
qual({{_,_,N},{_,_,M}}) ->
    {N, M};
qual(true) ->
    '*'.

%% Optional reports when running verbosely.
report(What, [F | A])
  when is_function(F) ->
    report(What, apply(F, A));
report(What, Data) ->
    report(get({?MODULE, verbose}), What, Data).

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
    dict:append(Key, [0, {word, 0, Mod}], Dict);
opt({Key, Val}, Dict) ->
    dict:store(Key, [0, {word, 0, Val}], Dict);
opt(_, Dict) ->
    Dict.

%% ===========================================================================
%% pass1/1
%%
%% Explode sections into additional dictionary entries plus semantic
%% checks.

pass1(Dict) ->
    true = no_messages_without_id(Dict),

    foldl(fun(K,D) -> foldl([fun p1/3, K], D, find(K,D)) end,
          Dict,
          [avp_types,  %% must precede inherits, grouped, enum
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
%% {avp_vendor_id, AvpName}                 -> [Lineno, Id::integer()]
%% {custom_types|codecs|inherits,  AvpName} -> [Lineno, Mod::string()]

explode({_, Line, AvpName}, Dict, {_, _, X}, K) ->
    true = K /= inherits orelse avp_not_local(AvpName, Line, Dict),

    store_new({key(K), AvpName},
              [Line, X],
              Dict,
              [AvpName, Line, K],
              avp_already_defined).

%% explode2/4

%% {define, {Name, Key}} -> [Lineno, Value::integer(), enum|define]

explode2([{_, Line, Key}, {_, _, Value}], Dict, {_, _, X}, K) ->
    store_new({key(K), {X, Key}},
              [Line, Value, K],
              Dict,
              [X, Key, K, Line],
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
    Dict1 = store_new({K, Name},
                      [Line | Toks],
                      Dict0,
                      [Name, Line],
                      avp_name_already_defined),

    [{number, _, _Code}, {word, _, Type}, {word, _, _Flags}] = Toks,

    true = avp_type_known(Type, Name, Line),

    Dict1;

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

    [{_,_, Code}, Vid] = Header,
    {DefLine, {_, _, Flags}} = grouped_flags(Name, Code, Dict0, Line),
    V = lists:member($V, Flags),

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

    [{_, _, Code}, Bits, ApplId] = Header,

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

xa([D|_], [[{{_, Line, Min}, {_, _, Max}}, D, _] | _], _, _, _)
  when Min > Max ->
    ?RETURN(invalid_qualifier, [Min, Max, Line]);

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
    %% Ensure there are no duplicate.
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

    %% pass4/1 tests that all referenced AVP's are either defined
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

inherit(Dict, Options) ->
    Path = [D || {include, D} <- Options]
        ++ [".", code:lib_dir(diameter, ebin)],
    foldl([fun find_avps/3, Path], [], find(inherits, Dict)).
%% Note that the module order of the returned lists is reversed
%% relative to @inherits.

find_avps([Line, {_,_,M} | Names], Acc, Path) ->
    Mod = ?A(M),
    report(inherit_from, Mod),
    Avps = avps_from_beam(find_beam(Mod, Path), Mod),  %% could be empty
    case find_avps(Names, Avps) of
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
    foldl(fun fa/2, {[], Names}, Avps).

fa({Name, _Code, _Type, _Flags} = A, {Found, Not} = Acc) ->
    case lists:keyfind(Name, 3, Not) of
        {_, Line, Name} ->
            {[{Line, A} | Found], lists:keydelete(Name, 3, Not)};
        false ->
            Acc
    end.

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
            ?RETURN(beam_not_on_path, [Mod])
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
    orddict:fetch(avp_types, dict(Mod)).

load_module(false, Mod, Path) ->
    R = filename:rootname(Path, code:objfile_extension()),
    {module, Mod} = code:load_abs(R),
    ok;
load_module({file, _}, _, _) ->
    ok.

dict(Mod) ->
    try Mod:dict() of
        [?VERSION | Dict] ->
            Dict;
        _ ->
            ?RETURN(recompile, [Mod])
    catch
        error:_ ->
            ?RETURN(no_dict, [Mod])
    end.

%% ===========================================================================
%% pass4/1
%%
%% Sanity checks.

pass4(Dict) ->
    dict:fold(fun(K, V, _) -> p4(K, V, Dict) end, ok, Dict),
    Dict.

%% Ensure enum AVP's have type Enumerated.
p4({enum, Name}, [Line | _], Dict)
  when is_list(Name) ->
    true = is_enumerated_avp(Name, Dict, Line);

%% Ensure all referenced AVP's are either defined locally or imported.
p4({K, {Name, AvpName}}, [Line | _], Dict)
  when (K == grouped orelse K == messages),
       is_list(Name),
       is_list(AvpName),
       AvpName /= "AVP" ->
    true = avp_is_defined(AvpName, Dict, Line);

%% Ditto.
p4({K, AvpName}, [Line | _], Dict)
  when K == avp_vendor_id;
       K == custom_types;
       K == codecs ->
    true = avp_is_defined(AvpName, Dict, Line);

p4(_, _, _) ->
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
