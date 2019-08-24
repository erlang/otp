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

-module(diameter_codegen).

%%
%% This module generates erl/hrl files for encode/decode modules from
%% the orddict parsed from a dictionary file by diameter_dict_util.
%% The generated code is simple (one-liners), and is called from
%% diameter_gen. The orddict itself is returned by dict/0 in the
%% generated module and diameter_dict_util calls this function when
%% importing dictionaries as a consequence of @inherits sections. That
%% is, @inherits introduces a dependency on the beam file of another
%% dictionary.
%%

-export([from_dict/4,
         is_printable_ascii/1]). %% used by ?TERM/1 in diameter_forms.hrl

-include("diameter_forms.hrl").
-include("diameter_vsn.hrl").

-define(S, atom_to_list).
-define(A, list_to_atom).

-define(Atom(T), ?ATOM(?A(T))).

%% ===========================================================================

-spec from_dict(File, ParseD, Opts, Mode)
   -> ok
    | term()
 when File :: string(),
      ParseD :: orddict:orddict(),
      Opts :: list(),
      Mode :: parse | forms | erl | hrl.

from_dict(File, ParseD, Opts, Mode) ->
    Outdir = proplists:get_value(outdir, Opts, "."),
    Return = proplists:get_value(return, Opts, false),
    Mod = mod(File, orddict:find(name, ParseD)),
    putr(verbose, lists:member(verbose, Opts)),
    try
        maybe_write(Return, Mode, Outdir, Mod, gen(Mode, ParseD, ?A(Mod)))
    after
        eraser(verbose)
    end.

mod(File, error) ->
    filename:rootname(filename:basename(File));
mod(_, {ok, Mod}) ->
    Mod.

maybe_write(true, _, _, _, T) ->
    T;

maybe_write(_, Mode, Outdir, Mod, T) ->
    Path = filename:join(Outdir, Mod),  %% minus extension
    do_write(Mode, [Path, $., ext(Mode)], T).

ext(parse) ->
    "D";
ext(forms) ->
    "F";
ext(T) ->
    ?S(T).

do_write(M, Path, T)
  when M == parse;
       M == forms ->
    write_term(Path, T);
do_write(_, Path, T) ->
    write(Path, T).

write(Path, T) ->
    write(Path, "~s", T).

write_term(Path, T) ->
    write(Path, "~p.~n", T).

write(Path, Fmt, T) ->
    {ok, Fd} = file:open(Path, [write]),
    io:fwrite(Fd, Fmt, [T]),
    ok = file:close(Fd).

%% Optional reports when running verbosely.
report(What, Data) ->
    report(getr(verbose), What, Data),
    Data.

report(true, Tag, Data) ->
    io:format(">>~n>> ~p ~p~n", [Tag, Data]);
report(false, _, _) ->
    ok.

putr(Key, Value) ->
    put({?MODULE, Key}, Value).

getr(Key) ->
    get({?MODULE, Key}).

eraser(Key) ->
    erase({?MODULE, Key}).

%% ===========================================================================
%% ===========================================================================

is_printable_ascii(C) ->
    16#20 =< C andalso C =< 16#7F.

get_value(Key, Plist) ->
    proplists:get_value(Key, Plist, []).

gen(parse, ParseD, _Mod) ->
    [?VERSION | ParseD];

gen(forms, ParseD, Mod) ->
    preprocess(Mod, erl_forms(Mod, ParseD));

gen(hrl, ParseD, Mod) ->
    gen_hrl(Mod, ParseD);

gen(erl, ParseD, Mod) ->
    [header(), prettypr(erl_forms(Mod, ParseD)), $\n].

erl_forms(Mod, ParseD) ->
    Forms = [[{?attribute, module, Mod},
              {?attribute, compile, {parse_transform, diameter_exprecs}},
              {?attribute, compile, nowarn_unused_function}],
             make_hrl_forms(ParseD),
             [{?attribute, export, [{name, 0},
                                    {id, 0},
                                    {vendor_id, 0},
                                    {vendor_name, 0},
                                    {decode_avps, 3}, %% in diameter_gen.hrl
                                    {encode_avps, 3}, %%
                                    {grouped_avp, 4}, %%
                                    {msg_name, 2},
                                    {msg_header, 1},
                                    {rec2msg, 1},
                                    {msg2rec, 1},
                                    {name2rec, 1},
                                    {avp_name, 2},
                                    {avp_arity, 1},
                                    {avp_arity, 2},
                                    {avp_header, 1},
                                    {avp, 4},
                                    {enumerated_avp, 3},
                                    {empty_value, 2},
                                    {dict, 0}]},
              %% diameter.hrl is included for #diameter_avp
              {?attribute, include_lib, "diameter/include/diameter.hrl"},
              {?attribute, include_lib, "diameter/include/diameter_gen.hrl"},
              f_name(Mod),
              f_id(ParseD),
              f_vendor_id(ParseD),
              f_vendor_name(ParseD),
              f_msg_name(ParseD),
              f_msg_header(ParseD),
              f_rec2msg(ParseD),
              f_msg2rec(ParseD),
              f_name2rec(ParseD),
              f_avp_name(ParseD),
              f_avp_arity_1(ParseD),
              f_avp_arity_2(ParseD),
              f_avp_header(ParseD),
              f_avp(ParseD),
              f_enumerated_avp(ParseD),
              f_empty_value(ParseD),
              f_dict(ParseD),
              {eof, ?LINE}]],

    lists:append(Forms).

make_hrl_forms(ParseD) ->
    {_Prefix, MsgRecs, GrpRecs, ImportedGrpRecs}
        = make_record_forms(ParseD),

    RecordForms = MsgRecs ++ GrpRecs ++ lists:flatmap(fun({_,Fs}) -> Fs end,
                                                      ImportedGrpRecs),

    RecNames = lists:map(fun({attribute,_,record,{N,_}}) -> N end,
                         RecordForms),

    %% export_records is used by the diameter_exprecs parse transform.
    [{?attribute, export_records, RecNames} | RecordForms].

make_record_forms(ParseD) ->
    Prefix = prefix(ParseD),

    MsgRecs = a_record(Prefix, fun msg_proj/1, get_value(messages, ParseD)),
    GrpRecs = a_record(Prefix, fun grp_proj/1, get_value(grouped, ParseD)),

    ImportedGrpRecs = [{M, a_record(Prefix, fun grp_proj/1, Gs)}
                       || {M,Gs} <- get_value(import_groups, ParseD)],

    {to_upper(Prefix), MsgRecs, GrpRecs, ImportedGrpRecs}.

msg_proj({Name, _, _, _, Avps}) ->
    {Name, Avps}.

grp_proj({Name, _, _, Avps}) ->
    {Name, Avps}.

%% a_record/3

a_record(Prefix, ProjF, L) ->
    lists:map(fun(T) -> a_record(ProjF(T), Prefix) end, L).

a_record({Nm, Avps}, Prefix) ->
    Name = list_to_atom(Prefix ++ Nm),
    Fields = lists:map(fun field/1, Avps),
    {?attribute, record, {Name, Fields}}.

field(Avp) ->
    {Name, Arity} = avp_info(Avp),
    if 1 == Arity ->
            {?record_field, ?Atom(Name)};
       true ->
            {?record_field, ?Atom(Name), ?NIL}
    end.

%%% ------------------------------------------------------------------------
%%% # name/0
%%% ------------------------------------------------------------------------

f_name(Name) ->
    {?function, name, 0,
     [{?clause, [], [], [?ATOM(Name)]}]}.

%%% ------------------------------------------------------------------------
%%% # id/0
%%% ------------------------------------------------------------------------

f_id(ParseD) ->
    {?function, id, 0,
     [c_id(orddict:find(id, ParseD))]}.

c_id({ok, Id}) ->
    {?clause, [], [], [?INTEGER(Id)]};

c_id(error) ->
    ?BADARG(0).

%%% ------------------------------------------------------------------------
%%% # vendor_id/0
%%% ------------------------------------------------------------------------

f_vendor_id(ParseD) ->
    {?function, vendor_id, 0,
     [{?clause, [], [], [b_vendor_id(orddict:find(vendor, ParseD))]}]}.

b_vendor_id({ok, {Id, _}}) ->
    ?INTEGER(Id);
b_vendor_id(error) ->
    ?APPLY(erlang, error, [?TERM(undefined)]).

%%% ------------------------------------------------------------------------
%%% # vendor_name/0
%%% ------------------------------------------------------------------------

f_vendor_name(ParseD) ->
    {?function, vendor_name, 0,
     [{?clause, [], [], [b_vendor_name(orddict:find(vendor, ParseD))]}]}.

b_vendor_name({ok, {_, Name}}) ->
    ?Atom(Name);
b_vendor_name(error) ->
    ?APPLY(erlang, error, [?TERM(undefined)]).

%%% ------------------------------------------------------------------------
%%% # msg_name/1
%%% ------------------------------------------------------------------------

f_msg_name(ParseD) ->
    {?function, msg_name, 2, msg_name(ParseD)}.

%% Return the empty name for any unknown command to which
%% DIAMETER_COMMAND_UNSUPPORTED should be replied.

msg_name(ParseD) ->
    lists:flatmap(fun c_msg_name/1, proplists:get_value(command_codes,
                                                        ParseD,
                                                        []))
        ++ [{?clause, [?VAR('_'), ?VAR('_')], [], [?ATOM('')]}].

c_msg_name({Code, Req, Ans}) ->
    [{?clause, [?INTEGER(Code), ?ATOM(true)],
      [],
      [?Atom(Req)]},
     {?clause, [?INTEGER(Code), ?ATOM(false)],
      [],
      [?Atom(Ans)]}].

%%% ------------------------------------------------------------------------
%%% # msg2rec/1
%%% ------------------------------------------------------------------------

f_msg2rec(ParseD) ->
    {?function, msg2rec, 1, msg2rec(ParseD)}.

msg2rec(ParseD) ->
    Pre = prefix(ParseD),
    lists:map(fun(T) -> c_msg2rec(T, Pre) end, get_value(messages, ParseD))
        ++ [?BADARG(1)].

c_msg2rec({N,_,_,_,_}, Pre) ->
    c_name2rec(N, Pre).

%%% ------------------------------------------------------------------------
%%% # rec2msg/1
%%% ------------------------------------------------------------------------

f_rec2msg(ParseD) ->
    {?function, rec2msg, 1, rec2msg(ParseD)}.

rec2msg(ParseD) ->
    Pre = prefix(ParseD),
    lists:map(fun(T) -> c_rec2msg(T, Pre) end, get_value(messages, ParseD))
        ++ [?BADARG(1)].

c_rec2msg({N,_,_,_,_}, Pre) ->
    {?clause, [?Atom(rec_name(N, Pre))], [], [?Atom(N)]}.

%%% ------------------------------------------------------------------------
%%% # name2rec/1
%%% ------------------------------------------------------------------------

f_name2rec(ParseD) ->
    {?function, name2rec, 1, name2rec(ParseD)}.

name2rec(ParseD) ->
    Pre = prefix(ParseD),
    Groups = get_value(grouped, ParseD)
          ++ lists:flatmap(fun avps/1, get_value(import_groups, ParseD)),
    lists:map(fun({N,_,_,_}) -> c_name2rec(N, Pre) end, Groups)
        ++ [{?clause, [?VAR('T')], [], [?CALL(msg2rec, [?VAR('T')])]}].

c_name2rec(Name, Pre) ->
    {?clause, [?Atom(Name)], [], [?Atom(rec_name(Name, Pre))]}.

avps({_Mod, Avps}) ->
    Avps.

%%% ------------------------------------------------------------------------
%%% # avp_name/1
%%% ------------------------------------------------------------------------

f_avp_name(ParseD) ->
    {?function, avp_name, 2, avp_name(ParseD)}.

%% 3588, 4.1:
%%
%%    AVP Code
%%       The AVP Code, combined with the Vendor-Id field, identifies the
%%       attribute uniquely.  AVP numbers 1 through 255 are reserved for
%%       backward compatibility with RADIUS, without setting the Vendor-Id
%%       field.  AVP numbers 256 and above are used for Diameter, which are
%%       allocated by IANA (see Section 11.1).

avp_name(ParseD) ->
    Avps = get_value(avp_types, ParseD),
    Imported = get_value(import_avps, ParseD),
    Vid = orddict:find(vendor, ParseD),
    Vs = vendor_id_map(ParseD),

    lists:map(fun(T) -> c_avp_name(T, Vs, Vid) end, Avps)
        ++ lists:flatmap(fun(T) -> c_imported_avp_name(T, Vs) end, Imported)
        ++ [{?clause, [?VAR('_'), ?VAR('_')], [], [?ATOM('AVP')]}].

c_avp_name({Name, Code, Type, Flags}, Vs, Vid) ->
    c_avp_name_(?TERM({?A(Name), ?A(Type)}),
                ?INTEGER(Code),
                vid(Name, Flags, Vs, Vid)).

%% Note that an imported AVP's vendor id is determined by
%% avp_vendor_id in the inheriting module and vendor in the inherited
%% module. In particular, avp_vendor_id in the inherited module is
%% ignored so can't just call Mod:avp_header/1 to retrieve the vendor
%% id. A vendor id specified in @grouped is equivalent to one
%% specified as avp_vendor_id.

c_imported_avp_name({Mod, Avps}, Vs) ->
    lists:map(fun(A) -> c_avp_name(A, Vs, {module, Mod}) end, Avps).

c_avp_name_(T, Code, undefined = U) ->
    {?clause, [Code, ?ATOM(U)],
     [],
     [T]};

c_avp_name_(T, Code, Vid) ->
    {?clause, [Code, ?INTEGER(Vid)],
     [],
     [T]}.

vendor_id_map(ParseD) ->
    lists:flatmap(fun({V,Ns}) -> [{N,V} || N <- Ns] end,
                  get_value(avp_vendor_id, ParseD))
        ++ lists:flatmap(fun({_,_,[],_}) -> [];
                            ({N,_,[V],_}) -> [{N,V}]
                         end,
                         get_value(grouped, ParseD)).

%%% ------------------------------------------------------------------------
%%% # avp_arity/1
%%% ------------------------------------------------------------------------

f_avp_arity_1(ParseD) ->
    {?function, avp_arity, 1, avp_arities(ParseD) ++ [?BADARG(1)]}.

avp_arities(ParseD) ->
    Msgs = get_value(messages, ParseD),
    Groups = get_value(grouped, ParseD)
          ++ lists:flatmap(fun avps/1, get_value(import_groups, ParseD)),
    lists:map(fun c_avp_arities/1, Msgs ++ Groups).

c_avp_arities({N,_,_,_,As}) ->
    c_avp_arities(N,As);
c_avp_arities({N,_,_,As}) ->
    c_avp_arities(N,As).

c_avp_arities(Name, Avps) ->
    Arities = [{?A(N), A} || T <- Avps, {N,A} <- [avp_info(T)]],
    {?clause, [?Atom(Name)], [], [?TERM(Arities)]}.

%%% ------------------------------------------------------------------------
%%% # avp_arity/2
%%% ------------------------------------------------------------------------

f_avp_arity_2(ParseD) ->
    {?function, avp_arity, 2, avp_arity(ParseD)}.

avp_arity(ParseD) ->
    Msgs = get_value(messages, ParseD),
    Groups = get_value(grouped, ParseD)
          ++ lists:flatmap(fun avps/1, get_value(import_groups, ParseD)),
    c_avp_arity(Msgs ++ Groups)
        ++ [{?clause, [?VAR('_'), ?VAR('_')], [], [?INTEGER(0)]}].

c_avp_arity(L)
  when is_list(L) ->
    lists:flatmap(fun c_avp_arity/1, L);

c_avp_arity({N,_,_,_,As}) ->
    c_avp_arity(N,As);
c_avp_arity({N,_,_,As}) ->
    c_avp_arity(N,As).

c_avp_arity(Name, Avps) ->
    lists:map(fun(A) -> c_arity(Name, A) end, Avps).

c_arity(Name, Avp) ->
    {AvpName, Arity} = avp_info(Avp),
    {?clause, [?Atom(Name), ?Atom(AvpName)], [], [?TERM(Arity)]}.

%%% ------------------------------------------------------------------------
%%% # avp/3
%%% ------------------------------------------------------------------------

f_avp(ParseD) ->
    {?function, avp, 4, avp(ParseD) ++ [?BADARG(4)]}.

avp(ParseD) ->
    Native     = get_value(avp_types, ParseD),
    CustomMods = get_value(custom_types, ParseD),
    TypeMods   = get_value(codecs, ParseD),
    Imported   = get_value(import_avps, ParseD),
    Enums      = get_value(enum, ParseD),

    Custom = lists:map(fun({M,As}) -> {M, custom_types, As} end,
                       CustomMods)
        ++ lists:map(fun({M,As}) -> {M, codecs, As} end,
                     TypeMods),
    avp(types(Native), Imported, Custom, Enums).

types(Avps) ->
    lists:map(fun({N,_,T,_}) -> {N,T} end, Avps).

avp(Native, Imported, Custom, Enums) ->
    report(native, Native),
    report(imported, Imported),
    report(custom, Custom),

    TypeDict = lists:foldl(fun({N,_,T,_}, D) -> orddict:store(N,T,D) end,
                           orddict:from_list(Native),
                           lists:flatmap(fun avps/1, Imported)),

    CustomNames = lists:flatmap(fun({_,_,Ns}) -> Ns end, Custom),

    lists:map(fun c_base_avp/1,
              lists:filter(fun({N,_}) -> not_in(CustomNames, N) end,
                           Native))
        ++ lists:flatmap(fun(I) -> cs_imported_avp(I, Enums, CustomNames) end,
                         Imported)
        ++ lists:flatmap(fun(C) -> cs_custom_avp(C, TypeDict) end, Custom).

not_in(List, X) ->
    not lists:member(X, List).

c_base_avp({AvpName, "Enumerated"}) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName), ?VAR('_')],
     [],
     [?CALL(enumerated_avp, [?VAR('T'), ?Atom(AvpName), ?VAR('Data')])]};

c_base_avp({AvpName, "Grouped"}) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName), ?VAR('Opts')],
     [],
     [?CALL(grouped_avp, [?VAR('T'),
                          ?Atom(AvpName),
                          ?VAR('Data'),
                          ?VAR('Opts')])]};

c_base_avp({AvpName, Type}) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName), ?VAR('Opts')],
     [],
     [?APPLY(diameter_types, ?A(Type), [?VAR('T'),
                                        ?VAR('Data'),
                                        ?VAR('Opts')])]}.

cs_imported_avp({Mod, Avps}, Enums, CustomNames) ->
    lists:map(fun(A) -> imported_avp(Mod, A, Enums) end,
              lists:filter(fun({N,_,_,_}) -> not_in(CustomNames, N) end,
                           Avps)).

imported_avp(_Mod, {AvpName, _, "Grouped" = T, _}, _) ->
    c_base_avp({AvpName, T});

imported_avp(Mod, {AvpName, _, "Enumerated" = T, _}, Enums) ->
    case lists:keymember(AvpName, 1, Enums) of
        true ->
            c_base_avp({AvpName, T});
        false ->
            c_imported_avp(Mod, AvpName)
    end;

imported_avp(Mod, {AvpName, _, _, _}, _) ->
    c_imported_avp(Mod, AvpName).

c_imported_avp(Mod, AvpName) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName), ?VAR('Opts')],
     [],
     [?CALL(avp, [?VAR('T'),
                  ?VAR('Data'),
                  ?Atom(AvpName),
                  ?VAR('Opts'),
                  ?ATOM(Mod)])]}.

cs_custom_avp({Mod, Key, Avps}, Dict) ->
    lists:map(fun(N) -> c_custom_avp(Mod, Key, N, orddict:fetch(N, Dict)) end,
              Avps).

c_custom_avp(Mod, Key, AvpName, Type) ->
    {F,A} = custom(Key, AvpName, Type),
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName), ?VAR('Opts')],
     [],
     [?APPLY(?A(Mod), ?A(F), [?VAR('T'),
                              ?Atom(A),
                              ?VAR('Data'),
                              ?VAR('Opts')])]}.

custom(custom_types, AvpName, Type) ->
    {AvpName, Type};
custom(codecs, AvpName, Type) ->
    {Type, AvpName}.

%%% ------------------------------------------------------------------------
%%% # enumerated_avp/3
%%% ------------------------------------------------------------------------

f_enumerated_avp(ParseD) ->
    {?function, enumerated_avp, 3, enumerated_avp(ParseD) ++ [?BADARG(3)]}.

enumerated_avp(ParseD) ->
    Enums = get_value(enum, ParseD),
    lists:flatmap(fun cs_enumerated_avp/1, Enums)
        ++ lists:flatmap(fun({M,Es}) -> enumerated_avp(M, Es, Enums) end,
                         get_value(import_enums, ParseD)).

enumerated_avp(Mod, Es, Enums) ->
    lists:flatmap(fun({N,_}) ->
                          cs_enumerated_avp(lists:keymember(N, 1, Enums),
                                            Mod,
                                            N)
                  end,
                  Es).

cs_enumerated_avp(true, Mod, Name) ->
    [{?clause, [?VAR('T'), ?Atom(Name), ?VAR('Data')],
     [],
     [?APPLY(Mod, enumerated_avp, [?VAR('T'),
                                   ?Atom(Name),
                                   ?VAR('Data')])]}];
cs_enumerated_avp(false, _, _) ->
    [].

cs_enumerated_avp({AvpName, Values}) ->
    lists:flatmap(fun(V) -> c_enumerated_avp(AvpName, V) end, Values).

c_enumerated_avp(AvpName, {_,I}) ->
    [{?clause, [?ATOM(decode), ?Atom(AvpName), ?TERM(<<I:32>>)],
      [],
      [?TERM(I)]},
     {?clause, [?ATOM(encode), ?Atom(AvpName), ?INTEGER(I)],
      [],
      [?TERM(<<I:32>>)]}].

%%% ------------------------------------------------------------------------
%%% msg_header/1
%%% ------------------------------------------------------------------------

f_msg_header(ParseD) ->
    {?function, msg_header, 1, msg_header(ParseD) ++ [?BADARG(1)]}.

msg_header(ParseD) ->
    msg_header(get_value(messages, ParseD), ParseD).

msg_header([], _) ->
    [];
msg_header(Msgs, ParseD) ->
    ApplId = orddict:fetch(id, ParseD),

    lists:map(fun({M,C,F,_,_}) -> c_msg_header(M, C, F, ApplId) end, Msgs).

%% Note that any application id in the message header spec is ignored.

c_msg_header(Name, Code, Flags, ApplId) ->
    {?clause, [?Atom(Name)],
     [],
     [?TERM({Code, encode_msg_flags(Flags), ApplId})]}.

encode_msg_flags(Flags) ->
    lists:foldl(fun emf/2, 0, Flags).

emf('REQ', N) -> N bor 2#10000000;
emf('PXY', N) -> N bor 2#01000000;
emf('ERR', N) -> N bor 2#00100000.

%%% ------------------------------------------------------------------------
%%% # avp_header/1
%%% ------------------------------------------------------------------------

f_avp_header(ParseD) ->
    {?function, avp_header, 1, avp_header(ParseD) ++ [?BADARG(1)]}.

avp_header(ParseD) ->
    Native = get_value(avp_types, ParseD),
    Imported = get_value(import_avps, ParseD),
    Vid = orddict:find(vendor, ParseD),
    Vs = vendor_id_map(ParseD),

    lists:flatmap(fun(A) -> c_avp_header(A, Vs, Vid) end,
                  Native ++ Imported).

c_avp_header({Name, Code, _Type, Flags}, Vs, Vid) ->
    [{?clause, [?Atom(Name)],
      [],
      [?TERM({Code, encode_avp_flags(Flags), vid(Name, Flags, Vs, Vid)})]}];

c_avp_header({Mod, Avps}, Vs, _Vid) ->
    lists:map(fun(A) -> c_imported_avp_header(A, Mod, Vs) end, Avps).

%% Note that avp_vendor_id in the inherited dictionary is ignored. The
%% value must be changed in the inheriting dictionary. This is
%% consistent with the semantics of avp_name/2.

c_imported_avp_header({Name, _Code, _Type, _Flags}, Mod, Vs) ->
    Apply = ?APPLY(Mod, avp_header, [?Atom(Name)]),
    {?clause, [?Atom(Name)],
     [],
     [case proplists:get_value(Name, Vs) of
          undefined ->
              Apply;
          Vid ->
              ?CALL(setelement, [?INTEGER(3), Apply, ?INTEGER(Vid)])
      end]}.

encode_avp_flags(Fs) ->
    lists:foldl(fun eaf/2, 0, Fs).

eaf($V, F) -> 2#10000000 bor F;
eaf($M, F) -> 2#01000000 bor F;
eaf($P, F) -> 2#00100000 bor F.

vid(Name, Flags, Vs, Vid) ->
    v(lists:member($V, Flags), Name, Vs, Vid).

v(true = T, Name, Vs, {module, Mod}) ->
    v(T, Name, Vs, {ok, {Mod:vendor_id(), Mod:vendor_name()}});

v(true, Name, Vs, Vid) ->
    case proplists:get_value(Name, Vs) of
        undefined ->
            {ok, {Id, _}} = Vid,
            Id;
        Id ->
            Id
    end;
v(false, _, _, _) ->
    undefined.

%%% ------------------------------------------------------------------------
%%% # empty_value/0
%%% ------------------------------------------------------------------------

f_empty_value(ParseD) ->
    {?function, empty_value, 2, empty_value(ParseD)}.

empty_value(ParseD) ->
    Imported = lists:flatmap(fun avps/1, get_value(import_enums, ParseD)),
    Groups = get_value(grouped, ParseD)
        ++ lists:flatmap(fun avps/1, get_value(import_groups, ParseD)),
    Enums = [T || {N,_} = T <- get_value(enum, ParseD),
                  not lists:keymember(N, 1, Imported)]
        ++ Imported,
    lists:map(fun c_empty_value/1, Groups ++ Enums)
        ++ [{?clause, [?VAR('Name'), ?VAR('Opts')],
             [],
             [?CALL(empty, [?VAR('Name'), ?VAR('Opts')])]}].

c_empty_value({Name, _, _, _}) ->
    {?clause, [?Atom(Name), ?VAR('Opts')],
     [],
     [?CALL(empty_group, [?Atom(Name), ?VAR('Opts')])]};

c_empty_value({Name, _}) ->
    {?clause, [?Atom(Name), ?VAR('_')],
     [],
     [?TERM(<<0:32>>)]}.

%%% ------------------------------------------------------------------------
%%% # dict/0
%%% ------------------------------------------------------------------------

f_dict(ParseD) ->
    {?function, dict, 0,
     [{?clause, [], [], [?TERM([?VERSION | ParseD])]}]}.

%%% ------------------------------------------------------------------------
%%% # gen_hrl/2
%%% ------------------------------------------------------------------------

gen_hrl(Mod, ParseD) ->
    {Prefix, MsgRecs, GrpRecs, ImportedGrpRecs}
        = make_record_forms(ParseD),

    [hrl_header(Mod),
     forms("Message records",     MsgRecs),
     forms("Grouped AVP records", GrpRecs),
     lists:map(fun({M,Fs}) ->
                       forms("Grouped AVP records from " ++ atom_to_list(M),
                             Fs)
               end,
               ImportedGrpRecs),
     format("ENUM Macros", m_enums(Prefix, false, get_value(enum, ParseD))),
     format("DEFINE Macros", m_enums(Prefix, false, get_value(define, ParseD))),
     lists:map(fun({M,Es}) ->
                       format("ENUM Macros from " ++ atom_to_list(M),
                              m_enums(Prefix, true, Es))
               end,
               get_value(import_enums, ParseD))].

forms(_, [] = No) ->
    No;
forms(Banner, Forms) ->
    format(Banner, prettypr(Forms)).

format(_, [] = No) ->
    No;
format(Banner, Str) ->
    [banner(Banner), Str, $\n].

prettypr(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms)).

banner(Heading) ->
    ["\n\n"
     "%%% -------------------------------------------------------\n"
     "%%% ", Heading, ":\n"
     "%%% -------------------------------------------------------\n\n"].

z(S) ->
    string:join(string:tokens(S, "\s\t"), "\s").

m_enums(Prefix, Wrap, Enums) ->
    lists:map(fun(T) -> m_enum(Prefix, Wrap, T) end, Enums).

m_enum(Prefix, B, {Name, Values}) ->
    P = Prefix ++ to_upper(Name) ++ "_",
    lists:map(fun({A,I}) ->
                      N = ["'", P, to_upper(z(A)), "'"],
                      wrap(B,
                           N,
                           ["-define(", N, ", ", integer_to_list(I), ").\n"])
              end,
              Values).

wrap(true, Name, Def) ->
    ["-ifndef(", Name, ").\n", Def, "-endif.\n"];
wrap(false, _, Def) ->
    Def.

to_upper(A) when is_atom(A) ->
    to_upper(atom_to_list(A));
to_upper(S) ->
    lists:map(fun tu/1, S).

tu(C) when C >= $a, C =< $z ->
    C + $A - $a;
tu(C) ->
    C.

header() ->
    ("%% -------------------------------------------------------------------\n"
     "%% This is a generated file.\n"
     "%% -------------------------------------------------------------------\n"
     "\n").

hrl_header(Name) ->
    header() ++ "-hrl_name('" ++ ?S(Name) ++ ".hrl').\n".

%% avp_info/1

avp_info(Entry) ->  %% {Name, Arity}
    case Entry of
        {{A}} -> {A, 1};
        {A}   -> {A, 1};
        [A]   -> {A, {0,1}};
        {Q,T} ->
            {A,_} = avp_info(T),
            {A, arity(T,Q)}
    end.

%% Normalize arity to 1 or {N,X} where N is an integer. A record field
%% for an AVP is list-valued iff the normalized arity is not 1.
arity({{_}}, '*' = Inf) -> {0, Inf};
arity([_],   '*' = Inf) -> {0, Inf};
arity({_},   '*' = Inf) -> {1, Inf};
arity(_,   {_,_} = Q)   -> Q.

prefix(ParseD) ->
    case orddict:find(prefix, ParseD) of
        {ok, P} ->
            P ++ "_";
        error ->
            ""
    end.

rec_name(Name, Prefix) ->
    Prefix ++ Name.

%% ===========================================================================
%% preprocess/2
%%
%% Preprocess forms as generated by 'forms' option. In particular,
%% replace the include_lib attributes in generated forms by the
%% corresponding forms, extracting the latter from an existing
%% dictionary (diameter_gen_relay). The resulting forms can be
%% compiled to beam using compile:forms/2 (which does no preprocessing
%% of it's own; DiY currently appears to be the only way to preprocess
%% a forms list).

preprocess(Mod, Forms) ->
    {_, Beam, _} = code:get_object_code(diameter_gen_relay),
    pp(Forms, remod(Mod, abstract_code(Beam))).

pp(Forms, {ok, Code}) ->
    Files = files(Code, []),
    lists:flatmap(fun(T) -> include(T, Files) end, Forms);

pp(Forms, {error, Reason}) ->
    erlang:error({forms, Reason, Forms}).

%% Replace literal diameter_gen_relay atoms in the extracted forms.
%% ?MODULE for example.

remod(Mod, L)
  when is_list(L) ->
    [remod(Mod, T) || T <- L];

remod(Mod, {atom, _, diameter_gen_relay} = T) ->
    setelement(3, T, Mod);

remod(Mod, T)
  when is_tuple(T) ->
    list_to_tuple(remod(Mod, tuple_to_list(T)));

remod(_, T) ->
    T.

%% Replace include_lib by the corresponding forms.

include({attribute, _, include_lib, Path}, Files) ->
    Inc = filename:basename(Path),
    [{Inc, Forms}] = [T || {F, _} = T <- Files, F == Inc], %% expect one
    lists:flatmap(fun filter/1, Forms);

include(T, _) ->
    [T].

%% Extract abstract code.

abstract_code(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_Mod, [{abstract_code, {_Vsn, Code}}]}} ->
            {ok, Code};
        {ok, {_Mod, [{abstract_code, no_abstract_code = No}]}} ->
            {error, No};
        {error = E, beam_lib, Reason} ->
            {E, Reason}
    end.

%% Extract filename/forms pairs for included forms.

files([{attribute, _, file, {Path, _}} | T], Acc) ->
    {Body, Rest} = lists:splitwith(fun({attribute, _, file, _}) -> false;
                                      (_) -> true
                                   end,
                                   T),
    files(Rest, [{filename:basename(Path), Body} | Acc]);

files([], Acc) ->
    Acc.

%% Only retain record diameter_avp and functions not generated by
%% diameter_exprecs.

filter({attribute, _, record, {diameter_avp, _}} = T) ->
    [T];

filter({function, _, Name, _, _} = T) ->
    case ?S(Name) of
        [$#|_] ->  %% generated by diameter_exprecs
            [];
        _ ->
            [T]
    end;

filter(_) ->
    [].
