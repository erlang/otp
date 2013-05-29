%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
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

-module(diameter_codegen).

%%
%% This module generates erl/hrl files for encode/decode modules
%% from the orddict parsed from a dictionary file (.dia) by
%% diameter_dict_util. The generated code is simple (one-liners),
%% the generated functions being called by code included iin the
%% generated modules from diameter_gen.hrl. The orddict itself is
%% returned by dict/0 in the generated module and diameter_dict_util
%% calls this function when importing dictionaries as a consequence
%% of @inherits sections. That is, @inherits introduces a dependency
%% on the beam file of another dictionary.
%%

-export([from_dict/4]).

%% Internal exports (for test).
-export([file/1,
         file/2,
         file/3]).

-include("diameter_forms.hrl").
-include("diameter_vsn.hrl").

-define(S, atom_to_list).
-define(A, list_to_atom).

-define(Atom(T), ?ATOM(?A(T))).

%% ===========================================================================

-spec from_dict(File, Spec, Opts, Mode)
   -> ok
 when File :: string(),
      Spec :: orddict:orddict(),
      Opts :: list(),
      Mode :: spec | erl | hrl.

from_dict(File, Spec, Opts, Mode) ->
    Outdir = proplists:get_value(outdir, Opts, "."),
    putr(verbose, lists:member(verbose, Opts)),
    putr(debug,   lists:member(debug, Opts)),
    codegen(File, Spec, Outdir, Mode).

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

%% ===========================================================================
%% ===========================================================================

%% Generate from parsed dictionary in a file.

file(F) ->
    file(F, spec).

file(F, Mode) ->
    file(F, ".", Mode).

file(F, Outdir, Mode) ->
    {ok, [Spec]} = file:consult(F),
    from_dict(F, Spec, Outdir, Mode).

%% ===========================================================================
%% ===========================================================================

get_value(Key, Plist) ->
    proplists:get_value(Key, Plist, []).

write(Path, Str) ->
    w(Path, Str, "~s").

write_term(Path, T) ->
    w(Path, T, "~p.").

w(Path, T, Fmt) ->
    {ok, Fd} = file:open(Path, [write]),
    io:fwrite(Fd, Fmt ++ "~n", [T]),
    file:close(Fd).

codegen(File, Spec, Outdir, Mode) ->
    Mod = mod(File, orddict:find(name, Spec)),
    Path = filename:join(Outdir, Mod),  %% minus extension
    gen(Mode, Spec, ?A(Mod), Path),
    ok.

mod(File, error) ->
    filename:rootname(filename:basename(File));
mod(_, {ok, Mod}) ->
    Mod.

gen(spec, Spec, _Mod, Path) ->
    write_term(Path ++ ".spec", [?VERSION | Spec]);

gen(hrl, Spec, Mod, Path) ->
    gen_hrl(Path ++ ".hrl", Mod, Spec);

gen(erl, Spec, Mod, Path) ->
    Forms = [{?attribute, module, Mod},
             {?attribute, compile, {parse_transform, diameter_exprecs}},
             {?attribute, compile, nowarn_unused_function},
             {?attribute, export, [{name, 0},
                                   {id, 0},
                                   {vendor_id, 0},
                                   {vendor_name, 0},
                                   {decode_avps, 2}, %% in diameter_gen.hrl
                                   {encode_avps, 2}, %%
                                   {msg_name, 2},
                                   {msg_header, 1},
                                   {rec2msg, 1},
                                   {msg2rec, 1},
                                   {name2rec, 1},
                                   {avp_name, 2},
                                   {avp_arity, 2},
                                   {avp_header, 1},
                                   {avp, 3},
                                   {grouped_avp, 3},
                                   {enumerated_avp, 3},
                                   {empty_value, 1},
                                   {dict, 0}]},
             %% diameter.hrl is included for #diameter_avp
             {?attribute, include_lib, "diameter/include/diameter.hrl"},
             {?attribute, include_lib, "diameter/include/diameter_gen.hrl"},
             f_name(Mod),
             f_id(Spec),
             f_vendor_id(Spec),
             f_vendor_name(Spec),
             f_msg_name(Spec),
             f_msg_header(Spec),
             f_rec2msg(Spec),
             f_msg2rec(Spec),
             f_name2rec(Spec),
             f_avp_name(Spec),
             f_avp_arity(Spec),
             f_avp_header(Spec),
             f_avp(Spec),
             f_enumerated_avp(Spec),
             f_empty_value(Spec),
             f_dict(Spec),
             {eof, ?LINE}],

    gen_erl(Path, insert_hrl_forms(Spec, Forms)).

gen_erl(Path, Forms) ->
    getr(debug) andalso write_term(Path ++ ".forms", Forms),
    write(Path ++ ".erl",
          header() ++ erl_prettypr:format(erl_syntax:form_list(Forms))).

insert_hrl_forms(Spec, Forms) ->
    {H,T} = lists:splitwith(fun is_header/1, Forms),
    H ++ make_hrl_forms(Spec) ++ T.

is_header({attribute, _, export, _}) ->
    false;
is_header(_) ->
    true.

make_hrl_forms(Spec) ->
    {_Prefix, MsgRecs, GrpRecs, ImportedGrpRecs}
        = make_record_forms(Spec),

    RecordForms = MsgRecs ++ GrpRecs ++ lists:flatmap(fun({_,Fs}) -> Fs end,
                                                      ImportedGrpRecs),

    RecNames = lists:map(fun({attribute,_,record,{N,_}}) -> N end,
                         RecordForms),

    %% export_records is used by the diameter_exprecs parse transform.
    [{?attribute, export_records, RecNames} | RecordForms].

make_record_forms(Spec) ->
    Prefix = prefix(Spec),

    MsgRecs = a_record(Prefix, fun msg_proj/1, get_value(messages, Spec)),
    GrpRecs = a_record(Prefix, fun grp_proj/1, get_value(grouped, Spec)),

    ImportedGrpRecs = [{M, a_record(Prefix, fun grp_proj/1, Gs)}
                       || {M,Gs} <- get_value(import_groups, Spec)],

    {Prefix, MsgRecs, GrpRecs, ImportedGrpRecs}.

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

f_id(Spec) ->
    {?function, id, 0,
     [c_id(orddict:find(id, Spec))]}.

c_id({ok, Id}) ->
    {?clause, [], [], [?INTEGER(Id)]};

c_id(error) ->
    ?BADARG(0).

%%% ------------------------------------------------------------------------
%%% # vendor_id/0
%%% ------------------------------------------------------------------------

f_vendor_id(Spec) ->
    {?function, vendor_id, 0,
     [{?clause, [], [], [b_vendor_id(orddict:find(vendor, Spec))]}]}.

b_vendor_id({ok, {Id, _}}) ->
    ?INTEGER(Id);
b_vendor_id(error) ->
    ?APPLY(erlang, error, [?TERM(undefined)]).

%%% ------------------------------------------------------------------------
%%% # vendor_name/0
%%% ------------------------------------------------------------------------

f_vendor_name(Spec) ->
    {?function, vendor_name, 0,
     [{?clause, [], [], [b_vendor_name(orddict:find(vendor, Spec))]}]}.

b_vendor_name({ok, {_, Name}}) ->
    ?Atom(Name);
b_vendor_name(error) ->
    ?APPLY(erlang, error, [?TERM(undefined)]).

%%% ------------------------------------------------------------------------
%%% # msg_name/1
%%% ------------------------------------------------------------------------

f_msg_name(Spec) ->
    {?function, msg_name, 2, msg_name(Spec)}.

%% Return the empty name for any unknown command to which
%% DIAMETER_COMMAND_UNSUPPORTED should be replied.

msg_name(Spec) ->
    lists:flatmap(fun c_msg_name/1, proplists:get_value(command_codes,
                                                        Spec,
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

f_msg2rec(Spec) ->
    {?function, msg2rec, 1, msg2rec(Spec)}.

msg2rec(Spec) ->
    Pre = prefix(Spec),
    lists:map(fun(T) -> c_msg2rec(T, Pre) end, get_value(messages, Spec))
        ++ [?BADARG(1)].

c_msg2rec({N,_,_,_,_}, Pre) ->
    c_name2rec(N, Pre).

%%% ------------------------------------------------------------------------
%%% # rec2msg/1
%%% ------------------------------------------------------------------------

f_rec2msg(Spec) ->
    {?function, rec2msg, 1, rec2msg(Spec)}.

rec2msg(Spec) ->
    Pre = prefix(Spec),
    lists:map(fun(T) -> c_rec2msg(T, Pre) end, get_value(messages, Spec))
        ++ [?BADARG(1)].

c_rec2msg({N,_,_,_,_}, Pre) ->
    {?clause, [?Atom(rec_name(N, Pre))], [], [?Atom(N)]}.

%%% ------------------------------------------------------------------------
%%% # name2rec/1
%%% ------------------------------------------------------------------------

f_name2rec(Spec) ->
    {?function, name2rec, 1, name2rec(Spec)}.

name2rec(Spec) ->
    Pre = prefix(Spec),
    Groups = get_value(grouped, Spec)
          ++ lists:flatmap(fun avps/1, get_value(import_groups, Spec)),
    lists:map(fun({N,_,_,_}) -> c_name2rec(N, Pre) end, Groups)
        ++ [{?clause, [?VAR('T')], [], [?CALL(msg2rec, [?VAR('T')])]}].

c_name2rec(Name, Pre) ->
    {?clause, [?Atom(Name)], [], [?Atom(rec_name(Name, Pre))]}.

avps({_Mod, Avps}) ->
    Avps.

%%% ------------------------------------------------------------------------
%%% # avp_name/1
%%% ------------------------------------------------------------------------

f_avp_name(Spec) ->
    {?function, avp_name, 2, avp_name(Spec)}.

%% 3588, 4.1:
%%
%%    AVP Code
%%       The AVP Code, combined with the Vendor-Id field, identifies the
%%       attribute uniquely.  AVP numbers 1 through 255 are reserved for
%%       backward compatibility with RADIUS, without setting the Vendor-Id
%%       field.  AVP numbers 256 and above are used for Diameter, which are
%%       allocated by IANA (see Section 11.1).

avp_name(Spec) ->
    Avps = get_value(avp_types, Spec),
    Imported = get_value(import_avps, Spec),
    Vid = orddict:find(vendor, Spec),
    Vs = vendor_id_map(Spec),

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

vendor_id_map(Spec) ->
    lists:flatmap(fun({V,Ns}) -> [{N,V} || N <- Ns] end,
                  get_value(avp_vendor_id, Spec))
        ++ lists:flatmap(fun({_,_,[],_}) -> [];
                            ({N,_,[V],_}) -> [{N,V}]
                         end,
                         get_value(grouped, Spec)).

%%% ------------------------------------------------------------------------
%%% # avp_arity/2
%%% ------------------------------------------------------------------------

f_avp_arity(Spec) ->
    {?function, avp_arity, 2, avp_arity(Spec)}.

avp_arity(Spec) ->
    Msgs = get_value(messages, Spec),
    Groups = get_value(grouped, Spec)
          ++ lists:flatmap(fun avps/1, get_value(import_groups, Spec)),
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

f_avp(Spec) ->
    {?function, avp, 3, avp(Spec) ++ [?BADARG(3)]}.

avp(Spec) ->
    Native     = get_value(avp_types, Spec),
    CustomMods = get_value(custom_types, Spec),
    TypeMods   = get_value(codecs, Spec),
    Imported   = get_value(import_avps, Spec),
    Enums      = get_value(enum, Spec),

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

c_base_avp({AvpName, T}) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName)],
     [],
     [b_base_avp(AvpName, T)]}.

b_base_avp(AvpName, "Enumerated") ->
    ?CALL(enumerated_avp, [?VAR('T'), ?Atom(AvpName), ?VAR('Data')]);

b_base_avp(AvpName, "Grouped") ->
    ?CALL(grouped_avp, [?VAR('T'), ?Atom(AvpName), ?VAR('Data')]);

b_base_avp(_, Type) ->
    ?APPLY(diameter_types, ?A(Type), [?VAR('T'), ?VAR('Data')]).

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
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName)],
     [],
     [?APPLY(Mod, avp, [?VAR('T'),
                        ?VAR('Data'),
                        ?Atom(AvpName)])]}.

cs_custom_avp({Mod, Key, Avps}, Dict) ->
    lists:map(fun(N) -> c_custom_avp(Mod, Key, N, orddict:fetch(N, Dict)) end,
              Avps).

c_custom_avp(Mod, Key, AvpName, Type) ->
    {F,A} = custom(Key, AvpName, Type),
    {?clause, [?VAR('T'), ?VAR('Data'), ?Atom(AvpName)],
     [],
     [?APPLY(?A(Mod), ?A(F), [?VAR('T'), ?Atom(A), ?VAR('Data')])]}.

custom(custom_types, AvpName, Type) ->
    {AvpName, Type};
custom(codecs, AvpName, Type) ->
    {Type, AvpName}.

%%% ------------------------------------------------------------------------
%%% # enumerated_avp/3
%%% ------------------------------------------------------------------------

f_enumerated_avp(Spec) ->
    {?function, enumerated_avp, 3, enumerated_avp(Spec) ++ [?BADARG(3)]}.

enumerated_avp(Spec) ->
    Enums = get_value(enum, Spec),
    lists:flatmap(fun cs_enumerated_avp/1, Enums)
        ++ lists:flatmap(fun({M,Es}) -> enumerated_avp(M, Es, Enums) end,
                         get_value(import_enums, Spec)).

enumerated_avp(Mod, Es, Enums) ->
    lists:flatmap(fun({N,_}) ->
                          cs_enumerated_avp(lists:keymember(N, 1, Enums),
                                            Mod,
                                            N)
                  end,
                  Es).

cs_enumerated_avp(true, Mod, Name) ->
    [c_imported_avp(Mod, Name)];
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

f_msg_header(Spec) ->
    {?function, msg_header, 1, msg_header(Spec) ++ [?BADARG(1)]}.

msg_header(Spec) ->
    msg_header(get_value(messages, Spec), Spec).

msg_header([], _) ->
    [];
msg_header(Msgs, Spec) ->
    ApplId = orddict:fetch(id, Spec),

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

f_avp_header(Spec) ->
    {?function, avp_header, 1, avp_header(Spec) ++ [?BADARG(1)]}.

avp_header(Spec) ->
    Native = get_value(avp_types, Spec),
    Imported = get_value(import_avps, Spec),
    Vid = orddict:find(vendor, Spec),
    Vs = vendor_id_map(Spec),

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

f_empty_value(Spec) ->
    {?function, empty_value, 1, empty_value(Spec)}.

empty_value(Spec) ->
    Imported = lists:flatmap(fun avps/1, get_value(import_enums, Spec)),
    Groups = get_value(grouped, Spec)
        ++ lists:flatmap(fun avps/1, get_value(import_groups, Spec)),
    Enums = [T || {N,_} = T <- get_value(enum, Spec),
                  not lists:keymember(N, 1, Imported)]
        ++ Imported,
    lists:map(fun c_empty_value/1, Groups ++ Enums)
        ++ [{?clause, [?VAR('Name')], [], [?CALL(empty, [?VAR('Name')])]}].

c_empty_value({Name, _, _, _}) ->
    {?clause, [?Atom(Name)],
     [],
     [?CALL(empty_group, [?Atom(Name)])]};

c_empty_value({Name, _}) ->
    {?clause, [?Atom(Name)],
     [],
     [?TERM(<<0:32>>)]}.

%%% ------------------------------------------------------------------------
%%% # dict/0
%%% ------------------------------------------------------------------------

f_dict(Spec) ->
    {?function, dict, 0,
     [{?clause, [], [], [?TERM([?VERSION | Spec])]}]}.

%%% ------------------------------------------------------------------------
%%% # gen_hrl/3
%%% ------------------------------------------------------------------------

gen_hrl(Path, Mod, Spec) ->
    {ok, Fd} = file:open(Path, [write]),

    {Prefix, MsgRecs, GrpRecs, ImportedGrpRecs}
        = make_record_forms(Spec),

    file:write(Fd, hrl_header(Mod)),

    forms("Message records",     Fd, MsgRecs),
    forms("Grouped AVP records", Fd, GrpRecs),

    lists:foreach(fun({M,Fs}) ->
                          forms("Grouped AVP records from " ++ atom_to_list(M),
                                Fd,
                                Fs)
                  end,
                  ImportedGrpRecs),

    PREFIX = to_upper(Prefix),

    write("ENUM Macros",
          Fd,
          m_enums(PREFIX, false, get_value(enum, Spec))),
    write("DEFINE Macros",
          Fd,
          m_enums(PREFIX, false, get_value(define, Spec))),

    lists:foreach(fun({M,Es}) ->
                          write("ENUM Macros from " ++ atom_to_list(M),
                                Fd,
                                m_enums(PREFIX, true, Es))
                  end,
                  get_value(import_enums, Spec)),

    file:close(Fd).

forms(_, _, []) ->
    ok;
forms(Banner, Fd, Forms) ->
    write(Banner, Fd, prettypr(Forms)).

write(_, _, []) ->
    ok;
write(Banner, Fd, Str) ->
    banner(Fd, Banner),
    io:fwrite(Fd, "~s~n", [Str]).

prettypr(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms)).

banner(Fd, Heading) ->
    file:write(Fd, banner(Heading)).

banner(Heading) ->
    ("\n\n"
     "%%% -------------------------------------------------------\n"
     "%%% " ++ Heading ++ ":\n"
     "%%% -------------------------------------------------------\n\n").

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
     "\n"
     "%%\n"
     "%% Copyright (c) Ericsson AB. All rights reserved.\n"
     "%%\n"
     "%% The information in this document is the property of Ericsson.\n"
     "%%\n"
     "%% Except as specifically authorized in writing by Ericsson, the\n"
     "%% receiver of this document shall keep the information contained\n"
     "%% herein confidential and shall protect the same in whole or in\n"
     "%% part from disclosure and dissemination to third parties.\n"
     "%%\n"
     "%% Disclosure and disseminations to the receivers employees shall\n"
     "%% only be made on a strict need to know basis.\n"
     "%%\n\n").

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

prefix(Spec) ->
    case orddict:find(prefix, Spec) of
        {ok, P} ->
            P ++ "_";
        error ->
            ""
    end.

rec_name(Name, Prefix) ->
    Prefix ++ Name.
