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

-module(diameter_codegen).

%%
%% This module generates .erl and .hrl files for encode/decode
%% modules from the orddict parsed from a .dia (aka spec) file by
%% dis_spec_util. The generated code is very simple (one-liners), the
%% generated functions being called by code included from dis_gen.hrl
%% in order to encode/decode messages and AVPs. The orddict itself is
%% returned by dict/0 in the generated module and dis_spec_util calls
%% this function when importing spec files. (That is, beam has to be
%% compiled from an imported spec file before it can be imported.)
%%

-export([from_spec/4]).

%% Internal exports (for test).
-export([file/1,
         file/2,
         file/3]).

-include("diameter_forms.hrl").

%% Generated functions that could have no generated clauses will have
%% a trailing ?UNEXPECTED clause that should never execute.
-define(UNEXPECTED(N), {?clause, [?VAR('_') || _ <- lists:seq(1,N)],
                        [],
                        [?APPLY(erlang,
                                error,
                                [?TERM({unexpected, getr(module)})])]}).


from_spec(File, Spec, Opts, Mode) ->
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

%% Generate from parsed spec in a file.

file(F) ->
    file(F, spec).

file(F, Mode) ->
    file(F, ".", Mode).

file(F, Outdir, Mode) ->
    {ok, [Spec]} = file:consult(F),
    from_spec(F, Spec, Outdir, Mode).

%% ===========================================================================
%% ===========================================================================

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

get_value(Key, Plist) ->
    proplists:get_value(Key, Plist, []).

write(Path, [C|_] = Spec)
  when is_integer(C) ->
    w(Path, Spec, "~s");
write(Path, Spec) ->
    w(Path, Spec, "~p.").

w(Path, Spec, Fmt) ->
    {ok, Fd} = file:open(Path, [write]),
    io:fwrite(Fd, Fmt ++ "~n", [Spec]),
    file:close(Fd).

codegen(File, Spec, Outdir, Mode) ->
    Mod = mod(File, orddict:find(name, Spec)),
    Path = filename:join(Outdir, Mod),  %% minus extension
    gen(Mode, Spec, Mod, Path),
    ok.

mod(File, error) ->
    filename:rootname(filename:basename(File));
mod(_, {ok, Mod}) ->
    atom_to_list(Mod).

gen(spec, Spec, _Mod, Path) ->
    write(Path ++ ".spec", Spec);

gen(hrl, Spec, Mod, Path) ->
    gen_hrl(Path ++ ".hrl", Mod, Spec);

gen(erl = Mode, Spec, Mod, Path)
  when is_list(Mod) ->
    gen(Mode, Spec, list_to_atom(Mod), Path);

gen(erl, Spec, Mod, Path) ->
    putr(module, Mod),  %% used by ?UNEXPECTED.

    Forms = [{?attribute, module, Mod},
             {?attribute, compile, [{parse_transform, diameter_exprecs}]},
             {?attribute, compile, [nowarn_unused_function]},
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
    getr(debug) andalso write(Path ++ ".forms", Forms),
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
    Name = list_to_atom(Prefix ++ atom_to_list(Nm)),
    Fields = lists:map(fun field/1, Avps),
    {?attribute, record, {Name, Fields}}.

field(Avp) ->
    {Name, Arity} = avp_info(Avp),
    if 1 == Arity ->
            {?record_field, ?ATOM(Name)};
       true ->
            {?record_field, ?ATOM(Name), ?NIL}
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
    ?UNEXPECTED(0).

%%% ------------------------------------------------------------------------
%%% # vendor_id/0
%%% ------------------------------------------------------------------------

f_vendor_id(Spec) ->
    {Id, _} = orddict:fetch(vendor, Spec),
    {?function, vendor_id, 0,
     [{?clause, [], [], [?INTEGER(Id)]}]}.

%%% ------------------------------------------------------------------------
%%% # vendor_name/0
%%% ------------------------------------------------------------------------

f_vendor_name(Spec) ->
    {_, Name} = orddict:fetch(vendor, Spec),
    {?function, vendor_name, 0,
     [{?clause, [], [], [?ATOM(Name)]}]}.

%%% ------------------------------------------------------------------------
%%% # msg_name/1
%%% ------------------------------------------------------------------------

f_msg_name(Spec) ->
    {?function, msg_name, 2, msg_name(Spec)}.

%% Return the empty name for any unknown command to which
%% DIAMETER_COMMAND_UNSUPPORTED should be replied.

msg_name(Spec) ->
    lists:flatmap(fun c_msg_name/1,
                  proplists:get_value(command_codes, Spec, []))
        ++ [{?clause, [?VAR('_'), ?VAR('_')], [], [?ATOM('')]}].

c_msg_name({Code, Req, Ans}) ->
    [{?clause, [?INTEGER(Code), ?ATOM(true)],
      [],
      [?ATOM(mname(Req))]},
     {?clause, [?INTEGER(Code), ?ATOM(false)],
      [],
      [?ATOM(mname(Ans))]}].

mname({N, _Abbr}) ->
    N;
mname(N) ->
    N.

%%% ------------------------------------------------------------------------
%%% # msg2rec/1
%%% ------------------------------------------------------------------------

f_msg2rec(Spec) ->
    {?function, msg2rec, 1, msg2rec(Spec)}.

msg2rec(Spec) ->
    Pre = prefix(Spec),
    Dict = dict:from_list(lists:flatmap(fun msgs/1,
                                        get_value(command_codes, Spec))),
    lists:flatmap(fun(T) -> msg2rec(T, Dict, Pre) end,
                  get_value(messages, Spec))
        ++ [?UNEXPECTED(1)].

msgs({_Code, Req, Ans}) ->
    [{mname(Req), Req}, {mname(Ans), Ans}].

msg2rec({N,_,_,_,_}, Dict, Pre) ->
    c_msg2rec(fetch_names(N, Dict), Pre).

fetch_names(Name, Dict) ->
    case dict:find(Name, Dict) of
        {ok, N} ->
            N;
        error ->
            Name
    end.

c_msg2rec({N,A}, Pre) ->
    [c_name2rec(N, N, Pre), c_name2rec(A, N, Pre)];
c_msg2rec(N, Pre) ->
    [c_name2rec(N, N, Pre)].

%%% ------------------------------------------------------------------------
%%% # rec2msg/1
%%% ------------------------------------------------------------------------

f_rec2msg(Spec) ->
    {?function, rec2msg, 1, rec2msg(Spec)}.

rec2msg(Spec) ->
    Pre = prefix(Spec),
    lists:map(fun(T) -> c_rec2msg(T, Pre) end, get_value(messages, Spec))
        ++ [?UNEXPECTED(1)].

c_rec2msg({N,_,_,_,_}, Pre) ->
    {?clause, [?ATOM(rec_name(N, Pre))], [], [?ATOM(N)]}.

%%% ------------------------------------------------------------------------
%%% # name2rec/1
%%% ------------------------------------------------------------------------

f_name2rec(Spec) ->
    {?function, name2rec, 1, name2rec(Spec)}.

name2rec(Spec) ->
    Pre = prefix(Spec),
    Groups = get_value(grouped, Spec)
          ++ lists:flatmap(fun avps/1, get_value(import_groups, Spec)),
    lists:map(fun({N,_,_,_}) -> c_name2rec(N, N, Pre) end, Groups)
        ++ [{?clause, [?VAR('T')], [], [?CALL(msg2rec, [?VAR('T')])]}].

c_name2rec(Name, Rname, Pre) ->
    {?clause, [?ATOM(Name)], [], [?ATOM(rec_name(Rname, Pre))]}.

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
    Avps = get_value(avp_types, Spec)
        ++ lists:flatmap(fun avps/1, get_value(import_avps, Spec)),
    {Vid, _} = orddict:fetch(vendor, Spec),
    Vs = lists:flatmap(fun({V,Ns}) -> [{N,V} || N <- Ns] end,
                       get_value(avp_vendor_id, Spec)),

    lists:map(fun(T) -> c_avp_name(T, Vid, Vs) end, Avps)
        ++ [{?clause, [?VAR('_'), ?VAR('_')], [], [?ATOM('AVP')]}].

c_avp_name({Name, Code, Type, Flags, _Encr}, Vid, Vs) ->
    c_avp_name({Name, Type},
               Code,
               lists:member('V', Flags),
               Vid,
               proplists:get_value(Name, Vs)).

c_avp_name(T, Code, false, _, undefined = U) ->
    {?clause, [?INTEGER(Code), ?ATOM(U)],
     [],
     [?TERM(T)]};

c_avp_name(T, Code, true, Vid, V)
  when is_integer(Vid) ->
    {?clause, [?INTEGER(Code), ?INTEGER(choose(V == undefined, Vid, V))],
     [],
     [?TERM(T)]}.

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
    {?clause, [?ATOM(Name), ?ATOM(AvpName)], [], [?TERM(Arity)]}.

%%% ------------------------------------------------------------------------
%%% # avp/3
%%% ------------------------------------------------------------------------

f_avp(Spec) ->
    {?function, avp, 3, avp(Spec) ++ [?UNEXPECTED(3)]}.

avp(Spec) ->
    Native   = get_value(avp_types, Spec),
    Custom   = get_value(custom_types, Spec),
    Imported = get_value(import_avps, Spec),
    Enums    = get_value(enums, Spec),
    avp([{N,T} || {N,_,T,_,_} <- Native], Imported, Custom, Enums).

avp(Native, Imported, Custom, Enums) ->
    Dict = orddict:from_list(Native),

    report(native, Dict),
    report(imported, Imported),
    report(custom, Custom),

    CustomNames = lists:flatmap(fun({_,Ns}) -> Ns end, Custom),

    lists:map(fun c_base_avp/1,
              lists:filter(fun({N,_}) ->
                                   false == lists:member(N, CustomNames)
                           end,
                           Native))
        ++ lists:flatmap(fun(I) -> cs_imported_avp(I, Enums) end, Imported)
        ++ lists:flatmap(fun(C) -> cs_custom_avp(C, Dict) end, Custom).

c_base_avp({AvpName, T}) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?ATOM(AvpName)],
     [],
     [base_avp(AvpName, T)]}.

base_avp(AvpName, 'Enumerated') ->
    ?CALL(enumerated_avp, [?VAR('T'), ?ATOM(AvpName), ?VAR('Data')]);

base_avp(AvpName, 'Grouped') ->
    ?CALL(grouped_avp, [?VAR('T'), ?ATOM(AvpName), ?VAR('Data')]);

base_avp(_, Type) ->
    ?APPLY(diameter_types, Type, [?VAR('T'), ?VAR('Data')]).

cs_imported_avp({Mod, Avps}, Enums) ->
    lists:map(fun(A) -> imported_avp(Mod, A, Enums) end, Avps).

imported_avp(_Mod, {AvpName, _, 'Grouped' = T, _, _}, _) ->
    c_base_avp({AvpName, T});

imported_avp(Mod, {AvpName, _, 'Enumerated' = T, _, _}, Enums) ->
    case lists:keymember(AvpName, 1, Enums) of
        true ->
            c_base_avp({AvpName, T});
        false ->
            c_imported_avp(Mod, AvpName)
    end;

imported_avp(Mod, {AvpName, _, _, _, _}, _) ->
    c_imported_avp(Mod, AvpName).

c_imported_avp(Mod, AvpName) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?ATOM(AvpName)],
     [],
     [?APPLY(Mod, avp, [?VAR('T'),
                        ?VAR('Data'),
                        ?ATOM(AvpName)])]}.

cs_custom_avp({Mod, Avps}, Dict) ->
    lists:map(fun(N) -> c_custom_avp(Mod, N, orddict:fetch(N, Dict)) end,
              Avps).

c_custom_avp(Mod, AvpName, Type) ->
    {?clause, [?VAR('T'), ?VAR('Data'), ?ATOM(AvpName)],
     [],
     [?APPLY(Mod, AvpName, [?VAR('T'), ?ATOM(Type), ?VAR('Data')])]}.

%%% ------------------------------------------------------------------------
%%% # enumerated_avp/3
%%% ------------------------------------------------------------------------

f_enumerated_avp(Spec) ->
    {?function, enumerated_avp, 3, enumerated_avp(Spec) ++ [?UNEXPECTED(3)]}.

enumerated_avp(Spec) ->
    Enums = get_value(enums, Spec),
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

c_enumerated_avp(AvpName, {I,_}) ->
    [{?clause, [?ATOM(decode), ?ATOM(AvpName), ?TERM(<<I:32/integer>>)],
      [],
      [?TERM(I)]},
     {?clause, [?ATOM(encode), ?ATOM(AvpName), ?INTEGER(I)],
      [],
      [?TERM(<<I:32/integer>>)]}].

%%% ------------------------------------------------------------------------
%%% msg_header/1
%%% ------------------------------------------------------------------------

f_msg_header(Spec) ->
    {?function, msg_header, 1, msg_header(Spec) ++ [?UNEXPECTED(1)]}.

msg_header(Spec) ->
    msg_header(get_value(messages, Spec), Spec).

msg_header([], _) ->
    [];
msg_header(Msgs, Spec) ->
    ApplId = orddict:fetch(id, Spec),

    lists:map(fun({M,C,F,_,_}) -> c_msg_header(M, C, F, ApplId) end, Msgs).

%% Note that any application id in the message header spec is ignored.

c_msg_header(Name, Code, Flags, ApplId) ->
    {?clause, [?ATOM(Name)],
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
    {?function, avp_header, 1, avp_header(Spec) ++ [?UNEXPECTED(1)]}.

avp_header(Spec) ->
    Native = get_value(avp_types, Spec),
    Imported = get_value(import_avps, Spec),
    {Vid, _} = orddict:fetch(vendor, Spec),
    Vs = lists:flatmap(fun({V,Ns}) -> [{N,V} || N <- Ns] end,
                       get_value(avp_vendor_id, Spec)),

    lists:flatmap(fun(A) -> c_avp_header({Vid, Vs}, A) end,
                  Native ++ Imported).

c_avp_header({Vid, Vs}, {Name, Code, _Type, Flags, _Encr}) ->
    [{?clause, [?ATOM(Name)],
      [],
      [?TERM({Code, encode_avp_flags(Flags), vid(Name, Flags, Vs, Vid)})]}];

c_avp_header({_, Vs}, {Mod, Avps}) ->
    lists:map(fun(A) -> c_avp_header(Vs, Mod, A) end, Avps).

c_avp_header(Vs, Mod, {Name, _, _, Flags, _}) ->
    Apply = ?APPLY(Mod, avp_header, [?ATOM(Name)]),
    {?clause, [?ATOM(Name)],
     [],
     [case proplists:get_value(Name, Vs) of
          undefined ->
              Apply;
          Vid ->
              true = lists:member('V', Flags),  %% sanity check
              ?CALL(setelement, [?INTEGER(3), Apply, ?INTEGER(Vid)])
      end]}.

encode_avp_flags(Fs) ->
    lists:foldl(fun eaf/2, 0, Fs).

eaf('V', F) -> 2#10000000 bor F;
eaf('M', F) -> 2#01000000 bor F;
eaf('P', F) -> 2#00100000 bor F.

vid(Name, Flags, Vs, Vid) ->
    v(lists:member('V', Flags), Name, Vs, Vid).

v(true, Name, Vs, Vid) ->
    proplists:get_value(Name, Vs, Vid);
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
    Enums = [T || {N,_} = T <- get_value(enums, Spec),
                  not lists:keymember(N, 1, Imported)]
        ++ Imported,
    lists:map(fun c_empty_value/1, Groups ++ Enums)
        ++ [{?clause, [?VAR('Name')], [], [?CALL(empty, [?VAR('Name')])]}].

c_empty_value({Name, _, _, _}) ->
    {?clause, [?ATOM(Name)],
     [],
     [?CALL(empty_group, [?ATOM(Name)])]};

c_empty_value({Name, _}) ->
    {?clause, [?ATOM(Name)],
     [],
     [?TERM(<<0:32/integer>>)]}.

%%% ------------------------------------------------------------------------
%%% # dict/0
%%% ------------------------------------------------------------------------

f_dict(Spec) ->
    {?function, dict, 0,
     [{?clause, [], [], [?TERM(Spec)]}]}.

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
          m_enums(PREFIX, false, get_value(enums, Spec))),
    write("RESULT CODE Macros",
          Fd,
          m_enums(PREFIX, false, get_value(result_codes, Spec))),

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
    lists:map(fun({I,A}) ->
                      N = ["'", P, to_upper(z(atom_to_list(A))), "'"],
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
    header() ++ "-hrl_name('" ++ Name ++ ".hrl').\n".

%% avp_info/1

avp_info(Entry) ->  %% {Name, Arity}
    case Entry of
        {'<',A,'>'} -> {A, 1};
        {A}         -> {A, 1};
        [A]         -> {A, {0,1}};
        {Q,T} ->
            {A,_} = avp_info(T),
            {A, arity(Q)}
    end.

%% Normalize arity to 1 or {N,X} where N is an integer. A record field
%% for an AVP is list-valued iff the normalized arity is not 1.
arity('*' = Inf) -> {0, Inf};
arity({'*', N})  -> {0, N};
arity({1,1})     -> 1;
arity(T)         -> T.

prefix(Spec) ->
    case orddict:find(prefix, Spec) of
        {ok, P} ->
            atom_to_list(P) ++ "_";
        error ->
            ""
    end.

rec_name(Name, Prefix) ->
    list_to_atom(Prefix ++ atom_to_list(Name)).
