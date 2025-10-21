%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
%% Purpose : Generate documentation as per EEP-48
%%
%% Pass to generate EEP-48 format for beam files.
%%
%% Example:
%%
%% 1> compile:file(test).
%%

-module(beam_doc).
-moduledoc false.

-export([main/4, format_error/1]).

-import(lists, [foldl/3, all/2, map/2, filter/2, reverse/1, join/2, filtermap/2,
                uniq/2, member/2, flatten/1]).

-include_lib("kernel/include/eep48.hrl").

-define(DEFAULT_MODULE_DOC_LOC, 1).
-define(DEFAULT_FORMAT, ~"text/markdown").


-record(docs, {%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% PREPROCESSOR FIELDS
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% These fields are used in a first pass to preprocess the AST.
               %% The fields are considered the source of truth.
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               cwd                 :: file:filename(),             % Cwd
               filename            :: file:filename(),
               curr_filename       :: file:filename(),
               opts                :: [opt()],

               module              :: module(),
               anno = none         :: none | erl_anno:anno(),
               deprecated = #{}    :: map(),

               docformat :: binary(),
               moduledoc = {erl_anno:new(?DEFAULT_MODULE_DOC_LOC), none} :: {erl_anno:anno(), none | map() | hidden},
               moduledoc_meta = none :: none | #{ _ := _ },

               behaviours = []     :: list(module()),

               %% If the module has any documentation attributes at all.
               %% If it does not and no documentation related options are
               %% passed, then we don't generate a doc chunk.
               has_docs = false :: boolean(),

               %% tracks exported functions from multiple `-export([...])`
               exported_functions = sets:new() :: sets:set({FunName :: atom(), Arity :: non_neg_integer()}),

               %% tracks exported type from multiple `-export_type([...])`
               exported_types     = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

               %% tracks type_defs to point to their creation annotation. used for throwing warnings
               %% about type definitions that are unreachable
               type_defs          = #{}        :: #{{TypeName :: atom(), Arity :: non_neg_integer()} := erl_anno:anno()},

               %% helper field to track hidden types
               hidden_types = sets:new() :: sets:set({Name :: atom(), Arity :: non_neg_integer()}),

               %% user defined types that need to be shown in the documentation. these are types that are not
               %% exported but that the documentation needs to show because exported functions referred to them.
               user_defined_types = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

               %% used to report warnings of types in exported functions where the types
               %% may have been set to hidden with a documentation attribute.
               types_from_exported_funs = #{} :: #{{TypeName :: atom(), Arity :: non_neg_integer()} := [erl_anno:anno()]},

               %% tracks the reachable type graph, i.e., type dependencies
               type_dependency = digraph:new() :: digraph:graph(),

               %% track any records found so that we can track
               records = #{} :: #{ atom() => term() },

               % keeps track of `-compile(export_all)`
               export_all         = false :: boolean(),

               %% signatures: used to create signatures from it.
               signatures = #{} :: #{{FunName    :: atom(), Arity      :: non_neg_integer()}
                                     => {FunName    :: atom(),
                                         ListOfVars :: [atom()],
                                         Arity      :: non_neg_integer()}},

               %% populates all function / types, callbacks. it is updated on an ongoing basis
               %% since a doc attribute `doc ...` is not known in a first pass to be attached
               %% to a function / type / callback.
               docs = #{} :: #{{Attribute :: function | type | opaque | nominal | callback,
                                FunName :: atom(),
                                Arity :: non_neg_integer()}
                               =>
                                  {Status :: none | {hidden, erl_anno:anno()}  | set,
                                   Documentation :: none | {DocText :: unicode:chardata(), Anno :: erl_anno:anno()},
                                   Meta :: map()}},

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% DOCUMENTATION TRACKING
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% Documentation attributes of the form `-doc ...` are not known
               %% to be attached to the callback / function / type until reading
               %% the next line. The following fields keep track of this state.
               %% As soon as this state is known to be attached to a type / callback/ function,
               %% this state should be saved in the `docs` field, which is a mapping
               %% of {function(), arity()} => {...} e.g. contains hidden fields, lines,
               %% documentation text, etc.
               %%
               %% one cannot rely on the fields below to keep track of documentation,
               %% as Erlang allows pretty unstructured code.
               %%
               %% e.g.,
               %%
               %% -doc false.
               %% -spec foo() -> ok.
               %%
               %% -spec bar() -> ok.
               %%
               %% -doc #{author => "X"}.
               %% -doc foo() -> ok.
               %%
               %% thus, after reading a terminal AST node (spec, type, fun declaration, opaque, nominal, callback),
               %% the intermediate state saved in the fields below needs to be
               %% saved in the `docs` field.

               hidden_status = none :: none | hidden,

               % Function/type/callback local doc. either none of some string was added
               %% Stateful since the documentation is a two-step process.
               %% First the documentation is entered, and the next terminal item (callback, fun, or type)
               %% determines to which element the documentation gets attached to.
               %%
               %% When getting to a terminal item, the documentation and its status gets attached
               %% to a terminal item in the global map `docs`.
               doc    = none  :: none
                               | {DocText :: unicode:chardata(), Anno :: erl_anno:anno()} ,

               %% track if the doc was never added (none), marked hidden (-doc hidden)
               %% or entered (-doc "..."). If entered, doc_status = set, and doc = "...".
               %% this field is needed because one we do the following:
               %%
               %% -doc hidden.
               %% -doc "This is a hidden function".
               %%
               %% Alternatively, one can merge `doc` and `doc_status` as:
               %%
               %% doc = none | {hidden, "" | none} | "".
               %%
               %% The order in which `-doc hidden.` and `-doc "documentation here"` is written
               %% is not defined, so one cannot assume that the following order:
               %%
               %% -doc "This is a hidden function".
               %% -doc hidden.
               %%
               %% Because of this, we use two fields to keep track of documentation.
               doc_status = none :: none  | {hidden, erl_anno:anno()} | set,

               % Function/type/callback local meta.
               %% exported => boolean(), keeps track of types that are private but used in public functions
               %% thus, they must be considered as exported for documentation purposes.
               %% only useful when processing types. thus, it must be remove from functions and callbacks.
               %% Stateful, need to be fixed as docs.
               meta   = #{exported => false} :: map(),

               %% on analyzing the AST, and upon finding a spec of a exported
               %% function, the types from the spec are added to the field
               %% below. if the function to which the spec belongs to is hidden,
               %% we purge types from this field. if the function to which the
               %% specs belong to are not hidden, they are added to
               %% user_defined_types. Essentially, `last_read_user_types` is a
               %% queue that accumulates types until they can be promoted to
               %% `user_defined_types` or purged (removed).
               %%
               %% RATIONALE / DESIGN
               %%
               %% this field keeps track of these types until we reach the
               %% function definition, which means that we already know if the
               %% function sets `-doc false.`. upon having this information, we
               %% can discard the user defined types when the function uses
               %% `-doc false.` (hidden), since that means that the function
               %% should not be displayed in the docs. if the function is not
               %% hidden, we add the user defined types to the field
               %% `user_defined_types` as these can be (non-)exported types. if
               %% the types are exported, the docs will show the type
               %% definition. if the types are not exported, the type definition
               %% will be shown as not exported.
               last_read_user_types = #{},

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% RESULT
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               ast_fns = [] :: list(),
               ast_types = [] :: list(),
               ast_callbacks = [] :: list(),
               warnings = [] :: warnings()
              }).

-type internal_docs() :: #docs{}.
-type opt() :: warn_missing_doc | warn_missing_doc_functions | warn_missing_doc_callbacks | warn_missing_doc_types |
               nowarn_missing_doc | nowarn_missing_doc_functions | nowarn_missing_doc_callbacks | nowarn_missing_doc_types |
               nowarn_hidden_doc | {nowarn_hidden_doc, {atom(), arity()}}.
-type kfa() :: {Kind :: function | type | callback, Name :: atom(), Arity :: arity()}.
-type warnings() :: [{file:filename(),
                      [{erl_anno:location(), beam_doc, warning()}]}].
-type warning() :: {missing_doc, kfa()} | missing_moduledoc | invalid_metadata |
                   {hidden_type_used_in_exported_fun | hidden_callback, {Name :: atom(), arity()}}.


-doc "
Transforms an Erlang abstract syntax form into EEP-48 documentation format.
".
-spec main(file:filename(), file:filename(), [erl_parse:abstract_form()], [opt()]) ->
          {ok, #docs_v1{}, warnings()} | {error, no_docs}.
main(Dirname, Filename, AST, CmdLineOpts) ->
    Opts = extract_opts(AST, CmdLineOpts),
    State0 = new_state(Dirname, Filename, Opts),
    State1 = preprocessing(AST, State0),
    if State1#docs.has_docs orelse Opts =/= [] ->
            Docs = extract_documentation(AST, State1),
            {ModuleDocAnno, ModuleDoc} = Docs#docs.moduledoc,
            Behaviours = lists:sort(Docs#docs.behaviours),
            Metadata0 = maps:merge(Docs#docs.moduledoc_meta, #{
                source_anno => Docs#docs.anno,
                behaviours => Behaviours}),
            Metadata = maybe_add_source_path_meta(Metadata0, Docs, CmdLineOpts),
            DocFormat = maps:get(format, Metadata, ?DEFAULT_FORMAT),
            Result = #docs_v1{ format = DocFormat,
                                    anno = ModuleDocAnno,
                                    metadata = Metadata,
                                    module_doc = ModuleDoc,
                                    docs = process_docs(Docs) },
            {ok, Result, Docs#docs.warnings };
       not State1#docs.has_docs ->
            {error, no_docs}
    end.

maybe_add_source_path_meta(Metadata, Docs, CmdLineOpts) ->
    case lists:member(deterministic, CmdLineOpts) of
        true ->
            Metadata;
        false ->
            Dir = filename:absname(Docs#docs.cwd),
            SourcePath = filename:join(Dir, Docs#docs.filename),
            Metadata#{source_path => SourcePath}
    end.

extract_opts(AST, CmdLineOpts) ->
    CompileOpts = lists:flatten([C || {attribute,_,compile,C} <- AST]),
    NormalizedOpts = normalize_warn_missing_doc(CmdLineOpts ++ CompileOpts),

    %% Filter out all unrelated opts
    [Opt || Opt <- NormalizedOpts,
            lists:member(Opt,[nowarn_hidden_doc]) orelse
                (is_tuple(Opt) andalso tuple_size(Opt) =:= 2 andalso
                 lists:member(element(1, Opt), [nowarn_hidden_doc,
                                                warn_missing_doc]))].

normalize_warn_missing_doc(Opts) ->
    normalize_warn_missing_doc(Opts, []).
normalize_warn_missing_doc([warn_missing_doc | Opts], _Warnings) ->
    normalize_warn_missing_doc(Opts, [function,callback,type]);
normalize_warn_missing_doc([nowarn_missing_doc | Opts], _Warnings) ->
    normalize_warn_missing_doc(Opts, []);
normalize_warn_missing_doc([warn_missing_doc_functions | Opts], Warnings) ->
    normalize_warn_missing_doc(Opts, lists:uniq([function | Warnings]));
normalize_warn_missing_doc([nowarn_missing_doc_functions | Opts], Warnings) ->
    normalize_warn_missing_doc(Opts, lists:uniq(Warnings -- [function]));
normalize_warn_missing_doc([warn_missing_doc_callbacks | Opts], Warnings) ->
    normalize_warn_missing_doc(Opts, lists:uniq([callback | Warnings]));
normalize_warn_missing_doc([nowarn_missing_doc_callbacks | Opts], Warnings) ->
    normalize_warn_missing_doc(Opts, lists:uniq(Warnings -- [callback]));
normalize_warn_missing_doc([warn_missing_doc_types | Opts], Warnings) ->
    normalize_warn_missing_doc(Opts, lists:uniq([type | Warnings]));
normalize_warn_missing_doc([nowarn_missing_doc_types | Opts], Warnings) ->
    normalize_warn_missing_doc(Opts, lists:uniq(Warnings -- [type]));
normalize_warn_missing_doc([Opt | Opts], Warnings) ->
    [Opt | normalize_warn_missing_doc(Opts, Warnings)];
normalize_warn_missing_doc([], []) ->
    [];
normalize_warn_missing_doc([], Warnings) ->
    [{warn_missing_doc,Warnings}].

-spec format_error(warning()) -> io_lib:chars().
format_error({hidden_type_used_in_exported_fun, {Type, Arity}}) ->
    io_lib:format("hidden type '~p/~p' used in exported function",
                  [Type, Arity]);
format_error({hidden_callback, {Name, Arity}}) ->
    io_lib:format("hidden callback '~p/~p' used", [Name, Arity]);
format_error({missing_doc, {Kind, Name, Arity}}) ->
    io_lib:format("missing -doc for ~w ~tw/~w", [Kind, Name, Arity]);
format_error(missing_moduledoc) ->
    io_lib:format("missing -moduledoc", []);
format_error({invalid_metadata, authors}) ->
    "authors should be a list of unicode strings, that is [unicode:chardata/0]";
format_error({invalid_metadata, Key}) ->
    io_lib:format("~p should be a unicode string, that is unicode:chardata/0",[Key]).

process_docs(#docs{ast_callbacks = AstCallbacks, ast_fns = AstFns, ast_types = AstTypes}) ->
    AstTypes ++ AstCallbacks ++ AstFns.


preprocessing(AST, State) ->
   PreprocessingFuns = fun (AST0, State0) ->
                             Funs = [% Order matters
                                     fun has_docs/2,
                                     fun extract_deprecated/2,
                                     fun extract_exported_types0/2,
                                     fun extract_signature_from_spec0/2,
                                     fun track_documentation/2,      %must be before upsert_documentation_from_terminal_item/2
                                     fun upsert_documentation_from_terminal_item/2,
                                     fun extract_moduledoc0/2,
                                     fun extract_exported_funs/2,
                                     fun extract_file/2,
                                     fun extract_record/2,
                                     fun extract_behaviours/2,
                                     fun extract_hidden_types0/2,
                                     fun extract_type_defs0/2,
                                     fun extract_type_dependencies/2],
                             foldl(fun (F, State1) -> F(AST0, State1) end, State0, Funs)
                       end,
   foldl(PreprocessingFuns, State, AST).

has_docs({attribute, _Anno, moduledoc, _}, State) ->
    State#docs{ has_docs = true };
has_docs({attribute, _Anno, doc, _}, State) ->
    State#docs{ has_docs = true };
has_docs(_, State) ->
    State.

extract_deprecated({attribute, Anno, DeprecatedType, Deprecations}, State)
  when is_list(Deprecations),
       DeprecatedType =:= deprecated orelse
       DeprecatedType =:= deprecated_type orelse
       DeprecatedType =:= deprecated_callback ->
    lists:foldl(fun(D, S) ->
                        extract_deprecated({attribute, Anno, DeprecatedType, D}, S)
                end, State, Deprecations);
extract_deprecated({attribute, Anno, deprecated, {F, A}}, State) ->
    extract_deprecated({attribute, Anno, deprecated, {F, A, undefined}}, State);
extract_deprecated({attribute, _, deprecated, {F, A, Reason}}, State) ->
    Deprecations = (State#docs.deprecated)#{ {function, F, A} => Reason },
    State#docs{ deprecated = Deprecations };
extract_deprecated({attribute, Anno, deprecated_type, {F, A}}, State) ->
    extract_deprecated({attribute, Anno, deprecated_type, {F, A, undefined}}, State);
extract_deprecated({attribute, _, deprecated_type, {F, A, Reason}}, State) ->
    Deprecations = (State#docs.deprecated)#{ {type, F, A} => Reason },
    State#docs{ deprecated = Deprecations };
extract_deprecated({attribute, Anno, deprecated_callback, {F, A}}, State) ->
    extract_deprecated({attribute, Anno, deprecated_callback, {F, A, undefined}}, State);
extract_deprecated({attribute, _, deprecated_callback, {F, A, Reason}}, State) ->
    Deprecations = (State#docs.deprecated)#{ {callback, F, A} => Reason },
    State#docs{ deprecated = Deprecations };
extract_deprecated(_, State) ->
   State.

extract_exported_types0({attribute,_ANNO,export_type,ExportedTypes}, State) ->
   update_export_types(State, ExportedTypes);
extract_exported_types0({attribute,Anno,module, Module}, State) ->
    State#docs{ module = Module, anno = Anno };
extract_exported_types0({attribute,_ANNO,compile, export_all}, State) ->
   update_export_all(State, true);
extract_exported_types0(_AST, State) ->
   State.

extract_signature_from_spec0({attribute, Anno, Tag, Form}, State) when Tag =:= spec; Tag =:= callback ->
   maybe
      {Name, Arity, Args} = extract_args_from_spec(Form),
      true ?= is_list(Args),

      Vars = foldl(fun (_, false) -> false;
                       ({var, _, Var}, Vars) -> [Var | Vars];
                       ({ann_type, _, [{var, _, Var} | _]}, Vars) -> [Var | Vars];
                       (_, _) -> false
                   end, [], Args),

      true ?= is_list(Vars),
      Arity ?= length(Vars),
      update_signature0(State, Anno, {Name, reverse(Vars), Arity})
   else
      _ ->
         State
   end;
extract_signature_from_spec0(_, State) ->
   State.

%%
%% extract arguments for the signature from the spec.
%% does not accept multi-clause callbacks / specs due to the ambiguity
%% of which spec clause to choose.
%%
extract_args_from_spec({{Name, Arity}, Types}) ->
   case Types of
      [{type, _, 'fun', [{type, _, product, Args}, _Return]}] ->
         {Name, Arity, Args};
      [{type, _, bounded_fun, [Args, _Constraints]}] ->
         extract_args_from_spec({{Name, Arity}, [Args]});
      _ ->
         {Name, Arity, false}
   end;
extract_args_from_spec({{_Mod, Name, Arity}, Types}) ->
   extract_args_from_spec({{Name, Arity}, Types}).

update_signature0(#docs{signatures = Signatures}=State, _Anno, {FunName, Vars, Arity}=Signature)
  when is_atom(FunName) andalso is_list(Vars) andalso is_number(Arity) ->
   State#docs{signatures = Signatures#{{FunName, Arity} => Signature}}.


%% Documentation tracking is a two-step (stateful phase).
%% First phase (this one) saves documentation attributes to fields
%% until reaching a terminal element where the docs are gathered globally.
track_documentation({attribute, Anno, doc, Meta}, State) when is_map(Meta) ->
    update_doc(update_meta(doc, State, Anno, Meta), none);
track_documentation({attribute, Anno, doc, DocStatus}, State)
  when DocStatus =:= hidden; DocStatus =:= false ->
   update_docstatus(State, {hidden, set_file_anno(Anno, State)});
track_documentation({attribute, Anno, doc, Doc}, State) when is_list(Doc)  ->
   update_doc(State, {Doc, Anno});
track_documentation({attribute, Anno, doc, Doc}, State) when is_binary(Doc) ->
   update_doc(State, {unicode:characters_to_list(Doc), Anno});
track_documentation(_, State) ->
   State.

upsert_documentation_from_terminal_item({function, Anno, F, Arity, _}, State) ->
   upsert_documentation(function, F, Arity, Anno, State);
upsert_documentation_from_terminal_item({attribute, Anno, TypeOrOpaque, {TypeName, _TypeDef, TypeArgs}},State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque; TypeOrOpaque =:= nominal ->
   Arity = length(fun_to_varargs(TypeArgs)),
   upsert_documentation(type, TypeName, Arity, Anno, State);
upsert_documentation_from_terminal_item({attribute, Anno, callback, {{CB, Arity}, _Form}}, State) ->
   upsert_documentation(callback, CB, Arity, Anno, State);
upsert_documentation_from_terminal_item(_, State) ->
   State.

upsert_documentation(Tag, Name, Arity, Anno, State) when Tag =:= function;
                                                         Tag =:= type;
                                                         Tag =:= opaque;
                                                         Tag =:= nominal;
                                                         Tag =:= callback ->
   Docs = State#docs.docs,
   State1 = case maps:get({Tag, Name, Arity}, Docs, none) of
               none ->
                  Status = State#docs.doc_status,
                  Doc = State#docs.doc,
                  Meta = (State#docs.meta)#{source_anno => Anno},
                  State#docs{docs = Docs#{{Tag, Name, Arity} => {Status, Doc, Meta}}};
               {Status, Documentation, Meta} ->
                  Status1 = upsert_state(Status, State#docs.doc_status),
                  Doc = upsert_doc(Documentation, State#docs.doc),
                  Meta1 = upsert_meta(Meta, State#docs.meta),
                  State#docs{docs = Docs#{{Tag, Name, Arity} := {Status1, Doc, Meta1}}}
   end,
   reset_state(State1).

%% Keep status unless there is a change.
upsert_state({hidden, _}=Hidden, _) ->
   Hidden;
upsert_state(Status, none) ->
   Status;
upsert_state(_Status, Tag) ->
   case Tag of
      {hidden, _} ->
         Tag;
      set ->
         Tag
   end.

upsert_doc(Documentation, none) ->
   Documentation;
upsert_doc(_, Documentation) ->
   Documentation.

upsert_meta(Meta0, Meta1) ->
   maps:merge(Meta0, Meta1).


%%
%% Sets module documentation attributes
%%
extract_moduledoc0({attribute, ModuleDocAnno, moduledoc, false}, State) ->
   extract_moduledoc0({attribute, ModuleDocAnno, moduledoc, hidden}, State);
extract_moduledoc0({attribute, ModuleDocAnno, moduledoc, hidden}, State) ->
   State#docs{moduledoc = {ModuleDocAnno, create_module_doc(hidden)}};
extract_moduledoc0({attribute, ModuleDocAnno, moduledoc, ModuleDoc}, State) when is_list(ModuleDoc) ->
   Doc = unicode:characters_to_binary(string:trim(ModuleDoc)),
   State#docs{moduledoc = {set_file_anno(ModuleDocAnno, State), create_module_doc(Doc)}};
extract_moduledoc0({attribute, ModuleDocAnno, moduledoc, Meta}, State) when is_map(Meta) ->
    update_meta(moduledoc, State, ModuleDocAnno, Meta);
extract_moduledoc0(_, State) ->
   State.

extract_exported_funs({attribute,_ANNO,export,ExportedFuns}, State) ->
   update_export_funs(State, ExportedFuns);
extract_exported_funs(_, State) ->
   State.


%% Sets the filename based on the module
extract_file({attribute, _Anno, file, {Filename, _A}}, State) ->
   update_filename(State, Filename);
extract_file(_, State) ->
   State.

extract_record({attribute, Anno, record, {Name, Fields}}, State) ->
    TypeFields = filtermap(
                   fun({typed_record_field, RecordField, Type}) ->
                           {true, {type, Anno, field_type, [element(3, RecordField), Type]}};
                      (_) ->
                           false
                   end, Fields),
    State#docs{ records = (State#docs.records)#{ Name => TypeFields } };
extract_record(_, State) ->
   State.

extract_behaviours({attribute, _Anno, behaviour, Behaviour}, State) ->
    State#docs{ behaviours = [Behaviour | State#docs.behaviours] };
extract_behaviours(_, State) ->
   State.

%%
%% Extracts types with documentation attribute set to `hidden` or `false`.
%%
%% E.g.:
%%
%%    -doc hidden.
%%    -type foo() :: integer().
%%
extract_hidden_types0({attribute, _Anno, doc, DocStatus}, State) when
   DocStatus =:= hidden; DocStatus =:= false ->
   State#docs{hidden_status = hidden};
extract_hidden_types0({attribute, _Anno, doc, _}, State) ->
   State;
extract_hidden_types0({attribute, _Anno, TypeOrOpaque, {Name, _Type, Args}},
                      #docs{hidden_status = hidden,
                            hidden_types = HiddenTypes}=State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque; TypeOrOpaque =:= nominal ->
   State#docs{hidden_status = none,
              hidden_types = sets:add_element({Name, length(Args)}, HiddenTypes)};
extract_hidden_types0(_, State) ->
   State#docs{hidden_status = none}.


%%
%% Adds type definitions / user-defined types to the state
%%
%% Necessary to provide warnings using the mapping
%% #{{TypeName, length(Args)} => Anno}.
%%
extract_type_defs0({attribute, Anno, TypeOrOpaque, {TypeName, _TypeDef, TypeArgs}}, #docs{type_defs = TypeDefs}=State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque; TypeOrOpaque =:= nominal ->
   Args = fun_to_varargs(TypeArgs),
   Type = {TypeName, length(Args)},
   State#docs{type_defs = TypeDefs#{Type => Anno}};
extract_type_defs0(_, State) ->
   State.

%%
%% Creates a reachable type graph.
%%
%% Given a type `-type X(Args) :: Args2.`, `X` is a vertex that
%% connects with vertices from Args and Args, creating a reachable
%% type graph.
%%
extract_type_dependencies({attribute, _Anno, TypeOrOpaque, {TypeName, TypeDef, TypeArgs}},
                          #docs{type_dependency = TypeDependency}=State)
  when TypeOrOpaque =:= type ->
   Types = extract_user_types([TypeArgs, TypeDef], State),
   Type = {TypeName, length(TypeArgs)},
   digraph:add_vertex(TypeDependency, Type),
   _ = [begin
           digraph:add_vertex(TypeDependency, TypeAndArity),
           digraph:add_edge(TypeDependency, Type, TypeAndArity)
        end || TypeAndArity <- maps:keys(Types)],
   State;
extract_type_dependencies(_, State) ->
   State.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Helper functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec create_module_doc(ModuleDoc :: binary() | atom()) -> map().
create_module_doc(ModuleDoc) when is_atom(ModuleDoc) ->
    ModuleDoc;
create_module_doc(ModuleDoc) when not is_atom(ModuleDoc) ->
    create_module_doc(<<"en">>, ModuleDoc).

-spec create_module_doc(Lang :: binary(), ModuleDoc :: binary()) -> map().
create_module_doc(Lang, ModuleDoc) ->
    #{Lang => ModuleDoc}.

-spec new_state(Dirname :: file:filename(), Filename :: file:filename(),
                Opts :: [opt()]) -> internal_docs().
new_state(Dirname, Filename, Opts) ->
    DocsV1 = #docs_v1{},
    reset_state(#docs{docformat = ?DEFAULT_FORMAT,
                      cwd = Dirname, filename = Filename,
                      curr_filename = Filename, opts = Opts,
                      moduledoc_meta = DocsV1#docs_v1.metadata}).


-spec reset_state(State :: internal_docs()) -> internal_docs().
reset_state(State) ->
    State#docs{doc = none,
               doc_status = none,
               meta = #{exported => false}}.

update_docstatus(State, V) ->
    State#docs{doc_status = V}.


update_ast(function, #docs{ast_fns=AST}=State, Fn) ->
    State#docs{ast_fns = [Fn | AST]};
update_ast(Type,#docs{ast_types=AST}=State, Fn) when Type =:= type; Type =:= opaque; Type =:= nominal->
    State#docs{ast_types = [Fn | AST]};
update_ast(callback, #docs{ast_callbacks = AST}=State, Fn) ->
    State#docs{ast_callbacks = [Fn | AST]}.

-spec update_meta(doc | moduledoc, State :: internal_docs(), Anno :: erl_anno:anno(), Meta :: map()) -> internal_docs().
update_meta(Scope, State0, Anno, Meta1) ->
    {Meta2, State1} =
        maps:fold(fun(equiv, {call,_,_Equiv,_Args}=Equiv, {Meta, State}) when Scope =:= doc ->
                          {Meta#{ equiv := unicode:characters_to_binary(erl_pp:expr(Equiv))}, State};
                     (equiv, {Func, Arity}, {Meta, State}) when Scope =:= doc->
                          {Meta#{ equiv := unicode:characters_to_binary(io_lib:format("~p/~p",[Func,Arity]))}, State};
                     (authors, Authors, {Meta, State}) when Scope =:= moduledoc ->
                          try
                              BinaryAuthors = lists:map(fun unicode:characters_to_binary/1, Authors),
                              {Meta#{ authors := BinaryAuthors }, State}
                          catch _:_ ->
                                  Warning = {invalid_metadata, authors},
                                  {Meta, add_warning(Anno, Warning, State)}
                          end;
                     (Key, Value, {Meta, State}) ->
                          Keys = if Scope =:= doc ->
                                         [group, since, deprecated, equiv];
                                    Scope =:= moduledoc ->
                                         [since, deprecated, format]
                                 end,
                          case lists:member(Key, Keys) of
                              true ->
                                  try {Meta#{ Key := unicode:characters_to_binary(Value) }, State}
                                  catch _:_ ->
                                          Warning = {invalid_metadata, Key},
                                          {Meta, add_warning(Anno, Warning, State)}
                                  end;
                              false -> {Meta, State}
                          end
                  end, {Meta1, State0}, Meta1),
    if Scope =:= doc ->
            State1#docs{meta = maps:merge(State0#docs.meta, Meta2)};
       Scope =:= moduledoc ->
            State1#docs{moduledoc_meta = maps:merge(State0#docs.moduledoc_meta, Meta2)}
    end.

-spec update_user_defined_types({type | callback | function, term(), integer()},
                                State :: internal_docs()) -> internal_docs().
update_user_defined_types({_Attr, _F, _A}=Key,
                          #docs{user_defined_types = UserDefinedTypes,
                                last_read_user_types = LastAddedTypes}=State) ->
   Docs = State#docs.docs,
   case maps:get(Key, Docs, none) of
      {{hidden, _Anno}, _, _} ->
         State#docs{last_read_user_types = #{}};
      _ ->
         State#docs{user_defined_types = sets:union(UserDefinedTypes, sets:from_list(maps:keys(LastAddedTypes))),
                    last_read_user_types = #{}}
   end.

-spec update_doc(State :: internal_docs(), Doc) -> internal_docs() when
     Doc :: {unicode:chardata(), erl_anno:anno()} | atom().
update_doc(#docs{doc_status = DocStatus}=State, Doc0) ->
    %% The exported := true only applies to types and should be ignored for functions.
    %% This is because we need to export private types that are used on public
    %% functions, or the documentation will create dead links.
    State1 = update_docstatus(State, set_doc_status(DocStatus)),
    State2 = update_meta(doc, State1, erl_anno:new(0), #{exported => true}),
    case Doc0 of
        none ->
            State2;
        {Doc, Anno} ->
            State2#docs{doc = {string:trim(Doc), set_file_anno(Anno, State2)}}
    end.

set_file_anno(Anno, State) ->
        case {State#docs.curr_filename, erl_anno:file(Anno)} of
            {ModuleName, undefined} when ModuleName =/= "",
                                         ModuleName =/= State#docs.filename ->
                erl_anno:set_file(ModuleName, Anno);
            _ ->
                Anno
        end.

%% Sets the doc status from `none` to `set`.
%% Leave unchanged if the status was already set to something.
%%
set_doc_status(none) ->
    set;
set_doc_status(Other) ->
    Other.

-spec update_filename(State :: internal_docs(), ModuleName :: unicode:chardata()) -> internal_docs().
update_filename(#docs{}=State, ModuleName) ->
    State#docs{curr_filename = ModuleName}.

-spec update_export_funs(State :: internal_docs(), proplists:proplist()) -> internal_docs().
update_export_funs(State, ExportedFuns) ->
    ExportedFuns1 = sets:union(State#docs.exported_functions, sets:from_list(ExportedFuns)),
    State#docs{exported_functions = ExportedFuns1}.

-spec update_export_types(State :: internal_docs(), proplists:proplist()) -> internal_docs().
update_export_types(State, ExportedTypes) ->
    ExportedTypes1 = sets:union(State#docs.exported_types, sets:from_list(ExportedTypes)),
    State#docs{exported_types = ExportedTypes1}.

update_export_all(State, ExportAll) ->
    State#docs{ export_all = ExportAll }.

remove_exported_type_info(Key, #docs{docs = Docs}=State) ->
   {Status, Doc, Meta} = maps:get(Key, Docs),
   Docs1 = maps:update(Key, {Status, Doc, maps:remove(exported, Meta)}, Docs),
   State#docs{docs = Docs1}.

extract_documentation(AST, State) ->
   State1 = foldl(fun extract_documentation0/2, State, AST),
   State2 = purge_types_not_used_from_exported_functions(State1),
   State3 = purge_unreachable_types(State2),
   warnings(AST, State3).

%%
%% purges types that are not used in exported functions.
%% the type dependency field in docs does not keep track of which type
%% is used in a public function, it simply connects all types.
%% types of hidden functions may exist in the reachable type graph
%% and they should be ignored unless reachable from
purge_types_not_used_from_exported_functions(#docs{user_defined_types = UserDefinedTypes}=State) ->
   AstTypes = filter(fun ({{_, F, A}, _Anno, _Signature, _Doc, #{exported := Exported}}) ->
                                  sets:is_element({F, A}, UserDefinedTypes) orelse Exported
                            end, State#docs.ast_types),
   State#docs{ast_types = AstTypes }.

purge_unreachable_types(#docs{types_from_exported_funs = TypesFromExportedFuns,
                              type_dependency = TypeDependency}=State) ->
   SetTypesFromExportedFns = sets:from_list(maps:keys(TypesFromExportedFuns)),
   SetTypes = sets:union(SetTypesFromExportedFns, State#docs.exported_types),
   ReachableTypes = digraph_utils:reachable(sets:to_list(SetTypes), TypeDependency),
   ReachableSet = sets:from_list(ReachableTypes),
   AstTypes = filter(fun ({{_, F, A}, _Anno, _Signature, _Doc, #{exported := Exported}}) ->
                           sets:is_element({F, A}, ReachableSet) orelse Exported
                     end, State#docs.ast_types),
   State#docs{ast_types = AstTypes }.

warnings(_AST, State) ->
   WarnFuns = [fun warn_hidden_types_used_in_public_fns/1,
               fun warn_missing_docs/1,
               fun warn_missing_moduledoc/1,
               fun warn_hidden_callback/1
              ],
   foldl(fun (W, State0) -> W(State0) end, State, WarnFuns).

warn_missing_docs(State = #docs{ moduledoc = {_, hidden} }) ->
   State;
warn_missing_docs(State) ->
   DocNodes = process_docs(State),
   foldl(fun warn_missing_docs/2, State, DocNodes).

warn_hidden_callback(State) ->
    L = maps:to_list(State#docs.docs),
    NoWarn = flatten(proplists:get_all_values(nowarn_hidden_doc, State#docs.opts)),
    case member(true, NoWarn) of
        false ->
            foldl(fun ({{callback, Name, Arity},{{hidden, Anno}, _, _}}, State0) ->
                          case member({Name, Arity}, NoWarn) of
                              false ->
                                  Warning = {hidden_callback, {Name, Arity}},
                                  add_warning(Anno, Warning, State0);
                              true ->
                                  State0
                          end;
                      (_, State0) ->
                          State0
                  end, State, L);
        true ->
            State
    end.

%% hidden types with `-doc hidden.` or `-doc false.`, which are public (inside
%% `export_type([])`), and used in public functions, they do not make sense. It
%% is a type that is not documented (due to hidden property), visible in the
%% docs (because it is in export_type), and reference / used by a public
%% function cannot be used.
%% A type that is hidden, private, and used in an exported function will be documented
%% by the doc generation showing the internal type structure.
warn_hidden_types_used_in_public_fns(#docs{types_from_exported_funs = TypesFromExportedFuns,
                                           type_dependency = TypeDependency,
                                           type_defs = TypeDefs}=State) ->
    NoWarn = flatten(proplists:get_all_values(nowarn_hidden_doc, State#docs.opts)),
    case member(true, NoWarn) of
        false ->
            HiddenTypes = State#docs.hidden_types,
            Types = maps:keys(TypesFromExportedFuns),
            ReachableTypes = digraph_utils:reachable(Types, TypeDependency),
            ReachableSet = sets:from_list(ReachableTypes),
            Warnings = sets:intersection(HiddenTypes, ReachableSet),
            FilteredWarnings = sets:filter(fun(Key) -> not member(Key, NoWarn) end, Warnings),
            WarningsWithAnno = sets:map(fun (Key) ->
                                                Anno = maps:get(Key, TypeDefs),
                                                Warn = {hidden_type_used_in_exported_fun, Key},
                                                create_warning(Anno, Warn, State)
                                        end, FilteredWarnings),
            State#docs{warnings = State#docs.warnings ++ sets:to_list(WarningsWithAnno) };
        true ->
            State
    end.

create_warning(Anno, Warning, State) ->
   Filename = erl_anno_file(Anno, State),
   Location = erl_anno:location(Anno),
   {Filename, [{Location, ?MODULE, Warning}]}.

add_warning(Anno, Warning, State) ->
   W = create_warning(Anno, Warning, State),
   State#docs{ warnings = [W | State#docs.warnings] }.

warn_missing_docs({{Kind, _, _} = KFA, Anno, _, Doc, MD}, State)
  when Doc =:= none, not is_map_key(equiv, MD) ->
    case lists:member(Kind, proplists:get_value(warn_missing_doc, State#docs.opts, [])) of
        true ->
            Warning = {missing_doc, KFA},
            add_warning(Anno, Warning, State);
        false ->
            State
    end;
warn_missing_docs(_, State) ->
    State.

warn_missing_moduledoc(State) ->
   {_, ModuleDoc} = State#docs.moduledoc,
   case proplists:get_value(warn_missing_doc, State#docs.opts, []) of
      %% If any warn_missing_doc flags is enabled, we also warn for missing moduledoc.
      [_|_] when ModuleDoc =:= none ->
         Anno = erl_anno:new(?DEFAULT_MODULE_DOC_LOC),
         Warning = missing_moduledoc,
         add_warning(Anno, Warning, State);
      _false ->
         State
   end.

%%
%% Extracts documentation
%%
%% This algorithm may use temporal state to keep track of documentation.
%% Example: By looking at `-doc ...` one cannot know whether the doc
%% is attached to a function, type, callback.
extract_documentation0({attribute, _Anno, file, {Filename, _A}}, State) ->
    update_filename(State, Filename);
extract_documentation0({attribute, _Anno, spec, _}=AST, State) ->
   extract_documentation_spec(AST, State);
extract_documentation0({function, _Anno, F, A, _Body}=AST, State) ->
    State1 = remove_exported_type_info({function, F, A}, State),
    extract_documentation_from_funs(AST, State1);
extract_documentation0({attribute, _Anno, TypeOrOpaque, _}=AST,State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque; TypeOrOpaque =:= nominal ->
    extract_documentation_from_type(AST, State);
extract_documentation0({attribute, _Anno, callback, {{CB, A}, _Form}}=AST, State) ->
    State1 = remove_exported_type_info({callback, CB, A}, State),
    extract_documentation_from_cb(AST, State1);
extract_documentation0(_, State) ->
    State.


extract_documentation_spec({attribute, Anno, spec, {{Name,Arity}, SpecTypes}}, #docs{exported_functions = ExpFuns}=State) ->
%% this is because public functions may use private types and these private
%% types need to be included in the beam and documentation.
   case sets:is_element({Name, Arity}, ExpFuns) orelse State#docs.export_all of
      true ->
         add_user_types(Anno, SpecTypes, State);
      false ->
         State
   end;
extract_documentation_spec({attribute, Anno, spec, {{_Mod, Name,Arity}, SpecTypes}}, State) ->
   extract_documentation_spec({attribute, Anno, spec, {{Name,Arity}, SpecTypes}}, State).

add_user_types(_Anno, SpecTypes, State) ->
   Types = extract_user_types(SpecTypes, State),
   State1 = set_types_used_in_public_funs(State, Types),
   set_last_read_user_types(State1, Types).

%% pre: only call this function to add types from external functions.
set_types_used_in_public_funs(#docs{types_from_exported_funs = TypesFromExportedFuns}=State, Types) ->
   Combiner = fun (_Key, Value1, Value2) -> Value1 ++ Value2 end,
   Types0 = maps:merge_with(Combiner, TypesFromExportedFuns, Types),
   State#docs{types_from_exported_funs = Types0}.

set_last_read_user_types(#docs{}=State, Types) ->
   State#docs{last_read_user_types = Types}.

extract_user_types(Args, #docs{ records = Records }) ->
    {Types, _Records} = extract_user_types(Args, {maps:new(), Records}),
    Types;
extract_user_types(Types, Acc) when is_list(Types) ->
    foldl(fun extract_user_types/2, Acc, Types);
extract_user_types({ann_type, _, [_Name, Type]}, Acc) ->
    extract_user_types(Type, Acc);
extract_user_types({type, _, 'fun', Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type, _, map, Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type, _,record,[{atom, _, Name} | Args]}, {Acc, Records}) ->
    NewArgs = uniq(fun({type, _, field_type, [{atom, _, FieldName} | _]}) ->
                           FieldName
                   end, Args ++ maps:get(Name, Records, [])),
    extract_user_types(NewArgs, {Acc, maps:remove(Name, Records)});
extract_user_types({remote_type,_,[_ModuleName,_TypeName,Args]}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type, _, tuple, Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type, _,union, Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({user_type, Anno, Name, Args}, {Acc, Records}) ->
    %% append user type and continue iterating through lists in case of other
    %% user-defined types to be added
    Fun = fun (Value) -> [Anno | Value] end,
    Acc1 = maps:update_with({Name, length(Args)}, Fun, [Anno], Acc),
    extract_user_types(Args, {Acc1, Records});
extract_user_types({type, _, bounded_fun, Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type,_,product,Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type,_,constraint,[{atom,_,is_subtype},Args]}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type, _, map_field_assoc, Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type, _, map_field_exact, Args}, Acc) ->
    extract_user_types(Args, Acc);
extract_user_types({type,_,field_type,[_Name, Type]}, Acc) ->
    extract_user_types(Type, Acc);
extract_user_types({type, _,_BuiltIn, Args}, Acc) when is_list(Args)->
    %% Handles built-in types such as 'list', 'nil' 'range'.
    extract_user_types(Args, Acc);
extract_user_types(_Else, Acc) ->
    Acc.

extract_documentation_from_type({attribute, Anno, TypeOrOpaque, {TypeName, _TypeDef, TypeArgs}=Types},
                      #docs{docs = Docs, exported_types=ExpTypes}=State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque; TypeOrOpaque =:= nominal ->
   Args = fun_to_varargs(TypeArgs),
   Key =  {type, TypeName, length(TypeArgs)},

   % we assume it exists because a first pass must have added it
   {Status, Doc, Meta} = maps:get(Key, Docs),

   State0 = add_last_read_user_type(Anno, Types, State),
   Type = {TypeName, length(Args)},

   Docs1 = maps:update(Key, {Status, Doc, Meta#{exported := sets:is_element(Type, ExpTypes)}}, Docs),
   State2 = State0#docs {docs = Docs1},
   State3 = gen_doc_with_signature({type, Anno, TypeName, length(Args), Args}, State2),
   add_type_defs(Type, State3).

add_type_defs(Type, #docs{type_defs = TypeDefs, ast_types = [{_KFA, Anno, _Signature, _Doc, _Meta} | _]}=State) ->
   State#docs{type_defs = TypeDefs#{Type => Anno}}.


add_last_read_user_type(_Anno, {_TypeName, TypeDef, TypeArgs}, State) ->
   Types = extract_user_types([TypeArgs, TypeDef], State),
   set_last_read_user_types(State, Types).

%% NOTE: Terminal elements for the documentation, such as `-type`, `-opaque`,
%% `-nominal`, `-callback`, and functions always need to reset the state when
%% they finish, so that new AST items start with a clean slate.
extract_documentation_from_funs({function, Anno, F, A, [{clause, _, ClauseArgs, _, _}]},
                      #docs{exported_functions = ExpFuns}=State) ->
    case (sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all) of
       true ->
          gen_doc_with_signature({function, Anno, F, A, ClauseArgs}, State);
       false ->
          reset_state(State)
    end;
extract_documentation_from_funs({function, _Anno0, F, A, _Body}=AST,
                                #docs{exported_functions=ExpFuns}=State) ->
   {Doc1, Anno1} = fetch_doc_and_anno(State, AST),
   case sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all of
      true ->
         {Signature, DocsWithoutSignature} = extract_signature(Doc1, State, F, A),
         AttrBody = {function, F, A},
         gen_doc(Anno1, AttrBody, Signature, DocsWithoutSignature, State);
      false ->
         reset_state(State)
   end.

extract_documentation_from_cb({attribute, Anno, callback, {{CB, A}, Form}}, State) ->
   %% adds user types as part of possible types that need to be exported
   State2 = add_user_types(Anno, Form, State),
   Args = case Form of
              [Fun] ->
                  fun_to_varargs(Fun);
              _ -> %% multi-clause
                  Form
          end,
   gen_doc_with_signature({callback, Anno, CB, A, Args}, State2).

%% Generates documentation
-spec gen_doc(Anno, AttrBody, Signature, Docs, State) -> Response when
      Anno      :: erl_anno:anno(),
      AttrBody  :: {function | type | callback, term(), integer()},
      Signature    :: unicode:chardata(),
      Docs      :: none | hidden | unicode:chardata() | #{ <<_:16>> => unicode:chardata() },
      State     :: internal_docs(),
      Response  :: internal_docs().
gen_doc(Anno, AttrBody, Signature, Docs, State) when not is_atom(Docs), not is_map(Docs) ->
    gen_doc(Anno, AttrBody, Signature, #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, State);
gen_doc(Anno, {Attr, _F, _A}=AttrBody, Signature, Docs, #docs{docs=DocsMap}=State) ->
   {_Status, _Doc, Meta} = maps:get(AttrBody, DocsMap),
   Result = {AttrBody, Anno, [unicode:characters_to_binary(Signature)], Docs,
             maybe_add_since(maybe_add_deprecation(AttrBody, Meta, State), State)},
   State1 = update_user_defined_types(AttrBody, State),
   reset_state(update_ast(Attr, State1, Result)).

erl_anno_file(Anno, State) ->
    case erl_anno:file(Anno) of
        undefined ->
            State#docs.filename;
        FN -> FN
    end.

maybe_add_since(#{ since := _} = Meta, _State) ->
    Meta;
maybe_add_since(Meta, #docs{ moduledoc_meta = #{ since := ModuleDocSince } }) ->
    Meta#{ since => ModuleDocSince };
maybe_add_since(Meta, _State) ->
    Meta.

maybe_add_deprecation(_KNA, #{ deprecated := _ } = Meta, _) ->
    Meta;
maybe_add_deprecation({Kind, Name, Arity}, Meta, #docs{ module = Module,
                                                        deprecated = Deprecations }) ->
    maybe
        error ?= maps:find({Kind, Name, Arity}, Deprecations),
        error ?= maps:find({Kind, Name, '_'}, Deprecations),
        error ?= maps:find({Kind, '_', Arity}, Deprecations),
        error ?= maps:find({Kind, '_', '_'}, Deprecations),
        Meta
    else
        {ok, Value} ->
            Text =
                if Kind =:= function ->
                        erl_lint:format_error({deprecated, {Module,Name,Arity},
                                               info_string(Value)});
                   Kind =:= type ->
                        erl_lint:format_error({deprecated_type, {Module,Name,Arity},
                                               info_string(Value)});
                   Kind =:= callback ->
                        erl_lint:format_error({deprecated_callback, {Module,Name,Arity},
                                               info_string(Value)})
                end,
            Meta#{ deprecated => unicode:characters_to_binary(Text) }
    end.

%% Copies from lib/stdlib/scripts/update_deprecations
info_string(undefined) ->
    "see the documentation for details";
info_string(next_version) ->
    "will be removed in the next version. "
        "See the documentation for details";
info_string(next_major_release) ->
    "will be removed in the next major release. "
        "See the documentation for details";
info_string(eventually) ->
    "will be removed in a future release. "
        "See the documentation for details";
info_string(String) when is_list(String) ->
    String.

%% Generates the documentation inferring the signature from the documentation.
gen_doc_with_signature({Attr, _Anno0, F, A, Args}=AST, State) ->
    {Doc1, Anno1} = fetch_doc_and_anno(State, AST),
    {Signature, DocsWithoutSignature} = extract_signature(Doc1, State, F, A, Args),
    AttrBody = {Attr, F, A},
    gen_doc(Anno1, AttrBody, Signature, DocsWithoutSignature, State).

fetch_doc_and_anno(#docs{docs = DocsMap}=State, {Attr, Anno0, F, A, _Args}) ->
    %% a first pass guarantees that DocsMap cannot be empty
    {DocStatus, Doc, _Meta} = maps:get({Attr, F, A}, DocsMap),
    case {DocStatus, Doc} of
        {{hidden, Anno}, _} -> {hidden, Anno};
        {_, none} -> {none, set_file_anno(Anno0, State)};
        {_, {Doc1, Anno}} -> {Doc1, Anno}
    end.

-spec fun_to_varargs(tuple() | term()) -> dynamic().
fun_to_varargs({type, _, bounded_fun, [T|_]}) ->
   fun_to_varargs(T);
fun_to_varargs({type, _, 'fun', [{type,_,product,Args}|_] }) when is_list(Args) ->
   map(fun fun_to_varargs/1, Args);
fun_to_varargs({ann_type, _, [Name|_]}) ->
   Name;
fun_to_varargs({var,_,_} = Name) ->
   Name;
fun_to_varargs(Else) ->
   Else.

extract_signature(Doc, State, F, A) ->
   extract_signature(Doc, State, F, A, [invalid]).
extract_signature(Doc, State, F, A, Args) ->
   %% order of the strategy matters
   StrategyOrder = [fun signature_strategy_doc_attr/5,
                    fun signature_strategy_spec/5,
                    fun signature_strategy_args/5,
                    fun signature_strategy_default/5],

   %% selection alg. tries strategy until one strategy
   %% returns value =/= false
   SignatureSelection = fun (Fun, false) -> Fun(Doc, State, F, A, Args);
                         (_F, Acc) -> Acc
                     end,
   foldl(SignatureSelection, false, StrategyOrder).


signature_strategy_doc_attr(Doc, _State, F, A, _Args) ->
   maybe
      false ?= Doc =:= none orelse Doc =:= hidden,
      [MaybeSignature | Rest] = string:split(Doc, "\n"),
      {ok, Toks, _} ?= erl_scan:string(unicode:characters_to_list([MaybeSignature,"."])),
      {ok, [{call,_,{atom,_,F},SignatureArgs}]} ?= erl_parse:parse_exprs(Toks),
      A ?= length(SignatureArgs),
      {MaybeSignature, Rest}
   else
      _ ->
         false
   end.

signature_strategy_spec(Doc, State, F, A, _Args) ->
   case maps:get({F, A}, State#docs.signatures, none) of
      {F, Vars, A} ->
         VarString = join(", ",[atom_to_list(Var) || Var <- Vars]),
         Signature = unicode:characters_to_list(io_lib:format("~p(~s)", [F, VarString])),
         {Signature, Doc};
      none ->
         false
   end.

signature_strategy_args(Doc, _State, F, _A, Args) ->
   case all(fun is_var_without_underscore/1, Args)  of
      true ->
         {extract_signature_from_args(F, Args), Doc};
      false ->
         false
   end.

signature_strategy_default(Doc, _State, F, A, _Args) ->
   {io_lib:format("~p/~p",[F,A]), Doc}.


is_var_without_underscore({var, _, N}) ->
   N =/= '_';
is_var_without_underscore(_) ->
   false.

extract_signature_from_args(F, Args) ->
   io_lib:format("~p(~ts)",[F, join(", ",[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).
