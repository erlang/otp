%% ---------------------------------------------------------------------
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc Making it simple to build a module with merl

-module(merl_build).

-export([init_module/1, module_forms/1, add_function/4, add_record/3,
         add_import/3, add_attribute/3, set_file/2]).

-import(merl, [term/1]).

-include("merl.hrl").

-type filename() :: string().

-record(module, { name          :: atom()
                , file          :: filename()
                , exports=[]    :: [{atom(), integer()}]
                , imports=[]    :: [{atom(), [{atom(), integer()}]}]
                , attributes=[] :: [{filename(), atom(), [term()]}]
                , records=[]    :: [{filename(), atom(),
                                     [{atom(), merl:tree()}]}]
                , functions=[]  :: [{filename(), atom(), [merl:tree()]}]
                }).

%% TODO: init module from a list of forms (from various sources)

%% @doc Create a new module representation, using the given module name.
init_module(Name) when is_atom(Name) ->
    %% use the module name as the default file name - better than nothing
    #module{name=Name, file=atom_to_list(Name)}.

%% @doc Get the list of syntax tree forms for a module representation. This can
%% be passed to compile/2.
module_forms(#module{name=Name,
                     exports=Xs,
                     imports=Is,
                     records=Rs,
                     attributes=As,
                     functions=Fs})
  when is_atom(Name), Name =/= undefined ->
    Module = ?Q("-module('@Name@')."),
    Exported = [erl_syntax:arity_qualifier(term(N), term(A))
                || {N,A} <- ordsets:from_list(Xs)],
    Export = ?Q("-export(['@_Exported'/1])."),
    Imports = [?Q("-import('@M@', ['@_NAs'/1]).")
               || {M, Ns} <- Is,
                  NAs <- [[erl_syntax:arity_qualifier(term(N), term(A))
                           || {N,A} <- ordsets:from_list(Ns)]]
              ],
    Attrs = [?Q("-file(\"'@File@\",1). -'@N@'('@T@').")
             || {File, N, T} <- lists:reverse(As)],
    Records = [?Q("-file(\"'@File@\",1). -record('@N@',{'@_RFs'=[]}).")
               || {File, N, Es} <- lists:reverse(Rs),
                  RFs <- [[erl_syntax:record_field(term(F), V)
                           || {F,V} <- Es]]
              ],
    Functions = [?Q("-file(\"'@File@\",1). '@_F'() -> [].")
                 || {File, N, Cs} <- lists:reverse(Fs),
                    F <- [erl_syntax:function(term(N), Cs)]],
    lists:flatten([Module, Export, Imports, Attrs, Records, Functions]).

%% @doc Set the source file name for all subsequently added functions,
%% records, and attributes.
set_file(Filename, #module{}=M) ->
    M#module{file=filename:flatten(Filename)}.

%% @doc Add a function to a module representation.
add_function(Exported, Name, Clauses,
             #module{file=File, exports=Xs, functions=Fs}=M)
  when is_boolean(Exported), is_atom(Name), Clauses =/= [] ->
    Arity = length(erl_syntax:clause_patterns(hd(Clauses))),
    Xs1 = case Exported of
              true -> [{Name,Arity} | Xs];
              false -> Xs
          end,
    M#module{exports=Xs1, functions=[{File, Name, Clauses} | Fs]}.

%% @doc Add a record declaration to a module representation.
add_record(Name, Fields, #module{file=File, records=Rs}=M)
  when is_atom(Name) ->
    M#module{records=[{File, Name, Fields} | Rs]}.

%% @doc Add a "wild" attribute, such as `-compile(Opts)' to a module
%% representation. Note that such attributes can only have a single argument.
add_attribute(Name, Term, #module{file=File, attributes=As}=M)
  when is_atom(Name) ->
    M#module{attributes=[{File, Name, Term} | As]}.

%% @doc Add an import declaration to a module representation.
add_import(From, Names, #module{imports=Is}=M)
  when is_atom(From), is_list(Names) ->
    M#module{imports=[{From, Names} | Is]}.
