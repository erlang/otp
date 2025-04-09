%%% -*- erlang-indent-level: 4 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
%% Do necessary checking of Erlang code.

-module(erl_lint).
-moduledoc """
The Erlang code linter.

This module is used to check Erlang code for illegal syntax and other bugs. It
also warns against coding practices that are not recommended.

The errors detected include:

- Redefined and undefined functions
- Unbound and unsafe variables
- Illegal record use

The warnings detected include:

- Unused functions and imports
- Unused variables
- Variables imported into matches
- Variables exported from `if`/`case`/`receive`
- Variables shadowed in funs and list comprehensions

Some of the warnings are optional, and can be turned on by specifying the
appropriate option, described below.

The functions in this module are invoked automatically by the Erlang compiler.
There is no reason to invoke these functions separately unless you have written
your own Erlang compiler.

## Error Information

`ErrorInfo` is the standard `ErrorInfo` structure that is returned from all I/O
modules. The format is as follows:

```erlang
{ErrorLine, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```

## See Also

`m:epp`, `m:erl_parse`
""".

-export([module/1,module/2,module/3,format_error/1]).
-export([exprs/2,exprs_opt/3,used_vars/2]). % Used from erl_eval.erl.
-export([is_pattern_expr/1,is_guard_test/1,is_guard_test/2,is_guard_test/3]).
-export([is_guard_expr/1]).
-export([bool_option/4,value_option/3,value_option/7]).

-export([check_format_string/1, check_format_string/2]).

-export_type([error_info/0, error_description/0]).
-export_type([fun_used_vars/0]). % Used from erl_eval.erl.

-import(lists, [all/2,any/2,
                foldl/3,foldr/3,
                map/2,mapfoldl/3,member/2,
                reverse/1]).

%% Removed functions

-removed([{modify_line,2,"use erl_parse:map_anno/2 instead"}]).

%% bool_option(OnOpt, OffOpt, Default, Options) -> boolean().
%% value_option(Flag, Default, Options) -> Value.
%% value_option(Flag, Default, OnOpt, OnVal, OffOpt, OffVal, Options) ->
%%              Value.
%%  The option handling functions.

-doc false.
-spec bool_option(atom(), atom(), boolean(), [compile:option()]) -> boolean().

bool_option(On, Off, Default, Opts) ->
    foldl(fun (Opt, _Def) when Opt =:= On -> true;
              (Opt, _Def) when Opt =:= Off -> false;
              (_Opt, Def) -> Def
          end, Default, Opts).

-doc false.
value_option(Flag, Default, Opts) ->
    foldl(fun ({Opt,Val}, _Def) when Opt =:= Flag -> Val;
              (_Opt, Def) -> Def
          end, Default, Opts).

-doc false.
value_option(Flag, Default, On, OnVal, Off, OffVal, Opts) ->
    foldl(fun ({Opt,Val}, _Def) when Opt =:= Flag -> Val;
              (Opt, _Def) when Opt =:= On -> OnVal;
              (Opt, _Def) when Opt =:= Off -> OffVal;
              (_Opt, Def) -> Def
          end, Default, Opts).

%% The maximum number of arguments allowed for a function.

-define(MAX_ARGUMENTS, 255).

%% The error and warning info structures, {Location,Module,Descriptor},
%% are kept in their seperate fields in the lint state record together
%% with the name of the file (when a new file is entered, marked by
%% the 'file' attribute, then the field 'file' of the lint record is
%% set). At the end of the run these lists are packed into a list of
%% {FileName,ErrorDescList} pairs which are returned.

-include_lib("stdlib/include/erl_bits.hrl").

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

-type anno() :: erl_anno:anno().     % a convenient alias
-type fa()   :: {atom(), arity()}.   % function+arity
-type ta()   :: {atom(), arity()}.   % type+arity

-type module_or_mfa() :: module() | mfa().

-type gexpr_context() :: 'guard' | 'bin_seg_size' | 'map_key'.

-record(typeinfo, {attr, anno}).

-type type_id() :: {'export', []}
                 | {'record', atom()}
                 | {'spec', mfa()}
                 | {'type', ta()}.

-record(used_type, {anno :: erl_anno:anno(),
                    at = {export, []} :: type_id()}).

-type used_type() :: #used_type{}.

-type fun_used_vars() :: #{erl_parse:abstract_expr() => {[atom()], fun_used_vars()}}.

%% Usage of records, functions, and imports. The variable table, which
%% is passed on as an argument, holds the usage of variables.
-record(usage, {
          calls = maps:new(),			%Who calls who
          imported = [],                        %Actually imported functions
          used_records = gb_sets:new()          %Used record definitions
              :: gb_sets:set(atom()),
          used_types = maps:new()               %Used type definitions
              :: #{ta() := [used_type()]}
         }).


%% Define the lint state record.
%% 'called' and 'exports' contain {Anno, {Function, Arity}},
%% the other function collections contain {Function, Arity}.
-record(lint, {state=start		:: 'start' | 'attribute' | 'function',
               module='',                       %Module
               behaviour=[],                    %Behaviour
               exports=gb_sets:empty()	:: gb_sets:set(fa()),%Exports
               imports=[] :: orddict:orddict(fa(), module()),%Imports
               remote_self_calls=#{} :: #{ fa() => gb_sets:set() },
               compile=[],                      %Compile flags
               records=maps:new()               %Record definitions
                   :: #{atom() => {anno(),Fields :: term()}},
               locals=gb_sets:empty()     %All defined functions (prescanned)
                   :: gb_sets:set(fa()),
               no_auto={set, gb_sets:empty()} %Functions explicitly not autoimported
                   :: 'all' | {set, gb_sets:set(fa())},
               defined=gb_sets:empty()          %Defined fuctions
                   :: gb_sets:set(fa()),
	       on_load=[] :: [fa()],		%On-load function
	       on_load_anno=erl_anno:new(0)	%Location for on_load
                   :: erl_anno:anno(),
	       clashes=[],			%Exported functions named as BIFs
               not_deprecated=[],               %Not considered deprecated
               not_removed=gb_sets:empty()      %Not considered removed
                   :: gb_sets:set(module_or_mfa()),
               func=[],                         %Current function
               type_id=[],                      %Current type id
               warn_format=0,                   %Warn format calls
	       enabled_warnings=[],		%All enabled warnings (ordset).
               nowarn_bif_clash=[],             %All no warn bif clashes (ordset).
               errors=[]   :: [{file:filename(),error_info()}], %Current errors
               warnings=[] :: [{file:filename(),error_info()}], %Current warnings
               file = ""        :: string(),	%From last file attribute
               recdef_top=false :: boolean(),	%true in record initialisation
						%outside any fun or lc
               xqlc= false :: boolean(),	%true if qlc.hrl included
               called= [] :: [{fa(),anno()}],   %Called functions
               fun_used_vars = undefined        %Funs used vars
                   :: fun_used_vars() | undefined,
               usage = #usage{}		:: #usage{},
               specs = maps:new()               %Type specifications
                   :: #{mfa() => anno()},
               hidden_docs = sets:new()                %Documentation enabled
                   :: sets:set({atom(),arity()}),
               callbacks = maps:new()           %Callback types
                   :: #{mfa() => anno()},
               optional_callbacks = maps:new()  %Optional callbacks
                   :: #{mfa() => anno()},
               types = maps:new()               %Type definitions
                   :: #{ta() => #typeinfo{}},
               exp_types=gb_sets:empty()        %Exported types
                   :: gb_sets:set(ta()),
               feature_keywords =               %Keywords in
                                                %configurable features
                   feature_keywords() :: #{atom() => atom()},
               bvt = none :: 'none' | [any()],  %Variables in binary pattern
               gexpr_context = guard            %Context of guard expression
                   :: gexpr_context(),
               load_nif=false :: boolean(),      %true if calls erlang:load_nif/2
               doc_defined = false :: {true, erl_anno:anno(), boolean()} | false,
               moduledoc_defined = false :: {true, erl_anno:anno(), boolean()} | false
              }).

-type lint_state() :: #lint{}.
-type error_description() :: term().
-type error_info() :: {erl_anno:location()|'none', module(), error_description()}.

%% format_error(Error)
%%  Return a string describing the error.

-doc """
Takes an `ErrorDescriptor` and returns a string that describes the error or
warning. This function is usually called implicitly when processing an
`ErrorInfo` structure (see section [Error Information](`m:erl_lint#module-error-information`)).
""".
-spec format_error(ErrorDescriptor) -> io_lib:chars() when
      ErrorDescriptor :: error_description().

format_error(Error) ->
    case format_error_1(Error) of
        {Format, Args} when is_list(Args) ->
            io_lib:format(Format, Args);
        Bin when is_binary(Bin) ->
            unicode:characters_to_list(Bin)
    end.

format_error_1(undefined_module) ->
    ~"no module definition";
format_error_1(redefine_module) ->
    ~"redefining module";
format_error_1(pmod_unsupported) ->
    ~"parameterized modules are no longer supported";
format_error_1(non_latin1_module_unsupported) ->
    ~"module names with non-latin1 characters are not supported";
format_error_1(empty_module_name) ->
    ~"the module name must not be empty";
format_error_1(blank_module_name) ->
    ~"the module name must contain at least one visible character";
format_error_1(ctrl_chars_in_module_name) ->
    ~"the module name must not contain control characters";

format_error_1(invalid_call) ->
    ~"invalid function call";
format_error_1(invalid_record) ->
    ~"invalid record expression";

format_error_1({future_feature, Ftr, Atom}) ->
    {~"atom '~p' is reserved in the experimental feature '~p'",
     [Atom, Ftr]};
format_error_1({attribute,A}) ->
    {~"attribute ~tw after function definitions", [A]};
format_error_1({missing_qlc_hrl,A}) ->
    {~"qlc:q/~w called, but \"qlc.hrl\" not included", [A]};
format_error_1({redefine_import,{{F,A},M}}) ->
    {~"function ~tw/~w already imported from ~w", [F,A,M]};
format_error_1({bad_inline,{F,A}}) ->
    {~"inlined function ~tw/~w undefined", [F,A]};
format_error_1({bad_inline,{F,A},GuessFA}) ->
    {~"inlined function ~tw/~w undefined, did you mean ~s?", [F,A,format_fa(GuessFA)]};
format_error_1({undefined_nif,{F,A}}) ->
    {~"nif ~tw/~w undefined", [F,A]};
format_error_1({undefined_nif,{F,A},GuessFA}) ->
    {~"nif ~tw/~w undefined, did you mean ~s?", [F,A,format_fa(GuessFA)]};
format_error_1(no_load_nif) ->
    {~"nifs defined, but no call to erlang:load_nif/2", []};
format_error_1({invalid_deprecated,D}) ->
    {~"badly formed deprecated attribute ~tw", [D]};
format_error_1({bad_deprecated,{F,A}}) ->
    {~"deprecated function ~tw/~w undefined or not exported",
     [F,A]};
format_error_1({invalid_removed,D}) ->
    {~"badly formed removed attribute ~tw", [D]};
format_error_1({bad_removed,{F,A}}) when F =:= '_'; A =:= '_' ->
    {~"at least one function matching ~tw/~w is still exported",
     [F,A]};
format_error_1({bad_removed,{F,A}}) ->
    {~"removed function ~tw/~w is still exported", [F,A]};
format_error_1({bad_nowarn_unused_function,{F,A}}) ->
    {~"function ~tw/~w undefined", [F,A]};
format_error_1({bad_nowarn_unused_function,{F,A},GuessFA}) ->
    {~"function ~tw/~w undefined, did you mean ~s?", [F,A,format_fa(GuessFA)]};
format_error_1({bad_nowarn_bif_clash,{F,A}}) ->
    {~"function ~tw/~w undefined", [F,A]};
format_error_1({bad_nowarn_bif_clash,{F,A},GuessFA}) ->
    {~"function ~tw/~w undefined, did you mean ~s?", [F,A,format_fa(GuessFA)]};
format_error_1(disallowed_nowarn_bif_clash) ->
    ~"""
     compile directive nowarn_bif_clash is no longer allowed --
     use explicit module names or -compile({no_auto_import, [F/A]})
     """;
format_error_1({bad_on_load,Term}) ->
    {~"badly formed on_load attribute: ~tw", [Term]};
format_error_1(multiple_on_loads) ->
    ~"more than one on_load attribute";
format_error_1({bad_on_load_arity,{F,A}}) ->
    {~"function ~tw/~w has wrong arity (must be 0)", [F,A]};
format_error_1({Tag, duplicate_doc_attribute, Ann}) ->
    {~"redefining documentation attribute (~p) previously set at line ~p",
     [Tag, Ann]};
format_error_1({undefined_on_load,{F,A}}) ->
    {~"function ~tw/~w undefined", [F,A]};
format_error_1({undefined_on_load,{F,A},GuessF}) ->
    {~"function ~tw/~w undefined, did you mean ~ts/~w?", [F,A,GuessF,A]};
format_error_1(nif_inline) ->
    ~"inlining is enabled - local calls to NIFs may call their Erlang implementation instead";

format_error_1(export_all) ->
    ~"export_all flag enabled - all functions will be exported";
format_error_1({duplicated_export, {F,A}}) ->
    {~"function ~tw/~w already exported", [F,A]};
format_error_1({unused_import,{{F,A},M}}) ->
    {~"import ~w:~tw/~w is unused", [M,F,A]};
format_error_1({undefined_function,{F,A}}) ->
    {~"function ~tw/~w undefined", [F,A]};
format_error_1({undefined_function,{F,A},GuessFA}) ->
    {~"function ~tw/~w undefined, did you mean ~s?", [F,A,format_fa(GuessFA)]};
format_error_1({redefine_function,{F,A}}) ->
    {~"function ~tw/~w already defined", [F,A]};
format_error_1({define_import,{F,A}}) ->
    {~"defining imported function ~tw/~w", [F,A]};
format_error_1({fun_import,{F,A}}) ->
    {~"creating a fun from imported name ~tw/~w is not allowed", [F,A]};
format_error_1({unused_function,{F,A}}) ->
    {~"function ~tw/~w is unused", [F,A]};
format_error_1({unexported_function, MFA}) ->
    {~"function ~ts is not exported", [format_mfa(MFA)]};
format_error_1({call_to_redefined_bif,{F,A}}) ->
    {~"""
      ambiguous call of overridden auto-imported BIF ~w/~w --
      use erlang:~w/~w or "-compile({no_auto_import,[~w/~w]})." to resolve name clash
      """, [F,A,F,A,F,A]};
format_error_1({redefine_bif_import,{F,A}}) ->
    {~"""
      import directive overrides auto-imported BIF ~w/~w --
      use "-compile({no_auto_import,[~w/~w]})." to resolve name clash
      """, [F,A,F,A]};
format_error_1({deprecated, MFA, String, Rel}) ->
    {~"~s is deprecated and will be removed in ~s; ~s",
     [format_mfa(MFA), Rel, String]};
format_error_1({deprecated, MFA, String}) when is_list(String) ->
    {~"~s is deprecated; ~s", [format_mfa(MFA), String]};
format_error_1({deprecated_type, {M1, F1, A1}, String, Rel}) ->
    {~"the type ~p:~p~s is deprecated and will be removed in ~s; ~s",
                  [M1, F1, gen_type_paren(A1), Rel, String]};
format_error_1({deprecated_type, {M1, F1, A1}, String}) when is_list(String) ->
    {~"the type ~p:~p~s is deprecated; ~s",
                  [M1, F1, gen_type_paren(A1), String]};
format_error_1({deprecated_callback, {M1, F1, A1}, String, Rel}) ->
    {~"the callback ~p:~p~s is deprecated and will be removed in ~s; ~s",
                  [M1, F1, gen_type_paren(A1), Rel, String]};
format_error_1({deprecated_callback, {M1, F1, A1}, String}) when is_list(String) ->
    {~"the callback ~p:~p~s is deprecated; ~s",
                  [M1, F1, gen_type_paren(A1), String]};
format_error_1({removed, MFA, ReplacementMFA, Rel}) ->
    {~"call to ~s will fail, since it was removed in ~s; use ~s",
     [format_mfa(MFA), Rel, format_mfa(ReplacementMFA)]};
format_error_1({removed, MFA, String}) when is_list(String) ->
    {~"~s is removed; ~s", [format_mfa(MFA), String]};
format_error_1({removed_type, MNA, String}) ->
    {~"the type ~s is removed; ~s", [format_mna(MNA), String]};
format_error_1({removed_callback, MNA, String}) ->
    {~"the callback ~s is removed; ~s", [format_mna(MNA), String]};
format_error_1({obsolete_guard, {F, A}}) ->
    {~"~p/~p obsolete (use is_~p/~p)", [F, A, F, A]};
format_error_1({obsolete_guard_overridden,Test}) ->
    {~"""
      obsolete ~s/1 (meaning is_~s/1) is illegal when there is a
      local/imported function named is_~p/1
      """, [Test,Test,Test]};
format_error_1({too_many_arguments,Arity}) ->
    {~"too many arguments (~w) -- maximum allowed is ~w", [Arity,?MAX_ARGUMENTS]};
format_error_1(update_literal) ->
    ~"expression updates a literal";
format_error_1(illegal_zip_generator) ->
    ~"only generators are allowed in a zip generator.";
%% --- patterns and guards ---
format_error_1(illegal_map_assoc_in_pattern) -> ~"illegal pattern, did you mean to use `:=`?";
format_error_1(illegal_pattern) -> ~"illegal pattern";
format_error_1(illegal_expr) -> ~"illegal expression";
format_error_1({illegal_guard_local_call, {F,A}}) ->
    {~"call to local/imported function ~tw/~w is illegal in guard",
     [F,A]};
format_error_1(illegal_guard_expr) -> ~"illegal guard expression";
format_error_1(match_float_zero) ->
    ~"""
     matching on the float 0.0 will no longer also match -0.0 in OTP 27.
     If you specifically intend to match 0.0 alone, write +0.0 instead.
     """;
%% --- maps ---
format_error_1(illegal_map_construction) ->
    ~"only association operators '=>' are allowed in map construction";
%% --- records ---
format_error_1({undefined_record,T}) ->
    {~"record ~tw undefined", [T]};
format_error_1({undefined_record,T,GuessT}) ->
    {~"record ~tw undefined, did you mean ~ts?", [T,GuessT]};
format_error_1({redefine_record,T}) ->
    {~"record ~tw already defined", [T]};
format_error_1({redefine_field,T,F}) ->
    {~"field ~tw already defined in record ~tw", [F,T]};
format_error_1(bad_multi_field_init) ->
    {~"'_' initializes no omitted fields", []};
format_error_1({undefined_field,T,F}) ->
    {~"field ~tw undefined in record ~tw", [F,T]};
format_error_1({undefined_field,T,F,GuessF}) ->
    {~"field ~tw undefined in record ~tw, did you mean ~ts?", [F,T,GuessF]};
format_error_1(illegal_record_info) ->
    ~"illegal record info";
format_error_1({field_name_is_variable,T,F}) ->
    {~"field ~tw is not an atom or _ in record ~tw", [F,T]};
format_error_1({wildcard_in_update,T}) ->
    {~"meaningless use of _ in update of record ~tw", [T]};
format_error_1({unused_record,T}) ->
    {~"record ~tw is unused", [T]};
format_error_1({untyped_record,T}) ->
    {~"record ~tw has field(s) without type information", [T]};
%% --- variables ----
format_error_1({unbound_var,V}) ->
    {~"variable ~w is unbound", [V]};
format_error_1({unbound_var,V,GuessV}) ->
    {~"variable ~w is unbound, did you mean '~s'?", [V,GuessV]};
format_error_1({unsafe_var,V,{What,Where}}) ->
    {~"variable ~w unsafe in ~w ~s",
                  [V,What,format_where(Where)]};
format_error_1({exported_var,V,{What,Where}}) ->
    {~"variable ~w exported from ~w ~s",
                  [V,What,format_where(Where)]};
format_error_1({match_underscore_var, V}) ->
    {~"""
      variable ~w is already bound. If you mean to ignore this
      value, use '_' or a different underscore-prefixed name
      """, [V]};
format_error_1({match_underscore_var_pat, V}) ->
    {~"""
      variable ~w is bound multiple times in this pattern.
      If you mean to ignore this value, use '_' or
      a different underscore-prefixed name
      """, [V]};
format_error_1({shadowed_var,V,In}) ->
    {~"variable ~w shadowed in ~w", [V,In]};
format_error_1({unused_var, V}) ->
    {~"variable ~w is unused", [V]};
format_error_1({variable_in_record_def,V}) ->
    {~"variable ~w in record definition", [V]};
format_error_1({stacktrace_guard,V}) ->
    {~"stacktrace variable ~w must not be used in a guard", [V]};
format_error_1({stacktrace_bound,V}) ->
    {~"stacktrace variable ~w must not be previously bound", [V]};
%% --- binaries ---
format_error_1({undefined_bittype,Type}) ->
    {~"bit type ~tw undefined", [Type]};
format_error_1({bittype_mismatch,Val1,Val2,What}) ->
    {~"conflict in ~s specification for bit field: '~p' and '~p'",
		  [What,Val1,Val2]};
format_error_1(bittype_unit) ->
    ~"a bit unit size must not be specified unless a size is specified too";
format_error_1(illegal_bitsize) ->
    ~"illegal bit size";
format_error_1({illegal_bitsize_local_call, {F,A}}) ->
    {~"""
      call to local/imported function ~tw/~w is illegal
      in a size expression for a binary segment
      """, [F,A]};
format_error_1(non_integer_bitsize) ->
    ~"""
     a size expression in a pattern evaluates to a non-integer value;
     this pattern cannot possibly match
     """;
format_error_1(unsized_binary_not_at_end) ->
    ~"a binary field without size is only allowed at the end of a binary pattern";
format_error_1(typed_literal_string) ->
    ~"a literal string in a binary pattern must not have a type or a size";
format_error_1(utf_bittype_size_or_unit) ->
    ~"neither size nor unit must be given for segments of type utf8/utf16/utf32";
format_error_1({bad_bitsize,Type}) ->
    {~"bad ~s bit size", [Type]};
format_error_1(unsized_binary_in_bin_gen_pattern) ->
    ~"binary fields without size are not allowed in patterns of bit string generators";
%% --- behaviours ---
format_error_1({conflicting_behaviours,{Name,Arity},B,FirstL,FirstB}) ->
    {~"conflicting behaviours -- callback ~tw/~w required by both '~p' and '~p' ~s",
     [Name,Arity,B,FirstB,format_where(FirstL)]};
format_error_1({undefined_behaviour_func, {Func,Arity}, Behaviour}) ->
    {~"undefined callback function ~tw/~w (behaviour '~w')",
		  [Func,Arity,Behaviour]};
format_error_1({undefined_behaviour,Behaviour}) ->
    {~"behaviour ~tw undefined", [Behaviour]};
format_error_1({undefined_behaviour_callbacks,Behaviour}) ->
    {~"behaviour ~w callback functions are undefined",
		  [Behaviour]};
format_error_1({ill_defined_behaviour_callbacks,Behaviour}) ->
    {~"behaviour ~w callback functions erroneously defined",
		  [Behaviour]};
format_error_1({ill_defined_optional_callbacks,Behaviour}) ->
    {~"behaviour ~w optional callback functions erroneously defined",
		  [Behaviour]};
format_error_1({behaviour_info, {_M,F,A}}) ->
    {~"cannot define callback attibute for ~tw/~w when behaviour_info is defined",
     [F,A]};
format_error_1({redefine_optional_callback, {F, A}}) ->
    {~"optional callback ~tw/~w duplicated", [F, A]};
format_error_1({undefined_callback, {_M, F, A}}) ->
    {~"callback ~tw/~w is undefined", [F, A]};
%% --- types and specs ---
format_error_1({singleton_typevar, Name}) ->
    {~"type variable ~w is only used once (is unbound)", [Name]};
format_error_1({bad_export_type, _ETs}) ->
    {~"bad export_type declaration", []};
format_error_1({duplicated_export_type, {T, A}}) ->
    {~"type ~tw/~w already exported", [T, A]};
format_error_1({undefined_type, {TypeName, Arity}}) ->
    {~"type ~tw~s undefined", [TypeName, gen_type_paren(Arity)]};
format_error_1({unused_type, {TypeName, Arity}}) ->
    {~"type ~tw~s is unused", [TypeName, gen_type_paren(Arity)]};
format_error_1({redefine_builtin_type, {TypeName, Arity}}) ->
    {~"local redefinition of built-in type: ~w~s",
		  [TypeName, gen_type_paren(Arity)]};
format_error_1({renamed_type, OldName, NewName}) ->
    {~"type ~w() is now called ~w(); please use the new name instead",
     [OldName, NewName]};
format_error_1({redefine_type, {TypeName, Arity}}) ->
    {~"type ~tw~s already defined",
     [TypeName, gen_type_paren(Arity)]};
format_error_1({type_syntax, Constr}) ->
    {~"bad ~tw type", [Constr]};
format_error_1(old_abstract_code) ->
    ~"""
     abstract code generated before Erlang/OTP 19.0 and
     having typed record fields cannot be compiled
     """;
format_error_1({redefine_spec, {M, F, A}}) ->
    {~"spec for ~tw:~tw/~w already defined", [M, F, A]};
format_error_1({redefine_spec, {F, A}}) ->
    {~"spec for ~tw/~w already defined", [F, A]};
format_error_1({redefine_callback, {F, A}}) ->
    {~"callback ~tw/~w already defined", [F, A]};
format_error_1({bad_callback, {M, F, A}}) ->
    {~"explicit module not allowed for callback ~tw:~tw/~w",
                  [M, F, A]};
format_error_1({bad_module, {M, F, A}}) ->
    {~"spec for function ~w:~tw/~w from other module", [M, F, A]};
format_error_1({spec_fun_undefined, {F, A}}) ->
    {~"spec for undefined function ~tw/~w", [F, A]};
format_error_1({missing_spec, {F,A}}) ->
    {~"missing specification for function ~tw/~w", [F, A]};
format_error_1(spec_wrong_arity) ->
    ~"spec has wrong arity";
format_error_1(callback_wrong_arity) ->
    ~"callback has wrong arity";
format_error_1({deprecated_builtin_type, {Name, Arity},
              Replacement, Rel}) ->
    UseS = case Replacement of
               {Mod, NewName} ->
                   io_lib:format(~"use ~w:~w/~w", [Mod, NewName, Arity]);
               {Mod, NewName, NewArity} ->
                   io_lib:format(~"use ~w:~w/~w or preferably ~w:~w/~w",
                                 [Mod, NewName, Arity,
                                  Mod, NewName, NewArity])
           end,
    {~"type ~w/~w is deprecated and will be removed in ~s; use ~s",
     [Name, Arity, Rel, UseS]};
format_error_1(deprecated_catch) ->
    ~"""
     'catch ...' is deprecated and will be removed in a
     future version of Erlang/OTP; please use 'try ... catch ... end' instead.
     Compile directive 'nowarn_deprecated_catch' can be used to suppress
     warnings in selected modules.
     """;
 format_error_1({not_exported_opaque, {TypeName, Arity}}) ->
    {~"opaque type ~tw~s is not exported",
                  [TypeName, gen_type_paren(Arity)]};
format_error_1({bad_dialyzer_attribute,Term}) ->
    {~"badly formed dialyzer attribute: ~tw", [Term]};
format_error_1({bad_dialyzer_option,Term}) ->
    {~"unknown dialyzer warning option: ~tw", [Term]};
%% --- obsolete? unused? ---
format_error_1({format_error, {Fmt, Args}}) ->
    {Fmt, Args}.

gen_type_paren(Arity) when is_integer(Arity), Arity >= 0 ->
    gen_type_paren_1(Arity, ")").

gen_type_paren_1(0, Acc) -> "(" ++ Acc;
gen_type_paren_1(1, Acc) -> "(_" ++ Acc;
gen_type_paren_1(N, Acc) -> gen_type_paren_1(N - 1, ",_" ++ Acc).

format_mfa({M, F, [_|_]=As}) ->
    ","++ArityString = lists:append([[$,|integer_to_list(A)] || A <- As]),
    format_mf(M, F, ArityString);
format_mfa({M, F, A}) when is_integer(A) ->
    format_mf(M, F, integer_to_list(A)).

format_fa({F, [_|_]=As}) ->
    ","++ArityString = lists:append([[$,|integer_to_list(A)] || A <- As]),
    atom_to_list(F) ++ "/" ++ ArityString.

format_mf(M, F, ArityString) when is_atom(M), is_atom(F) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ ArityString.

format_mna({M, N, A}) when is_integer(A) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(N) ++ gen_type_paren(A).

format_where(L) when is_integer(L) ->
    io_lib:format("(line ~p)", [L]);
format_where({L,C}) when is_integer(L), is_integer(C) ->
    io_lib:format("(line ~p, column ~p)", [L, C]).

%% Local functions that are somehow automatically generated.

pseudolocals() ->
    [{module_info,0}, {module_info,1}, {record_info,2}].

%%
%% Used by erl_eval.erl to check commands.
%%
-doc false.
exprs(Exprs, BindingsList) ->
    exprs_opt(Exprs, BindingsList, []).

-doc false.
exprs_opt(Exprs, BindingsList, Opts) ->
    {St0,Vs} = foldl(fun({{record,_SequenceNumber,_Name},Attr0}, {St1,Vs1}) ->
                             Attr = set_file(Attr0, "none"),
			     {attribute_state(Attr, St1),Vs1};
                        ({V,_}, {St1,Vs1}) ->
			     {St1,[{V,{bound,unused,[]}} | Vs1]}
		     end, {start("nofile",Opts),[]}, BindingsList),
    Vt = orddict:from_list(Vs),
    {_Evt,St} = exprs(set_file(Exprs, "nofile"), Vt, St0),
    return_status(St).

-doc false.
used_vars(Exprs, BindingsList) ->
    Vs = foldl(fun({{record,_SequenceNumber,_Name},_Attr}, Vs0) -> Vs0;
		  ({V,_Val}, Vs0) -> [{V,{bound,unused,[]}} | Vs0]
	       end, [], BindingsList),
    Vt = orddict:from_list(Vs),
    St0 = (start())#lint{fun_used_vars=maps:new()},
    {_Evt,St1} = exprs(Exprs, Vt, St0),
    St1#lint.fun_used_vars.

%% module([Form]) ->
%% module([Form], FileName) ->
%% module([Form], FileName, [CompileOption]) ->
%%      {ok,[Warning]} | {error,[Error],[Warning]}
%%  Start processing a module. Define predefined functions and exports and
%%  apply_lambda/2 has been called to shut lint up. N.B. these lists are
%%  really all ordsets!

-doc(#{equiv => module/3}).
-spec(module(AbsForms) -> {ok, Warnings} | {error, Errors, Warnings} when
      AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      Warnings :: [{SourceFile,[ErrorInfo]}],
      Errors :: [{SourceFile,[ErrorInfo]}],
      SourceFile :: file:filename(),
      ErrorInfo :: error_info()).

module(Forms) ->
    Opts = compiler_options(Forms),
    St = forms(Forms, start("nofile", Opts)),
    return_status(St).

-doc(#{equiv => module/3}).
-spec(module(AbsForms, FileName) ->
             {ok, Warnings} | {error, Errors, Warnings} when
      AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      FileName :: atom() | string(),
      Warnings :: [{SourceFile,[ErrorInfo]}],
      Errors :: [{SourceFile,[ErrorInfo]}],
      SourceFile :: file:filename(),
      ErrorInfo :: error_info()).

module(Forms, FileName) ->
    Opts = compiler_options(Forms),
    St = forms(Forms, start(FileName, Opts)),
    return_status(St).

-doc """
Checks all the forms in a module for errors. It returns:

- **`{ok,Warnings}`** - There are no errors in the module.

- **`{error,Errors,Warnings}`** - There are errors in the module.

As this module is of interest only to the maintainers of the compiler, and to
avoid the same description in two places, the elements of `Options` that control
the warnings are only described in the [`compile`](`m:compile#erl_lint_options`)
module.

`AbsForms` of a module, which comes from a file that is read through `epp`, the
Erlang preprocessor, can come from many files. This means that any references to
errors must include the filename, see the `m:epp` module or parser (see the
`m:erl_parse` module). The returned errors and warnings have the following
format:

```text
[{SourceFile,[ErrorInfo]}]
```

The errors and warnings are listed in the order in which they are encountered in
the forms. The errors from one file can therefore be split into different
entries in the list of errors.
""".
-spec(module(AbsForms, FileName, CompileOptions) ->
             {ok, Warnings} | {error, Errors, Warnings} when
      AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      FileName :: atom() | string(),
      CompileOptions :: [compile:option()],
      Warnings :: [{SourceFile,[ErrorInfo]}],
      Errors :: [{SourceFile,[ErrorInfo]}],
      SourceFile :: file:filename(),
      ErrorInfo :: error_info()).

module(Forms, FileName, Opts0) ->
    %% FIXME Hmm, this is not coherent with the semantics of features
    %% We want the options given on the command line to take
    %% precedence over options in the module.
    Opts = Opts0 ++ compiler_options(Forms),
    St = forms(Forms, start(FileName, Opts)),
    return_status(St).

compiler_options(Forms) ->
    lists:flatten([C || {attribute,_,compile,C} <- Forms]).

%% start() -> State
%% start(FileName, [Option]) -> State

start() ->
    start("nofile", []).

start(File, Opts) ->
    Enabled0 = [Category || {Category,true} <- bool_options()],
    Enabled1 = ordsets:from_list(Enabled0),
    Enabled = parse_options(Opts, Enabled1),
    Calls = case ordsets:is_element(unused_function, Enabled) of
		true ->
		    #{{module_info,1} => pseudolocals()};
		false ->
		    undefined
	    end,
    #lint{state = start,
          exports = gb_sets:from_list([{module_info,0},{module_info,1}]),
          compile = Opts,
          %% Internal pseudo-functions must appear as defined/reached.
          defined = gb_sets:from_list(pseudolocals()),
	  called = [{F,0} || F <- pseudolocals()],
          usage = #usage{calls=Calls},
          warn_format = value_option(warn_format, 1, warn_format, 1,
				     nowarn_format, 0, Opts),
	  enabled_warnings = Enabled,
          nowarn_bif_clash = nowarn_function(nowarn_bif_clash, Opts),
          file = File
         }.

parse_options([Opt0|Opts], Enabled0) when is_atom(Opt0) ->
    {Opt2,Enable} = case atom_to_binary(Opt0) of
                        <<"warn_",Opt1/binary>> ->
                            {Opt1,true};
                        <<"nowarn_",Opt1/binary>> ->
                            {Opt1,false};
                        _ ->
                            {none,none}
                    end,
    Opt = try
              binary_to_existing_atom(Opt2)
          catch
              _:_ ->
                  []
          end,
    Enabled =
        maybe
            true ?= is_atom(Opt),
            true ?= lists:keymember(Opt, 1, bool_options()),
            if
                Enable ->
                    ordsets:add_element(Opt, Enabled0);
                not Enable ->
                    ordsets:del_element(Opt, Enabled0)
            end
        else
            _ ->
                Enabled0
        end,
    parse_options(Opts, Enabled);
parse_options([_|Opts], Enabled) ->
    parse_options(Opts, Enabled);
parse_options([], Enabled) ->
    Enabled.

bool_options() ->
    [{unused_vars,true},
     {underscore_match,true},
     {export_all,true},
     {export_vars,false},
     {shadow_vars,true},
     {unused_import,false},
     {unused_function,true},
     {unused_type,true},
     {bif_clash,true},
     {unused_record,true},
     {deprecated_function,true},
     {deprecated_type,true},
     {deprecated_callback,true},
     {deprecated_catch,false},
     {obsolete_guard,true},
     {untyped_record,false},
     {missing_spec,false},
     {missing_spec_documented,false},
     {missing_spec_all,false},
     {removed,true},
     {nif_inline,true},
     {keywords,false},
     {redefined_builtin_type,true},
     {match_float_zero,true},
     {update_literal,true},
     {behaviours,true},
     {conflicting_behaviours,true},
     {undefined_behaviour_func,true},
     {undefined_behaviour,true},
     {undefined_behaviour_callbacks,true},
     {ill_defined_behaviour_callbacks,true},
     {ill_defined_optional_callbacks,true},
     {unexported_function,true}].

%% is_warn_enabled(Category, St) -> boolean().
%%  Check whether a warning of category Category is enabled.
is_warn_enabled(Type, #lint{enabled_warnings=Enabled}) ->
    ordsets:is_element(Type, Enabled).

%% return_status(State) ->
%%      {ok,[Warning]} | {error,[Error],[Warning]}
%%  Pack errors and warnings properly and return ok | error.

return_status(St) ->
    Ws = pack_warnings(St#lint.warnings),
    case pack_errors(St#lint.errors) of
        [] -> {ok,Ws};
        Es -> {error,Es,Ws}
    end.

%% pack_errors([{File,ErrD}]) -> [{File,[ErrD]}].
%%  Sort on (reversed) insertion order.

pack_errors(Es) ->
    {Es1,_} = mapfoldl(fun ({File,E}, I) -> {{File,{I,E}}, I-1} end, -1, Es),
    map(fun ({File,EIs}) -> {File, map(fun ({_I,E}) -> E end, EIs)} end,
        pack_warnings(Es1)).

%% pack_warnings([{File,ErrD}]) -> [{File,[ErrD]}]
%%  Sort on line number.

pack_warnings(Ws) ->
    [{File,lists:sort([W || {F,W} <- Ws, F =:= File])} ||
        File <- lists:usort([F || {F,_} <- Ws])].

%% add_error(ErrorDescriptor, State) -> State'
%% add_error(Anno, Error, State) -> State'
%% add_warning(ErrorDescriptor, State) -> State'
%% add_warning(Anno, Error, State) -> State'

add_error(E, St) -> add_lint_error(E, St#lint.file, St).

add_error(Anno, E0, #lint{gexpr_context=Context}=St) ->
    E = case {E0,Context} of
            {illegal_guard_expr,bin_seg_size} ->
                illegal_bitsize;
            {{illegal_guard_local_call,FA},bin_seg_size} ->
                {illegal_bitsize_local_call,FA};
            {_,_} -> E0
        end,
    {File,Location} = loc(Anno, St),
    add_lint_error({Location,erl_lint,E}, File, St).

add_lint_error(E, File, St) ->
    St#lint{errors=[{File,E}|St#lint.errors]}.

add_warning(W, St) -> add_lint_warning(W, St#lint.file, St).

add_warning(Anno, W, St) ->
    {File,Location} = loc(Anno, St),
    add_lint_warning({Location,erl_lint,W}, File, St).

add_lint_warning(W, File, St) ->
    St#lint{warnings=[{File,W}|St#lint.warnings]}.

maybe_add_warning(Anno, W, St) ->
    Tag = if
              is_tuple(W) -> element(1, W);
              is_atom(W) -> W
          end,
    case is_warn_enabled(Tag, St) of
        true ->
            add_warning(Anno, W, St);
        false ->
            St
    end.

loc(Anno, St) ->
    Location = erl_anno:location(Anno),
    case erl_anno:file(Anno) of
        undefined -> {St#lint.file,Location};
        File -> {File,Location}
    end.

%% forms([Form], State) -> State'

forms(Forms0, St0) ->
    Forms = eval_file_attribute(Forms0, St0),
    %% Annotations from now on include the 'file' item.
    Locals = local_functions(Forms),
    AutoImportSuppressed = auto_import_suppressed(St0#lint.compile),
    StDeprecated = disallowed_compile_flags(Forms,St0),
    St1 = includes_qlc_hrl(Forms, StDeprecated#lint{locals = Locals,
						    no_auto = AutoImportSuppressed}),
    St2 = bif_clashes(Forms, St1),
    St3 = not_deprecated(Forms, St2),
    St4 = not_removed(Forms, St3),
    St5 = foldl(fun form/2, pre_scan(Forms, St4), Forms),
    post_traversal_check(Forms, St5).

pre_scan([{attribute,A,compile,C} | Fs], St) ->
    case is_warn_enabled(export_all, St) andalso
	member(export_all, lists:flatten([C])) of
 	true ->
	    pre_scan(Fs, add_warning(A, export_all, St));
 	false ->
	    pre_scan(Fs, St)
    end;
pre_scan([_ | Fs], St) ->
    pre_scan(Fs, St);
pre_scan([], St) ->
    St.

includes_qlc_hrl(Forms, St) ->
    %% QLC calls erl_lint several times, sometimes with the compile
    %% attribute removed. The file attribute, however, is left as is.
    QH = [File || {attribute,_,file,{File,_line}} <- Forms,
                  filename:basename(File) =:= "qlc.hrl"],
    St#lint{xqlc = QH =/= []}.

eval_file_attribute(Forms, St) ->
    eval_file_attr(Forms, St#lint.file).

eval_file_attr([{attribute,_A,file,{File,_Line}}=Form | Forms], _File) ->
    [Form | eval_file_attr(Forms, File)];
eval_file_attr([Form0 | Forms], File) ->
    Form = set_form_file(Form0, File),
    [Form | eval_file_attr(Forms, File)];
eval_file_attr([], _File) ->
    [].

%% Sets the file only on the form. This is used on post-traversal.
%% For the remaining of the AST we rely on #lint.file.

set_form_file({attribute,A,K,V}, File) ->
    {attribute,erl_anno:set_file(File, A),K,V};
set_form_file({function,Anno,N,A,C}, File) ->
    {function,erl_anno:set_file(File, Anno),N,A,C};
set_form_file(Form, _File) ->
    Form.

set_file(Ts, File) when is_list(Ts) ->
    [anno_set_file(T, File) || T <- Ts];
set_file(T, File) ->
    anno_set_file(T, File).

anno_set_file(T, File) ->
    F = fun(Anno) -> erl_anno:set_file(File, Anno) end,
    erl_parse:map_anno(F, T).

%% form(Form, State) -> State'
%%  Check a form returning the updated State. Handle generic cases here.

form({error,E}, St)   -> add_error(E, St);
form({warning,W}, St) -> add_warning(W, St);
form({attribute,_A,file,{File,_Line}}, St) ->
    St#lint{file = File};
form({attribute,_A,compile,_}, St) ->
    St;
form(Form, #lint{state=State}=St) ->
    case State of
	start -> start_state(Form, St);
	attribute -> attribute_state(Form, St);
	function -> function_state(Form, St)
    end.

%% start_state(Form, State) -> State'

start_state({attribute,Anno,module,{_,_}}=Form, St0) ->
    St1 = add_error(Anno, pmod_unsupported, St0),
    attribute_state(Form, St1#lint{state=attribute});
start_state({attribute,Anno,module,M}, St0) ->
    St1 = St0#lint{module=M},
    St2 = St1#lint{state=attribute},
    check_module_name(M, Anno, St2);
start_state(Form, St) ->
    Anno = case Form of
               {eof, Location} -> erl_anno:new(Location);
               %% {warning, Warning} and {error, Error} not possible here.
               _ -> element(2, Form)
           end,
    St1 = add_error(Anno, undefined_module, St),
    attribute_state(Form, St1#lint{state=attribute}).

%% attribute_state(Form, State) ->
%%      State'

attribute_state({attribute,_A,module,_M}, #lint{module=''}=St) ->
    St;
attribute_state({attribute,A,module,_M}, St) ->
    add_error(A, redefine_module, St);
attribute_state({attribute,A,export,Es}, St) ->
    export(A, Es, St);
attribute_state({attribute,A,export_type,Es}, St) ->
    export_type(A, Es, St);
attribute_state({attribute,A,import,Is}, St) ->
    import(A, Is, St);
attribute_state({attribute,A,record,{Name,Fields}}, St) ->
    record_def(A, Name, Fields, St);
attribute_state({attribute,Aa,behaviour,Behaviour}, St) ->
    St#lint{behaviour=St#lint.behaviour ++ [{Aa,Behaviour}]};
attribute_state({attribute,Aa,behavior,Behaviour}, St) ->
    St#lint{behaviour=St#lint.behaviour ++ [{Aa,Behaviour}]};
attribute_state({attribute,A,type,{TypeName,TypeDef,Args}}, St) ->
    St1 = untrack_doc({type, TypeName, length(Args)}, St),
    type_def(type, A, TypeName, TypeDef, Args, St1);
attribute_state({attribute,A,opaque,{TypeName,TypeDef,Args}}, St) ->
    St1 = untrack_doc({type, TypeName, length(Args)}, St),
    type_def(opaque, A, TypeName, TypeDef, Args, St1);
attribute_state({attribute,A,nominal,{TypeName,TypeDef,Args}}=AST, St) ->
    St1 = untrack_doc(AST, St),
    type_def(nominal, A, TypeName, TypeDef, Args, St1);
attribute_state({attribute,A,spec,{Fun,Types}}, St) ->
    spec_decl(A, Fun, Types, St);
attribute_state({attribute,A,callback,{Fun,Types}}, St) ->
    St1 = untrack_doc({callback, Fun}, St),
    callback_decl(A, Fun, Types, St1);
attribute_state({attribute,A,optional_callbacks,Es}, St) ->
    optional_callbacks(A, Es, St);
attribute_state({attribute,A,on_load,Val}, St) ->
    on_load(A, Val, St);
attribute_state({attribute, _A, moduledoc, _Doc}=AST, St)  ->
    track_doc(AST, St);
attribute_state({attribute, _A, doc, _Doc}=AST, St)  ->
    track_doc(AST, St);
attribute_state({attribute,_A,_Other,_Val}, St) -> % Ignore others
    St;
attribute_state(Form, St) ->
    function_state(Form, St#lint{state=function}).


%% Tracks whether we have read a documentation attribute string multiple times.
%% Terminal elements that reset the state of the documentation attribute tracking
%% are:
%%
%% - function,
%% - opaque,
%% - type
%% - callback
%%
%% These terminal elements are also the only ones where one should place
%% documentation attributes.
track_doc({attribute, A, Tag, Doc}, #lint{}=St) when
      is_list(Doc) orelse is_binary(Doc) orelse Doc =:= false orelse Doc =:= hidden ->
    case get_doc_attr(Tag, St) of
        {true, Ann, _} -> add_error(A, {Tag, duplicate_doc_attribute, erl_anno:line(Ann)}, St);
        false -> update_doc_attr(Tag, A, Doc =:= hidden orelse Doc =:= false, St)
    end;
track_doc(_, St) ->
    St.

%%
%% Helper functions to track documentation attributes
%%
get_doc_attr(moduledoc, #lint{moduledoc_defined = Moduledoc}) -> Moduledoc;
get_doc_attr(doc, #lint{doc_defined = Doc}) -> Doc.

update_doc_attr(moduledoc, A, Hidden, #lint{}=St) ->
    St#lint{moduledoc_defined = {true, A, Hidden}};
update_doc_attr(doc, A, Hidden, #lint{}=St) ->
    St#lint{doc_defined = {true, A, Hidden}}.

%% Reset the tracking of a documentation attribute.
%%
%% That is, assume that a terminal object was reached, thus we need to reset
%% the state so that the linter understands that we have not seen any other
%% documentation attribute.
untrack_doc({callback,{_M, F, A}}, St) ->
    untrack_doc({callback, F, A}, St);
untrack_doc({callback,{F, A}}, St) ->
    untrack_doc({callback,F, A}, St);
untrack_doc({function, F, A}, #lint{ hidden_docs = Ds, doc_defined = {_, _, true} } = St) ->
    St#lint{hidden_docs = sets:add_element({F,A}, Ds), doc_defined = false};
untrack_doc({function, F, A}, #lint{ hidden_docs = Ds, moduledoc_defined = {_, _, true} } = St) ->
    St#lint{hidden_docs = sets:add_element({F,A}, Ds), doc_defined = false};
untrack_doc(_KFA, St) ->
    St#lint{ doc_defined = false }.


%% function_state(Form, State) ->
%%      State'
%%  Allow for record, type and opaque type definitions and spec
%%  declarations to be intersperced within function definitions.
%%  Dialyzer attributes are also allowed everywhere.

function_state({attribute,A,record,{Name,Fields}}, St) ->
    record_def(A, Name, Fields, St);
function_state({attribute,A,type,{TypeName,TypeDef,Args}}, St) ->
    St1 = untrack_doc({type, TypeName, length(Args)}, St),
    type_def(type, A, TypeName, TypeDef, Args, St1);
function_state({attribute,A,opaque,{TypeName,TypeDef,Args}}, St) ->
    St1 = untrack_doc({type, TypeName, length(Args)}, St),
    type_def(opaque, A, TypeName, TypeDef, Args, St1);
function_state({attribute,A,nominal,{TypeName,TypeDef,Args}}=AST, St) ->
    St1 = untrack_doc(AST, St),
    type_def(nominal, A, TypeName, TypeDef, Args, St1);
function_state({attribute,A,spec,{Fun,Types}}, St) ->
    spec_decl(A, Fun, Types, St);
function_state({attribute,_A,doc,_Val}=AST, St) ->
    track_doc(AST, St);
function_state({attribute,_A,moduledoc,_Val}=AST, St) ->
    track_doc(AST, St);
function_state({attribute,_A,dialyzer,_Val}, St) ->
    St;
function_state({attribute,Aa,Attr,_Val}, St) ->
    add_error(Aa, {attribute,Attr}, St);
function_state({function,Anno,N,A,Cs}, St) ->
    St1 = untrack_doc({function, N, A}, St),
    function(Anno, N, A, Cs, St1);
function_state({eof,Location}, St) -> eof(Location, St).

%% eof(LastLocation, State) ->
%%      State'

eof(_Location, St0) ->
    St0.

%% bif_clashes(Forms, State0) -> State.

bif_clashes(Forms, #lint{nowarn_bif_clash=Nowarn} = St) ->
    Clashes0 = [{Name,Arity} || {function,_A,Name,Arity,_Cs} <- Forms,
                                erl_internal:bif(Name, Arity)],
    Clashes = ordsets:subtract(ordsets:from_list(Clashes0), Nowarn),
    St#lint{clashes=Clashes}.

%% not_deprecated(Forms, State0) -> State

not_deprecated(Forms, #lint{compile=Opts}=St0) ->
    %% There are no line numbers in St0#lint.compile.
    MFAsAnno = [{MFA,Anno} ||
                {attribute, Anno, compile, Args} <- Forms,
                {nowarn_deprecated_function, MFAs0} <- lists:flatten([Args]),
                MFA <- lists:flatten([MFAs0])],
    Nowarn = [MFA ||
                 {nowarn_deprecated_function, MFAs0} <- Opts,
                 MFA <- lists:flatten([MFAs0])],
    MAnno = [{M,Anno} || {{M,_F,_A},Anno} <- MFAsAnno, is_atom(M)],
    St1 = foldl(fun ({M,Anno}, St2) ->
                        check_module_name(M, Anno, St2)
                end, St0, MAnno),
    St1#lint{not_deprecated = ordsets:from_list(Nowarn)}.

%% not_removed(Forms, State0) -> State

not_removed(Forms, #lint{compile=Opts}=St0) ->
    %% There are no line numbers in St0#lint.compile.
    MFAsAnno = [{MFA,Anno} ||
                {attribute, Anno, compile, Args} <- Forms,
                {nowarn_removed, MFAs0} <- lists:flatten([Args]),
                MFA <- lists:flatten([MFAs0])],
    Nowarn = [MFA ||
                 {nowarn_removed, MFAs0} <- Opts,
                 MFA <- lists:flatten([MFAs0])],
    St1 = foldl(fun ({{M, _F, _A}, Anno}, St2) ->
                        check_module_name(M, Anno, St2);
                    ({M,Anno}, St2) ->
                        check_module_name(M, Anno, St2)
                end, St0, MFAsAnno),
    St1#lint{not_removed = gb_sets:from_list(Nowarn)}.

%% The nowarn_bif_clash directive is not only deprecated, it's actually an error from R14A
disallowed_compile_flags(Forms, St0) ->
    %% There are (still) no line numbers in St0#lint.compile.
    Errors0 =  [ {St0#lint.file,{L,erl_lint,disallowed_nowarn_bif_clash}} ||
		    {attribute,A,compile,nowarn_bif_clash} <- Forms,
                   {_,L} <- [loc(A, St0)] ],
    Errors1 = [ {St0#lint.file,{L,erl_lint,disallowed_nowarn_bif_clash}} ||
		    {attribute,A,compile,{nowarn_bif_clash, {_,_}}} <- Forms,
                   {_,L} <- [loc(A, St0)] ],
    Disabled = (not is_warn_enabled(bif_clash, St0)),
    Errors = if
		   Disabled andalso Errors0 =:= [] ->
		       [{St0#lint.file,{erl_lint,disallowed_nowarn_bif_clash}} | St0#lint.errors];
                   Disabled ->
		       Errors0 ++ Errors1 ++ St0#lint.errors;
		   true ->
		       Errors1 ++ St0#lint.errors
	       end,
    St0#lint{errors=Errors}.

%% post_traversal_check(Forms, State0) -> State.
%% Do some further checking after the forms have been traversed and
%% data about calls etc. have been collected.

post_traversal_check(Forms, St0) ->
    St1 = check_behaviour(St0),
    St2 = check_deprecated(Forms, St1),
    St3 = check_imports(Forms, St2),
    St4 = check_inlines(Forms, St3),
    St5 = check_undefined_functions(St4),
    St6 = check_unused_functions(Forms, St5),
    St7 = check_bif_clashes(Forms, St6),
    St8 = check_specs_without_function(St7),
    St9 = check_functions_without_spec(Forms, St8),
    StA = check_undefined_types(St9),
    StB = check_unused_types(Forms, StA),
    StC = check_untyped_records(Forms, StB),
    StD = check_on_load(StC),
    StE = check_unused_records(Forms, StD),
    StF = check_local_opaque_types(StE),
    StG = check_dialyzer_attribute(Forms, StF),
    StH = check_callback_information(StG),
    StI = check_nifs(Forms, StH),
    StJ = check_unexported_functions(StI),
    check_removed(Forms, StJ).

%% check_behaviour(State0) -> State
%% Check that the behaviour attribute is valid.

check_behaviour(St) ->
    case is_warn_enabled(behaviours, St) of
        true ->
            behaviour_check(St#lint.behaviour, St);
        false ->
            St
    end.

%% behaviour_check([{Anno,Behaviour}], State) -> State'
%%  Check behaviours for existence and defined functions.

behaviour_check(Bs, St0) ->
    {AllBfs0, St1} = all_behaviour_callbacks(Bs, [], St0),
    St2 = behaviour_missing_callbacks(AllBfs0, St1),
    Exports = exports(St0),
    F = fun(Bfs, OBfs) ->
                [B || B <- Bfs,
                      not lists:member(B, OBfs)
                      orelse gb_sets:is_member(B, Exports)]
        end,
    %% After fixing missing callbacks new warnings may be emitted.
    AllBfs = [{Item,F(Bfs0, OBfs0)} || {Item,Bfs0,OBfs0} <- AllBfs0],
    St3 = behaviour_conflicting(AllBfs, St2),
    behaviour_deprecated(AllBfs0, Exports, St3).

all_behaviour_callbacks([{Anno,B}|Bs], Acc, St0) ->
    {Bfs0,OBfs0,St} = behaviour_callbacks(Anno, B, St0),
    all_behaviour_callbacks(Bs, [{{Anno,B},Bfs0,OBfs0}|Acc], St);
all_behaviour_callbacks([], Acc, St) -> {reverse(Acc),St}.

add_behaviour_warning(Anno, Warning, St) when is_tuple(Warning) ->
    maybe_add_warning(Anno, Warning, St).

behaviour_callbacks(Anno, B, St0) ->
    try B:behaviour_info(callbacks) of
        undefined ->
            St1 = add_behaviour_warning(Anno,
                                        {undefined_behaviour_callbacks, B},
                                        St0),
            {[], [], St1};
        Funcs ->
            case is_fa_list(Funcs) of
                true ->
                    try B:behaviour_info(optional_callbacks) of
                        undefined ->
                            {Funcs, [], St0};
                        OptFuncs ->
                            %% OptFuncs should always be OK thanks to
                            %% sys_pre_expand.
                            case is_fa_list(OptFuncs) of
                                true ->
                                    {Funcs, OptFuncs, St0};
                                false ->
                                    W = {ill_defined_optional_callbacks, B},
                                    St1 = add_behaviour_warning(Anno, W, St0),
                                    {Funcs, [], St1}
                            end
                    catch
                        _:_ ->
                            {Funcs, [], St0}
                    end;
                false ->
                    St1 = add_behaviour_warning(Anno,
                                                {ill_defined_behaviour_callbacks, B},
                                                St0),
                    {[], [], St1}
            end
    catch
        _:_ ->
            St1 = add_behaviour_warning(Anno, {undefined_behaviour, B}, St0),
            St2 = check_module_name(B, Anno, St1),
            {[], [], St2}
    end.

behaviour_deprecated([{{Anno, B}, Bfs, _OBfs} | T], Exports, St) ->
    behaviour_deprecated(T, Exports,
                         behaviour_deprecated(Anno, B, Bfs, Exports, St));
behaviour_deprecated([], _Exports, St) ->
    St.

-dialyzer({no_match, behaviour_deprecated/5}).

behaviour_deprecated(Anno, B, [{F, A} | T], Exports, St0) ->
    St =
        case gb_sets:is_member({F,A}, Exports) of
            false -> St0;
            true ->
                case otp_internal:obsolete_callback(B, F, A) of
                    {deprecated, String} when is_list(String) ->
                        maybe_add_warning(Anno, {deprecated_callback, {B, F, A}, String}, St0);
                    {removed, String} ->
                        add_warning(Anno, {removed_callback, {B, F, A}, String}, St0);
                    no ->
                        St0
                end
        end,
    behaviour_deprecated(Anno, B, T, Exports, St);
behaviour_deprecated(_Anno, _B, [], _Exports, St) ->
    St.

behaviour_missing_callbacks([{{Anno,B},Bfs0,OBfs}|T], St0) ->
    Bfs = ordsets:subtract(ordsets:from_list(Bfs0), ordsets:from_list(OBfs)),
    Exports = gb_sets:to_list(exports(St0)),
    Missing = ordsets:subtract(Bfs, Exports),
    St = foldl(fun (F, S0) ->
                       case is_fa(F) of
                           true ->
                               M = {undefined_behaviour_func,F,B},
                               add_behaviour_warning(Anno, M, S0);
                           false ->
                               S0 % ill_defined_behaviour_callbacks
                       end
	       end, St0, Missing),
    behaviour_missing_callbacks(T, St);
behaviour_missing_callbacks([], St) -> St.

behaviour_conflicting(AllBfs, St) ->
    R0 = sofs:relation(AllBfs, [{item,[callback]}]),
    R1 = sofs:family_to_relation(R0),
    R2 = sofs:converse(R1),
    R3 = sofs:relation_to_family(R2),
    R4 = sofs:family_specification(fun(S) -> sofs:no_elements(S) > 1 end, R3),
    R = sofs:to_external(R4),
    behaviour_add_conflicts(R, St).

behaviour_add_conflicts([{Cb,[{FirstAnno,FirstB}|Cs]}|T], St0) ->
    FirstL = element(2, loc(FirstAnno, St0)),
    St = behaviour_add_conflict(Cs, Cb, FirstL, FirstB, St0),
    behaviour_add_conflicts(T, St);
behaviour_add_conflicts([], St) -> St.

behaviour_add_conflict([{Anno,B}|Cs], Cb, FirstL, FirstB, St0) ->
    St = add_behaviour_warning(Anno,
                               {conflicting_behaviours,Cb,B,FirstL,FirstB},
                               St0),
    behaviour_add_conflict(Cs, Cb, FirstL, FirstB, St);
behaviour_add_conflict([], _, _, _, St) -> St.

%% check_deprecated(Forms, State0) -> State

check_deprecated(Forms, St0) ->
    Exports = exports(St0),
    X = ignore_predefined_funcs(gb_sets:to_list(Exports)),
    #lint{module = Mod} = St0,
    Bad = [{E,Anno} || {attribute, Anno, deprecated, Depr} <- Forms,
                    D <- lists:flatten([Depr]),
                    E <- depr_cat(D, X, Mod)],
    foldl(fun ({E,Anno}, St1) ->
                  add_error(Anno, E, St1)
          end, St0, Bad).

depr_cat({F, A, Flg}=D, X, Mod) ->
    case deprecated_flag(Flg) of
        false -> [{invalid_deprecated,D}];
        true -> depr_fa(F, A, X, Mod)
    end;
depr_cat({F, A}, X, Mod) ->
    depr_fa(F, A, X, Mod);
depr_cat(module, _X, _Mod) ->
    [];
depr_cat(D, _X, _Mod) ->
    [{invalid_deprecated,D}].

depr_fa('_', '_', _X, _Mod) ->
    [];
depr_fa(F, '_', X, _Mod) when is_atom(F) ->
    %% Don't use this syntax for built-in functions.
    case lists:filter(fun({F1,_}) -> F1 =:= F end, X) of
        [] -> [{bad_deprecated,{F,'_'}}];
        _ -> []
    end;
depr_fa(F, A, X, Mod) when is_atom(F), is_integer(A), A >= 0 ->
    case lists:member({F,A}, X) of
        true -> [];
        false ->
            case erlang:is_builtin(Mod, F, A) of
                true -> [];
                false -> [{bad_deprecated,{F,A}}]
            end
    end;
depr_fa(F, A, _X, _Mod) ->
    [{invalid_deprecated,{F,A}}].

deprecated_flag(next_version) -> true;
deprecated_flag(next_major_release) -> true;
deprecated_flag(eventually) -> true;
deprecated_flag(String) -> deprecated_desc(String).

deprecated_desc([Char | Str]) when is_integer(Char) -> deprecated_desc(Str);
deprecated_desc([]) -> true;
deprecated_desc(_) -> false.

%% check_removed(Forms, State0) -> State

check_removed(Forms, St0) ->
    Exports = exports(St0),
    X = ignore_predefined_funcs(gb_sets:to_list(Exports)),
    #lint{module = Mod} = St0,
    Bad = [{E,Anno} || {attribute, Anno, removed, Removed} <- Forms,
                    R <- lists:flatten([Removed]),
                    E <- removed_cat(R, X, Mod)],
    foldl(fun ({E,Anno}, St1) ->
                  add_error(Anno, E, St1)
          end, St0, Bad).

removed_cat({F, A, Desc}=R, X, Mod) ->
    case removed_desc(Desc) of
        false -> [{invalid_removed,R}];
        true -> removed_fa(F, A, X, Mod)
    end;
removed_cat({F, A}, X, Mod) ->
    removed_fa(F, A, X, Mod);
removed_cat(module, X, Mod) ->
    removed_fa('_', '_', X, Mod);
removed_cat(R, _X, _Mod) ->
    [{invalid_removed,R}].

removed_fa('_', '_', X, _Mod) ->
    case X of
        [_|_] -> [{bad_removed,{'_','_'}}];
        [] -> []
    end;
removed_fa(F, '_', X, _Mod) when is_atom(F) ->
    %% Don't use this syntax for built-in functions.
    case lists:filter(fun({F1,_}) -> F1 =:= F end, X) of
        [_|_] -> [{bad_removed,{F,'_'}}];
        _ -> []
    end;
removed_fa(F, A, X, Mod) when is_atom(F), is_integer(A), A >= 0 ->
    case lists:member({F,A}, X) of
        true ->
            [{bad_removed,{F,A}}];
        false ->
            case erlang:is_builtin(Mod, F, A) of
                true -> [{bad_removed,{F,A}}];
                false -> []
            end
    end;
removed_fa(F, A, _X, _Mod) ->
    [{invalid_removed,{F,A}}].

removed_desc([Char | Str]) when is_integer(Char) -> removed_desc(Str);
removed_desc([]) -> true;
removed_desc(_) -> false.

%% Ignores functions added by erl_internal:add_predefined_functions/1
ignore_predefined_funcs([{behaviour_info,1} | Fs]) ->
    ignore_predefined_funcs(Fs);
ignore_predefined_funcs([{module_info,0} | Fs]) ->
    ignore_predefined_funcs(Fs);
ignore_predefined_funcs([{module_info,1} | Fs]) ->
    ignore_predefined_funcs(Fs);
ignore_predefined_funcs([Other | Fs]) ->
    [Other | ignore_predefined_funcs(Fs)];
ignore_predefined_funcs([]) ->
    [].

%% check_imports(Forms, State0) -> State

check_imports(Forms, St0) ->
    case is_warn_enabled(unused_import, St0) of
        false ->
            St0;
        true ->
            Usage = St0#lint.usage,
            Unused = ordsets:subtract(St0#lint.imports, Usage#usage.imported),
            Imports = [{{FA,Mod},Anno} ||
		 {attribute,Anno,import,{Mod,Fs}} <- Forms,
		 FA <- lists:usort(Fs)],
            Bad = [{FM,Anno} || FM <- Unused, {FM2,Anno} <- Imports, FM =:= FM2],
            func_location_warning(unused_import, Bad, St0)
    end.

%% check_inlines(Forms, State0) -> State

check_inlines(Forms, St0) ->
    check_option_functions(Forms, inline, bad_inline, St0).

%% check_unused_functions(Forms, State0) -> State

check_unused_functions(Forms, St0) ->
    St1 = check_option_functions(Forms, nowarn_unused_function,
                                 bad_nowarn_unused_function, St0),
    Opts = St1#lint.compile,
    case member(export_all, Opts) orelse
	not is_warn_enabled(unused_function, St1) of
        true ->
            St1;
        false ->
            Nowarn = nowarn_function(nowarn_unused_function, Opts),
            Usage = St1#lint.usage,
            Used = reached_functions(initially_reached(St1),
				     Usage#usage.calls),
            UsedOrNowarn = ordsets:union(Used, Nowarn),
            Unused = ordsets:subtract(gb_sets:to_list(St1#lint.defined),
				      UsedOrNowarn),
            Functions = [{{N,A},Anno} || {function,Anno,N,A,_} <- Forms],
            Bad = [{FA,Anno} || FA <- Unused, {FA2,Anno} <- Functions, FA =:= FA2],
            func_location_warning(unused_function, Bad, St1)
    end.

initially_reached(#lint{exports=Exp,on_load=OnLoad}) ->
    OnLoad ++ gb_sets:to_list(Exp).

%% reached_functions(RootSet, CallRef) -> [ReachedFunc].
%% reached_functions(RootSet, CallRef, [ReachedFunc]) -> [ReachedFunc].

reached_functions(Root, Ref) ->
    reached_functions(Root, [], Ref, gb_sets:empty()).

reached_functions([R|Rs], More0, Ref, Reached0) ->
    case gb_sets:is_element(R, Reached0) of
        true -> reached_functions(Rs, More0, Ref, Reached0);
        false ->
            Reached = gb_sets:add_element(R, Reached0), %It IS reached
            case maps:find(R, Ref) of
                {ok,More} -> reached_functions(Rs, [More|More0], Ref, Reached);
                error -> reached_functions(Rs, More0, Ref, Reached)
            end
    end;
reached_functions([], [_|_]=More, Ref, Reached) ->
    reached_functions(lists:append(More), [], Ref, Reached);
reached_functions([], [], _Ref, Reached) -> gb_sets:to_list(Reached).

%% check_undefined_functions(State0) -> State

check_undefined_functions(#lint{called=Called0,defined=Def0}=St0) ->
    Called = sofs:relation(Called0, [{func,location}]),
    Def = sofs:from_external(gb_sets:to_list(Def0), [func]),
    Undef = sofs:to_external(sofs:drestriction(Called, Def)),
    FAList = sofs:to_external(Def),
    func_location_error(undefined_function, Undef, St0, FAList).

most_possible_string(Name, PossibleNames) ->
    case PossibleNames of
        [] -> [];
        _ ->
            %% kk and kl has a similarity of 0.66. Short names are common in
            %% Erlang programs, therefore we choose a relatively low threshold
            %% here.
            SufficientlySimilar = 0.66,
            NameString = atom_to_list(Name),
            Similarities = [{string:jaro_similarity(NameString, F), F} ||
                               F <- PossibleNames],
            {MaxSim, GuessName} = lists:last(lists:sort(Similarities)),
            case MaxSim > SufficientlySimilar of
                true -> list_to_existing_atom(GuessName);
                false -> []
            end
    end.

%% check_undefined_types(State0) -> State

check_undefined_types(#lint{usage=Usage,types=Def}=St0) ->
    Used = Usage#usage.used_types,
    UTAs = maps:keys(Used),
    Undef = [{TA,map_get(TA, Used)} ||
		TA <- UTAs,
		not is_map_key(TA, Def),
		not is_default_type(TA)],
    foldl(fun ({TA,UsedTypeList}, St) ->
                  foldl( fun(#used_type{anno = Anno}, St1) ->
                                 add_error(Anno, {undefined_type,TA}, St1)
                         end, St, UsedTypeList)
	  end, St0, Undef).

%% check_bif_clashes(Forms, State0) -> State

check_bif_clashes(Forms, St0) ->
    %% St0#lint.defined is now complete.
    check_option_functions(Forms, nowarn_bif_clash,
                           bad_nowarn_bif_clash, St0).

check_option_functions(Forms, Tag0, Type, St0) ->
    %% There are no line numbers in St0#lint.compile.
    FAsAnno = [{FA,Anno} || {attribute, Anno, compile, Args} <- Forms,
                      {Tag, FAs0} <- lists:flatten([Args]),
                      Tag0 =:= Tag,
                      FA <- lists:flatten([FAs0])],
    DefFunctions = (gb_sets:to_list(St0#lint.defined) -- pseudolocals()) ++
	[{F,A} || {{F,A},_} <- orddict:to_list(St0#lint.imports)],
    Bad = [{FA,Anno} || {FA,Anno} <- FAsAnno, not member(FA, DefFunctions)],
    func_location_error(Type, Bad, St0, DefFunctions).

check_nifs(Forms, St0) ->
    FAsAnno = [{FA,Anno} || {attribute, Anno, nifs, Args} <- Forms,
                            FA <- Args],
    St1 = case {FAsAnno, St0#lint.load_nif} of
              {[{_,Anno1}|_], false} ->
                  add_warning(Anno1, no_load_nif, St0);
              _ ->
                  St0
          end,
    DefFunctions = gb_sets:subtract(St1#lint.defined, gb_sets:from_list(pseudolocals())),
    Bad = [{FA,Anno} || {FA,Anno} <- FAsAnno, not gb_sets:is_element(FA, DefFunctions)],
    DefFunctions1 = gb_sets:to_list(DefFunctions),
    func_location_error(undefined_nif, Bad, St1, DefFunctions1).

check_unexported_functions(#lint{callbacks=Cs,
                                 optional_callbacks=OCs,
                                 exports=Es0}=St) ->
    Es = case Cs =/= #{} orelse OCs =/= #{} of
            true -> gb_sets:add({behaviour_info, 1}, Es0);
            false -> Es0
         end,
    maps:fold(fun check_unexported_functions_1/3,
              St#lint{exports=Es},
              St#lint.remote_self_calls).

check_unexported_functions_1({F, A}=Key, Annos, Acc0) ->
    #lint{module=M,exports=Es} = Acc0,
    case not gb_sets:is_element(Key, Es) of
        true ->
            gb_sets:fold(fun(Anno, Acc) ->
                                 add_warning(Anno,
                                             {unexported_function, {M, F, A}},
                                             Acc)
                         end, Acc0, Annos);
        false ->
            Acc0
    end.

nowarn_function(Tag, Opts) ->
    ordsets:from_list([FA || {Tag1,FAs} <- Opts,
                             Tag1 =:= Tag,
                             FA <- lists:flatten([FAs])]).

func_location_warning(Type, Fs, St) ->
    foldl(fun ({F,Anno}, St0) -> add_warning(Anno, {Type,F}, St0) end, St, Fs).

func_location_error(Type, [{F,Anno}|Fs], St0, FAList) ->
    {Name, Arity} = F,
    PossibleAs = lists:sort([A || {FName, A} <:- FAList, FName =:= Name]),
    case PossibleAs of
        [] ->
            PossibleFs = [atom_to_list(Func) ||
                             {Func, A} <:- FAList, A =:= Arity],
            St1 = case most_possible_string(Name, PossibleFs) of
                      [] ->
                          add_error(Anno, {Type,F}, St0);
                      GuessF ->
                          add_error(Anno, {Type,F,{GuessF,[Arity]}}, St0)
                  end,
            func_location_error(Type, Fs, St1, FAList);
        _ ->
            St1 = add_error(Anno, {Type,F,{Name,PossibleAs}}, St0),
            func_location_error(Type, Fs, St1, FAList)
    end;
func_location_error(_, [], St, _) ->
    St.

check_untyped_records(Forms, St0) ->
    case is_warn_enabled(untyped_record, St0) of
	true ->
	    %% Use the names of all records *defined* in the module (not used)
	    RecNames = maps:keys(St0#lint.records),
	    %% these are the records with field(s) containing type info
	    TRecNames = [Name ||
			    {attribute,_,record,{Name,Fields}} <- Forms,
			    lists:all(fun ({typed_record_field,_,_}) -> true;
					  (_) -> false
				      end, Fields)],
	    foldl(fun (N, St) ->
			  {Anno, Fields} = map_get(N, St0#lint.records),
			  case Fields of
			      [] -> St; % exclude records with no fields
			      [_|_] -> add_warning(Anno, {untyped_record, N}, St)
			  end
		  end, St0, ordsets:subtract(ordsets:from_list(RecNames),
                                             ordsets:from_list(TRecNames)));
	false ->
	    St0
    end.

check_unused_records(Forms, St0) ->
    AttrFiles = [File || {attribute,_A,file,{File,_Line}} <- Forms],
    case {is_warn_enabled(unused_record, St0),AttrFiles} of
        {true,[FirstFile|_]} ->
            %% There are no line numbers in St0#lint.compile.
            RecAnno = [{Rec, Anno} ||
                          {attribute, Anno, compile, Args} <- Forms,
                          {nowarn_unused_record, Recs0} <- lists:flatten([Args]),
                          Rec <- lists:flatten([Recs0])],
            St1 = foldl(fun ({Rec, Anno}, St2) ->
                                exist_record(Anno, Rec, St2)
                        end, St0, RecAnno),
            %% The check is a bit imprecise in that uses from unused
            %% functions count.
            Usage = St1#lint.usage,
            UsedRecords = Usage#usage.used_records,
            URecs = gb_sets:fold(fun (Used, Recs) ->
                                         maps:remove(Used, Recs)
                                 end, St1#lint.records, UsedRecords),
            Unused = [{Name,Anno} ||
                         Name := {Anno,_Fields} <- URecs,
                         element(1, loc(Anno, St1)) =:= FirstFile],
            foldl(fun ({N,Anno}, St) ->
                          add_warning(Anno, {unused_record, N}, St)
                  end, St1, Unused);
        _ ->
            St0
    end.

check_callback_information(#lint{callbacks = Callbacks,
                                 optional_callbacks = OptionalCbs,
				 defined = Defined} = St0) ->
    OptFun = fun(MFA, Anno, St) ->
                     case is_map_key(MFA, Callbacks) of
                         true ->
                             St;
                         false ->
                             add_error(Anno, {undefined_callback, MFA}, St)
                     end
             end,
    St1 = maps:fold(OptFun, St0, OptionalCbs),
    case gb_sets:is_member({behaviour_info, 1}, Defined) of
	false -> St1;
	true ->
	    case map_size(Callbacks) of
		0 -> St1;
		_ ->
                    FoldFun =
			fun(Fa, Anno, St) ->
				add_error(Anno, {behaviour_info, Fa}, St)
			end,
                    maps:fold(FoldFun, St1, Callbacks)
	    end
    end.

%% For storing the import list we use the orddict module.
%% We know an empty set is [].

-spec export(anno(), [fa()], lint_state()) -> lint_state().
%%  Mark functions as exported, also as called from the export line.

export(Anno, Es, #lint{exports = Es0, called = Called} = St0) ->
    {Es1,C1,St1} =
        foldl(fun (NA, {E,C,St2}) ->
                      St = case gb_sets:is_element(NA, E) of
                               true ->
                                   Warn = {duplicated_export,NA},
                                   add_warning(Anno, Warn, St2);
                               false ->
                                   St2
                           end,
                      {gb_sets:add_element(NA, E), [{NA,Anno}|C], St}
              end,
              {Es0,Called,St0}, Es),
    St1#lint{exports = Es1, called = C1}.

-spec export_type(anno(), [ta()], lint_state()) -> lint_state().
%%  Mark types as exported; also mark them as used from the export line.

export_type(Anno, ETs, #lint{exp_types = ETs0} = St0) ->
    try foldl(fun ({T,A}=TA, {E,St2}) when is_atom(T), is_integer(A) ->
		      St = case gb_sets:is_element(TA, E) of
			       true ->
				   Warn = {duplicated_export_type,TA},
				   add_warning(Anno, Warn, St2);
			       false ->
                                   St3 = St2#lint{type_id = {export, []}},
                                   used_type(TA, Anno, St3)
			   end,
		      {gb_sets:add_element(TA, E), St}
	      end,
	      {ETs0,St0}, ETs) of
	{ETs1,St1} ->
	    St1#lint{exp_types = ETs1}
    catch
	error:_ ->
	    add_error(Anno, {bad_export_type, ETs}, St0)
    end.

-spec exports(lint_state()) -> gb_sets:set(fa()).

exports(#lint{compile = Opts, defined = Defs, exports = Es}) ->
    case lists:member(export_all, Opts) of
        true -> Defs;
        false -> Es
    end.

-type import() :: {module(), [fa()]} | module().
-spec import(anno(), import(), lint_state()) -> lint_state().

import(Anno, {Mod,Fs}, St00) ->
    St = check_module_name(Mod, Anno, St00),
    Mfs = ordsets:from_list(Fs),
    case check_imports(Anno, Mfs, St#lint.imports) of
	[] ->
	    St#lint{imports=add_imports(Mod, Mfs,
					St#lint.imports)};
	Efs ->
	    {Err, St1} =
		foldl(fun ({bif,{F,A},_}, {Err,St0}) ->
			      %% BifClash - import directive
			      Warn = is_warn_enabled(bif_clash, St0) andalso
				  (not bif_clash_specifically_disabled(St0,{F,A})),
			      AutoImpSup = is_autoimport_suppressed(St0#lint.no_auto,{F,A}),
			      {Err,if
				       Warn and (not AutoImpSup) ->
					   add_warning
					     (Anno,
					      {redefine_bif_import, {F,A}},
					      St0);
				       true ->
					   St0
				   end};
			  (Ef, {_Err,St0}) ->
			      {true,add_error(Anno,
					      {redefine_import,Ef},
					      St0)}
		      end,
		      {false,St}, Efs),
	    if
		not Err ->
		    St1#lint{imports=add_imports(Mod, Mfs,
						 St#lint.imports)};
		true ->
		    St1
	    end
    end.

check_imports(_Anno, Fs, Is) ->
    foldl(fun (F, Efs) ->
              case orddict:find(F, Is) of
                  {ok,Mod} -> [{F,Mod}|Efs];
                  error ->
                      {N,A} = F,
                      case erl_internal:bif(N, A) of
                          true ->
                              [{bif,F,erlang}|Efs];
                          false ->
                              Efs
                      end
              end end, [], Fs).

add_imports(Mod, Fs, Is) ->
    foldl(fun (F, Is0) -> orddict:store(F, Mod, Is0) end, Is, Fs).

-spec imported(atom(), arity(), lint_state()) -> {'yes',module()} | 'no'.

imported(F, A, St) ->
    case orddict:find({F,A}, St#lint.imports) of
        {ok,Mod} -> {yes,Mod};
        error -> no
    end.

-spec on_load(erl_anno:anno(), fa(), lint_state()) -> lint_state().
%%  Check an on_load directive and remember it.

on_load(Anno, {Name,Arity}=Fa, #lint{on_load=OnLoad0}=St0)
  when is_atom(Name), is_integer(Arity) ->
    %% Always add the function name (even if there is a problem),
    %% to avoid irrelevant warnings for unused functions.
    St = St0#lint{on_load=[Fa|OnLoad0],on_load_anno=Anno},
    case St of
	#lint{on_load=[{_,0}]} ->
	    %% This is the first on_load attribute seen in the module
	    %% and it has the correct arity.
	    St;
	#lint{on_load=[{_,_}]} ->
	    %% Wrong arity.
	    add_error(Anno, {bad_on_load_arity,Fa}, St);
	#lint{on_load=[_,_|_]} ->
	    %% Multiple on_load attributes.
	    add_error(Anno, multiple_on_loads, St)
    end;
on_load(Anno, Val, St) ->
    %% Bad syntax.
    add_error(Anno, {bad_on_load,Val}, St).

check_on_load(#lint{defined=Defined,on_load=[{_,0}=Fa],
		    on_load_anno=Anno}=St) ->
    case gb_sets:is_member(Fa, Defined) of
        true -> St;
        false ->
            DefFunctions = gb_sets:to_list(Defined),
            {Name, _} = Fa,
            PossibleFs = [atom_to_list(F) || {F, 0} <- DefFunctions],
            case most_possible_string(Name, PossibleFs) of
                [] -> add_error(Anno, {undefined_on_load,Fa}, St);
                GuessF -> add_error(Anno, {undefined_on_load,Fa,GuessF}, St)
            end
    end;
check_on_load(St) -> St.

-spec call_function(anno(), atom(), arity(), lint_state()) -> lint_state().
%%  Add to both called and calls.

call_function(Anno0, F, A, #lint{usage=Usage0,called=Cd,func=Func,file=File}=St) ->
    #usage{calls = Cs} = Usage0,
    NA = {F,A},
    Usage = case Cs of
		undefined -> Usage0;
		_ -> Usage0#usage{calls=maps_prepend(Func, NA, Cs)}
	    end,
    Anno = erl_anno:set_file(File, Anno0),
    St#lint{called=[{NA,Anno}|Cd], usage=Usage}.

%% function(Anno, Name, Arity, Clauses, State) -> State.

function(Anno, Name, Arity, Cs, St0) ->
    St1 = St0#lint{func={Name,Arity}},
    St2 = define_function(Anno, Name, Arity, St1),
    clauses(Cs, St2).

-spec define_function(anno(), atom(), arity(), lint_state()) -> lint_state().

define_function(Anno, Name, Arity, St0) ->
    St1 = keyword_warning(Anno, Name, St0),
    NA = {Name,Arity},
    case gb_sets:is_member(NA, St1#lint.defined) of
        true ->
            add_error(Anno, {redefine_function,NA}, St1);
        false ->
	    St2 = function_check_max_args(Anno, Arity, St1),
            St3 = St2#lint{defined=gb_sets:add_element(NA, St2#lint.defined)},
            case imported(Name, Arity, St3) of
                {yes,_M} -> add_error(Anno, {define_import,NA}, St3);
                no -> St3
            end
    end.

function_check_max_args(Anno, Arity, St) when Arity > ?MAX_ARGUMENTS ->
    add_error(Anno, {too_many_arguments,Arity}, St);
function_check_max_args(_, _, St) -> St.

%% clauses([Clause], State) -> {VarTable, State}.

clauses(Cs, St) ->
    foldl(fun (C, St0) ->
                  {_,St1} = clause(C, St0),
                  St1
          end, St, Cs).

clause({clause,_Anno,H,G,B}, St0) ->
    Vt0 = [],
    {Hvt,Hnew,St1} = head(H, Vt0, St0),
    Vt1 = vtupdate(Hvt, vtupdate(Hnew, Vt0)),
    {Gvt,St2} = guard(G, Vt1, St1),
    Vt2 = vtupdate(Gvt, Vt1),
    {Bvt,St3} = exprs(B, Vt2, St2),
    Upd = vtupdate(Bvt, Vt2),
    check_unused_vars(Upd, Vt0, St3).

%% head([HeadPattern], VarTable, State) ->
%%      {VarTable,NewVars,State}
%%  Check a patterns in head returning "all" variables. Not updating the
%%  known variable list will result in multiple error messages/warnings.

head(Ps, Vt, St0) ->
    head(Ps, Vt, Vt, St0).    % Old = Vt

head([P|Ps], Vt0, Old, St0) ->
    {Pvt, Pnew, St1} = pattern(P, Vt0, Old, St0),
    {Psvt, Psnew, St2} = head(Ps, Vt0, Old, St1),
    {Vt, St3} = vtmerge_pat(Pvt, Psvt, St2),
    {New, St4} = vtmerge_pat(Pnew, Psnew, St3),
    {Vt, New, St4};
head([], _Vt, _Env, St) -> {[],[],St}.

%% pattern(Pattern, VarTable, Old, State) ->
%%                  {UpdVarTable,NewVars,State}.
%%  Check pattern return variables. VarTable is the set of variables
%%  outside the pattern, used for map keys and binary sizes. Old is empty
%%  if vars are shadowing, as in fun heads and comprehension generators, or
%%  is otherwise equal to VarTable; this is used for deciding whether an
%%  occurrence is a binding occurrence or a use.
%%  UpdVarTable is updated when a previously existing variable is used in
%%  the pattern. New variables and their uses are recorded in NewVars. The
%%  caller can then decide what to do with it depending on whether or not
%%  variables in the pattern shadow old variables. This separation is
%%  one way of dealing with these:
%%  A = 4, fun(<<A:A>>) -> % A #2 unused
%%  A = 4, fun(<<A:8,16:A>>) -> % A #1 unused

pattern(P, Vt, St) ->
    pattern(P, Vt, Vt, St).    % Old = Vt

pattern({var,_Anno,'_'}, _Vt, _Old, St) ->
    {[],[],St}; %Ignore anonymous variable
pattern({var,Anno,V}, _Vt, Old, St) ->
    pat_var(V, Anno, Old, [], St);
pattern({char,_Anno,_C}, _Vt, _Old, St) -> {[],[],St};
pattern({integer,_Anno,_I}, _Vt, _Old, St) -> {[],[],St};
pattern({float,Anno,F}, _Vt, _Old, St0) ->
    St = if
             F == 0 ->
                 maybe_add_warning(Anno, match_float_zero, St0);
             true ->
                 St0
         end,
    {[], [], St};
pattern({atom,Anno,A}, _Vt, _Old, St) ->
    {[],[],keyword_warning(Anno, A, St)};
pattern({string,_Anno,_S}, _Vt, _Old, St) -> {[],[],St};
pattern({nil,_Anno}, _Vt, _Old, St) -> {[],[],St};
pattern({cons,_Anno,H,T}, Vt0, Old, St0) ->
    {Hvt, Hnew, St1} = pattern(H, Vt0, Old, St0),
    {Tvt, Tnew, St2} = pattern(T, Vt0, Old, St1),
    {Vt1, St3} = vtmerge_pat(Hvt, Tvt, St2),
    {New, St4} = vtmerge_pat(Hnew, Tnew, St3),
    {Vt1, New, St4};
pattern({tuple,_Anno,Ps}, Vt, Old, St) ->
    pattern_list(Ps, Vt, Old, St);
pattern({map,_Anno,Ps}, Vt, Old, St) ->
    pattern_map(Ps, Vt, Old, St);
pattern({record_index,Anno,Name,Field}, _Vt, _Old, St) ->
    {Vt1,St1} =
        check_record(Anno, Name, St,
                     fun (Dfs, St1) ->
                             pattern_field(Field, Name, Dfs, St1)
                     end),
    {Vt1,[],St1};
pattern({record,Anno,Name,Pfs}, Vt, Old, St) ->
    case maps:find(Name, St#lint.records) of
        {ok,{_Anno,Fields}} ->
            St1 = used_record(Name, St),
            St2 = check_multi_field_init(Pfs, Anno, Fields, St1),
            pattern_fields(Pfs, Name, Fields, Vt, Old, St2);
        error ->
            DefRecords = [atom_to_list(R) || R <- maps:keys(St#lint.records)],
            case most_possible_string(Name, DefRecords) of
                [] -> {[],[],add_error(Anno, {undefined_record,Name}, St)};
                GuessF -> {[],[],add_error(Anno, {undefined_record,Name,GuessF}, St)}
            end
    end;
pattern({bin,_,Fs}, Vt, Old, St) ->
    pattern_bin(Fs, Vt, Old, St);
pattern({op,_Anno,'++',{nil,_},R}, Vt, Old, St) ->
    pattern(R, Vt, Old, St);
pattern({op,_Anno,'++',{cons,Ai,{char,_A2,_C},T},R}, Vt, Old, St) ->
    pattern({op,Ai,'++',T,R}, Vt, Old, St);    %Char unimportant here
pattern({op,_Anno,'++',{cons,Ai,{integer,_A2,_I},T},R}, Vt, Old, St) ->
    pattern({op,Ai,'++',T,R}, Vt, Old, St);    %Weird, but compatible!
pattern({op,_Anno,'++',{string,_Ai,_S},R}, Vt, Old, St) ->
    pattern(R, Vt, Old, St);                   %String unimportant here
pattern({match,_Anno,Pat1,Pat2}, Vt0, Old, St0) ->
    {Lvt, Lnew, St1} = pattern(Pat1, Vt0, Old, St0),
    {Rvt, Rnew, St2} = pattern(Pat2, Vt0, Old, St1),
    {Vt1, St3} = vtmerge_pat(Lvt, Rvt, St2),
    {New, St4} = vtmerge_pat(Lnew, Rnew, St3),
    {Vt1, New, St4};
%% Catch legal constant expressions, including unary +,-.
pattern(Pat, _Vt, _Old, St) ->
    case is_pattern_expr(Pat) of
        true -> {[],[],St};
        false -> {[],[],add_error(element(2, Pat), illegal_pattern, St)}
    end.

pattern_list(Ps, Vt0, Old, St) ->
    foldl(fun (P, {Psvt, Psnew,St0}) ->
                  {Pvt, Pnew, St1} = pattern(P, Vt0, Old, St0),
                  {Vt1, St2} = vtmerge_pat(Pvt, Psvt, St1),
                  {New, St3} = vtmerge_pat(Psnew, Pnew, St2),
                  {Vt1, New, St3}
          end, {[],[],St}, Ps).

%% Check for '_' initializing no fields.
check_multi_field_init(Fs, Anno, Fields, St) ->
    case init_fields(Fs, Anno, Fields) =:= [] of
        true ->
            case has_wildcard_field(Fs) of
                no -> St;
                WildAnno -> add_error(WildAnno, bad_multi_field_init, St)
            end;
        false -> St
    end.

%% is_pattern_expr(Expression) -> boolean().
%%  Test if a general expression is a valid pattern expression.

-doc false.
is_pattern_expr(Expr) ->
    case is_pattern_expr_1(Expr) of
	false -> false;
	true ->
	    %% Expression is syntactically correct - make sure that it
	    %% also can be evaluated.
	    case erl_eval:partial_eval(Expr) of
		{integer,_,_} -> true;
		{char,_,_} -> true;
		{float,_,_} -> true;
		{atom,_,_} -> true;
		_ -> false
	    end
    end.

is_pattern_expr_1({char,_Anno,_C}) -> true;
is_pattern_expr_1({integer,_Anno,_I}) -> true;
is_pattern_expr_1({float,_Anno,_F}) -> true;
is_pattern_expr_1({atom,_Anno,_A}) -> true;
is_pattern_expr_1({tuple,_Anno,Es}) ->
    all(fun is_pattern_expr_1/1, Es);
is_pattern_expr_1({nil,_Anno}) -> true;
is_pattern_expr_1({cons,_Anno,H,T}) ->
    is_pattern_expr_1(H) andalso is_pattern_expr_1(T);
is_pattern_expr_1({op,_Anno,Op,A}) ->
    erl_internal:arith_op(Op, 1) andalso is_pattern_expr_1(A);
is_pattern_expr_1({op,_Anno,Op,A1,A2}) ->
    erl_internal:arith_op(Op, 2) andalso all(fun is_pattern_expr_1/1, [A1,A2]);
is_pattern_expr_1(_Other) -> false.

pattern_map(Ps, Vt0, Old, St0) ->
    foldl(fun({map_field_assoc,A,_,_}, {Psvt,Psnew,St1}) ->
                  {Psvt,Psnew,add_error(A, illegal_map_assoc_in_pattern, St1)};
             ({map_field_exact,_A,K,V}, {Psvt,Psnew,St1}) ->
                  St2 = St1#lint{gexpr_context=map_key},
                  {Kvt, St3} = gexpr(K, Vt0, St2),
                  {Vvt, Vnew, St4} = pattern(V, Vt0, Old, St3),
                  {Vt1, St5} = vtmerge_pat(Kvt, Vvt, St4),
                  {Vt2, St6} = vtmerge_pat(Vt1, Psvt, St5),
                  {New, St7} = vtmerge_pat(Psnew, Vnew, St6),
                  {Vt2, New, St7}
          end, {[],[], St0}, Ps).

%% pattern_bin([Element], VarTable, Old, State) ->
%%           {UpdVarTable,NewVars,State}.
%%  Check a pattern group.

pattern_bin(Es, Vt, Old, St0) ->
    {_,Esvt,Esnew,St1} = foldl(fun (E, Acc) ->
                                       pattern_element(E, Vt, Old, Acc)
                               end, {{0,0},[],[],St0}, Es),
    {Esvt,Esnew,St1}.

pattern_element({bin_element,Anno,{string,_,_},Size,Ts}=Be, Vt,
		Old, {Sz,Esvt,Esnew,St0}=Acc) ->
    case good_string_size_type(Size, Ts) of
	true ->
	    pattern_element_1(Be, Vt, Old, Acc);
	false ->
	    St = add_error(Anno, typed_literal_string, St0),
	    {Sz,Esvt,Esnew,St}
    end;
pattern_element(Be, Vt, Old, Acc) ->
    pattern_element_1(Be, Vt, Old, Acc).

pattern_element_1({bin_element,Anno,E,Sz0,Ts}, Vt, Old,
                  {{PrevSize,PrevAnno},Esvt,Esnew,St0}) ->
    {Pevt,Penew,St1} = pat_bit_expr(E, Old, Esnew, St0),
    {Sz1,Szvt,Sznew,St2} = pat_bit_size(Sz0, Vt, Esnew, St1),
    {Sz2,Bt,St3} = bit_type(Anno, Sz1, Ts, St2),
    {Sz3,St4} = bit_size_check(Anno, Sz2, Bt, St3),
    Sz4 = case {E,Sz3} of
	      {{string,_,S},all} -> 8*length(S);
	      {_,_} -> Sz3
	  end,
    St5 = case PrevSize of
              all -> add_error(PrevAnno, unsized_binary_not_at_end, St4);
              _ -> St4
          end,
    {{Sz4,Anno},vtmerge(Szvt,vtmerge(Pevt, Esvt)),
     vtmerge(Sznew,vtmerge(Esnew, Penew)), St5}.

good_string_size_type(default, default) ->
    true;
good_string_size_type(default, Ts) ->
    lists:any(fun(utf8) -> true;
		 (utf16) -> true;
		 (utf32) -> true;
		 (_) -> false
	      end, Ts);
good_string_size_type(_, _) -> false.

%% pat_bit_expr(Pattern, OldVarTable, NewVars, State) ->
%%              {UpdVarTable,UpdNewVars,State}.
%%  Check pattern bit expression, only allow really valid patterns!

pat_bit_expr({var,_,'_'}, _Old, _New, St) -> {[],[],St};
pat_bit_expr({var,Anno,V}, Old, New, St) -> pat_var(V, Anno, Old, New, St);
pat_bit_expr({string,_,_}, _Old, _new, St) -> {[],[],St};
pat_bit_expr({bin,A,_}, _Old, _New, St) ->
    {[],[],add_error(A, illegal_pattern, St)};
pat_bit_expr(P, _Old, _New, St) ->
    case is_pattern_expr(P) of
        true -> {[],[],St};
        false -> {[],[],add_error(element(2, P), illegal_pattern, St)}
    end.

%% pat_bit_size(Size, VarTable, NewVars, State) ->
%%             {Value,UpdVarTable,UpdNewVars,State}.
%%  Check pattern size expression, only allow really valid sizes!

pat_bit_size(default, _Vt, _New, St) -> {default,[],[],St};
pat_bit_size({var,Anno,V}, Vt0, New0, St0) ->
    {Vt,New,St1} = pat_binsize_var(V, Anno, Vt0, New0, St0),
    {unknown,Vt,New,St1};
pat_bit_size(Size, Vt0, New0, St0) ->
    Anno = element(2, Size),
    case erl_eval:partial_eval(Size) of
        {integer,Anno,I} -> {I,[],[],St0};
        Expr ->
            %% The size is an expression using operators
            %% and/or guard BIFs calls. If the expression
            %% happens to evaluate to a non-integer value, the
            %% pattern will fail to match.
            St1 = St0#lint{bvt=New0,gexpr_context=bin_seg_size},
            {Vt,#lint{bvt=New}=St2} = gexpr(Size, Vt0, St1),
            St3 = St2#lint{bvt=none,gexpr_context=St0#lint.gexpr_context},
            St = case is_bit_size_illegal(Expr) of
                     true ->
                         %% The size is a non-integer literal or a simple
                         %% expression that does not evaluate to an
                         %% integer value. Issue a warning.
                         add_warning(Anno, non_integer_bitsize, St3);
                     false -> St3
                 end,
            {unknown,Vt,New,St}
    end.

is_bit_size_illegal({atom,_,_}) -> true;
is_bit_size_illegal({bin,_,_}) -> true;
is_bit_size_illegal({cons,_,_,_}) -> true;
is_bit_size_illegal({float,_,_}) -> true;
is_bit_size_illegal({map,_,_}) -> true;
is_bit_size_illegal({nil,_}) -> true;
is_bit_size_illegal({tuple,_,_}) -> true;
is_bit_size_illegal(_) -> false.

%% expr_bin([Element], VarTable, State, CheckFun) -> {UpdVarTable,State}.
%%  Check an expression group.

expr_bin(Es, Vt, St0, Check) ->
    {Esvt,St1} = foldl(fun (E, Acc) ->
                               bin_element(E, Vt, Acc, Check)
                       end, {[],St0}, Es),
    {Esvt,St1}.

bin_element({bin_element,Anno,E,Sz0,Ts}, Vt, {Esvt,St0}, Check) ->
    {Vt1,St1} = Check(E, Vt, St0),
    {Sz1,Vt2,St2} = bit_size(Sz0, Vt, St1, Check),
    {Sz2,Bt,St3} = bit_type(Anno, Sz1, Ts, St2),
    {_Sz3,St4} = bit_size_check(Anno, Sz2, Bt, St3),
    {vtmerge([Vt2,Vt1,Esvt]),St4}.

bit_size(default, _Vt, St, _Check) -> {default,[],St};
bit_size({atom,_Anno,all}, _Vt, St, _Check) -> {all,[],St};
bit_size(Size, Vt, St, Check) ->
    %% Try to safely evaluate Size if constant to get size,
    %% otherwise just treat it as an expression.
    Info = is_guard_test2_info(St),
    case is_gexpr(Size, Info) of
        true ->
            case erl_eval:partial_eval(Size) of
                {integer,_ILn,I} -> {I,[],St};
                _Other ->
                    {Evt,St1} = Check(Size, Vt, St),
                    {unknown,Evt,St1}
            end;
        false ->
            {Evt,St1} = Check(Size, Vt, St),
            {unknown,Evt,St1}
    end.

%% bit_type(Anno, Size, TypeList, State) ->  {Size,#bittype,St}.
%%  Perform warning check on type and size.

bit_type(Anno, Size0, Type, St) ->
    case erl_bits:set_bit_type(Size0, Type) of
        {ok,Size1,Bt} -> {Size1,Bt,St};
	{error,What} ->
            %% Flag error and generate a default.
            {ok,Size1,Bt} = erl_bits:set_bit_type(default, []),
            {Size1,Bt,add_error(Anno, What, St)}
    end.

%% bit_size_check(Anno, Size, BitType, State) -> {BitSize,State}.
%%  Do some checking & warnings on types
%%   float == 16 or 32 or 64

bit_size_check(_Anno, unknown, _, St) -> {unknown,St};
bit_size_check(_Anno, undefined, #bittype{type=Type}, St) ->
    true = (Type =:= utf8) or (Type =:= utf16) or (Type =:= utf32), %Assertion.
    {undefined,St};
bit_size_check(Anno, all, #bittype{type=Type}, St) ->
    case Type of
        binary -> {all,St};
        _ -> {unknown,add_error(Anno, illegal_bitsize, St)}
    end;
bit_size_check(Anno, Size, #bittype{type=Type,unit=Unit}, St)
  when is_integer(Size), is_integer(Unit) ->
    Sz = Unit * Size,                           %Total number of bits!
    St2 = elemtype_check(Anno, Type, Sz, St),
    {Sz,St2}.

elemtype_check(_Anno, float, 16, St) -> St;
elemtype_check(_Anno, float, 32, St) -> St;
elemtype_check(_Anno, float, 64, St) -> St;
elemtype_check(Anno, float, _Size, St) ->
    add_warning(Anno, {bad_bitsize,"float"}, St);
elemtype_check(_Anno, _Type, _Size, St) ->  St.


%% guard([GuardTest], VarTable, State) ->
%%      {UsedVarTable,State}
%%  Check a guard, return all variables.

%% Disjunction of guard conjunctions
guard([L|R], Vt, St0) when is_list(L) ->
    {Gvt, St1} = guard_tests(L, Vt, St0),
    {Gsvt, St2} = guard(R, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt),St2};
guard(L, Vt, St0) ->
    guard_tests(L, Vt, St0).

%% guard conjunction
guard_tests([G|Gs], Vt, St0) ->
    {Gvt,St1} = guard_test(G, Vt, St0),
    {Gsvt,St2} = guard_tests(Gs, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt),St2};
guard_tests([], _Vt, St) -> {[],St}.

%% guard_test(Test, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check one guard test, returns NewVariables.  We now allow more
%%  expressions in guards including the new is_XXX type tests, but
%%  only allow the old type tests at the top level.

guard_test(G, Vt, St0) ->
    St1 = obsolete_guard(G, St0),
    guard_test2(G, Vt, St1).

%% Specially handle record type test here.
guard_test2({call,Anno,{atom,Ar,record},[E,A]}, Vt, St0) ->
    gexpr({call,Anno,{atom,Ar,is_record},[E,A]}, Vt, St0);
guard_test2({call,Anno,{atom,_Aa,F},As}=G, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),       %Always check this.
    A = length(As),
    case erl_internal:type_test(F, A) of
        true when F =/= is_record, A =/= 2 ->
	    case no_guard_bif_clash(St1, {F,A}) of
		false ->
		    {Asvt,add_error(Anno, {illegal_guard_local_call,{F,A}}, St1)};
		true ->
		    {Asvt,St1}
	    end;
	_ ->
	    gexpr(G, Vt, St0)
    end;
guard_test2(G, Vt, St) ->
    %% Everything else is a guard expression.
    gexpr(G, Vt, St).

%% gexpr(GuardExpression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a guard expression, returns NewVariables.

gexpr({var,Anno,V}, Vt, St) ->
    expr_var(V, Anno, Vt, St);
gexpr({char,_Anno,_C}, _Vt, St) -> {[],St};
gexpr({integer,_Anno,_I}, _Vt, St) -> {[],St};
gexpr({float,_Anno,_F}, _Vt, St) -> {[],St};
gexpr({atom,Anno,A}, _Vt, St) ->
    {[],keyword_warning(Anno, A, St)};
gexpr({string,_Anno,_S}, _Vt, St) -> {[],St};
gexpr({nil,_Anno}, _Vt, St) -> {[],St};
gexpr({cons,_Anno,H,T}, Vt, St) ->
    gexpr_list([H,T], Vt, St);
gexpr({tuple,_Anno,Es}, Vt, St) ->
    gexpr_list(Es, Vt, St);
gexpr({map,_Anno,Es}, Vt, St) ->
    map_fields(Es, Vt, check_assoc_fields(Es, St), fun gexpr_list/3);
gexpr({map,_Anno,Src,Es}, Vt, St) ->
    {Svt,St1} = gexpr(Src, Vt, St),
    {Fvt,St2} = map_fields(Es, Vt, St1, fun gexpr_list/3),
    {vtmerge(Svt, Fvt),St2};
gexpr({record_index,Anno,Name,Field}, _Vt, St) ->
    check_record(Anno, Name, St,
                 fun (Dfs, St1) -> record_field(Field, Name, Dfs, St1) end );
gexpr({record_field,Anno,Rec,Name,Field}, Vt, St0) ->
    {Rvt,St1} = gexpr(Rec, Vt, St0),
    {Fvt,St2} = check_record(Anno, Name, St1,
                             fun (Dfs, St) ->
                                     record_field(Field, Name, Dfs, St)
                             end),
    {vtmerge(Rvt, Fvt),St2};
gexpr({record,Anno,Name,Inits}, Vt, St) ->
    check_record(Anno, Name, St,
                 fun (Dfs, St1) ->
                         ginit_fields(Inits, Anno, Name, Dfs, Vt, St1)
                 end);
gexpr({bin,_Anno,Fs}, Vt,St) ->
    expr_bin(Fs, Vt, St, fun gexpr/3);
gexpr({call,_Anno,{atom,_Ar,is_record},[E,{atom,An,Name}]}, Vt, St0) ->
    {Rvt,St1} = gexpr(E, Vt, St0),
    {Rvt,exist_record(An, Name, St1)};
gexpr({call,Anno,{atom,_Ar,is_record},[E,R]}, Vt, St0) ->
    {Asvt,St1} = gexpr_list([E,R], Vt, St0),
    {Asvt,add_error(Anno, illegal_guard_expr, St1)};
gexpr({call,Anno,{remote,_Ar,{atom,_Am,erlang},{atom,Af,is_record}},[E,A]},
      Vt, St0) ->
    gexpr({call,Anno,{atom,Af,is_record},[E,A]}, Vt, St0);
gexpr({call,Anno,{atom,_Ar,is_record},[E0,{atom,_,_Name},{integer,_,_}]},
      Vt, St0) ->
    {E,St1} = gexpr(E0, Vt, St0),
    case no_guard_bif_clash(St0, {is_record,3}) of
	true ->
	    {E,St1};
	false ->
	    {E,add_error(Anno, {illegal_guard_local_call,{is_record,3}}, St1)}
    end;
gexpr({call,Anno,{atom,_Ar,is_record},[_,_,_]=Asvt0}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(Asvt0, Vt, St0),
    {Asvt,add_error(Anno, illegal_guard_expr, St1)};
gexpr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,is_record}=Isr},[_,_,_]=Args},
      Vt, St0) ->
    gexpr({call,Anno,Isr,Args}, Vt, St0);
gexpr({call,Anno,{atom,_Aa,F},As}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    %% BifClash - Function called in guard
    case erl_internal:guard_bif(F, A) andalso no_guard_bif_clash(St1,{F,A}) of
        true ->
	    %% Assert that it is auto-imported.
	    true = erl_internal:bif(F, A),
	    {Asvt,St1};
        false ->
	    case is_local_function(St1#lint.locals,{F,A}) orelse
		is_imported_function(St1#lint.imports,{F,A}) of
		true ->
		    {Asvt,add_error(Anno, {illegal_guard_local_call,{F,A}}, St1)};
		_ ->
		    {Asvt,add_error(Anno, illegal_guard_expr, St1)}
	    end
    end;
gexpr({call,Anno,{remote,_Ar,{atom,_Am,erlang},{atom,_Af,F}},As}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:guard_bif(F, A) orelse is_gexpr_op(F, A) of
        true -> {Asvt,St1};
        false -> {Asvt,add_error(Anno, illegal_guard_expr, St1)}
    end;
gexpr({op,Anno,Op,A}, Vt, St0) ->
    {Avt,St1} = gexpr(A, Vt, St0),
    case is_gexpr_op(Op, 1) of
        true -> {Avt,St1};
        false -> {Avt,add_error(Anno, illegal_guard_expr, St1)}
    end;
gexpr({op,_,'andalso',L,R}, Vt, St) ->
    gexpr_list([L,R], Vt, St);
gexpr({op,_,'orelse',L,R}, Vt, St) ->
    gexpr_list([L,R], Vt, St);
gexpr({op,_Anno,EqOp,L,R}, Vt, St0) when EqOp =:= '=:='; EqOp =:= '=/=' ->
    St1 = expr_check_match_zero(R, expr_check_match_zero(L, St0)),
    gexpr_list([L,R], Vt, St1);
gexpr({op,Anno,Op,L,R}, Vt, St0) ->
    {Avt,St1} = gexpr_list([L,R], Vt, St0),
    case is_gexpr_op(Op, 2) of
        true -> {Avt,St1};
        false -> {Avt,add_error(Anno, illegal_guard_expr, St1)}
    end;
%% Everything else is illegal! You could put explicit tests here to
%% better error diagnostics.
gexpr(E, _Vt, St) ->
    {[],add_error(element(2, E), illegal_guard_expr, St)}.

%% gexpr_list(Expressions, VarTable, State) ->
%%      {UsedVarTable,State'}

gexpr_list(Es, Vt, St) ->
    foldl(fun (E, {Esvt,St0}) ->
                  {Evt,St1} = gexpr(E, Vt, St0),
                  {vtmerge(Evt, Esvt),St1}
          end, {[],St}, Es).

%% is_guard_test(Expression) -> boolean().
%%  Test if a general expression is a guard test.
%%
%%  Note: Only use this function in contexts where there can be
%%  no definition of a local function that may override a guard BIF
%%  (for example, in the shell).
-doc """
Tests if `Expr` is a legal guard test. `Expr` is an Erlang term representing the
abstract form for the expression.
[`erl_parse:parse_exprs(Tokens)`](`erl_parse:parse_exprs/1`) can be used to
generate a list of `Expr`.
""".
-spec is_guard_test(Expr) -> boolean() when
      Expr :: erl_parse:abstract_expr().

is_guard_test(E) ->
    is_guard_test2(E, {maps:new(),fun(_) -> false end}).

%% is_guard_test(Expression, Forms) -> boolean().
-doc false.
is_guard_test(Expression, Forms) ->
    is_guard_test(Expression, Forms, fun(_) -> false end).


%% is_guard_test(Expression, Forms, IsOverridden) -> boolean().
%%  Test if a general expression is a guard test.
%%
%%  IsOverridden({Name,Arity}) should return 'true' if Name/Arity is
%%  a local or imported function in the module. If the abstract code has
%%  passed through erl_expand_records, any call without an explicit
%%  module is to a local function, so IsOverridden can be defined as:
%%
%%    fun(_) -> true end
%%
-doc false.
-spec is_guard_test(Expr, Forms, IsOverridden) -> boolean() when
      Expr :: erl_parse:abstract_expr(),
      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      IsOverridden :: fun((fa()) -> boolean()).

is_guard_test(Expression, Forms, IsOverridden) ->
    NoFileExpression = set_file(Expression, "nofile"),

    %% Unless the expression constructs a record, the record
    %% definitions are not needed. Therefore, because there can be a
    %% huge number of record definitions in the forms, delay
    %% processing the forms until we'll know that the record
    %% definitions are truly needed.
    F = fun() ->
                #{Name => {A,Fs} ||
                    {attribute, A, record, {Name, Fs}} <- Forms}
        end,

    is_guard_test2(NoFileExpression, {F,IsOverridden}).

%% is_guard_test2(Expression, RecordDefs :: dict:dict()) -> boolean().
is_guard_test2({call,Anno,{atom,Ar,record},[E,A]}, Info) ->
    is_gexpr({call,Anno,{atom,Ar,is_record},[E,A]}, Info);
is_guard_test2({call,_Anno,{atom,_Aa,Test},As}=Call, {_,IsOverridden}=Info) ->
    A = length(As),
    not IsOverridden({Test,A}) andalso
	case erl_internal:type_test(Test, A) of
	    true -> is_gexpr_list(As, Info);
	    false -> is_gexpr(Call, Info)
	end;
is_guard_test2(G, Info) ->
    %%Everything else is a guard expression.
    is_gexpr(G, Info).

%% is_guard_expr(Expression) -> boolean().
%%  Test if an expression is a guard expression.

-doc false.
is_guard_expr(E) -> is_gexpr(E, {[],fun({_,_}) -> false end}).

is_gexpr({var,_A,_V}, _Info) -> true;
is_gexpr({char,_A,_C}, _Info) -> true;
is_gexpr({integer,_A,_I}, _Info) -> true;
is_gexpr({float,_A,_F}, _Info) -> true;
is_gexpr({atom,_A,_Atom}, _Info) -> true;
is_gexpr({string,_A,_S}, _Info) -> true;
is_gexpr({nil,_A}, _Info) -> true;
is_gexpr({cons,_A,H,T}, Info) -> is_gexpr_list([H,T], Info);
is_gexpr({tuple,_A,Es}, Info) -> is_gexpr_list(Es, Info);
is_gexpr({map,_A,Es}, Info) ->
    is_map_fields(Es, Info);
is_gexpr({map,_A,Src,Es}, Info) ->
    is_gexpr(Src, Info) andalso is_map_fields(Es, Info);
is_gexpr({record_index,_A,_Name,Field}, Info) ->
    is_gexpr(Field, Info);
is_gexpr({record_field,_A,Rec,_Name,Field}, Info) ->
    is_gexpr_list([Rec,Field], Info);
is_gexpr({record,A,Name,Inits}, Info0) ->
    Info = case Info0 of
               {#{},_} ->
                   Info0;
               {F,IsOverridden} when is_function(F, 0) ->
                   {F(),IsOverridden}
           end,
    is_gexpr_fields(Inits, A, Name, Info);
is_gexpr({bin,_A,Fs}, Info) ->
    all(fun ({bin_element,_Anno,E,Sz,_Ts}) ->
                is_gexpr(E, Info) and (Sz =:= default orelse is_gexpr(Sz, Info))
        end, Fs);
is_gexpr({call,_A,{atom,_Af,F},As}, {_,IsOverridden}=Info) ->
    A = length(As),
    not IsOverridden({F,A}) andalso erl_internal:guard_bif(F, A)
	andalso is_gexpr_list(As, Info);
is_gexpr({call,_A,{remote,_Ar,{atom,_Am,erlang},{atom,_Af,F}},As}, Info) ->
    A = length(As),
    (erl_internal:guard_bif(F, A) orelse is_gexpr_op(F, A))
        andalso is_gexpr_list(As, Info);
is_gexpr({call,A,{tuple,At,[{atom,Am,erlang},{atom,Af,F}]},As}, Info) ->
    is_gexpr({call,A,{remote,At,{atom,Am,erlang},{atom,Af,F}},As}, Info);
is_gexpr({op,_A,Op,A}, Info) ->
    is_gexpr_op(Op, 1) andalso is_gexpr(A, Info);
is_gexpr({op,_A,'andalso',A1,A2}, Info) ->
    is_gexpr_list([A1,A2], Info);
is_gexpr({op,_A,'orelse',A1,A2}, Info) ->
    is_gexpr_list([A1,A2], Info);
is_gexpr({op,_A,Op,A1,A2}, Info) ->
    is_gexpr_op(Op, 2) andalso is_gexpr_list([A1,A2], Info);
is_gexpr(_Other, _Info) -> false.

is_gexpr_op(Op, A) ->
    try erl_internal:op_type(Op, A) of
        arith -> true;
        bool  -> true;
        comp  -> true;
	list  -> false;
	send  -> false
    catch _:_ -> false
    end.

is_gexpr_list(Es, Info) -> all(fun (E) -> is_gexpr(E, Info) end, Es).

is_map_fields([{Tag,_,K,V}|Fs], Info) when Tag =:= map_field_assoc;
                                           Tag =:= map_field_exact ->
    is_gexpr(K, Info) andalso
    is_gexpr(V, Info) andalso
    is_map_fields(Fs, Info);
is_map_fields([], _Info) -> true;
is_map_fields(_T, _Info) -> false.

is_gexpr_fields(Fs, A, Name, {RDs,_}=Info) ->
    IFs = case maps:find(Name, RDs) of
              {ok,{_Anno,Fields}} -> Fs ++ init_fields(Fs, A, Fields);
              error  -> Fs
          end,
    all(fun ({record_field,_Af,_Name,V}) -> is_gexpr(V, Info);
            (_Other) -> false end, IFs).

%% exprs(Sequence, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a sequence of expressions, return all variables.

exprs([E|Es], Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Esvt,St2} = exprs(Es, vtupdate(Evt, Vt), St1),
    {vtupdate(Evt, Esvt),St2};
exprs([], _Vt, St) -> {[],St}.

%% expr(Expression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check an expression, returns NewVariables. Assume naive users and
%%  mark illegally exported variables, e.g. from catch, as unsafe to better
%%  show why unbound.

expr({var,Anno,V}, Vt, St) ->
    expr_var(V, Anno, Vt, St);
expr({char,_Anno,_C}, _Vt, St) -> {[],St};
expr({integer,_Anno,_I}, _Vt, St) -> {[],St};
expr({float,_Anno,_F}, _Vt, St) -> {[],St};
expr({atom,Anno,A}, _Vt, St) ->
    {[],keyword_warning(Anno, A, St)};
expr({string,_Anno,_S}, _Vt, St) -> {[],St};
expr({nil,_Anno}, _Vt, St) -> {[],St};
expr({cons,_Anno,H,T}, Vt, St) ->
    expr_list([H,T], Vt, St);
expr({lc,_Anno,E,Qs}, Vt, St) ->
    handle_comprehension(E, Qs, Vt, St);
expr({bc,_Anno,E,Qs}, Vt, St) ->
    handle_comprehension(E, Qs, Vt, St);
expr({mc,_Anno,E,Qs}, Vt, St) ->
    handle_comprehension(E, Qs, Vt, St);
expr({tuple,_Anno,Es}, Vt, St) ->
    expr_list(Es, Vt, St);
expr({map,_Anno,Es}, Vt, St) ->
    map_fields(Es, Vt, check_assoc_fields(Es, St), fun expr_list/3);
expr({map,Anno,Src,Es}, Vt, St) ->
    {Svt,St1} = expr(Src, Vt, St),
    {Fvt,St2} = map_fields(Es, Vt, St1, fun expr_list/3),
    {vtupdate(Svt, Fvt), warn_if_literal_update(Anno, Src, St2)};
expr({record_index,Anno,Name,Field}, _Vt, St) ->
    check_record(Anno, Name, St,
                 fun (Dfs, St1) -> record_field(Field, Name, Dfs, St1) end);
expr({record,Anno,Name,Inits}, Vt, St) ->
    check_record(Anno, Name, St,
                 fun (Dfs, St1) ->
                         init_fields(Inits, Anno, Name, Dfs, Vt, St1)
                 end);
expr({record_field,Anno,Rec,Name,Field}, Vt, St0) ->
    {Rvt,St1} = record_expr(Anno, Rec, Vt, St0),
    {Fvt,St2} = check_record(Anno, Name, St1,
                             fun (Dfs, St) ->
                                     record_field(Field, Name, Dfs, St)
                             end),
    {vtmerge(Rvt, Fvt),St2};
expr({record,Anno,Rec,Name,Upds}, Vt, St0) ->
    {Rvt,St1} = record_expr(Anno, Rec, Vt, St0),
    {Usvt,St2} = check_record(Anno, Name, St1,
                          fun (Dfs, St) ->
                                  update_fields(Upds, Name, Dfs, Vt, St)
                          end ),
    case has_wildcard_field(Upds) of
        no -> {vtmerge(Rvt, Usvt), warn_if_literal_update(Anno, Rec, St2)};
        WildAnno -> {[],add_error(WildAnno, {wildcard_in_update,Name}, St2)}
    end;
expr({bin,_Anno,Fs}, Vt, St) ->
    expr_bin(Fs, Vt, St, fun expr/3);
expr({block,_Anno,Es}, Vt, St) ->
    %% Unfold block into a sequence.
    exprs(Es, Vt, St);
expr({'if',Anno,Cs}, Vt, St) ->
    icrt_clauses(Cs, {'if',Anno}, Vt, St);
expr({'case',Anno,E,Cs}, Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Cvt,St2} = icrt_clauses(Cs, {'case',Anno}, vtupdate(Evt, Vt), St1),
    {vtmerge(Evt, Cvt),St2};
expr({'receive',Anno,Cs}, Vt, St) ->
    icrt_clauses(Cs, {'receive',Anno}, Vt, St);
expr({'receive',Anno,Cs,To,ToEs}, Vt, St0) ->
    %% Are variables from the timeout expression visible in the clauses? NO!
    {Tvt,St1} = expr(To, Vt, St0),
    {Tevt,St2} = exprs(ToEs, Vt, St1),
    {Cvt,St3} = icrt_clauses(Cs, Vt, St2),
    %% Csvts = [vtnew(Tevt, Vt)|Cvt],           %This is just NEW variables!
    Csvts = [Tevt|Cvt],
    Rvt = icrt_export(Csvts, Vt, {'receive',Anno}, St3),
    {vtmerge([Tvt,Tevt,Rvt]),St3};
expr({'fun',Anno,Body}, Vt, St) ->
    %%No one can think funs export!
    case Body of
        {clauses,Cs} ->
            fun_clauses(Cs, Vt, St);
        {function,record_info,2} ->
            %% It is illegal to call record_info/2 with unknown arguments.
            {[],add_error(Anno, illegal_record_info, St)};
        {function,F,A} ->
            St1 = case is_imported_function(St#lint.imports,{F,A}) of
                      true ->
                          add_error(Anno, {fun_import,{F,A}}, St);
                      false ->
                          %% check function use like for a call
                          As = lists:duplicate(A, undefined), % dummy args
                          check_call(Anno, F, As, Anno, St)
                  end,
            %% do not mark as used as a local function if listed as
            %% imported (either auto-imported or explicitly)
            case not is_local_function(St1#lint.locals,{F,A}) andalso
                (is_imported_function(St1#lint.imports,{F,A})
                 orelse
                   (erl_internal:bif(F, A) andalso
                    not is_autoimport_suppressed(St1#lint.no_auto,{F,A}))) of
                true -> {[],St1};
                false -> {[],call_function(Anno, F, A, St1)}
            end;
        {function, {atom, _, M}, {atom, _, F}, {integer, _, A}} ->
            {[], check_remote_self_call(Anno, M, F, A, St)};
        {function,M,F,A} ->
            expr_list([M,F,A], Vt, St)
    end;
expr({named_fun,_,'_',Cs}, Vt, St) ->
    fun_clauses(Cs, Vt, St);
expr({named_fun,Anno,Name,Cs}, Vt, St0) ->
    Nvt0 = [{Name,{bound,unused,[Anno]}}],
    St1 = shadow_vars(Nvt0, Vt, 'named fun', St0),
    Nvt1 = vtupdate(vtsubtract(Vt, Nvt0), Nvt0),
    {Csvt,St2} = fun_clauses(Cs, Nvt1, St1),
    {_,St3} = check_unused_vars(vtupdate(Csvt, Nvt0), [], St2),
    {vtold(Csvt, Vt),St3};
expr({call,_Anno,{atom,_Ar,is_record},[E,{atom,An,Name}]}, Vt, St0) ->
    {Rvt,St1} = expr(E, Vt, St0),
    {Rvt,exist_record(An, Name, St1)};
expr({call,Anno,{remote,_Ar,{atom,_Am,erlang},{atom,Af,is_record}},[E,A]},
      Vt, St0) ->
    expr({call,Anno,{atom,Af,is_record},[E,A]}, Vt, St0);
expr({call,A,{tuple,At,[{atom,Am,erlang},{atom,Af,is_record}]},As}, Vt, St) ->
    expr({call,A,{remote,At,{atom,Am,erlang},{atom,Af,is_record}},As}, Vt, St);
expr({call,Anno,{remote,_Ar,{atom,_Am,M},{atom,Af,F}},As}, Vt, St0) ->
    St1 = keyword_warning(Af, F, St0),
    St2 = check_remote_function(Anno, M, F, As, St1),
    St3 = check_module_name(M, Anno, St2),
    St4 = check_remote_self_call(Anno, M, F, length(As), St3),
    expr_list(As, Vt, St4);
expr({call,Anno,{remote,_Ar,M,F},As}, Vt, St0) ->
    St1 = keyword_warning(Anno, M, St0),
    St2 = keyword_warning(Anno, F, St1),
    St3 = case M of
              {atom,Am,Mod} ->
                  check_module_name(Mod, Am, St2);
              _ ->
                  St2
          end,
    expr_list([M,F|As], Vt, St3);
expr({call,Anno,{atom,Aa,F},As}, Vt, St0) ->
    St1 = keyword_warning(Aa, F, St0),
    {Asvt,St2} = expr_list(As, Vt, St1),
    {Asvt, check_call(Anno, F, As, Aa, St2)};
expr({call,Anno,F,As}, Vt, St0) ->
    St = warn_invalid_call(Anno,F,St0),
    expr_list([F|As], Vt, St);                  %They see the same variables
expr({'try',Anno,Es,Scs,Ccs,As}, Vt, St0) ->
    %% The only exports we allow are from the try expressions to the
    %% success clauses.
    {Evt0,St1} = exprs(Es, Vt, St0),
    TryAnno = {'try',Anno},
    Uvt = vtunsafe(TryAnno, Evt0, Vt),
    {Sccs,St2} = try_clauses(Scs, Ccs, TryAnno,
                             vtupdate(Evt0, Vt), Uvt, St1),
    Evt1 = vtupdate(Uvt, Evt0),
    Rvt0 = Sccs,
    Rvt1 = vtupdate(vtunsafe(TryAnno, Rvt0, Vt), Rvt0),
    Evt2 = vtmerge(Evt1, Rvt1),
    {Avt0,St} = exprs(As, vtupdate(Evt2, Vt), St2),
    Avt1 = vtupdate(vtunsafe(TryAnno, Avt0, Vt), Avt0),
    Avt = vtmerge(Evt2, Avt1),
    {Avt,St};
expr({'catch',Anno,E}, Vt, St0) ->
    %% No new variables added, flag new variables as unsafe.
    {Evt,St} = expr(E, Vt, St0),
    St1 = case is_warn_enabled(deprecated_catch, St) of
              true -> add_warning(Anno, deprecated_catch, St);
              false -> St
          end,
    {vtupdate(vtunsafe({'catch',Anno}, Evt, Vt), Evt),St1};
expr({match,_Anno,P,E}, Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Pvt,Pnew,St} = pattern(P, vtupdate(Evt, Vt), St1),
    {vtupdate(Pnew, vtmerge(Evt, Pvt)),St};
expr({maybe_match,Anno,P,E}, Vt, St0) ->
    expr({match,Anno,P,E}, Vt, St0);
expr({'maybe',Anno,Es}, Vt, St) ->
    %% No variables are exported.
    {Evt0, St1} = exprs(Es, Vt, St),
    Evt1 = vtupdate(vtunsafe({'maybe',Anno}, Evt0, Vt), Vt),
    Evt2 = vtmerge(Evt0, Evt1),
    {Evt2,St1};
expr({'maybe',MaybeAnno,Es,{'else',ElseAnno,Cs}}, Vt, St) ->
    %% No variables are exported.
    {Evt0, St1} = exprs(Es, Vt, St),
    Evt1 = vtupdate(vtunsafe({'maybe',MaybeAnno}, Evt0, Vt), Vt),
    {Cvt0, St2} = icrt_clauses(Cs, {'else',ElseAnno}, Evt1, St1),
    Cvt1 = vtupdate(vtunsafe({'else',ElseAnno}, Cvt0, Vt), Vt),
    Evt2 = vtmerge(Evt0, Evt1),
    Cvt2 = vtmerge(Cvt0, Cvt1),
    {vtmerge(Evt2, Cvt2),St2};
%% No comparison or boolean operators yet.
expr({op,_Anno,_Op,A}, Vt, St) ->
    expr(A, Vt, St);
expr({op,Anno,Op,L,R}, Vt, St0) when Op =:= 'orelse'; Op =:= 'andalso' ->
    {Evt1,St1} = expr(L, Vt, St0),
    Vt1 = vtupdate(Evt1, Vt),
    {Evt2,St2} = expr(R, Vt1, St1),
    Evt3 = vtupdate(vtunsafe({Op,Anno}, Evt2, Vt1), Evt2),
    {vtmerge(Evt1, Evt3),St2};
expr({op,_Anno,EqOp,L,R}, Vt, St0) when EqOp =:= '=:='; EqOp =:= '=/=' ->
    St = expr_check_match_zero(R, expr_check_match_zero(L, St0)),
    expr_list([L,R], Vt, St);                   %They see the same variables
expr({op,_Anno,_Op,L,R}, Vt, St) ->
    expr_list([L,R], Vt, St);                   %They see the same variables
%% The following are not allowed to occur anywhere!
expr({remote,_Anno,M,_F}, _Vt, St) ->
    {[],add_error(erl_parse:first_anno(M), illegal_expr, St)};
expr({executable_line,_,_}, _Vt, St) ->
    {[], St};
expr({ssa_check_when,_Anno,_WantedResult,_Args,_Tag,_Exprs}, _Vt, St) ->
    {[], St}.

%% Check a call to function without a module name. This can be a call
%% to a BIF or a local function.
check_call(Anno, record_info, [_,_]=As, Aa, St0) ->
    check_record_info_call(Anno, Aa, As, St0);
check_call(Anno, F, As, _Aa, St0) ->
    A = length(As),
    case imported(F, A, St0) of
        {yes,M} ->
            St = check_remote_function(Anno, M, F, As, St0),
            U0 = St#lint.usage,
            Imp = ordsets:add_element({{F,A},M}, U0#usage.imported),
            St#lint{usage=U0#usage{imported = Imp}};
        no ->
            IsLocal = is_local_function(St0#lint.locals, {F,A}),
            IsAutoBif = erl_internal:bif(F, A),
            AutoSuppressed = is_autoimport_suppressed(St0#lint.no_auto, {F,A}),
            if
                not IsLocal andalso IsAutoBif andalso not AutoSuppressed ->
                    %% This is is remote call to erlang:F/A. Check whether
                    %% this function is deprecated.
                    deprecated_function(Anno, erlang, F, As, St0);
                true ->
                    FA = {F,A},
                    %% Clash between a local function and a BIF.
                    %% Issue these diagnostics even for recursive calls...
                    St = maybe
                             true ?= IsAutoBif,
                             true ?= IsLocal,
                             false ?= AutoSuppressed,
                             true ?= is_warn_enabled(bif_clash, St0),
                             false ?= bif_clash_specifically_disabled(St0, {F,A}),
                             add_warning(Anno, {call_to_redefined_bif, {F,A}}, St0)
                         else
                             _ ->
                                 St0
                         end,
                    %% ...but don't lint recursive calls.
                    if
                        FA =:= St#lint.func ->
                            St;
                        true ->
                            call_function(Anno, F, A, St)
                    end
            end
    end.

%% Checks whether 0.0 occurs naked in the LHS or RHS of an equality check. Note
%% that we do not warn when it's being used as arguments for expressions in
%% in general: `A =:= abs(0.0)` is fine.
expr_check_match_zero({float,Anno,F}, St) ->
    if
        F == 0 ->
            maybe_add_warning(Anno, match_float_zero, St);
        true ->
            St
    end;
expr_check_match_zero({cons,_Anno,H,T}, St) ->
    expr_check_match_zero(H, expr_check_match_zero(T, St));
expr_check_match_zero({tuple,_Anno,Es}, St) ->
    foldl(fun expr_check_match_zero/2, St, Es);
expr_check_match_zero(_Expr, St) ->
    St.

%% expr_list(Expressions, Variables, State) ->
%%      {UsedVarTable,State}

expr_list(Es, Vt, St0) ->
    foldl(fun (E, {Esvt, St1}) ->
                  {Evt, St2} = expr(E, Vt, St1),
                  vtmerge_pat(Evt, Esvt, St2)
          end, {[], St0}, Es).

record_expr(Anno, Rec, Vt, St0) ->
    St1 = warn_invalid_record(Anno, Rec, St0),
    expr(Rec, Vt, St1).

check_assoc_fields([{map_field_exact,Anno,_,_}|Fs], St) ->
    check_assoc_fields(Fs, add_error(Anno, illegal_map_construction, St));
check_assoc_fields([{map_field_assoc,_,_,_}|Fs], St) ->
    check_assoc_fields(Fs, St);
check_assoc_fields([], St) ->
    St.

map_fields([{Tag,_,K,V}|Fs], Vt, St, F) when Tag =:= map_field_assoc;
                                             Tag =:= map_field_exact ->
    {Pvt,St2} = F([K,V], Vt, St),
    {Vts,St3} = map_fields(Fs, Vt, St2, F),
    {vtupdate(Pvt, Vts),St3};
map_fields([], _, St, _) ->
    {[],St}.

%% warn_invalid_record(Anno, Record, State0) -> State
%% Adds warning if the record is invalid.

warn_invalid_record(Anno, R, St) ->
    case is_valid_record(R) of
        true -> St;
        false -> add_warning(Anno, invalid_record, St)
    end.

%% is_valid_record(Record) -> boolean().

is_valid_record(Rec) ->
    case Rec of
        {char, _, _} -> false;
        {integer, _, _} -> false;
        {float, _, _} -> false;
        {atom, _, _} -> false;
        {string, _, _} -> false;
        {cons, _, _, _} -> false;
        {nil, _} -> false;
        {lc, _, _, _} -> false;
        {record_index, _, _, _} -> false;
        {'fun', _, _} -> false;
        {named_fun, _, _, _} -> false;
        _ -> true
    end.

%% warn_invalid_call(Anno, Call, State0) -> State
%% Adds warning if the call is invalid.

warn_invalid_call(Anno, F, St) ->
    case is_valid_call(F) of
        true -> St;
        false -> add_warning(Anno, invalid_call, St)
    end.

%% is_valid_call(Call) -> boolean().

is_valid_call(Call) ->
    case Call of
        {char, _, _} -> false;
        {integer, _, _} -> false;
        {float, _, _} -> false;
        {string, _, _} -> false;
        {cons, _, _, _} -> false;
        {nil, _} -> false;
        {lc, _, _, _} -> false;
        {record_index, _, _, _} -> false;
        {tuple, _, Exprs} when length(Exprs) =/= 2 -> false;
        _ -> true
    end.

%% Raises a warning if we're remote-calling an unexported function (or
%% referencing it with `fun M:F/A`), as this is likely to be unintentional.
check_remote_self_call(Anno, M, F, A,
                       #lint{module=M,
                             compile=Opts,
                             exports=Es,
                             remote_self_calls=Rsc0} = St) ->
    case (is_warn_enabled(unexported_function, St)
          andalso (not lists:member(export_all, Opts))
          andalso (not gb_sets:is_element({F, A}, Es))) of
        true ->
            Locs0 = maps:get({F, A}, Rsc0, gb_sets:empty()),
            Locs = gb_sets:add_element(Anno, Locs0),
            Rsc = Rsc0#{ {F, A} => Locs },
            St#lint{remote_self_calls=Rsc};
        false ->
            St
    end;
check_remote_self_call(_Anno, _M, _F, _A, St) ->
    St.

%% record_def(Anno, RecordName, [RecField], State) -> State.
%%  Add a record definition if it does not already exist. Normalise
%%  so that all fields have explicit initial value.

record_def(Anno, Name, Fs0, St0) ->
    case is_map_key(Name, St0#lint.records) of
        true -> add_error(Anno, {redefine_record,Name}, St0);
        false ->
            {Fs1,St1} = def_fields(normalise_fields(Fs0), Name, St0),
            St2 = St1#lint{records=maps:put(Name, {Anno,Fs1},
                                            St1#lint.records)},
            Types = [T || {typed_record_field, _, T} <- Fs0],
            St3 = St2#lint{type_id = {record, Name}},
            check_type({type, nowarn(), product, Types}, St3)
    end.

%% def_fields([RecDef], RecordName, State) -> {[DefField],State}.
%%  Check (normalised) fields for duplicates.  Return unduplicated
%%  record and set State.

def_fields(Fs0, Name, St0) ->
    foldl(fun ({record_field,Af,{atom,Aa,F},V}, {Fs,St}) ->
                  case exist_field(F, Fs) of
                      true -> {Fs,add_error(Af, {redefine_field,Name,F}, St)};
                      false ->
                          St1 = St#lint{recdef_top = true},
                          {_,St2} = expr(V, [], St1),
                          %% Warnings and errors found are kept, but
                          %% updated calls, records, etc. are discarded.
                          St3 = St1#lint{warnings = St2#lint.warnings,
                                         errors = St2#lint.errors,
                                         called = St2#lint.called,
                                         recdef_top = false},
                          %% This is one way of avoiding a loop for
                          %% "recursive" definitions.
                          NV = case St2#lint.errors =:= St1#lint.errors of
                                   true -> V;
                                   false -> {atom,Aa,undefined}
                               end,
                          {[{record_field,Af,{atom,Aa,F},NV}|Fs],St3}
                  end
          end, {[],St0}, Fs0).

%% normalise_fields([RecDef]) -> [Field].
%%  Normalise the field definitions to always have a default value. If
%%  none has been given then use 'undefined'.
%%  Also, strip type information from typed record fields.

normalise_fields(Fs) ->
    map(fun ({record_field,Af,Field}) ->
		{record_field,Af,Field,{atom,Af,undefined}};
	    ({typed_record_field,{record_field,Af,Field},_Type}) ->
		{record_field,Af,Field,{atom,Af,undefined}};
	    ({typed_record_field,Field,_Type}) ->
		Field;
            (F) -> F end, Fs).

%% exist_record(Anno, RecordName, State) -> State.
%%  Check if a record exists.  Set State.

exist_record(Anno, Name, St) ->
    case is_map_key(Name, St#lint.records) of
        true -> used_record(Name, St);
        false ->
            RecordNames = [atom_to_list(R) || R <- maps:keys(St#lint.records)],
            case most_possible_string(Name, RecordNames) of
                [] -> add_error(Anno, {undefined_record,Name}, St);
                GuessF -> add_error(Anno, {undefined_record,Name,GuessF}, St)
            end
    end.

%% check_record(Anno, RecordName, State, CheckFun) ->
%%      {UpdVarTable, State}.
%%  The generic record checking function, first checks that the record
%%  exists then calls the specific check function.  N.B. the check
%%  function can safely assume that the record exists.
%%
%%  The check function is called:
%%      CheckFun(RecordDefFields, State)
%%  and must return
%%      {UpdatedVarTable,State}

check_record(Anno, Name, St, CheckFun) ->
    case maps:find(Name, St#lint.records) of
        {ok,{_Anno,Fields}} -> CheckFun(Fields, used_record(Name, St));
        error ->
            RecordNames = [atom_to_list(R) || R <- maps:keys(St#lint.records)],
            case most_possible_string(Name, RecordNames) of
                [] -> {[],add_error(Anno, {undefined_record,Name}, St)};
                GuessF -> {[],add_error(Anno, {undefined_record,Name,GuessF}, St)}
            end
    end.

used_record(Name, #lint{usage=Usage}=St) ->
    UsedRecs = gb_sets:add_element(Name, Usage#usage.used_records),
    St#lint{usage = Usage#usage{used_records=UsedRecs}}.

%%% Record check functions.

%% check_fields([ChkField], RecordName, [RecDefField], VarTable, State, CheckFun) ->
%%      {UpdVarTable,State}.

check_fields(Fs, Name, Fields, Vt0, St0, CheckFun) ->
    {_SeenFields,Uvt,St1} =
        foldl(fun (Field, {Sfsa,Vta,Sta}) ->
                      {Sfsb,{Vtb,Stb}} = check_field(Field, Name, Fields,
                                                     Vt0, Sta, Sfsa, CheckFun),
                      {Vt1, St1} = vtmerge_pat(Vta, Vtb, Stb),
                      {Sfsb, Vt1, St1}
              end, {[],[], St0}, Fs),
    {Uvt,St1}.

check_field({record_field,Af,{atom,Aa,F},Val}, Name, Fields,
            Vt, St, Sfs, CheckFun) ->
    case member(F, Sfs) of
        true -> {Sfs,{[],add_error(Af, {redefine_field,Name,F}, St)}};
        false ->
            {[F|Sfs],
             case find_field(F, Fields) of
                 {ok,_I} -> CheckFun(Val, Vt, St);
                 error ->
                     FieldNames = [atom_to_list(R) || {record_field, _L, {_, _, R}, _} <- Fields],
                     case most_possible_string(F, FieldNames) of
                         [] -> {[],add_error(Aa, {undefined_field,Name,F}, St)};
                         GuessF -> {[],add_error(Aa, {undefined_field,Name,F,GuessF}, St)}
                     end
             end}
    end;
check_field({record_field,_Af,{var,Aa,'_'=F},Val}, _Name, _Fields,
            Vt, St, Sfs, CheckFun) ->
    case member(F, Sfs) of
        true -> {Sfs,{[],add_error(Aa, bad_multi_field_init, St)}};
        false -> {[F|Sfs],CheckFun(Val, Vt, St)}
    end;
check_field({record_field,_Af,{var,Aa,V},_Val}, Name, _Fields,
            Vt, St, Sfs, _CheckFun) ->
    {Sfs,{Vt,add_error(Aa, {field_name_is_variable,Name,V}, St)}}.

%% pattern_field(Field, RecordName, [RecDefField], State) ->
%%      {UpdVarTable,State}.
%%  Test if record RecordName has field Field. Set State.

pattern_field({atom,Aa,F}, Name, Fields, St) ->
    case find_field(F, Fields) of
        {ok,_I} -> {[],St};
        error ->
            FieldNames = [atom_to_list(R) || {record_field, _L, {_, _, R}, _} <- Fields],
            case most_possible_string(F, FieldNames) of
                [] -> {[],add_error(Aa, {undefined_field,Name,F}, St)};
                GuessF -> {[],add_error(Aa, {undefined_field,Name,F,GuessF}, St)}
            end
    end.

%% pattern_fields([PatField],RecordName,[RecDefField],
%%                VarTable,Old,State) ->
%%      {UpdVarTable,NewVars,State}.

pattern_fields(Fs, Name, Fields, Vt0, Old, St0) ->
    CheckFun = fun (Val, Vt, St) -> pattern(Val, Vt, Old, St) end,
    {_SeenFields,Uvt,Unew,St1} =
        foldl(fun (Field, {Sfsa,Vta,Newa,Sta}) ->
                      case check_field(Field, Name, Fields,
                                       Vt0, Sta, Sfsa, CheckFun) of
                          {Sfsb,{Vtb,Stb}} ->
                              {Vt, St1} = vtmerge_pat(Vta, Vtb, Stb),
                              {Sfsb, Vt, [], St1};
                          {Sfsb,{Vtb,Newb,Stb}} ->
                              {Vt, Mst0} = vtmerge_pat(Vta, Vtb, Stb),
                              {New, Mst} = vtmerge_pat(Newa, Newb, Mst0),
                              {Sfsb, Vt, New, Mst}
                      end
              end, {[],[],[],St0}, Fs),
    {Uvt,Unew,St1}.

%% record_field(Field, RecordName, [RecDefField], State) ->
%%      {UpdVarTable,State}.
%%  Test if record RecordName has field Field. Set State.

record_field({atom,Aa,F}, Name, Fields, St) ->
    case find_field(F, Fields) of
        {ok,_I} -> {[],St};
        error ->
            FieldNames = [atom_to_list(R) || {record_field, _L, {_, _, R}, _} <- Fields],
            case most_possible_string(F, FieldNames) of
                [] -> {[],add_error(Aa, {undefined_field,Name,F}, St)};
                GuessF -> {[],add_error(Aa, {undefined_field,Name,F,GuessF}, St)}
            end
    end.

%% init_fields([InitField], InitAnno, RecordName, [DefField], VarTable, State) ->
%%      {UpdVarTable,State}.
%% ginit_fields([InitField], InitAnno, RecordName, [DefField], VarTable, State) ->
%%      {UpdVarTable,State}.
%%  Check record initialisation. Explicit initialisations are checked
%%  as is, while default values are checked only if there are no
%%  explicit inititialisations of the fields. Be careful not to
%%  duplicate warnings (and possibly errors, but def_fields
%%  substitutes 'undefined' for bogus inititialisations) from when the
%%  record definitions were checked. Usage of records, imports, and
%%  functions is collected.

init_fields(Ifs, Anno, Name, Dfs, Vt0, St0) ->
    {Vt1,St1} = check_fields(Ifs, Name, Dfs, Vt0, St0, fun expr/3),
    Defs = init_fields(Ifs, Anno, Dfs),
    {_,St2} = check_fields(Defs, Name, Dfs, Vt1, St1, fun expr/3),
    {Vt1,St1#lint{usage = St2#lint.usage}}.

ginit_fields(Ifs, Anno, Name, Dfs, Vt0, St0) ->
    {Vt1,St1} = check_fields(Ifs, Name, Dfs, Vt0, St0, fun gexpr/3),
    Defs = init_fields(Ifs, Anno, Dfs),
    St2 = St1#lint{errors = []},
    {_,St3} = check_fields(Defs, Name, Dfs, Vt1, St2, fun gexpr/3),
    #lint{usage = Usage, errors = IllErrors} = St3,
    St4 = St1#lint{usage = Usage, errors = IllErrors ++ St1#lint.errors},
    {Vt1,St4}.

%% Default initializations to be carried out
init_fields(Ifs, Anno, Dfs) ->
    [ {record_field,Af,{atom,Aa,F},copy_expr(Di, Anno)} ||
        {record_field,Af,{atom,Aa,F},Di} <- Dfs,
        not exist_field(F, Ifs) ].

%% update_fields(UpdFields, RecordName, RecDefFields, VarTable, State) ->
%%      {UpdVarTable,State}

update_fields(Ufs, Name, Dfs, Vt, St) ->
    check_fields(Ufs, Name, Dfs, Vt, St, fun expr/3).

%% exist_field(FieldName, [Field]) -> boolean().
%%  Find a record field in a field list.

exist_field(F, [{record_field,_Af,{atom,_Aa,F},_Val}|_Fs]) -> true;
exist_field(F, [_|Fs]) -> exist_field(F, Fs);
exist_field(_F, []) -> false.

%% find_field(FieldName, [Field]) -> {ok,Val} | error.
%%  Find a record field in a field list.

find_field(F, [{record_field,_Af,{atom,_Aa,F},Val}|_Fs]) -> {ok,Val};
find_field(F, [_|Fs]) -> find_field(F, Fs);
find_field(_F, []) -> error.

%% type_def(Attr, Anno, TypeName, PatField, Args, State) -> State.
%%    Attr :: 'type' | 'opaque'
%% Checks that a type definition is valid.

-dialyzer({no_match, type_def/6}).

type_def(Attr, Anno, TypeName, ProtoType, Args, St0) ->
    TypeDefs = St0#lint.types,
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    Info = #typeinfo{attr = Attr, anno = Anno},
    StoreType =
        fun(St) ->
                NewDefs = maps:put(TypePair, Info, TypeDefs),
                CheckType = {type, nowarn(), product, [ProtoType|Args]},
                St1 = St#lint{types=NewDefs, type_id={type, TypePair}},
                check_type(CheckType, St1)
        end,
    case is_default_type(TypePair) andalso
        not member(no_auto_import_types, St0#lint.compile) of
        true ->
            case is_obsolete_builtin_type(TypePair) of
                true ->
                    StoreType(St0);
                false ->
                    %% Starting from OTP 26, redefining built-in types
                    %% is allowed.
                    St1 = StoreType(St0),
                    warn_redefined_builtin_type(Anno, TypePair, St1)
            end;
        false ->
            case is_map_key(TypePair, TypeDefs) of
                true ->
                    add_error(Anno, {redefine_type, TypePair}, St0);
                false ->
                    StoreType(St0)
	    end
    end.

warn_redefined_builtin_type(Anno, TypePair, #lint{compile=Opts}=St) ->
    case is_warn_enabled(redefined_builtin_type, St) of
        true ->
            NoWarn = [Type ||
                         {nowarn_redefined_builtin_type, Type0} <- Opts,
                         Type <- lists:flatten([Type0])],
            case lists:member(TypePair, NoWarn) of
                true ->
                    St;
                false ->
                    Warn = {redefine_builtin_type, TypePair},
                    add_warning(Anno, Warn, St)
            end;
        false ->
            St
    end.

check_type(Types, St) ->
    {SeenVars, St1} = check_type_1(Types, maps:new(), St),
    maps:fold(fun(Var, {SeenOnce, Anno}, AccSt)
                    when SeenOnce =:= seen_once;
                         SeenOnce =:= seen_once_union ->
                      case atom_to_list(Var) of
                          "_"++_ -> AccSt;
                          _ -> add_error(Anno, {singleton_typevar, Var}, AccSt)
                      end;
                 (_Var, seen_multiple, AccSt) ->
                      AccSt
              end, St1, SeenVars).

check_type_1({type, Anno, TypeName, Args}=Type, SeenVars, #lint{types=Types}=St) ->
    TypePair = {TypeName,
                if
                    is_list(Args) -> length(Args);
                    true -> 0
                end},
    case is_map_key(TypePair, Types) of
        true ->
            check_type_2(Type, SeenVars, used_type(TypePair, Anno, St));
        false ->
            check_type_2(Type, SeenVars, St)
    end;
check_type_1(Types, SeenVars, St) ->
    check_type_2(Types, SeenVars, St).

check_type_2({ann_type, _A, [_Var, Type]}, SeenVars, St) ->
    check_type_1(Type, SeenVars, St);
check_type_2({remote_type, A, [{atom, _, Mod}, {atom, _, Name}, Args]},
	   SeenVars, St00) ->
    St0 = check_module_name(Mod, A, St00),
    St = deprecated_type(A, Mod, Name, Args, St0),
    CurrentMod = St#lint.module,
    case Mod =:= CurrentMod of
	true -> check_type_2({user_type, A, Name, Args}, SeenVars, St);
	false ->
	    lists:foldl(fun(T, {AccSeenVars, AccSt}) ->
				check_type_1(T, AccSeenVars, AccSt)
			end, {SeenVars, St}, Args)
    end;
check_type_2({integer, _A, _}, SeenVars, St) -> {SeenVars, St};
check_type_2({atom, _A, _}, SeenVars, St) -> {SeenVars, St};
check_type_2({var, _A, '_'}, SeenVars, St) -> {SeenVars, St};
check_type_2({var, A, Name}, SeenVars, St) ->
    NewSeenVars =
	case maps:find(Name, SeenVars) of
	    {ok, {seen_once, _}} -> maps:put(Name, seen_multiple, SeenVars);
	    {ok, {seen_once_union, _}} -> maps:put(Name, seen_multiple, SeenVars);
	    {ok, seen_multiple} -> SeenVars;
	    error -> maps:put(Name, {seen_once, A}, SeenVars)
	end,
    {NewSeenVars, St};
check_type_2({type, A, bool, []}, SeenVars, St) ->
    {SeenVars, add_warning(A, {renamed_type, bool, boolean}, St)};
check_type_2({type, A, 'fun', [Dom, Range]}, SeenVars, St) ->
    St1 =
	case Dom of
	    {type, _, product, _} -> St;
	    {type, _, any} -> St;
	    _ -> add_error(A, {type_syntax, 'fun'}, St)
	end,
    check_type_2({type, nowarn(), product, [Dom, Range]}, SeenVars, St1);
check_type_2({type, A, range, [From, To]}, SeenVars, St) ->
    St1 =
	case {erl_eval:partial_eval(From), erl_eval:partial_eval(To)} of
	    {{integer, _, X}, {integer, _, Y}} when X < Y -> St;
	    _ -> add_error(A, {type_syntax, range}, St)
	end,
    {SeenVars, St1};
check_type_2({type, _A, map, any}, SeenVars, St) ->
    {SeenVars, St};
check_type_2({type, _A, map, Pairs}, SeenVars, St) ->
    lists:foldl(fun(Pair, {AccSeenVars, AccSt}) ->
                        check_type_2(Pair, AccSeenVars, AccSt)
		end, {SeenVars, St}, Pairs);
check_type_2({type, _A, map_field_assoc, [Dom, Range]}, SeenVars, St) ->
    check_type_2({type, nowarn(), product, [Dom, Range]}, SeenVars, St);
check_type_2({type, _A, tuple, any}, SeenVars, St) -> {SeenVars, St};
check_type_2({type, _A, any}, SeenVars, St) -> {SeenVars, St};
check_type_2({type, A, binary, [Base, Unit]}, SeenVars, St) ->
    St1 =
	case {erl_eval:partial_eval(Base), erl_eval:partial_eval(Unit)} of
	    {{integer, _, BaseVal},
	     {integer, _, UnitVal}} when BaseVal >= 0, UnitVal >= 0 -> St;
	    _ -> add_error(A, {type_syntax, binary}, St)
	end,
    {SeenVars, St1};
check_type_2({type, A, record, [Name|Fields]}, SeenVars, St) ->
    case Name of
	{atom, _, Atom} ->
	    St1 = used_record(Atom, St),
	    check_record_types(A, Atom, Fields, SeenVars, St1);
	_ -> {SeenVars, add_error(A, {type_syntax, record}, St)}
    end;
check_type_2({type, _A, Tag, Args}=_F, SeenVars, St) when Tag =:= product;
                                                          Tag =:= tuple ->
    lists:foldl(fun(T, {AccSeenVars, AccSt}) ->
                        check_type_1(T, AccSeenVars, AccSt)
                end, {SeenVars, St}, Args);
check_type_2({type, _A, union, Args}=_F, SeenVars0, St) ->
    lists:foldl(fun(T, {AccSeenVars0, AccSt}) ->
                        {SeenVars1, St0} = check_type_1(T, SeenVars0, AccSt),
                        AccSeenVars = maps:merge_with(
                                        fun (K, {seen_once, Anno}, {seen_once, _}) ->
                                                case SeenVars0 of
                                                    #{K := _} ->
                                                        %% Unused outside of this union.
                                                        {seen_once, Anno};
                                                    #{} ->
                                                        {seen_once_union, Anno}
                                                end;
                                            (_K, {seen_once, Anno}, {seen_once_union, _}) ->
                                                {seen_once_union, Anno};
                                            (_K, {seen_once_union, _}=R, {seen_once, _}) -> R;
                                            (_K, {seen_once_union, _}=R, {seen_once_union, _}) -> R;
                                            (_K, {seen_once_union, _}, Else) -> Else;
                                            (_K, {seen_once, _}, Else) -> Else;
                                            (_K, Else, {seen_once_union, _}) -> Else;
                                            (_K, Else, {seen_once, _}) -> Else;
                                            (_K, Else1, _Else2)        -> Else1
                                        end, AccSeenVars0, SeenVars1),
                        {AccSeenVars, St0}
                end, {SeenVars0, St}, Args);
check_type_2({type, Anno, TypeName, Args}, SeenVars, St) ->
    #lint{module = Module, types=Types} = St,
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    Obsolete = (is_warn_enabled(deprecated_type, St)
                andalso obsolete_builtin_type(TypePair)),
    St1 = case Obsolete of
              {deprecated, Repl, _} when element(1, Repl) =/= Module ->
                  case maps:find(TypePair, Types) of
                      {ok, _} ->
                          used_type(TypePair, Anno, St);
                      error ->
                          {deprecated, Replacement, Rel} = Obsolete,
                          Tag = deprecated_builtin_type,
                          W = {Tag, TypePair, Replacement, Rel},
                          add_warning(Anno, W, St)
                  end;
              _ ->
                  case is_default_type(TypePair) of
                      true ->
                          used_type(TypePair, Anno, St);
                      false ->
                          St
                  end
          end,
    check_type_2({type, nowarn(), product, Args}, SeenVars, St1);
check_type_2({user_type, A, TypeName, Args}, SeenVars, St) ->
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    St1 = used_type(TypePair, A, St),
    lists:foldl(fun(T, {AccSeenVars, AccSt}) ->
			check_type_1(T, AccSeenVars, AccSt)
		end, {SeenVars, St1}, Args);
check_type_2([{typed_record_field,Field,_T}|_], SeenVars, St) ->
    {SeenVars, add_error(element(2, Field), old_abstract_code, St)};
check_type_2(I, SeenVars, St) ->
    case erl_eval:partial_eval(I) of
        {integer,_A,_Integer} -> {SeenVars, St};
        _Other ->
            {SeenVars, add_error(element(2, I), {type_syntax, integer}, St)}
    end.

check_record_types(Anno, Name, Fields, SeenVars, St) ->
    case maps:find(Name, St#lint.records) of
        {ok,{_A,DefFields}} ->
	    case lists:all(fun({type, _, field_type, _}) -> true;
			      (_) -> false
			   end, Fields) of
		true ->
		    check_record_types(Fields, Name, DefFields, SeenVars, St, []);
		false ->
		    {SeenVars, add_error(Anno, {type_syntax, record}, St)}
	    end;
        error ->
            RecordNames = [atom_to_list(R) || R <- maps:keys(St#lint.records)],
            case most_possible_string(Name, RecordNames) of
                [] -> {SeenVars, add_error(Anno, {undefined_record, Name}, St)};
                GuessF -> {SeenVars, add_error(Anno, {undefined_record, Name, GuessF}, St)}
            end
    end.

check_record_types([{type, _, field_type, [{atom, Anno, FName}, Type]}|Left],
		   Name, DefFields, SeenVars, St, SeenFields) ->
    %% Check that the field name is valid
    St1 = case exist_field(FName, DefFields) of
              true -> St;
              false ->
                  FieldNames = [atom_to_list(R) || {record_field, _L, {_, _, R}, _} <- DefFields],
                  case most_possible_string(FName, FieldNames) of
                      [] -> add_error(Anno, {undefined_field,Name,FName}, St);
                      GuessF -> add_error(Anno, {undefined_field,Name,FName,GuessF}, St)
                  end
          end,
    %% Check for duplicates
    St2 = case ordsets:is_element(FName, SeenFields) of
	      true -> add_error(Anno, {redefine_field, Name, FName}, St1);
	      false -> St1
	  end,
    %% Check Type
    {NewSeenVars, St3} = check_type_2(Type, SeenVars, St2),
    NewSeenFields = ordsets:add_element(FName, SeenFields),
    check_record_types(Left, Name, DefFields, NewSeenVars, St3, NewSeenFields);
check_record_types([], _Name, _DefFields, SeenVars, St, _SeenFields) ->
    {SeenVars, St}.

used_type(TypePair, Anno, #lint{usage = Usage, file = File} = St) ->
    Used = Usage#usage.used_types,
    UsedType = #used_type{anno = erl_anno:set_file(File, Anno),
                          at = St#lint.type_id},
    NewUsed = maps_prepend(TypePair, UsedType, Used),
    St#lint{usage=Usage#usage{used_types=NewUsed}}.

is_default_type({Name, NumberOfTypeVariables}) ->
    erl_internal:is_type(Name, NumberOfTypeVariables).

is_obsolete_builtin_type(TypePair) ->
    obsolete_builtin_type(TypePair) =/= no.

%% To keep Dialyzer silent...
obsolete_builtin_type({1, 255}) ->
    {deprecated, {2, 255}, ""};
obsolete_builtin_type({Name, A}) when is_atom(Name), is_integer(A) -> no.

%% spec_decl(Anno, Fun, Types, State) -> State.

spec_decl(Anno, MFA0, TypeSpecs, #lint{specs = Specs, module = Mod} = St0) ->
    MFA = case MFA0 of
	      {F, Arity} -> {Mod, F, Arity};
	      {_M, _F, Arity} -> MFA0
	  end,
    St1 = St0#lint{specs = maps:put(MFA, Anno, Specs)},
    case is_map_key(MFA, Specs) of
	true -> add_error(Anno, {redefine_spec, MFA0}, St1);
	false ->
            St2 = case MFA of
                      {Mod, _, _} ->
                          St1;
                      _ ->
                          St1int = case MFA0 of
                                       {M, _, _} ->
                                           check_module_name(M, Anno, St1);
                                       _ ->
                                           St1
                                   end,
                          add_error(Anno, {bad_module, MFA}, St1int)
                  end,
            St3 = St2#lint{type_id = {spec, MFA}},
            check_specs(TypeSpecs, spec_wrong_arity, Arity, St3)
    end.

%% callback_decl(Anno, Fun, Types, State) -> State.

callback_decl(Anno, MFA0, TypeSpecs,
	      St0 = #lint{callbacks = Callbacks, module = Mod}) ->
    case MFA0 of
        {M, _F, _A} ->
            St1 = check_module_name(M, Anno, St0),
            add_error(Anno, {bad_callback, MFA0}, St1);
        {F, Arity} ->
            MFA = {Mod, F, Arity},
            St1 = St0#lint{callbacks = maps:put(MFA, Anno, Callbacks)},
            case is_map_key(MFA, Callbacks) of
                true -> add_error(Anno, {redefine_callback, MFA0}, St1);
                false ->
                    St2 = St1#lint{type_id = {spec, MFA}},
                    check_specs(TypeSpecs, callback_wrong_arity, Arity, St2)
            end
    end.

%% optional_callbacks(Anno, FAs, State) -> State.

optional_callbacks(Anno, Term, St0) ->
    try true = is_fa_list(Term), Term of
        FAs ->
            optional_cbs(Anno, FAs, St0)
    catch
        _:_ ->
            St0 % ignore others
    end.

optional_cbs(_Anno, [], St) ->
    St;
optional_cbs(Anno, [{F,A}|FAs], St0) ->
    #lint{optional_callbacks = OptionalCbs, module = Mod} = St0,
    MFA = {Mod, F, A},
    St1 = St0#lint{optional_callbacks = maps:put(MFA, Anno, OptionalCbs)},
    St2 = case is_map_key(MFA, OptionalCbs) of
              true ->
                  add_error(Anno, {redefine_optional_callback, {F,A}}, St1);
              false ->
                  St1
          end,
    optional_cbs(Anno, FAs, St2).

is_fa_list([E|L]) -> is_fa(E) andalso is_fa_list(L);
is_fa_list([]) -> true;
is_fa_list(_) -> false.

is_fa({FuncName, Arity})
  when is_atom(FuncName), is_integer(Arity), Arity >= 0 -> true;
is_fa(_) -> false.

check_module_name(M, Anno, St0) ->
    AllChars = atom_to_list(M),
    VisibleChars = remove_non_visible(AllChars),
    case {AllChars, VisibleChars} of
        {[], []} ->
            add_error(Anno, empty_module_name, St0);
        {[_|_], []} ->
            add_error(Anno, blank_module_name, St0);
        {Cs,[_|_]} ->
            St1 = case io_lib:latin1_char_list(Cs) of
                      true ->
                          St0;
                      false ->
                          add_error(Anno,
                                    non_latin1_module_unsupported,
                                    St0)
                  end,
            case any_control_characters(Cs) of
                true ->
                    add_error(Anno, ctrl_chars_in_module_name, St1);
                false ->
                    St1
            end
    end.

remove_non_visible(Cs) ->
    SP = $\s,                                   %Plain space.
    NBSP = 16#A0,                               %Non-breaking space.
    SHY = 16#AD,                                %Soft hyphen.
    [C || C <- Cs, C =/= SP, C =/= NBSP, C =/= SHY].

any_control_characters(Cs) ->
    any(fun(C) when is_integer(C), 0 =< C, C < 16#20;
                    is_integer(C), 16#7F =< C, C < 16#A0 -> true;
           (_) -> false
        end, Cs).

check_specs([FunType|Left], ETag, Arity, St0) ->
    {FunType1, CTypes} =
	case FunType of
	    {type, _, bounded_fun, [FT = {type, _, 'fun', _}, Cs]} ->
		Types0 = [T || {type, _, constraint, [_, T]} <- Cs],
		{FT, lists:append(Types0)};
	    {type, _, 'fun', _} = FT -> {FT, []}
	end,
    {type, A, 'fun', [{type, _, product, D}, _]} = FunType1,
    SpecArity = length(D),
    St1 = case Arity =:= SpecArity of
	      true -> St0;
	      false -> %% Cannot happen if called from the compiler.
                  add_error(A, ETag, St0)
	  end,
    St2 = check_type({type, nowarn(), product, [FunType1|CTypes]}, St1),
    check_specs(Left, ETag, Arity, St2);
check_specs([], _ETag, _Arity, St) ->
    St.

nowarn() ->
    A0 = erl_anno:new(0),
    A1 = erl_anno:set_generated(true, A0),
    erl_anno:set_file("", A1).

check_specs_without_function(#lint{module=Mod,defined=Funcs,specs=Specs}=St) ->
    Fun = fun({M, F, A}, Anno, AccSt) when M =:= Mod ->
                  FA = {F, A},
		  case gb_sets:is_element(FA, Funcs) of
		      true -> AccSt;
		      false -> add_error(Anno, {spec_fun_undefined, FA}, AccSt)
		  end;
	     ({_M, _F, _A}, _Anno, AccSt) -> AccSt
	  end,
    maps:fold(Fun, St, Specs).

%% This generates warnings for functions without specs; if the user has
%% specified both options, we do not generate the same warnings twice.
check_functions_without_spec(Forms, St0) ->
    case is_warn_enabled(missing_spec_all, St0) of
	true ->
	    add_missing_spec_warnings(Forms, St0, all);
	false ->
	    case is_warn_enabled(missing_spec, St0) of
		true ->
		    add_missing_spec_warnings(Forms, St0, exported);
		false ->
                    case is_warn_enabled(missing_spec_documented, St0) of
                        true ->
                            add_missing_spec_warnings(Forms, St0, documented);
                        false ->
                            St0
                    end
            end
    end.

add_missing_spec_warnings(Forms, St0, Type) ->
    Specs = [{F,A} || {_M,F,A} <- maps:keys(St0#lint.specs)],
    Warns = %% functions + line numbers for which we should warn
	case Type of
	    all ->
		[{FA,Anno} || {function,Anno,F,A,_} <- Forms,
			   not lists:member(FA = {F,A}, Specs)];
	    _ ->
                Exps0 = gb_sets:to_list(exports(St0)) -- pseudolocals(),
                Exps1 =
                    if Type =:= documented ->
                            Exps0 -- sets:to_list(St0#lint.hidden_docs);
                       true ->
                            Exps0
                    end,
                Exps = Exps1 -- Specs,
		[{FA,Anno} || {function,Anno,F,A,_} <- Forms,
			   member(FA = {F,A}, Exps)]
	end,
    foldl(fun ({FA,Anno}, St) ->
		  add_warning(Anno, {missing_spec,FA}, St)
	  end, St0, Warns).

check_unused_types(Forms, St) ->
    case is_warn_enabled(unused_type, St) of
        true -> check_unused_types_1(Forms, St);
        false -> St
    end.

check_unused_types_1(Forms, #lint{types=Ts}=St) ->
    case [File || {attribute,_A,file,{File,_Anno}} <- Forms] of
	[FirstFile|_] ->
            L = reached_types(St),
	    UsedTypes = gb_sets:from_list(L),
	    FoldFun =
                fun({{record, _}=_Type, 0}, _, AccSt) ->
                        AccSt; % Before Erlang/OTP 19.0
                   (Type, #typeinfo{anno = Anno}, AccSt) ->
                        case loc(Anno, AccSt) of
			    {FirstFile, _} ->
				case gb_sets:is_member(Type, UsedTypes) of
				    true -> AccSt;
				    false ->
					Warn = {unused_type,Type},
					add_warning(Anno, Warn, AccSt)
				end;
			    _ ->
				%% No warns about unused types in include files
				AccSt
			end
		end,
	    maps:fold(FoldFun, St, Ts);
	[] ->
	    St
    end.

reached_types(#lint{usage = Usage}) ->
    Es = [{From, {type, To}} ||
             To := UsedTs <- Usage#usage.used_types,
             #used_type{at = From} <- UsedTs],
    Initial = initially_reached_types(Es),
    G = sofs:family_to_digraph(sofs:rel2fam(sofs:relation(Es))),
    R = digraph_utils:reachable(Initial, G),
    true = digraph:delete(G),
    [T || {type, T} <- R].

initially_reached_types(Es) ->
    [FromTypeId || {{T, _}=FromTypeId, _} <- Es, T =/= type].

check_local_opaque_types(St) ->
    #lint{types=Ts, exp_types=ExpTs} = St,
    FoldFun =
        fun(_Type, #typeinfo{attr = type}, AccSt) ->
                AccSt;
           (_Type, #typeinfo{attr = nominal, anno = _Anno}, AccSt) ->
                AccSt;
           (Type, #typeinfo{attr = opaque, anno = Anno}, AccSt) ->
                case gb_sets:is_element(Type, ExpTs) of
                    true -> AccSt;
                    false ->
                        Warn = {not_exported_opaque,Type},
                        add_warning(Anno, Warn, AccSt)
                end
        end,
    maps:fold(FoldFun, St, Ts).

check_dialyzer_attribute(Forms, St0) ->
    Vals = [{Anno,V} ||
               {attribute,Anno,dialyzer,Val} <- Forms,
               V0 <- lists:flatten([Val]),
               V <- case V0 of
                        {O,F} ->
                            [{A,B} ||
                                A <- lists:flatten([O]),
                                B <- lists:flatten([F])];
                        T -> [T]
                    end],
    {Wellformed, Bad} =
        lists:partition(fun ({_,{Option,FA}}) when is_atom(Option) ->
                                is_fa(FA);
                            ({_,Option}) when is_atom(Option) -> true;
                            (_) -> false
                        end, Vals),
    St1 = foldl(fun ({Anno,Term}, St) ->
		  add_error(Anno, {bad_dialyzer_attribute,Term}, St)
	  end, St0, Bad),
    DefFunctions = (gb_sets:to_list(St0#lint.defined) -- pseudolocals()),
    Fun = fun ({Anno,{Option,FA}}, St) ->
                  case is_function_dialyzer_option(Option) of
                      true ->
                          case lists:member(FA, DefFunctions) of
                              true -> St;
                              false ->
                                  func_location_error(undefined_function,
                                                      [{FA,Anno}],
                                                      St,
                                                      DefFunctions)
                          end;
                      false ->
                          add_error(Anno, {bad_dialyzer_option,Option}, St)
                  end;
              ({Anno,Option}, St) ->
                  case is_module_dialyzer_option(Option) of
                      true -> St;
                      false ->
                          add_error(Anno, {bad_dialyzer_option,Option}, St)
                  end
          end,
    foldl(Fun, St1, Wellformed).

is_function_dialyzer_option(nowarn_function) -> true;
is_function_dialyzer_option(Option) ->
    is_module_dialyzer_option(Option).

is_module_dialyzer_option(Option) ->
    lists:member(Option,
                 [no_return,no_unused,no_improper_lists,no_fun_app,
                  no_match,no_opaque,no_fail_call,no_contracts,no_unknown,
                  no_behaviours,no_undefined_callbacks,unmatched_returns,
                  error_handling,race_conditions,no_missing_calls,
                  specdiffs,overspecs,underspecs,unknown,
                  no_underspecs,extra_return,no_extra_return,
                  missing_return,no_missing_return,overlapping_contract,
                  opaque_union,no_opaque_union
                 ]).

%% try_catch_clauses(Scs, Ccs, In, ImportVarTable, State) ->
%%      {UpdVt,State}.

try_clauses(Scs, Ccs, In, Vt, Uvt, St0) ->
    {Csvt0,St1} = icrt_clauses(Scs, Vt, St0),
    {Csvt1,St2} = catch_clauses(Ccs, vtupdate(Uvt, Vt), St1),
    Csvt = Csvt0 ++ Csvt1,
    UpdVt = icrt_export(Csvt, Vt, In, St2),
    {UpdVt,St2}.

%% icrt_clauses(Clauses, In, ImportVarTable, State) ->
%%      {UpdVt,State}.

icrt_clauses(Cs, In, Vt, St0) ->
    {Csvt,St1} = icrt_clauses(Cs, Vt, St0),
    UpdVt = icrt_export(Csvt, Vt, In, St1),
    {UpdVt,St1}.

%% icrt_clauses(Clauses, ImportVarTable, State) ->
%%      {NewVts,State}.

icrt_clauses(Cs, Vt, St) ->
    mapfoldl(fun (C, St0) -> icrt_clause(C, Vt, St0) end, St, Cs).

icrt_clause({clause,_Anno,H,G,B}, Vt0, St0) ->
    {Hvt,Hnew,St1} = head(H, Vt0, St0),
    Vt1 = vtupdate(Hvt, Hnew),
    {Gvt,St2} = guard(G, vtupdate(Vt1, Vt0), St1),
    Vt2 = vtupdate(Gvt, Vt1),
    {Bvt,St3} = exprs(B, vtupdate(Vt2, Vt0), St2),
    {vtupdate(Bvt, Vt2),St3}.

catch_clauses(Cs, Vt, St) ->
    mapfoldl(fun(C, St0) -> catch_clause(C, Vt, St0) end, St, Cs).

catch_clause({clause,_Anno,H,G,B}, Vt0, St0) ->
    [{tuple,_,[_,_,Stack]}] = H,
    {Hvt,Hnew,St1} = head(H, Vt0, St0),
    Vt1 = vtupdate(Hvt, Hnew),
    %% check and mark the stack trace variable before checking the guard
    {GuardVt,St2} = taint_stack_var(Stack, vtupdate(Vt1, Vt0), St1),
    {Gvt,St3} = guard(G, GuardVt, St2),
    Vt2 = vtupdate(Gvt, Vt1),
    {Bvt,St4} = exprs(B, vtupdate(Vt2, Vt0), St3),
    {vtupdate(Bvt, Vt2),St4}.

taint_stack_var({var,Anno,V}, Vt, St) when V =/= '_' ->
    St1 = case orddict:find(V, Vt) of
              {ok,{_,used,_}} ->
                  %% the stack var must be unused after processing the pattern;
                  %% it can be used either if bound/unsafe before the try, or
                  %% if it occurs in the class or term part of the pattern
                  add_error(Anno, {stacktrace_bound,V}, St);
              _ ->
                  St
          end,
    {vtupdate([{V,{stacktrace,unused,[Anno]}}], Vt), St1};
taint_stack_var(_, Vt, St) ->
    {Vt, St}.

icrt_export(Vts, Vt, {Tag,Attrs}, St) ->
    {_File,Loc} = loc(Attrs, St),
    icrt_export(lists:merge(Vts), Vt, {Tag,Loc}, length(Vts), []).

icrt_export([{V,{{export,_},_,_}}|Vs0], [{V,{{export,_}=S0,_,As}}|Vt],
            In, I, Acc) ->
    %% V was an exported variable and has been used in an expression in at least
    %% one clause. Its state needs to be merged from all clauses to silence any
    %% exported var warning already emitted.
    {VVs,Vs} = lists:partition(fun ({K,_}) -> K =:= V end, Vs0),
    S = foldl(fun ({_,{S1,_,_}}, AccS) -> merge_state(AccS, S1) end, S0, VVs),
    icrt_export(Vs, Vt, In, I, [{V,{S,used,As}}|Acc]);
icrt_export([{V,_}|Vs0], [{V,{_,_,As}}|Vt], In, I, Acc) ->
    %% V was either unsafe or bound and has now been reused. It may also have
    %% been an export but as it was not matched by the previous clause, it means
    %% it has been changed to 'bound' in at least one clause because it was used
    %% in a pattern.
    Vs = lists:dropwhile(fun ({K,_}) -> K =:= V end, Vs0),
    icrt_export(Vs, Vt, In, I, [{V,{bound,used,As}}|Acc]);
icrt_export([{V1,_}|_]=Vs, [{V2,_}|Vt], In, I, Acc) when V1 > V2 ->
    %% V2 was already in scope and has not been reused in any clause.
    icrt_export(Vs, Vt, In, I, Acc);
icrt_export([{V,_}|_]=Vs0, Vt, In, I, Acc) ->
    %% V is a new variable.
    {VVs,Vs} = lists:partition(fun ({K,_}) -> K =:= V end, Vs0),
    F = fun ({_,{S,U,As}}, {AccI,AccS0,AccAs0}) ->
                AccS = case {S,AccS0} of
                           {{unsafe,_},{unsafe,_}} ->
                               %% V was found unsafe in a previous clause, mark
                               %% it as unsafe for the whole parent expression.
                               {unsafe,In};
                           {{unsafe,_},_} ->
                               %% V was unsafe in a clause, keep that state and
                               %% generalize it to the whole expression if it
                               %% is found unsafe in another one.
                               S;
                           _ ->
                               %% V is either bound or exported, keep original
                               %% state.
                               AccS0
                       end,
                AccAs = case U of
                            used -> AccAs0;
                            unused -> merge_annos(AccAs0, As)
                        end,
                {AccI + 1,AccS,AccAs}
        end,
    %% Initial state is exported from the current expression.
    {Count,S1,As} = foldl(F, {0,{export,In},[]}, VVs),
    S = case Count of
            I ->
                %% V was found in all clauses, keep computed state.
                S1;
            _ ->
                %% V was not bound in some clauses, mark as unsafe.
                {unsafe,In}
        end,
    U = case As of [] -> used; _ -> unused end,
    icrt_export(Vs, Vt, In, I, [{V,{S,U,As}}|Acc]);
icrt_export([], _, _, _, Acc) ->
    reverse(Acc).

handle_comprehension(E, Qs, Vt0, St0) ->
    {Vt1, Uvt, St1} = lc_quals(Qs, Vt0, St0),
    {Evt,St2} = comprehension_expr(E, Vt1, St1),
    Vt2 = vtupdate(Evt, Vt1),
    %% Shadowed global variables.
    {_,St3} = check_old_unused_vars(Vt2, Uvt, St2),
    %% There may be local variables in Uvt that are not global.
    {_,St4} = check_unused_vars(Uvt, Vt0, St3),
    %% Local variables that have not been shadowed.
    {_,St} = check_unused_vars(Vt2, Vt0, St4),
    Vt3 = vtmerge(vtsubtract(Vt2, Uvt), Uvt),
    %% Don't export local variables.
    Vt4 = vtold(Vt3, Vt0),
    %% Forget about old variables which were not used as well as unsafe
    %% variables, preventing them from being marked as used and bound by
    %% icrt_export/4.
    Vt = vt_no_unsafe(vt_no_unused(Vt4)),
    {Vt, St}.

comprehension_expr({map_field_assoc,_,K,V}, Vt0, St0) ->
    expr_list([K,V], Vt0, St0);
comprehension_expr(E, Vt, St) ->
    expr(E, Vt, St).

%% lc_quals(Qualifiers, ImportVarTable, State) ->
%%      {VarTable,ShadowedVarTable,State}
%%  Test list comprehension qualifiers, return all variables. Allow
%%  filters to be both guard tests and general expressions, but the errors
%%  will be for expressions. Return the complete updated vartable including
%%  local variables and all updates. ShadowVarTable contains the state of
%%  each shadowed variable. All variable states of variables in ImportVarTable
%%  that have been shadowed are included in ShadowVarTable. In addition, all
%%  shadowed variables that are not included in ImportVarTable are included
%%  in ShadowVarTable (these are local variables that are not global variables).

lc_quals(Qs, Vt0, St0) ->
    OldRecDef = St0#lint.recdef_top,
    {Vt,Uvt,St} = lc_quals(Qs, Vt0, [], St0#lint{recdef_top = false}),
    {Vt,Uvt,St#lint{recdef_top = OldRecDef}}.

lc_quals([{zip,_Anno,Gens0} | Qs], Vt0, Uvt0, St0) ->
    {Gens1, St1} = filter_generators(Gens0, [], St0),
    {Vt,Uvt,St2} = case Gens1 of
        [] ->
            %% No valid generators.
            {Vt0, Uvt0, St1};
        _ ->
            handle_generators(Gens1, Vt0, Uvt0, St1)
        end,
    lc_quals(Qs, Vt, Uvt, St2);
lc_quals([{generate,_Anno,P,E} | Qs], Vt0, Uvt0, St0) ->
    {Vt,Uvt,St} = handle_generator(P,E,Vt0,Uvt0,St0),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([{generate_strict,_Anno,P,E} | Qs], Vt0, Uvt0, St0) ->
    {Vt,Uvt,St} = handle_generator(P,E,Vt0,Uvt0,St0),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([{b_generate,_Anno,P,E} | Qs], Vt0, Uvt0, St0) ->
    St1 = handle_bitstring_gen_pat(P,St0),
    {Vt,Uvt,St} = handle_generator(P,E,Vt0,Uvt0,St1),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([{b_generate_strict,_Anno,P,E} | Qs], Vt0, Uvt0, St0) ->
    St1 = handle_bitstring_gen_pat(P,St0),
    {Vt,Uvt,St} = handle_generator(P,E,Vt0,Uvt0,St1),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([{m_generate,_Anno,P,E} | Qs], Vt0, Uvt0, St0) ->
    {Vt,Uvt,St} = handle_generator(P,E,Vt0,Uvt0,St0),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([{m_generate_strict,_Anno,P,E} | Qs], Vt0, Uvt0, St0) ->
    {Vt,Uvt,St} = handle_generator(P,E,Vt0,Uvt0,St0),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([F|Qs], Vt, Uvt, St0) ->
    Info = is_guard_test2_info(St0),
    {Fvt,St1} = case is_guard_test2(F, Info) of
		    true -> guard_test(F, Vt, St0);
		    false -> expr(F, Vt, St0)
		end,
    lc_quals(Qs, vtupdate(Fvt, Vt), Uvt, St1);
lc_quals([], Vt, Uvt, St) ->
    {Vt, Uvt, St}.

is_guard_test2_info(#lint{records=RDs,locals=Locals,imports=Imports}) ->
    {RDs,fun(FA) ->
		 is_local_function(Locals, FA) orelse
		     is_imported_function(Imports, FA)
	 end}.

filter_generators([{generate,_,_,_}=Q|Qs], Acc, St) ->
    filter_generators(Qs, [Q|Acc], St);
filter_generators([{generate_strict,_,_,_}=Q|Qs], Acc, St) ->
    filter_generators(Qs, [Q|Acc], St);
filter_generators([{b_generate,_,_,_}=Q|Qs], Acc, St) ->
    filter_generators(Qs, [Q|Acc], St);
filter_generators([{b_generate_strict,_,_,_}=Q|Qs], Acc, St) ->
    filter_generators(Qs, [Q|Acc], St);
filter_generators([{m_generate,_,_,_}=Q|Qs], Acc, St) ->
    filter_generators(Qs, [Q|Acc], St);
filter_generators([{m_generate_strict,_,_,_}=Q|Qs], Acc, St) ->
    filter_generators(Qs, [Q|Acc], St);
filter_generators([Q|Qs], Acc, St0) ->
    Anno1 = element(2, Q),
    St1 = add_error(Anno1, illegal_zip_generator, St0),
    filter_generators(Qs, Acc, St1);
filter_generators([], Acc, St) ->
    {reverse(Acc), St}.

handle_generators(Gens,Vt,Uvt,St0) ->
    Ps = [P || {_,_,P,_} <- Gens],
    Es = [E || {_,_,_,E} <- Gens],
    {Evt,St1} = exprs(Es, Vt, St0),
    %% Forget variables local to E immediately.
    Vt1 = vtupdate(vtold(Evt, Vt), Vt),
    {_, St2} = check_unused_vars(Evt, Vt, St1),
    {Pvt,Pnew,St3} = comprehension_pattern(Ps, Vt1, St2),
    %% Have to keep fresh variables separated from used variables somehow
    %% in order to handle for example X = foo(), [X || <<X:X>> <- bar()].
    %%                                1           2      2 1
    Vt2 = vtupdate(Pvt, Vt1),
    St4 = shadow_vars(Pnew, Vt1, generate, St3),
    Svt = vtold(Vt2, Pnew),
    {_, St5} = check_old_unused_vars(Svt, Uvt, St4),
    NUvt = vtupdate(vtnew(Svt, Uvt), Uvt),
    Vt3 = vtupdate(vtsubtract(Vt2, Pnew), Pnew),
    {Vt3,NUvt,St5}.

handle_generator(P,E,Vt,Uvt,St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    %% Forget variables local to E immediately.
    Vt1 = vtupdate(vtold(Evt, Vt), Vt),
    {_, St2} = check_unused_vars(Evt, Vt, St1),
    {Pvt,Pnew,St3} = comprehension_pattern(P, Vt1, St2),
    %% Have to keep fresh variables separated from used variables somehow
    %% in order to handle for example X = foo(), [X || <<X:X>> <- bar()].
    %%                                1           2      2 1
    Vt2 = vtupdate(Pvt, Vt1),
    St4 = shadow_vars(Pnew, Vt1, generate, St3),
    Svt = vtold(Vt2, Pnew),
    {_, St5} = check_old_unused_vars(Svt, Uvt, St4),
    NUvt = vtupdate(vtnew(Svt, Uvt), Uvt),
    Vt3 = vtupdate(vtsubtract(Vt2, Pnew), Pnew),
    {Vt3,NUvt,St5}.

comprehension_pattern([_|_]=Ps, Vt, St) ->
    Mps = [K || {map_field_exact,_,K,_} <- Ps] ++ [V || {map_field_exact,_,_,V} <- Ps],
    Ps1 = [P || P <- Ps, element(1,P)=/=map_field_exact],
    pattern_list(Ps1++Mps, Vt, [], St);
comprehension_pattern({map_field_exact,_,K,V}, Vt, St) ->
    pattern_list([K,V], Vt, [], St);
comprehension_pattern(P, Vt, St) ->
    pattern(P, Vt, [], St).

handle_bitstring_gen_pat({bin,_,Segments=[_|_]},St) ->
    case lists:last(Segments) of
        {bin_element,Anno,_,default,Flags} when is_list(Flags) ->
            case member(binary, Flags) orelse member(bytes, Flags)
              orelse member(bits, Flags) orelse member(bitstring, Flags) of
                true ->
                    add_error(Anno, unsized_binary_in_bin_gen_pattern, St);
                false ->
                    St
            end;
        _ ->
            St
    end;
handle_bitstring_gen_pat(_,St) ->
    St.

%% fun_clauses(Clauses, ImportVarTable, State) ->
%%      {UsedVars, State}.
%%  Fun's cannot export any variables.

%% It is an error if variable is bound inside a record definition
%% unless it was introduced in a fun or an lc. Only if pat_var finds
%% such variables can the correct line number be given.

%% If fun_used_vars is set, we want to compute the tree of used
%% vars across functions. This is by erl_eval to compute used vars
%% without having to traverse the tree multiple times.

fun_clauses(Cs, Vt, #lint{fun_used_vars=(#{}=FUV)} = St) ->
    {Uvt, St0} = fun_clauses1(Cs, Vt, St#lint{fun_used_vars=maps:new()}),
    #lint{fun_used_vars=InnerFUV} = St0,
    UsedVars = [V || {V, {_, used, _}} <- Uvt],
    OuterFUV = maps:put(Cs, {UsedVars, InnerFUV}, FUV),
    {Uvt, St0#lint{fun_used_vars=OuterFUV}};
fun_clauses(Cs, Vt, St) ->
    fun_clauses1(Cs, Vt, St).

fun_clauses1(Cs, Vt, St) ->
    OldRecDef = St#lint.recdef_top,
    {Bvt,St2} = foldl(fun (C, {Bvt0, St0}) ->
                              {Cvt,St1} = fun_clause(C, Vt, St0),
                              {vtmerge(Cvt, Bvt0),St1}
                      end, {[],St#lint{recdef_top = false}}, Cs),
    Uvt = vt_no_unsafe(vt_no_unused(vtold(Bvt, Vt))),
    {Uvt,St2#lint{recdef_top = OldRecDef}}.

fun_clause({clause,_Anno,H,G,B}, Vt0, St0) ->
    {Hvt,Hnew,St1} = head(H, Vt0, [], St0), % No imported pattern variables
    Vt1 = vtupdate(Hvt, Vt0),
    St2 = shadow_vars(Hnew, Vt0, 'fun', St1),
    Vt2 = vtupdate(vtsubtract(Vt1, Hnew), Hnew),
    {Gvt,St3} = guard(G, Vt2, St2),
    Vt3 = vtupdate(Gvt, Vt2),
    {Bvt,St4} = exprs(B, Vt3, St3),
    Cvt = vtupdate(Bvt, Vt3),
    %% Check new local variables.
    {_, St5} = check_unused_vars(Cvt, Vt0, St4),
    %% Check all shadowing variables.
    Svt = vtold(Vt1, Hnew),
    {_, St6} = check_old_unused_vars(Cvt, Svt, St5),
    Vt4 = vtmerge(Svt, vtsubtract(Cvt, Svt)),
    {vtold(Vt4, Vt0),St6}.

%% In the variable table we store information about variables. The
%% information is a tuple {State,Usage,Annos}, the variables state and
%% usage. A variable can be in the following states:
%%
%% bound                everything is normal
%% {export,From}        variable has been exported
%% {unsafe,In}          variable is unsafe
%%
%% The usage information has the following form:
%%
%% used         variable has been used
%% unused       variable has been bound but not used
%%
%% Annos is a list of annotations including the location where the
%% variable was bound.
%%
%% Report variable errors/warnings as soon as possible and then change
%% the state to ok. This simplifies the code and reports errors only
%% once. Having the usage information like this makes it easy too when
%% merging states.

%% For keeping track of which variables are bound, ordsets are used.
%% In order to be able to give warnings about unused variables, a
%% possible value is {bound, unused, [Anno]}. The usual value when a
%% variable is used is {bound, used, [Anno]}. An exception occurs for
%% variables in the size position in a bin element in a pattern.
%% Currently, such a variable is never matched out, always used, and
%% therefore it makes no sense to warn for "variable imported in
%% match".

%% For storing the variable table we use the orddict module.
%% We know an empty set is [].

%% pat_var(Variable, Anno, VarTable, NewVars, State) ->
%%         {UpdVarTable,UpdNewVars,State}
%% A pattern variable has been found. Handle errors and warnings. Return
%% all used variables as bound so errors and warnings are only reported
%% once. New shadows Vt here, which is necessary in order to separate
%% uses of shadowed and shadowing variables. See also pat_binsize_var.

pat_var(V, Anno, Vt, New, St0) ->
    case orddict:find(V, New) of
        {ok, {bound,_Usage,As}} ->
            %% variable already in NewVars, mark as used
            St = warn_underscore_match(V, Anno, St0),
            {[],[{V,{bound,used,As}}],St};
        error ->
            case orddict:find(V, Vt) of
                {ok,{bound,_Usage,Ls}} ->
                    St = warn_underscore_match(V, Anno, St0),
                    {[{V,{bound,used,Ls}}],[],St};
                {ok,{{unsafe,In},_Usage,Ls}} ->
                    {[{V,{bound,used,Ls}}],[],
                     add_error(Anno, {unsafe_var,V,In}, St0)};
                {ok,{{export,From},_Usage,Ls}} ->
                    St = warn_underscore_match(V, Anno, St0),
                    {[{V,{bound,used,Ls}}],[],
                     %% As this is matching, exported vars are risky.
                     add_warning(Anno, {exported_var,V,From}, St)};
                error when St0#lint.recdef_top ->
                    {[],[{V,{bound,unused,[Anno]}}],
                     add_error(Anno, {variable_in_record_def,V}, St0)};
                error ->
                    %% add variable to NewVars, not yet used
                    {[],[{V,{bound,unused,[Anno]}}],St0}
            end
    end.

%% Underscore-prefixed variables merely suppress unused variable warnings, but
%% are often misunderstood and thought to behave like '_' which can introduce
%% subtle errors. Warn about this since matching them on purpose is rare.
warn_underscore_match(V, Anno, St) ->
    case {is_warn_enabled(underscore_match, St), atom_to_list(V)} of
        {true, [$_ | _]} ->
            add_warning(Anno, {match_underscore_var,V}, St);
        {_, _} ->
            St
    end.

warn_underscore_match_pat(V, Annos, St) ->
    case {is_warn_enabled(underscore_match, St), atom_to_list(V)} of
        {true, [$_ | _]} ->
            warn_underscore_match_pat_1(Annos, V, St);
        {_, _} ->
            St
    end.

warn_underscore_match_pat_1([Anno | Annos], V, St0) ->
    St = add_warning(Anno, {match_underscore_var_pat,V}, St0),
    warn_underscore_match_pat_1(Annos, V, St);
warn_underscore_match_pat_1([], _V, St) ->
    St.

%% pat_binsize_var(Variable, LineNo, VarTable, NewVars, State) ->
%%      {UpdVarTable,UpdNewVars,State'}
%% Special case of pat_var/expr_var for variables in binary size expressions
%% (never adds variables to NewVars, only marks uses).

pat_binsize_var(V, Anno, Vt, New, St) ->
    case orddict:find(V, New) of
        {ok,{bound,_Used,As}} ->
            {[],[{V,{bound,used,As}}],St};
        error ->
            case orddict:find(V, Vt) of
                {ok,{bound,_Used,As}} ->
                    {[{V,{bound,used,As}}],[],St};
                {ok,{{unsafe,In},_Used,As}} ->
                    {[{V,{bound,used,As}}],[],
                     add_error(Anno, {unsafe_var,V,In}, St)};
                {ok,{{export,From},_Used,As}} ->
                    {[{V,{bound,used,As}}],[],
                     %% As this is not matching, exported vars are
                     %% probably safe.
                     exported_var(Anno, V, From, St)};
                error ->
                    PossibleVs = [atom_to_list(DefV) || {DefV, _A} <- Vt],
                    case most_possible_string(V, PossibleVs) of
                        [] ->
                            {[{V,{bound,used,[Anno]}}],[],
                             add_error(Anno, {unbound_var,V}, St)};
                        GuessV ->
                            {[{V,{bound,used,[Anno]}}],[],
                             add_error(Anno, {unbound_var,V,GuessV}, St)}
                    end
            end
    end.

%% expr_var(Variable, Anno, VarTable, State) ->
%%      {UpdVarTable,State}
%%  Check if a variable is defined, or if there is an error or warning
%%  connected to its usage. Return all variables as bound so errors
%%  and warnings are only reported once.  As this is not matching
%%  exported vars are probably safe, warn only if warn_export_vars is
%%  set.

expr_var(V, Anno, Vt, #lint{bvt=none}=St) ->
    do_expr_var(V, Anno, Vt, St);
expr_var(V, Anno, Vt0, #lint{bvt=Bvt0}=St0) when is_list(Bvt0) ->
    %% handles variables in a binary segment size expression
    {Vt,Bvt,St} = pat_binsize_var(V, Anno, Vt0, Bvt0, St0),
    {Vt,St#lint{bvt=vtmerge(Bvt0, Bvt)}}.

do_expr_var(V, Anno, Vt, St) ->
    case orddict:find(V, Vt) of
        {ok,{bound,_Usage,As}} ->
            {[{V,{bound,used,As}}],St};
        {ok,{{unsafe,In},_Usage,As}} ->
            {[{V,{bound,used,As}}],
             add_error(Anno, {unsafe_var,V,In}, St)};
        {ok,{{export,From},_Usage,As}} ->
            case is_warn_enabled(export_vars, St) of
                true ->
                    {[{V,{bound,used,As}}],
                     add_warning(Anno, {exported_var,V,From}, St)};
                false ->
                    {[{V,{{export,From},used,As}}],St}
            end;
        {ok,{stacktrace,_Usage,As}} ->
            {[{V,{bound,used,As}}],
             add_error(Anno, {stacktrace_guard,V}, St)};
        error ->
            PossibleVs = [atom_to_list(DefV) || {DefV, _A} <- Vt],
            case most_possible_string(V, PossibleVs) of
                [] ->
                    {[{V,{bound,used,[Anno]}}],
                     add_error(Anno, {unbound_var,V}, St)};
                GuessV ->
                    {[{V,{bound,used,[Anno]}}],
                     add_error(Anno, {unbound_var,V,GuessV}, St)}
            end
    end.

exported_var(Anno, V, From, St) ->
    case is_warn_enabled(export_vars, St) of
        true -> add_warning(Anno, {exported_var,V,From}, St);
        false -> St
    end.

shadow_vars(Vt, Vt0, In, St0) ->
    case is_warn_enabled(shadow_vars, St0) of
        true ->
            foldl(fun ({V,{_,_,[A | _]}}, St) ->
                          add_warning(A, {shadowed_var,V,In}, St);
                      (_, St) -> St
                  end, St0, vtold(Vt, vt_no_unsafe(Vt0)));
        false -> St0
    end.

check_unused_vars(Vt, Vt0, St0) ->
    U = unused_vars(Vt, Vt0, St0),
    warn_unused_vars(U, Vt, St0).

check_old_unused_vars(Vt, Vt0, St0) ->
    U = unused_vars(vtold(Vt, Vt0), [], St0),
    warn_unused_vars(U, Vt, St0).

unused_vars(Vt, Vt0, _St0) ->
    U0 = orddict:filter(fun (V, {_State,unused,_As}) ->
                                case atom_to_list(V) of
                                    "_"++_ -> false;
                                    _ -> true
                                end;
                            (_V, _How) -> false
                        end, Vt),
    vtnew(U0, Vt0). % Only new variables.

warn_unused_vars([], Vt, St0) ->
    {Vt,St0};
warn_unused_vars(U, Vt, St0) ->
    St1 = case is_warn_enabled(unused_vars, St0) of
	      false -> St0;
	      true ->
		  foldl(fun ({V,{_,unused,As}}, St) ->
				foldl(fun (A, St2) ->
					      add_warning(A, {unused_var,V},
							  St2)
				      end, St, As)
			end, St0, U)
	  end,
    %% Return all variables as bound so warnings are only reported once.
    UVt = map(fun ({V,{State,_,As}}) -> {V,{State,used,As}} end, U),
    {vtmerge(Vt, UVt), St1}.

%% vtupdate(UpdVarTable, VarTable) -> VarTable.
%%  Add the variables in the updated vartable to VarTable. The variables
%%  will be updated with their property in UpdVarTable. The state of
%%  the variables in UpdVarTable will be returned.

vtupdate(Uvt, Vt0) ->
    orddict:merge(fun (_V, {S,U1,A1}, {_S,U2,A2}) ->
                          {S, merge_used(U1, U2), merge_annos(A1, A2)}
                  end, Uvt, Vt0).

%% vtunsafe(From, UpdVarTable, VarTable) -> UnsafeVarTable.
%%  Return all new variables in UpdVarTable as unsafe.

vtunsafe({Tag,Anno}, Uvt, Vt) ->
    Location = erl_anno:location(Anno),
    [{V,{{unsafe,{Tag,Location}},U,As}} || {V,{_,U,As}} <- vtnew(Uvt, Vt)].

%% vtmerge(VarTable, VarTable) -> VarTable.
%%  Merge two variables tables generating a new vartable. Give priority to
%%  errors then warnings.

vtmerge(Vt1, Vt2) ->
    orddict:merge(fun (_V, {S1,U1,A1}, {S2,U2,A2}) ->
                          {merge_state(S1, S2),
                           merge_used(U1, U2),
                           merge_annos(A1, A2)}
                  end, Vt1, Vt2).

vtmerge(Vts) -> foldl(fun (Vt, Mvts) -> vtmerge(Vt, Mvts) end, [], Vts).

%% This is similar to vtmerge/2 but marks repeated variables as 'used' because
%% they're used in the pattern itself (matching). It also returns warnings for
%% iffy variable uses when appropriate, such as matching on underscore-prefixed
%% variables:
%%
%% f(_Same, _Same) -> ok. %% Should raise a warning
vtmerge_pat(VtA, VtB, St0) ->
    Vt0 = orddict:merge(fun(_V, {S1, Usage1, Annos1}, {S2, Usage2, Annos2}) ->
                                Annos = merge_annos(Annos1, Annos2),
                                Usage = case {Usage1, Usage2} of
                                            {unused, unused} ->
                                                {matched, Annos};
                                            {unused, _} ->
                                                {matched, Annos1};
                                            {_, unused} ->
                                                {matched, Annos2};
                                            {_, _} ->
                                                used
                                        end,
                                {merge_state(S1, S2), Usage, Annos}
                        end, VtA, VtB),
    lists:mapfoldl(fun({Name, {State, {matched, MatchAs}, As}}, St1) ->
                           St = warn_underscore_match_pat(Name, MatchAs, St1),
                           {{Name, {State, used, As}}, St};
                      (Var, St) ->
                          {Var, St}
                   end, St0, Vt0).

merge_annos(As1, As2) ->
    ordsets:union(As1,As2).

merge_state({unsafe,_F1}=S1, _S2) -> S1;          %Take the error case
merge_state(_S1, {unsafe,_F2}=S2) -> S2;
merge_state(bound, S2) -> S2;                   %Take the warning
merge_state(S1, bound) -> S1;
merge_state({export,F1},{export,_F2}) ->        %Sanity check
    %% We want to report the outermost construct
    {export,F1}.

merge_used(used, _Usage2) -> used;
merge_used(_Usage1, used) -> used;
merge_used(unused, unused) -> unused.

%% vtnew(NewVarTable, OldVarTable) -> NewVarTable.
%%  Return all the truly new variables in NewVarTable.

vtnew(New, Old) ->
    orddict:filter(fun (V, _How) -> not orddict:is_key(V, Old) end, New).

%% vtsubtract(VarTable1, VarTable2) -> NewVarTable.
%%  Return all the variables in VarTable1 which don't occur in VarTable2.
%%  Same thing as vtnew, but a more intuitive name for some uses.
vtsubtract(New, Old) ->
    vtnew(New, Old).

%% vtold(NewVarTable, OldVarTable) -> OldVarTable.
%%  Return all the truly old variables in NewVarTable.

vtold(New, Old) ->
    orddict:filter(fun (V, _How) -> orddict:is_key(V, Old) end, New).

vt_no_unsafe(Vt) -> [V || {_,{S,_U,_A}}=V <- Vt,
                          case S of
                              {unsafe,_} -> false;
                              _ -> true
                          end].

vt_no_unused(Vt) -> [V || {_,{_,U,_A}}=V <- Vt, U =/= unused].

%% copy_expr(Expr, Anno) -> Expr.
%%  Make a copy of Expr converting all annotations numbers to Anno.

copy_expr(Expr, Anno) ->
    erl_parse:map_anno(fun(_A) -> Anno end, Expr).

%% Check a record_info call. We have already checked that it is not
%% shadowed by an import.

check_record_info_call(_Anno,Aa,[{atom,Ai,Info},{atom,_An,Name}],St) ->
    case member(Info, [fields,size]) of
        true -> exist_record(Aa, Name, St);
        false -> add_error(Ai, illegal_record_info, St)
    end;
check_record_info_call(Anno,_Aa,_As,St) ->
    add_error(Anno, illegal_record_info, St).

has_wildcard_field([{record_field,_Af,{var,Aa,'_'},_Val}|_Fs]) -> Aa;
has_wildcard_field([_|Fs]) -> has_wildcard_field(Fs);
has_wildcard_field([]) -> no.

%% Raises a warning when updating a (map, record) literal, as that is most
%% likely unintentional. For example, if we forget a comma in a list like the
%% following:
%%
%%    -record(foo, {bar}).
%%
%%    list() ->
%%        [
%%            #foo{bar = foo} %% MISSING COMMA!
%%            #foo{bar = bar}
%%        ].
%%
%% We only raise a warning when the expression-to-be-updated is a map or a
%% record, as better warnings will be raised elsewhere for other funny
%% constructs.
warn_if_literal_update(Anno, Expr, St) ->
    case is_literal_update(Expr) andalso is_warn_enabled(update_literal, St) of
        true -> add_warning(Anno, update_literal, St);
        false -> St
    end.

is_literal_update({record, _, Inner, _, _}) ->
    is_literal_update(Inner);
is_literal_update({record, _, _, _}) ->
    true;
is_literal_update({map, _, Inner, _}) ->
    is_literal_update(Inner);
is_literal_update({map, _, _}) ->
    true;
is_literal_update(_Expr) ->
    false.

%% check_remote_function(Anno, ModuleName, FuncName, [Arg], State) -> State.
%%  Perform checks on known remote calls.

check_remote_function(Anno, M, F, As, St0) ->
    St1 = deprecated_function(Anno, M, F, As, St0),
    St2 = check_qlc_hrl(Anno, M, F, As, St1),
    St3 = check_load_nif(Anno, M, F, As, St2),
    format_function(Anno, M, F, As, St3).

%% check_load_nif(Anno, ModName, FuncName, [Arg], State) -> State
%%  Add warning if erlang:load_nif/2 is called when any kind of inlining has
%%  been enabled.
check_load_nif(Anno, erlang, load_nif, [_, _], St0) ->
    St = St0#lint{load_nif = true},
    case is_warn_enabled(nif_inline, St) of
        true -> check_nif_inline(Anno, St);
        false -> St
    end;
check_load_nif(_Anno, _ModName, _FuncName, _Args, St) ->
    St.

check_nif_inline(Anno, St) ->
    case any(fun is_inline_opt/1, St#lint.compile) of
        true -> add_warning(Anno, nif_inline, St);
        false -> St
    end.

is_inline_opt({inline, [_|_]=_FAs}) -> true;
is_inline_opt(inline) -> true;
is_inline_opt(_) -> false.

%% check_qlc_hrl(Anno, ModName, FuncName, [Arg], State) -> State
%%  Add warning if qlc:q/1,2 has been called but qlc.hrl has not
%%  been included.

check_qlc_hrl(Anno, M, F, As, St) ->
    Arity = length(As),
    case As of
        [{lc,_A,_E,_Qs}|_] when M =:= qlc, F =:= q,
                                Arity < 3, not St#lint.xqlc ->
            add_warning(Anno, {missing_qlc_hrl, Arity}, St);
        _ ->
            St
    end.

%% deprecated_function(Anno, ModName, FuncName, [Arg], State) -> State.
%%  Add warning for calls to deprecated functions.

-dialyzer({no_match, deprecated_function/5}).

deprecated_function(Anno, M, F, As, St) ->
    Arity = length(As),
    MFA = {M, F, Arity},
    case otp_internal:obsolete(M, F, Arity) of
	{deprecated, String} when is_list(String) ->
            case not is_warn_enabled(deprecated_function, St) orelse
		ordsets:is_element(MFA, St#lint.not_deprecated) of
                true ->
		    St;
                false ->
		    add_warning(Anno, {deprecated, MFA, String}, St)
            end;
	{deprecated, Replacement, Rel} ->
            case not is_warn_enabled(deprecated_function, St) orelse
		ordsets:is_element(MFA, St#lint.not_deprecated) of
                true ->
		    St;
                false ->
		    add_warning(Anno, {deprecated, MFA, Replacement, Rel}, St)
            end;
	{removed, String} when is_list(String) ->
	    add_removed_warning(Anno, MFA, {removed, MFA, String}, St);
	{removed, Replacement, Rel} ->
	    add_removed_warning(Anno, MFA, {removed, MFA, Replacement, Rel}, St);
        no ->
	    St
    end.

add_removed_warning(Anno, {M, _, _}=MFA, Warning, #lint{not_removed=NotRemoved}=St) ->
    case is_warn_enabled(removed, St) andalso
        not gb_sets:is_element(M, NotRemoved) andalso
        not gb_sets:is_element(MFA, NotRemoved) of
        true ->
            add_warning(Anno, Warning, St);
        false ->
            St
    end.

-dialyzer({no_match, deprecated_type/5}).

deprecated_type(Anno, M, N, As, St) ->
    NAs = length(As),
    case otp_internal:obsolete_type(M, N, NAs) of
        {deprecated, String} when is_list(String) ->
            maybe_add_warning(Anno, {deprecated_type, {M,N,NAs}, String}, St);
        {removed, String} ->
            add_warning(Anno, {removed_type, {M,N,NAs}, String}, St);
        no ->
            St
    end.

obsolete_guard({call,Anno,{atom,Ar,F},As}, St0) ->
    Arity = length(As),
    case erl_internal:old_type_test(F, Arity) of
	false ->
	    deprecated_function(Anno, erlang, F, As, St0);
	true ->
	    St = maybe_add_warning(Ar, {obsolete_guard, {F, Arity}}, St0),
	    test_overriden_by_local(Ar, F, Arity, St)
    end;
obsolete_guard(_G, St) ->
    St.

test_overriden_by_local(Anno, OldTest, Arity, St) ->
    ModernTest = list_to_atom("is_"++atom_to_list(OldTest)),
    case is_local_function(St#lint.locals, {ModernTest, Arity}) of
	true ->
	    add_error(Anno, {obsolete_guard_overridden,OldTest}, St);
	false ->
	    St
    end.

feature_keywords() ->
    Features = erl_features:configurable(),
    G = fun(Ftr, Map) ->
                Keywords = erl_features:keywords(Ftr),
                Add = fun(Keyword, M) -> maps:put(Keyword, Ftr, M) end,
                lists:foldl(Add, Map, Keywords)
        end,
    lists:foldl(G, #{}, Features).

%% keyword_warning(Anno, Atom, State) -> State.
%%  Add warning for atoms that will be reserved keywords in the future.
%%  (Currently, no such keywords to warn for.)
keyword_warning(Anno, Atom, St) ->
    case is_warn_enabled(keywords, St) of
        true ->
            case erl_anno:text(Anno) of
                [$'| _] ->
                    %% Don't warn for quoted atoms
                    St;
                _ ->
                    Keywords = St#lint.feature_keywords,
                    case maps:find(Atom, Keywords) of
                        error -> St;
                        {ok, Ftr} ->
                            add_warning(Anno, {future_feature, Ftr, Atom}, St)
                    end
            end;
        false ->
            St
    end.

%% format_function(Anno, ModName, FuncName, [Arg], State) -> State.
%%  Add warning for bad calls to io:fwrite/format functions.

format_function(DefAnno, M, F, As, St) ->
    maybe
        true ?= is_format_function(M, F),
        Lev = St#lint.warn_format,
        true ?= Lev > 0,
        case check_format_1(As) of
            {warn,Level,Fmt,Fas} when Level =< Lev ->
                add_warning(DefAnno, {format_error,{Fmt,Fas}}, St);
            {warn,Level,Anno,Fmt,Fas} when Level =< Lev ->
                add_warning(Anno, {format_error,{Fmt,Fas}}, St);
            _ -> St
        end
    else
        false -> St
    end.

is_format_function(io, fwrite) -> true;
is_format_function(io, format) -> true;
is_format_function(io_lib, fwrite) -> true;
is_format_function(io_lib, format) -> true;
is_format_function(M, F) when is_atom(M), is_atom(F) -> false.

%% check_format_1([Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_1([Fmt]) ->
    check_format_1([Fmt,no_argument_list]);
check_format_1([Fmt,As]) ->
    check_format_2(Fmt, canonicalize_string(As));
check_format_1([_Dev,Fmt,As]) ->
    check_format_1([Fmt,As]);
check_format_1(_As) ->
    {warn,1,"format call with wrong number of arguments",[]}.

canonicalize_string({string,Anno,Cs}) ->
    foldr(fun (C, T) -> {cons,Anno,{integer,Anno,C},T} end, {nil,Anno}, Cs);
canonicalize_string(Term) ->
    Term.

check_format_2(Fmt, As) ->
    case Fmt of
        {string,A,S} ->
            check_format_2a(S, A, As);
        {atom,A,Atom} ->
            check_format_2a(atom_to_list(Atom), A, As);
        _ ->
            Anno = erl_parse:first_anno(Fmt),
            {warn,2,Anno,"format string not a textual constant",[]}
    end.

check_format_2a(Fmt, FmtAnno, no_argument_list=As) ->
    check_format_3(Fmt, FmtAnno, As);
check_format_2a(Fmt, FmtAnno, As) ->
    case args_list(As) of
        true ->
            check_format_3(Fmt, FmtAnno, As);
        false ->
            Anno = element(2, As),
            {warn,1,Anno,"format arguments not a list",[]};
        'maybe' ->
            Anno = erl_parse:first_anno(As),
            {warn,2,Anno,"format arguments perhaps not a list",[]}
    end.

check_format_3(Fmt, FmtAnno, As) ->
    case check_format_string(Fmt, true) of
        {ok,Need} ->
            check_format_4(Need, FmtAnno, As);
        {error,S} ->
            {warn,1,FmtAnno,"format string invalid (~ts)",[S]}
    end.

check_format_4([], _FmtAnno, no_argument_list) ->
    ok;
check_format_4(Need, FmtAnno, no_argument_list) ->
    Msg = "the format string requires an argument list with ~s, but no argument list is given",
    {warn,1,FmtAnno,Msg,[arguments(length(Need))]};
check_format_4(Need, _FmtAnno, As) ->
    Anno = element(2, As),
    Prefix = "the format string requires an argument list with ~s, but the argument list ",
    case {args_length(As),length(Need)} of
        {Same,Same} ->
            ok;
        {Actual,0} ->
            Msg = "the format string requires an empty argument list, but the argument list contains ~s",
            {warn,1,Anno,Msg,[arguments(Actual)]};
        {0,Needed} ->
            Msg = Prefix ++ "is empty",
            {warn,1,Anno,Msg,[arguments(Needed)]};
        {Actual,Needed} when Actual < Needed ->
            Msg = Prefix ++ "contains only ~s",
            {warn,1,Anno,Msg,[arguments(Needed),arguments(Actual)]};
        {Actual,Needed} when Actual > Needed ->
            Msg = Prefix ++ "contains ~s",
            {warn,1,Anno,Msg,[arguments(Needed),arguments(Actual)]}
    end.

arguments(1) ->
    "1 argument";
arguments(N) ->
    [integer_to_list(N), " arguments"].

args_list({cons,_A,_H,T}) -> args_list(T);
%% Strange case: user has written something like [a | "bcd"]; pretend
%% we don't know:
args_list({string,_A,_Cs}) -> 'maybe';
args_list({nil,_A}) -> true;
args_list({atom,_,_}) -> false;
args_list({integer,_,_}) -> false;
args_list({float,_,_}) -> false;
args_list(_Other) -> 'maybe'.

args_length({cons,_A,_H,T}) -> 1 + args_length(T);
args_length({nil,_A}) -> 0.


-doc false.
check_format_string(Fmt) ->
    check_format_string(Fmt, true).

-doc false.
check_format_string(Fmt, Strict) when is_atom(Fmt) ->
    check_format_string(atom_to_list(Fmt), Strict);
check_format_string(Fmt, Strict) when is_binary(Fmt) ->
    check_format_string(binary_to_list(Fmt), Strict);
check_format_string(Fmt, Strict) ->
    extract_sequences(Fmt, [], Strict).

extract_sequences(Fmt, Need0, Strict) ->
    case string:find(Fmt, [$~]) of
        nomatch -> {ok,lists:reverse(Need0)};         %That's it
        [$~|Fmt1] ->
            case extract_sequence(1, Fmt1, Need0, Strict) of
                {ok,Need1,Rest} -> extract_sequences(Rest, Need1, Strict);
                Error -> Error
            end
    end.

extract_sequence(1, [$-,C|Fmt], Need, Strict)
  when is_integer(C), C >= $0, C =< $9 ->
    extract_sequence_digits(1, Fmt, Need, Strict);
extract_sequence(1, [C|Fmt], Need, Strict)
  when is_integer(C), C >= $0, C =< $9 ->
    extract_sequence_digits(1, Fmt, Need, Strict);
extract_sequence(1, [$-,$*|Fmt], Need, Strict) ->
    extract_sequence(2, Fmt, [int|Need], Strict);
extract_sequence(1, [$*|Fmt], Need, Strict) ->
    extract_sequence(2, Fmt, [int|Need], Strict);
extract_sequence(1, Fmt, Need, Strict) ->
    extract_sequence(2, Fmt, Need, Strict);

extract_sequence(2, [$.,C|Fmt], Need, Strict)
  when is_integer(C), C >= $0, C =< $9 ->
    extract_sequence_digits(2, Fmt, Need, Strict);
extract_sequence(2, [$.,$*|Fmt], Need, Strict) ->
    extract_sequence(3, Fmt, [int|Need], Strict);
extract_sequence(2, [$.|Fmt], Need, Strict) ->
    extract_sequence(3, Fmt, Need, Strict);
extract_sequence(2, Fmt, Need, Strict) ->
    extract_sequence(4, Fmt, Need, Strict);

extract_sequence(3, [$.,$*|Fmt], Need, Strict) ->
    extract_sequence(4, Fmt, [int|Need], Strict);
extract_sequence(3, [$.,_|Fmt], Need, Strict) ->
    extract_sequence(4, Fmt, Need, Strict);
extract_sequence(3, Fmt, Need, Strict) ->
    extract_sequence(4, Fmt, Need, Strict);

extract_sequence(4, Fmt0, Need0, Strict) ->
    case extract_modifiers(Fmt0, [], Need0, Strict) of
        {error, _} = Error ->
            Error;
        {[C|Fmt], Modifiers, Need1} when Strict ->
            maybe
                ok ?= check_modifiers(C, Modifiers),
                extract_sequence(5, [C|Fmt], Need1, Strict)
            end;
        {Fmt, _, Need1} ->
            extract_sequence(5, Fmt, Need1, Strict)
    end;

extract_sequence(5, [C|Fmt], Need0, _Strict) ->
    case control_type(C, Need0) of
        error -> {error,"invalid control ~" ++ [C]};
        Need1 -> {ok,Need1,Fmt}
    end;
extract_sequence(_, [], _Need, _Strict) -> {error,"truncated"}.

extract_sequence_digits(Fld, [C|Fmt], Need, Strict)
  when is_integer(C), C >= $0, C =< $9 ->
    extract_sequence_digits(Fld, Fmt, Need, Strict);
extract_sequence_digits(Fld, Fmt, Need, Strict) ->
    extract_sequence(Fld+1, Fmt, Need, Strict).

extract_modifiers([C|Fmt], Modifiers0, Need0, Strict) ->
    case is_modifier(C, Need0) of
	{true, Need1} when Strict ->
            case ordsets:add_element(C, Modifiers0) of
                Modifiers0 ->
                    {error, "repeated modifier " ++ [C]};
                Modifiers ->
                    extract_modifiers(Fmt, Modifiers, Need1, Strict)
            end;
	{true, Need1} ->
	    extract_modifiers(Fmt, ordsets:add_element(C, Modifiers0), Need1, Strict);
        false ->
            {[C|Fmt], Modifiers0, Need0}
    end;
extract_modifiers([], Modifiers, Need, _Strict) ->
    {[], Modifiers, Need}.

check_modifiers(C, Modifiers) ->
    maybe
        ok ?= check_modifiers_1("l", Modifiers, C, "Pp"),
        ok ?= check_modifiers_1("lt", Modifiers, C, "cPpsWw"),
        ok ?= check_modifiers_1("Kk", Modifiers, C, "PpWw")
    end.

check_modifiers_1(M, Modifiers, C, Cs) ->
    case ordsets:intersection(ordsets:from_list(M), Modifiers) of
        [_]=Mod ->
            case lists:member(C, Cs) of
                true ->
                    ok;
                false ->
                    {error, "invalid modifier/control combination ~" ++
                         Mod ++ [C]}
            end;
        [] ->
            ok;
        [_,_]=M ->
            {error, "conflicting modifiers ~" ++ M ++ [C]}
    end.

is_modifier($k, Need) -> {true, Need};
is_modifier($K, Need) -> {true, ['fun'|Need]};
is_modifier($l, Need) -> {true, Need};
is_modifier($t, Need) -> {true, Need};
is_modifier(_, _) -> false.

control_type($~, Need) -> Need;
control_type($c, Need) -> [int|Need];
control_type($f, Need) -> [float|Need];
control_type($e, Need) -> [float|Need];
control_type($g, Need) -> [float|Need];
control_type($s, Need) -> [string|Need];
control_type($w, Need) -> [term|Need];
control_type($p, Need) -> [term|Need];
control_type($W, Need) -> [int,term|Need]; %% Note: reversed
control_type($P, Need) -> [int,term|Need]; %% Note: reversed
control_type($b, Need) -> [int|Need];
control_type($B, Need) -> [int|Need];
control_type($x, Need) -> [string,int|Need]; %% Note: reversed
control_type($X, Need) -> [string,int|Need]; %% Note: reversed
control_type($+, Need) -> [term|Need];
control_type($#, Need) -> [term|Need];
control_type($n, Need) -> Need;
control_type($i, Need) -> [term|Need];
control_type(_C, _Need) -> error.


%% Prebuild set of local functions (to override auto-import)
local_functions(Forms) ->
    gb_sets:from_list([ {Func,Arity} || {function,_,Func,Arity,_} <- Forms ]).
%% Predicate to find out if the function is locally defined
is_local_function(LocalSet,{Func,Arity}) ->
    gb_sets:is_element({Func,Arity},LocalSet).
%% Predicate to see if a function is explicitly imported
is_imported_function(ImportSet,{Func,Arity}) ->
    case orddict:find({Func,Arity}, ImportSet) of
        {ok,_Mod} -> true;
        error -> false
    end.
%% Predicate to see if a function is explicitly imported from the erlang module
is_imported_from_erlang(ImportSet,{Func,Arity}) ->
    case orddict:find({Func,Arity}, ImportSet) of
        {ok,erlang} -> true;
        _ -> false
    end.
%% Build set of functions where auto-import is explicitly suppressed
auto_import_suppressed(CompileFlags) ->
    case lists:member(no_auto_import, CompileFlags) of
        true ->
            all;
        false ->
            L0 = [ X || {no_auto_import,X} <- CompileFlags ],
            L1 = [ {Y,Z} || {Y,Z} <- lists:flatten(L0), is_atom(Y), is_integer(Z) ],
            {set, gb_sets:from_list(L1)}
    end.
%% Predicate to find out if autoimport is explicitly suppressed for a function
is_autoimport_suppressed(all,{_Func,_Arity}) ->
    true;
is_autoimport_suppressed({set, NoAutoSet},{Func,Arity}) ->
    gb_sets:is_element({Func,Arity},NoAutoSet).
%% Predicate to find out if a function specific bif-clash suppression (old deprecated) is present
bif_clash_specifically_disabled(St,{F,A}) ->
    lists:member({F,A},St#lint.nowarn_bif_clash).

%% Predicate to find out if an autoimported guard_bif is not overriden in some way
%% Guard Bif without module name is disallowed if
%% * It is overridden by local function
%% * It is overridden by -import and that import is not of itself (i.e. from module erlang)
%% * The autoimport is suppressed or it's not reimported by -import directive
%% Otherwise it's OK (given that it's actually a guard bif and actually is autoimported)
no_guard_bif_clash(St,{F,A}) ->
    (
      (not is_local_function(St#lint.locals,{F,A}))
      andalso
      (
        (not is_imported_function(St#lint.imports,{F,A})) orelse
	 is_imported_from_erlang(St#lint.imports,{F,A})
      )
      andalso
      (
        (not is_autoimport_suppressed(St#lint.no_auto, {F,A})) orelse
	 is_imported_from_erlang(St#lint.imports,{F,A})
      )
    ).

%% maps_prepend(Key, Value, Map) -> Map.

maps_prepend(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, Values} ->
            maps:put(Key, [Value|Values], Map);
        error ->
            maps:put(Key, [Value], Map)
    end.
