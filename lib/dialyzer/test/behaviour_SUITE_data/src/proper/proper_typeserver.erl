%%% Copyright 2010-2016 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2016 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis

%%% @doc Erlang type system - PropEr type system integration module.
%%%
%%% PropEr can parse types expressed in Erlang's type language and convert them
%%% to its own type format. Such expressions can be used instead of regular type
%%% constructors in the second argument of `?FORALL's. No extra notation is
%%% required; PropEr will detect which calls correspond to native types by
%%% applying a parse transform during compilation. This parse transform is
%%% automatically applied to any module that includes the `proper.hrl' header
%%% file. You can disable this feature by compiling your modules with
%%% `-DPROPER_NO_TRANS'. Note that this will currently also disable the
%%% automatic exporting of properties.
%%%
%%% The use of native types in properties is subject to the following usage
%%% rules:
%%% <ul>
%%% <li>Native types cannot be used outside of `?FORALL's.</li>
%%% <li>Inside `?FORALL's, native types can be combined with other native
%%%   types, and even with PropEr types, inside tuples and lists (the constructs
%%%   `[...]', `{...}' and `++' are all allowed).</li>
%%% <li>All other constructs of Erlang's built-in type system (e.g. `|' for
%%%   union, `_' as an alias of `any()', `<<_:_>>' binary type syntax and
%%%   `fun((...) -> ...)' function type syntax) are not allowed in `?FORALL's,
%%%   because they are rejected by the Erlang parser.</li>
%%% <li>Anything other than a tuple constructor, list constructor, `++'
%%%   application, local or remote call will automatically be considered a
%%%   PropEr type constructor and not be processed further by the parse
%%%   transform.</li>
%%% <li>Parametric native types are fully supported; of course, they can only
%%%   appear instantiated in a `?FORALL'. The arguments of parametric native
%%%   types are always interpreted as native types.</li>
%%% <li>Parametric PropEr types, on the other hand, can take any kind of
%%%   argument. You can even mix native and PropEr types in the arguments of a
%%%   PropEr type. For example, assuming that the following declarations are
%%%   present:
%%%   ``` my_proper_type() -> ?LET(...).
%%%       -type my_native_type() :: ... .'''
%%%   Then the following expressions are all legal:
%%%   ``` vector(2, my_native_type())
%%%       function(0, my_native_type())
%%%       union([my_proper_type(), my_native_type()])''' </li>
%%% <li>Some type constructors can take native types as arguments (but only
%%%   inside `?FORALL's):
%%%   <ul>
%%%   <li>`?SUCHTHAT', `?SUCHTHATMAYBE', `non_empty', `noshrink': these work
%%%     with native types too</li>
%%%   <li>`?LAZY', `?SHRINK', `resize', `?SIZED': these don't work with native
%%%     types</li>
%%%   <li>`?LET', `?LETSHRINK': only the top-level base type can be a native
%%%     type</li>
%%%   </ul></li>
%%% <li>Native type declarations in the `?FORALL's of a module can reference any
%%%   custom type declared in a `-type' or `-opaque' attribute of the same
%%%   module, as long as no module identifier is used.</li>
%%% <li>Typed records cannot be referenced inside `?FORALL's using the
%%%   `#rec_name{}' syntax. To use a typed record in a `?FORALL', enclose the
%%%   record in a custom type like so:
%%%   ``` -type rec_name() :: #rec_name{}. '''
%%%   and use the custom type instead.</li>
%%% <li>`?FORALL's may contain references to self-recursive or mutually
%%%   recursive native types, so long as each type in the hierarchy has a clear
%%%   base case.
%%%   Currently, PropEr requires that the toplevel of any recursive type
%%%   declaration is either a (maybe empty) list or a union containing at least
%%%   one choice that doesn't reference the type directly (it may, however,
%%%   reference any of the types that are mutually recursive with it). This
%%%   means, for example, that some valid recursive type declarations, such as
%%%   this one:
%%%   ``` ?FORALL(..., a(), ...) '''
%%%   where:
%%%   ``` -type a() :: {'a','none' | a()}. '''
%%%   are not accepted by PropEr. However, such types can be rewritten in a way
%%%   that allows PropEr to parse them:
%%%   ``` ?FORALL(..., a(), ...) '''
%%%   where:
%%%   ``` -type a() :: {'a','none'} | {'a',a()}. '''
%%%   This also means that recursive record declarations are not allowed:
%%%   ``` ?FORALL(..., rec(), ...) '''
%%%   where:
%%%   ``` -type rec() :: #rec{}.
%%%       -record(rec, {a = 0 :: integer(), b = 'nil' :: 'nil' | #rec{}}). '''
%%%   A little rewritting can usually remedy this problem as well:
%%%   ``` ?FORALL(..., rec(), ...) '''
%%%   where:
%%%   ``` -type rec() :: #rec{b :: 'nil'} | #rec{b :: rec()}.
%%%       -record(rec, {a = 0 :: integer(), b = 'nil' :: 'nil' | #rec{}}). '''
%%%   </li>
%%% <li>Remote types may be referenced in a `?FORALL', so long as they are
%%%   exported from the remote module. Currently, PropEr requires that any
%%%   remote modules whose types are directly referenced from within properties
%%%   are present in the code path at compile time, either compiled with
%%%   `debug_info' enabled or in source form. If PropEr cannot find a remote
%%%   module at all, finds only a compiled object file with no debug
%%%   information or fails to compile the source file, all calls to that module
%%%   will automatically be considered calls to PropEr type constructors.</li>
%%% <li>For native types to be translated correctly, both the module that
%%%   contains the `?FORALL' declaration as well as any module that contains
%%%   the declaration of a type referenced (directly or indirectly) from inside
%%%   a `?FORALL' must be present in the code path at runtime, either compiled
%%%   with `debug_info' enabled or in source form.</li>
%%% <li>Local types with the same name as an auto-imported BIF are not accepted
%%%   by PropEr, unless the BIF in question has been declared in a
%%%   `no_auto_import' option.</li>
%%% <li>When an expression can be interpreted both as a PropEr type and as a
%%%   native type, the former takes precedence. This means that a function
%%%   `foo()' will shadow a type `foo()' if they are both present in the module.
%%%   The same rule applies to remote functions and types as well.</li>
%%% <li>The above may cause some confusion when list syntax is used:
%%%   <ul>
%%%   <li>The expression `[integer()]' can be interpreted both ways, so the
%%%     PropEr way applies. Therefore, instances of this type will always be
%%%     lists of length 1, not arbitrary integer lists, as would be expected
%%%     when interpreting the expression as a native type.</li>
%%%   <li>Assuming that a custom type foo/1 has been declared, the expression
%%%     `foo([integer()])' can only be interpreted as a native type declaration,
%%%     which means that the generic type of integer lists will be passed to
%%%     `foo/1'.</li>
%%%   </ul></li>
%%% <li>Currently, PropEr does not detect the following mistakes:
%%%   <ul>
%%%   <li>inline record-field specializations that reference non-existent
%%%     fields</li>
%%%   <li>type parameters that are not present in the RHS of a `-type'
%%%     declaration</li>
%%%   <li>using `_' as a type variable in the LHS of a `-type' declaration</li>
%%%   <li>using the same variable in more than one position in the LHS of a
%%%     `-type' declaration</li>
%%%   </ul>
%%% </li>
%%% </ul>
%%%
%%% You can use <a href="#index">these</a> functions to try out the type
%%% translation subsystem.
%%%
%%% CAUTION: These functions should never be used inside properties. They are
%%% meant for demonstration purposes only.

-module(proper_typeserver).
-behaviour(gen_server).
-export([demo_translate_type/2, demo_is_instance/3]).

-export([start/0, restart/0, stop/0, create_spec_test/3, get_exp_specced/1,
	 is_instance/3, translate_type/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).
-export([get_exp_info/1, match/2]).

-export_type([imm_type/0, mod_exp_types/0, mod_exp_funs/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(SRC_FILE_EXT, ".erl").

-ifdef(AT_LEAST_19).
-define(anno(L), erl_anno:new(L)).
-else.
-define(anno(L), L).
-endif.

%% Starting with 18.0 we need to handle both 'type' and 'user_type' tags;
%% prior Erlang/OTP releases had only 'type' as a tag.
-define(IS_TYPE_TAG(T), (T =:= type orelse T =:= user_type)).

%% CAUTION: all these must be sorted
-define(STD_TYPES_0,
	[any,arity,atom,binary,bitstring,bool,boolean,byte,char,float,integer,
	 list,neg_integer,non_neg_integer,number,pos_integer,string,term,
	 timeout]).
-define(HARD_ADTS,
	%% gb_trees:iterator and gb_sets:iterator are NOT hardcoded
	[{{array,0},array},      {{array,1},proper_array},
	 {{dict,0},dict},        {{dict,2},proper_dict},
	 {{gb_set,0},gb_sets},   {{gb_set,1},proper_gb_sets},
	 {{gb_tree,0},gb_trees}, {{gb_tree,2},proper_gb_trees},
	                         {{orddict,2},proper_orddict},
	                         {{ordset,1},proper_ordsets},
	 {{queue,0},queue},      {{queue,1},proper_queue},
	 {{set,0},sets},         {{set,1},proper_sets}]).
-define(HARD_ADT_MODS,
	[{array, [{{array,0},
		   {{type,0,record,[{atom,0,array}]},[]}}]},
	 {dict, [{{dict,0},
		  {{type,0,record,[{atom,0,dict}]},[]}}]},
	 {gb_sets, [{{gb_set,0},
		     {{type,0,tuple,[{type,0,non_neg_integer,[]},
				     {type,0,gb_set_node,[]}]},[]}}]},
	 {gb_trees, [{{gb_tree,0},
		      {{type,0,tuple,[{type,0,non_neg_integer,[]},
				      {type,0,gb_tree_node,[]}]},[]}}]},
	 %% Our parametric ADTs are already declared as normal types, we just
	 %% need to change them to opaques.
	 {proper_array, [{{array,1},already_declared}]},
	 {proper_dict, [{{dict,2},already_declared}]},
	 {proper_gb_sets, [{{gb_set,1},already_declared},
			   {{iterator,1},already_declared}]},
	 {proper_gb_trees, [{{gb_tree,2},already_declared},
			    {{iterator,2},already_declared}]},
	 {proper_orddict, [{{orddict,2},already_declared}]},
	 {proper_ordsets, [{{ordset,1},already_declared}]},
	 {proper_queue, [{{queue,1},already_declared}]},
	 {proper_sets, [{{set,1},already_declared}]},
	 {queue, [{{queue,0},
		   {{type,0,tuple,[{type,0,list,[]},{type,0,list,[]}]},[]}}]},
	 {sets, [{{set,0},
		  {{type,0,record,[{atom,0,set}]},[]}}]}]).


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type type_name() :: atom().
-type var_name() :: atom(). %% TODO: also integers?
-type field_name() :: atom().

-type type_kind() :: 'type' | 'record'.
-type type_ref() :: {type_kind(),type_name(),arity()}.
-ifdef(NO_MODULES_IN_OPAQUES).
-type substs_dict() :: dict(). %% dict(field_name(),ret_type())
-else.
-type substs_dict() :: dict:dict(field_name(),ret_type()).
-endif.
-type full_type_ref() :: {mod_name(),type_kind(),type_name(),
			  [ret_type()] | substs_dict()}.
-type symb_info() :: 'not_symb' | {'orig_abs',abs_type()}.
-type type_repr() :: {'abs_type',abs_type(),[var_name()],symb_info()}
		   | {'cached',fin_type(),abs_type(),symb_info()}
		   | {'abs_record',[{field_name(),abs_type()}]}.
-type gen_fun() :: fun((size()) -> fin_type()).
-type rec_fun() :: fun(([gen_fun()],size()) -> fin_type()).
-type rec_arg() :: {boolean() | {'list',boolean(),rec_fun()},full_type_ref()}.
-type rec_args() :: [rec_arg()].
-type ret_type() :: {'simple',fin_type()} | {'rec',rec_fun(),rec_args()}.
-type rec_fun_info() :: {pos_integer(),pos_integer(),[arity(),...],
			 [rec_fun(),...]}.

-type imm_type_ref() :: {type_name(),arity()}.
-type hard_adt_repr() :: {abs_type(),[var_name()]} | 'already_declared'.
-type fun_ref() :: {fun_name(),arity()}.
-type fun_repr() :: fun_clause_repr().
-type fun_clause_repr() :: {[abs_type()],abs_type()}.
-type proc_fun_ref() :: {fun_name(),[abs_type()],abs_type()}.
-type full_imm_type_ref() :: {mod_name(),type_name(),arity()}.
-type imm_stack() :: [full_imm_type_ref()].
-type pat_field() :: 0 | 1 | atom().
-type pattern() :: loose_tuple(pat_field()).
-type next_step() :: 'none' | 'take_head' | {'match_with',pattern()}.

-ifdef(NO_MODULES_IN_OPAQUES).
%% @private_type
-type mod_exp_types() :: set(). %% set(imm_type_ref())
-type mod_types() :: dict(). %% dict(type_ref(),type_repr())
%% @private_type
-type mod_exp_funs() :: set(). %% set(fun_ref())
-type mod_specs() :: dict(). %% dict(fun_ref(),fun_repr())
-else.
%% @private_type
-type mod_exp_types() :: sets:set(imm_type_ref()).
-type mod_types() :: dict:dict(type_ref(),type_repr()).
%% @private_type
-type mod_exp_funs() :: sets:set(fun_ref()).
-type mod_specs() :: dict:dict(fun_ref(),fun_repr()).
-endif.

-ifdef(NO_MODULES_IN_OPAQUES).
-record(state,
	{cached    = dict:new() :: dict(),   %% dict(imm_type(),fin_type())
	 exp_types = dict:new() :: dict(),   %% dict(mod_name(),mod_exp_types())
	 types     = dict:new() :: dict(),   %% dict(mod_name(),mod_types())
	 exp_specs = dict:new() :: dict()}). %% dict(mod_name(),mod_specs())
-else.
-record(state,
	{cached    = dict:new() :: dict:dict(imm_type(),fin_type()),
	 exp_types = dict:new() :: dict:dict(mod_name(),mod_exp_types()),
	 types     = dict:new() :: dict:dict(mod_name(),mod_types()),
	 exp_specs = dict:new() :: dict:dict(mod_name(),mod_specs())}).
-endif.
-type state() :: #state{}.

-record(mod_info,
	{mod_exp_types = sets:new() :: mod_exp_types(),
	 mod_types     = dict:new() :: mod_types(),
	 mod_opaques   = sets:new() :: mod_exp_types(),
	 mod_exp_funs  = sets:new() :: mod_exp_funs(),
	 mod_specs     = dict:new() :: mod_specs()}).
-type mod_info() :: #mod_info{}.

-type stack() :: [full_type_ref() | 'tuple' | 'list' | 'union' | 'fun'].
-ifdef(NO_MODULES_IN_OPAQUES).
-type var_dict() :: dict(). %% dict(var_name(),ret_type())
-else.
-type var_dict() :: dict:dict(var_name(),ret_type()).
-endif.
%% @private_type
-type imm_type() :: {mod_name(),string()}.
%% @alias
-type fin_type() :: proper_types:type().
-type tagged_result(T) :: {'ok',T} | 'error'.
-type tagged_result2(T,S) :: {'ok',T,S} | 'error'.
%% @alias
-type rich_result(T) :: {'ok',T} | {'error',term()}.
-type rich_result2(T,S) :: {'ok',T,S} | {'error',term()}.
-type false_positive_mfas() :: proper:false_positive_mfas().

-type server_call() :: {'create_spec_test',mfa(),timeout(),false_positive_mfas()}
		     | {'get_exp_specced',mod_name()}
		     | {'get_type_repr',mod_name(),type_ref(),boolean()}
		     | {'translate_type',imm_type()}.
-type server_response() :: rich_result(proper:test())
			 | rich_result([mfa()])
			 | rich_result(type_repr())
			 | rich_result(fin_type()).


%%------------------------------------------------------------------------------
%% Server interface functions
%%------------------------------------------------------------------------------

%% @private
-spec start() -> 'ok'.
start() ->
    {ok,TypeserverPid} = gen_server:start_link(?MODULE, dummy, []),
    put('$typeserver_pid', TypeserverPid),
    ok.

%% @private
-spec restart() -> 'ok'.
restart() ->
    TypeserverPid = get('$typeserver_pid'),
    case (TypeserverPid =:= undefined orelse not is_process_alive(TypeserverPid)) of
        true -> start();
        false -> ok
    end.

%% @private
-spec stop() -> 'ok'.
stop() ->
    TypeserverPid = get('$typeserver_pid'),
    erase('$typeserver_pid'),
    gen_server:cast(TypeserverPid, stop).

%% @private
-spec create_spec_test(mfa(), timeout(), false_positive_mfas()) -> rich_result(proper:test()).
create_spec_test(MFA, SpecTimeout, FalsePositiveMFAs) ->
    TypeserverPid = get('$typeserver_pid'),
    gen_server:call(TypeserverPid, {create_spec_test,MFA,SpecTimeout,FalsePositiveMFAs}).

%% @private
-spec get_exp_specced(mod_name()) -> rich_result([mfa()]).
get_exp_specced(Mod) ->
    TypeserverPid = get('$typeserver_pid'),
    gen_server:call(TypeserverPid, {get_exp_specced,Mod}).

-spec get_type_repr(mod_name(), type_ref(), boolean()) ->
	  rich_result(type_repr()).
get_type_repr(Mod, TypeRef, IsRemote) ->
    TypeserverPid = get('$typeserver_pid'),
    gen_server:call(TypeserverPid, {get_type_repr,Mod,TypeRef,IsRemote}).

%% @private
-spec translate_type(imm_type()) -> rich_result(fin_type()).
translate_type(ImmType) ->
    TypeserverPid = get('$typeserver_pid'),
    gen_server:call(TypeserverPid, {translate_type,ImmType}).

%% @doc Translates the native type expression `TypeExpr' (which should be
%% provided inside a string) into a PropEr type, which can then be passed to any
%% of the demo functions defined in the {@link proper_gen} module. PropEr acts
%% as if it found this type expression inside the code of module `Mod'.
-spec demo_translate_type(mod_name(), string()) -> rich_result(fin_type()).
demo_translate_type(Mod, TypeExpr) ->
    start(),
    Result = translate_type({Mod,TypeExpr}),
    stop(),
    Result.

%% @doc Checks if `Term' is a valid instance of native type `TypeExpr' (which
%% should be provided inside a string). PropEr acts as if it found this type
%% expression inside the code of module `Mod'.
-spec demo_is_instance(term(), mod_name(), string()) ->
	  boolean() | {'error',term()}.
demo_is_instance(Term, Mod, TypeExpr) ->
    case parse_type(TypeExpr) of
	{ok,TypeForm} ->
	    start(),
	    Result =
		%% Force the typeserver to load the module.
		case translate_type({Mod,"integer()"}) of
		    {ok,_FinType} ->
			try is_instance(Term, Mod, TypeForm)
			catch
			    throw:{'$typeserver',Reason} -> {error, Reason}
			end;
		    {error,_Reason} = Error ->
			Error
		end,
	    stop(),
	    Result;
	{error,_Reason} = Error ->
	    Error
    end.


%%------------------------------------------------------------------------------
%% Implementation of gen_server interface
%%------------------------------------------------------------------------------

%% @private
-spec init(_) -> {'ok',state()}.
init(_) ->
    {ok, #state{}}.

%% @private
-spec handle_call(server_call(), _, state()) ->
	  {'reply',server_response(),state()}.
handle_call({create_spec_test,MFA,SpecTimeout,FalsePositiveMFAs}, _From, State) ->
    case create_spec_test(MFA, SpecTimeout, FalsePositiveMFAs, State) of
	{ok,Test,NewState} ->
	    {reply, {ok,Test}, NewState};
	{error,_Reason} = Error ->
	    {reply, Error, State}
    end;
handle_call({get_exp_specced,Mod}, _From, State) ->
    case get_exp_specced(Mod, State) of
	{ok,MFAs,NewState} ->
	    {reply, {ok,MFAs}, NewState};
	{error,_Reason} = Error ->
	    {reply, Error, State}
    end;
handle_call({get_type_repr,Mod,TypeRef,IsRemote}, _From, State) ->
    case get_type_repr(Mod, TypeRef, IsRemote, State) of
	{ok,TypeRepr,NewState} ->
	    {reply, {ok,TypeRepr}, NewState};
	{error,_Reason} = Error ->
	    {reply, Error, State}
    end;
handle_call({translate_type,ImmType}, _From, State) ->
    case translate_type(ImmType, State) of
	{ok,FinType,NewState} ->
	    {reply, {ok,FinType}, NewState};
	{error,_Reason} = Error ->
	    {reply, Error, State}
    end.

%% @private
-spec handle_cast('stop', state()) -> {'stop','normal',state()}.
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @private
-spec handle_info(term(), state()) -> {'stop',{'received_info',term()},state()}.
handle_info(Info, State) ->
    {stop, {received_info,Info}, State}.

%% @private
-spec terminate(term(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), state(), _) -> {'ok',state()}.
code_change(_OldVsn, State, _) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Top-level interface
%%------------------------------------------------------------------------------

-spec create_spec_test(mfa(), timeout(), false_positive_mfas(), state()) ->
	  rich_result2(proper:test(),state()).
create_spec_test(MFA, SpecTimeout, FalsePositiveMFAs, State) ->
    case get_exp_spec(MFA, State) of
	{ok,FunRepr,NewState} ->
	    make_spec_test(MFA, FunRepr, SpecTimeout, FalsePositiveMFAs, NewState);
	{error,_Reason} = Error ->
	    Error
    end.

-spec get_exp_spec(mfa(), state()) -> rich_result2(fun_repr(),state()).
get_exp_spec({Mod,Fun,Arity} = MFA, State) ->
    case add_module(Mod, State) of
	{ok,#state{exp_specs = ExpSpecs} = NewState} ->
	    ModExpSpecs = dict:fetch(Mod, ExpSpecs),
	    case dict:find({Fun,Arity}, ModExpSpecs) of
		{ok,FunRepr} ->
		    {ok, FunRepr, NewState};
		error ->
		    {error, {function_not_exported_or_specced,MFA}}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec make_spec_test(mfa(), fun_repr(), timeout(), false_positive_mfas(), state()) ->
	  rich_result2(proper:test(),state()).
make_spec_test({Mod,_Fun,_Arity}=MFA, {Domain,_Range}=FunRepr, SpecTimeout, FalsePositiveMFAs, State) ->
    case convert(Mod, {type,?anno(0),'$fixed_list',Domain}, State) of
	{ok,FinType,NewState} ->
            Test = ?FORALL(Args, FinType, apply_spec_test(MFA, FunRepr, SpecTimeout, FalsePositiveMFAs, Args)),
            {ok, Test, NewState};
	{error,_Reason} = Error ->
	    Error
    end.

-spec apply_spec_test(mfa(), fun_repr(), timeout(), false_positive_mfas(), term()) -> proper:test().
apply_spec_test({Mod,Fun,_Arity}=MFA, {_Domain,Range}, SpecTimeout, FalsePositiveMFAs, Args) ->
    ?TIMEOUT(SpecTimeout,
             begin
                 %% NOTE: only call apply/3 inside try/catch (do not trust ?MODULE:is_instance/3)
                 Result =
                     try apply(Mod, Fun, Args) of
                         X -> {ok, X}
                     catch
                         X:Y:S -> {{X, Y}, S}
                     end,
                 case Result of
                     {ok, Z} ->
                         case ?MODULE:is_instance(Z, Mod, Range) of
                             true ->
                                 true;
                             false when is_function(FalsePositiveMFAs) ->
                                 FalsePositiveMFAs(MFA, Args, {fail, Z});
                             false ->
                                 false
                         end;
                     {Exception, S2} when is_function(FalsePositiveMFAs) ->
                         case FalsePositiveMFAs(MFA, Args, Exception) of
                             true ->
                                 true;
                             false ->
                                 error(Exception, S2)
                         end;
                     {Exception, S3} ->
                         error(Exception, S3)
                 end
             end).

-spec get_exp_specced(mod_name(), state()) -> rich_result2([mfa()],state()).
get_exp_specced(Mod, State) ->
    case add_module(Mod, State) of
	{ok,#state{exp_specs = ExpSpecs} = NewState} ->
	    ModExpSpecs = dict:fetch(Mod, ExpSpecs),
	    ExpSpecced = [{Mod,F,A} || {F,A} <- dict:fetch_keys(ModExpSpecs)],
	    {ok, ExpSpecced, NewState};
	{error,_Reason} = Error ->
	    Error
    end.

-spec get_type_repr(mod_name(), type_ref(), boolean(), state()) ->
	  rich_result2(type_repr(),state()).
get_type_repr(Mod, {type,Name,Arity} = TypeRef, true, State) ->
    case prepare_for_remote(Mod, Name, Arity, State) of
	{ok,NewState} ->
	    get_type_repr(Mod, TypeRef, false, NewState);
	{error,_Reason} = Error ->
	    Error
    end;
get_type_repr(Mod, TypeRef, false, #state{types = Types} = State) ->
    ModTypes = dict:fetch(Mod, Types),
    case dict:find(TypeRef, ModTypes) of
	{ok,TypeRepr} ->
	    {ok, TypeRepr, State};
	error ->
	    {error, {missing_type,Mod,TypeRef}}
    end.

-spec prepare_for_remote(mod_name(), type_name(), arity(), state()) ->
	  rich_result(state()).
prepare_for_remote(RemMod, Name, Arity, State) ->
    case add_module(RemMod, State) of
	{ok,#state{exp_types = ExpTypes} = NewState} ->
	    RemModExpTypes = dict:fetch(RemMod, ExpTypes),
	    case sets:is_element({Name,Arity}, RemModExpTypes) of
		true  -> {ok, NewState};
		false -> {error, {type_not_exported,{RemMod,Name,Arity}}}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec translate_type(imm_type(), state()) -> rich_result2(fin_type(),state()).
translate_type({Mod,Str} = ImmType, #state{cached = Cached} = State) ->
    case dict:find(ImmType, Cached) of
	{ok,Type} ->
	    {ok, Type, State};
	error ->
	    case parse_type(Str) of
		{ok,TypeForm} ->
		    case add_module(Mod, State) of
			{ok,NewState} ->
			    case convert(Mod, TypeForm, NewState) of
				{ok,FinType,
				 #state{cached = Cached} = FinalState} ->
				    NewCached = dict:store(ImmType, FinType,
							   Cached),
				    {ok, FinType,
				     FinalState#state{cached = NewCached}};
				{error,_Reason} = Error ->
				    Error
			    end;
			{error,_Reason} = Error ->
			    Error
		    end;
		{error,Reason} ->
		    {error, {parse_error,Str,Reason}}
	    end
    end.

-spec parse_type(string()) -> rich_result(abs_type()).
parse_type(Str) ->
    TypeStr = "-type mytype() :: " ++ Str ++ ".",
    case erl_scan:string(TypeStr) of
	{ok,Tokens,_EndLocation} ->
	    case erl_parse:parse_form(Tokens) of
		{ok,{attribute,_Line,type,{mytype,TypeExpr,[]}}} ->
		    {ok, TypeExpr};
		{error,_ErrorInfo} = Error ->
		    Error
	    end;
	{error,ErrorInfo,_EndLocation} ->
	    {error, ErrorInfo}
    end.

-spec add_module(mod_name(), state()) -> rich_result(state()).
add_module(Mod, #state{exp_types = ExpTypes} = State) ->
    case dict:is_key(Mod, ExpTypes) of
	true ->
	    {ok, State};
	false ->
	    case get_code_and_exports(Mod) of
		{ok,AbsCode,ModExpFuns} ->
		    RawModInfo = get_mod_info(Mod, AbsCode, ModExpFuns),
		    ModInfo = process_adts(Mod, RawModInfo),
		    {ok, store_mod_info(Mod, ModInfo, State)};
		{error,Reason} ->
		    {error, {cant_load_code,Mod,Reason}}
	    end
    end.

%% @private
-spec get_exp_info(mod_name()) -> rich_result2(mod_exp_types(),mod_exp_funs()).
get_exp_info(Mod) ->
    case get_code_and_exports(Mod) of
	{ok,AbsCode,ModExpFuns} ->
	    RawModInfo = get_mod_info(Mod, AbsCode, ModExpFuns),
	    {ok, RawModInfo#mod_info.mod_exp_types, ModExpFuns};
	{error,_Reason} = Error ->
	    Error
    end.

-spec get_code_and_exports(mod_name()) ->
	  rich_result2([abs_form()],mod_exp_funs()).
get_code_and_exports(Mod) ->
    case code:get_object_code(Mod) of
	{Mod, ObjBin, _ObjFileName} ->
	    case get_chunks(ObjBin) of
		{ok,_AbsCode,_ModExpFuns} = Result ->
		    Result;
		{error,Reason} ->
		    get_code_and_exports_from_source(Mod, Reason)
	    end;
	error ->
	    get_code_and_exports_from_source(Mod, cant_find_object_file)
    end.

-spec get_code_and_exports_from_source(mod_name(), term()) ->
	  rich_result2([abs_form()],mod_exp_funs()).
get_code_and_exports_from_source(Mod, ObjError) ->
    SrcFileName = atom_to_list(Mod) ++ ?SRC_FILE_EXT,
    case code:where_is_file(SrcFileName) of
	FullSrcFileName when is_list(FullSrcFileName) ->
	    Opts = [binary,debug_info,return_errors,{d,'PROPER_REMOVE_PROPS'}],
	    case compile:file(FullSrcFileName, Opts) of
		{ok,Mod,Binary} ->
		    get_chunks(Binary);
		{error,Errors,_Warnings} ->
		    {error, {ObjError,{cant_compile_source_file,Errors}}}
	    end;
	non_existing ->
	    {error, {ObjError,cant_find_source_file}}
    end.

-spec get_chunks(string() | binary()) ->
	  rich_result2([abs_form()],mod_exp_funs()).
get_chunks(ObjFile) ->
    case beam_lib:chunks(ObjFile, [abstract_code,exports]) of
	{ok,{_Mod,[{abstract_code,AbsCodeChunk},{exports,ExpFunsList}]}} ->
	    case AbsCodeChunk of
		{raw_abstract_v1,AbsCode} ->
		    %% HACK: Add a declaration for iolist() to every module
		    {ok, add_iolist(AbsCode), sets:from_list(ExpFunsList)};
		no_abstract_code ->
		    {error, no_abstract_code};
		_ ->
		    {error, unsupported_abstract_code_format}
	    end;
	{error,beam_lib,Reason} ->
	    {error, Reason}
    end.

-spec add_iolist([abs_form()]) -> [abs_form()].
add_iolist(Forms) ->
    IOListDef =
	{type,0,maybe_improper_list,
	 [{type,0,union,[{type,0,byte,[]},{type,0,binary,[]},
			 {type,0,iolist,[]}]},
	  {type,0,binary,[]}]},
    IOListDecl = {attribute,0,type,{iolist,IOListDef,[]}},
    [IOListDecl | Forms].

-spec get_mod_info(mod_name(), [abs_form()], mod_exp_funs()) -> mod_info().
get_mod_info(Mod, AbsCode, ModExpFuns) ->
    StartModInfo = #mod_info{mod_exp_funs = ModExpFuns},
    ImmModInfo = lists:foldl(fun add_mod_info/2, StartModInfo, AbsCode),
    #mod_info{mod_specs = AllModSpecs} = ImmModInfo,
    IsExported = fun(FunRef,_FunRepr) -> sets:is_element(FunRef,ModExpFuns) end,
    ModExpSpecs = dict:filter(IsExported, AllModSpecs),
    ModInfo = ImmModInfo#mod_info{mod_specs = ModExpSpecs},
    case orddict:find(Mod, ?HARD_ADT_MODS) of
	{ok,ModADTs} ->
	    #mod_info{mod_exp_types = ModExpTypes, mod_types = ModTypes,
		      mod_opaques = ModOpaques} = ModInfo,
	    ModADTsSet =
		sets:from_list([ImmTypeRef
				|| {ImmTypeRef,_HardADTRepr} <- ModADTs]),
	    NewModExpTypes = sets:union(ModExpTypes, ModADTsSet),
	    NewModTypes = lists:foldl(fun store_hard_adt/2, ModTypes, ModADTs),
	    NewModOpaques = sets:union(ModOpaques, ModADTsSet),
	    ModInfo#mod_info{mod_exp_types = NewModExpTypes,
			     mod_types = NewModTypes,
			     mod_opaques = NewModOpaques};
	error ->
	    ModInfo
    end.

-spec store_hard_adt({imm_type_ref(),hard_adt_repr()}, mod_types()) ->
	  mod_types().
store_hard_adt({_ImmTypeRef,already_declared}, ModTypes) ->
    ModTypes;
store_hard_adt({{Name,Arity},{TypeForm,VarNames}}, ModTypes) ->
    TypeRef = {type,Name,Arity},
    TypeRepr = {abs_type,TypeForm,VarNames,not_symb},
    dict:store(TypeRef, TypeRepr, ModTypes).

-spec add_mod_info(abs_form(), mod_info()) -> mod_info().
add_mod_info({attribute,_Line,export_type,TypesList},
	     #mod_info{mod_exp_types = ModExpTypes} = ModInfo) ->
    NewModExpTypes = sets:union(sets:from_list(TypesList), ModExpTypes),
    ModInfo#mod_info{mod_exp_types = NewModExpTypes};
add_mod_info({attribute,_Line,type,{{record,RecName},Fields,[]}},
	     #mod_info{mod_types = ModTypes} = ModInfo) ->
    FieldInfo = [process_rec_field(F) || F <- Fields],
    NewModTypes = dict:store({record,RecName,0}, {abs_record,FieldInfo},
			     ModTypes),
    ModInfo#mod_info{mod_types = NewModTypes};
add_mod_info({attribute,Line,record,{RecName,Fields}},
	     #mod_info{mod_types = ModTypes} = ModInfo) ->
    case dict:is_key(RecName, ModTypes) of
	true ->
	    ModInfo;
	false ->  % fake an opaque term by using the same Line as annotation
	    TypedRecord = {attribute,Line,type,{{record,RecName},Fields,[]}},
	    add_mod_info(TypedRecord, ModInfo)
    end;
add_mod_info({attribute,_Line,Kind,{Name,TypeForm,VarForms}},
	     #mod_info{mod_types = ModTypes,
		       mod_opaques = ModOpaques} = ModInfo)
	when Kind =:= type; Kind =:= opaque ->
    Arity = length(VarForms),
    VarNames = [V || {var,_,V} <- VarForms],
    %% TODO: No check whether variables are different, or non-'_'.
    NewModTypes = dict:store({type,Name,Arity},
			     {abs_type,TypeForm,VarNames,not_symb}, ModTypes),
    NewModOpaques =
	case Kind of
	    type   -> ModOpaques;
	    opaque -> sets:add_element({Name,Arity}, ModOpaques)
	end,
    ModInfo#mod_info{mod_types = NewModTypes, mod_opaques = NewModOpaques};
add_mod_info({attribute,_Line,spec,{RawFunRef,[RawFirstClause | _Rest]}},
	     #mod_info{mod_specs = ModSpecs} = ModInfo) ->
    FunRef = case RawFunRef of
		 {_Mod,Name,Arity}  -> {Name,Arity};
		 {_Name,_Arity} = F -> F
	     end,
    %% TODO: We just take the first function clause.
    FirstClause = process_fun_clause(RawFirstClause),
    NewModSpecs = dict:store(FunRef, FirstClause, ModSpecs),
    ModInfo#mod_info{mod_specs = NewModSpecs};
add_mod_info(_Form, ModInfo) ->
    ModInfo.

-spec process_rec_field(abs_rec_field()) -> {field_name(),abs_type()}.
process_rec_field({record_field,_,{atom,_,FieldName}}) ->
    {FieldName, {type,0,any,[]}};
process_rec_field({record_field,_,{atom,_,FieldName},_Initialization}) ->
    {FieldName, {type,0,any,[]}};
process_rec_field({typed_record_field,RecField,FieldType}) ->
    {FieldName,_} = process_rec_field(RecField),
    {FieldName, FieldType}.

-spec process_fun_clause(abs_type()) -> fun_clause_repr().
process_fun_clause({type,_,'fun',[{type,_,product,Domain},Range]}) ->
    {Domain, Range};
process_fun_clause({type,_,bounded_fun,[MainClause,Constraints]}) ->
    {RawDomain,RawRange} = process_fun_clause(MainClause),
    VarSubsts = [{V,T} || {type,_,constraint,
			   [{atom,_,is_subtype},[{var,_,V},T]]} <- Constraints,
			  V =/= '_'],
    VarSubstsDict = dict:from_list(VarSubsts),
    Domain = [update_vars(A, VarSubstsDict, false) || A <- RawDomain],
    Range = update_vars(RawRange, VarSubstsDict, false),
    {Domain, Range}.

-spec store_mod_info(mod_name(), mod_info(), state()) -> state().
store_mod_info(Mod, #mod_info{mod_exp_types = ModExpTypes, mod_types = ModTypes,
			      mod_specs = ImmModExpSpecs},
	       #state{exp_types = ExpTypes, types = Types,
		      exp_specs = ExpSpecs} = State) ->
    NewExpTypes = dict:store(Mod, ModExpTypes, ExpTypes),
    NewTypes = dict:store(Mod, ModTypes, Types),
    ModExpSpecs = dict:map(fun unbound_to_any/2, ImmModExpSpecs),
    NewExpSpecs = dict:store(Mod, ModExpSpecs, ExpSpecs),
    State#state{exp_types = NewExpTypes, types = NewTypes,
		exp_specs = NewExpSpecs}.

-spec unbound_to_any(fun_ref(), fun_repr()) -> fun_repr().
unbound_to_any(_FunRef, {Domain,Range}) ->
    EmptySubstsDict = dict:new(),
    NewDomain = [update_vars(A,EmptySubstsDict,true) || A <- Domain],
    NewRange = update_vars(Range, EmptySubstsDict, true),
    {NewDomain, NewRange}.


%%------------------------------------------------------------------------------
%% ADT translation functions
%%------------------------------------------------------------------------------

-spec process_adts(mod_name(), mod_info()) -> mod_info().
process_adts(Mod,
	     #mod_info{mod_exp_types = ModExpTypes, mod_opaques = ModOpaques,
		       mod_specs = ModExpSpecs} = ModInfo) ->
    %% TODO: No warning on unexported opaques.
    case sets:to_list(sets:intersection(ModExpTypes,ModOpaques)) of
	[] ->
	    ModInfo;
	ModADTs ->
	    %% TODO: No warning on unexported API functions.
	    ModExpSpecsList = [{Name,Domain,Range}
			       || {{Name,_Arity},{Domain,Range}}
				  <- dict:to_list(ModExpSpecs)],
	    AddADT = fun(ADT,Acc) -> add_adt(Mod,ADT,Acc,ModExpSpecsList) end,
	    lists:foldl(AddADT, ModInfo, ModADTs)
    end.

-spec add_adt(mod_name(), imm_type_ref(), mod_info(), [proc_fun_ref()]) ->
	  mod_info().
add_adt(Mod, {Name,Arity}, #mod_info{mod_types = ModTypes} = ModInfo,
	ModExpFunSpecs) ->
    ADTRef = {type,Name,Arity},
    {abs_type,InternalRepr,VarNames,not_symb} = dict:fetch(ADTRef, ModTypes),
    FullADTRef = {Mod,Name,Arity},
    %% TODO: No warning on unsuitable range.
    SymbCalls1 = [get_symb_call(FullADTRef,Spec) || Spec <- ModExpFunSpecs],
    %% TODO: No warning on bad use of variables.
    SymbCalls2 = [fix_vars(FullADTRef,Call,RangeVars,VarNames)
		  || {ok,Call,RangeVars} <- SymbCalls1],
    case [Call || {ok,Call} <- SymbCalls2] of
	[] ->
	    %% TODO: No warning on no acceptable spec.
	    ModInfo;
	SymbCalls3 ->
	    NewADTRepr = {abs_type,{type,0,union,SymbCalls3},VarNames,
			  {orig_abs,InternalRepr}},
	    NewModTypes = dict:store(ADTRef, NewADTRepr, ModTypes),
	    ModInfo#mod_info{mod_types = NewModTypes}
    end.

-spec get_symb_call(full_imm_type_ref(), proc_fun_ref()) ->
	  tagged_result2(abs_type(),[var_name()]).
get_symb_call({Mod,_TypeName,_Arity} = FullADTRef, {FunName,Domain,Range}) ->
    A = ?anno(0),
    BaseCall = {type,A,tuple,[{atom,A,'$call'},{atom,A,Mod},{atom,A,FunName},
			      {type,A,'$fixed_list',Domain}]},
    unwrap_range(FullADTRef, BaseCall, Range, false).

-spec unwrap_range(full_imm_type_ref(), abs_type() | next_step(), abs_type(),
		   boolean()) ->
	  tagged_result2(abs_type() | next_step(),[var_name()]).
unwrap_range(FullADTRef, Call, {paren_type,_,[Type]}, TestRun) ->
    unwrap_range(FullADTRef, Call, Type, TestRun);
unwrap_range(FullADTRef, Call, {ann_type,_,[_Var,Type]}, TestRun) ->
    unwrap_range(FullADTRef, Call, Type, TestRun);
unwrap_range(FullADTRef, Call, {type,_,list,[ElemType]}, TestRun) ->
    unwrap_list(FullADTRef, Call, ElemType, TestRun);
unwrap_range(FullADTRef, Call, {type,_,maybe_improper_list,[Cont,_Term]},
	     TestRun) ->
    unwrap_list(FullADTRef, Call, Cont, TestRun);
unwrap_range(FullADTRef, Call, {type,_,nonempty_list,[ElemType]}, TestRun) ->
    unwrap_list(FullADTRef, Call, ElemType, TestRun);
unwrap_range(FullADTRef, Call, {type,_,nonempty_improper_list,[Cont,_Term]},
	     TestRun) ->
    unwrap_list(FullADTRef, Call, Cont, TestRun);
unwrap_range(FullADTRef, Call,
	     {type,_,nonempty_maybe_improper_list,[Cont,_Term]}, TestRun) ->
    unwrap_list(FullADTRef, Call, Cont, TestRun);
unwrap_range(_FullADTRef, _Call, {type,_,tuple,any}, _TestRun) ->
    error;
unwrap_range(FullADTRef, Call, {type,_,tuple,FieldForms}, TestRun) ->
    Translates = fun(T) -> unwrap_range(FullADTRef,none,T,true) =/= error end,
    case proper_arith:find_first(Translates, FieldForms) of
	none ->
	    error;
	{TargetPos,TargetElem} ->
	    Pattern = get_pattern(TargetPos, FieldForms),
	    case TestRun of
		true ->
		    NewCall =
			case Call of
			    none -> {match_with,Pattern};
			    _    -> Call
			end,
		    {ok, NewCall, []};
		false ->
		    AbsPattern = term_to_singleton_type(Pattern),
		    A = ?anno(0),
		    NewCall =
			{type,A,tuple,
			 [{atom,A,'$call'},{atom,A,?MODULE},{atom,A,match},
			  {type,A,'$fixed_list',[AbsPattern,Call]}]},
		    unwrap_range(FullADTRef, NewCall, TargetElem, TestRun)
	    end
    end;
unwrap_range(FullADTRef, Call, {type,_,union,Choices}, TestRun) ->
    TestedChoices = [unwrap_range(FullADTRef,none,C,true) || C <- Choices],
    NotError = fun(error) -> false; (_) -> true end,
    case proper_arith:find_first(NotError, TestedChoices) of
	none ->
	    error;
	{_ChoicePos,{ok,none,_RangeVars}} ->
	    error;
	{ChoicePos,{ok,NextStep,_RangeVars}} ->
	    {A, [ChoiceElem|B]} = lists:split(ChoicePos-1, Choices),
	    OtherChoices = A ++ B,
	    DistinctChoice =
		case NextStep of
		    take_head ->
			fun cant_have_head/1;
		    {match_with,Pattern} ->
			fun(C) -> cant_match(Pattern, C) end
		end,
	    case {lists:all(DistinctChoice,OtherChoices), TestRun} of
		{true,true} ->
		    {ok, NextStep, []};
		{true,false} ->
		    unwrap_range(FullADTRef, Call, ChoiceElem, TestRun);
		{false,_} ->
		    error
	    end
    end;
unwrap_range({_Mod,SameName,Arity}, Call, {type,_,SameName,ArgForms},
	     _TestRun) ->
    RangeVars = [V || {var,_,V} <- ArgForms, V =/= '_'],
    case length(ArgForms) =:= Arity andalso length(RangeVars) =:= Arity of
	true  -> {ok, Call, RangeVars};
	false -> error
    end;
unwrap_range({SameMod,SameName,_Arity} = FullADTRef, Call,
	     {remote_type,_,[{atom,_,SameMod},{atom,_,SameName},ArgForms]},
	     TestRun) ->
    unwrap_range(FullADTRef, Call, {type,?anno(0),SameName,ArgForms}, TestRun);
unwrap_range(_FullADTRef, _Call, _Range, _TestRun) ->
    error.

-spec unwrap_list(full_imm_type_ref(), abs_type() | next_step(), abs_type(),
		  boolean()) ->
	  tagged_result2(abs_type() | next_step(),[var_name()]).
unwrap_list(FullADTRef, Call, HeadType, TestRun) ->
    NewCall =
	case TestRun of
	    true ->
		case Call of
		    none -> take_head;
		    _    -> Call
		end;
	    false ->
		{type,0,tuple,[{atom,0,'$call'},{atom,0,erlang},{atom,0,hd},
			       {type,0,'$fixed_list',[Call]}]}
	end,
    unwrap_range(FullADTRef, NewCall, HeadType, TestRun).

-spec fix_vars(full_imm_type_ref(), abs_type(), [var_name()], [var_name()]) ->
	  tagged_result(abs_type()).
fix_vars(FullADTRef, Call, RangeVars, VarNames) ->
    NotAnyVar = fun(V) -> V =/= '_' end,
    case no_duplicates(VarNames) andalso lists:all(NotAnyVar,VarNames) of
	true ->
	    RawUsedVars =
		collect_vars(FullADTRef, Call, [[V] || V <- RangeVars]),
	    UsedVars = [lists:usort(L) || L <- RawUsedVars],
	    case correct_var_use(UsedVars) of
		true ->
		    PairAll = fun(L,Y) -> [{X,{var,0,Y}} || X <- L] end,
		    VarSubsts =
			lists:flatten(lists:zipwith(PairAll,UsedVars,VarNames)),
		    VarSubstsDict = dict:from_list(VarSubsts),
		    {ok, update_vars(Call,VarSubstsDict,true)};
		false ->
		    error
	    end;
	false ->
	    error
    end.

-spec no_duplicates(list()) -> boolean().
no_duplicates(L) ->
    length(lists:usort(L)) =:= length(L).

-spec correct_var_use([[var_name() | 0]]) -> boolean().
correct_var_use(UsedVars) ->
    NoNonVarArgs = fun([0|_]) -> false; (_) -> true end,
    lists:all(NoNonVarArgs, UsedVars)
    andalso no_duplicates(lists:flatten(UsedVars)).

-spec collect_vars(full_imm_type_ref(), abs_type(), [[var_name() | 0]]) ->
	  [[var_name() | 0]].
collect_vars(FullADTRef, {paren_type,_,[Type]}, UsedVars) ->
    collect_vars(FullADTRef, Type, UsedVars);
collect_vars(FullADTRef, {ann_type,_,[_Var,Type]}, UsedVars) ->
    collect_vars(FullADTRef, Type, UsedVars);
collect_vars(_FullADTRef, {type,_,tuple,any}, UsedVars) ->
    UsedVars;
collect_vars({_Mod,SameName,Arity} = FullADTRef, {type,_,SameName,ArgForms},
	     UsedVars) ->
    case length(ArgForms) =:= Arity of
	true ->
	    VarArgs = [V || {var,_,V} <- ArgForms, V =/= '_'],
	    case length(VarArgs) =:= Arity of
		true ->
		    AddToList = fun(X,L) -> [X | L] end,
		    lists:zipwith(AddToList, VarArgs, UsedVars);
		false ->
		    [[0|L] || L <- UsedVars]
	    end;
	false ->
	    multi_collect_vars(FullADTRef, ArgForms, UsedVars)
    end;
collect_vars(FullADTRef, {type,_,_Name,ArgForms}, UsedVars) ->
    multi_collect_vars(FullADTRef, ArgForms, UsedVars);
collect_vars({SameMod,SameName,_Arity} = FullADTRef,
	     {remote_type,_,[{atom,_,SameMod},{atom,_,SameName},ArgForms]},
	     UsedVars) ->
    collect_vars(FullADTRef, {type,?anno(0),SameName,ArgForms}, UsedVars);
collect_vars(FullADTRef, {remote_type,_,[_RemModForm,_NameForm,ArgForms]},
	     UsedVars) ->
    multi_collect_vars(FullADTRef, ArgForms, UsedVars);
collect_vars(_FullADTRef, _Call, UsedVars) ->
    UsedVars.

-spec multi_collect_vars(full_imm_type_ref(), [abs_type()],
			 [[var_name() | 0]]) -> [[var_name() | 0]].
multi_collect_vars({_Mod,_Name,Arity} = FullADTRef, Forms, UsedVars) ->
    NoUsedVars = lists:duplicate(Arity, []),
    MoreUsedVars = [collect_vars(FullADTRef,T,NoUsedVars) || T <- Forms],
    CombineVars = fun(L1,L2) -> lists:zipwith(fun erlang:'++'/2, L1, L2) end,
    lists:foldl(CombineVars, UsedVars, MoreUsedVars).

-ifdef(NO_MODULES_IN_OPAQUES).
-type var_substs_dict() :: dict().
-else.
-type var_substs_dict() :: dict:dict(var_name(),abs_type()).
-endif.
-spec update_vars(abs_type(), var_substs_dict(), boolean()) -> abs_type().
update_vars({paren_type,Line,[Type]}, VarSubstsDict, UnboundToAny) ->
    {paren_type, Line, [update_vars(Type,VarSubstsDict,UnboundToAny)]};
update_vars({ann_type,Line,[Var,Type]}, VarSubstsDict, UnboundToAny) ->
    {ann_type, Line, [Var,update_vars(Type,VarSubstsDict,UnboundToAny)]};
update_vars({var,Line,VarName} = Call, VarSubstsDict, UnboundToAny) ->
    case dict:find(VarName, VarSubstsDict) of
	{ok,SubstType} ->
	    SubstType;
	error when UnboundToAny =:= false ->
	    Call;
	error when UnboundToAny =:= true ->
	    {type,Line,any,[]}
    end;
update_vars({remote_type,Line,[RemModForm,NameForm,ArgForms]}, VarSubstsDict,
	    UnboundToAny) ->
    NewArgForms = [update_vars(A,VarSubstsDict,UnboundToAny) || A <- ArgForms],
    {remote_type, Line, [RemModForm,NameForm,NewArgForms]};
update_vars({T,_,tuple,any} = Call, _VarSubstsDict, _UnboundToAny) when ?IS_TYPE_TAG(T) ->
    Call;
update_vars({T,Line,Name,ArgForms}, VarSubstsDict, UnboundToAny) when ?IS_TYPE_TAG(T) ->
    NewArgForms = [update_vars(A,VarSubstsDict,UnboundToAny) || A <- ArgForms],
    {T, Line, Name, NewArgForms};
update_vars(Call, _VarSubstsDict, _UnboundToAny) ->
    Call.


%%------------------------------------------------------------------------------
%% Match-related functions
%%------------------------------------------------------------------------------

-spec get_pattern(position(), [abs_type()]) -> pattern().
get_pattern(TargetPos, FieldForms) ->
    {0,RevPattern} = lists:foldl(fun add_field/2, {TargetPos,[]}, FieldForms),
    list_to_tuple(lists:reverse(RevPattern)).

-spec add_field(abs_type(), {non_neg_integer(),[pat_field()]}) ->
	  {non_neg_integer(),[pat_field(),...]}.
add_field(_Type, {1,Acc}) ->
    {0, [1|Acc]};
add_field({atom,_,Tag}, {Left,Acc}) ->
    {erlang:max(0,Left-1), [Tag|Acc]};
add_field(_Type, {Left,Acc}) ->
    {erlang:max(0,Left-1), [0|Acc]}.

%% @private
-spec match(pattern(), tuple()) -> term().
match(Pattern, Term) when tuple_size(Pattern) =:= tuple_size(Term) ->
    match(tuple_to_list(Pattern), tuple_to_list(Term), none, false);
match(_Pattern, _Term) ->
    throw(no_match).

-spec match([pat_field()], [term()], 'none' | {'ok',T}, boolean()) -> T.
match([], [], {ok,Target}, _TypeMode) ->
    Target;
match([0|PatRest], [_|ToMatchRest], Acc, TypeMode) ->
    match(PatRest, ToMatchRest, Acc, TypeMode);
match([1|PatRest], [Target|ToMatchRest], none, TypeMode) ->
    match(PatRest, ToMatchRest, {ok,Target}, TypeMode);
match([Tag|PatRest], [X|ToMatchRest], Acc, TypeMode) when is_atom(Tag) ->
    MatchesTag =
	case TypeMode of
	    true  -> can_be_tag(Tag, X);
	    false -> Tag =:= X
	end,
    case MatchesTag of
	true  -> match(PatRest, ToMatchRest, Acc, TypeMode);
	false -> throw(no_match)
    end.

%% CAUTION: these must be sorted
-define(NON_ATOM_TYPES,
	[arity,binary,bitstring,byte,char,float,'fun',function,integer,iodata,
	 iolist,list,maybe_improper_list,mfa,neg_integer,nil,no_return,
	 non_neg_integer,none,nonempty_improper_list,nonempty_list,
	 nonempty_maybe_improper_list,nonempty_string,number,pid,port,
	 pos_integer,range,record,reference,string,tuple]).
-define(NON_TUPLE_TYPES,
	[arity,atom,binary,bitstring,bool,boolean,byte,char,float,'fun',
	 function,identifier,integer,iodata,iolist,list,maybe_improper_list,
	 neg_integer,nil,no_return,node,non_neg_integer,none,
	 nonempty_improper_list,nonempty_list,nonempty_maybe_improper_list,
	 nonempty_string,number,pid,port,pos_integer,range,reference,string,
	 timeout]).
-define(NO_HEAD_TYPES,
	[arity,atom,binary,bitstring,bool,boolean,byte,char,float,'fun',
	 function,identifier,integer,mfa,module,neg_integer,nil,no_return,node,
	 non_neg_integer,none,number,pid,port,pos_integer,range,record,
	 reference,timeout,tuple]).

-spec can_be_tag(atom(), abs_type()) -> boolean().
can_be_tag(Tag, {ann_type,_,[_Var,Type]}) ->
    can_be_tag(Tag, Type);
can_be_tag(Tag, {paren_type,_,[Type]}) ->
    can_be_tag(Tag, Type);
can_be_tag(Tag, {atom,_,Atom}) ->
    Tag =:= Atom;
can_be_tag(_Tag, {integer,_,_Int}) ->
    false;
can_be_tag(_Tag, {op,_,_Op,_Arg}) ->
    false;
can_be_tag(_Tag, {op,_,_Op,_Arg1,_Arg2}) ->
    false;
can_be_tag(Tag, {type,_,BName,[]}) when BName =:= bool; BName =:= boolean ->
    is_boolean(Tag);
can_be_tag(Tag, {type,_,timeout,[]}) ->
    Tag =:= infinity;
can_be_tag(Tag, {type,_,union,Choices}) ->
    lists:any(fun(C) -> can_be_tag(Tag,C) end, Choices);
can_be_tag(_Tag, {type,_,Name,_Args}) ->
    not ordsets:is_element(Name, ?NON_ATOM_TYPES);
can_be_tag(_Tag, _Type) ->
    true.

-spec cant_match(pattern(), abs_type()) -> boolean().
cant_match(Pattern, {ann_type,_,[_Var,Type]}) ->
    cant_match(Pattern, Type);
cant_match(Pattern, {paren_type,_,[Type]}) ->
    cant_match(Pattern, Type);
cant_match(_Pattern, {atom,_,_Atom}) ->
    true;
cant_match(_Pattern, {integer,_,_Int}) ->
    true;
cant_match(_Pattern, {op,_,_Op,_Arg}) ->
    true;
cant_match(_Pattern, {op,_,_Op,_Arg1,_Arg2}) ->
    true;
cant_match(Pattern, {type,Anno,mfa,[]}) ->
    MFA_Ts = [{type,Anno,atom,[]}, {type,Anno,atom,[]}, {type,Anno,arity,[]}],
    cant_match(Pattern, {type,Anno,tuple,MFA_Ts});
cant_match(Pattern, {type,_,union,Choices}) ->
    lists:all(fun(C) -> cant_match(Pattern,C) end, Choices);
cant_match(_Pattern, {type,_,tuple,any}) ->
    false;
cant_match(Pattern, {type,_,tuple,Fields}) ->
    tuple_size(Pattern) =/= length(Fields) orelse
    try match(tuple_to_list(Pattern), Fields, none, true) of
	_ -> false
    catch
	throw:no_match -> true
    end;
cant_match(_Pattern, {type,_,Name,_Args}) ->
    ordsets:is_element(Name, ?NON_TUPLE_TYPES);
cant_match(_Pattern, _Type) ->
    false.

-spec cant_have_head(abs_type()) -> boolean().
cant_have_head({ann_type,_,[_Var,Type]}) ->
    cant_have_head(Type);
cant_have_head({paren_type,_,[Type]}) ->
    cant_have_head(Type);
cant_have_head({atom,_,_Atom}) ->
    true;
cant_have_head({integer,_,_Int}) ->
    true;
cant_have_head({op,_,_Op,_Arg}) ->
    true;
cant_have_head({op,_,_Op,_Arg1,_Arg2}) ->
    true;
cant_have_head({type,_,union,Choices}) ->
    lists:all(fun cant_have_head/1, Choices);
cant_have_head({type,_,Name,_Args}) ->
    ordsets:is_element(Name, ?NO_HEAD_TYPES);
cant_have_head(_Type) ->
    false.

%% Only covers atoms, integers and tuples, i.e. those that can be specified
%% through singleton types.
-spec term_to_singleton_type(atom() | integer()
			     | loose_tuple(atom() | integer())) -> abs_type().
term_to_singleton_type(Atom) when is_atom(Atom) ->
    {atom,?anno(0),Atom};
term_to_singleton_type(Int) when is_integer(Int), Int >= 0 ->
    {integer,?anno(0),Int};
term_to_singleton_type(Int) when is_integer(Int), Int < 0 ->
    A = ?anno(0),
    {op,A,'-',{integer,A,-Int}};
term_to_singleton_type(Tuple) when is_tuple(Tuple) ->
    Fields = tuple_to_list(Tuple),
    {type,?anno(0),tuple,[term_to_singleton_type(F) || F <- Fields]}.


%%------------------------------------------------------------------------------
%% Instance testing functions
%%------------------------------------------------------------------------------

%% CAUTION: this must be sorted
-define(EQUIV_TYPES,
	[{arity, {type,0,range,[{integer,0,0},{integer,0,255}]}},
	 {bool, {type,0,boolean,[]}},
	 {byte, {type,0,range,[{integer,0,0},{integer,0,255}]}},
	 {char, {type,0,range,[{integer,0,0},{integer,0,16#10ffff}]}},
	 {function, {type,0,'fun',[]}},
	 {identifier, {type,0,union,[{type,0,pid,[]},{type,0,port,[]},
				     {type,0,reference,[]}]}},
	 {iodata, {type,0,union,[{type,0,binary,[]},{type,0,iolist,[]}]}},
	 {iolist, {type,0,maybe_improper_list,
		   [{type,0,union,[{type,0,byte,[]},{type,0,binary,[]},
				   {type,0,iolist,[]}]},
		    {type,0,binary,[]}]}},
	 {list, {type,0,list,[{type,0,any,[]}]}},
	 {maybe_improper_list, {type,0,maybe_improper_list,[{type,0,any,[]},
							    {type,0,any,[]}]}},
	 {mfa, {type,0,tuple,[{type,0,atom,[]},{type,0,atom,[]},
			      {type,0,arity,[]}]}},
	 {node, {type,0,atom,[]}},
	 {nonempty_list, {type,0,nonempty_list,[{type,0,any,[]}]}},
	 {nonempty_maybe_improper_list, {type,0,nonempty_maybe_improper_list,
					 [{type,0,any,[]},{type,0,any,[]}]}},
	 {nonempty_string, {type,0,nonempty_list,[{type,0,char,[]}]}},
	 {string, {type,0,list,[{type,0,char,[]}]}},
	 {term, {type,0,any,[]}},
	 {timeout, {type,0,union,[{atom,0,infinity},
				  {type,0,non_neg_integer,[]}]}}]).

%% @private
%% TODO: Most of these functions accept an extended form of abs_type(), namely
%%	 the addition of a custom wrapper: {'from_mod',mod_name(),...}
-spec is_instance(term(), mod_name(), abs_type()) -> boolean().
is_instance(X, Mod, TypeForm) ->
    is_instance(X, Mod, TypeForm, []).

-spec is_instance(term(), mod_name(), abs_type(), imm_stack()) -> boolean().
is_instance(X, _Mod, {from_mod,OrigMod,Type}, Stack) ->
    is_instance(X, OrigMod, Type, Stack);
is_instance(_X, _Mod, {var,_,'_'}, _Stack) ->
    true;
is_instance(_X, _Mod, {var,_,Name}, _Stack) ->
    %% All unconstrained spec vars have been replaced by 'any()' and we always
    %% replace the variables on the RHS of types before recursing into them.
    %% Provided that '-type' declarations contain no unbound variables, we
    %% don't expect to find any non-'_' variables while recursing.
    throw({'$typeserver',{unbound_var_in_type_declaration,Name}});
is_instance(X, Mod, {ann_type,_,[_Var,Type]}, Stack) ->
    is_instance(X, Mod, Type, Stack);
is_instance(X, Mod, {paren_type,_,[Type]}, Stack) ->
    is_instance(X, Mod, Type, Stack);
is_instance(X, Mod, {remote_type,_,[{atom,_,RemMod},{atom,_,Name},ArgForms]},
	    Stack) ->
    is_custom_instance(X, Mod, RemMod, Name, ArgForms, true, Stack);
is_instance(SameAtom, _Mod, {atom,_,SameAtom}, _Stack) ->
    true;
is_instance(SameInt, _Mod, {integer,_,SameInt}, _Stack) ->
    true;
is_instance(X, _Mod, {op,_,_Op,_Arg} = Expr, _Stack) ->
    is_int_const(X, Expr);
is_instance(X, _Mod, {op,_,_Op,_Arg1,_Arg2} = Expr, _Stack) ->
    is_int_const(X, Expr);
is_instance(_X, _Mod, {type,_,any,[]}, _Stack) ->
    true;
is_instance(X, _Mod, {type,_,atom,[]}, _Stack) ->
    is_atom(X);
is_instance(X, _Mod, {type,_,binary,[]}, _Stack) ->
    is_binary(X);
is_instance(X, _Mod, {type,_,binary,[BaseExpr,UnitExpr]}, _Stack) ->
    %% <<_:X,_:_*Y>> means "bitstrings of X + k*Y bits, k >= 0"
    case eval_int(BaseExpr) of
	{ok,Base} when Base >= 0 ->
	    case eval_int(UnitExpr) of
		{ok,Unit} when Unit >= 0 ->
		    case is_bitstring(X) of
			true ->
			    BitSizeX = bit_size(X),
			    case Unit =:= 0 of
				true ->
				    BitSizeX =:= Base;
			        false ->
				    BitSizeX >= Base
					andalso
					  (BitSizeX - Base) rem Unit =:= 0
			    end;
			false -> false
		    end;
		_ ->
		    abs_expr_error(invalid_unit, UnitExpr)
	    end;
	_ ->
	    abs_expr_error(invalid_base, BaseExpr)
    end;
is_instance(X, _Mod, {type,_,bitstring,[]}, _Stack) ->
    is_bitstring(X);
is_instance(X, _Mod, {type,_,boolean,[]}, _Stack) ->
    is_boolean(X);
is_instance(X, _Mod, {type,_,float,[]}, _Stack) ->
    is_float(X);
is_instance(X, _Mod, {type,_,'fun',[]}, _Stack) ->
    is_function(X);
%% TODO: how to check range type? random inputs? special case for 0-arity?
is_instance(X, _Mod, {type,_,'fun',[{type,_,any,[]},_Range]}, _Stack) ->
    is_function(X);
is_instance(X, _Mod, {type,_,'fun',[{type,_,product,Domain},_Range]}, _Stack) ->
    is_function(X, length(Domain));
is_instance(X, _Mod, {type,_,integer,[]}, _Stack) ->
    is_integer(X);
is_instance(X, Mod, {type,_,list,[Type]}, _Stack) ->
    list_test(X, Mod, Type, dummy, true, true, false);
is_instance(X, Mod, {type,_,maybe_improper_list,[Cont,Term]}, _Stack) ->
    list_test(X, Mod, Cont, Term, true, true, true);
is_instance(X, _Mod, {type,_,module,[]}, _Stack) ->
    is_atom(X) orelse
    is_tuple(X) andalso X =/= {} andalso is_atom(element(1,X));
is_instance([], _Mod, {type,_,nil,[]}, _Stack) ->
    true;
is_instance(X, _Mod, {type,_,neg_integer,[]}, _Stack) ->
    is_integer(X) andalso X < 0;
is_instance(X, _Mod, {type,_,non_neg_integer,[]}, _Stack) ->
    is_integer(X) andalso X >= 0;
is_instance(X, Mod, {type,_,nonempty_list,[Type]}, _Stack) ->
    list_test(X, Mod, Type, dummy, false, true, false);
is_instance(X, Mod, {type,_,nonempty_improper_list,[Cont,Term]}, _Stack) ->
    list_test(X, Mod, Cont, Term, false, false, true);
is_instance(X, Mod, {type,_,nonempty_maybe_improper_list,[Cont,Term]},
	    _Stack) ->
    list_test(X, Mod, Cont, Term, false, true, true);
is_instance(X, _Mod, {type,_,number,[]}, _Stack) ->
    is_number(X);
is_instance(X, _Mod, {type,_,pid,[]}, _Stack) ->
    is_pid(X);
is_instance(X, _Mod, {type,_,port,[]}, _Stack) ->
    is_port(X);
is_instance(X, _Mod, {type,_,pos_integer,[]}, _Stack) ->
    is_integer(X) andalso X > 0;
is_instance(_X, _Mod, {type,_,product,_Elements}, _Stack) ->
    throw({'$typeserver',{internal,product_in_is_instance}});
is_instance(X, _Mod, {type,_,range,[LowExpr,HighExpr]}, _Stack) ->
    case {eval_int(LowExpr),eval_int(HighExpr)} of
	{{ok,Low},{ok,High}} when Low =< High ->
	    X >= Low andalso X =< High;
	_ ->
	    abs_expr_error(invalid_range, LowExpr, HighExpr)
    end;
is_instance(X, Mod, {type,_,record,[{atom,_,Name} = NameForm | RawSubsts]},
	    Stack) ->
    Substs = [{N,T} || {type,_,field_type,[{atom,_,N},T]} <- RawSubsts],
    SubstsDict = dict:from_list(Substs),
    case get_type_repr(Mod, {record,Name,0}, false) of
	{ok,{abs_record,OrigFields}} ->
	    Fields = [case dict:find(FieldName, SubstsDict) of
			  {ok,NewFieldType} -> NewFieldType;
			  error             -> OrigFieldType
		      end
		      || {FieldName,OrigFieldType} <- OrigFields],
	    is_instance(X, Mod, {type,?anno(0),tuple,[NameForm|Fields]}, Stack);
	{error,Reason} ->
	    throw({'$typeserver',Reason})
    end;
is_instance(X, _Mod, {type,_,reference,[]}, _Stack) ->
    is_reference(X);
is_instance(X, _Mod, {type,_,tuple,any}, _Stack) ->
    is_tuple(X);
is_instance(X, Mod, {type,_,tuple,Fields}, _Stack) ->
    is_tuple(X) andalso tuple_test(tuple_to_list(X), Mod, Fields);
is_instance(X, Mod, {type,_,union,Choices}, Stack) ->
    IsInstance = fun(Choice) -> is_instance(X,Mod,Choice,Stack) end,
    lists:any(IsInstance, Choices);
is_instance(X, Mod, {T,_,Name,[]}, Stack) when ?IS_TYPE_TAG(T) ->
    case orddict:find(Name, ?EQUIV_TYPES) of
	{ok,EquivType} ->
	    is_instance(X, Mod, EquivType, Stack);
	error ->
	    is_maybe_hard_adt(X, Mod, Name, [], Stack)
    end;
is_instance(X, Mod, {T,_,Name,ArgForms}, Stack) when ?IS_TYPE_TAG(T) ->
    is_maybe_hard_adt(X, Mod, Name, ArgForms, Stack);
is_instance(_X, _Mod, _Type, _Stack) ->
    false.

-spec is_int_const(term(), abs_expr()) -> boolean().
is_int_const(X, Expr) ->
    case eval_int(Expr) of
	{ok,Int} ->
	    X =:= Int;
	error ->
	    abs_expr_error(invalid_int_const, Expr)
    end.

%% TODO: We implicitly add the '| []' at the termination of maybe_improper_list.
%% TODO: We ignore a '[]' termination in improper_list.
-spec list_test(term(), mod_name(), abs_type(), 'dummy' | abs_type(), boolean(),
		boolean(), boolean()) -> boolean().
list_test(X, Mod, Content, Termination, CanEmpty, CanProper, CanImproper) ->
    is_list(X) andalso
    list_rec(X, Mod, Content, Termination, CanEmpty, CanProper, CanImproper).

-spec list_rec(term(), mod_name(), abs_type(), 'dummy' | abs_type(), boolean(),
	       boolean(), boolean()) -> boolean().
list_rec([], _Mod, _Content, _Termination, CanEmpty, CanProper, _CanImproper) ->
    CanEmpty andalso CanProper;
list_rec([X | Rest], Mod, Content, Termination, _CanEmpty, CanProper,
	 CanImproper) ->
    is_instance(X, Mod, Content, []) andalso
    list_rec(Rest, Mod, Content, Termination, true, CanProper, CanImproper);
list_rec(X, Mod, _Content, Termination, _CanEmpty, _CanProper, CanImproper) ->
    CanImproper andalso is_instance(X, Mod, Termination, []).

-spec tuple_test([term()], mod_name(), [abs_type()]) -> boolean().
tuple_test([], _Mod, []) ->
    true;
tuple_test([X | XTail], Mod, [T | TTail]) ->
    is_instance(X, Mod, T, []) andalso tuple_test(XTail, Mod, TTail);
tuple_test(_, _Mod, _) ->
    false.

-spec is_maybe_hard_adt(term(), mod_name(), type_name(), [abs_type()],
			imm_stack()) -> boolean().
is_maybe_hard_adt(X, Mod, Name, ArgForms, Stack) ->
    case orddict:find({Name,length(ArgForms)}, ?HARD_ADTS) of
	{ok,ADTMod} ->
	    is_custom_instance(X, Mod, ADTMod, Name, ArgForms, true, Stack);
	error ->
	    is_custom_instance(X, Mod, Mod, Name, ArgForms, false, Stack)
    end.

-spec is_custom_instance(term(), mod_name(), mod_name(), type_name(),
			 [abs_type()], boolean(), imm_stack()) -> boolean().
is_custom_instance(X, Mod, RemMod, Name, RawArgForms, IsRemote, Stack) ->
    ArgForms = case Mod =/= RemMod of
		   true  -> [{from_mod,Mod,A} || A <- RawArgForms];
		   false -> RawArgForms
	       end,
    Arity = length(ArgForms),
    FullTypeRef = {RemMod,Name,Arity},
    case lists:member(FullTypeRef, Stack) of
	true ->
	    throw({'$typeserver',{self_reference,FullTypeRef}});
	false ->
	    TypeRef = {type,Name,Arity},
	    AbsType = get_abs_type(RemMod, TypeRef, ArgForms, IsRemote),
	    is_instance(X, RemMod, AbsType, [FullTypeRef|Stack])
    end.

-spec get_abs_type(mod_name(), type_ref(), [abs_type()], boolean()) ->
	  abs_type().
get_abs_type(RemMod, TypeRef, ArgForms, IsRemote) ->
    case get_type_repr(RemMod, TypeRef, IsRemote) of
	{ok,TypeRepr} ->
	    {FinalAbsType,SymbInfo,VarNames} =
		case TypeRepr of
		    {cached,_FinType,FAT,SI} -> {FAT,SI,[]};
		    {abs_type,FAT,VN,SI}     -> {FAT,SI,VN}
		end,
	    AbsType =
		case SymbInfo of
		    not_symb               -> FinalAbsType;
		    {orig_abs,OrigAbsType} -> OrigAbsType
		end,
	    VarSubstsDict = dict:from_list(lists:zip(VarNames,ArgForms)),
	    update_vars(AbsType, VarSubstsDict, false);
	{error,Reason} ->
	    throw({'$typeserver',Reason})
    end.

-spec abs_expr_error(atom(), abs_expr()) -> no_return().
abs_expr_error(ImmReason, Expr) ->
    {error,Reason} = expr_error(ImmReason, Expr),
    throw({'$typeserver',Reason}).

-spec abs_expr_error(atom(), abs_expr(), abs_expr()) -> no_return().
abs_expr_error(ImmReason, Expr1, Expr2) ->
    {error,Reason} = expr_error(ImmReason, Expr1, Expr2),
    throw({'$typeserver',Reason}).


%%------------------------------------------------------------------------------
%% Type translation functions
%%------------------------------------------------------------------------------

-spec convert(mod_name(), abs_type(), state()) ->
	  rich_result2(fin_type(),state()).
convert(Mod, TypeForm, State) ->
    case convert(Mod, TypeForm, State, [], dict:new()) of
	{ok,{simple,Type},NewState} ->
	    {ok, Type, NewState};
	{ok,{rec,_RecFun,_RecArgs},_NewState} ->
	    {error, {internal,rec_returned_to_toplevel}};
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert(mod_name(), abs_type(), state(), stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert(Mod, {paren_type,_,[Type]}, State, Stack, VarDict) ->
    convert(Mod, Type, State, Stack, VarDict);
convert(Mod, {ann_type,_,[_Var,Type]}, State, Stack, VarDict) ->
    convert(Mod, Type, State, Stack, VarDict);
convert(_Mod, {var,_,'_'}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:any()}, State};
convert(_Mod, {var,_,VarName}, State, _Stack, VarDict) ->
    case dict:find(VarName, VarDict) of
	%% TODO: do we need to check if we are at toplevel of a recursive?
	{ok,RetType} -> {ok, RetType, State};
	error        -> {error, {unbound_var,VarName}}
    end;
convert(Mod, {remote_type,_,[{atom,_,RemMod},{atom,_,Name},ArgForms]}, State,
	Stack, VarDict) ->
    case prepare_for_remote(RemMod, Name, length(ArgForms), State) of
	{ok,NewState} ->
	    convert_custom(Mod,RemMod,Name,ArgForms,NewState,Stack,VarDict);
	{error,_Reason} = Error ->
	    Error
    end;
convert(_Mod, {atom,_,Atom}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:exactly(Atom)}, State};
convert(_Mod, {integer,_,_Int} = IntExpr, State, _Stack, _VarDict) ->
    convert_integer(IntExpr, State);
convert(_Mod, {op,_,_Op,_Arg} = OpExpr, State, _Stack, _VarDict) ->
    convert_integer(OpExpr, State);
convert(_Mod, {op,_,_Op,_Arg1,_Arg2} = OpExpr, State, _Stack, _VarDict) ->
    convert_integer(OpExpr, State);
convert(_Mod, {type,_,binary,[BaseExpr,UnitExpr]}, State, _Stack, _VarDict) ->
    %% <<_:X,_:_*Y>> means "bitstrings of X + k*Y bits, k >= 0"
    case eval_int(BaseExpr) of
	{ok,0} ->
	    case eval_int(UnitExpr) of
		{ok,0} -> {ok, {simple,proper_types:exactly(<<>>)}, State};
		{ok,1} -> {ok, {simple,proper_types:bitstring()}, State};
		{ok,8} -> {ok, {simple,proper_types:binary()}, State};
		{ok,N} when N > 0 ->
		    Gen = ?LET(L, proper_types:list(proper_types:bitstring(N)),
			       concat_bitstrings(L)),
		    {ok, {simple,Gen}, State};
		_      -> expr_error(invalid_unit, UnitExpr)
	    end;
	{ok,Base} when Base > 0 ->
	    Head = proper_types:bitstring(Base),
	    case eval_int(UnitExpr) of
		{ok,0} -> {ok, {simple,Head}, State};
		{ok,1} ->
		    Tail = proper_types:bitstring(),
		    {ok, {simple,concat_binary_gens(Head, Tail)}, State};
		{ok,8} ->
		    Tail = proper_types:binary(),
		    {ok, {simple,concat_binary_gens(Head, Tail)}, State};
		{ok,N} when N > 0 ->
		    Tail =
			?LET(L, proper_types:list(proper_types:bitstring(N)),
			     concat_bitstrings(L)),
		    {ok, {simple,concat_binary_gens(Head, Tail)}, State};
		_      -> expr_error(invalid_unit, UnitExpr)
	    end;
	_ ->
	    expr_error(invalid_base, BaseExpr)
    end;
convert(_Mod, {type,_,range,[LowExpr,HighExpr]}, State, _Stack, _VarDict) ->
    case {eval_int(LowExpr),eval_int(HighExpr)} of
	{{ok,Low},{ok,High}} when Low =< High ->
	    {ok, {simple,proper_types:integer(Low,High)}, State};
	_ ->
	    expr_error(invalid_range, LowExpr, HighExpr)
    end;
convert(_Mod, {type,_,nil,[]}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:exactly([])}, State};
convert(Mod, {type,_,list,[ElemForm]}, State, Stack, VarDict) ->
    convert_list(Mod, false, ElemForm, State, Stack, VarDict);
convert(Mod, {type,_,nonempty_list,[ElemForm]}, State, Stack, VarDict) ->
    convert_list(Mod, true, ElemForm, State, Stack, VarDict);
convert(_Mod, {type,_,nonempty_list,[]}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:non_empty(proper_types:list())}, State};
convert(_Mod, {type,_,nonempty_string,[]}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:non_empty(proper_types:string())}, State};
convert(_Mod, {type,_,tuple,any}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:tuple()}, State};
convert(Mod, {type,_,tuple,ElemForms}, State, Stack, VarDict) ->
    convert_tuple(Mod, ElemForms, false, State, Stack, VarDict);
convert(Mod, {type,_,'$fixed_list',ElemForms}, State, Stack, VarDict) ->
    convert_tuple(Mod, ElemForms, true, State, Stack, VarDict);
convert(Mod, {type,_,record,[{atom,_,Name}|FieldForms]}, State, Stack,
	VarDict) ->
    convert_record(Mod, Name, FieldForms, State, Stack, VarDict);
convert(Mod, {type,_,union,ChoiceForms}, State, Stack, VarDict) ->
    convert_union(Mod, ChoiceForms, State, Stack, VarDict);
convert(Mod, {type,_,'fun',[{type,_,product,Domain},Range]}, State, Stack,
	VarDict) ->
    convert_fun(Mod, length(Domain), Range, State, Stack, VarDict);
%% TODO: These types should be replaced with accurate types.
%% TODO: Add support for nonempty_improper_list/2.
convert(Mod, {type,Anno,maybe_improper_list,[]}, State, Stack, VarDict) ->
    convert(Mod, {type,Anno,list,[]}, State, Stack, VarDict);
convert(Mod, {type,A,maybe_improper_list,[Cont,_Ter]}, State, Stack, VarDict) ->
    convert(Mod, {type,A,list,[Cont]}, State, Stack, VarDict);
convert(Mod, {type,A,nonempty_maybe_improper_list,[]}, State, Stack, VarDict) ->
    convert(Mod, {type,A,nonempty_list,[]}, State, Stack, VarDict);
convert(Mod, {type,A,nonempty_maybe_improper_list,[Cont,_Term]}, State, Stack,
	VarDict) ->
    convert(Mod, {type,A,nonempty_list,[Cont]}, State, Stack, VarDict);
convert(Mod, {type,A,iodata,[]}, State, Stack, VarDict) ->
    RealType = {type,A,union,[{type,A,binary,[]},{type,A,iolist,[]}]},
    convert(Mod, RealType, State, Stack, VarDict);
convert(Mod, {T,_,Name,[]}, State, Stack, VarDict) when ?IS_TYPE_TAG(T) ->
    case ordsets:is_element(Name, ?STD_TYPES_0) of
	true ->
	    {ok, {simple,proper_types:Name()}, State};
	false ->
	    convert_maybe_hard_adt(Mod, Name, [], State, Stack, VarDict)
    end;
convert(Mod, {T,_,Name,ArgForms}, State, Stack, VarDict) when ?IS_TYPE_TAG(T) ->
    convert_maybe_hard_adt(Mod, Name, ArgForms, State, Stack, VarDict);
convert(_Mod, TypeForm, _State, _Stack, _VarDict) ->
    {error, {unsupported_type,TypeForm}}.

-spec concat_bitstrings([bitstring()]) -> bitstring().
concat_bitstrings(BitStrings) ->
    concat_bitstrings_tr(BitStrings, <<>>).

-spec concat_bitstrings_tr([bitstring()], bitstring()) -> bitstring().
concat_bitstrings_tr([], Acc) ->
    Acc;
concat_bitstrings_tr([BitString | Rest], Acc) ->
    concat_bitstrings_tr(Rest, <<Acc/bits,BitString/bits>>).

-spec concat_binary_gens(fin_type(), fin_type()) -> fin_type().
concat_binary_gens(HeadType, TailType) ->
    ?LET({H,T}, {HeadType,TailType}, <<H/bits,T/bits>>).

-spec convert_fun(mod_name(), arity(), abs_type(), state(), stack(),
		  var_dict()) -> rich_result2(ret_type(),state()).
convert_fun(Mod, Arity, Range, State, Stack, VarDict) ->
    case convert(Mod, Range, State, ['fun' | Stack], VarDict) of
	{ok,{simple,RangeType},NewState} ->
	    {ok, {simple,proper_types:function(Arity,RangeType)}, NewState};
	{ok,{rec,RecFun,RecArgs},NewState} ->
	    case at_toplevel(RecArgs, Stack) of
		true  -> base_case_error(Stack);
		false -> convert_rec_fun(Arity, RecFun, RecArgs, NewState)
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_rec_fun(arity(), rec_fun(), rec_args(), state()) ->
	  {'ok',ret_type(),state()}.
convert_rec_fun(Arity, RecFun, RecArgs, State) ->
    %% We bind the generated value by size.
    NewRecFun =
	fun(GenFuns,Size) ->
	    proper_types:function(Arity, RecFun(GenFuns,Size))
	end,
    NewRecArgs = clean_rec_args(RecArgs),
    {ok, {rec,NewRecFun,NewRecArgs}, State}.

-spec convert_list(mod_name(), boolean(), abs_type(), state(), stack(),
		   var_dict()) -> rich_result2(ret_type(),state()).
convert_list(Mod, NonEmpty, ElemForm, State, Stack, VarDict) ->
    case convert(Mod, ElemForm, State, [list | Stack], VarDict) of
	{ok,{simple,ElemType},NewState} ->
	    InnerType = proper_types:list(ElemType),
	    FinType = case NonEmpty of
			  true  -> proper_types:non_empty(InnerType);
			  false -> InnerType
		      end,
	    {ok, {simple,FinType}, NewState};
	{ok,{rec,RecFun,RecArgs},NewState} ->
	    case {at_toplevel(RecArgs,Stack), NonEmpty} of
		{true,true} ->
		    base_case_error(Stack);
		{true,false} ->
		    NewRecFun =
			fun(GenFuns,Size) ->
			    ElemGen = fun(S) -> ?LAZY(RecFun(GenFuns,S)) end,
			    proper_types:distlist(Size, ElemGen, false)
			end,
		    NewRecArgs = clean_rec_args(RecArgs),
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState};
		{false,_} ->
		    {NewRecFun,NewRecArgs} =
			convert_rec_list(RecFun, RecArgs, NonEmpty),
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_rec_list(rec_fun(), rec_args(), boolean()) ->
	  {rec_fun(),rec_args()}.
convert_rec_list(RecFun, [{true,FullTypeRef}] = RecArgs, NonEmpty) ->
    {NewRecFun,_NormalRecArgs} =
	convert_normal_rec_list(RecFun, RecArgs, NonEmpty),
    AltRecFun =
	fun([InstListGen],Size) ->
	    InstTypesList =
		proper_types:get_prop(internal_types, InstListGen(Size)),
	    proper_types:fixed_list([RecFun([fun(_Size) -> I end],0)
				     || I <- InstTypesList])
	end,
    NewRecArgs = [{{list,NonEmpty,AltRecFun},FullTypeRef}],
    {NewRecFun, NewRecArgs};
convert_rec_list(RecFun, RecArgs, NonEmpty) ->
    convert_normal_rec_list(RecFun, RecArgs, NonEmpty).

-spec convert_normal_rec_list(rec_fun(), rec_args(), boolean()) ->
	  {rec_fun(),rec_args()}.
convert_normal_rec_list(RecFun, RecArgs, NonEmpty) ->
    NewRecFun = fun(GenFuns,Size) ->
		    ElemGen = fun(S) -> RecFun(GenFuns, S) end,
		    proper_types:distlist(Size, ElemGen, NonEmpty)
		end,
    NewRecArgs = clean_rec_args(RecArgs),
    {NewRecFun, NewRecArgs}.

-spec convert_tuple(mod_name(), [abs_type()], boolean(), state(), stack(),
		    var_dict()) -> rich_result2(ret_type(),state()).
convert_tuple(Mod, ElemForms, ToList, State, Stack, VarDict) ->
    case process_list(Mod, ElemForms, State, [tuple | Stack], VarDict) of
	{ok,RetTypes,NewState} ->
	    case combine_ret_types(RetTypes, {tuple,ToList}) of
		{simple,_FinType} = RetType ->
		    {ok, RetType, NewState};
		{rec,_RecFun,RecArgs} = RetType ->
		    case at_toplevel(RecArgs, Stack) of
			true  -> base_case_error(Stack);
			false -> {ok, RetType, NewState}
		    end
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_union(mod_name(), [abs_type()], state(), stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert_union(Mod, ChoiceForms, State, Stack, VarDict) ->
    case process_list(Mod, ChoiceForms, State, [union | Stack], VarDict) of
	{ok,RawChoices,NewState} ->
	    ProcessChoice = fun(T,A) -> process_choice(T,A,Stack) end,
	    {RevSelfRecs,RevNonSelfRecs,RevNonRecs} =
		lists:foldl(ProcessChoice, {[],[],[]}, RawChoices),
	    case {lists:reverse(RevSelfRecs),lists:reverse(RevNonSelfRecs),
		  lists:reverse(RevNonRecs)} of
		{_SelfRecs,[],[]} ->
		    base_case_error(Stack);
		{[],NonSelfRecs,NonRecs} ->
		    {ok, combine_ret_types(NonRecs ++ NonSelfRecs, union),
		     NewState};
		{SelfRecs,NonSelfRecs,NonRecs} ->
		    {BCaseRecFun,BCaseRecArgs} =
			case combine_ret_types(NonRecs ++ NonSelfRecs, union) of
			    {simple,BCaseType} ->
				{fun([],_Size) -> BCaseType end,[]};
			    {rec,BCRecFun,BCRecArgs} ->
				{BCRecFun,BCRecArgs}
			end,
		    NumBCaseGens = length(BCaseRecArgs),
		    [ParentRef | _Upper] = Stack,
		    FallbackRecFun = fun([SelfGen],_Size) -> SelfGen(0) end,
		    FallbackRecArgs = [{false,ParentRef}],
		    FallbackRetType = {rec,FallbackRecFun,FallbackRecArgs},
		    {rec,RCaseRecFun,RCaseRecArgs} =
			combine_ret_types([FallbackRetType] ++ SelfRecs
					  ++ NonSelfRecs, wunion),
		    NewRecFun =
			fun(AllGens,Size) ->
			    {BCaseGens,RCaseGens} =
				lists:split(NumBCaseGens, AllGens),
			    case Size of
				0 -> BCaseRecFun(BCaseGens,0);
				_ -> RCaseRecFun(RCaseGens,Size)
			    end
			end,
		    NewRecArgs = BCaseRecArgs ++ RCaseRecArgs,
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec process_choice(ret_type(), {[ret_type()],[ret_type()],[ret_type()]},
		     stack()) -> {[ret_type()],[ret_type()],[ret_type()]}.
process_choice({simple,_} = RetType, {SelfRecs,NonSelfRecs,NonRecs}, _Stack) ->
    {SelfRecs, NonSelfRecs, [RetType | NonRecs]};
process_choice({rec,RecFun,RecArgs}, {SelfRecs,NonSelfRecs,NonRecs}, Stack) ->
    case at_toplevel(RecArgs, Stack) of
	true ->
	    case partition_by_toplevel(RecArgs, Stack, true) of
		{[],[],_,_} ->
		    NewRecArgs = clean_rec_args(RecArgs),
		    {[{rec,RecFun,NewRecArgs} | SelfRecs], NonSelfRecs,
		     NonRecs};
		{SelfRecArgs,SelfPos,OtherRecArgs,_OtherPos} ->
		    NumInstances = length(SelfRecArgs),
		    IsListInst = fun({true,_FTRef})                  -> false
				  ; ({{list,_NE,_AltRecFun},_FTRef}) -> true
				 end,
		    NewRecFun =
			case proper_arith:filter(IsListInst,SelfRecArgs) of
			    {[],[]} ->
				no_list_inst_rec_fun(RecFun,NumInstances,
						     SelfPos);
			    {[{{list,NonEmpty,AltRecFun},_}],[ListInstPos]} ->
				list_inst_rec_fun(AltRecFun,NumInstances,
						  SelfPos,NonEmpty,ListInstPos)
			end,
		    [{_B,SelfRef} | _] = SelfRecArgs,
		    NewRecArgs =
			[{false,SelfRef} | clean_rec_args(OtherRecArgs)],
		    {[{rec,NewRecFun,NewRecArgs} | SelfRecs], NonSelfRecs,
		     NonRecs}
	    end;
	false ->
	    NewRecArgs = clean_rec_args(RecArgs),
	    {SelfRecs, [{rec,RecFun,NewRecArgs} | NonSelfRecs], NonRecs}
    end.

-spec no_list_inst_rec_fun(rec_fun(), pos_integer(), [position()]) -> rec_fun().
no_list_inst_rec_fun(RecFun, NumInstances, SelfPos) ->
    fun([SelfGen|OtherGens], Size) ->
	?LETSHRINK(
	    Instances,
	    %% Size distribution will be a little off if both normal and
	    %% instance-accepting generators are present.
	    lists:duplicate(NumInstances, SelfGen(Size div NumInstances)),
	    begin
		InstGens = [fun(_Size) -> proper_types:exactly(I) end
			    || I <- Instances],
		AllGens = proper_arith:insert(InstGens, SelfPos, OtherGens),
		RecFun(AllGens, Size)
	    end)
    end.

-spec list_inst_rec_fun(rec_fun(), pos_integer(), [position()], boolean(),
			position()) -> rec_fun().
list_inst_rec_fun(AltRecFun, NumInstances, SelfPos, NonEmpty, ListInstPos) ->
    fun([SelfGen|OtherGens], Size) ->
	?LETSHRINK(
	    AllInsts,
	    lists:duplicate(NumInstances - 1, SelfGen(Size div NumInstances))
	    ++ proper_types:distlist(Size div NumInstances, SelfGen, NonEmpty),
	    begin
		{Instances,InstList} = lists:split(NumInstances - 1, AllInsts),
		InstGens = [fun(_Size) -> proper_types:exactly(I) end
			    || I <- Instances],
		InstTypesList = [proper_types:exactly(I) || I <- InstList],
		InstListGen =
		    fun(_Size) -> proper_types:fixed_list(InstTypesList) end,
		AllInstGens = proper_arith:list_insert(ListInstPos, InstListGen,
						       InstGens),
		AllGens = proper_arith:insert(AllInstGens, SelfPos, OtherGens),
		AltRecFun(AllGens, Size)
	    end)
    end.

-spec convert_maybe_hard_adt(mod_name(), type_name(), [abs_type()], state(),
			     stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert_maybe_hard_adt(Mod, Name, ArgForms, State, Stack, VarDict) ->
    Arity = length(ArgForms),
    case orddict:find({Name,Arity}, ?HARD_ADTS) of
	{ok,Mod} ->
	    convert_custom(Mod, Mod, Name, ArgForms, State, Stack, VarDict);
	{ok,ADTMod} ->
	    A = ?anno(0),
	    ADT = {remote_type,A,[{atom,A,ADTMod},{atom,A,Name},ArgForms]},
	    convert(Mod, ADT, State, Stack, VarDict);
	error ->
	    convert_custom(Mod, Mod, Name, ArgForms, State, Stack, VarDict)
    end.

-spec convert_custom(mod_name(), mod_name(), type_name(), [abs_type()], state(),
		     stack(), var_dict()) -> rich_result2(ret_type(),state()).
convert_custom(Mod, RemMod, Name, ArgForms, State, Stack, VarDict) ->
    case process_list(Mod, ArgForms, State, Stack, VarDict) of
	{ok,Args,NewState} ->
	    Arity = length(Args),
	    TypeRef = {type,Name,Arity},
	    FullTypeRef = {RemMod,type,Name,Args},
	    convert_type(TypeRef, FullTypeRef, NewState, Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_record(mod_name(), type_name(), [abs_type()], state(), stack(),
		     var_dict()) -> rich_result2(ret_type(),state()).
convert_record(Mod, Name, RawSubsts, State, Stack, VarDict) ->
    Substs = [{N,T} || {type,_,field_type,[{atom,_,N},T]} <- RawSubsts],
    {SubstFields,SubstTypeForms} = lists:unzip(Substs),
    case process_list(Mod, SubstTypeForms, State, Stack, VarDict) of
	{ok,SubstTypes,NewState} ->
	    SubstsDict = dict:from_list(lists:zip(SubstFields, SubstTypes)),
	    TypeRef = {record,Name,0},
	    FullTypeRef = {Mod,record,Name,SubstsDict},
	    convert_type(TypeRef, FullTypeRef, NewState, Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_type(type_ref(), full_type_ref(), state(), stack()) ->
	  rich_result2(ret_type(),state()).
convert_type(TypeRef, {Mod,_Kind,_Name,_Spec} = FullTypeRef, State, Stack) ->
    case stack_position(FullTypeRef, Stack) of
	none ->
	    case get_type_repr(Mod, TypeRef, false, State) of
		{ok,TypeRepr,NewState} ->
		    convert_new_type(TypeRef, FullTypeRef, TypeRepr, NewState,
				     Stack);
		{error,_Reason} = Error ->
		    Error
	    end;
	1 ->
	    base_case_error(Stack);
	_Pos ->
	    {ok, {rec,fun([Gen],Size) -> Gen(Size) end,[{true,FullTypeRef}]},
	     State}
    end.

-spec convert_new_type(type_ref(), full_type_ref(), type_repr(), state(),
		       stack()) -> rich_result2(ret_type(),state()).
convert_new_type(_TypeRef, {_Mod,type,_Name,[]},
		 {cached,FinType,_TypeForm,_SymbInfo}, State, _Stack) ->
    {ok, {simple,FinType}, State};
convert_new_type(TypeRef, {Mod,type,_Name,Args} = FullTypeRef,
		 {abs_type,TypeForm,Vars,SymbInfo}, State, Stack) ->
    VarDict = dict:from_list(lists:zip(Vars, Args)),
    case convert(Mod, TypeForm, State, [FullTypeRef | Stack], VarDict) of
	{ok, {simple,ImmFinType}, NewState} ->
	    FinType = case SymbInfo of
			  not_symb ->
			      ImmFinType;
			  {orig_abs,_OrigAbsType} ->
			      proper_symb:internal_well_defined(ImmFinType)
		      end,
	    FinalState = case Vars of
			     [] -> cache_type(Mod, TypeRef, FinType, TypeForm,
					      SymbInfo, NewState);
			     _  -> NewState
			 end,
	    {ok, {simple,FinType}, FinalState};
	{ok, {rec,RecFun,RecArgs}, NewState} ->
	    convert_maybe_rec(FullTypeRef, SymbInfo, RecFun, RecArgs, NewState,
			      Stack);
	{error,_Reason} = Error ->
	    Error
    end;
convert_new_type(_TypeRef, {Mod,record,Name,SubstsDict} = FullTypeRef,
		 {abs_record,OrigFields}, State, Stack) ->
    Fields = [case dict:find(FieldName, SubstsDict) of
		  {ok,NewFieldType} -> NewFieldType;
		  error             -> OrigFieldType
	      end
	      || {FieldName,OrigFieldType} <- OrigFields],
    case convert_tuple(Mod, [{atom,0,Name} | Fields], false, State,
		       [FullTypeRef | Stack], dict:new()) of
	{ok, {simple,_FinType}, _NewState} = Result ->
	    Result;
	{ok, {rec,RecFun,RecArgs}, NewState} ->
	    convert_maybe_rec(FullTypeRef, not_symb, RecFun, RecArgs, NewState,
			      Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec cache_type(mod_name(), type_ref(), fin_type(), abs_type(), symb_info(),
		 state()) -> state().
cache_type(Mod, TypeRef, FinType, TypeForm, SymbInfo,
	   #state{types = Types} = State) ->
    TypeRepr = {cached,FinType,TypeForm,SymbInfo},
    ModTypes = dict:fetch(Mod, Types),
    NewModTypes = dict:store(TypeRef, TypeRepr, ModTypes),
    NewTypes = dict:store(Mod, NewModTypes, Types),
    State#state{types = NewTypes}.

-spec convert_maybe_rec(full_type_ref(), symb_info(), rec_fun(), rec_args(),
			state(), stack()) -> rich_result2(ret_type(),state()).
convert_maybe_rec(FullTypeRef, SymbInfo, RecFun, RecArgs, State, Stack) ->
    case at_toplevel(RecArgs, Stack) of
	true  -> base_case_error(Stack);
	false -> safe_convert_maybe_rec(FullTypeRef, SymbInfo, RecFun, RecArgs,
					State)
    end.

-spec safe_convert_maybe_rec(full_type_ref(),symb_info(),rec_fun(),rec_args(),
			     state()) -> rich_result2(ret_type(),state()).
safe_convert_maybe_rec(FullTypeRef, SymbInfo, RecFun, RecArgs, State) ->
    case partition_rec_args(FullTypeRef, RecArgs, false) of
	{[],[],_,_} ->
	    {ok, {rec,RecFun,RecArgs}, State};
	{MyRecArgs,MyPos,OtherRecArgs,_OtherPos} ->
	    case lists:all(fun({B,_T}) -> B =:= false end, MyRecArgs) of
		true  -> convert_rec_type(SymbInfo, RecFun, MyPos, OtherRecArgs,
					  State);
		false -> {error, {internal,true_rec_arg_reached_type}}
	    end
    end.

-spec convert_rec_type(symb_info(), rec_fun(), [position()], rec_args(),
		       state()) -> {ok, ret_type(), state()}.
convert_rec_type(SymbInfo, RecFun, MyPos, [], State) ->
    NumRecArgs = length(MyPos),
    M = fun(GenFun) ->
	    fun(Size) ->
		GenFuns = lists:duplicate(NumRecArgs, GenFun),
		RecFun(GenFuns, erlang:max(0,Size - 1))
	    end
	end,
    SizedGen = y(M),
    ImmFinType = ?SIZED(Size,SizedGen(Size + 1)),
    FinType = case SymbInfo of
		  not_symb ->
		      ImmFinType;
		  {orig_abs,_OrigAbsType} ->
		      proper_symb:internal_well_defined(ImmFinType)
	      end,
    {ok, {simple,FinType}, State};
convert_rec_type(_SymbInfo, RecFun, MyPos, OtherRecArgs, State) ->
    NumRecArgs = length(MyPos),
    NewRecFun =
	fun(OtherGens,TopSize) ->
	    M = fun(GenFun) ->
		    fun(Size) ->
			GenFuns = lists:duplicate(NumRecArgs, GenFun),
			AllGens =
			    proper_arith:insert(GenFuns, MyPos, OtherGens),
			RecFun(AllGens, erlang:max(0,Size - 1))
		    end
		end,
	    (y(M))(TopSize)
	end,
    NewRecArgs = clean_rec_args(OtherRecArgs),
    {ok, {rec,NewRecFun,NewRecArgs}, State}.

%% Y Combinator: Read more at http://bc.tech.coop/blog/070611.html.
-spec y(fun((fun((T) -> S))  ->  fun((T) -> S))) -> fun((T) -> S).
y(M) ->
    G = fun(F) ->
	    M(fun(A) -> (F(F))(A) end)
	end,
    G(G).

-spec process_list(mod_name(), [abs_type() | ret_type()], state(), stack(),
		   var_dict()) -> rich_result2([ret_type()],state()).
process_list(Mod, RawTypes, State, Stack, VarDict) ->
    Process = fun({simple,_FinType} = Type, {ok,Types,State1}) ->
		     {ok, [Type|Types], State1};
		 ({rec,_RecFun,_RecArgs} = Type, {ok,Types,State1}) ->
		     {ok, [Type|Types], State1};
		 (TypeForm, {ok,Types,State1}) ->
		     case convert(Mod, TypeForm, State1, Stack, VarDict) of
			 {ok,Type,State2} -> {ok,[Type|Types],State2};
			 {error,_} = Err  -> Err
		     end;
		 (_RawType, {error,_} = Err) ->
		     Err
	      end,
    case lists:foldl(Process, {ok,[],State}, RawTypes) of
	{ok,RevTypes,NewState} ->
	    {ok, lists:reverse(RevTypes), NewState};
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_integer(abs_expr(), state()) -> rich_result2(ret_type(),state()).
convert_integer(Expr, State) ->
    case eval_int(Expr) of
	{ok,Int} -> {ok, {simple,proper_types:exactly(Int)}, State};
	error    -> expr_error(invalid_int_const, Expr)
    end.

-spec eval_int(abs_expr()) -> tagged_result(integer()).
eval_int(Expr) ->
    NoBindings = erl_eval:new_bindings(),
    try erl_eval:expr(Expr, NoBindings) of
	{value,Value,_NewBindings} when is_integer(Value) ->
	    {ok, Value};
	_ ->
	    error
    catch
	error:_ ->
	    error
    end.

-spec expr_error(atom(), abs_expr()) -> {'error',term()}.
expr_error(Reason, Expr) ->
    {error, {Reason,lists:flatten(erl_pp:expr(Expr))}}.

-spec expr_error(atom(), abs_expr(), abs_expr()) -> {'error',term()}.
expr_error(Reason, Expr1, Expr2) ->
    Str1 = lists:flatten(erl_pp:expr(Expr1)),
    Str2 = lists:flatten(erl_pp:expr(Expr2)),
    {error, {Reason,Str1,Str2}}.

-spec base_case_error(stack()) -> {'error',term()}.
%% TODO: This might confuse, since it doesn't record the arguments to parametric
%%	 types or the type subsitutions of a record.
base_case_error([{Mod,type,Name,Args} | _Upper]) ->
    Arity = length(Args),
    {error, {no_base_case,{Mod,type,Name,Arity}}};
base_case_error([{Mod,record,Name,_SubstsDict} | _Upper]) ->
    {error, {no_base_case,{Mod,record,Name}}}.


%%------------------------------------------------------------------------------
%% Helper datatypes handling functions
%%------------------------------------------------------------------------------

-spec stack_position(full_type_ref(), stack()) -> 'none' | pos_integer().
stack_position(FullTypeRef, Stack) ->
    SameType = fun(A) -> same_full_type_ref(A,FullTypeRef) end,
    case proper_arith:find_first(SameType, Stack) of
	{Pos,_} -> Pos;
	none    -> none
    end.

-spec partition_by_toplevel(rec_args(), stack(), boolean()) ->
	  {rec_args(),[position()],rec_args(),[position()]}.
partition_by_toplevel(RecArgs, [], _OnlyInstanceAccepting) ->
    {[],[],RecArgs,lists:seq(1,length(RecArgs))};
partition_by_toplevel(RecArgs, [_Parent | _Upper], _OnlyInstanceAccepting)
	when is_atom(_Parent) ->
    {[],[],RecArgs,lists:seq(1,length(RecArgs))};
partition_by_toplevel(RecArgs, [Parent | _Upper], OnlyInstanceAccepting) ->
    partition_rec_args(Parent, RecArgs, OnlyInstanceAccepting).

-spec at_toplevel(rec_args(), stack()) -> boolean().
at_toplevel(RecArgs, Stack) ->
    case partition_by_toplevel(RecArgs, Stack, false) of
	{[],[],_,_} -> false;
	_           -> true
    end.

-spec partition_rec_args(full_type_ref(), rec_args(), boolean()) ->
	  {rec_args(),[position()],rec_args(),[position()]}.
partition_rec_args(FullTypeRef, RecArgs, OnlyInstanceAccepting) ->
    SameType =
	case OnlyInstanceAccepting of
	    true  -> fun({false,_T}) -> false
		      ; ({_B,T})     -> same_full_type_ref(T,FullTypeRef) end;
	    false -> fun({_B,T}) -> same_full_type_ref(T,FullTypeRef) end
	end,
    proper_arith:partition(SameType, RecArgs).

%% Tuples can be of 0 arity, unions of 1 and wunions at least of 2.
-spec combine_ret_types([ret_type()], {'tuple',boolean()} | 'union'
				      | 'wunion') -> ret_type().
combine_ret_types(RetTypes, EnclosingType) ->
    case lists:all(fun is_simple_ret_type/1, RetTypes) of
	true ->
	    %% This should never happen for wunion.
	    Combine = case EnclosingType of
			  {tuple,false} -> fun proper_types:tuple/1;
			  {tuple,true}  -> fun proper_types:fixed_list/1;
			  union         -> fun proper_types:union/1
		      end,
	    FinTypes = [T || {simple,T} <- RetTypes],
	    {simple, Combine(FinTypes)};
	false ->
	    NumTypes = length(RetTypes),
	    {RevRecFuns,RevRecArgsList,NumRecs} =
		lists:foldl(fun add_ret_type/2, {[],[],0}, RetTypes),
	    RecFuns = lists:reverse(RevRecFuns),
	    RecArgsList = lists:reverse(RevRecArgsList),
	    RecArgLens = [length(RecArgs) || RecArgs <- RecArgsList],
	    RecFunInfo = {NumTypes,NumRecs,RecArgLens,RecFuns},
	    FlatRecArgs = lists:flatten(RecArgsList),
	    {NewRecFun,NewRecArgs} =
		case EnclosingType of
		    {tuple,ToList} ->
			{tuple_rec_fun(RecFunInfo,ToList),
			 soft_clean_rec_args(FlatRecArgs,RecFunInfo,ToList)};
		    union ->
			{union_rec_fun(RecFunInfo),clean_rec_args(FlatRecArgs)};
		    wunion ->
			{wunion_rec_fun(RecFunInfo),
			 clean_rec_args(FlatRecArgs)}
		end,
	    {rec, NewRecFun, NewRecArgs}
    end.

-spec tuple_rec_fun(rec_fun_info(), boolean()) -> rec_fun().
tuple_rec_fun({_NumTypes,NumRecs,RecArgLens,RecFuns}, ToList) ->
    Combine = case ToList of
		  true  -> fun proper_types:fixed_list/1;
		  false -> fun proper_types:tuple/1
	      end,
    fun(AllGFs,TopSize) ->
	Size = TopSize div NumRecs,
	GFsList = proper_arith:unflatten(AllGFs, RecArgLens),
	ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
	ZipFun = fun erlang:apply/2,
	Combine(lists:zipwith(ZipFun, RecFuns, ArgsList))
    end.

-spec union_rec_fun(rec_fun_info()) -> rec_fun().
union_rec_fun({_NumTypes,_NumRecs,RecArgLens,RecFuns}) ->
    fun(AllGFs,Size) ->
	GFsList = proper_arith:unflatten(AllGFs, RecArgLens),
	ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
	ZipFun = fun(F,A) -> ?LAZY(apply(F,A)) end,
	proper_types:union(lists:zipwith(ZipFun, RecFuns, ArgsList))
    end.

-spec wunion_rec_fun(rec_fun_info()) -> rec_fun().
wunion_rec_fun({NumTypes,_NumRecs,RecArgLens,RecFuns}) ->
    fun(AllGFs,Size) ->
	GFsList = proper_arith:unflatten(AllGFs, RecArgLens),
	ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
	ZipFun = fun(W,F,A) -> {W,?LAZY(apply(F,A))} end,
	RecWeight = erlang:max(1, Size div (NumTypes - 1)),
	Weights = [1 | lists:duplicate(NumTypes - 1, RecWeight)],
	WeightedChoices = lists:zipwith3(ZipFun, Weights, RecFuns, ArgsList),
	proper_types:wunion(WeightedChoices)
    end.

-spec add_ret_type(ret_type(), {[rec_fun()],[rec_args()],non_neg_integer()}) ->
	  {[rec_fun()],[rec_args()],non_neg_integer()}.
add_ret_type({simple,FinType}, {RecFuns,RecArgsList,NumRecs}) ->
    {[fun([],_) -> FinType end | RecFuns], [[] | RecArgsList], NumRecs};
add_ret_type({rec,RecFun,RecArgs}, {RecFuns,RecArgsList,NumRecs}) ->
    {[RecFun | RecFuns], [RecArgs | RecArgsList], NumRecs + 1}.

-spec is_simple_ret_type(ret_type()) -> boolean().
is_simple_ret_type({simple,_FinType}) ->
    true;
is_simple_ret_type({rec,_RecFun,_RecArgs}) ->
    false.

-spec clean_rec_args(rec_args()) -> rec_args().
clean_rec_args(RecArgs) ->
    [{false,F} || {_B,F} <- RecArgs].

-spec soft_clean_rec_args(rec_args(), rec_fun_info(), boolean()) -> rec_args().
soft_clean_rec_args(RecArgs, RecFunInfo, ToList) ->
    soft_clean_rec_args_tr(RecArgs, [], RecFunInfo, ToList, false, 1).

-spec soft_clean_rec_args_tr(rec_args(), rec_args(), rec_fun_info(), boolean(),
			     boolean(), position()) -> rec_args().
soft_clean_rec_args_tr([], Acc, _RecFunInfo, _ToList, _FoundListInst, _Pos) ->
    lists:reverse(Acc);
soft_clean_rec_args_tr([{{list,_NonEmpty,_AltRecFun},FTRef} | Rest], Acc,
		       RecFunInfo, ToList, true, Pos) ->
    NewArg = {false,FTRef},
    soft_clean_rec_args_tr(Rest, [NewArg|Acc], RecFunInfo, ToList, true, Pos+1);
soft_clean_rec_args_tr([{{list,NonEmpty,AltRecFun},FTRef} | Rest], Acc,
		       RecFunInfo, ToList, false, Pos) ->
    {NumTypes,NumRecs,RecArgLens,RecFuns} = RecFunInfo,
    AltRecFunPos = get_group(Pos, RecArgLens),
    AltRecFuns = proper_arith:list_update(AltRecFunPos, AltRecFun, RecFuns),
    AltRecFunInfo = {NumTypes,NumRecs,RecArgLens,AltRecFuns},
    NewArg = {{list,NonEmpty,tuple_rec_fun(AltRecFunInfo,ToList)},FTRef},
    soft_clean_rec_args_tr(Rest, [NewArg|Acc], RecFunInfo, ToList, true, Pos+1);
soft_clean_rec_args_tr([Arg | Rest], Acc, RecFunInfo, ToList, FoundListInst,
		       Pos) ->
    soft_clean_rec_args_tr(Rest, [Arg | Acc], RecFunInfo, ToList, FoundListInst,
			   Pos+1).

-spec get_group(pos_integer(), [non_neg_integer()]) -> pos_integer().
get_group(Pos, AllMembers) ->
    get_group_tr(Pos, AllMembers, 1).

-spec get_group_tr(pos_integer(), [non_neg_integer()], pos_integer()) ->
	  pos_integer().
get_group_tr(Pos, [Members | Rest], GroupNum) ->
    case Pos =< Members of
	true  -> GroupNum;
	false -> get_group_tr(Pos - Members, Rest, GroupNum + 1)
    end.

-spec same_full_type_ref(full_type_ref(), term()) -> boolean().
same_full_type_ref({SameMod,type,SameName,Args1},
		   {SameMod,type,SameName,Args2}) ->
    length(Args1) =:= length(Args2)
    andalso lists:all(fun({A,B}) -> same_ret_type(A,B) end,
		      lists:zip(Args1, Args2));
same_full_type_ref({SameMod,record,SameName,SubstsDict1},
		   {SameMod,record,SameName,SubstsDict2}) ->
    same_substs_dict(SubstsDict1, SubstsDict2);
same_full_type_ref(_, _) ->
    false.

-spec same_ret_type(ret_type(), ret_type()) -> boolean().
same_ret_type({simple,FinType1}, {simple,FinType2}) ->
    same_fin_type(FinType1, FinType2);
same_ret_type({rec,RecFun1,RecArgs1}, {rec,RecFun2,RecArgs2}) ->
    NumRecArgs = length(RecArgs1),
    length(RecArgs2) =:= NumRecArgs
    andalso lists:all(fun({A1,A2}) -> same_rec_arg(A1,A2,NumRecArgs) end,
		      lists:zip(RecArgs1,RecArgs2))
    andalso same_rec_fun(RecFun1, RecFun2, NumRecArgs);
same_ret_type(_, _) ->
    false.

%% TODO: Is this too strict?
-spec same_rec_arg(rec_arg(), rec_arg(), arity()) -> boolean().
same_rec_arg({{list,SameBool,AltRecFun1},FTRef1},
	     {{list,SameBool,AltRecFun2},FTRef2}, NumRecArgs) ->
    same_rec_fun(AltRecFun1, AltRecFun2, NumRecArgs)
    andalso same_full_type_ref(FTRef1, FTRef2);
same_rec_arg({true,FTRef1}, {true,FTRef2}, _NumRecArgs) ->
    same_full_type_ref(FTRef1, FTRef2);
same_rec_arg({false,FTRef1}, {false,FTRef2}, _NumRecArgs) ->
    same_full_type_ref(FTRef1, FTRef2);
same_rec_arg(_, _, _NumRecArgs) ->
    false.

-spec same_substs_dict(substs_dict(), substs_dict()) -> boolean().
same_substs_dict(SubstsDict1, SubstsDict2) ->
    SameKVPair = fun({{_K,V1},{_K,V2}}) -> same_ret_type(V1,V2);
		    (_)                 -> false
		 end,
    SubstsKVList1 = lists:sort(dict:to_list(SubstsDict1)),
    SubstsKVList2 = lists:sort(dict:to_list(SubstsDict2)),
    length(SubstsKVList1) =:= length(SubstsKVList2)
    andalso lists:all(SameKVPair, lists:zip(SubstsKVList1,SubstsKVList2)).

-spec same_fin_type(fin_type(), fin_type()) -> boolean().
same_fin_type(Type1, Type2) ->
    proper_types:equal_types(Type1, Type2).

-spec same_rec_fun(rec_fun(), rec_fun(), arity()) -> boolean().
same_rec_fun(RecFun1, RecFun2, NumRecArgs) ->
    %% It's ok that we return a type, even if there's a 'true' for use of
    %% an instance.
    GenFun = fun(_Size) -> proper_types:exactly('$dummy') end,
    GenFuns = lists:duplicate(NumRecArgs,GenFun),
    same_fin_type(RecFun1(GenFuns,0), RecFun2(GenFuns,0)).
