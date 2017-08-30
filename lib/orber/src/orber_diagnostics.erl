%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_diagnostics.erl
%% 
%% Description:
%%
%%-----------------------------------------------------------------

-module(orber_diagnostics).


%%-----------------------------------------------------------------
%% Includes
%%-----------------------------------------------------------------
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContext.hrl").
-include_lib("orber/COSS/CosNaming/orber_cosnaming.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([nameservice/0, nameservice/1, 
	 objectkeys/0, 
	 missing_modules/0]).

%%-----------------------------------------------------------------
%% Internal Exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

-define(DIAGNOSTICS_PING_EXTERNAL, 16#01).

%%-----------------------------------------------------------------
%% Function  : missing_modules
%% Args      : -
%% Returns   : 
%%-----------------------------------------------------------------
missing_modules() ->
    List =
	case orber:light_ifr() of
	    false ->
		Unions = mnesia:dirty_select(ir_UnionDef, 
					     [{#ir_UnionDef{absolute_name='$1', 
							    _='_'}, 
					       [], ['$1']}]),
		Structs = mnesia:dirty_select(ir_StructDef, 
					      [{#ir_StructDef{absolute_name='$1', 
							      _='_'}, 
						[], ['$1']}]),
		Exc = mnesia:dirty_select(ir_ExceptionDef, 
					  [{#ir_ExceptionDef{absolute_name='$1', 
							     _='_'}, 
					    [], ['$1']}]),
		Interface = mnesia:dirty_select(ir_InterfaceDef, 
						[{#ir_InterfaceDef{absolute_name='$1', 
								   _='_'}, 
						  [], ['$1']}]),
		Acc1 = create_module_names(Unions, [], ?IFR_UnionDef),
		Acc2 = create_module_names(Structs, Acc1, ?IFR_StructDef),
		Acc3 = create_module_names(Exc, Acc2, ?IFR_ExceptionDef),
		create_module_names(Interface, Acc3, ?IFR_InterfaceDef);
	    true ->
		mnesia:dirty_select(orber_light_ifr, 
				    [{#orber_light_ifr{module='$1', 
						       type='$2', _='_'}, 
				      [{'=/=', '$2', ?IFR_ModuleDef},
				       {'=/=', '$2', ?IFR_ConstantDef},
				       {'=/=', '$2', ?IFR_AliasDef},
				       {'=/=', '$2', ?IFR_EnumDef}], ['$$']}])
	end,
    io:format("Need to check for ~p modules.~n", [length(List)]),
    Count = missing_modules_helper(List, 0),
    io:format("Check completed. ~p missing modules.~n", [Count]),
    Count.
   
create_module_names([], Acc, _) ->
    Acc;
create_module_names([[$:,$:|N]|T], Acc, Type) ->
    create_module_names(T, [[change_colons_to_underscore(N, []), Type]|Acc], Type).

change_colons_to_underscore([$:, $: | T], Acc) ->
    change_colons_to_underscore(T, [$_ |Acc]);
change_colons_to_underscore([H |T], Acc) ->
    change_colons_to_underscore(T, [H |Acc]);
change_colons_to_underscore([], Acc) ->
    list_to_atom(lists:reverse(Acc)).

missing_modules_helper([], ErrorsFound) ->
    ErrorsFound;
missing_modules_helper([[Mod, Type]|T], ErrorsFound) when Type == ?IFR_StructDef;
							  Type == ?IFR_UnionDef;
							  Type == ?IFR_ExceptionDef ->
    case catch Mod:tc() of
	{'EXIT', _} ->
	    io:format("Missing (~s): ~p~n", [type2str(Type), Mod]),
	    missing_modules_helper(T, ErrorsFound + 1);
	_ ->
	    missing_modules_helper(T, ErrorsFound)
    end;
missing_modules_helper([[Mod, Type]|T], ErrorsFound) when Type == ?IFR_InterfaceDef ->
    case catch Mod:oe_get_interface() of
	{'EXIT', {undef,[{Mod, _, _, _}|_]}} ->
	    io:format("Missing (Interface): ~p~n", [Mod]),
	    missing_modules_helper(T, ErrorsFound + 1);
	{'EXIT', {undef,[{OtherMod, _, _, _}|_]}} ->
	    io:format("Missing (Inherited by the ~p Interface): ~p~n", 
		      [Mod, OtherMod]),
	    missing_modules_helper(T, ErrorsFound + 1);
	_ ->
	    missing_modules_helper(T, ErrorsFound)
    end;
missing_modules_helper([_|T], ErrorsFound) ->
    missing_modules_helper(T, ErrorsFound).

type2str(?IFR_StructDef) ->
    "Struct";
type2str(?IFR_UnionDef) ->
    "Union";
type2str(?IFR_ExceptionDef) -> 
    "Exception".
   

%%-----------------------------------------------------------------
%% Function  : nameservice
%% Args      : - | integer()
%% Returns   : 
%%-----------------------------------------------------------------
nameservice() ->
    nameservice(0).

nameservice(Flags) ->
    case catch ns(?ORB_FLAG_TEST(Flags, ?DIAGNOSTICS_PING_EXTERNAL)) of
	ok ->
	    ok;
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_diagnostics:nameservice(~p);~n"
		      "Reason: ~p", [?LINE, Flags, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	What ->
	    orber:dbg("[~p] orber_diagnostics:nameservice(~p);~n"
		      "Reason: ~p", [?LINE, Flags, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

ns(Ping) ->
    NS = corba:resolve_initial_references("NameService"),
    display_names(NS, "", Ping).


display_names(NS, Prefix, Ping) ->
    {ok, [], Iter} = 'CosNaming_NamingContextExt':list(NS, 0),
    More = not corba_object:is_nil(Iter),
    iter_names(NS, Prefix, Iter, More, Ping).

iter_names(_NS, _Prefix, Iter, false, _) ->
    destroy_iter(Iter);
iter_names(NS, Prefix, Iter, true, Ping) ->
    {More, #'CosNaming_Binding'{binding_name = Name, binding_type = Type}} =
        'CosNaming_BindingIterator':next_one(Iter),
    Fun = fun(#'CosNaming_NameComponent'{id = Id, kind = Kind}, Acc) ->
              case Kind of
                  "" -> Acc ++ Id ++ "/";
                  _  -> Acc ++ Id ++ "." ++ Kind ++ "/"
              end
          end,
    Prefix2 = lists:foldl(Fun, Prefix, Name),
    if
         More == false ->
            ignore;
         Type == nobject ->
            Object = 'CosNaming_NamingContext':resolve(NS, Name),
	    Status = 
		case corba_object:is_remote(Object) of
		    false ->
			corba_object:non_existent(Object);
		    _ when Ping == true ->
			case catch corba_object:non_existent(Object) of
			    Boolean when is_atom(Boolean) ->
				Boolean;
			    _Other ->
				undefined
			end;
		    _ ->
			external
		end,
            io:format("~s [~p] ~s\n", 
		      [Prefix2, Status, iop_ior:get_typeID(Object)]);
         Type == ncontext ->
            Context = 'CosNaming_NamingContext':resolve(NS, Name),
            io:format("~s\n", [Prefix2]),
            display_names(Context, Prefix2, Ping)
    end,
    iter_names(NS, Prefix, Iter, More, Ping).

destroy_iter(Iter) ->
    case corba_object:is_nil(Iter) of
        false -> 
	    'CosNaming_BindingIterator':destroy(Iter),
	    ok;
        true  -> 
	    ok
    end.

objectkeys() ->
    ok.



%%---------------- END OF MODULE ----------------------------------
