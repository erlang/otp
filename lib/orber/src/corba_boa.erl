%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%% File: corba_boa.erl
%% 
%% Description:
%%    This file contains the CORBA::BOA interface
%%
%%-----------------------------------------------------------------
-module(corba_boa).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([%create/3,
	 dispose/1,
	 get_id/1]).
%	 change_implementation/2,
%	 set_exception/3,
%	 impl_is_ready/1,
%	 deactivate_impl/1,
%	 obj_is_ready/2,
%	 deactivate_obj/1,
%	 get_principal/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
%create(Id, Interface, Implementation) ->
%    corba:create(Implementation#orb_ImplDef.module,
%		 Interface#fullinterfacedescription.id).

dispose(Object) ->
    case binary_to_term(iop_ior:get_privfield(Object)) of
	undefined ->
	    case catch iop_ior:get_key(Object) of
		{'internal', Key, _, _, _} ->
		    case orber_objectkeys:get_pid(Key) of
			{error, Reason} ->
			    orber:dbg("[~p] corba_boa:dispose(~p); object not found(~p)", 
				      [?LINE, Object, Reason], ?DEBUG_LEVEL),
			    corba:raise(#'TRANSIENT'{completion_status=?COMPLETED_NO});
			Pid ->
			    gen_server:call(Pid, stop)
		    end;
		{'internal_registered', Key, _, _, _} -> 	
		    case Key of
			{pseudo, Module} ->
			    Module:terminate(normal, undefined),
			    ok;
			_ ->
			    case whereis(Key) of
				undefined ->
				    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
				Pid ->
				    gen_server:call(Pid, stop)
			    end
		    end;
		{'external', _} -> 
		    orber:dbg("[~p] corba_boa:dispose(~p); external object.", 
			      [?LINE, Object], ?DEBUG_LEVEL),
		    %% Must be fixed !!!!!!!!
		    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO})
	    end;
	Other ->
	    case iop_ior:get_key(Object) of
		{_, {pseudo, Module}, _, _, _} ->
		    Module:terminate(normal, Other),
		    ok;
		Why ->
		    orber:dbg("[~p] corba_boa:dispose(~p); probably subobject key set(~p)", 
			      [?LINE, Object, Why], ?DEBUG_LEVEL),
		    corba:raise(#'NO_PERMISSION'{completion_status=?COMPLETED_NO})
	    end
    end.

get_id(Object) ->
    iop_ior:get_objkey(Object).

%change_implementation(Object, ImplementationDef) ->
%    ok.

%get_principal(Object, Env) ->
%    ok.

%set_exception(Major, Id, Param) ->
%    ok.

%impl_is_ready(ImplementationDef) ->
%    ok.

%deactivate_impl(ImplementationDef) ->
%    ok.

%obj_is_ready(Object, ImplementationDef) ->
%    ok.

%deactivate_obj(Object) ->
%    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
