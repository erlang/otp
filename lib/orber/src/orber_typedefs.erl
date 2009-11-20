%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_typedefs.erl
%% Description:
%%    This file contains some functions for internal typedef checking
%%
%%-----------------------------------------------------------------
-module(orber_typedefs).

-include("orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([get_op_def/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: get_op_def/2
%%
get_op_def(_Objkey, '_is_a') ->
    {orber_tc:boolean(),[orber_tc:string(0)],[]};
%% First the OMG specified this operation to be '_not_existent' and then
%% changed it to '_non_existent' without suggesting that both must be supported.
%% See CORBA2.3.1 page 15-34, Minor revision 2.3.1: October 1999
get_op_def(_Objkey, '_not_existent') ->
    {orber_tc:boolean(),[],[]};
get_op_def(_Objkey, '_non_existent') ->
    {orber_tc:boolean(),[],[]};
%% Defined in the Fault Tolerant section of the CORBA specification.
get_op_def(_Objkey, '_FT_HB') ->
    {orber_tc:void(),[],[]};
get_op_def(Objkey, Op) ->
    case catch iop_ior:get_key(Objkey) of
	{_Local, _Key, _, _, Module} ->
	    case catch Module:oe_tc(Op) of
		{'EXIT', What} ->
		    orber:dbg("[~p] orber_typedefs:get_op_def(~p);~n"
			      "The call-back module does not exist or incorrect~n"
			      "IC-version used. Reason:~n~p", 
			      [?LINE, Module, What], ?DEBUG_LEVEL),
		    corba:raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
					     completion_status=?COMPLETED_NO});
		undefined ->
		    corba:raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
						 completion_status=?COMPLETED_NO});
		TC ->
		    TC
	    end;
	_ ->
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end.

