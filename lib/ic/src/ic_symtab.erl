%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(ic_symtab).


-include_lib("ic/src/ic.hrl").
-include_lib("ic/src/icforms.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([new/0, store/3, retrieve/2, soft_retrieve/2, intf_resolv/3]).
-export([get_full_scoped_name/3, scoped_id_new_global/1, scoped_id_new/1]).
-export([scoped_id_strip/1,symtab_add_faked_included_types/1]).
-export([scoped_id_is_global/1, scoped_id_add/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------


%%--------------------------------------------------------------------
%%
%% Symbol table routines
%%
%%	Symbol tables handles mappings Id -> Value, where Id is an
%%	ordinary Id from the parser (or a string) and value is an
%%	arbitrary term.
%%
%%--------------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: new/0 (used to be symtab_new)
%%-----------------------------------------------------------------
new() ->
    ets:new(symtab, [set, public]).

%%-----------------------------------------------------------------
%% Func: store/3 (used to be symtab_store)
%%-----------------------------------------------------------------
store(G, N, X) ->
    Name = [ic_forms:get_id2(X) | N],
    %%io:format("Adding id: ~p~n", [N]),
    case soft_retrieve(G, Name) of
	{error, _} ->
	    ets:insert(G#genobj.symtab, {Name, X});
	{ok, Y} when is_record(Y, forward) ->
	    ets:insert(G#genobj.symtab, {Name, X});
	{ok, Y} when is_record(Y, constr_forward) ->
	    ets:insert(G#genobj.symtab, {Name, X});
	{ok, _Y} ->
	    ic_error:error(G, {multiply_defined, X})
    end.


%%-----------------------------------------------------------------
%% Func: retrieve/2 (used to be symtab_retrieve)
%%
%%	Makes a lookup in the symbol table for Id. Will throw
%%	not_found if it fails.
%%-----------------------------------------------------------------
retrieve(G, Id) ->
    case ets:lookup(G#genobj.symtab, Id) of
	[{_, Val}] -> Val;
	[] -> ic_error:error(G, {symtab_not_found, Id})
    end.


%%-----------------------------------------------------------------
%% Func: soft_retrieve/2 (used to be symtab_soft_retrieve)
%%
%%	Same as retrieve but will use tagged return values.
%%
%%-----------------------------------------------------------------
soft_retrieve(G, Id) ->
    case ets:lookup(G#genobj.symtab, Id) of
	[{_, Val}] -> {ok, Val};
	[] -> {error, {symtab_not_found, Id}}
    end.


%%-----------------------------------------------------------------
%% Func: intf_resolv/3 and  resolv2/3
%%       (used to be symtab_intf_resolv  and symtab_intf_resolv2)
%%
%%	Tries to resolv the interface identifier reference. The id can
%%	be either a scoped name or an standard identifier. The
%%	function returns a global reference to the id.
%%
%%	Will throw not_found if the id really cannot be found. Will
%%	throw illegal_forward if any forward references are founf in
%%	the inheritance list.
%%
%%-----------------------------------------------------------------
intf_resolv(G, Scope, Id) ->
    case scoped_id_is_global(Id) of
	true ->
	    retrieve(G, Id),
	    Id;
	false ->
	    intf_resolv2(G, Scope, Id)
    end.

intf_resolv2(G, Scope, Id) ->
    N = scoped_id_add(Scope, Id),
    case soft_retrieve(G, scoped_id_strip(N)) of
	{ok, F} when is_record(F, forward) ->
	    ic_error:error(G, {illegal_forward, Id}), [];
	{ok, _Val} -> 
	    scoped_id_mk_global(N);
	_ ->
	    case scoped_id_is_top(Scope) of
		false ->
		    intf_resolv2(G, scoped_id_up_one(Scope), Id);
		true ->
		    ic_error:error(G, {symtab_not_found, Id}), []
	    end
    end.



%%--------------------------------------------------------------------
%%
%% Scoped id routines
%%
%%	A scoped id is an id written as M::Id in IDL. Scoped ids are
%%	implemented as lists of id in reverse order, so M1::F1 becomes
%%	[F1, M1].
%%
%%--------------------------------------------------------------------

get_full_scoped_name(G, N, S)  when element(1, S) == scoped_id ->
    ictype:scoped_lookup(G, ic_genobj:tktab(G), N, S).

scoped_id_new_global(Id) ->
    X=scoped_id_new(Id), X#scoped_id{type=global}.

scoped_id_new(Id) ->
    #scoped_id{line=ic_forms:get_line(Id), id=[ic_forms:get_id(Id)]}.

%% Adds one more id to the list of ids
scoped_id_add(S1, S2) when is_record(S2, scoped_id) ->
    S1#scoped_id{id=S2#scoped_id.id ++  S1#scoped_id.id, 
		 line=S2#scoped_id.line};
scoped_id_add(S, Id) ->
    S#scoped_id{id=[ic_forms:get_id(Id) | S#scoped_id.id], line=ic_forms:get_line(Id)}.


scoped_id_mk_global(S) -> S#scoped_id{type=global}.

scoped_id_is_global(S) when is_record(S, scoped_id), S#scoped_id.type==global -> 
    true;
scoped_id_is_global(_) -> false.

%% Top level scope (i.e no more cd ..)
scoped_id_is_top(S) when S#scoped_id.id==[] -> true;
scoped_id_is_top(_) -> false.


scoped_id_up_one(S) -> S#scoped_id{id=tl(S#scoped_id.id)}. % cd .. in scope
%%scoped_id_get_def(S) -> hd(S#scoped_id.id).	% Last added id
scoped_id_strip(S) -> S#scoped_id.id.		% Strips all junk




% Add CORBA::<Types> that as if they
% were defined in an included file.
% This is only supported in the case 
% of Corba backend
symtab_add_faked_included_types(G) ->
    case ic_options:get_opt(G, be) of
	false ->
	    %% Add TypeCode as if it were defiend in included file
	    ets:insert(G#genobj.symtab, {["CORBA"], 
					 {interface,{'<identifier>',0,"TypeCode"},
					  [],
					  [],
					  [],
					  {tk_objref,
					   "IDL:omg.org/CORBA/TypeCode:1.0",
					   "TypeCode"}}});
	erl_corba ->
	    %% Add TypeCode as if it were defiend in included file
	    ets:insert(G#genobj.symtab, {["CORBA"], 
					 {interface,{'<identifier>',0,"TypeCode"},
					  [],
					  [],
					  [],
					  {tk_objref,
					   "IDL:omg.org/CORBA/TypeCode:1.0",
					   "TypeCode"}}}); 
	erl_template ->
	    %% Add TypeCode as if it were defiend in included file
	    ets:insert(G#genobj.symtab, {["CORBA"], 
					 {interface,{'<identifier>',0,"TypeCode"},
					  [],
					  [],
					  [],
					  {tk_objref,
					   "IDL:omg.org/CORBA/TypeCode:1.0",
					   "TypeCode"}}}); 
	_ ->
	    ok
    end.



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
