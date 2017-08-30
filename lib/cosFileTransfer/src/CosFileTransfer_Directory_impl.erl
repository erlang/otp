%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File        : CosFileTransfer_Directory_impl
%% Description : 
%%
%% Created     : 12 Sept 2000
%%----------------------------------------------------------------------
-module('CosFileTransfer_Directory_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-include_lib("cosProperty/include/CosPropertyService.hrl").

-include("cosFileTransferApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2]).

%% Interface functions
-export([list/3]).
 
%% Inherited CosFileTransfer::File
-export(['_get_name'/2, 
	 '_get_complete_file_name'/2, 
	 '_get_parent'/2,
	 '_get_associated_session'/2]).
 
%% Inherited CosPropertyService::PropertySetDef
-export([get_allowed_property_types/2, 
	 get_allowed_properties/2, 
	 define_property_with_mode/5,
	 define_properties_with_modes/3, 
	 get_property_mode/3, 
	 get_property_modes/3,
	 set_property_mode/4, 
	 set_property_modes/3]).
 
%% Inherited CosPropertyService::PropertySet
-export([define_property/4, 
	 define_properties/3, 
	 get_number_of_properties/2,
	 get_all_property_names/3, 
	 get_property_value/3, 
	 get_properties/3,
	 get_all_properties/3, 
	 delete_property/3, 
	 delete_properties/3,
	 delete_all_properties/2, 
	 is_property_defined/3]).
 
%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {property, 
		name, 
		completeName, 
		parent, 
		assocSession}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(create_InitState(Pr, N, C, Pa, A),
	#state{property     = Pr,
	       name         = N,
	       completeName = C,
	       parent       = Pa,
	       assocSession = A}).

-define(get_PropertyRef(S),     S#state.property).
-define(get_Name(S),            S#state.name).
-define(get_CompleteName(S),    S#state.completeName).
-define(get_Parent(S),          S#state.parent).
-define(get_AssocSession(S),    S#state.assocSession).

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([Name, CompleteName, Parent, AssocSession]) ->
    PropTypes = [tk_long, tk_boolean],
    %% WARNING if the order of the properties is changed so must
    %% get_all_properties/3 in this module be as well!
    PropDefs = [#'CosPropertyService_PropertyDef'
		{property_name  = "num_children", 
		 property_value = #any{typecode=tk_long, value=0}, 
		 property_mode  = fixed_readonly},
		#'CosPropertyService_PropertyDef'
		{property_name  = "is_directory", 
		 property_value = #any{typecode=tk_boolean, value=true}, 
		 property_mode  = fixed_readonly}],
    Prop = cosProperty:create_static_SetDef(PropTypes, PropDefs),
    {ok, ?create_InitState(Prop, Name, CompleteName, Parent, AssocSession)}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%======================================================================
%% CosFileTransfer::Directory
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : list
%% Arguments  : Max - ulong()
%% Returns    : {ok, FileList, FileIterator}
%% Description: 
%%----------------------------------------------------------------------
list(OE_This, State, Max) ->
    case 'CosFileTransfer_FileTransferSession':
	oe_orber_get_content(?get_AssocSession(State), ?get_CompleteName(State), 
			     OE_This) of
	[] ->
	    {reply, {ok, [], corba:create_nil_objref()}, State};
	FileList when length(FileList) > Max ->
	    {reply, 
	     {ok, lists:sublist(FileList, Max), 
	     'CosFileTransfer_FileIterator':oe_create([lists:nthtail(Max, 
								     FileList)])}, 
	    State};
	FileList ->
	    {reply, {ok, FileList, corba:create_nil_objref()}, State}
    end.

%%======================================================================
%% CosFileTransfer::File
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : '_get_name'
%% Arguments  : -
%% Returns    : CosFileTransfer::FileName - string
%% Description: 
%%----------------------------------------------------------------------
'_get_name'(_OE_This, State) ->
    {reply, ?get_Name(State), State}.

%%---------------------------------------------------------------------%
%% Function   : '_get_complete_file_name'
%% Arguments  : -
%% Returns    : CosFileTransfer::FileNameList - a list of strings's
%% Description: 
%%----------------------------------------------------------------------
'_get_complete_file_name'(_OE_This, State) ->
    {reply, ?get_CompleteName(State), State}.

%%---------------------------------------------------------------------%
%% Function   : '_get_parent'
%% Arguments  : -
%% Returns    : CosFileTransfer::Directory
%% Description: 
%%----------------------------------------------------------------------
'_get_parent'(_OE_This, State) ->
    {reply, ?get_Parent(State), State}.

%%---------------------------------------------------------------------%
%% Function   : '_get_associated_session'
%% Arguments  : -
%% Returns    : CosFileTransfer::FileTransferSession
%% Description: 
%%----------------------------------------------------------------------
'_get_associated_session'(_OE_This, State) ->
    {reply, ?get_AssocSession(State), State}.

%%======================================================================
%% CosPropertyService::PropertySetDef
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : get_allowed_property_types
%% Arguments  : -
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_allowed_property_types(_OE_This, State) ->
    {reply, 'CosPropertyService_PropertySetDef':
      get_allowed_property_types(?get_PropertyRef(State)), State}.

%%---------------------------------------------------------------------%
%% Function   : get_allowed_properties
%% Arguments  : -
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_allowed_properties(_OE_This, State) ->
    {reply, 'CosPropertyService_PropertySetDef':
     get_allowed_properties(?get_PropertyRef(State)), State}.

%%---------------------------------------------------------------------%
%% Function   : define_property_with_mode
%% Arguments  : Name  - string()
%%              Value - #any{}
%%              Mode  - normal | read_only | fixed_normal | fixed_readonly
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
define_property_with_mode(_OE_This, State, Name, Value, Mode) ->
    {reply, 'CosPropertyService_PropertySetDef':
     define_property_with_mode(?get_PropertyRef(State), Name, Value, Mode), State}.

%%---------------------------------------------------------------------%
%% Function   : define_properties_with_modes
%% Arguments  : PropertyDefs - list of #'CosPropertyService_PropertyDef'{}
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
define_properties_with_modes(_OE_This, State, PropertyDefs) ->
    {reply, 'CosPropertyService_PropertySetDef':
     define_properties_with_modes(?get_PropertyRef(State), PropertyDefs), State}.

%%---------------------------------------------------------------------%
%% Function   : get_property_mode
%% Arguments  : Name - string()
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_property_mode(_OE_This, State, Name) ->
    {reply, 'CosPropertyService_PropertySetDef':
     get_property_mode(?get_PropertyRef(State), Name), State}.


%%---------------------------------------------------------------------%
%% Function   : get_property_modes
%% Arguments  : Names - a list of Name (i.e. string()'s).
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_property_modes(_OE_This, State, Names) ->
    {reply, 'CosPropertyService_PropertySetDef':
	get_property_modes(?get_PropertyRef(State), Names), State}.

%%---------------------------------------------------------------------%
%% Function   : set_property_mode
%% Arguments  : Name - string()
%%              Mode - normal | read_only | fixed_normal | fixed_readonly
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
set_property_mode(_OE_This, State, Name, Mode) ->
    {reply, 'CosPropertyService_PropertySetDef':
     set_property_mode(?get_PropertyRef(State), Name, Mode), State}.


%%---------------------------------------------------------------------%
%% Function   : set_property_modes
%% Arguments  : Modes - a list of #'CosPropertyService_PropertyModes'{}
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
set_property_modes(_OE_This, State, PropertyModes) ->
    {reply, 'CosPropertyService_PropertySetDef':
     set_property_modes(?get_PropertyRef(State), PropertyModes), State}.

%%======================================================================
%% CosPropertyService::PropertySet
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : define_property
%% Arguments  : Name  - string()
%%              Value - #any{}
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
define_property(_OE_This, State, Name, Value) ->
    {reply, 'CosPropertyService_PropertySet':
     define_property(?get_PropertyRef(State), Name, Value), State}.

%%---------------------------------------------------------------------%
%% Function   : define_properties
%% Arguments  : Properties - a list of #'CosPropertyService_Property'{}
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
define_properties(_OE_This, State, Properties) ->
    {reply, 'CosPropertyService_PropertySet':
     define_properties(?get_PropertyRef(State), Properties), State}.


%%---------------------------------------------------------------------%
%% Function   : get_number_of_properties
%% Arguments  : -
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_number_of_properties(_OE_This, State) ->
    {reply, 'CosPropertyService_PropertySet':
     get_number_of_properties(?get_PropertyRef(State)), State}.

%%---------------------------------------------------------------------%
%% Function   : get_all_property_names
%% Arguments  : Max - ulong()
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_all_property_names(_OE_This, State, Max) ->
    {reply, 'CosPropertyService_PropertySet':
     get_all_property_names(?get_PropertyRef(State), Max), State}.

%%---------------------------------------------------------------------%
%% Function   : get_property_value
%% Arguments  : Name - string()
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_property_value(_OE_This, State, "num_children") ->
    Count = 'CosFileTransfer_FileTransferSession':
	oe_orber_count_children(?get_AssocSession(State), ?get_CompleteName(State)),
    {reply, #any{typecode=tk_long, value=Count}, State};
get_property_value(_OE_This, State, Name) ->
    {reply, 'CosPropertyService_PropertySet':
     get_property_value(?get_PropertyRef(State), Name), State}.

%%---------------------------------------------------------------------%
%% Function   : get_properties
%% Arguments  : Names - a list of Name (i.e. string()'s)
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_properties(_OE_This, State, Names) ->
    {Bool, Props} = 
	'CosPropertyService_PropertySet':get_properties(?get_PropertyRef(State), 
							Names),
    NewProps = change_property(Props, [], State),
    {reply, {Bool, NewProps}, State}.

change_property([], Acc, _State) ->
    Acc;
change_property([H|T], Acc, State) ->
    NewAcc = 
	case H of
	    #'CosPropertyService_Property'{property_name = "num_children"} ->
		Count = 'CosFileTransfer_FileTransferSession':
		    oe_orber_count_children(?get_AssocSession(State), 
					    ?get_CompleteName(State)),
		[H#'CosPropertyService_Property'
		 {property_value = #any{typecode=tk_long, value=Count}}|Acc];
	    _ ->
		[H|Acc]
    end,
    change_property(T, NewAcc, State).

%%---------------------------------------------------------------------%
%% Function   : get_all_properties
%% Arguments  : Max - ulong()
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
get_all_properties(_OE_This, State, 0) ->
    %% WARNING if we start supporting more than 10 Properties the next line must
    %% be updated!
    {ok, Props, _Iterator} = 
	'CosPropertyService_PropertySet':get_all_properties(?get_PropertyRef(State), 
							    10),
    NewProps = change_property(Props, [], State),
    %% WARNING if the cosProperty:start_PropertiesIterator/1 is updated
    %% it must be done hear as well.
    {reply, {ok, [], cosProperty:start_PropertiesIterator(NewProps)}, State};
get_all_properties(_OE_This, State, Max) ->
    {ok, Props, Iterator} = 
	'CosPropertyService_PropertySet':get_all_properties(?get_PropertyRef(State), 
							    Max),
    NewProps = change_property(Props, [], State),
    {reply, {ok, NewProps, Iterator}, State}.

%%---------------------------------------------------------------------%
%% Function   : delete_property
%% Arguments  : Name - string()
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
delete_property(_OE_This, State, Name) ->
    {reply, 'CosPropertyService_PropertySet':
     delete_property(?get_PropertyRef(State), Name), State}.


%%---------------------------------------------------------------------%
%% Function   : delete_properties
%% Arguments  : Names - a list of Name (i.e. string()'s)
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
delete_properties(_OE_This, State, Names) ->
    {reply, 'CosPropertyService_PropertySet':
     delete_properties(?get_PropertyRef(State), Names), State}.


%%---------------------------------------------------------------------%
%% Function   : delete_all_properties
%% Arguments  : -
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
delete_all_properties(_OE_This, State) ->
    {reply, 'CosPropertyService_PropertySet':
     delete_all_properties(?get_PropertyRef(State)), State}.

%%---------------------------------------------------------------------%
%% Function   : is_property_defined
%% Arguments  : Name - string()
%% Returns    : See cosProperty application.
%% Description: 
%%----------------------------------------------------------------------
is_property_defined(_OE_This, State, Name) ->
    {reply, 'CosPropertyService_PropertySet':
     is_property_defined(?get_PropertyRef(State), Name), State}.

%%======================================================================
%% Internal functions
%%======================================================================

%%======================================================================
%% END OF MODULE
%%======================================================================

