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
%%----------------------------------------------------------------------
%% File    : ifr_types.hrl
%% Purpose : Record definitions for structs used in the interface repository
%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% *********************************************************************
%%% *									*
%%% *                        PLEASE NOTE				*
%%% *									*
%%% * If a record is removed or added in this file, select/2 in		*
%%% * orber_ifr.erl _MUST_ be updated accordingly.			*
%%% *									*
%%% *********************************************************************
%%%----------------------------------------------------------------------

-record(contained_description, {kind, value}).

-record(structmember, {name, type, type_def}).

-record(unionmember, {name, label, type, type_def}).

-record(container_description, {contained_object, kind, value}).

-record(moduledescription, {name, id, defined_in, version}).

-record(constantdescription, {name, id, defined_in, version, type, value}).

-record(typedescription, {name, id, defined_in, version, type}).

-define(make_typedescription(Obj,Object_type),
	#typedescription{name = Obj#Object_type.name,
			 id = Obj#Object_type.id,
			 defined_in = Obj#Object_type.defined_in,
			 version = Obj#Object_type.version,
			 type = Obj#Object_type.type}).

-record(exceptiondescription, {name, id, defined_in, version, type}).

-record(attributedescription, {name, id, defined_in, version, type, mode}).

-record(parameterdescription, {name, type, type_def, mode}).

-record(operationdescription, {name, id, defined_in, version, result, mode,
			       contexts, parameters, exceptions}).

-record(fullinterfacedescription, {name, id, defined_in, version, operations,
				   attributes, base_interfaces, type}).

-record(interfacedescription, {name, id, defined_in, version,
			       base_interfaces}).
