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

%% Parser output
-record(pdata, {mib_version, 
		mib_name,
		imports,
		defs}).

%% compilation information record
-record(cdata, {module_identity,
		asn1_types = [], 
		mes = [], 
		traps = [], 
		mibfuncs, 
		sequences = [],
		imported_macros = [], 
		objectgroups = [],
		notificationgroups = [], 
		imports,
		oid_ets,
		status_ets}).


-record(mc_module_identity,
	{name,
	 last_updated,
	 organization,
	 contact_info,
	 description,
	 revisions = [],   %% A list of mc_revision
	 name_assign
	}
       ).

-record(mc_revision,
	{revision,
	 description
	}
       ).

-record(mc_object_type,
	{name,
	 syntax,
	 units,
	 max_access,
	 status,
	 description,
	 reference,
	 kind, 
	 name_assign
	}
       ).


-record(mc_new_type,
	{name, 
	 macro,
	 status,
	 description,
	 reference,
	 display_hint,
	 syntax
	}
       ).


-record(mc_trap,
	{name,
	 enterprise,
	 vars,
	 description,
	 reference,
	 num
	}
       ).


-record(mc_notification, 
	{name,
	 vars,
	 status,
	 description,
	 reference,
	 name_assign
	 }
       ).


-record(mc_agent_capabilities,
	{name,
	 product_release,
	 status,
	 description,
	 reference,
	 modules,
	 name_assign
	}
       ).

-record(mc_ac_module, 
	{name,
	 groups,
	 variation
	}
       ).

-record(mc_ac_object_variation,
	{name,
	 syntax, 
	 write_syntax,
	 access,
	 creation,
	 default_value,
	 description
	}
       ).

-record(mc_ac_notification_variation,
	{name,
	 access,
	 description
	}
       ).


-record(mc_module_compliance,
	{name,
	 status,
	 description,
	 reference,
	 modules,
	 name_assign
	}
       ).

-record(mc_mc_compliance_group, 
	{name,
	 description
	}
       ).

-record(mc_mc_object, 
	{name,
	 syntax,
	 write_syntax,
	 access, 
	 description
	}
       ).

-record(mc_mc_module, 
	{name,
	 mandatory,
	 compliance
	}
       ).


-record(mc_object_group,
	{name,
	 objects,
	 status,
	 description,
	 reference,
	 name_assign
	}
       ).


-record(mc_notification_group,
	{name,
	 objects,
	 status,
	 description,
	 reference,
	 name_assign
	}
       ).


-record(mc_sequence, 
	{name,
	 fields
	}
       ).


-record(mc_internal,
	{name, 
	 macro,
	 parent,
	 sub_index
	}
       ).

