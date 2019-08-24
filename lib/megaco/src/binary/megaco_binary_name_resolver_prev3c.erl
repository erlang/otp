%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2019. All Rights Reserved.
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
%% Purpose: Handle meta data about packages
%%----------------------------------------------------------------------

-module(megaco_binary_name_resolver_prev3c).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").


-define(LOWER(Char),
	if
	    Char >= $A, Char =< $Z ->
		Char - ($A - $a);
	    true ->
		Char
	end).

-export([packages/0,
	 capabilities/0,
	 capabilities/1,
         decode_name/3,
	 encode_name/3
        ]).

encode_name(Config, term_id, TermId) ->
    case megaco:encode_binary_term_id(Config, TermId) of
	{ok, TermId2} ->
	    TermId2;
	{error, Reason} ->
	    exit({bad_term_id, TermId, Reason})
    end;	
encode_name(_Config, Scope, Item) ->
    ?d("encode_name(~p) -> entry with"
       "~n   Item: ~p", [Scope, Item]),
    encode(Scope, Item).

decode_name(Config, term_id, TermId) ->
    case megaco:decode_binary_term_id(Config, TermId) of
	{ok, TermId2} ->
	    TermId2;
	{error, Reason} ->
	    exit({bad_term_id, TermId, Reason})
    end;
decode_name(_Config, Scope, Item) ->
    ?d("decode_name(~p) -> entry with"
       "~n   Item: ~p", [Scope, Item]),
    decode(Scope, Item).



%%----------------------------------------------------------------------
%% 12.1.1	Package
%% 
%% Overall description of the package, specifying:
%% 
%% 	Package Name: only descriptive
%% 
%% 	PackageID: is an identifier
%% 
%% 	Description: is a description of the package
%% 
%% 	Version: 
%% 
%%            A new version of a package can only add additional Properties, 
%%            Events, Signals, Statistics and new possible values for an 
%%            existing parameter described in the original package. No 
%%            deletions or modifications shall be allowed. A version is an 
%%            integer in the range from 1 to 99.
%% 
%% 	Designed to be extended only (Optional): Yes
%%             
%%            This indicates that the package has been expressly designed to 
%%            be extended by others, not to be directly referenced. For 
%%            example, the package may not have any function on its own or be 
%%            nonsensical on its own. The MG SHOULD NOT publish this PackageID 
%%            when reporting packages.
%% 
%% 	Extends: existing package Descriptor
%% 
%%            A package may extend an existing package. The version of the 
%%            original package must be specified. When a package extends 
%%            another package it shall only add additional Properties, Events, 
%%            Signals, Statistics and new possible values for an existing 
%%            parameter described in the original package. An extended package 
%%            shall not redefine or overload an identifier defined in the 
%%            original package and packages it may have extended (multiple 
%%            levels of extension). Hence, if package B version 1 extends 
%%            package A version 1, version 2 of B will not be able to extend 
%%            the A version 2 if A version 2 defines a name already in B 
%%            version 1. If the package does not extend another package, it 
%%            shall specify "none".
%% 
%% 
%% 12.1.2	Properties
%% 
%% Properties defined by the package, specifying:
%% 
%% 	Property Name: only descriptive
%% 
%% 	PropertyID: is an identifier
%% 
%% 	Description: is a description of the function of the property
%% 
%% 	Type: One of:
%% 
%%            Boolean
%% 
%%            String: UTF-8 string
%% 
%%            Octet String: A number of octets. See Annex A and B.3 for 
%%                          encoding
%% 
%%            Integer: 4 byte signed integer
%% 
%%            Double: 8 byte signed integer
%% 
%%            Character: unicode UTF-8 encoding of a single letter. 
%%                       Could be more than one octet.
%% 
%%            Enumeration: one of a list of possible unique values (see 12.3)
%% 
%%            Sub-list: a list of several values from a list. 
%%                      The type of sub-list SHALL also be specified. 
%%                      The type shall be chosen from the types specified in 
%%                      this section (with the exception of sub-list). For 
%%                      example, Type: sub-list of enumeration. The encoding 
%%                      of sub-lists is specified in Annexes A and B.3.
%% 
%% 	Possible values:
%% 
%%            A package MUST specify either a specific set of values or a 
%%            description of how values are determined. A package MUST also 
%%            specify a default value or the default behaviour when the value 
%%            is omitted from its descriptor. For example, a package may 
%%            specify that procedures related to the property are suspended 
%%            when its value is omitted.
%% 
%% 	Default:
%% 
%%            A default value (but not procedures) may be specified as 
%%            provisionable.
%% 
%% 	Defined in:
%% 
%%            Which H.248.1 descriptor the property is defined in. 
%%            LocalControl is for stream-dependent properties. 
%%            TerminationState is for stream-independent properties. 
%%            ContextAttribute is for properties that affect the context as 
%%            a whole, i.e., mixing properties. These are expected to be the 
%%            most common cases, but it is possible for properties to be 
%%            defined in other descriptors. Context properties MUST be defined 
%%            in the ContextAttribute descriptor.
%% 
%% 	Characteristics: Read/Write or both, and (optionally), global: 
%% 
%%            Indicates whether a property is read-only, or read-write, and 
%%            if it is global. If Global is omitted, the property is not 
%%            global. If a property is declared as global, the value of the 
%%            property is shared by all Terminations realizing the package.  
%%            If a context property is declared as global, the property is 
%%            shared by all contexts realizing the package.
%% 
%% 
%% 12.1.3	Events
%% 
%% Events defined by the package, specifying:
%% 
%% 	Event name: only descriptive
%% 
%% 	EventID: is an identifier
%% 
%% 	Description: a description of the function of the event
%% 
%% 	EventsDescriptor Parameters: 
%% 
%%            Parameters used by the MGC to configure the event, and found in 
%%            the EventsDescriptor. See 12.2. If there are no parameters for 
%%            the Events Descriptor, then "none" shall be specified.
%% 
%% 	ObservedEventsDescriptor Parameters: 
%% 
%%            Parameters returned to the MGC in Notify requests and in replies 
%%            to command requests from the MGC that audit 
%%            ObservedEventsDescriptor, and found in the 
%%            ObservedEventsDescriptor. See 12.2. If there are no parameters 
%%            for the ObservedEvents Descriptor, then 'none' shall be specified.
%% 
%% 
%% 12.1.4	Signals
%% 
%% Signals defined by the package, specifying:
%% 
%% 	Signal Name: only descriptive
%% 
%% 	SignalID: is an identifier. SignalID is used in a SignalsDescriptor
%% 
%% 	Description: a description of the function of the signal
%% 
%% 	SignalType: one of:
%% 
%%            OO (On/Off) 
%% 
%%            TO (TimeOut)
%% 
%%            BR (Brief)
%% 
%% 	   NOTE - SignalType may be defined such that it is dependent on
%%            the value of one or more parameters. The package MUST specify a 
%%            default signal type. If the default type is TO, the package MUST 
%%            specify a default duration which may be provisioned. A default 
%%            duration is meaningless for BR. 
%% 
%% 	Duration: in hundredths of seconds
%% 
%% 	Additional Parameters: see 12.2
%% 
%% 
%% 12.1.5	Statistics
%% 
%% Statistics defined by the package, specifying:
%% 
%% 	Statistic name: only descriptive
%% 
%% 	StatisticID: is an identifier
%% 
%% 	StatisticID is used in a StatisticsDescriptor
%% 
%% 	Description: a description of the statistic
%% 
%% 	Type: One of:
%% 
%% 	   Boolean
%% 
%%            String: UTF-8 string
%% 
%%            Octet String: A number of octets.  
%%                          See Annex A and Annex B.3 for encoding
%% 
%%            Integer: 4 byte signed integer
%% 
%%            Double: 8 byte signed integer
%% 
%%            Character: Unicode UTF-8 encoding of a single letter. 
%%                       Could be more than one octet.
%% 
%%            Enumeration: One of a list of possible unique values (See 12.3)
%% 
%%            Sub-list: A list of several values from a list. 
%%                      The type of sub-list SHALL also be specified.  
%%                      The type shall be chosen from the types specified in 
%%                      this section (with the exception of sub-list). 
%%                      For example, Type: sub-list of enumeration.  
%%                      The encoding of sub-lists is specified in Annexes A 
%%                      and B.3.
%% 
%% 	Possible Values:
%% 
%%            A package must indicate the unit of measure, e.g. milliseconds, 
%%            packets, either here or along with the type above, as well as 
%%            indicating any restriction on the range.
%% 
%% 	Level: Specify if the statistic can be kept at the Termination 
%%                level, Stream level or Either.
%% 
%% 
%% 12.1.6	Error Codes
%% 
%% If the package does not define any error codes, this section may be omitted.  
%% Otherwise, it describes error codes defined by the package, specifying:
%% 
%% 	Error Code #: The error code number.
%% 
%% 	Name: Name of the error
%% 
%% 	Definition: A description of the error code.
%% 
%% 	Error Text in the Error Descriptor:
%% 
%%            A description of what text to return in the Error Descriptor.
%% 
%% 	Comment: Any further comments on the use of the error code.
%% 
%% 
%% 12.1.7	Procedures
%% 
%% Additional guidance on the use of the package.
%% 
%% 
%% 12.2	Guidelines to defining parameters to events and signals
%% 
%%         Parameter Name: only descriptive
%% 
%%         ParameterID: is an identifier. The textual ParameterID of 
%%         parameters to Events and Signals shall not start with "EPA" and 
%%         "SPA", respectively. The textual ParameterID shall also not be 
%%         "ST", "Stream", "SY", "SignalType", "DR", "Duration", "NC", 
%%         "NotifyCompletion", "KA", "KeepActive", "EB", "Embed", "DM", 
%%         "DigitMap", "DI", "Direction", "RQ" or "RequestID".
%% 
%%         Description: a description of the function of the parameter.
%% 
%%         Type: One of:
%% 
%%            Boolean
%% 
%%            String: UTF-8 octet string
%% 
%%            Octet String: A number of octets. See Annex A and B.3 for 
%%                          encoding
%% 
%%            Integer: 4-octet signed integer
%% 
%%            Double: 8-octet signed integer
%% 
%%            Character: Unicode UTF-8 encoding of a single letter. Could be 
%%                       more than one octet.
%% 
%%            Enumeration: one of a list of possible unique values (see 12.3)
%% 
%%            Sub-list: a list of several values from a list (not supported 
%%                      for statistics). The type of sub-list SHALL also be 
%%                      specified The type shall be chosen from the types 
%%                      specified in this section (with the exception of 
%%                      sub-list). For example, Type: sub-list of enumeration. 
%%                      The encoding of sub-lists is specified in Annex A 
%%                      and B.3.
%% 
%%            Optional: Yes/No
%% 
%%               Describes if the parameter may be omitted from the signal or 
%%               event.
%% 
%%            Possible values:
%% 
%%               A package MUST specify either a specific set of values or a 
%%               description of how values are determined. A package MUST 
%%               also specify a default value or the default behavior when the 
%%               value is omitted from its descriptor. For example, a package 
%%               may specify that procedures related to the parameter are 
%%               suspended when its value is omitted.
%% 
%%            Default:
%% 
%%               A default value (but not procedures) may be specified as 
%%               provisionable.
%% 
%% 
%% 12.3	Lists
%% 
%% Possible values for parameters include enumerations. Enumerations may be 
%% defined in a list. It is recommended that the list be IANA registered so 
%% that packages that extend the list can be defined without concern for 
%% conflicting names.
%% 
%% 
%% 12.4	Identifiers
%% 
%% Identifiers in text encoding shall be strings of up to 64 characters, 
%% containing no spaces, starting with an alphabetic character and consisting 
%% of alphanumeric characters and/or digits, and possibly including the 
%% special character underscore ("_").
%% 
%% Identifiers in binary encoding are 2 octets long.
%% 
%% Both text and binary values shall be specified for each identifier, 
%% including identifiers used as values in enumerated types.
%% 
%% 
%% 12.5	Package registration
%% 
%% A package can be registered with IANA for interoperability reasons. See 
%% clause 14 for IANA considerations.
%% 
%%----------------------------------------------------------------------

capabilities() ->
    [{P, capabilities(P)} || P <- packages()].

%% -record(property, {name, type, values, defined_in, characteristics}).

%%----------------------------------------------------------------------
%% List all known packages
%% 'native' and 'all' are not real packages
%%----------------------------------------------------------------------

packages() ->
    [
     "g",        % Generic
     "root",	 % Base Root Package
     "tonegen",	 % Tone Generator Package
     "tonedet",	 % Tone Detection Package
     "dg",	 % Basic DTMF Generator Package
     "dd",	 % DTMF detection Package
     "cg",	 % Call Progress Tones Generator Package
     "cd",	 % Call Progress Tones Detection Package
     "al",	 % Analog Line Supervision Package
     "ct",	 % Basic Continuity Package
     "nt",	 % Network Package
     "rtp",	 % RTP Package
     "swb",	 % SwitchBoard Package
     "tdmc",     % TDM Circuit Package
     ""          % Native pseudo package
    ].

%%----------------------------------------------------------------------
%% List all matching capabilities
%%----------------------------------------------------------------------

capabilities(Package) ->
    case Package of
        "g"       -> capabilities_g();
        "root"    -> capabilities_root();
        "tonegen" -> capabilities_tonegen();
        "tonedet" -> capabilities_tonedet();
        "dg"      -> capabilities_dg();
        "dd"      -> capabilities_dd();
        "cg"      -> capabilities_cg();
        "cd"      -> capabilities_cd();
        "al"      -> capabilities_al();
        "ct"      -> capabilities_ct();
        "nt"      -> capabilities_nt();
        "rtp"     -> capabilities_rtp();
	"swb"     -> capabilities_swb();
        "tdmc"    -> capabilities_tdmc();
        ""        -> capabilities_native()
    end.

%%----------------------------------------------------------------------
%% Decode package name to internal form
%% Scope  ::= property | event | signal | statistics
%%----------------------------------------------------------------------

decode(mid, Package) ->
    decode_mid(Package);
decode(package, Package) ->
    decode_package(Package);
decode(profile, Package) ->
    decode_profile(Package);
decode(dialplan, Dialplan) ->
    decode_dialplan(Dialplan);
decode(Scope, [A, B | Item]) when is_atom(Scope) ->
    ?d("decode(~p) -> entry with"
       "~n   A:    ~p"
       "~n   B:    ~p"
       "~n   Item: ~p", [Scope, A, B, Item]),
    case decode_package([A, B]) of
	"" ->
 	    ?d("decode -> \"no\" package",[]),
	    decode_item(Scope, [A, B], Item);
	Package ->
 	    ?d("decode -> Package: ~p", [Package]),
	    Package ++ "/" ++ decode_item(Scope, [A, B], Item)
    end;
decode({Scope, [A, B | Item]}, SubItem) when is_atom(Scope) ->
    ?d("decode(~p) -> entry with"
       "~n   A:       ~p"
       "~n   B:       ~p"
       "~n   Item:    ~p"
       "~n   SubItem: ~p", [Scope, A, B, Item, SubItem]),
    decode_item({Scope, Item}, [A, B], SubItem).

decode_item(Scope, [A, B], Item) ->
    ?d("decode_item -> entry",[]),
    case A of
        16#00 -> 
            case B of
                16#01 -> decode_g(Scope, Item);
                16#02 -> decode_root(Scope, Item);
                16#03 -> decode_tonegen(Scope, Item);
                16#04 -> decode_tonedet(Scope, Item);
                16#05 -> decode_dg(Scope, Item);
                16#06 -> decode_dd(Scope, Item);
                16#07 -> decode_cg(Scope, Item);
                16#08 -> decode_cd(Scope, Item);
                16#09 -> decode_al(Scope, Item);
                16#0a -> decode_ct(Scope, Item);
                16#0b -> decode_nt(Scope, Item);
                16#0c -> decode_rtp(Scope, Item);
                16#0d -> decode_tdmc(Scope, Item);
                16#00 -> decode_native(Scope, Item)
            end;
        16#fe ->
            case B of
                %% Proprietary extension
                16#fe -> decode_swb(Scope, Item)
            end;
        16#ff ->
            case B of
                16#ff when Item =:= [16#ff, 16#ff] -> "*"
            end
    end.

decode_package(Package) ->
    ?d("decode_package -> entry with"
       "~n   Package: ~p", [Package]),
    [A, B] = Package,
    case A of
        16#00 -> 
            case B of
                16#01 -> "g";
                16#02 -> "root";
                16#03 -> "tonegen";
                16#04 -> "tonedet";
                16#05 -> "dg";
                16#06 -> "dd";
                16#07 -> "cg";
                16#08 -> "cd";
                16#09 -> "al";
                16#0a -> "ct";
                16#0b -> "nt";
                16#0c -> "rtp";
                16#0d -> "tdmc";
                16#00 -> ""
            end;
        16#fe ->
            case B of
                16#fe ->  "swb"
            end;
        16#ff ->
            case B of
                16#ff -> "*"
            end
    end.

decode_profile([A, B]) ->
    case A of
        16#00 -> 
            case B of
                16#fe -> "resgw";
		_     -> "profile" ++ [A + $0, B + $0]
            end;
	_ ->
	    "profile" ++ [A + $0, B + $0]
    end.

decode_dialplan([A, B]) ->
    "dialplan" ++ [A + $0, B + $0].

decode_mid(Mid) ->
    case Mid of
	{domainName, DN} ->
	    Lower = to_lower(DN#'DomainName'.name),	    
	    {domainName, DN#'DomainName'{name = Lower}};
	{deviceName, PathName} ->
	    Lower = to_lower(PathName),
	    {deviceName, Lower};
	Other ->
	    Other
    end.

to_lower(Chars) ->
    [?LOWER(Char) || Char <- Chars].

%%----------------------------------------------------------------------
%% Encode package name from internal form
%% Scope  ::= property | event | signal | statistics
%%----------------------------------------------------------------------

encode(mid, Package) ->
    encode_mid(Package);
encode(package, Package) ->
    encode_package(Package);
encode(profile, Profile) ->
    encode_profile(Profile);
encode(dialplan, Dialplan) ->
    encode_dialplan(Dialplan);
encode(Scope, PackageItem) when is_atom(Scope) ->
    ?d("encode(~p) -> entry with"
       "~n   PackageItem: ~p", [Scope, PackageItem]),
    case string:tokens(PackageItem, [$/]) of
	[Package, Item] ->
	    ?d("encode -> "
	       "~n   Package: ~p"
	       "~n   Item:    ~p", [Package, Item]),
	    encode_package(Package) ++ encode_item(Scope, Package, Item);
	[Item] ->
	    ?d("encode -> Item: ~p", [Item]),
	    [16#00, 16#00 | encode_native(Scope, Item)]
    end;
encode({Scope, PackageItem}, SubItem) when is_atom(Scope) ->
    ?d("encode(~p) -> entry with"
       "~n   PackageItem: ~p"
       "~n   SubItem:     ~p", [Scope, PackageItem, SubItem]),
    case string:tokens(PackageItem, [$/]) of
	[Package, Item] ->
	    ?d("encode -> "
	       "~n   Package: ~p"
	       "~n   Item:    ~p", [Package, Item]),
	    encode_item({Scope, Item}, Package, SubItem);
	[_Item] ->
	    ?d("encode -> _Item: ~p", [_Item]),
	    encode_native(Scope, SubItem)
    end.

encode_item(_Scope, _Package, "*") ->
    [16#ff, 16#ff];
encode_item(Scope, Package, Item) ->
    ?d("encode_item(~s) -> entry", [Package]),
    case Package of
        "g"       -> encode_g(Scope, Item);
        "root"    -> encode_root(Scope, Item);
        "tonegen" -> encode_tonegen(Scope, Item);
        "tonedet" -> encode_tonedet(Scope, Item);
        "dg"      -> encode_dg(Scope, Item);
        "dd"      -> encode_dd(Scope, Item);
        "cg"      -> encode_cg(Scope, Item);
        "cd"      -> encode_cd(Scope, Item);
        "al"      -> encode_al(Scope, Item);
        "ct"      -> encode_ct(Scope, Item);
        "nt"      -> encode_nt(Scope, Item);
        "rtp"     -> encode_rtp(Scope, Item);
        "tdmc"    -> encode_tdmc(Scope, Item);
        "swb"     -> encode_swb(Scope, Item)
    end.

encode_package(Package) ->
    case Package of
        "g"       -> [16#00, 16#01];
        "root"    -> [16#00, 16#02];
        "tonegen" -> [16#00, 16#03];
        "tonedet" -> [16#00, 16#04];
        "dg"      -> [16#00, 16#05];
        "dd"      -> [16#00, 16#06];
        "cg"      -> [16#00, 16#07];
        "cd"      -> [16#00, 16#08];
        "al"      -> [16#00, 16#09];
        "ct"      -> [16#00, 16#0a];
        "nt"      -> [16#00, 16#0b];
        "rtp"     -> [16#00, 16#0c];
        "tdmc"    -> [16#00, 16#0d];
        ""        -> [16#00, 16#00];
	"*"       -> [16#ff, 16#ff];
        "swb"     -> [16#fe, 16#fe]
    end.

encode_profile(Profile) ->
    case Profile of
        "resgw" ->
	    [16#00, 16#fe];
	[$p, $r, $o, $f, $i, $l, $e | Name] ->
	    case Name of
		[A, B] -> [A - $0, B - $0];
		[B]    -> [0, B - $0];
		[]     -> [0, 0]
	    end
    end.

encode_dialplan(Dialplan) ->
    case Dialplan of
	[$d, $i, $a, $l, $p, $l, $a, $n | Name] ->
	    case Name of
		[A, B] -> [A - $0, B - $0];
		[B]    -> [0, B - $0];
		[]     -> [0, 0]
	    end
    end.

encode_mid(Mid) ->
    Mid.


%%----------------------------------------------------------------------
%% Name:    g - Generic
%% Version: 1
%% Extends: None
%% Purpose: Generic package for commonly encountered items
%%----------------------------------------------------------------------

capabilities_g() ->
    [
     {event, "cause"},
     {event, "sc"}
    ].

encode_g(event, Item) ->
    case Item of
	"cause"  -> [16#00, 16#01];
	"sc"     -> [16#00, 16#02]
    end;

encode_g({event_parameter, Item}, SubItem) ->
    case Item of
	"cause"  -> 
	    case SubItem of
		"Generalcause" -> [16#00, 16#01];
		"Failurecause" -> [16#00, 16#02]
	    end;
	"sc" ->
	    case SubItem of
		"SigID" -> [16#00, 16#01];
		"Meth"  -> [16#00, 16#02];
		"SLID"  -> [16#00, 16#03];
		"RID"   -> [16#00, 16#04]
	    end
    end.

decode_g(event, Item) ->
    case Item of
	[16#00, 16#01] -> "cause";
	[16#00, 16#02] -> "sc"
    end;

decode_g({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event: cause
            case SubItem of
                [16#00, 16#01] -> "Generalcause";
		[16#00, 16#02] -> "Failurecause"
            end;

        [16#00, 16#02] -> % Event: sc
            case SubItem of
                [16#00, 16#01] -> "SigID";
                [16#00, 16#02] -> "Meth";
		[16#00, 16#03] -> "SLID";
		[16#00, 16#04] -> "RID"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    root - Base Root Package
%% Version: 2
%% Extends: None
%% Purpose: This package defines Gateway wide properties.
%%----------------------------------------------------------------------

capabilities_root() ->
    [
     {property, "maxNumberOfContexts"},
     {property, "maxTerminationsPerContext"},
     {property, "normalMGExecutionTime"},
     {property, "normalMGCExecutionTime"},
     {property, "MGProvisionalResponseTimerValue"},
     {property, "MGCProvisionalResponseTimerValue"},
     {property, "MGCOriginatedPendingLimit"},
     {property, "MGOriginatedPendingLimit"}
    ].

encode_root(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                "maxNumberOfContexts"              -> [16#00, 16#01];
                "maxTerminationsPerContext"        -> [16#00, 16#02];
                "normalMGExecutionTime"            -> [16#00, 16#03];
                "normalMGCExecutionTime"           -> [16#00, 16#04];
                "MGProvisionalResponseTimerValue"  -> [16#00, 16#05];
                "MGCProvisionalResponseTimerValue" -> [16#00, 16#06];
                "MGCOriginatedPendingLimit"        -> [16#00, 16#07];
                "MGOriginatedPendingLimit"         -> [16#00, 16#08]
            end
    end.

decode_root(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                [16#00, 16#01] -> "maxNumberOfContexts";
                [16#00, 16#02] -> "maxTerminationsPerContext";
                [16#00, 16#03] -> "normalMGExecutionTime";
                [16#00, 16#04] -> "normalMGCExecutionTime";
                [16#00, 16#05] -> "MGProvisionalResponseTimerValue";
		[16#00, 16#06] -> "MGCProvisionalResponseTimerValue";
		[16#00, 16#07] -> "MGCOriginatedPendingLimit";
		[16#00, 16#08] -> "MGOriginatedPendingLimit"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    tonegen - Tone Generator Package
%% Version: 2
%% Extends: None
%% Purpose: This package defines signals to generate audio tones.
%%          This package does not specify parameter values. It is
%%          intended to be extendable. Generally, tones are defined
%%          as an individual signal with a parameter, ind,
%%          representing "interdigit" time delay, and a tone id to
%%          be used with playtones.  A tone id should be kept
%%          consistent with any tone generation for the same tone.
%%          MGs are expected to be provisioned with the characteristics
%%          of appropriate tones for the country in which the MG is located.
%%----------------------------------------------------------------------

capabilities_tonegen() ->
    [
     {signal, "pt"}
    ].

encode_tonegen(signal, Item) ->
    case Item of
	"pt" -> [16#00, 16#01]
    end;

encode_tonegen({signal_parameter, Item}, SubItem) ->
    case Item of
        "pt" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "ind" -> [16#00, 16#02];
                "btd" -> [16#00, 16#03]
            end
    end.

decode_tonegen(signal, Item) ->
    case Item of
	[16#00, 16#01] -> "pt"
    end;

decode_tonegen({signal_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event: pt
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#02] -> "ind";
                [16#00, 16#03] -> "btd"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    tonedet - Tone Detection Package
%% Version: 1
%% Extends: None
%% Purpose: This Package defines events for audio tone detection.
%%          Tones are selected by name (tone id). MGs are expected
%%          to be provisioned with the characteristics of appropriate
%%          tones for the country in which the MG is located.
%%          
%%          This package does not specify parameter values.
%%          It is intended to be extendable.
%%----------------------------------------------------------------------

capabilities_tonedet() ->
    [
     {event, "std"},
     {event, "etd"},
     {event, "ltd"}
    ].

encode_tonedet(event, Item) ->
    case Item of
	"std" -> [16#00, 16#01];
	"etd" -> [16#00, 16#02];
	"ltd" -> [16#00, 16#03]
    end;

encode_tonedet({event_parameter, Item}, SubItem) ->
    case Item of
        "std" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "tid" -> [16#00, 16#03]
            end;
        "etd" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "tid" -> [16#00, 16#03];
                "dur" -> [16#00, 16#02]
            end;
        "ltd" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "dur" -> [16#00, 16#02];
                "tid" -> [16#00, 16#03]
            end
    end.

decode_tonedet(event, Item) ->
    case Item of
	[16#00, 16#01] -> "std";
	[16#00, 16#02] -> "etd";
	[16#00, 16#03] -> "ltd"
    end;

decode_tonedet({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event std
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#03] -> "tid"
            end;
        [16#00, 16#02] -> % Event etd
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#03] -> "tid";
                [16#00, 16#02] -> "dur"
            end;
        [16#00, 16#03] -> % Event ltd
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#02] -> "dur";
                [16#00, 16#03] -> "tid"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    dg - Basic DTMF Generator Package
%% Version: 1
%% Extends: tonegen  version 1
%% Purpose: This package defines the basic DTMF tones as signals and
%%          extends the allowed values of parameter tl of playtone
%%          in tonegen.
%%----------------------------------------------------------------------

capabilities_dg() ->
    [
     {signal, "d0"},
     {signal, "d1"},
     {signal, "d2"},
     {signal, "d3"},
     {signal, "d4"},
     {signal, "d5"},
     {signal, "d6"},
     {signal, "d7"},
     {signal, "d8"},
     {signal, "d9"},
     {signal, "ds"},
     {signal, "do"},
     {signal, "da"},
     {signal, "db"},
     {signal, "dc"},
     {signal, "dd"}
    ].

encode_dg(signal, Item) ->
    case Item of
	"d0" -> [16#00, 16#10];
	"d1" -> [16#00, 16#11];
	"d2" -> [16#00, 16#12];
	"d3" -> [16#00, 16#13];
	"d4" -> [16#00, 16#14];
	"d5" -> [16#00, 16#15];
	"d6" -> [16#00, 16#16];
	"d7" -> [16#00, 16#17];
	"d8" -> [16#00, 16#18];
	"d9" -> [16#00, 16#19];
	"ds" -> [16#00, 16#20];
	"do" -> [16#00, 16#21];
	"da" -> [16#00, 16#1a];
	"db" -> [16#00, 16#1b];
	"dc" -> [16#00, 16#1c];
	"dd" -> [16#00, 16#1d]
    end;

encode_dg({signal_parameter, Item}, SubItem) ->
    case Item of
	"d0" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d1" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d2" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d3" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d4" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d5" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d6" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d7" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d8" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d9" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"ds" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"do" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"da" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"db" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"dc" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"dd" ->
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end
    end.

decode_dg(signal, Item) ->
    case Item of
	[16#00, 16#10] -> "d0";
	[16#00, 16#11] -> "d1";
	[16#00, 16#12] -> "d2";
	[16#00, 16#13] -> "d3";
	[16#00, 16#14] -> "d4";
	[16#00, 16#15] -> "d5";
	[16#00, 16#16] -> "d6";
	[16#00, 16#17] -> "d7";
	[16#00, 16#18] -> "d8";
	[16#00, 16#19] -> "d9";
	[16#00, 16#20] -> "ds";
	[16#00, 16#21] -> "do";
	[16#00, 16#1a] -> "da";
	[16#00, 16#1b] -> "db";
	[16#00, 16#1c] -> "dc";
	[16#00, 16#1d] -> "dd"
    end;

decode_dg({signal_parameter, Item}, SubItem) ->
    case Item of
	[16#00, 16#10] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#11] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#12] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#13] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#14] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#15] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#16] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#17] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#18] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#19] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#20] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#21] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1a] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1b] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1c] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1d] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end
    end.


%%----------------------------------------------------------------------
%% Name:    dd - DTMF detection Package
%% Version: 1
%% Extends: tonedet version 1
%% Purpose: This package defines the basic DTMF tones detection.
%%          Tones are selected by name (tone id). MGs are expected
%%          to be provisioned with the characteristics of appropriate
%%          tones for the country in which the MG is located.
%%          
%%          This package does not specify parameter values.
%%          It is intended to be extendable.
%% 
%% Additional tone id values are all tone ids described in package dg
%% (basic DTMF generator package).
%% 
%% The following table maps DTMF events to digit map symbols as described
%% in section 7.1.14.
%% 
%%                    _________________________________
%%                   | DTMF Event        | Symbol     |
%%                   | d0                |  "0"       |
%%                   | d1                |  "1"       |
%%                   | d2                |  "2"       |
%%                   | d3                |  "3"       |
%%                   | d4                |  "4"       |
%%                   | d5                |  "5"       |
%%                   | d6                |  "6"       |
%%                   | d7                |  "7"       |
%%                   | d8                |  "8"       |
%%                   | d9                |  "9"       |
%%                   | da                |  "A" or "a"|
%%                   | db                |  "B" or "b"|
%%                   | dc                |  "C" or "c"|
%%                   | dd                |  "D" or "d"|
%%                   | ds                |  "E" or "e"|
%%                   | do                |  "F" or "f"|
%%                   |___________________|____________|
%% 
%%----------------------------------------------------------------------

capabilities_dd() ->
    [
     {event, "ce"},
     {event, "d0"},
     {event, "d1"},
     {event, "d2"},
     {event, "d3"},
     {event, "d4"},
     {event, "d5"},
     {event, "d6"},
     {event, "d7"},
     {event, "d8"},
     {event, "d9"},
     {event, "ds"},
     {event, "do"},
     {event, "da"},
     {event, "db"},
     {event, "dc"},
     {event, "dd"}
    ].

encode_dd(event, Item) ->
    case Item of
	"ce" -> [16#00, 16#04];
	"d0" -> [16#00, 16#10];
	"d1" -> [16#00, 16#11];
	"d2" -> [16#00, 16#12];
	"d3" -> [16#00, 16#13];
	"d4" -> [16#00, 16#14];
	"d5" -> [16#00, 16#15];
	"d6" -> [16#00, 16#16];
	"d7" -> [16#00, 16#17];
	"d8" -> [16#00, 16#18];
	"d9" -> [16#00, 16#19];
	"ds" -> [16#00, 16#20];
	"do" -> [16#00, 16#21];
	"da" -> [16#00, 16#1a];
	"db" -> [16#00, 16#1b];
	"dc" -> [16#00, 16#1c];
	"dd" -> [16#00, 16#1d]
    end;

encode_dd({event_parameter, Item}, SubItem) ->
    case Item of
        "ce" ->
            case SubItem of
                "ds"   -> [16#00, 16#01];
		"Meth" -> [16#00, 16#03]
            end;
	"d0" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d1" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d2" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d3" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d4" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d5" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d6" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d7" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d8" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"d9" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"ds" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"do" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"da" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"db" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"dc" -> 
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end;
	"dd" ->
	    case SubItem of
		"btd" -> [16#00, 16#01]
	    end
    end.

decode_dd(event, Item) ->
    case Item of
	[16#00, 16#04] -> "ce";
	[16#00, 16#10] -> "d0";
	[16#00, 16#11] -> "d1";
	[16#00, 16#12] -> "d2";
	[16#00, 16#13] -> "d3";
	[16#00, 16#14] -> "d4";
	[16#00, 16#15] -> "d5";
	[16#00, 16#16] -> "d6";
	[16#00, 16#17] -> "d7";
	[16#00, 16#18] -> "d8";
	[16#00, 16#19] -> "d9";
	[16#00, 16#20] -> "ds";
	[16#00, 16#21] -> "do";
	[16#00, 16#1a] -> "da";
	[16#00, 16#1b] -> "db";
	[16#00, 16#1c] -> "dc";
	[16#00, 16#1d] -> "dd"
    end;

decode_dd({event_parameter, Item}, SubItem) ->
    case Item of
	[16#00, 16#04] -> % Event ce
            case SubItem of
                [16#00, 16#01] -> "ds";
		[16#00, 16#03] -> "Meth"
            end;
	[16#00, 16#10] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#11] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#12] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#13] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#14] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#15] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#16] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#17] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#18] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#19] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#20] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#21] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1a] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1b] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1c] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end;
	[16#00, 16#1d] -> 
	    case SubItem of
		[16#00, 16#01] -> "btd"
	    end
    end.

%%----------------------------------------------------------------------
%% Name:    cg - Call Progress Tones Generator Package
%% Version: 1
%% Extends: tonegen version 1
%% Purpose: This package defines the basic call progress tones as signals
%%          and extends the allowed values of the tl parameter of
%%          playtone in tonegen.
%%----------------------------------------------------------------------

capabilities_cg() ->
    [
     {signal, "dt"},
     {signal, "rt"},
     {signal, "bt"},
     {signal, "ct"},
     {signal, "sit"},
     {signal, "wt"},
     {signal, "prt"},
     {signal, "cw"},
     {signal, "cr"}
    ].


encode_cg(Scope, Item) ->
    case Scope of
        signal ->
            case Item of
                "dt"  -> [16#00, 16#30];
                "rt"  -> [16#00, 16#31];
                "bt"  -> [16#00, 16#32];
                "ct"  -> [16#00, 16#33];
                "sit" -> [16#00, 16#34];
                "wt"  -> [16#00, 16#35];
                "prt" -> [16#00, 16#36];
                "cw"  -> [16#00, 16#37];
                "cr"  -> [16#00, 16#38]
            end
    end.

decode_cg(Scope, Item) ->
    case Scope of
        signal ->
            case Item of
                [16#00, 16#30] -> "dt";              
                [16#00, 16#31] -> "rt";           
                [16#00, 16#32] -> "bt";              
                [16#00, 16#33] -> "ct";        
                [16#00, 16#34] -> "sit";         
                [16#00, 16#35] -> "wt";              
                [16#00, 16#36] -> "prt"; 
                [16#00, 16#37] -> "cw";         
                [16#00, 16#38] -> "cr"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    cd - Call Progress Tones Detection Package
%% Version: 1
%% Extends: tonedet version 1
%% Purpose: This package defines the basic call progress detection tones.
%%          This Package extends the possible values of tone id
%%          in the "start tone detected", "end tone detected" and
%%          "long tone detected" events.
%%       Additional values
%%             tone id values are defined for start tone detected,
%%                   end tone detected and long tone detected with
%%                   the same values as those in package cg (call
%%                   progress tones generation package).
%% 
%% The required set of tone ids corresponds to Recommendation E.180/Q.35
%% [ITU-T Recommendation E.180/Q.35 (1998)].  See Recommendation E.180/Q.35
%% for definition of the meanings of these tones.
%%----------------------------------------------------------------------

capabilities_cd() ->
    [
     {event, "dt"},
     {event, "rt"},
     {event, "bt"},
     {event, "ct"},
     {event, "sit"},
     {event, "wt"},
     {event, "prt"},
     {event, "cw"},
     {event, "cr"}
    ].


encode_cd(Scope, Item) ->
    case Scope of
        event ->
            case Item of
                "dt" -> [16#00, 16#30];
                "rt" -> [16#00, 16#31];
                "bt" -> [16#00, 16#32];
                "ct" -> [16#00, 16#33];
                "sit"-> [16#00, 16#34];
                "wt" -> [16#00, 16#35];
                "prt"-> [16#00, 16#36];
                "cw" -> [16#00, 16#37];
                "cr" -> [16#00, 16#38]
            end
    end.

decode_cd(Scope, Item) ->
    case Scope of
        event ->
            case Item of
                [16#00, 16#30] -> "dt";              
                [16#00, 16#31] -> "rt";           
                [16#00, 16#32] -> "bt";              
                [16#00, 16#33] -> "ct";        
                [16#00, 16#34] -> "sit";         
                [16#00, 16#35] -> "wt";              
                [16#00, 16#36] -> "prt"; 
                [16#00, 16#37] -> "cw";         
                [16#00, 16#38] -> "cr"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    al - Analog Line Supervision Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines events and signals for an analog line.
%%----------------------------------------------------------------------

capabilities_al() ->
    [
     {event,  "on"},
     {event,  "of"},
     {event,  "fl"},
     {signal, "ri"}
    ].

encode_al(event, Item) ->
    ?d("encode_al(event) -> entry with"
       "~n   Item: ~p", [Item]),
    case Item of
	"on" -> [16#00, 16#04];
	"of" -> [16#00, 16#05];
	"fl" -> [16#00, 16#06]
    end;

encode_al({event_parameter, Item}, SubItem) ->
    ?d("encode_al({event_parameter,~p}) -> entry with"
       "~n   SubItem: ~p", [Item, SubItem]),
    case Item of
	"on" ->
            case SubItem of
                "strict" -> [16#00, 16#01];
                "init"   -> [16#00, 16#02]
            end;
        "of" ->
            case SubItem of
                "strict" -> [16#00, 16#01];
                "init"   -> [16#00, 16#02]
            end;
        "fl" ->
            case SubItem of
                "mindur" -> [16#00, 16#04];
                "maxdur" -> [16#00, 16#05]
            end
    end;

encode_al(signal, Item) ->
    ?d("encode_al(signal) -> entry with"
       "~n   Item: ~p", [Item]),
    case Item of
	"ri"    -> [16#00, 16#02]
    end;

encode_al({signal_parameter, Item}, SubItem) ->
    ?d("encode_al({signal_parameter,~p}) -> entry with"
       "~n   SubItem: ~p", [Item, SubItem]),
    case Item of
        "ri" ->
            case SubItem of
                "cad"  -> [16#00, 16#06];
                "freq" -> [16#00, 16#07]
            end
    end.

decode_al(event, SubItem) ->
    ?d("decode_al(event) -> entry with"
       "~n   SubItem: ~p", [SubItem]),
    case SubItem of
	[16#00, 16#04] -> "on";
	[16#00, 16#05] -> "of";
	[16#00, 16#06] -> "fl"
    end;

decode_al({event_parameter, Item}, SubItem) ->
    ?d("decode_al({event_parameter,~p}) -> entry with"
       "~n   SubItem: ~p", [Item, SubItem]),
    case Item of
        [16#00,16#04] -> %% Event: on
            case SubItem of
		[16#00, 16#01] -> "strict";
                [16#00, 16#02] -> "init"
            end;
        [16#00,16#05] -> %% Event: of
            case SubItem of
		[16#00, 16#01] -> "strict";
                [16#00, 16#02] -> "init"
            end;
        [16#00,16#06] -> %% Event: fl
            case SubItem of
		[16#00, 16#04] -> "mindur";
                [16#00, 16#05] -> "maxdur"
            end
    end;

decode_al(signal, SubItem) ->
    ?d("decode_al(signal) -> entry with"
       "~n   SubItem: ~p", [SubItem]),
    case SubItem of
	[16#00, 16#02] -> "ri"
    end;

decode_al({signal_parameter, Item}, SubItem) ->
    ?d("decode_al({signal_parameter,~p}) -> entry with"
       "~n   SubItem: ~p", [Item, SubItem]),
    case Item of
        [16#00,16#02] -> %% Event: ri
            case SubItem of
		[16#00, 16#06] -> "cad";
                [16#00, 16#07] -> "freq"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    ct - Basic Continuity Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines events and signals for continuity test.
%%          The continuity test includes provision of either a loopback
%%          or transceiver functionality.
%%----------------------------------------------------------------------

capabilities_ct() ->
    [
     {event,  "cmp"},
     {signal, "ct"},
     {signal, "rsp"}
    ].

encode_ct(event, Item) ->
    case Item of
	"cmp" -> [16#00, 16#05]
    end;
encode_ct({event_parameter, Item}, SubItem) ->
    case Item of
        "cmp" ->
            case SubItem of
                "res" -> [16#00, 16#08]
            end
    end;
encode_ct(signal, Item) ->
    case Item of
	"ct"  -> [16#00, 16#03];
	"rsp" -> [16#00, 16#04]
    end.

decode_ct(event, Item) ->
    case Item of
	[16#00, 16#05] -> "cmp"
    end;
decode_ct({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#05] -> % Event cmp
            case SubItem of
                [16#00, 16#08] -> "res"
            end
    end;
decode_ct(signal, Item) ->
    case Item of
	[16#00, 16#03] -> "ct";
	[16#00, 16#04] -> "rsp"
    end.

%%----------------------------------------------------------------------
%% Name:    nt - Network Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines properties of network terminations
%%          independent of network type.
%%----------------------------------------------------------------------

capabilities_nt() ->
    [
     {property,   "jit"},
     {event,      "netfail"},
     {event,      "qualert"},
     {statistics, "dur"},
     {statistics, "os"},
     {statistics, "or"}
    ].

encode_nt(property, Item) ->
    case Item of
	"jit" -> [16#00, 16#07]
    end;
encode_nt(event, Item) ->
    case Item of
	"netfail" -> [16#00, 16#05];
	"qualert" -> [16#00, 16#06]
    end;
encode_nt({event_parameter, Item}, SubItem) ->
    case Item of
        "netfail" ->
            case SubItem of
                "cs" -> [16#00, 16#01]
            end;
        "qualert" ->
            case SubItem of
                "th"  -> [16#00, 16#01]
            end
    end;
encode_nt(statistics, Item) ->
    case Item of
	"dur" -> [16#00, 16#01];
	"os"  -> [16#00, 16#02];
	"or"  -> [16#00, 16#03]
    end.

decode_nt(property, Item) ->
    case Item of
	[16#00, 16#07] -> "jit"
    end;
decode_nt(event, Item) ->
    case Item of
	[16#00, 16#05] -> "netfail";
	[16#00, 16#06] -> "qualert"
    end;
decode_nt({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#05] -> % Event netfail
            case SubItem of
                [16#00, 16#01] -> "cs"
            end;
        [16#00, 16#06] -> % Event qualert
            case SubItem of
                [16#00, 16#01] -> "th"
            end
	end;
decode_nt(statistics, Item) ->
    case Item of
	[16#00, 16#01] -> "dur";
	[16#00, 16#02] -> "os";
	[16#00, 16#03] -> "or"
    end.

%%----------------------------------------------------------------------
%% Name:    rtp - RTP Package
%% Version: 1
%% Extends: nt version 1
%% Purpose: This package is used to support packet based multimedia
%%          data transfer by means of the Real-time Transport Protocol
%%          (RTP) [RFC 1889].
%%----------------------------------------------------------------------

capabilities_rtp() ->
    [
     {event,      "pltrans"},
     {statistics, "ps"},
     {statistics, "pr"},
     {statistics, "pl"},
     {statistics, "jit"},
     {statistics, "delay"}
    ].

encode_rtp(event, Item) ->
    case Item of
	"pltrans" -> [16#00, 16#01]
    end;
encode_rtp({event_parameter, Item}, SubItem) ->
    case Item of
        "pltrans" ->
            case SubItem of
                "rtppltype" -> [16#00, 16#01]
            end
    end;
encode_rtp(statistics, Item) ->
    case Item of
	"ps"    -> [16#00, 16#04];
	"pr"    -> [16#00, 16#05];
	"pl"    -> [16#00, 16#06];
	"jit"   -> [16#00, 16#07];
	"delay" -> [16#00, 16#08]
    end.

decode_rtp(event, Item) ->
    case Item of
	[16#00, 16#01] -> "pltrans"
    end;
decode_rtp({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event pltrans
            case SubItem of
                [16#00, 16#01] -> "rtppltype"
            end
    end;
decode_rtp(statistics, Item) ->
    case Item of
	[16#00, 16#04] -> "ps";
	[16#00, 16#05] -> "pr";
	[16#00, 16#06] -> "pl";
	[16#00, 16#07] -> "jit";
	[16#00, 16#08] -> "delay"
    end.


%%----------------------------------------------------------------------
%% Name:    tdmc - TDM Circuit Package
%% Version: 1
%% Extends: nt version 1
%% Purpose: This package is used to support TDM circuit terminations.
%%----------------------------------------------------------------------

capabilities_tdmc() ->
    [
     {property, "ec"},
     {property, "gain"}
    ].

encode_tdmc(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                "ec"   -> [16#00, 16#08];
                "gain" -> [16#00, 16#0a]
            end
    end.

decode_tdmc(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                [16#00, 16#08] -> "ec";
                [16#00, 16#0a] -> "gain"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    swb - SwitchBoard Package
%% Version: 1
%% Extends: none
%% Purpose: This package is used to support SwitchBoard specials
%%----------------------------------------------------------------------

capabilities_swb() ->
    [
     {statistics, "fs"}, % Free slots
     {statistics, "as"}  % Allocated slots
    ].

encode_swb(Scope, Item) ->
    case Scope of
        statistics ->
            case Item of
                "fs" -> [16#00, 16#00];
                "as" -> [16#00, 16#01]
            end
    end.

decode_swb(Scope, Item) ->
    case Scope of
        statistics ->
            case Item of
                [16#00, 16#00] -> "fs";
                [16#00, 16#01] -> "as"
            end
    end.


%%----------------------------------------------------------------------
%% Name:  native -  Pseudo package  
%% Version: 1
%% Extends: None
%% Purpose: Native tags for media stream properties
%%
%% Parameters for Local descriptors and Remote descriptors are
%% specified as tag-value pairs if binary encoding is used for the
%% protocol.  This annex contains the property names (PropertyID), the
%% tags (Property Tag), type of the property (Type) and the values
%% (Value).Values presented in the Value field when the field contains
%% references shall be regarded as "information". The reference
%% contains the normative values.  If a value field does not contain a
%% reference then the values in that field can be considered as
%% "normative".
%% 
%% Tags are given as hexadecimal numbers in this annex. When setting
%% the value of a property, a MGC may underspecify the value according
%% to one of the mechanisms specified in section 7.1.1.
%% 
%% For type "enumeration" the value is represented by the value in brack-
%% ets, e.g., Send(0), Receive(1).
%%----------------------------------------------------------------------
%% 
%% C.6.  IP
%% 
%%     ________________________________________________________________
%%    | PropertyID|  Tag       |  Type        |  Value                |
%%    | IPv4      |  6001      |  32 BITS     |  Ipv4Address          |
%%    | IPv6      |  6002      |  128 BITS    |  IPv6 Address         |
%%    | Port      |  6003      |  Unsigned Int|  Port                 |
%%    | Porttype  |  6004      |  Enumerated  |  TCP(0),UDP(1),SCTP(2)|
%%    |___________|____________|______________|_______________________|
%%    
%% 
%% C.11.  SDP Equivalents
%% 
%%      ______________________________________________________________
%%     | PropertyID|  Tag |  Type  |  Value                          |
%%     | SDP_V     |  B001|  STRING|  Protocol Version               |
%%     | SDP_O     |  B002|  STRING|  Owner-creator and session ID   |
%%     | SDP_S     |  B003|  STRING|  Sesson name                    |
%%     | SDP_I     |  B004|  STRING|  Session identifier             |
%%     | SDP_U     |  B005|  STRING|  URI of descriptor              |
%%     | SDC_E     |  B006|  STRING|  email address                  |
%%     | SDP_P     |  B007|  STRING|  phone number                   |
%%     | SDP_C     |  B008|  STRING|  Connection information         |
%%     | SDP_B     |  B009|  STRING|  Bandwidth Information          |
%%     | SDP_Z     |  B00A|  STRING|  time zone adjustment           |
%%     | SDP_K     |  B00B|  STRING|  Encryption Key                 |
%%     | SDP_A     |  B00C|  STRING|  Zero or more session attributes|
%%     | SDP_T     |  B00D|  STRING|  Active Session Time            |
%%     | SDP_R     |  B00E|  STRING|  Zero or more repeat times      |
%%     | SDP_M     |  B00F|  STRING|  Media name and transport addr  |
%%     |           |      |        |  Reference: IETF RFC 2327       |
%%     |___________|______|________|_________________________________|
%% 
%%----------------------------------------------------------------------

capabilities_native() ->
    [
     %% C.6.  IP 
     {property, "IPv4"},
     {property, "IPv6"},
     {property, "Port"},
     {property, "Porttype"},

     %% C.11. SDP Equivalents
     {property, "v"},
     {property, "o"},
     {property, "s"},
     {property, "i"},
     {property, "u"},
     {property, "e"},
     {property, "p"},
     {property, "c"},
     {property, "b"},
     {property, "z"},
     {property, "k"},
     {property, "a"},
     {property, "t"},
     {property, "r"},
     {property, "m"}
    ].

encode_native(Scope, Item) ->
    case Scope of
        property ->
	    case Item of
		%% IP
		"IPv4"     -> [16#60, 16#01];
		"IPv6"     -> [16#60, 16#02];
		"Port"     -> [16#60, 16#03];
		"Porttype" -> [16#60, 16#04];

		%% SDP
		"v" -> [16#b0, 16#01];
		"o" -> [16#b0, 16#02];
		"s" -> [16#b0, 16#03];
		"i" -> [16#b0, 16#04];
		"u" -> [16#b0, 16#05];
		"e" -> [16#b0, 16#06];
		"p" -> [16#b0, 16#07];
		"c" -> [16#b0, 16#08];
		"b" -> [16#b0, 16#09];
		"z" -> [16#b0, 16#0a];
		"k" -> [16#b0, 16#0b];
		"a" -> [16#b0, 16#0c];
		"t" -> [16#b0, 16#0d];
		"r" -> [16#b0, 16#0e];
		"m" -> [16#b0, 16#0f] 
	    end
    end.

decode_native(Scope, [Type, Item]) ->
    case Scope of
        property ->
            case Type of
		16#60 ->
		    case Item of
			16#01 -> "IPv4";
			16#02 -> "IPv6";
			16#03 -> "Port";
			16#04 -> "Porttype"
		    end;

                16#b0 ->
		    case Item of
			16#01 -> "v";
			16#02 -> "o";
			16#03 -> "s";
			16#04 -> "i";
			16#05 -> "u";
			16#06 -> "e";
			16#07 -> "p";
			16#08 -> "c";
			16#09 -> "b";
			16#0a -> "z";
			16#0b -> "k";
			16#0c -> "a";
			16#0d -> "t";
			16#0e -> "r";
			16#0f -> "m"
		    end
            end
    end.

%% -------------------------------------------------------------------

% error(Reason) ->
%     erlang:error(Reason).
 
