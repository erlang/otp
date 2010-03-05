%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : CosNotification_Definitions.hrl
%% Purpose : 
%%----------------------------------------------------------------------

-ifndef(COSNOTIFICATION_DEFINITIONS_HRL).
-define(COSNOTIFICATION_DEFINITIONS_HRL, true).

%% ---------------- General comment ------------------------------------
%% ******* README ********
%% The prefix 'not' is short for notification, and is used to separate locally
%% defined macros from the global ones, i.e., do NOT confuse this with a negation!!
%%
%% In this file you find globally used data structures, constants etc.
%%

%%--------------- INCLUDES ---------------------------------------------

%%-------- Constants -------------------------------------------------
-define(not_SupportedGrammars, ["EXTENDED_TCL"]).

%% !!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%
%% If OMG redefines the values for the constants the definitions
%% below must be redefined!!
%%
%% !!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-define(not_BestEffort,            0).
-define(not_Persistent,            1).
-define(not_EventReliability,      "EventReliability").
-define(not_ConnectionReliability, "ConnectionReliability").
-define(not_Priority,              "Priority").
-define(not_LowestPriority,        -32767).
-define(not_HighestPriority,        32767).
-define(not_DefaultPriority,        0).
-define(not_StartTime,              "StartTime").
-define(not_StopTime,               "StopTime").
-define(not_Timeout,                "Timeout").
-define(not_OrderPolicy,            "OrderPolicy").
-define(not_AnyOrder,               0).
-define(not_FifoOrder,              1).
-define(not_PriorityOrder,          2).
-define(not_DeadlineOrder,          3).
-define(not_DiscardPolicy,          "DiscardPolicy").
-define(not_LifoOrder,              4).
-define(not_RejectNewEvents,        5).
-define(not_MaximumBatchSize,       "MaximumBatchSize").
-define(not_PacingInterval,         "PacingInterval").
-define(not_StartTimeSupported,     "StartTimeSupported").
-define(not_StopTimeSupported,      "StopTimeSupported").
-define(not_MaxEventsPerConsumer,   "MaxEventsPerConsumer").
-define(not_MaxQueueLength,         "MaxQueueLength").
-define(not_MaxConsumers,           "MaxConsumers").
-define(not_MaxSuppliers,           "MaxSuppliers").

%%--------------- QOS DEFINITIONS ----------------------------
%% Limits for QoS. These are our own limits.
-define(not_MaxBatchSize,      10000).
-define(not_MinBatchSize,      1).
-define(not_MinTimeout,        0).
-define(not_MaxTimeout,        100000000000).
-define(not_MinPacing,         0).
-define(not_MaxPacing,         100000000000).
-define(not_MinConsumerEvents, 1).
-define(not_MaxConsumerEvents, 10000).

-define(not_QOS_LIMITS, 
[#'CosNotification_NamedPropertyRange'
 {name=?not_EventReliability, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:short(), ?not_BestEffort), 
    high_val=any:create(orber_tc:short(), ?not_Persistent)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_ConnectionReliability, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:short(), ?not_BestEffort), 
    high_val=any:create(orber_tc:short(), ?not_Persistent)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_Priority, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:short(), ?not_LowestPriority), 
    high_val=any:create(orber_tc:short(), ?not_HighestPriority)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_StartTimeSupported, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:boolean(), false), 
    high_val=any:create(orber_tc:boolean(), true)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_StopTimeSupported, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:boolean(), false), 
    high_val=any:create(orber_tc:boolean(), true)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_Timeout, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:unsigned_long_long(), ?not_MinTimeout), 
    high_val=any:create(orber_tc:unsigned_long_long(), ?not_MaxTimeout)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_OrderPolicy, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:short(), ?not_AnyOrder), 
    high_val=any:create(orber_tc:short(), ?not_PriorityOrder)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_DiscardPolicy, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:short(), ?not_AnyOrder), 
    high_val=any:create(orber_tc:short(), ?not_PriorityOrder)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_MaximumBatchSize, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:long(), ?not_MinBatchSize), 
    high_val=any:create(orber_tc:long(), ?not_MaxBatchSize)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_PacingInterval, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:unsigned_long_long(), ?not_MinPacing), 
    high_val=any:create(orber_tc:unsigned_long_long(), ?not_MaxPacing)
   }},
 #'CosNotification_NamedPropertyRange'
 {name=?not_MaxEventsPerConsumer, 
  range=
  #'CosNotification_PropertyRange'{
    low_val=any:create(orber_tc:long(), ?not_MinConsumerEvents), 
    high_val=any:create(orber_tc:long(), ?not_MaxConsumerEvents)
   }}
]).



%% Local record used internally, and the reason for this is we get faster
%% access to QoS settings.
-record(qos, {'EventReliability',
	      'ConnectionReliability',
	      'Priority',
	      'StartTimeSupported',
	      'StopTimeSupported',
	      'Timeout',
	      'OrderPolicy',
	      'DiscardPolicy',
	      'MaximumBatchSize',
	      'PacingInterval',
	      'MaxEventsPerConsumer'}).

%% Global (OMG) representation of QoS.
-define(not_DEFAULT_QOS, 
[#'CosNotification_Property'{name=?not_MaximumBatchSize, 
			     value=any:create(orber_tc:long(), 1)},
 #'CosNotification_Property'{name=?not_PacingInterval, 
			     value=any:create(orber_tc:unsigned_long_long(), 0)},
 #'CosNotification_Property'{name=?not_Timeout, 
			     value=any:create(orber_tc:unsigned_long_long(), 0)},
 #'CosNotification_Property'{name=?not_MaxEventsPerConsumer, 
			     value=any:create(orber_tc:long(), 100)},
 #'CosNotification_Property'{name=?not_OrderPolicy, 
			     value=any:create(orber_tc:short(), 
					      ?not_PriorityOrder)},
 #'CosNotification_Property'{name=?not_EventReliability, 
			     value=any:create(orber_tc:short(), 
					      ?not_BestEffort)},
 #'CosNotification_Property'{name=?not_ConnectionReliability, 
			     value=any:create(orber_tc:short(), 
					      ?not_BestEffort)},
 #'CosNotification_Property'{name=?not_DiscardPolicy, 
			     value=any:create(orber_tc:short(), 
					      ?not_RejectNewEvents)},
 #'CosNotification_Property'{name=?not_StartTimeSupported, 
			     value=any:create(orber_tc:boolean(), false)},
 #'CosNotification_Property'{name=?not_StopTimeSupported, 
			     value=any:create(orber_tc:boolean(), false)},
 #'CosNotification_Property'{name=?not_Priority, 
			     value=any:create(orber_tc:short(), ?not_DefaultPriority)}]).

%%--------------- QOS CREATORS -------------------------------
-define(not_CreateInitQoS(), #qos{}).

%%--------------- QOS DESTRUCTORS ----------------------------
-define(not_DestroyQoS(Q), ok).

%%--------------- QOS SELECTORS ------------------------------
-define(not_GetEventReliability(Q),        Q#qos.'EventReliability').
-define(not_GetConnectionReliability(Q),   Q#qos.'ConnectionReliability').
-define(not_GetPriority(Q),                Q#qos.'Priority').
-define(not_GetStartTimeSupported(Q),      Q#qos.'StartTimeSupported').
-define(not_GetStopTimeSupported(Q),       Q#qos.'StopTimeSupported').
-define(not_GetTimeout(Q),                 Q#qos.'Timeout').
-define(not_GetOrderPolicy(Q),             Q#qos.'OrderPolicy').
-define(not_GetDiscardPolicy(Q),           Q#qos.'DiscardPolicy').
-define(not_GetMaximumBatchSize(Q),        Q#qos.'MaximumBatchSize').
-define(not_GetPacingInterval(Q),          Q#qos.'PacingInterval').
-define(not_GetMaxEventsPerConsumer(Q),    Q#qos.'MaxEventsPerConsumer').

%%--------------- QOS MODIFIERS ------------------------------
-define(not_SetEventReliability(Q,D),      Q#qos{'EventReliability'=D}).
-define(not_SetConnectionReliability(Q,D), Q#qos{'ConnectionReliability'=D}).
-define(not_SetPriority(Q,D),              Q#qos{'Priority'=D}).
-define(not_SetStartTimeSupported(Q,D),    Q#qos{'StartTimeSupported'=D}).
-define(not_SetStopTimeSupported(Q,D),     Q#qos{'StopTimeSupported'=D}).
-define(not_SetTimeout(Q,D),               Q#qos{'Timeout'=D}).
-define(not_SetOrderPolicy(Q,D),           Q#qos{'OrderPolicy'=D}).
-define(not_SetDiscardPolicy(Q,D),         Q#qos{'DiscardPolicy'=D}).
-define(not_SetMaximumBatchSize(Q,D),      Q#qos{'MaximumBatchSize'=D}).
-define(not_SetPacingInterval(Q,D),        Q#qos{'PacingInterval'=D}).
-define(not_SetMaxEventsPerConsumer(Q,D),  Q#qos{'MaxEventsPerConsumer'=D}).

%%--------------- StructuredEvent CREATORS -------------------
-define(not_CreateSE(StrD,StrT,StrE,PSeqV,PSeqF,AnyR),
#'CosNotification_StructuredEvent'{header = 
   #'CosNotification_EventHeader'{fixed_header = 
		  #'CosNotification_FixedEventHeader'{event_type =
				      #'CosNotification_EventType'{domain_name=StrD,
						   type_name=StrT},
				      event_name = StrE},
		  variable_header = PSeqV},
   filterable_data = PSeqF,
   remainder_of_body = AnyR}).
%% Can be used in guards.
-define(not_isConvertedAny(E), 
	(((E#'CosNotification_StructuredEvent'.header)
	  #'CosNotification_EventHeader'.fixed_header)
	 #'CosNotification_FixedEventHeader'.event_type)
	#'CosNotification_EventType'.type_name == "%ANY").
%% Can NOT be used in guards!!!!!
-define(not_isConvertedStructured(E), 
	any:get_typecode(E) == 'CosNotification_StructuredEvent':tc()).

%%--------------- StructuredEvent DESTRUCTORS ----------------
-define(not_DestroySE(E), ok).

%%--------------- StructuredEvent SELECTORS ------------------
-define(not_GetSEHeader(E), E#'StructuredEvent'.header).
-define(not_GetSEFixedHeader(E), E#'StructuredEvent'.header).

%%--------------- StructuredEvent MODIFIERS ------------------

%%-------- QoS support -----------------------------------------------
-define(not_SUPPORTED_QOS, 
[{?not_EventReliability,      'EventReliability'},
 {?not_ConnectionReliability, 'ConnectionReliability'},
 {?not_Priority,              'Priority'},
 {?not_StartTimeSupported,    'StartTimeSupported'},
 {?not_StopTimeSupported,     'StopTimeSupported'},
 {?not_Timeout,               'Timeout'},
 {?not_OrderPolicy,           'OrderPolicy'},
 {?not_DiscardPolicy,         'DiscardPolicy'},
 {?not_MaximumBatchSize,      'MaximumBatchSize'},
 {?not_PacingInterval,        'PacingInterval'},
 {?not_MaxEventsPerConsumer,  'MaxEventsPerConsumer'}]).

%%-------- ADMINPROPERTIESADMIN --------------------------------------

%% According to the OMG TC Document telecom/98-11-01, p 63 (section 2.5.7), the
%% default-value for these 3 admin properties is zero, which means that no limit
%% applies to that property.
-define(not_DEFAULT_ADMINPROPERTIES,
[#'CosNotification_Property'{name=?not_MaxQueueLength, 
			     value=any:create(orber_tc:long(), 0)},
 #'CosNotification_Property'{name=?not_MaxConsumers, 
			     value=any:create(orber_tc:long(), 0)},
 #'CosNotification_Property'{name=?not_MaxSuppliers, 
			     value=any:create(orber_tc:long(), 0)}]).

-define(not_SUPPORTED_ADMINPROPERTIES,
[{?not_MaxQueueLength, 'MaxQueueLength'},
 {?not_MaxConsumers, 'MaxConsumers'},
 {?not_MaxSuppliers, 'MaxSuppliers'}]).


%%-------- MISC --------------------------------------------------------

-define(not_DEFAULT_SETTINGS, [{pullInterval, 20},
			       {filterOp, 'OR_OP'},
			       {gcTime, 60},
			       {gcLimit, 50},
			       {timeService, undefined},
			       {typecheck, true},
			       {tty, false},
			       {logfile, false},
			       {server_options, []}]).
-define(not_CreateDBKey, term_to_binary({now(), node()})).

-define(DEBUG_LEVEL, 3).

-ifdef(debug).

-define(debug_print(F,A), io:format("[~p(~p)] "++F,[?MODULE, ?LINE]++A)).
-define(DBG(F,A), io:format("[~p(~p)] "++F,[?MODULE, ?LINE]++A)).
-define(not_TypeCheck(O,I), ok).
%-define(not_TypeCheck(O,M), 'CosNotification_Common':type_check(O,M)).

-else.

-define(debug_print(F,A), ok).
-define(DBG(F,A), ok).
-define(not_TypeCheck(O,I), ok).

-endif.    



-endif.
%%--------------- END OF MODULE ------------------------------
