%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Define common data structures and error codes
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Receive handle
%%
%% The receive handle contains enough information in order
%% to be able to decode a message and send a reply to the
%% originator.
%%----------------------------------------------------------------------

-record(megaco_receive_handle,
        {
          local_mid,
          encoding_mod,
          encoding_config,
          send_mod,
	  protocol_version = dynamic % dynamic | integer()
         }).

%%----------------------------------------------------------------------
%% Connection handle
%%
%% The connecion handle provides a locally unique identity of a
%% connection. It is generated when a connection is established.
%%----------------------------------------------------------------------

-record(megaco_conn_handle,
        {
          local_mid,
          remote_mid
         }).

%%----------------------------------------------------------------------
%% Incremental timer
%%
%% The timer sleeps in WaitFor milli seconds and when it
%% times out, a new time out value is computed:
%%
%%   WaitFor2 = WaitFor * Factor + Incr
%%   
%% And the timer starts all over again with the new WaitFor value.
%% The procedure is repeated at at most MaxRetries.
%% 
%% There is a special case for this timer. When the max_retries has
%% the value infinity_restartable it means that the timer is
%% restartable as long as some external event occurs (e.g. receipt of
%% a pending message for instance). But the timer will never be
%% restarted "by itself". I.e. when the timer expires (whatever the
%% timeout time), so does the timer. Whever the timer is restarted,
%% the timeout time will be calculated in the usual way!
%% 
%%----------------------------------------------------------------------

-record(megaco_incr_timer,
        {wait_for    = timer:seconds(7),% Initial timeout (milli seconds)
         factor      = 2,               % Factor to multiply with at timeout
         incr        = 0,               % Milli seconds to add at timeout
         max_retries = infinity}).      % infinity | infinity_restartable | Integer

%%----------------------------------------------------------------------
%% The internal representation of a termination id
%%----------------------------------------------------------------------
%%
%% id()                 -> [level()]
%% level()              -> [char()]
%% char()               -> wildcard() | $0 | $1 | text_only_char()
%% text_only_char()     -> $2..$9 | $a..$z | $_ | $. | $@
%% wildcard()           -> all() | choose()
%% all()                -> $*
%% choose()             -> $$
%% contains_wildcards() -> true | false
%%
%% The id field contains all info about the termination id, while the
%% presense of the contains_wildcards field is just as a matter of
%% convenience in order to make it simple to determine whether a
%% simple id lookup will suffice or if some more complicated matching
%% algorithm is needed.
%% 
%% Some text encoding examples:
%%
%%   "ROOT"     -> {megaco_term_id, false, [[$r,$o,$o,$t]]}
%%   "*"        -> {megaco_term_id, true,  [[$*]]}
%%   "$"        -> {megaco_term_id, true,  [[$$]]}
%%   "R1/101/1" -> {megaco_term_id, false, [[$r, $1], [$1, $0,  $1], [$1]]}
%%   "R1/1*1/*" -> {megaco_term_id, true,  [[$r, $1], [$1, all, $1]], [$*]}
%%   "R1/1*"    -> {megaco_term_id, true,  [[$r, $1], [$1, $*]]}
%%   "R1/1*/"   -> {megaco_term_id, true,  [[$r, $1], [$1, $*], []]}
%%   
%% Given the terminations "R1/10", "R1/101/0" and "R1/101/1" the above 
%% termination identifiers would be resolved to the following ones:
%% 
%%   "ROOT"     -> "ROOT"
%%   "*"        -> "R1/10"    and "R1/101/0" and "R1/101/1"
%%   "$"        -> "R1/10"    or  "R1/101/0" or  "R1/101/1"
%%   "R1/101/1" -> "R1/101/1"
%%   "R1/1*1/*" -> "R1/101/1" and "R1/101/0"
%%   "R1/1*"    -> "R1/10"    and "R1/101/0" and "R1/101/1"
%%   "R1/1*/"   -> "R1/10"
%%
%% In the binary encoding it is possible to express whether the last
%% wildcard pertains to a certain level, in the hierarchical naming
%% scheme (in the internal form this is expressed as an empty list as
%% last level, see the internal form for "R1/1*/" above) or if the
%% match also should include all lower levels recursively (see the
%% internal form for "R1/1*" above).
%%
%% Observe that a termination id with a trailing slash (such as
%% "R1/1*/") violates the text encoding protocol. But we allow it
%% anyway since the corresponding internal form is needed for the
%% binary encoding. It is also nice to be able to pretty print any
%% binary termination id as text.
%%
%% A fully specified binary termination id:
%%
%%   #'TerminationID'{wildcard = [],
%%                   id        = [2#00000001, 02#0011110, 2#00000000]}
%%
%% is internally represented as:
%%   
%%   #megaco_term_id{contains_wildcards = false,
%%                   id                 = [[$0, $0, $0, $1, $1, $1, $1, $0],
%%                                         [$0, $1, $0, $1, $0, $1, $0, $1],
%%                                         [$0, $0, $0, $0, $0, $0, $0, $0]]}
%%                            
%% Addressing all names with prefix 00000001 00011110 is done as follows:
%% 
%%   #'TerminationID'{wildcard = [2#10000111],
%%                   id        = [2#00000001, 2#00011110, 2#00000000]}
%%   
%% is internally represented as:
%%   
%%   #megaco_term_id{contains_wildcards = true,
%%                   id                 = [[$0, $0, $0, $0, $0, $0, $0, $1],
%%                                         [$0, $0, $0, $1, $1, $1, $1, $0],
%%                                         [?megaco_all]}
%% 
%% Indicating to the receiver that is must choose a name with 00011110 as
%% the second octet is done as follows:
%% 
%%   #'TerminationID'{wildcard = [2#00010111, 2#00000111],
%%                   id        = [2#00000000, 2#00011110, 2#00000000]}
%%   
%% is internally represented as:
%%   
%%   #megaco_term_id{contains_wildcards = true,
%%                   id                 = [[?megaco_choose],
%%                                         [$0, $0, $0, $1, $1, $1, $1, $0],
%%                                         [?megaco_choose]]}
%%                           
%% Finally, a choose-wildcarded name with the highest level of the name
%% equal to 00000001 is specified as follows:
%%  
%%   #'TerminationID'{wildcard = [2#01001111],
%%                    id       = [2#00000001, 2#00000000, 2#00000000]}
%%   
%% is internally represented as:
%%
%%   #megaco_term_id{contains_wildcards = true,
%%                   id                 = [[$0, $0, $0, $0, $0, $0, $0, $1],
%%                                         [?megaco_choose],
%%                                         [?megaco_choose]]}
%%----------------------------------------------------------------------

-record(megaco_term_id, {contains_wildcards = false, id}).

-define(megaco_root_termination_id, #megaco_term_id{id = [[$r,$o,$o,$t]]}).

-define(megaco_all,    $*).
-define(megaco_choose, $$).

%%----------------------------------------------------------------------
%% Predefined context identifiers
%%----------------------------------------------------------------------

-define(megaco_null_context_id,   0).           % 0
-define(megaco_choose_context_id, 16#FFFFFFFE). % 4294967294
-define(megaco_all_context_id,    16#FFFFFFFF). % 4294967295

%%----------------------------------------------------------------------
%% Predefined request identifiers
%%----------------------------------------------------------------------

-define(megaco_all_request_id,    16#FFFFFFFF). % 4294967295

%%----------------------------------------------------------------------
%% Command error codes
%%----------------------------------------------------------------------

-define(megaco_bad_request,                                 400).
-define(megaco_protocol_error,                              401).
-define(megaco_unauthorized,                                402).
-define(megaco_syntax_error_in_transaction,                 403).
-define(megaco_version_not_supported,                       406).
-define(megaco_incorrect_identifier,                        410).
-define(megaco_unknown_context_id,                          411).
-define(megaco_no_context_id_available,                     412).
-define(megaco_num_of_trans_exceeds_max,                    413).    % v3
-define(megaco_unknown_action_or_illegal_combination_of_actions, 421).
-define(megaco_syntax_error_in_action,                      422).
-define(megaco_unknown_termination_id,                      430).
-define(megaco_no_termination_id_matched_a_wildcard,        431).
-define(megaco_out_of_termination_ids_or_no_termination_id_available, 432).
-define(megaco_termination_id_already_in_context,           433).
-define(megaco_max_number_of_terminations_in_context_exceeded, 434). % v2
-define(megaco_terminations_id_not_in_specified_context,       435). % v2

-define(megaco_unsupported_or_unknown_package,              440).
-define(megaco_missing_remote_or_local_descriptor,          441).    % v2
-define(megaco_missing_remote_descriptor,
	?megaco_missing_remote_or_local_descriptor).                 
-define(megaco_missing_local_descriptor,
	?megaco_missing_remote_or_local_descriptor).                 
-define(megaco_syntax_error_in_command,                     442).
-define(megaco_unsupported_or_unknown_command,              443).
-define(megaco_unsupported_or_unknown_descriptor,           444).
-define(megaco_unsupported_or_unknown_property,             445).
-define(megaco_unsupported_or_unknown_parameter,            446).
-define(megaco_descriptor_not_legal_in_this_command,        447).
-define(megaco_descriptor_appears_twice_in_this_command,    448).
-define(megaco_unsup_or_unknown_param_or_prop_value,        449).    % v3
-define(megaco_unsupported_parameter_value, 
        ?megaco_unsup_or_unknown_param_or_prop_value).
-define(megaco_unsupported_proprty_value, 
        ?megaco_unsup_or_unknown_param_or_prop_value).
-define(megaco_unknown_parameter_value, 
        ?megaco_unsup_or_unknown_param_or_prop_value).
-define(megaco_unknown_proprty_value, 
        ?megaco_unsup_or_unknown_param_or_prop_value).
-define(megaco_no_such_property_in_this_package,            450).
-define(megaco_no_such_event_in_this_package,               451).
-define(megaco_no_such_signal_in_this_package,              452).
-define(megaco_no_such_statistic_in_this_package,           453).
-define(megaco_no_such_parameter_in_this_package,           454).
-define(megaco_property_illegal_in_this_descriptor,         455).    % v2
-define(megaco_parameter_illegal_in_this_descriptor, 
	?megaco_property_illegal_in_this_descriptor).               
-define(megaco_property_appears_twice_in_this_descriptor,   456).    % v2
-define(megaco_parameter_or_property_appears_twice_in_this_descriptor, 
	?megaco_property_appears_twice_in_this_descriptor). 
-define(megaco_missing_parameter_in_signal_or_event,        457).    % v2
-define(megaco_unexpected_event_or_request_id,              458).    % v3
-define(megaco_unexpected_event, 
        ?megaco_unexpected_event_or_request_id).
-define(megaco_unexpected_request_id, 
        ?megaco_unexpected_event_or_request_id).
-define(megaco_segments_not_received,                       459).    % v3
-define(megaco_unable_to_set_statistic_on_stream,           460).    % v3
-define(megaco_implied_add_for_multiplex_failure,           471).
-define(megaco_internal_gateway_error,                      500).
-define(megaco_not_implemented,                             501).
-define(megaco_not_ready,                                   502).         
-define(megaco_service_unavailable,                         503).
-define(megaco_command_received_from_unauthorized_entity,   504).
-define(megaco_transaction_req_received_before_servicechange_reply, 505). % v2
-define(megaco_command_received_before_restart_response, 
	?megaco_transaction_req_received_before_servicechange_reply).
-define(megaco_number_of_transactionpending_exceeded,       506).    % v2
-define(megaco_insufficient_resources,                      510).
-define(megaco_mg_unequipped_to_detect_requested_event,     512).
-define(megaco_mg_unequipped_to_generate_requested_signals, 513).
-define(megaco_mg_cannot_send_the_specified_announcement,   514).
-define(megaco_unsupported_media_type,                      515).
-define(megaco_unsupported_or_invalid_mode,                 517).
-define(megaco_event_buffer_full,                           518).
-define(megaco_out_of_space_to_store_digit_map,             519).
-define(megaco_mg_does_not_have_a_digit_map,                520).
-define(megaco_termination_is_service_changing,             521).
-define(megaco_unsupported_func_req_in_topology_triple,     522). % v3
-define(megaco_insufficient_bandwidth,                      526).
-define(megaco_internal_hardware_failure,                   529).
-define(megaco_temporary_network_failure,                   530).
-define(megaco_permanent_network_failure,                   531).
-define(megaco_audit_prop_stat_event_or_sig_does_not_exist, 532). % v2
-define(megaco_response_exceeds_maximum_transport_pdu_size, 533). % v2
-define(megaco_illegal_write_of_read_only_property,         534). % v2
-define(megaco_unexpected_initial_hook_state,               540). % v2
-define(megaco_command_not_allowed_on_this_termination,     542). % v3
-define(megaco_does_not_exist,                              581).


%%----------------------------------------------------------------------
%% Service change reasons
%%----------------------------------------------------------------------

-define(megaco_service_restored,                 "900").        
-define(megaco_cold_boot,                        "901").
-define(megaco_warm_boot,                        "902").
-define(megaco_mgc_directed_change,              "903").
-define(megaco_termination_malfunctioning,       "904").
-define(megaco_termination_taken_out_of_service, "905").
-define(megaco_loss_of_lower_layer_connectivity, "906").
-define(megaco_transmission_failure,             "907").
-define(megaco_mg_impending_failure,             "908").
-define(megaco_mgc_impending_failure,            "909").
-define(megaco_media_capability_failure,         "910").
-define(megaco_modem_capability_failure,         "911").
-define(megaco_mux_capability_failure,           "912").
-define(megaco_signal_capability_failure,        "913").
-define(megaco_event_capability_failure,         "914").
-define(megaco_state_loss,                       "915").
-define(megaco_packages_change,                  "916"). % v2
-define(megaco_capabilities_change,              "917"). % v2
-define(megaco_cancel_gracefull,                 "918"). % v3
-define(megaco_warm_failover,                    "919"). % v3
-define(megaco_cold_failover,                    "920"). % v3


%%----------------------------------------------------------------------
%% MGC listen ports for both TCP/IP and UDP/IP
%% The port numbers are standardized by IANA (ww.iana.org).
%%----------------------------------------------------------------------

-define(megaco_ip_port_text,   2944).
-define(megaco_ip_port_binary, 2945).
