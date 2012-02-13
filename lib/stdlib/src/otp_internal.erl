%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
-module(otp_internal).

-export([obsolete/3]).

%%----------------------------------------------------------------------

-type tag()     :: 'deprecated' | 'removed'. %% | 'experimental'.
-type mfas()    :: mfa() | {atom(), atom(), [byte()]}.
-type release() :: string().

-spec obsolete(atom(), atom(), byte()) ->
	'no' | {tag(), string()} | {tag(), mfas(), release()}.

obsolete(Module, Name, Arity) ->
    case obsolete_1(Module, Name, Arity) of
	{deprecated=Tag,{_,_,_}=Replacement} ->
	    {Tag,Replacement,"in a future release"};
	{_,String}=Ret when is_list(String) ->
	    Ret;
	{_,_,_}=Ret ->
	    Ret;
	no ->
	    no
    end.

obsolete_1(net, _, _) ->
    {deprecated, "module 'net' obsolete; use 'net_adm'"};

obsolete_1(erl_internal, builtins, 0) ->
    {deprecated, {erl_internal, bif, 2}};

obsolete_1(erl_eval, seq, 2) ->
    {deprecated, {erl_eval, exprs, 2}};
obsolete_1(erl_eval, seq, 3) ->
    {deprecated, {erl_eval, exprs, 3}};
obsolete_1(erl_eval, arg_list, 2) ->
    {deprecated, {erl_eval, expr_list, 2}};
obsolete_1(erl_eval, arg_list, 3) ->
    {deprecated, {erl_eval, expr_list, 3}};

obsolete_1(erlang, hash, 2) ->
    {deprecated, {erlang, phash2, 2}};

obsolete_1(calendar, local_time_to_universal_time, 1) ->
    {deprecated, {calendar, local_time_to_universal_time_dst, 1}};

obsolete_1(rpc, safe_multi_server_call, A) when A =:= 2; A =:= 3 ->
    {deprecated, {rpc, multi_server_call, A}};


%% *** SNMP ***

obsolete_1(snmp, N, A) ->
    case is_snmp_agent_function(N, A) of
	false ->
	    no;
	true ->
	    {deprecated,"Deprecated; use snmpa:"++atom_to_list(N)++"/"++
	     integer_to_list(A)++" instead"}
    end;

obsolete_1(snmpm, agent_info, 3) ->
    {deprecated, {snmpm, agent_info, 2}, "R16B"};
obsolete_1(snmpm, update_agent_info, 5) ->
    {deprecated, {snmpm, update_agent_info, 4}, "R16B"};
obsolete_1(snmpm, g, 3) ->
    {deprecated, {snmpm, sync_get, 3}, "R16B"};
obsolete_1(snmpm, g, 4) ->
    {deprecated, {snmpm, sync_get, [3,4]}, "R16B"};
obsolete_1(snmpm, g, 5) ->
    {deprecated, {snmpm, sync_get, [4,5]}, "R16B"};
obsolete_1(snmpm, g, 6) ->
    {deprecated, {snmpm, sync_get, [5,6]}, "R16B"};
obsolete_1(snmpm, g, 7) ->
    {deprecated, {snmpm, sync_get, 6}, "R16B"};
obsolete_1(snmpm, ag, 3) ->
    {deprecated, {snmpm, async_get, 3}, "R16B"};
obsolete_1(snmpm, ag, 4) ->
    {deprecated, {snmpm, async_get, [3,4]}, "R16B"};
obsolete_1(snmpm, ag, 5) ->
    {deprecated, {snmpm, async_get, [4,5]}, "R16B"};
obsolete_1(snmpm, ag, 6) ->
    {deprecated, {snmpm, async_get, [5,6]}, "R16B"};
obsolete_1(snmpm, ag, 7) ->
    {deprecated, {snmpm, async_get, 6}, "R16B"};
obsolete_1(snmpm, gn, 3) ->
    {deprecated, {snmpm, sync_get_next, 3}, "R16B"};
obsolete_1(snmpm, gn, 4) ->
    {deprecated, {snmpm, sync_get_next, [3,4]}, "R16B"};
obsolete_1(snmpm, gn, 5) ->
    {deprecated, {snmpm, sync_get_next, [4,5]}, "R16B"};
obsolete_1(snmpm, gn, 6) ->
    {deprecated, {snmpm, sync_get_next, [5,6]}, "R16B"};
obsolete_1(snmpm, gn, 7) ->
    {deprecated, {snmpm, sync_get_next, 6}, "R16B"};
obsolete_1(snmpm, agn, 3) ->
    {deprecated, {snmpm, async_get_next, 3}, "R16B"};
obsolete_1(snmpm, agn, 4) ->
    {deprecated, {snmpm, async_get_next, [3,4]}, "R16B"};
obsolete_1(snmpm, agn, 5) ->
    {deprecated, {snmpm, async_get_next, [4,5]}, "R16B"};
obsolete_1(snmpm, agn, 6) ->
    {deprecated, {snmpm, async_get_next, [5,6]}, "R16B"};
obsolete_1(snmpm, agn, 7) ->
    {deprecated, {snmpm, async_get_next, 6}, "R16B"};
obsolete_1(snmpm, s, 3) ->
    {deprecated, {snmpm, sync_set, 3}, "R16B"};
obsolete_1(snmpm, s, 4) ->
    {deprecated, {snmpm, sync_set, [3,4]}, "R16B"};
obsolete_1(snmpm, s, 5) ->
    {deprecated, {snmpm, sync_set, [4,5]}, "R16B"};
obsolete_1(snmpm, s, 6) ->
    {deprecated, {snmpm, sync_set, [5,6]}, "R16B"};
obsolete_1(snmpm, s, 7) ->
    {deprecated, {snmpm, sync_set, 6}, "R16B"};
obsolete_1(snmpm, as, 3) ->
    {deprecated, {snmpm, async_set, 3}, "R16B"};
obsolete_1(snmpm, as, 4) ->
    {deprecated, {snmpm, async_set, [3,4]}, "R16B"};
obsolete_1(snmpm, as, 5) ->
    {deprecated, {snmpm, async_set, [4,5]}, "R16B"};
obsolete_1(snmpm, as, 6) ->
    {deprecated, {snmpm, async_set, [5,6]}, "R16B"};
obsolete_1(snmpm, as, 7) ->
    {deprecated, {snmpm, async_set, 6}, "R16B"};
obsolete_1(snmpm, gb, 5) ->
    {deprecated, {snmpm, sync_get_bulk, 5}, "R16B"};
obsolete_1(snmpm, gb, 6) ->
    {deprecated, {snmpm, sync_get_bulk, [5,6]}, "R16B"};
obsolete_1(snmpm, gb, 7) ->
    {deprecated, {snmpm, sync_get_bulk, [6,7]}, "R16B"};
obsolete_1(snmpm, gb, 8) ->
    {deprecated, {snmpm, sync_get_bulk, [7,8]}, "R16B"};
obsolete_1(snmpm, gb, 9) ->
    {deprecated, {snmpm, sync_get_bulk, 8}, "R16B"};
obsolete_1(snmpm, agb, 5) ->
    {deprecated, {snmpm, async_get_bulk, 5}, "R16B"};
obsolete_1(snmpm, agb, 6) ->
    {deprecated, {snmpm, async_get_bulk, [5,6]}, "R16B"};
obsolete_1(snmpm, agb, 7) ->
    {deprecated, {snmpm, async_get_bulk, [6,7]}, "R16B"};
obsolete_1(snmpm, agb, 8) ->
    {deprecated, {snmpm, async_get_bulk, [7,8]}, "R16B"};
obsolete_1(snmpm, agb, 9) ->
    {deprecated, {snmpm, async_get_bulk, 8}, "R16B"};


%% *** MEGACO ***

obsolete_1(megaco, format_versions, 1) ->
    {deprecated, "Deprecated; use megaco:print_version_info/0,1 instead"};


%% *** OS-MON-MIB ***

obsolete_1(os_mon_mib, init, 1) ->
    {deprecated, {os_mon_mib, load, 1}};
obsolete_1(os_mon_mib, stop, 1) ->
    {deprecated, {os_mon_mib, unload, 1}};

obsolete_1(auth, is_auth, 1) ->
    {deprecated, {net_adm, ping, 1}};
obsolete_1(auth, cookie, 0) ->
    {deprecated, {erlang, get_cookie, 0}};
obsolete_1(auth, cookie, 1) ->
    {deprecated, {erlang, set_cookie, 2}};
obsolete_1(auth, node_cookie, 1) ->
    {deprecated, "Deprecated; use erlang:set_cookie/2 and net_adm:ping/1 instead"};
obsolete_1(auth, node_cookie, 2) ->
    {deprecated, "Deprecated; use erlang:set_cookie/2 and net_adm:ping/1 instead"};

obsolete_1(erlang, is_constant, 1) ->
    {removed, "Removed in R13B"};

%% Added in R12B-0.
obsolete_1(ssl, port, 1) ->
    {removed, {ssl, sockname, 1}, "R13B"};
obsolete_1(ssl, accept, A) when A =:= 1; A =:= 2 ->
    {removed, "deprecated; use ssl:transport_accept/1,2 and ssl:ssl_accept/1,2"};
obsolete_1(erlang, fault, 1) ->
    {removed, {erlang,error,1}, "R13B"};
obsolete_1(erlang, fault, 2) ->
    {removed, {erlang,error,2}, "R13B"};

%% Added in R12B-2.
obsolete_1(file, rawopen, 2) ->
    {removed, "deprecated (will be removed in R13B); use file:open/2 with the raw option"};

obsolete_1(http, request, 1) 	      -> {removed,{httpc,request,1},"R15B"};
obsolete_1(http, request, 2) 	      -> {removed,{httpc,request,2},"R15B"};
obsolete_1(http, request, 4) 	      -> {removed,{httpc,request,4},"R15B"};
obsolete_1(http, request, 5) 	      -> {removed,{httpc,request,5},"R15B"};
obsolete_1(http, cancel_request, 1)   -> {removed,{httpc,cancel_request,1},"R15B"};
obsolete_1(http, cancel_request, 2)   -> {removed,{httpc,cancel_request,2},"R15B"};
obsolete_1(http, set_option, 2)       -> {removed,{httpc,set_option,2},"R15B"};
obsolete_1(http, set_option, 3)       -> {removed,{httpc,set_option,3},"R15B"};
obsolete_1(http, set_options, 1)      -> {removed,{httpc,set_options,1},"R15B"};
obsolete_1(http, set_options, 2)      -> {removed,{httpc,set_options,2},"R15B"};
obsolete_1(http, verify_cookies, 2)   -> {removed,{httpc,store_cookies,2},"R15B"};
obsolete_1(http, verify_cookies, 3)   -> {removed,{httpc,store_cookies,3},"R15B"};
obsolete_1(http, cookie_header, 1)    -> {removed,{httpc,cookie_header,1},"R15B"};
obsolete_1(http, cookie_header, 2)    -> {removed,{httpc,cookie_header,2},"R15B"};
obsolete_1(http, stream_next, 1)      -> {removed,{httpc,stream_next,1},"R15B"};
obsolete_1(http, default_profile, 0)  -> {removed,{httpc,default_profile,0},"R15B"};

obsolete_1(httpd, start, 0) 	      -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(httpd, start, 1) 	      -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(httpd, start_link, 0)      -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(httpd, start_link, 1)      -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(httpd, start_child, 0)     -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(httpd, start_child, 1)     -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(httpd, stop, 0) 	      -> {removed,{inets,stop,2},"R14B"};
obsolete_1(httpd, stop, 1)            -> {removed,{inets,stop,2},"R14B"};
obsolete_1(httpd, stop, 2)            -> {removed,{inets,stop,2},"R14B"};
obsolete_1(httpd, stop_child, 0)      -> {removed,{inets,stop,2},"R14B"};
obsolete_1(httpd, stop_child, 1)      -> {removed,{inets,stop,2},"R14B"};
obsolete_1(httpd, stop_child, 2)      -> {removed,{inets,stop,2},"R14B"};
obsolete_1(httpd, restart, 0) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, restart, 1) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, restart, 2) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, block, 0) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, block, 1) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, block, 2) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, block, 3) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, block, 4)	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, unblock, 0) 	      -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, unblock, 1)         -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd, unblock, 2)         -> {removed,{httpd,reload_config,2},"R14B"};
obsolete_1(httpd_util, key1search, 2) -> {removed,{proplists,get_value,2},"R13B"};
obsolete_1(httpd_util, key1search, 3) -> {removed,{proplists,get_value,3},"R13B"};
obsolete_1(ftp, open, 3)              -> {removed,{inets,start,[2,3]},"R14B"};
obsolete_1(ftp, force_active, 1)      -> {removed,{inets,start,[2,3]},"R14B"};

%% Added in R12B-4.
obsolete_1(ssh_cm, connect, A) when 1 =< A, A =< 3 ->
    {removed,{ssh,connect,A},"R14B"};
obsolete_1(ssh_cm, listen, A) when 2 =< A, A =< 4 ->
    {removed,{ssh,daemon,A},"R14B"};
obsolete_1(ssh_cm, stop_listener, 1) ->
    {removed,{ssh,stop_listener,[1,2]},"R14B"};
obsolete_1(ssh_cm, session_open, A) when A =:= 2; A =:= 4 ->
    {removed,{ssh_connection,session_channel,A},"R14B"};
obsolete_1(ssh_cm, direct_tcpip, A) when A =:= 6; A =:= 8 ->
    {removed,{ssh_connection,direct_tcpip,A}};
obsolete_1(ssh_cm, tcpip_forward, 3) ->
    {removed,{ssh_connection,tcpip_forward,3},"R14B"};
obsolete_1(ssh_cm, cancel_tcpip_forward, 3) ->
    {removed,{ssh_connection,cancel_tcpip_forward,3},"R14B"};
obsolete_1(ssh_cm, open_pty, A) when A =:= 3; A =:= 7; A =:= 9 ->
    {removed,{ssh_connection,open_pty,A},"R14"};
obsolete_1(ssh_cm, setenv, 5) ->
    {removed,{ssh_connection,setenv,5},"R14B"};
obsolete_1(ssh_cm, shell, 2) ->
    {removed,{ssh_connection,shell,2},"R14B"};
obsolete_1(ssh_cm, exec, 4) ->
    {removed,{ssh_connection,exec,4},"R14B"};
obsolete_1(ssh_cm, subsystem, 4) ->
    {removed,{ssh_connection,subsystem,4},"R14B"};
obsolete_1(ssh_cm, winch, A) when A =:= 4; A =:= 6 ->
    {removed,{ssh_connection,window_change,A},"R14B"};
obsolete_1(ssh_cm, signal, 3) ->
    {removed,{ssh_connection,signal,3},"R14B"};
obsolete_1(ssh_cm, attach, A) when A =:= 2; A =:= 3 ->
    {removed,{ssh,attach,A}};
obsolete_1(ssh_cm, detach, 2) ->
    {removed,"no longer useful; will be removed in R14B"};
obsolete_1(ssh_cm, set_user_ack, 4) ->
    {removed,"no longer useful; will be removed in R14B"};
obsolete_1(ssh_cm, adjust_window, 3) ->
    {removed,{ssh_connection,adjust_window,3},"R14B"};
obsolete_1(ssh_cm, close, 2) ->
    {removed,{ssh_connection,close,2},"R14B"};
obsolete_1(ssh_cm, stop, 1) ->
    {removed,{ssh,close,1},"R14B"};
obsolete_1(ssh_cm, send_eof, 2) ->
    {removed,{ssh_connection,send_eof,2},"R14B"};
obsolete_1(ssh_cm, send, A) when A =:= 3; A =:= 4 ->
    {removed,{ssh_connection,send,A},"R14B"};
obsolete_1(ssh_cm, send_ack, A) when 3 =< A, A =< 5 ->
    {removed,{ssh_connection,send,[3,4]},"R14B"};
obsolete_1(ssh_ssh, connect, A) when 1 =< A, A =< 3 ->
    {removed,{ssh,shell,A},"R14B"};
obsolete_1(ssh_sshd, listen, A) when 0 =< A, A =< 3 ->
    {removed,{ssh,daemon,[1,2,3]},"R14"};
obsolete_1(ssh_sshd, stop, 1) ->
    {removed,{ssh,stop_listener,1}};

%% Added in R13A.
obsolete_1(regexp, _, _) ->
    {removed, "removed in R15; use the re module instead"};

obsolete_1(lists, flat_length, 1) ->
    {removed,{lists,flatlength,1},"R14"};

obsolete_1(ssh_sftp, connect, A) when 1 =< A, A =< 3 ->
    {removed,{ssh_sftp,start_channel,A},"R14B"};
obsolete_1(ssh_sftp, stop, 1) ->
    {removed,{ssh_sftp,stop_channel,1},"R14B"};

%% Added in R13B01.
obsolete_1(ssl_pkix, decode_cert_file, A) when A =:= 1; A =:= 2 ->
    {removed,"removed in R14A; use public_key:pem_to_der/1 and public_key:pkix_decode_cert/2 instead"};
obsolete_1(ssl_pkix, decode_cert, A) when A =:= 1; A =:= 2 ->
    {removed,{public_key,pkix_decode_cert,2},"R14A"};

%% Added in R13B04.
obsolete_1(erlang, concat_binary, 1) ->
    {removed,{erlang,list_to_binary,1},"R15B"};

%% Added in R14A.
obsolete_1(ssl, peercert, 2) ->
    {deprecated,"deprecated (will be removed in R15A); use ssl:peercert/1 and public_key:pkix_decode_cert/2 instead"};

%% Added in R14B.
obsolete_1(public_key, pem_to_der, 1) ->
    {deprecated,"deprecated (will be removed in R15A); use file:read_file/1 and public_key:pem_decode/1"};
obsolete_1(public_key, decode_private_key, A) when A =:= 1; A =:= 2 ->
    {deprecated,{public_key,pem_entry_decode,1},"R15A"};

%% Added in R14B03.
obsolete_1(docb_gen, _, _) ->
    {removed,"the DocBuilder application was removed in R15B"};
obsolete_1(docb_transform, _, _) ->
    {removed,"the DocBuilder application was removed in R15B"};
obsolete_1(docb_xml_check, _, _) ->
    {removed,"the DocBuilder application was removed in R15B"};

%% Added in R15B
obsolete_1(asn1rt, F, _) when F == load_driver; F == unload_driver ->
    {deprecated,"deprecated (will be removed in R16A); has no effect as drivers are no longer used."};
obsolete_1(ssl, pid, 1) ->
    {deprecated,"deprecated (will be removed in R17); is no longer needed"};
obsolete_1(inviso, _, _) ->
    {deprecated,"the inviso application has been deprecated and will be removed in R16"};

%% Added in R15B01.
obsolete_1(gs, _, _) ->
    {deprecated,"the gs application has been deprecated and will be removed in R16; use the wx application instead"};
obsolete_1(ssh, sign_data, 2) ->
    {deprecated,"deprecated (will be removed in R16A); use public_key:pem_decode/1, public_key:pem_entry_decode/1 "
     "and public_key:sign/3 instead"};
obsolete_1(ssh, verify_data, 3) ->
    {deprecated,"deprecated (will be removed in R16A); use public_key:ssh_decode/1, and public_key:verify/4 instead"};
obsolete_1(_, _, _) ->
    no.

-spec is_snmp_agent_function(atom(), byte()) -> boolean().

is_snmp_agent_function(c,                     1) -> true;
is_snmp_agent_function(c,                     2) -> true;
is_snmp_agent_function(compile,               3) -> true;
is_snmp_agent_function(is_consistent,         1) -> true;
is_snmp_agent_function(mib_to_hrl,            1) -> true;
is_snmp_agent_function(change_log_size,       1) -> true;
is_snmp_agent_function(log_to_txt,            2) -> true;
is_snmp_agent_function(log_to_txt,            3) -> true;
is_snmp_agent_function(log_to_txt,            4) -> true;
is_snmp_agent_function(current_request_id,    0) -> true;
is_snmp_agent_function(current_community,     0) -> true;
is_snmp_agent_function(current_address,       0) -> true;
is_snmp_agent_function(current_context,       0) -> true;
is_snmp_agent_function(current_net_if_data,   0) -> true;
is_snmp_agent_function(get_symbolic_store_db, 0) -> true;
is_snmp_agent_function(name_to_oid,           1) -> true;
is_snmp_agent_function(name_to_oid,           2) -> true;
is_snmp_agent_function(oid_to_name,           1) -> true;
is_snmp_agent_function(oid_to_name,           2) -> true;
is_snmp_agent_function(int_to_enum,           2) -> true;
is_snmp_agent_function(int_to_enum,           3) -> true;
is_snmp_agent_function(enum_to_int,           2) -> true;
is_snmp_agent_function(enum_to_int,           3) -> true;
is_snmp_agent_function(get,                   2) -> true;
is_snmp_agent_function(info,                  1) -> true;
is_snmp_agent_function(load_mibs,             2) -> true;
is_snmp_agent_function(unload_mibs,           2) -> true;
is_snmp_agent_function(dump_mibs,             0) -> true;
is_snmp_agent_function(dump_mibs,             1) -> true;
is_snmp_agent_function(register_subagent,     3) -> true;
is_snmp_agent_function(unregister_subagent,   2) -> true;
is_snmp_agent_function(send_notification,     3) -> true;
is_snmp_agent_function(send_notification,     4) -> true;
is_snmp_agent_function(send_notification,     5) -> true;
is_snmp_agent_function(send_notification,     6) -> true;
is_snmp_agent_function(send_trap,             3) -> true;
is_snmp_agent_function(send_trap,             4) -> true;
is_snmp_agent_function(add_agent_caps,        2) -> true;
is_snmp_agent_function(del_agent_caps,        1) -> true;
is_snmp_agent_function(get_agent_caps,        0) -> true;
is_snmp_agent_function(_,		      _) -> false.
