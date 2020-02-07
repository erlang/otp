%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
-module(otp_internal).

-export([obsolete/3, obsolete_type/3]).

%%----------------------------------------------------------------------

-dialyzer({no_match, obsolete/3}).

-type tag()     :: 'deprecated' | 'removed'. %% | 'experimental'.
-type mfas()    :: mfa() | {atom(), atom(), [byte()]}.
-type release() :: string().

-spec obsolete(module(), atom(), arity()) ->
	'no' | {tag(), string()} | {tag(), mfas(), release()}.

obsolete(Module, Name, Arity) ->
    case obsolete_1(Module, Name, Arity) of
	{deprecated=Tag,{_,_,_}=Replacement} ->
	    {Tag,Replacement,"a future release"};
	{_,String}=Ret when is_list(String) ->
	    Ret;
	{_,_,_}=Ret ->
	    Ret;
	no ->
	    no
    end.

%% *** kernel added in OTP 23 ***

obsolete_1(pg2, F, A) ->
    IsFun = case {F, A} of
                {create, 1} -> true;
                {delete, 1} -> true;
                {join, 2} -> true;
                {leave, 2} -> true;
                {get_members, 1} -> true;
                {get_local_members, 1} -> true;
                {get_closest_pid, 1} -> true;
                {which_groups, 1} -> true;
                {start, 0} -> true;
                {start_link, 0} -> true;
                {init, 1} -> true;
                {handle_call, 3} -> true;
                {handle_cast, 2} -> true;
                {handle_info, 2} -> true;
                {terminate, 2} -> true;
                _ -> false
            end,
    
    {deprecated,
     if IsFun == true -> "";
        true ->
             "(unknown function) however, "
     end
     ++ "the module pg2 is deprecated and scheduled "
     ++ "for removal in OTP 24; use pg instead"};

%% ***

obsolete_1(net, call, 4) ->
    {deprecated, {rpc, call, 4}};
obsolete_1(net, cast, 4) ->
    {deprecated, {rpc, cast, 4}};
obsolete_1(net, broadcast, 3) ->
    {deprecated, {rpc, eval_everywhere, 3}};
obsolete_1(net, ping, 1) ->
    {deprecated, {net_adm, ping, 1}};
obsolete_1(net, sleep, 1) ->
    {deprecated, "Use 'receive after T -> ok end' instead"};
obsolete_1(net, relay, 1) ->
    {deprecated, {slave, relay, 1}};


obsolete_1(erlang, now, 0) ->
    {deprecated,
     "Deprecated BIF. See the \"Time and Time Correction in Erlang\" "
     "chapter of the ERTS User's Guide for more information."};

obsolete_1(calendar, local_time_to_universal_time, 1) ->
    {deprecated, {calendar, local_time_to_universal_time_dst, 1}};

%% *** inets added in OTP 23 ***

obsolete_1(http_uri, parse, 1) ->
    {deprecated, "deprecated; use uri_string functions instead"};

obsolete_1(http_uri, parse, 2) ->
    {deprecated,  "deprecated; use uri_string functions instead"};

obsolete_1(http_uri, encode, 1) ->
    {deprecated,  "deprecated; use uri_string functions instead"};

obsolete_1(http_uri, decode, 1) ->
    {deprecated,  "deprecated; use uri_string functions instead"};

obsolete_1(http_uri, scheme_defaults, 0) ->
    {deprecated,  "deprecated; use uri_string functions instead"};

obsolete_1(httpd, parse_query, 1) ->
    {deprecated, {uri_string, dissect_query, 1}};

%% *** STDLIB added in OTP 22 ***

obsolete_1(sys, get_debug, 3) ->
    {deprecated,
     "Deprecated function. "
     "Incorrectly documented and in fact only for internal use. "
     "Can often be replaced with sys:get_log/1."};

%% *** STDLIB added in OTP 20 ***

obsolete_1(gen_fsm, start, 3) ->
    {deprecated, {gen_statem, start, 3}};
obsolete_1(gen_fsm, start, 4) ->
    {deprecated, {gen_statem, start, 4}};

obsolete_1(gen_fsm, start_link, 3) ->
    {deprecated, {gen_statem, start_link, 3}};
obsolete_1(gen_fsm, start_link, 4) ->
    {deprecated, {gen_statem, start_link, 4}};

obsolete_1(gen_fsm, stop, 1) ->
    {deprecated, {gen_statem, stop, 1}};
obsolete_1(gen_fsm, stop, 3) ->
    {deprecated, {gen_statem, stop, 3}};

obsolete_1(gen_fsm, enter_loop, 4) ->
    {deprecated, {gen_statem, enter_loop, 4}};
obsolete_1(gen_fsm, enter_loop, 5) ->
    {deprecated, {gen_statem, enter_loop, 5}};
obsolete_1(gen_fsm, enter_loop, 6) ->
    {deprecated, {gen_statem, enter_loop, 6}};

obsolete_1(gen_fsm, reply, 2) ->
    {deprecated, {gen_statem, reply, 2}};

obsolete_1(gen_fsm, send_event, 2) ->
    {deprecated, {gen_statem, cast, 2}};
obsolete_1(gen_fsm, send_all_state_event, 2) ->
    {deprecated, {gen_statem, cast, 2}};

obsolete_1(gen_fsm, sync_send_event, 2) ->
    {deprecated, {gen_statem, call, 2}};
obsolete_1(gen_fsm, sync_send_event, 3) ->
    {deprecated, {gen_statem, call, 3}};

obsolete_1(gen_fsm, sync_send_all_state_event, 2) ->
    {deprecated, {gen_statem, call, 2}};
obsolete_1(gen_fsm, sync_send_all_state_event, 3) ->
    {deprecated, {gen_statem, call, 3}};

obsolete_1(gen_fsm, start_timer, 2) ->
    {deprecated, {erlang, start_timer, 3}};
obsolete_1(gen_fsm, cancel_timer, 1) ->
    {deprecated, {erlang, cancel_timer, 1}};
obsolete_1(gen_fsm, send_event_after, 2) ->
    {deprecated, {erlang, send_after, 3}};

%% *** CRYPTO added in OTP 22.2 ***

obsolete_1(crypto, next_iv, 2) ->
    {deprecated,
     "Deprecated. See the 'New and Old API' chapter of the CRYPTO User's Guide."
    };
obsolete_1(crypto, next_iv, 3) ->
    {deprecated,
     "Deprecated. See the 'New and Old API' chapter of the CRYPTO User's Guide."
    };

obsolete_1(crypto, hmac, 3) ->
    {deprecated, {crypto, mac, 4}};
obsolete_1(crypto, hmac, 4) ->
    {deprecated, {crypto, macN, 5}};

obsolete_1(crypto, hmac_init, 2) ->
    {deprecated, {crypto, mac_init, 3}};
obsolete_1(crypto, hmac_update, 2) ->
    {deprecated, {crypto, mac_update, 2}};
obsolete_1(crypto, hmac_final, 1) ->
    {deprecated, {crypto, mac_final, 1}};
obsolete_1(crypto, hmac_final_n, 2) ->
    {deprecated, {crypto, mac_finalN, 2}};

obsolete_1(crypto, cmac, 3) ->
    {deprecated, {crypto, mac, 4}};
obsolete_1(crypto, cmac, 4) ->
    {deprecated, {crypto, macN, 5}};

obsolete_1(crypto, poly1305, 2) ->
    {deprecated, {crypto, mac, 3}};

obsolete_1(crypto, stream_init, 2) ->
    {deprecated,
     "Deprecated and will be removed in a future release; "
     "Use crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1 or "
     "crypto:crypto_one_time/4."
    };
obsolete_1(crypto, stream_init, 3) ->
    {deprecated,
     "Deprecated and will be removed in a future release; "
     "Use crypto:crypto_init/4 + crypto:crypto_update/2 + crypto:crypto_final/1 or "
     "crypto:crypto_one_time/5."
    };
obsolete_1(crypto, stream_encrypt, 2) ->
    {deprecated, {crypto, crypto_update, 2}};
obsolete_1(crypto, stream_decrypt, 2) ->
    {deprecated, {crypto, crypto_update, 2}};

obsolete_1(crypto, block_encrypt, 3) ->
    {deprecated,
     "Deprecated and will be removed in a future release; "
     "Use crypto:crypto_one_time/4 "
     "or crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1."
    };
obsolete_1(crypto, block_encrypt, 4) ->
    {deprecated,
     "Deprecated. and will be removed in a future release; "
     "Use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 "
     "or crypto:crypto_(dyn_iv)?_init + crypto:crypto_(dyn_iv)?_update + crypto:crypto_final."
    };
obsolete_1(crypto, block_decrypt, 3) ->
    {deprecated,
     "Deprecated and will be removed in a future release; "
     "Use crypto:crypto_one_time/4 "
     "or crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1."
    };
obsolete_1(crypto, block_decrypt, 4) ->
    {deprecated,
     "Deprecated and will be removed in a future release; "
     "Use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 "
     "or crypto:crypto_(dyn_iv)?_init + crypto:crypto_(dyn_iv)?_update + crypto:crypto_final."
    };

%% *** CRYPTO added in OTP 20 ***

obsolete_1(crypto, rand_uniform, 2) ->
    {deprecated, {rand, uniform, 1}};

%% *** CRYPTO added in OTP 19 ***

obsolete_1(crypto, rand_bytes, 1) ->
    {removed, {crypto, strong_rand_bytes, 1}, "20.0"};

%% *** CRYPTO added in R16B01 ***

obsolete_1(crypto, md4, 1) ->
    {removed, {crypto, hash, 2}, "20.0"};
obsolete_1(crypto, md5, 1) ->
    {removed, {crypto, hash, 2}, "20.0"};
obsolete_1(crypto, sha, 1) ->
    {removed, {crypto, hash, 2}, "20.0"};

obsolete_1(crypto, md4_init, 0) ->
    {removed, {crypto, hash_init, 1}, "20.0"};
obsolete_1(crypto, md5_init, 0) ->
    {removed, {crypto, hash_init, 1}, "20.0"};
obsolete_1(crypto, sha_init, 0) ->
    {removed, {crypto, hash_init, 1}, "20.0"};

obsolete_1(crypto, md4_update, 2) ->
    {removed, {crypto, hash_update, 2}, "20.0"};
obsolete_1(crypto, md5_update, 2) ->
    {removed, {crypto, hash_update, 2}, "20.0"};
obsolete_1(crypto, sha_update, 2) ->
    {removed, {crypto, hash_update, 2}, "20.0"};

obsolete_1(crypto, md4_final, 1) ->
    {removed, {crypto, hash_final, 1}, "20.0"};
obsolete_1(crypto, md5_final, 1) ->
    {removed, {crypto, hash_final, 1}, "20.0"};
obsolete_1(crypto, sha_final, 1) ->
    {removed, {crypto, hash_final, 1}, "20.0"};

obsolete_1(crypto, md5_mac, 2) ->
    {removed, {crypto, hmac, 3}, "20.0"};
obsolete_1(crypto, sha_mac, 2) ->
    {removed, {crypto, hmac, 3}, "20.0"};
obsolete_1(crypto, sha_mac, 3) ->
    {removed, {crypto, hmac, 4}, "20.0"};

obsolete_1(crypto, sha_mac_96, 2) ->
    {removed, {crypto, hmac, 4}, "20.0"};
obsolete_1(crypto, md5_mac_96, 2) ->
    {removed, {crypto, hmac, 4}, "20.0"};

obsolete_1(crypto, rsa_sign, 2) ->
    {removed, {crypto, sign, 4}, "20.0"};
obsolete_1(crypto, rsa_sign, 3) ->
    {removed, {crypto, sign, 4}, "20.0"};
obsolete_1(crypto, rsa_verify, 3) ->
    {removed, {crypto, verify, 5}, "20.0"};
obsolete_1(crypto, rsa_verify, 4) ->
    {removed, {crypto, verify, 5}, "20.0"};

obsolete_1(crypto, dss_sign, 2) ->
    {removed, {crypto, sign, 4}, "20.0"};
obsolete_1(crypto, dss_sign, 3) ->
    {removed, {crypto, sign, 4}, "20.0"};

obsolete_1(crypto, dss_verify, 3) ->
    {removed, {crypto, verify, 5}, "20.0"};
obsolete_1(crypto, dss_verify, 4) ->
    {removed, {crypto, verify, 5}, "20.0"};

obsolete_1(crypto, mod_exp, 3) ->
    {removed, {crypto, mod_pow, 3}, "20.0"};

obsolete_1(crypto, dh_compute_key, 3) ->
    {removed, {crypto, compute_key, 4}, "20.0"};
obsolete_1(crypto, dh_generate_key, 1) ->
    {removed, {crypto, generate_key, 2}, "20.0"};
obsolete_1(crypto, dh_generate_key, 2) ->
    {removed, {crypto, generate_key, 3}, "20.0"};

obsolete_1(crypto, des_cbc_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, des3_cbc_encrypt, 5) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, des_ecb_encrypt, 2) ->
    {removed, {crypto, block_encrypt, 3}, "20.0"};
obsolete_1(crypto, des_ede3_cbc_encrypt, 5) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, des_cfb_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, des3_cfb_encrypt, 5) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, blowfish_ecb_encrypt, 2) ->
    {removed, {crypto, block_encrypt, 3}, "20.0"};
obsolete_1(crypto, blowfish_cbc_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, blowfish_cfb64_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, blowfish_ofb64_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, aes_cfb_128_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, aes_cbc_128_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto, aes_cbc_256_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto,rc2_cbc_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};
obsolete_1(crypto,rc2_40_cbc_encrypt, 3) ->
    {removed, {crypto, block_encrypt, 4}, "20.0"};

obsolete_1(crypto, des_cbc_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, des3_cbc_decrypt, 5) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, des_ecb_decrypt, 2) ->
    {removed, {crypto, block_decrypt, 3}, "20.0"};
obsolete_1(crypto, des_ede3_cbc_decrypt, 5) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, des_cfb_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, des3_cfb_decrypt, 5) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, blowfish_ecb_decrypt, 2) ->
    {removed, {crypto, block_decrypt, 3}, "20.0"};
obsolete_1(crypto, blowfish_cbc_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, blowfish_cfb64_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, blowfish_ofb64_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, aes_cfb_128_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, aes_cbc_128_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto, aes_cbc_256_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto,rc2_cbc_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};
obsolete_1(crypto,rc2_40_cbc_decrypt, 3) ->
    {removed, {crypto, block_decrypt, 4}, "20.0"};

obsolete_1(crypto, aes_ctr_stream_decrypt, 2) ->
    {removed, {crypto, stream_decrypt, 2}, "20.0"};
obsolete_1(crypto, aes_ctr_stream_encrypt, 2) ->
    {removed, {crypto, stream_encrypt, 2}, "20.0"};
obsolete_1(crypto, aes_ctr_decrypt, 3) ->
    {removed, {crypto, stream_decrypt, 2}, "20.0"};
obsolete_1(crypto, aes_ctr_encrypt, 3) ->
    {removed, {crypto, stream_encrypt, 2}, "20.0"};
obsolete_1(crypto, rc4_encrypt, 2) ->
    {removed, {crypto, stream_encrypt, 2}, "20.0"};
obsolete_1(crypto, rc4_encrypt_with_state, 2) ->
    {removed, {crypto, stream_encrypt, 2}, "20.0"};
obsolete_1(crypto, aes_ctr_stream_init, 2) ->
    {removed, {crypto, stream_init, 3}, "20.0"};
obsolete_1(crypto, rc4_set_key, 1) ->
    {removed, {crypto, stream_init, 2}, "20.0"};

obsolete_1(crypto, rsa_private_decrypt, 3) ->
    {removed, {crypto, private_decrypt, 4}, "20.0"};
obsolete_1(crypto, rsa_public_decrypt, 3) ->
    {removed, {crypto, public_decrypt, 4}, "20.0"};
obsolete_1(crypto, rsa_private_encrypt, 3) ->
    {removed, {crypto, private_encrypt, 4}, "20.0"};
obsolete_1(crypto, rsa_public_encrypt, 3) ->
    {removed, {crypto, public_encrypt, 4}, "20.0"};

obsolete_1(crypto, des_cfb_ivec, 2) ->
    {removed, {crypto, next_iv, 3}, "20.0"};
obsolete_1(crypto,des_cbc_ivec, 1) ->
    {removed, {crypto, next_iv, 2}, "20.0"};
obsolete_1(crypto, aes_cbc_ivec, 1) ->
    {removed, {crypto, next_iv, 2}, "20.0"};

obsolete_1(crypto,info, 0) ->
    {removed, {crypto, module_info, 0}, "20.0"};

obsolete_1(crypto, strong_rand_mpint, 3) ->
    {removed, "removed in 20.0; only needed by removed functions"};
obsolete_1(crypto, erlint, 1) ->
    {removed, "removed in 20.0; only needed by removed functions"};
obsolete_1(crypto, mpint, 1) ->
    {removed, "removed in 20.0; only needed by removed functions"};


%% *** SNMP ***

obsolete_1(snmp, N, A) ->
    case is_snmp_agent_function(N, A) of
	false ->
	    no;
	true ->
	    {deprecated, "Deprecated (will be removed in OTP 18); use snmpa:"++atom_to_list(N)++"/"++
	     integer_to_list(A)++" instead"}
    end;

obsolete_1(snmpa, old_info_format, 1) ->
    {deprecated, "Deprecated; (will be removed in OTP 18); use \"new\" format instead"};


%% *** MEGACO ***

obsolete_1(megaco, format_versions, 1) ->
    {deprecated, "Deprecated; use megaco:print_version_info/0,1 instead"};


%% *** OS-MON-MIB ***

%% FIXME: Remove this warning in OTP 24.
obsolete_1(os_mon_mib, _, _) ->
    {removed, "was removed in 22.0"};

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

%% Added in R16
obsolete_1(wxCalendarCtrl, enableYearChange, _) -> %% wx bug documented?
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxDC, computeScaleAndOrigin, 1) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxClientDC, new, 0) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxPaintDC, new, 0) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxWindowDC, new, 0) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxGraphicsRenderer, createLinearGradientBrush, 7) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxGraphicsRenderer, createRadialGradientBrush, 8) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxGridCellEditor, endEdit, 4) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxGridCellEditor, paintBackground, 3) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxIdleEvent, canSend, 1) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxMDIClientWindow, new, 1) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxMDIClientWindow, new, 2) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxPostScriptDC, getResolution, 0) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxPostScriptDC, setResolution, 1) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxCursor, new, 3) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};
obsolete_1(wxCursor, new, 4) ->
    {deprecated,"deprecated function not available in wxWidgets-2.9 and later"};

%% Added in OTP 17.
obsolete_1(asn1ct, decode,3) ->
    {removed,"removed; use Mod:decode/2 instead"};
obsolete_1(asn1ct, encode, 2) ->
    {removed,"removed; use Mod:encode/2 instead"};
obsolete_1(asn1ct, encode, 3) ->
    {removed,"removed; use Mod:encode/2 instead"};
obsolete_1(asn1rt, decode,3) ->
    {removed,"removed; use Mod:decode/2 instead"};
obsolete_1(asn1rt, encode, 2) ->
    {removed,"removed; use Mod:encode/2 instead"};
obsolete_1(asn1rt, encode, 3) ->
    {removed,"removed; use Mod:encode/2 instead"};
obsolete_1(asn1rt, info, 1) ->
    {removed,"removed; use Mod:info/0 instead"};
obsolete_1(asn1rt, utf8_binary_to_list, 1) ->
    {removed,{unicode,characters_to_list,1},"OTP 20"};
obsolete_1(asn1rt, utf8_list_to_binary, 1) ->
    {removed,{unicode,characters_to_binary,1},"OTP 20"};

%% Added in OTP 18.
obsolete_1(core_lib, get_anno, 1) ->
    {removed,{cerl,get_ann,1},"19"};
obsolete_1(core_lib, set_anno, 2) ->
    {removed,{cerl,set_ann,2},"19"};
obsolete_1(core_lib, is_literal, 1) ->
    {removed,{cerl,is_literal,1},"19"};
obsolete_1(core_lib, is_literal_list, 1) ->
    {removed,"removed; use lists:all(fun cerl:is_literal/1, L)"
     " instead"};
obsolete_1(core_lib, literal_value, 1) ->
    {removed,{core_lib,concrete,1},"19"};
obsolete_1(erl_scan, set_attribute, 3) ->
    {removed,{erl_anno,set_line,2},"19.0"};
obsolete_1(erl_scan, attributes_info, 1) ->
    {removed,"removed in 19.0; use "
     "erl_anno:{column,line,location,text}/1 instead"};
obsolete_1(erl_scan, attributes_info, 2) ->
    {removed,"removed in 19.0; use "
     "erl_anno:{column,line,location,text}/1 instead"};
obsolete_1(erl_scan, token_info, 1) ->
    {removed,"removed in 19.0; use "
     "erl_scan:{category,column,line,location,symbol,text}/1 instead"};
obsolete_1(erl_scan, token_info, 2) ->
    {removed,"removed in 19.0; use "
     "erl_scan:{category,column,line,location,symbol,text}/1 instead"};
obsolete_1(erl_parse, set_line, 2) ->
    {removed,{erl_anno,set_line,2},"19.0"};
obsolete_1(erl_parse, get_attributes, 1) ->
    {removed,"removed in 19.0; use "
     "erl_anno:{column,line,location,text}/1 instead"};
obsolete_1(erl_parse, get_attribute, 2) ->
    {removed,"removed in 19.0; use "
     "erl_anno:{column,line,location,text}/1 instead"};
obsolete_1(erl_lint, modify_line, 2) ->
    {removed,{erl_parse,map_anno,2},"19.0"};
obsolete_1(ssl, negotiated_next_protocol, 1) ->
    {removed,"removed in 20.0; use ssl:negotiated_protocol/1 instead"};
obsolete_1(ssl, connection_info, 1) ->
    {removed, "removed in 20.0; use ssl:connection_information/[1,2] instead"};

obsolete_1(httpd_conf, check_enum, 2) ->
    {removed, "removed; use lists:member/2 instead"};
obsolete_1(httpd_conf, clean, 1) ->
    {removed, "removed; use sting:strip/1 instead or possible the re module"};
obsolete_1(httpd_conf, custom_clean, 3) ->
    {removed, "removed; use sting:strip/3 instead or possible the re module"};
obsolete_1(httpd_conf, is_directory, 1) ->
    {removed, "removed; use filelib:is_dir/1 instead"};
obsolete_1(httpd_conf, is_file, 1) ->
    {removed, "removed; use filelib:is_file/1 instead"};
obsolete_1(httpd_conf, make_integer, 1) ->
    {removed, "removed; use erlang:list_to_integer/1 instead"};

%% Added in OTP 19.

obsolete_1(random, _, _) ->
    {deprecated, "the 'random' module is deprecated; "
     "use the 'rand' module instead"};
obsolete_1(code, rehash, 0) ->
    {deprecated, "deprecated because the code path cache feature has been removed"};
obsolete_1(queue, lait, 1) ->
    {deprecated, {queue,liat,1}};

%% Removed in OTP 19.

obsolete_1(rpc, safe_multi_server_call, A) when A =:= 2; A =:= 3 ->
    {removed, {rpc, multi_server_call, A}, "19.0"};

%% Added in OTP 20.

obsolete_1(filename, find_src, 1) ->
    {deprecated, "deprecated; use filelib:find_source/1 instead"};
obsolete_1(filename, find_src, 2) ->
    {deprecated, "deprecated; use filelib:find_source/3 instead"};

obsolete_1(erlang, get_stacktrace, 0) ->
    {deprecated, "deprecated; use the new try/catch syntax for retrieving the stack backtrace"};

%% Removed in OTP 20.

obsolete_1(erlang, hash, 2) ->
    {removed, {erlang, phash2, 2}, "20.0"};

%% Add in OTP 21.

obsolete_1(ssl, ssl_accept, 1) ->
    {deprecated, "deprecated; use ssl:handshake/1 instead"};
obsolete_1(ssl, ssl_accept, 2) ->
    {deprecated, "deprecated; use ssl:handshake/2 instead"};
obsolete_1(ssl, ssl_accept, 3) ->
    {deprecated, "deprecated; use ssl:handshake/3 instead"};
obsolete_1(otp_mib, F, _) when F =:= load; F =:= unload ->
    {deprecated, "deprecated; functionality will be removed in a future release"};

%% not obsolete

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

-dialyzer({no_match, obsolete_type/3}).

-spec obsolete_type(module(), atom(), arity()) ->
	'no' | {tag(), string()} | {tag(), mfas(), release()}.

-dialyzer({no_match, obsolete_type/3}).
obsolete_type(Module, Name, NumberOfVariables) ->
    case obsolete_type_1(Module, Name, NumberOfVariables) of
        {deprecated=Tag,{_,_,_}=Replacement} ->
            {Tag,Replacement,"in a future release"};
	{_,String}=Ret when is_list(String) ->
	    Ret;
        {_,_,_}=Ret ->
            Ret;
	no ->
	    no
    end.

obsolete_type_1(erl_scan,column,0) ->
    {removed,{erl_anno,column,0},"19.0"};
obsolete_type_1(erl_scan,line,0) ->
    {removed,{erl_anno,line,0},"19.0"};
obsolete_type_1(erl_scan,location,0) ->
    {removed,{erl_anno,location,0},"19.0"};
obsolete_type_1(_,_,_) ->
    no.
