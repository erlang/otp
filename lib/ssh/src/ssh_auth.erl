%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(ssh_auth).
-moduledoc false.

-include_lib("public_key/include/public_key.hrl").

-include("ssh.hrl").
-include("ssh_auth.hrl").
-include("ssh_agent.hrl").
-include("ssh_transport.hrl").

-export([get_public_key/2,
         publickey_msg/1, password_msg/1, keyboard_interactive_msg/1,
	 service_request_msg/1, init_userauth_request_msg/1,
	 userauth_request_msg/1, handle_userauth_request/3, ssh_msg_userauth_result/1,
	 handle_userauth_info_request/2, handle_userauth_info_response/2
	]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/3]).

%%--------------------------------------------------------------------
%%% Internal application API
%%--------------------------------------------------------------------
%%%----------------------------------------------------------------
userauth_request_msg(#ssh{userauth_methods = ServerMethods,
			  userauth_supported_methods = UserPrefMethods,
			  userauth_preference = ClientMethods0
			 } = Ssh0) ->
    case sort_select_mthds(ClientMethods0, UserPrefMethods, ServerMethods) of
	[] ->
            {send_disconnect, ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE, Ssh0};
	
	[{Pref,Module,Function,Args} | Prefs] ->
	    Ssh = case Pref of
		      "keyboard-interactive" -> Ssh0;
		      _ -> Ssh0#ssh{userauth_preference = Prefs}
		  end,
	    case Module:Function(Args ++ [Ssh]) of
		{not_ok, Ssh1} ->
		    userauth_request_msg(Ssh1#ssh{userauth_preference = Prefs});
		Result ->
		    {Pref,Result}
	    end
    end.



sort_select_mthds(Clients, undefined, Servers) ->
    %% User has not expressed an opinion via option "auth_methods", use the server's prefs
    sort_select_mthds1(Clients, Servers, string:tokens(?SUPPORTED_AUTH_METHODS,","));

sort_select_mthds(Clients, Users0, Servers0) ->
    %% The User has an opinion, use the intersection of that and the Servers whishes but
    %% in the Users order
    sort_select_mthds1(Clients, string:tokens(Users0,","), Servers0).


sort_select_mthds1(Clients, Users0, Servers0) ->
    Servers = unique(Servers0),
    Users = unique(Users0),
    [C || Key <- Users,
	  lists:member(Key, Servers),
	  C <- Clients,
	  element(1,C) == Key].

unique(L) -> 
    lists:reverse(
      lists:foldl(fun(E,Acc) -> 
			  case lists:member(E,Acc) of
			      true -> Acc;
			      false -> [E|Acc]
			  end
		  end, [], L)).
    

%%%---- userauth_request_msg "callbacks"
password_msg([#ssh{opts = Opts,
		   user = User,
                   service = Service} = Ssh0]) ->
    IoCb = ?GET_INTERNAL_OPT(io_cb, Opts),
    {Password,Ssh} = 
	case ?GET_OPT(password, Opts) of
	    undefined when IoCb == ssh_no_io ->
		{not_ok, Ssh0};
	    undefined -> 
		{IoCb:read_password("ssh password: ",Opts), Ssh0};
	    PW ->
		%% If "password" option is given it should not be tried again
		{PW, Ssh0#ssh{opts = ?PUT_OPT({password,not_ok}, Opts)}}
	end,
    case Password of
	not_ok ->
	    {not_ok, Ssh};
	_  ->
            {#ssh_msg_userauth_request{user = User,
                                       service = Service,
                                       method = "password",
                                       data =
                                           <<?BOOLEAN(?FALSE),
                                             ?STRING(unicode:characters_to_binary(Password))>>},
             Ssh}
    end.

%% See RFC 4256 for info on keyboard-interactive
keyboard_interactive_msg([#ssh{user = User,
			       opts = Opts,
			       service = Service} = Ssh]) ->
    case ?GET_OPT(password, Opts) of
	not_ok ->
	    {not_ok,Ssh};       % No need to use a failed pwd once more
	_ ->
            {#ssh_msg_userauth_request{user = User,
                                       service = Service,
                                       method = "keyboard-interactive",
                                       data = << ?STRING(<<"">>),
                                                 ?STRING(<<>>) >> },
             Ssh}
    end.


get_public_key(SigAlg, #ssh{opts = Opts}) ->
    KeyAlg = key_alg(SigAlg),
    case ssh_transport:call_KeyCb(user_key, [KeyAlg], Opts) of
        {ok, {ssh2_pubkey, PubKeyBlob}} ->
            {ok, {ssh2_pubkey, PubKeyBlob}};

        {ok, PrivKey} ->
            try
                %% Check the key - the KeyCb may be a buggy plugin
                true = ssh_transport:valid_key_sha_alg(private, PrivKey, KeyAlg),
                Key = ssh_file:extract_public_key(PrivKey),
                ssh_message:ssh2_pubkey_encode(Key)
            of
                PubKeyBlob -> {ok, {PrivKey, PubKeyBlob}}
            catch
                _:_ -> 
                    not_ok
            end;

        _Error ->
            not_ok
    end.


publickey_msg([SigAlg, #ssh{user = User,
                            session_id = SessionId,
                            service = Service,
                            opts = Opts} = Ssh]) ->
    case get_public_key(SigAlg, Ssh) of
        {ok, {_, PubKeyBlob} = Key} ->
            SigAlgStr = atom_to_list(SigAlg),
            SigData = build_sig_data(SessionId, User, Service, PubKeyBlob, SigAlgStr),

            SigRes = case Key of
                         {ssh2_pubkey, PubKeyBlob} ->
                             {ok, ssh_transport:call_KeyCb(sign, [PubKeyBlob, SigData], Opts)};
                         {PrivKey, PubKeyBlob} ->
                             ssh_transport:sign(SigData, SigAlg, PrivKey, Ssh)
                     end,
            case SigRes of
                {ok,Sig} ->
                    SigBlob = list_to_binary([?string(SigAlgStr),
                                              ?binary(Sig)]),

                    {#ssh_msg_userauth_request{user = User,
                                               service = Service,
                                               method = "publickey",
                                               data = [?TRUE,
                                                       ?string(SigAlgStr),
                                                       ?binary(PubKeyBlob),
                                                       ?binary(SigBlob)]},
                     Ssh};
                {error,_} ->
                    {not_ok, Ssh}
            end;

        _ ->
            {not_ok, Ssh}
    end.

%%%----------------------------------------------------------------
service_request_msg(Ssh) ->
    {#ssh_msg_service_request{name = "ssh-userauth"},
     Ssh#ssh{service = "ssh-userauth"}}.

%%%----------------------------------------------------------------
init_userauth_request_msg(#ssh{opts = Opts} = Ssh) ->
    %% Client side
    case ?GET_OPT(user, Opts) of
	undefined ->
	    ?DISCONNECT(?SSH_DISCONNECT_ILLEGAL_USER_NAME,
                        "Could not determine the users name");
	User ->
            {#ssh_msg_userauth_request{user = User,
                                       service = "ssh-connection",
                                       method = "none",
                                       data = <<>>},
             Ssh#ssh{user = User,
                     userauth_preference = method_preference(Ssh#ssh.userauth_pubkeys),
                     userauth_methods = none,
                     service = "ssh-connection"}
            }
    end.

%%%----------------------------------------------------------------
%%% called by server
handle_userauth_request(#ssh_msg_service_request{name = Name = "ssh-userauth"},
			_, Ssh) ->
    {ok, {#ssh_msg_service_accept{name = Name},
          Ssh#ssh{service = "ssh-connection"}}};

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "password",
						  data = <<?FALSE, ?UINT32(Sz), BinPwd:Sz/binary>>}, _, 
			#ssh{userauth_supported_methods = Methods} = Ssh) ->
    Password = unicode:characters_to_list(BinPwd),
    case check_password(User, Password, Ssh) of
	{true,Ssh1} ->
	    {authorized, User,
	     {#ssh_msg_userauth_success{}, Ssh1}
            };
	{false,Ssh1}  ->
	    {not_authorized, {User, {error,"Bad user or password"}}, 
	     {#ssh_msg_userauth_failure{authentications = Methods,
                                        partial_success = false}, Ssh1}
            }
    end;

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "password",
						  data = <<?TRUE,
							   _/binary
							   %% ?UINT32(Sz1), OldBinPwd:Sz1/binary,
							   %% ?UINT32(Sz2), NewBinPwd:Sz2/binary
							 >>
						 }, _, 
			#ssh{userauth_supported_methods = Methods} = Ssh) ->
    %% Password change without us having sent SSH_MSG_USERAUTH_PASSWD_CHANGEREQ (because we never do)
    %% RFC 4252 says:
    %%   SSH_MSG_USERAUTH_FAILURE without partial success - The password
    %%   has not been changed.  Either password changing was not supported,
    %%   or the old password was bad. 

    {not_authorized, {User, {error,"Password change not supported"}}, 
     {#ssh_msg_userauth_failure{authentications = Methods,
                                partial_success = false}, Ssh}
    };

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "none"}, _,
			#ssh{userauth_supported_methods = Methods,
                             opts = Opts} = Ssh) ->
    case ?GET_OPT(no_auth_needed, Opts) of
        false ->
            %% The normal case
            {not_authorized, {User, undefined},
             {#ssh_msg_userauth_failure{authentications = Methods,
                                        partial_success = false}, Ssh}
            };
        true ->
            %% RFC 4252  5.2
	    {authorized, User,
             {#ssh_msg_userauth_success{}, Ssh}
            }
    end;

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "publickey",
						  data = <<?BYTE(?FALSE),
							   ?UINT32(ALen), BAlg:ALen/binary,
							   ?UINT32(KLen), KeyBlob:KLen/binary,
							   _/binary
							 >>
						 }, 
			_SessionId, 
			#ssh{userauth_supported_methods = Methods} = Ssh0) ->
    Ssh =
        case check_user(User, Ssh0) of
            {true,Ssh01} -> Ssh01#ssh{user=User};
            {false,Ssh01} -> Ssh01#ssh{user=false}
        end,

    case
        pre_verify_sig(User, KeyBlob, Ssh)
    of
	true ->
	    {not_authorized, {User, undefined},
             {#ssh_msg_userauth_pk_ok{algorithm_name = binary_to_list(BAlg),
                                     key_blob = KeyBlob}, Ssh}
            };
	false ->
	    {not_authorized, {User, undefined}, 
	     {#ssh_msg_userauth_failure{authentications = Methods,
                                        partial_success = false}, Ssh}
            }
    end;

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "publickey",
						  data = <<?BYTE(?TRUE),
							   ?UINT32(ALen), BAlg:ALen/binary,
							   ?UINT32(KLen), KeyBlob:KLen/binary,
							   SigWLen/binary>>
						 }, 
			SessionId, 
			#ssh{user = PreVerifyUser,
                             userauth_supported_methods = Methods} = Ssh0) ->
    
    {UserOk,Ssh} = check_user(User, Ssh0),
    case
        ((PreVerifyUser == User) orelse (PreVerifyUser == undefined)) andalso
        UserOk andalso
        verify_sig(SessionId, User, "ssh-connection", BAlg, KeyBlob, SigWLen, Ssh)
    of
	true ->
	    {authorized, User, 
             {#ssh_msg_userauth_success{}, Ssh}
            };
	false ->
	    {not_authorized, {User, undefined}, 
	     {#ssh_msg_userauth_failure{authentications = Methods,
                                        partial_success = false}, Ssh}
            }
    end;

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "keyboard-interactive",
						  data = _},
			_, #ssh{opts = Opts,
				kb_tries_left = KbTriesLeft,
				userauth_supported_methods = Methods} = Ssh) ->
    case KbTriesLeft of
	N when N<1 ->
	    {not_authorized, {User, {authmethod, "keyboard-interactive"}}, 
             {#ssh_msg_userauth_failure{authentications = Methods,
                                        partial_success = false}, Ssh}
            };

	_ ->
	    %% RFC4256
	    %% The data field contains:
	    %%   - language tag (deprecated). If =/=[] SHOULD use it however. We skip
	    %%                                it for simplicity.
	    %%   - submethods. "... the user can give a hint of which actual methods
	    %%                  he wants to use. ...".  It's a "MAY use" so we skip
	    %%                  it. It also needs an understanding between the client
	    %%                  and the server.
	    %%                  
	    %% "The server MUST reply with an SSH_MSG_USERAUTH_SUCCESS,
	    %%  SSH_MSG_USERAUTH_FAILURE, or SSH_MSG_USERAUTH_INFO_REQUEST message."
	    Default = {"SSH server",
		       "Enter password for \""++User++"\"",
		       "password: ",
		       false},

	    {Name, Instruction, Prompt, Echo} =
		case ?GET_OPT(auth_method_kb_interactive_data, Opts) of
		    undefined -> 
			Default;
		    {_,_,_,_}=V -> 
			V;
                    F when is_function(F, 4) ->
			{_,PeerName} = Ssh#ssh.peer,
			F(PeerName, User, "ssh-connection", Ssh#ssh.pwdfun_user_state);
		    F when is_function(F) ->
			{_,PeerName} = Ssh#ssh.peer,
			F(PeerName, User, "ssh-connection")
		end,
	    EchoEnc = case Echo of
			  true -> <<?TRUE>>;
			  false -> <<?FALSE>>
		      end,
	    Msg = #ssh_msg_userauth_info_request{name = unicode:characters_to_list(Name),
						 instruction = unicode:characters_to_list(Instruction),
						 language_tag = "",
						 num_prompts = 1,
						 data = <<?STRING(unicode:characters_to_binary(Prompt)),
							  EchoEnc/binary
							>>
						},
	    {not_authorized, {User, undefined}, 
	     {Msg, Ssh#ssh{user = User}}
            }
    end;

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = Other}, _,
			#ssh{userauth_supported_methods = Methods} = Ssh) ->
    {not_authorized, {User, {authmethod, Other}}, 
     {#ssh_msg_userauth_failure{authentications = Methods,
                                partial_success = false}, Ssh}
    }.


%%%----------------------------------------------------------------
%%% keyboard-interactive client
handle_userauth_info_request(#ssh_msg_userauth_info_request{name = Name,
							    instruction = Instr,
							    num_prompts = NumPrompts,
							    data  = Data},
			     #ssh{opts=Opts} = Ssh) ->
    PromptInfos = decode_keyboard_interactive_prompts(NumPrompts,Data),
    case keyboard_interact_get_responses(Opts, Name, Instr, PromptInfos) of
	not_ok ->
	    not_ok;
	Responses ->
	    {ok, 
	     {#ssh_msg_userauth_info_response{num_responses = NumPrompts,
                                              data = Responses},
              Ssh}}
    end.

%%%----------------------------------------------------------------
%%% keyboard-interactive server
handle_userauth_info_response(#ssh_msg_userauth_info_response{num_responses = 1,
							      data = <<?UINT32(Sz), Password:Sz/binary>>},
			      #ssh{opts = Opts,
				   kb_tries_left = KbTriesLeft,
				   user = User,
				   userauth_supported_methods = Methods} = Ssh) ->
    SendOneEmpty =
	(?GET_OPT(tstflg,Opts) == one_empty)
	orelse 
	proplists:get_value(one_empty, ?GET_OPT(tstflg,Opts), false),

    case check_password(User, unicode:characters_to_list(Password), Ssh) of
	{true,Ssh1} when SendOneEmpty==true ->
	    {authorized_but_one_more, User,
             {#ssh_msg_userauth_info_request{name = "",
                                             instruction = "",
                                             language_tag = "",
                                             num_prompts = 0,
                                             data = <<?BOOLEAN(?FALSE)>>
                                            },
              Ssh1}};

	{true,Ssh1} ->
	    {authorized, User,
	     {#ssh_msg_userauth_success{}, Ssh1}};

	{false,Ssh1} ->
	    {not_authorized, {User, {error,"Bad user or password"}}, 
	     {#ssh_msg_userauth_failure{authentications = Methods,
                                        partial_success = false}, 
              Ssh1#ssh{kb_tries_left = max(KbTriesLeft-1, 0)}}}
    end;

handle_userauth_info_response({extra,#ssh_msg_userauth_info_response{}},
			      #ssh{user = User} = Ssh) ->
    {authorized, User,
     {#ssh_msg_userauth_success{}, Ssh}};

handle_userauth_info_response(#ssh_msg_userauth_info_response{},
			      _Auth) ->
    ?DISCONNECT(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                "Server does not support keyboard-interactive").

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
method_preference(SigKeyAlgs) ->
    %% PubKeyAlgs: List of user (client) public key algorithms to try to use.
    %% All of the acceptable algorithms is the default values.
    PubKeyDefs = [{"publickey", ?MODULE, publickey_msg, [A]} || A <- SigKeyAlgs],
    NonPKmethods = [{"password", ?MODULE, password_msg, []},
                    {"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []}
                   ],
    PubKeyDefs ++ NonPKmethods.

check_user(User, Ssh) ->
    case ?GET_OPT(pk_check_user, Ssh#ssh.opts) of
        true ->
            check_password(User, pubkey, Ssh);
        _ ->
            {true, Ssh} % i.e, skip the test
    end.

check_password(User, Password, #ssh{opts=Opts} = Ssh) ->
    case ?GET_OPT(pwdfun, Opts) of
        undefined when Password==pubkey ->
            %% Just check the User name
            case lists:keysearch(User, 1, ?GET_OPT(user_passwords,Opts)) of
                {value, {User, _}} -> {true, Ssh};
                false -> {false, Ssh}
            end;

	undefined ->
	    Static = get_password_option(Opts, User),
	    {ssh_lib:comp(Password,Static), Ssh};

	Checker when is_function(Checker,2) ->
	    {Checker(User, Password), Ssh};

	Checker when is_function(Checker,4) ->
	    #ssh{pwdfun_user_state = PrivateState,
		 peer = {_,PeerAddr={_,_}}
		} = Ssh,
	    case Checker(User, Password, PeerAddr, PrivateState) of
		true ->
		    {true,Ssh};
		false ->
		    {false,Ssh};
		{true,NewState} ->
		    {true, Ssh#ssh{pwdfun_user_state=NewState}};
		{false,NewState} ->
		    {false, Ssh#ssh{pwdfun_user_state=NewState}};
		disconnect ->
		    ?DISCONNECT(?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
                                "")
	    end
    end.

get_password_option(Opts, User) ->
    Passwords = ?GET_OPT(user_passwords, Opts),
    case lists:keysearch(User, 1, Passwords) of
	{value, {User, Pw}} -> Pw;
	false -> ?GET_OPT(password, Opts)
    end.
	    
pre_verify_sig(User, KeyBlob,  #ssh{opts=Opts}) ->
    try
	Key = ssh_message:ssh2_pubkey_decode(KeyBlob), % or exception
        ssh_transport:call_KeyCb(is_auth_key, [Key, User], Opts)
    catch
	_:_ ->
	    false
    end.

verify_sig(SessionId, User, Service, AlgBin, KeyBlob, SigWLen, #ssh{opts=Opts} = Ssh) ->
    try
        Alg = binary_to_list(AlgBin),
        true = lists:member(list_to_existing_atom(Alg), 
                            proplists:get_value(public_key,
                                                ?GET_OPT(preferred_algorithms,Opts))),
        Key = ssh_message:ssh2_pubkey_decode(KeyBlob), % or exception
        true = ssh_transport:call_KeyCb(is_auth_key, [Key, User], Opts),
        PlainText = build_sig_data(SessionId, User, Service, KeyBlob, Alg),
        <<?UINT32(AlgSigLen), AlgSig:AlgSigLen/binary>> = SigWLen,
        <<?UINT32(AlgLen), _Alg:AlgLen/binary,
          ?UINT32(SigLen), Sig:SigLen/binary>> = AlgSig,
        ssh_transport:verify(PlainText, list_to_existing_atom(Alg), Sig, Key, Ssh)
    catch
	_:_ ->
	    false
    end.

build_sig_data(SessionId, User, Service, KeyBlob, Alg) ->
    Sig = [?binary(SessionId),
	   ?SSH_MSG_USERAUTH_REQUEST,
	   ?string_utf8(User),
	   ?string(Service),
	   ?binary(<<"publickey">>),
	   ?TRUE,
	   ?string(Alg),
	   ?binary(KeyBlob)],
    list_to_binary(Sig).



key_alg('rsa-sha2-256') -> 'ssh-rsa';
key_alg('rsa-sha2-512') -> 'ssh-rsa';
key_alg(Alg) -> Alg.

%%%================================================================
%%%
%%% Keyboard-interactive
%%% 

decode_keyboard_interactive_prompts(_NumPrompts, Data) ->
    ssh_message:decode_keyboard_interactive_prompts(Data, []).

keyboard_interact_get_responses(Opts, Name, Instr, PromptInfos) ->
    keyboard_interact_get_responses(?GET_OPT(user_interaction, Opts),
				    ?GET_OPT(keyboard_interact_fun, Opts),
				    ?GET_OPT(password, Opts),
                                    Name,
				    Instr,
                                    PromptInfos,
                                    Opts).


%% Don't re-try an already rejected password. This could happen if both keyboard-interactive
%% and password methods are tried:
keyboard_interact_get_responses(_, _, not_ok, _, _, _, _) ->
    not_ok;

%% Only one password requestedm and we have got one via the 'password' option for the daemon:
keyboard_interact_get_responses(_, undefined, Pwd, _, _, [_], _) when Pwd =/= undefined ->
    [Pwd]; %% Password auth implemented with keyboard-interaction and passwd is known

%% No password requested (keyboard-interactive):
keyboard_interact_get_responses(_, _, _, _, _, [], _)  ->
    [];

%% user_interaction is forbidden (by option user_interaction) and we have to ask
%% the user for one or more.
%% Throw an error:
keyboard_interact_get_responses(false, undefined, undefined, _, _, [Prompt|_], Opts) ->
    ssh_no_io:read_line(Prompt, Opts);

%% One or more passwords are requested, we may prompt the user and no fun is used
%% to get the responses:
keyboard_interact_get_responses(true, undefined, _, Name, Instr, PromptInfos, Opts) ->
    prompt_user_for_passwords(Name, Instr, PromptInfos, Opts);

%% The passwords are provided with a fun. Use that one!
keyboard_interact_get_responses(true, Fun, _Pwd, Name, Instr, PromptInfos, _Opts) ->
    keyboard_interact_fun(Fun, Name, Instr, PromptInfos).



prompt_user_for_passwords(Name, Instr, PromptInfos, Opts) ->
    IoCb = ?GET_INTERNAL_OPT(io_cb, Opts),
    write_if_nonempty(IoCb, Name),
    write_if_nonempty(IoCb, Instr),
    lists:map(fun({Prompt, true})  -> IoCb:read_line(Prompt, Opts);
		 ({Prompt, false}) -> IoCb:read_password(Prompt, Opts)
	      end,
	      PromptInfos).

keyboard_interact_fun(KbdInteractFun, Name, Instr,  PromptInfos) ->
    case KbdInteractFun(Name, Instr, PromptInfos) of
	Responses when is_list(Responses),
                     length(Responses) == length(PromptInfos) ->
	    Responses;
	_ ->
            nok
    end.


write_if_nonempty(_, "") -> ok;
write_if_nonempty(_, <<>>) -> ok;
write_if_nonempty(IoCb, Text) -> IoCb:format("~s~n",[Text]).

%%%----------------------------------------------------------------
%%% Called just for the tracer ssh_dbg
ssh_msg_userauth_result(_R) -> ok.

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [authentication].

ssh_dbg_flags(authentication) -> [c].

ssh_dbg_on(authentication) -> dbg:tp(?MODULE, handle_userauth_request, 3, x),
                              dbg:tp(?MODULE, init_userauth_request_msg, 1, x),
                              dbg:tp(?MODULE, ssh_msg_userauth_result, 1, x),
                              dbg:tp(?MODULE, userauth_request_msg, 1, x).

ssh_dbg_off(authentication) -> dbg:ctpg(?MODULE, handle_userauth_request, 3),
                               dbg:ctpg(?MODULE, init_userauth_request_msg, 1),
                               dbg:ctpg(?MODULE, ssh_msg_userauth_result, 1),
                               dbg:ctpg(?MODULE, userauth_request_msg, 1).



%%% Server ----------------
ssh_dbg_format(authentication, {call, {?MODULE,handle_userauth_request, [Req,_SessionID,Ssh]}},
               Stack) ->
    {skip, [{Req,Ssh}|Stack]};


ssh_dbg_format(authentication, {return_from, {?MODULE,handle_userauth_request,3},
                                {ok,{#ssh_msg_service_accept{name=Name},_Ssh}}},
               [{#ssh_msg_service_request{name=Name},_} | Stack]) ->
    {skip, Stack};

ssh_dbg_format(authentication, {return_from, {?MODULE,handle_userauth_request,3},
                                {authorized,User,_Repl}},
              [{#ssh_msg_userauth_request{}=Req,Ssh}|Stack]) ->
    {["AUTH srvr: Peer client authorized\n",
      io_lib:format("user = ~p~n", [User]),
      fmt_req(Req, Ssh)],
     Stack};

ssh_dbg_format(authentication, {return_from, {?MODULE,handle_userauth_request,3},
                                {not_authorized,{User,_X},_Repl}},
               [{#ssh_msg_userauth_request{method="none"},Ssh}|Stack]) ->
    Methods = Ssh#ssh.userauth_supported_methods,
    {["AUTH srvr: Peer queries auth methods\n",
      io_lib:format("user = ~p~nsupported methods = ~p ?", [User,Methods])
     ],
     Stack};

ssh_dbg_format(authentication, {return_from, {?MODULE,handle_userauth_request,3},
                                {not_authorized,{User,_X}, Repl}
                               },
              [{#ssh_msg_userauth_request{method = "publickey",
                                          data = <<?BYTE(?FALSE), _/binary>>
                                         }=Req,Ssh}|Stack]) ->
    {case Repl of
         {#ssh_msg_userauth_pk_ok{}, _} ->
             ["AUTH srvr: Answer - pub key supported\n"];
          {#ssh_msg_userauth_failure{}, _} ->
             ["AUTH srvr: Answer - pub key not supported\n"];
          {Other, _} ->
             ["AUTH srvr: Answer - strange answer\n",
              io_lib:format("strange answer = ~p~n",[Other])
             ]
      end
     ++ [io_lib:format("user = ~p~n", [User]),
         fmt_req(Req, Ssh)],
     Stack};

ssh_dbg_format(authentication, {return_from, {?MODULE,handle_userauth_request,3},
                                {not_authorized,{User,_X},
                                 {#ssh_msg_userauth_info_request{},_Ssh}}},
               [{#ssh_msg_userauth_request{method="keyboard-interactive"
                                          } = Req,Ssh}|Stack]) ->
    {["AUTH srvr: Ask peer client for password\n",
      io_lib:format("user = ~p~n", [User]),
      fmt_req(Req, Ssh)],
     Stack};


ssh_dbg_format(authentication, {call, {?MODULE,ssh_msg_userauth_result,[success]}},
               Stack) ->
    {["AUTH client: Success"],Stack};
ssh_dbg_format(authentication, {return_from, {?MODULE,ssh_msg_userauth_result,1}, _Result},
               Stack) ->
    {skip, Stack};

ssh_dbg_format(authentication, {return_from, {?MODULE,handle_userauth_request,3},
                                {not_authorized,{User,_X},_Repl}},
              [{#ssh_msg_userauth_request{}=Req,Ssh}|Stack]) ->
    {["AUTH srvr: Peer client authorization failed\n",
      io_lib:format("user = ~p~n", [User]),
      fmt_req(Req, Ssh)],
     Stack};

%%% Client ----------------
ssh_dbg_format(authentication, {call, {?MODULE,init_userauth_request_msg, [#ssh{opts = Opts}]}},
               Stack) ->
    {["AUTH client: Service ssh-userauth accepted\n",
      case ?GET_OPT(user, Opts) of
          undefined ->
              io_lib:format("user = undefined *** ERROR ***", []);
          User ->
              io_lib:format("user = ~p", [User])
      end
     ],
     Stack};
ssh_dbg_format(authentication, {return_from, {?MODULE,init_userauth_request_msg,1},
                                {Repl = #ssh_msg_userauth_request{user = User,
                                                                  service = "ssh-connection",
                                                                  method = "none"},
                                 _Ssh}},
               Stack) ->
    {["AUTH client: Query for accepted methods\n",
      io_lib:format("user = ~p", [User])],
     [Repl|Stack]};

ssh_dbg_format(authentication,  {call, {?MODULE,userauth_request_msg,
                                        [#ssh{userauth_methods = Methods}]}},
               [ #ssh_msg_userauth_request{user = User,
                                           service = "ssh-connection",
                                           method = "none"} | Stack]) ->
    {["AUTH client: Server supports\n",
      io_lib:format("user = ~p~nmethods = ~p", [User,Methods])],
     Stack};

ssh_dbg_format(authentication,  {call, {?MODULE,userauth_request_msg,[_Ssh]}},
               Stack) ->
    {skip,Stack};

ssh_dbg_format(authentication, {return_from, {?MODULE,userauth_request_msg,1},
                                {send_disconnect, _Code, _Ssh}},
               Stack) ->
    {skip,Stack};
ssh_dbg_format(authentication, {return_from, {?MODULE,userauth_request_msg,1},
                                {Method,{_Msg,_Ssh}}},
               Stack) ->
    {["AUTH client: Try auth with\n",
      io_lib:format("method = ~p", [Method])],
     Stack};

               

ssh_dbg_format(authentication, Unhandled, Stack) ->
    case Unhandled of
        {call, {?MODULE,_F,_Args}} -> ok;
        {return_from, {?MODULE,_F,_A}, _Resp} -> ok
    end,
    {["UNHANDLED AUTH FORMAT\n",
      io_lib:format("Unhandled = ~p~nStack = ~p", [Unhandled,Stack])],
     Stack}.


%%% Dbg helpers ----------------


fmt_req(#ssh_msg_userauth_request{user = User,
                                  service = "ssh-connection",
                                  method = Method,
                                  data = Data}, 
        #ssh{kb_tries_left = KbTriesLeft,
             userauth_supported_methods = Methods}) ->
    [io_lib:format("req user = ~p~n"
                   "req method = ~p~n"
                   "supported methods = ~p",
                   [User,Method,Methods]),
     case Method of
         "none" -> "";
         "password" -> fmt_bool(Data);
         "keyboard-interactive" -> fmt_kb_tries_left(KbTriesLeft);
         "publickey" -> [case Data of
                             <<?BYTE(_), ?UINT32(ALen), Alg:ALen/binary, _/binary>> ->
                                 io_lib:format("~nkey-type = ~p", [Alg]);
                             _ ->
                                 ""
                         end];
         _ -> ""
     end].


fmt_kb_tries_left(N) when is_integer(N)->
    io_lib:format("~ntries left = ~p", [N-1]).


fmt_bool(<<?BYTE(Bool),_/binary>>) ->
    io_lib:format("~nBool = ~s",
                  [case Bool of
                       ?TRUE -> "true";
                       ?FALSE -> "false";
                       _ -> io_lib:format("? (~p)",[Bool])
                   end]);
fmt_bool(<<>>) ->
    "".



