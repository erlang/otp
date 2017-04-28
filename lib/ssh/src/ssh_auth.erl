%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-include_lib("public_key/include/public_key.hrl").

-include("ssh.hrl").
-include("ssh_auth.hrl").
-include("ssh_transport.hrl").

-export([publickey_msg/1, password_msg/1, keyboard_interactive_msg/1,
	 service_request_msg/1, init_userauth_request_msg/1,
	 userauth_request_msg/1, handle_userauth_request/3,
	 handle_userauth_info_request/2, handle_userauth_info_response/2
	]).

%%--------------------------------------------------------------------
%%% Internal application API
%%--------------------------------------------------------------------
%%%----------------------------------------------------------------
userauth_request_msg(#ssh{userauth_methods = ServerMethods,
			  userauth_supported_methods = UserPrefMethods, % Note: this is not documented as supported for clients
			  userauth_preference = ClientMethods0
			 } = Ssh0) ->
    case sort_select_mthds(ClientMethods0, UserPrefMethods, ServerMethods) of
	[] ->
	    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
				      description = "Unable to connect using the available authentication methods",
				      language = "en"},
	    {disconnect, Msg, ssh_transport:ssh_packet(Msg, Ssh0)};
	
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
password_msg([#ssh{opts = Opts, io_cb = IoCb,
		   user = User, service = Service} = Ssh0]) ->
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
	    ssh_transport:ssh_packet(
	      #ssh_msg_userauth_request{user = User,
					service = Service,
					method = "password",
					data =
					    <<?BOOLEAN(?FALSE),
					      ?STRING(unicode:characters_to_binary(Password))>>},
	      Ssh)
    end.

%% See RFC 4256 for info on keyboard-interactive
keyboard_interactive_msg([#ssh{user = User,
			       opts = Opts,
			       service = Service} = Ssh]) ->
    case ?GET_OPT(password, Opts) of
	not_ok ->
	    {not_ok,Ssh};       % No need to use a failed pwd once more
	_ ->
	    ssh_transport:ssh_packet(
	      #ssh_msg_userauth_request{user = User,
					service = Service,
					method = "keyboard-interactive",
					data = << ?STRING(<<"">>),
						  ?STRING(<<>>) >> },
	      Ssh)
    end.

publickey_msg([SigAlg, #ssh{user = User,
		       session_id = SessionId,
		       service = Service,
		       opts = Opts} = Ssh]) ->
    Hash = ssh_transport:sha(SigAlg),
    KeyAlg = key_alg(SigAlg),
    {KeyCb,KeyCbOpts} = ?GET_OPT(key_cb, Opts),
    UserOpts = ?GET_OPT(user_options, Opts),
    case KeyCb:user_key(KeyAlg, [{key_cb_private,KeyCbOpts}|UserOpts]) of
	{ok, PrivKey} ->
	    SigAlgStr = atom_to_list(SigAlg),
            try
                Key = ssh_transport:extract_public_key(PrivKey),
                public_key:ssh_encode(Key, ssh2_pubkey)
            of
		PubKeyBlob ->
		    SigData = build_sig_data(SessionId, User, Service,
                                             PubKeyBlob, SigAlgStr),
		    Sig = ssh_transport:sign(SigData, Hash, PrivKey),
		    SigBlob = list_to_binary([?string(SigAlgStr),
                                              ?binary(Sig)]),
		    ssh_transport:ssh_packet(
		      #ssh_msg_userauth_request{user = User,
						service = Service,
						method = "publickey",
						data = [?TRUE,
							?string(SigAlgStr),
							?binary(PubKeyBlob),
							?binary(SigBlob)]},
		      Ssh)
            catch
                _:_ -> 
		    {not_ok, Ssh}
            end;
     	_Error ->
	    {not_ok, Ssh}
    end.

%%%----------------------------------------------------------------
service_request_msg(Ssh) ->
    ssh_transport:ssh_packet(#ssh_msg_service_request{name = "ssh-userauth"},
			   Ssh#ssh{service = "ssh-userauth"}).

%%%----------------------------------------------------------------
init_userauth_request_msg(#ssh{opts = Opts} = Ssh) ->
    %% Client side
    case ?GET_OPT(user, Opts) of
	undefined ->
	    ErrStr = "Could not determine the users name",
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_ILLEGAL_USER_NAME,
				  description = ErrStr});
        
	User ->
            ssh_transport:ssh_packet(
              #ssh_msg_userauth_request{user = User,
                                        service = "ssh-connection",
                                        method = "none",
                                        data = <<>>},
              Ssh#ssh{user = User,
                      userauth_preference = method_preference(Ssh#ssh.userauth_pubkeys),
                      userauth_methods = none,
                      service = "ssh-connection"}
             )
    end.

%%%----------------------------------------------------------------
%%% called by server
handle_userauth_request(#ssh_msg_service_request{name = Name = "ssh-userauth"},
			_, Ssh) ->
    {ok, ssh_transport:ssh_packet(#ssh_msg_service_accept{name = Name},
				  Ssh#ssh{service = "ssh-connection"})};

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "password",
						  data = <<?FALSE, ?UINT32(Sz), BinPwd:Sz/binary>>}, _, 
			#ssh{opts = Opts,
			     userauth_supported_methods = Methods} = Ssh) ->
    Password = unicode:characters_to_list(BinPwd),
    case check_password(User, Password, Opts, Ssh) of
	{true,Ssh1} ->
	    {authorized, User,
	     ssh_transport:ssh_packet(#ssh_msg_userauth_success{}, Ssh1)};
	{false,Ssh1}  ->
	    {not_authorized, {User, {error,"Bad user or password"}}, 
	     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
		     authentications = Methods,
		     partial_success = false}, Ssh1)}
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
     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
				 authentications = Methods,
				 partial_success = false}, Ssh)};

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = "none"}, _,
			#ssh{userauth_supported_methods = Methods} = Ssh) ->
    {not_authorized, {User, undefined},
     ssh_transport:ssh_packet(
       #ssh_msg_userauth_failure{authentications = Methods,
				 partial_success = false}, Ssh)};

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
			#ssh{opts = Opts,
			     userauth_supported_methods = Methods} = Ssh) ->

    case pre_verify_sig(User, KeyBlob, Opts) of
	true ->
	    {not_authorized, {User, undefined},
	     ssh_transport:ssh_packet(
	       #ssh_msg_userauth_pk_ok{algorithm_name = binary_to_list(BAlg),
				       key_blob = KeyBlob}, Ssh)};
	false ->
	    {not_authorized, {User, undefined}, 
	     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
					 authentications = Methods,
					 partial_success = false}, Ssh)}
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
			#ssh{opts = Opts,
			     userauth_supported_methods = Methods} = Ssh) ->
    
    case verify_sig(SessionId, User, "ssh-connection", 
		    BAlg, KeyBlob, SigWLen, Opts) of
	true ->
	    {authorized, User, 
	     ssh_transport:ssh_packet(
	       #ssh_msg_userauth_success{}, Ssh)};
	false ->
	    {not_authorized, {User, undefined}, 
	     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
					 authentications = Methods,
					 partial_success = false}, Ssh)}
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
	     ssh_transport:ssh_packet(
	       #ssh_msg_userauth_failure{authentications = Methods,
					 partial_success = false}, Ssh)};

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
	     ssh_transport:ssh_packet(Msg, Ssh#ssh{user = User
						  })}
    end;

handle_userauth_request(#ssh_msg_userauth_request{user = User,
						  service = "ssh-connection",
						  method = Other}, _,
			#ssh{userauth_supported_methods = Methods} = Ssh) ->
    {not_authorized, {User, {authmethod, Other}}, 
     ssh_transport:ssh_packet(
       #ssh_msg_userauth_failure{authentications = Methods,
				 partial_success = false}, Ssh)}.


%%%----------------------------------------------------------------
%%% keyboard-interactive client
handle_userauth_info_request(#ssh_msg_userauth_info_request{name = Name,
							    instruction = Instr,
							    num_prompts = NumPrompts,
							    data  = Data},
			     #ssh{opts = Opts,
				  io_cb = IoCb
				 } = Ssh) ->
    PromptInfos = decode_keyboard_interactive_prompts(NumPrompts,Data),
    case keyboard_interact_get_responses(IoCb, Opts, Name, Instr, PromptInfos) of
	not_ok ->
	    not_ok;
	Responses ->
	    {ok, 
	     ssh_transport:ssh_packet(
	       #ssh_msg_userauth_info_response{num_responses = NumPrompts,
					       data = Responses}, Ssh)}
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

    case check_password(User, unicode:characters_to_list(Password), Opts, Ssh) of
	{true,Ssh1} when SendOneEmpty==true ->
	    Msg = #ssh_msg_userauth_info_request{name = "",
						 instruction = "",
						 language_tag = "",
						 num_prompts = 0,
						 data = <<?BOOLEAN(?FALSE)>>
						},
	    {authorized_but_one_more, User,
	     ssh_transport:ssh_packet(Msg, Ssh1)};

	{true,Ssh1} ->
	    {authorized, User,
	     ssh_transport:ssh_packet(#ssh_msg_userauth_success{}, Ssh1)};

	{false,Ssh1} ->
	    {not_authorized, {User, {error,"Bad user or password"}}, 
	     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
					 authentications = Methods,
					 partial_success = false}, 
				      Ssh1#ssh{kb_tries_left = max(KbTriesLeft-1, 0)}
				     )}
    end;

handle_userauth_info_response({extra,#ssh_msg_userauth_info_response{}},
			      #ssh{user = User} = Ssh) ->
    {authorized, User,
     ssh_transport:ssh_packet(#ssh_msg_userauth_success{}, Ssh)};

handle_userauth_info_response(#ssh_msg_userauth_info_response{},
			      _Auth) ->
    ssh_connection_handler:disconnect(
      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			  description = "Server does not support keyboard-interactive"
			 }).


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

check_password(User, Password, Opts, Ssh) ->
    case ?GET_OPT(pwdfun, Opts) of
	undefined ->
	    Static = get_password_option(Opts, User),
	    {Password == Static, Ssh};

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
		    ssh_connection_handler:disconnect(
		      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
					  description = "Unable to connect using the available authentication methods"
					 })
	    end
    end.

get_password_option(Opts, User) ->
    Passwords = ?GET_OPT(user_passwords, Opts),
    case lists:keysearch(User, 1, Passwords) of
	{value, {User, Pw}} -> Pw;
	false -> ?GET_OPT(password, Opts)
    end.
	    
pre_verify_sig(User, KeyBlob, Opts) ->
    try
	Key = public_key:ssh_decode(KeyBlob, ssh2_pubkey), % or exception
        {KeyCb,KeyCbOpts} = ?GET_OPT(key_cb, Opts),
        UserOpts = ?GET_OPT(user_options, Opts),
        KeyCb:is_auth_key(Key, User, [{key_cb_private,KeyCbOpts}|UserOpts])
    catch
	_:_ ->
	    false
    end.

verify_sig(SessionId, User, Service, AlgBin, KeyBlob, SigWLen, Opts) ->
    try
        Alg = binary_to_list(AlgBin),
        {KeyCb,KeyCbOpts} = ?GET_OPT(key_cb, Opts),
        UserOpts = ?GET_OPT(user_options, Opts),
        Key = public_key:ssh_decode(KeyBlob, ssh2_pubkey), % or exception
        true = KeyCb:is_auth_key(Key, User, [{key_cb_private,KeyCbOpts}|UserOpts]),
        PlainText = build_sig_data(SessionId, User, Service, KeyBlob, Alg),
        <<?UINT32(AlgSigLen), AlgSig:AlgSigLen/binary>> = SigWLen,
        <<?UINT32(AlgLen), _Alg:AlgLen/binary,
          ?UINT32(SigLen), Sig:SigLen/binary>> = AlgSig,
        ssh_transport:verify(PlainText, ssh_transport:sha(Alg), Sig, Key)
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



decode_keyboard_interactive_prompts(_NumPrompts, Data) ->
    ssh_message:decode_keyboard_interactive_prompts(Data, []).

keyboard_interact_get_responses(IoCb, Opts, Name, Instr, PromptInfos) ->
    NumPrompts = length(PromptInfos),
    keyboard_interact_get_responses(?GET_OPT(user_interaction, Opts),
				    ?GET_OPT(keyboard_interact_fun, Opts),
				    ?GET_OPT(password, Opts), IoCb, Name,
				    Instr, PromptInfos, Opts, NumPrompts).


keyboard_interact_get_responses(_, _, not_ok, _, _, _, _, _, _) ->
    not_ok;
keyboard_interact_get_responses(_, undefined, Password, _, _, _, _, _,
				1) when Password =/= undefined ->
    [Password]; %% Password auth implemented with keyboard-interaction and passwd is known
keyboard_interact_get_responses(_, _, _, _, _, _, _, _, 0)  ->
    [];
keyboard_interact_get_responses(false, undefined, undefined, _, _, _, [Prompt|_], Opts, _) ->
    ssh_no_io:read_line(Prompt, Opts); %% Throws error as keyboard interaction is not allowed
keyboard_interact_get_responses(true, undefined, _,IoCb, Name, Instr, PromptInfos, Opts, _) ->
    keyboard_interact(IoCb, Name, Instr, PromptInfos, Opts);
keyboard_interact_get_responses(true, Fun, _Pwd, _IoCb, Name, Instr, PromptInfos, _Opts, NumPrompts) ->
    keyboard_interact_fun(Fun, Name, Instr, PromptInfos, NumPrompts).

keyboard_interact(IoCb, Name, Instr, Prompts, Opts) ->
    write_if_nonempty(IoCb, Name),
    write_if_nonempty(IoCb, Instr),
    lists:map(fun({Prompt, true})  -> IoCb:read_line(Prompt, Opts);
		 ({Prompt, false}) -> IoCb:read_password(Prompt, Opts)
	      end,
	      Prompts).

write_if_nonempty(_, "") -> ok;
write_if_nonempty(_, <<>>) -> ok;
write_if_nonempty(IoCb, Text) -> IoCb:format("~s~n",[Text]).


keyboard_interact_fun(KbdInteractFun, Name, Instr,  PromptInfos, NumPrompts) ->
    Prompts = lists:map(fun({Prompt, _Echo}) -> Prompt end,
			PromptInfos),
    case KbdInteractFun(Name, Instr, Prompts) of
	Rs when length(Rs) == NumPrompts ->
	    Rs;
	Rs ->
	    throw({mismatching_number_of_responses,
		   {got,Rs},
		   {expected, NumPrompts},
		   #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
				       description = "User interaction failed",
				       language = "en"}})
    end.


key_alg('rsa-sha2-256') -> 'ssh-rsa';
key_alg('rsa-sha2-512') -> 'ssh-rsa';
key_alg(Alg) -> Alg.
