%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2014. All Rights Reserved.
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
	 handle_userauth_info_request/3, handle_userauth_info_response/2
	]).

%%--------------------------------------------------------------------
%%% Internal application API
%%--------------------------------------------------------------------
publickey_msg([Alg, #ssh{user = User,
		       session_id = SessionId,
		       service = Service,
		       opts = Opts} = Ssh]) ->
    Hash = sha, %% Maybe option?!
    KeyCb = proplists:get_value(key_cb, Opts, ssh_file),
    case KeyCb:user_key(Alg, Opts) of
	{ok, PrivKey} ->
	    StrAlgo = atom_to_list(Alg),
            case encode_public_key(StrAlgo, ssh_transport:extract_public_key(PrivKey)) of
		not_ok ->
		    not_ok;
		PubKeyBlob ->
		    SigData = build_sig_data(SessionId, 
					     User, Service, PubKeyBlob, StrAlgo),
		    Sig = ssh_transport:sign(SigData, Hash, PrivKey),
		    SigBlob = list_to_binary([?string(StrAlgo), ?binary(Sig)]),
		    ssh_transport:ssh_packet(
		      #ssh_msg_userauth_request{user = User,
						service = Service,
						method = "publickey",
						data = [?TRUE,
							?string(StrAlgo),
							?binary(PubKeyBlob),
							?binary(SigBlob)]},
		      Ssh)
	    end;
     	_Error ->
	    not_ok
    end.

password_msg([#ssh{opts = Opts, io_cb = IoCb,
		   user = User, service = Service} = Ssh]) ->
    Password = case proplists:get_value(password, Opts) of
		   undefined -> 
		       user_interaction(IoCb, Ssh);
		   PW -> 
		       PW
	       end,
    case Password of
	not_ok ->
	    not_ok;
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

user_interaction(ssh_no_io, _) ->
    not_ok;
user_interaction(IoCb, Ssh) ->
    IoCb:read_password("ssh password: ", Ssh).


%% See RFC 4256 for info on keyboard-interactive
keyboard_interactive_msg([#ssh{user = User,
			       service = Service} = Ssh]) ->
    ssh_transport:ssh_packet(
      #ssh_msg_userauth_request{user = User,
				service = Service,
				method = "keyboard-interactive",
				data = << ?STRING(<<"">>),
					  ?STRING(<<>>) >> },
      Ssh).

service_request_msg(Ssh) ->
    ssh_transport:ssh_packet(#ssh_msg_service_request{name = "ssh-userauth"},
			   Ssh#ssh{service = "ssh-userauth"}).

init_userauth_request_msg(#ssh{opts = Opts} = Ssh) ->
    case user_name(Opts) of
	{ok, User} ->
	    Msg = #ssh_msg_userauth_request{user = User,
					    service = "ssh-connection",
					    method = "none",
					    data = <<>>},
	    Algs0 = proplists:get_value(pref_public_key_algs, Opts, ?SUPPORTED_USER_KEYS),
	    %% The following line is not strictly correct. The call returns the
	    %% supported HOST key types while we are interested in USER keys. However,
	    %% they "happens" to be the same (for now).  This could change....
	    %% There is no danger as long as the set of user keys is a subset of the set
	    %% of host keys.
	    CryptoSupported = ssh_transport:supported_algorithms(public_key),
	    Algs = [A || A <- Algs0,
			 lists:member(A, CryptoSupported)],

	    Prefs = method_preference(Algs),
	    ssh_transport:ssh_packet(Msg, Ssh#ssh{user = User,
						  userauth_preference = Prefs,
						  userauth_methods = none,
						  service = "ssh-connection"});
	{error, no_user} ->
	    ErrStr = "Could not determine the users name",
	    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_ILLEGAL_USER_NAME,
				      description = ErrStr,
				      language = "en"})
    end.

userauth_request_msg(#ssh{userauth_preference = []} = Ssh) ->    
    Msg = #ssh_msg_disconnect{code = 
			      ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
			      description = "Unable to connect using the available"
			      " authentication methods",
			      language = "en"},
    {disconnect, Msg, ssh_transport:ssh_packet(Msg, Ssh)};

userauth_request_msg(#ssh{userauth_methods = Methods, 
			  userauth_preference = [{Pref, Module,
					      Function, Args} | Prefs]} 
		     = Ssh0) ->
    Ssh = Ssh0#ssh{userauth_preference = Prefs},
    case lists:member(Pref, Methods) of
	true ->
	    case Module:Function(Args ++ [Ssh]) of
		not_ok ->
		    userauth_request_msg(Ssh);
		Result ->
		    {Pref,Result}
	    end;
	false ->
	    userauth_request_msg(Ssh)
    end.


handle_userauth_request(#ssh_msg_service_request{name = 
						 Name = "ssh-userauth"},
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
						  data = Data}, 
			SessionId, 
			#ssh{opts = Opts,
			     userauth_supported_methods = Methods} = Ssh) ->
    <<?BYTE(HaveSig), ?UINT32(ALen), BAlg:ALen/binary, 
     ?UINT32(KLen), KeyBlob:KLen/binary, SigWLen/binary>> = Data,
    Alg = binary_to_list(BAlg),
    case HaveSig of
	?TRUE ->
	    case verify_sig(SessionId, User, "ssh-connection", Alg,
			    KeyBlob, SigWLen, Opts) of
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
	?FALSE ->
	    {not_authorized, {User, undefined},
	     ssh_transport:ssh_packet(
	       #ssh_msg_userauth_pk_ok{algorithm_name = Alg,
				       key_blob = KeyBlob}, Ssh)}
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
		case proplists:get_value(auth_method_kb_interactive_data, Opts) of
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



handle_userauth_info_request(
  #ssh_msg_userauth_info_request{name = Name,
				 instruction = Instr,
				 num_prompts = NumPrompts,
				 data  = Data}, IoCb, 
  #ssh{opts = Opts} = Ssh) ->
    PromptInfos = decode_keyboard_interactive_prompts(NumPrompts,Data),
    Responses = keyboard_interact_get_responses(IoCb, Opts,
					    Name, Instr, PromptInfos),
    {ok, 
     ssh_transport:ssh_packet(
       #ssh_msg_userauth_info_response{num_responses = NumPrompts,
				       data = Responses}, Ssh)}.

handle_userauth_info_response(#ssh_msg_userauth_info_response{num_responses = 1,
							      data = <<?UINT32(Sz), Password:Sz/binary>>},
			      #ssh{opts = Opts,
				   kb_tries_left = KbTriesLeft,
				   user = User,
				   userauth_supported_methods = Methods} = Ssh) ->
    case check_password(User, unicode:characters_to_list(Password), Opts, Ssh) of
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

handle_userauth_info_response(#ssh_msg_userauth_info_response{},
			      _Auth) ->
    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			      description = "Server does not support"
			      "keyboard-interactive",
			      language = "en"}).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
method_preference(Algs) ->
    lists:foldr(fun(A, Acc) ->
		       [{"publickey", ?MODULE, publickey_msg, [A]} | Acc]
	       end, 
	       [{"password", ?MODULE, password_msg, []},
		{"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []},
		{"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []},
		{"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []},
		{"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []},
		{"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []},
		{"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []}
	       ],
	       Algs).

user_name(Opts) ->
    Env = case os:type() of
	      {win32, _} -> 
		  "USERNAME";
	      {unix, _} -> 
		  "LOGNAME"
	  end,
    case proplists:get_value(user, Opts, os:getenv(Env)) of
	false ->
	    case os:getenv("USER") of
		false -> 
		    {error, no_user};
		User -> 
		    {ok, User}
	    end;
	User ->
	    {ok, User}
    end.

check_password(User, Password, Opts, Ssh) ->
    case proplists:get_value(pwdfun, Opts) of
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
		    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
					      description = 
						  "Unable to connect using the available authentication methods",
					      language = ""})
	    end
    end.

get_password_option(Opts, User) ->
    Passwords = proplists:get_value(user_passwords, Opts, []),
    case lists:keysearch(User, 1, Passwords) of
	{value, {User, Pw}} -> Pw;
	false -> proplists:get_value(password, Opts, false)
    end.
	    
verify_sig(SessionId, User, Service, Alg, KeyBlob, SigWLen, Opts) ->
    {ok, Key} = decode_public_key_v2(KeyBlob, Alg),
    KeyCb =  proplists:get_value(key_cb, Opts, ssh_file),

    case KeyCb:is_auth_key(Key, User, Opts) of
	true ->
	    PlainText = build_sig_data(SessionId, User,
				       Service, KeyBlob, Alg),
	    <<?UINT32(AlgSigLen), AlgSig:AlgSigLen/binary>> = SigWLen,
	    <<?UINT32(AlgLen), _Alg:AlgLen/binary,
	      ?UINT32(SigLen), Sig:SigLen/binary>> = AlgSig,
	    ssh_transport:verify(PlainText, sha, Sig, Key);
	false ->
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
    keyboard_interact_get_responses(proplists:get_value(user_interaction, Opts, true),
				    proplists:get_value(keyboard_interact_fun, Opts),
				    proplists:get_value(password, Opts, undefined), IoCb, Name,
				    Instr, PromptInfos, Opts, NumPrompts).

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
    if Name /= "" -> IoCb:format("~s~n", [Name]);
       true       -> ok
    end,
    if Instr /= "" -> IoCb:format("~s~n", [Instr]);
       true        -> ok
    end,
    lists:map(fun({Prompt, true})  -> IoCb:read_line(Prompt, Opts);
		 ({Prompt, false}) -> IoCb:read_password(Prompt, Opts)
	      end,
	      Prompts).

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

decode_public_key_v2(Bin, _Type) ->
    try 
	public_key:ssh_decode(Bin, ssh2_pubkey)
    of
	Key -> {ok, Key}
    catch
	_:_ -> {error, bad_format}
    end.

encode_public_key(_Alg, Key) ->
    try
	public_key:ssh_encode(Key, ssh2_pubkey)
    catch
	_:_ -> not_ok
    end.
