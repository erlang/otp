%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-module(ssh_auth).

-include("ssh.hrl").

-include("ssh_auth.hrl").
-include("ssh_transport.hrl").

-export([publickey_msg/1, password_msg/1, keyboard_interactive_msg/1,
	 service_request_msg/1, init_userauth_request_msg/1,
	 userauth_request_msg/1, handle_userauth_request/3,
	 handle_userauth_info_request/3, handle_userauth_info_response/2,
	 userauth_messages/0
	]).

%%--------------------------------------------------------------------
%%% Internal application API
%%--------------------------------------------------------------------
publickey_msg([Cb, #ssh{user = User,
		       session_id = SessionId,
		       service = Service,
		       opts = Opts} = Ssh]) ->
    ssh_bits:install_messages(userauth_pk_messages()),
    Alg = Cb:alg_name(),
    case ssh_file:private_identity_key(Alg, Opts) of
	{ok, PrivKey} ->
	    PubKeyBlob = ssh_file:encode_public_key(PrivKey),
	    SigData = build_sig_data(SessionId, 
				     User, Service, Alg, PubKeyBlob),
	    Sig = Cb:sign(PrivKey, SigData),
	    SigBlob = list_to_binary([?string(Alg), ?binary(Sig)]),
	    ssh_transport:ssh_packet(
	      #ssh_msg_userauth_request{user = User,
					service = Service,
					method = "publickey",
					data = [?TRUE,
						?string(Alg),
						?binary(PubKeyBlob),
						?binary(SigBlob)]},
	      Ssh);
	_Error ->
	  not_ok
    end.

password_msg([#ssh{opts = Opts, io_cb = IoCb,
		   user = User, service = Service} = Ssh]) ->
    ssh_bits:install_messages(userauth_passwd_messages()),
    Password = case proplists:get_value(password, Opts) of
		   undefined -> 
		       IoCb:read_password("ssh password: ");
		   PW -> 
		       PW
	       end,
    ssh_transport:ssh_packet(
      #ssh_msg_userauth_request{user = User,
				service = Service,
				method = "password",
				data =
				<<?BOOLEAN(?FALSE),
				 ?STRING(list_to_binary(Password))>>},
      Ssh).

%% See RFC 4256 for info on keyboard-interactive
keyboard_interactive_msg([#ssh{user = User,
			      service = Service} = Ssh]) ->
    ssh_bits:install_messages(userauth_keyboard_interactive_messages()),
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
	    CbFirst = proplists:get_value(public_key_alg, Opts, 
					  ?PREFERRED_PK_ALG),
	    CbSecond = other_cb(CbFirst),
	    AllowUserInt =  proplists:get_value(allow_user_interaction, Opts,
						true),
	    Prefs = method_preference(CbFirst, CbSecond, AllowUserInt),
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
		    Result
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
						  data = Data}, _, 
			#ssh{opts = Opts} = Ssh) ->
    <<_:8, ?UINT32(Sz), BinPwd:Sz/binary>> = Data,
    Password = binary_to_list(BinPwd),
    
    case check_password(User, Password, Opts) of
	true ->
	    {authorized, User,
	     ssh_transport:ssh_packet(#ssh_msg_userauth_success{}, Ssh)};
	false  ->
	    {not_authorized, {User, {passwd, Password}}, 
	     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
		     authentications = "",
		     partial_success = false}, Ssh)}
    end;

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
			SessionId, #ssh{opts = Opts} = Ssh) ->
    <<?BYTE(HaveSig), ?UINT32(ALen), BAlg:ALen/binary, 
     ?UINT32(KLen), KeyBlob:KLen/binary, SigWLen/binary>> = Data,
    Alg = binary_to_list(BAlg),
    case HaveSig of
	?TRUE ->
	    case verify_sig(SessionId, User, "ssh-connection", Alg,
			    KeyBlob, SigWLen, Opts) of
		ok ->
		    {authorized, User, 
		     ssh_transport:ssh_packet(
		       #ssh_msg_userauth_success{}, Ssh)};
		{error, Reason} ->
		    {not_authorized, {User, {error, Reason}}, 
		     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
			     authentications="publickey,password",
			     partial_success = false}, Ssh)}
	    end;
	?FALSE ->
	    ssh_bits:install_messages(userauth_pk_messages()),
	    {not_authorized, {User, undefined},
	     ssh_transport:ssh_packet(
	       #ssh_msg_userauth_pk_ok{algorithm_name = Alg,
				       key_blob = KeyBlob}, Ssh)}
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
    Resps = keyboard_interact_get_responses(IoCb, Opts,
					    Name, Instr, PromptInfos),
    %%?dbg(true, "keyboard_interactive_reply: resps=~n#~p ~n", [Resps]),
    RespBin = list_to_binary(
		lists:map(fun(S) -> <<?STRING(list_to_binary(S))>> end,
			  Resps)),
    {ok, 
     ssh_transport:ssh_packet(
       #ssh_msg_userauth_info_response{num_responses = NumPrompts,
				       data = RespBin}, Ssh)}.

handle_userauth_info_response(#ssh_msg_userauth_info_response{},
			      _Auth) ->
    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			      description = "Server does not support" 
			      "keyboard-interactive", 
			      language = "en"}).
userauth_messages() ->
    [ {ssh_msg_userauth_request, ?SSH_MSG_USERAUTH_REQUEST,
       [string, 
	string, 
	string, 
	'...']},

      {ssh_msg_userauth_failure, ?SSH_MSG_USERAUTH_FAILURE,
       [string, 
	boolean]},

      {ssh_msg_userauth_success, ?SSH_MSG_USERAUTH_SUCCESS,
       []},

      {ssh_msg_userauth_banner, ?SSH_MSG_USERAUTH_BANNER,
       [string, 
	string]}].
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
method_preference(Callback1, Callback2, true) ->
    [{"publickey", ?MODULE, publickey_msg, [Callback1]},
     {"publickey", ?MODULE, publickey_msg,[Callback2]},
     {"password", ?MODULE, password_msg, []},
     {"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []}
    ];
method_preference(Callback1, Callback2, false) ->
    [{"publickey", ?MODULE, publickey_msg, [Callback1]},
     {"publickey", ?MODULE, publickey_msg,[Callback2]},
     {"password", ?MODULE, password_msg, []}
    ].

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

check_password(User, Password, Opts) ->
    %%?dbg(true, " ~p ~p ~p ~n", [User, Password, Opts]),
    case proplists:get_value(pwdfun, Opts) of
	undefined ->
	    Static = get_password_option(Opts, User),
	    Password == Static;
	Cheker ->
	    Cheker(User, Password)
    end.

get_password_option(Opts, User) ->
    Passwords = proplists:get_value(user_passwords, Opts, []),
    case lists:keysearch(User, 1, Passwords) of
	{value, {User, Pw}} -> Pw;
	false -> proplists:get_value(password, Opts, false)
    end.
	    
verify_sig(SessionId, User, Service, Alg, KeyBlob, SigWLen, Opts) ->
    case ssh_file:lookup_user_key(User, Alg, Opts) of
	{ok, OurKey} ->
	    {ok, Key} = ssh_file:decode_public_key_v2(KeyBlob, Alg),
	    case OurKey of
		Key ->
		    NewSig = build_sig_data(SessionId, 
					    User, Service, Alg, KeyBlob),
		    <<?UINT32(AlgSigLen), AlgSig:AlgSigLen/binary>> = SigWLen,
		    <<?UINT32(AlgLen), _Alg:AlgLen/binary,
		     ?UINT32(SigLen), Sig:SigLen/binary>> = AlgSig,
		    M = alg_to_module(Alg),
		    M:verify(OurKey, NewSig, Sig);
		_ ->
		    {error, key_unacceptable}
	    end;
	Error -> Error
    end.

build_sig_data(SessionId, User, Service, Alg, KeyBlob) ->
    Sig = [?binary(SessionId),
	   ?SSH_MSG_USERAUTH_REQUEST,
	   ?string(User),
	   ?string(Service),
	   ?binary(<<"publickey">>),
	   ?TRUE,
	   ?string(Alg),
	   ?binary(KeyBlob)],
    list_to_binary(Sig).

decode_keyboard_interactive_prompts(NumPrompts, Data) ->
    Types = lists:append(lists:duplicate(NumPrompts, [string, boolean])),
    pairwise_tuplify(ssh_bits:decode(Data, Types)).

pairwise_tuplify([E1, E2 | Rest]) -> [{E1, E2} | pairwise_tuplify(Rest)];
pairwise_tuplify([])              -> [].
    

keyboard_interact_get_responses(IoCb, Opts, Name, Instr, PromptInfos) ->
    NumPrompts = length(PromptInfos),
    case proplists:get_value(keyboard_interact_fun, Opts) of
	undefined when NumPrompts == 1 ->
	    %% Special case/fallback for just one prompt
	    %% (assumed to be the password prompt)
	    case proplists:get_value(password, Opts) of
		undefined -> keyboard_interact(IoCb, Name, Instr, PromptInfos);
		PW        -> [PW]
	    end;
	undefined ->
	    keyboard_interact(IoCb, Name, Instr, PromptInfos);
	KbdInteractFun ->
	    Prompts = lists:map(fun({Prompt, _Echo}) -> Prompt end,
				PromptInfos),
	    case KbdInteractFun(Name, Instr, Prompts) of
		Rs when length(Rs) == NumPrompts ->
		    Rs;
		Rs ->
		    erlang:error({mismatching_number_of_responses,
				  {got,Rs},
				  {expected,NumPrompts}})
	    end
    end.

keyboard_interact(IoCb, Name, Instr, Prompts) ->
    if Name /= "" -> IoCb:format("~s", [Name]);
       true       -> ok
    end,
    if Instr /= "" -> IoCb:format("~s", [Instr]);
       true        -> ok
    end,
    lists:map(fun({Prompt, true})  -> IoCb:read_line(Prompt);
		 ({Prompt, false}) -> IoCb:read_password(Prompt)
	      end,
	      Prompts).

userauth_passwd_messages() ->
    [ 
      {ssh_msg_userauth_passwd_changereq, ?SSH_MSG_USERAUTH_PASSWD_CHANGEREQ,
       [string, 
	string]}
     ].

userauth_keyboard_interactive_messages() ->
    [ {ssh_msg_userauth_info_request, ?SSH_MSG_USERAUTH_INFO_REQUEST,
       [string, 
	string,
	string,
	uint32,
	'...']},

      {ssh_msg_userauth_info_response, ?SSH_MSG_USERAUTH_INFO_RESPONSE,
       [uint32, 
	'...']}
     ].

userauth_pk_messages() ->
    [ {ssh_msg_userauth_pk_ok, ?SSH_MSG_USERAUTH_PK_OK,
       [string, % algorithm name
	binary]} % key blob
     ].

alg_to_module("ssh-dss") ->
    ssh_dsa;
alg_to_module("ssh-rsa") ->
    ssh_rsa.

other_cb(ssh_rsa) -> 
    ssh_dsa;
other_cb(ssh_dsa) -> 
    ssh_rsa.
