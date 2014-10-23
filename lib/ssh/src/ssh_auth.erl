%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2014. All Rights Reserved.
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
	{ok, Key} ->
	    StrAlgo = algorithm_string(Alg),
	    PubKeyBlob = encode_public_key(Key),
	    SigData = build_sig_data(SessionId, 
				     User, Service, PubKeyBlob, StrAlgo),
	    Sig = ssh_transport:sign(SigData, Hash, Key),
	    SigBlob = list_to_binary([?string(StrAlgo), ?binary(Sig)]),
	    ssh_transport:ssh_packet(
	      #ssh_msg_userauth_request{user = User,
					service = Service,
					method = "publickey",
					data = [?TRUE,
						?string(StrAlgo),
						?binary(PubKeyBlob),
						?binary(SigBlob)]},
	      Ssh);
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
	    case proplists:get_value(pref_public_key_algs, Opts, false) of
		false ->
		    FirstAlg = proplists:get_value(public_key_alg, Opts, ?PREFERRED_PK_ALG),
		    SecondAlg = other_alg(FirstAlg),
		    Prefs = method_preference(FirstAlg, SecondAlg),
		    ssh_transport:ssh_packet(Msg, Ssh#ssh{user = User,
							  userauth_preference = Prefs,
							  userauth_methods = none,
							  service = "ssh-connection"});
		Algs ->
		    FirstAlg = lists:nth(1, Algs),
		    case length(Algs) =:= 2 of
			true ->
			    SecondAlg = other_alg(FirstAlg),
			    Prefs = method_preference(FirstAlg, SecondAlg),
			    ssh_transport:ssh_packet(Msg, Ssh#ssh{user = User,
								  userauth_preference = Prefs,
								  userauth_methods = none,
								  service = "ssh-connection"});
			_ ->
			    Prefs = method_preference(FirstAlg),
			    ssh_transport:ssh_packet(Msg, Ssh#ssh{user = User,
								  userauth_preference = Prefs,
								  userauth_methods = none,
								  service = "ssh-connection"})
		    end
	    end;
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
						  data = <<?FALSE, ?UINT32(Sz), BinPwd:Sz/binary>>}, _, 
			#ssh{opts = Opts} = Ssh) ->
    Password = unicode:characters_to_list(BinPwd),
    case check_password(User, Password, Opts) of
	true ->
	    {authorized, User,
	     ssh_transport:ssh_packet(#ssh_msg_userauth_success{}, Ssh)};
	false  ->
	    {not_authorized, {User, {error,"Bad user or password"}}, 
	     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
		     authentications = "",
		     partial_success = false}, Ssh)}
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
			Ssh) ->
    %% Password change without us having sent SSH_MSG_USERAUTH_PASSWD_CHANGEREQ (because we never do)
    %% RFC 4252 says:
    %%   SSH_MSG_USERAUTH_FAILURE without partial success - The password
    %%   has not been changed.  Either password changing was not supported,
    %%   or the old password was bad. 

    {not_authorized, {User, {error,"Password change not supported"}}, 
     ssh_transport:ssh_packet(#ssh_msg_userauth_failure{
				 authentications = "",
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
			SessionId, #ssh{opts = Opts} = Ssh) ->
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
			     authentications="publickey,password",
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

handle_userauth_info_response(#ssh_msg_userauth_info_response{},
			      _Auth) ->
    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			      description = "Server does not support"
			      "keyboard-interactive",
			      language = "en"}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
method_preference(Alg1, Alg2) ->
    [{"publickey", ?MODULE, publickey_msg, [Alg1]},
     {"publickey", ?MODULE, publickey_msg,[Alg2]},
     {"password", ?MODULE, password_msg, []},
     {"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []}
    ].
method_preference(Alg1) ->
    [{"publickey", ?MODULE, publickey_msg, [Alg1]},
     {"password", ?MODULE, password_msg, []},
     {"keyboard-interactive", ?MODULE, keyboard_interactive_msg, []}
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

algorithm_string('ssh-rsa') ->
    "ssh-rsa";
algorithm_string('ssh-dss') ->
    "ssh-dss".

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
    [""];
keyboard_interact_get_responses(false, undefined, undefined, _, _, _, [Prompt|_], Opts, _) ->
    ssh_no_io:read_line(Prompt, Opts); %% Throws error as keyboard interaction is not allowed
keyboard_interact_get_responses(true, undefined, _,IoCb, Name, Instr, PromptInfos, Opts, _) ->
    keyboard_interact(IoCb, Name, Instr, PromptInfos, Opts);
keyboard_interact_get_responses(true, Fun, _, Name, Instr, PromptInfos, _, _, NumPrompts) ->
    keyboard_interact_fun(Fun, Name, Instr, PromptInfos, NumPrompts).

keyboard_interact(IoCb, Name, Instr, Prompts, Opts) ->
    if Name /= "" -> IoCb:format("~s", [Name]);
       true       -> ok
    end,
    if Instr /= "" -> IoCb:format("~s", [Instr]);
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

other_alg('ssh-rsa') ->
    'ssh-dss';
other_alg('ssh-dss') ->
    'ssh-rsa'.
decode_public_key_v2(<<?UINT32(Len0), _:Len0/binary,
		       ?UINT32(Len1), BinE:Len1/binary,
		       ?UINT32(Len2), BinN:Len2/binary>>
			 ,"ssh-rsa") ->
    E = ssh_bits:erlint(Len1, BinE),
    N = ssh_bits:erlint(Len2, BinN),
    {ok, #'RSAPublicKey'{publicExponent = E, modulus = N}};
decode_public_key_v2(<<?UINT32(Len0), _:Len0/binary,
		       ?UINT32(Len1), BinP:Len1/binary,
		       ?UINT32(Len2), BinQ:Len2/binary,
		       ?UINT32(Len3), BinG:Len3/binary,
		       ?UINT32(Len4), BinY:Len4/binary>>
			 , "ssh-dss") ->
    P = ssh_bits:erlint(Len1, BinP),
    Q = ssh_bits:erlint(Len2, BinQ),
    G = ssh_bits:erlint(Len3, BinG),
    Y = ssh_bits:erlint(Len4, BinY),
    {ok, {Y, #'Dss-Parms'{p = P, q = Q, g = G}}};

decode_public_key_v2(_, _) ->
    {error, bad_format}.

encode_public_key(#'RSAPrivateKey'{publicExponent = E, modulus = N}) ->
    ssh_bits:encode(["ssh-rsa",E,N], [string,mpint,mpint]);
encode_public_key(#'DSAPrivateKey'{p = P, q = Q, g = G, y = Y}) ->
    ssh_bits:encode(["ssh-dss",P,Q,G,Y], [string,mpint,mpint,mpint,mpint]).
