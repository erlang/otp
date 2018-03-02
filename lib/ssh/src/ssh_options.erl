%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

-module(ssh_options).

-include("ssh.hrl").
-include_lib("kernel/include/file.hrl").

-export([default/1,
         get_value/5,  get_value/6,
         put_value/5,
         delete_key/5,
         handle_options/2
        ]).

-export_type([options/0
             ]).

%%%================================================================
%%% Types

-type option_in() :: proplists:property() | proplists:proplist() .

-type option_class() :: internal_options | socket_options | user_options . 

-type option_declaration() :: #{class := user_options,
                                chk := fun((any) -> boolean() | {true,any()}),
                                default => any()
                               }.

-type option_declarations() :: #{ {option_key(),def} := option_declaration() }.

-type error() :: {error,{eoptions,any()}} .

%%%================================================================
%%%
%%% Get an option
%%%

-spec get_value(option_class(), option_key(), options(),
                atom(), non_neg_integer()) -> any() | no_return().

get_value(Class, Key, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    case Class of
        internal_options -> maps:get(Key, maps:get(internal_options,Opts));
        socket_options   -> proplists:get_value(Key, maps:get(socket_options,Opts));
        user_options     -> maps:get(Key, Opts)
    end;
get_value(Class, Key, Opts, _CallerMod, _CallerLine) ->
    error({bad_options,Class, Key, Opts, _CallerMod, _CallerLine}).


-spec get_value(option_class(), option_key(), options(), fun(() -> any()),
                atom(), non_neg_integer()) -> any() | no_return().

get_value(socket_options, Key, Opts, DefFun, _CallerMod, _CallerLine) when is_map(Opts) ->
    proplists:get_value(Key, maps:get(socket_options,Opts), DefFun);
get_value(Class, Key, Opts, DefFun, CallerMod, CallerLine) when is_map(Opts) ->
    try get_value(Class, Key, Opts, CallerMod, CallerLine)
    of
        undefined -> DefFun();
        Value -> Value
    catch
        error:{badkey,Key} -> DefFun()
    end;
get_value(Class, Key, Opts, _DefFun, _CallerMod, _CallerLine) ->
    error({bad_options,Class, Key, Opts, _CallerMod, _CallerLine}).


%%%================================================================
%%%
%%% Put an option
%%%

-spec put_value(option_class(), option_in(), options(),
                atom(), non_neg_integer()) -> options().

put_value(user_options, KeyVal, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    put_user_value(KeyVal, Opts);

put_value(internal_options, KeyVal, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    InternalOpts = maps:get(internal_options,Opts),
    Opts#{internal_options := put_internal_value(KeyVal, InternalOpts)};

put_value(socket_options, KeyVal, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    SocketOpts = maps:get(socket_options,Opts),
    Opts#{socket_options := put_socket_value(KeyVal, SocketOpts)}.


%%%----------------
put_user_value(L, Opts) when is_list(L) ->
    lists:foldl(fun put_user_value/2, Opts, L);
put_user_value({Key,Value}, Opts) ->
    Opts#{Key := Value}.
    
%%%----------------
put_internal_value(L, IntOpts) when is_list(L) ->
    lists:foldl(fun put_internal_value/2, IntOpts, L);
put_internal_value({Key,Value}, IntOpts) ->
    IntOpts#{Key => Value}.

%%%----------------
put_socket_value(L, SockOpts) when is_list(L) ->
    L ++ SockOpts;
put_socket_value({Key,Value}, SockOpts) ->
    [{Key,Value} | SockOpts];
put_socket_value(A, SockOpts) when is_atom(A) ->
    [A | SockOpts].

%%%================================================================
%%%
%%% Delete an option
%%%

-spec delete_key(option_class(), option_key(), options(),
                 atom(), non_neg_integer()) -> options().

delete_key(internal_options, Key, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    InternalOpts = maps:get(internal_options,Opts),
    Opts#{internal_options := maps:remove(Key, InternalOpts)}.
        

%%%================================================================
%%%
%%% Initialize the options
%%%

-spec handle_options(role(), proplists:proplist()) -> options() | error() .

-spec handle_options(role(), proplists:proplist(), options()) -> options() | error() .

handle_options(Role, PropList0) ->
    handle_options(Role, PropList0, #{socket_options   => [],
                                      internal_options => #{},
                                      user_options     => []
                                     }).

handle_options(Role, PropList0, Opts0) when is_map(Opts0),
                                             is_list(PropList0) ->
    PropList1 = proplists:unfold(PropList0), 
    try
        OptionDefinitions = default(Role),
        InitialMap =
            maps:fold(
              fun({K,def}, #{default:=V}, M) -> M#{K=>V};
                 (_,_,M) -> M
              end,
              Opts0#{user_options => 
                         maps:get(user_options,Opts0) ++ PropList1
                   },
              OptionDefinitions),
        %% Enter the user's values into the map; unknown keys are
        %% treated as socket options
        final_preferred_algorithms(
          lists:foldl(fun(KV, Vals) ->
                              save(KV, OptionDefinitions, Vals)
                      end, InitialMap, PropList1))
    catch
        error:{eoptions, KV, undefined} -> 
            {error, {eoptions,KV}};

        error:{eoptions, KV, Txt} when is_list(Txt) -> 
            {error, {eoptions,{KV,lists:flatten(Txt)}}};

        error:{eoptions, KV, Extra} ->
            {error, {eoptions,{KV,Extra}}}
    end.


check_fun(Key, Defs) ->
    #{chk := Fun} = maps:get({Key,def}, Defs),
    Fun.

%%%================================================================
%%%
%%% Check and save one option
%%%


%%% First some prohibited inet options:
save({K,V}, _, _) when K == reuseaddr ;
                       K == active
                       ->
    forbidden_option(K, V);

%%% then compatibility conversions:
save({allow_user_interaction,V}, Opts, Vals) ->
    save({user_interaction,V}, Opts, Vals);

%% Special case for socket options 'inet' and 'inet6'
save(Inet, Defs, OptMap) when Inet==inet ; Inet==inet6 ->
    save({inet,Inet}, Defs, OptMap);

%% Two clauses to prepare for a proplists:unfold
save({Inet,true}, Defs, OptMap) when Inet==inet ; Inet==inet6 ->  save({inet,Inet}, Defs, OptMap);
save({Inet,false}, _Defs, OptMap) when Inet==inet ; Inet==inet6 -> OptMap;

%% and finaly the 'real stuff':
save({Key,Value}, Defs, OptMap) when is_map(OptMap) ->
    try (check_fun(Key,Defs))(Value)
    of
        true ->
            OptMap#{Key := Value};
        {true, ModifiedValue} ->
            OptMap#{Key := ModifiedValue};
        false ->
            error({eoptions, {Key,Value}, "Bad value"})
    catch
        %% An unknown Key (= not in the definition map) is
        %% regarded as an inet option:
        error:{badkey,{inet,def}} ->
            %% atomic (= non-tuple) options 'inet' and 'inet6':
            OptMap#{socket_options := [Value | maps:get(socket_options,OptMap)]};
        error:{badkey,{Key,def}} ->
            OptMap#{socket_options := [{Key,Value} | maps:get(socket_options,OptMap)]};

        %% But a Key that is known but the value does not validate
        %% by the check fun will give an error exception:
        error:{check,{BadValue,Extra}} ->
            error({eoptions, {Key,BadValue}, Extra})
    end;
save(Opt, _Defs, OptMap) when is_map(OptMap) ->
    OptMap#{socket_options := [Opt | maps:get(socket_options,OptMap)]}.


%%%================================================================
%%%
%%% Default options
%%%

-spec default(role() | common) -> option_declarations() .

default(server) ->
    (default(common))
        #{
      {subsystems, def} =>
          #{default => [ssh_sftpd:subsystem_spec([])],
            chk => fun(L) ->
                           is_list(L) andalso
                               lists:all(fun({Name,{CB,Args}}) ->
                                                 check_string(Name) andalso
                                                     is_atom(CB) andalso
                                                     is_list(Args);
                                            (_) ->
                                                 false
                                         end, L)
                   end,
            class => user_options
           },

      {shell, def} =>
          #{default => ?DEFAULT_SHELL,
            chk => fun({M,F,A}) -> is_atom(M) andalso is_atom(F) andalso is_list(A);
                      (V) -> check_function1(V) orelse check_function2(V)
                   end,
            class => user_options
           },

      {exec, def} =>
          #{default => undefined,
            chk => fun({direct, V}) ->  check_function1(V) orelse check_function2(V) orelse check_function3(V);
                      %% Compatibility (undocumented):
                      ({M,F,A}) -> is_atom(M) andalso is_atom(F) andalso is_list(A);
                      (V) -> check_function1(V) orelse check_function2(V) orelse check_function3(V)
                   end,
            class => user_options
           },

      {ssh_cli, def} =>
          #{default => undefined,
            chk => fun({Cb, As}) -> is_atom(Cb) andalso is_list(As);
                      (V) -> V == no_cli
                   end,
            class => user_options
           },

      {system_dir, def} =>
          #{default => "/etc/ssh",
            chk => fun(V) -> check_string(V) andalso check_dir(V) end,
            class => user_options
           },

      {auth_method_kb_interactive_data, def} =>
          #{default => undefined, % Default value can be constructed when User is known
            chk => fun({S1,S2,S3,B}) ->
                           check_string(S1) andalso
                               check_string(S2) andalso
                               check_string(S3) andalso
                               is_boolean(B);
                      (F) ->
                           check_function3(F)
                   end,
            class => user_options
           },

      {user_passwords, def} =>
          #{default => [],
            chk => fun(V) ->
                           is_list(V) andalso
                               lists:all(fun({S1,S2}) ->
                                                 check_string(S1) andalso 
                                                     check_string(S2)   
                                         end, V)
                   end,
            class => user_options
           },

      {password, def} =>
          #{default => undefined,
            chk => fun check_string/1,
            class => user_options
           },

      {dh_gex_groups, def} =>
          #{default => undefined,
            chk => fun check_dh_gex_groups/1,
            class => user_options
           },

      {dh_gex_limits, def} =>
          #{default => {0, infinity},
            chk => fun({I1,I2}) ->
                           check_pos_integer(I1) andalso
                               check_pos_integer(I2) andalso
                               I1 < I2;
                      (_) ->
                           false
                   end,
            class => user_options
           },

      {pwdfun, def} =>
          #{default => undefined,
            chk => fun(V) -> check_function4(V) orelse check_function2(V) end,
            class => user_options
           },

      {negotiation_timeout, def} =>
          #{default => 2*60*1000,
            chk => fun check_timeout/1,
            class => user_options
           },

      {max_sessions, def} =>
          #{default => infinity,
            chk => fun check_pos_integer/1,
            class => user_options
           },

      {max_channels, def} =>
          #{default => infinity,
            chk => fun check_pos_integer/1,
            class => user_options
           },

      {parallel_login, def} =>
          #{default => false,
            chk => fun erlang:is_boolean/1,
            class => user_options
           },

      {minimal_remote_max_packet_size, def} =>
          #{default => 0,
            chk => fun check_pos_integer/1,
            class => user_options
           },

      {failfun, def} =>
          #{default => fun(_,_,_) -> void end,
            chk => fun(V) -> check_function3(V) orelse
                                 check_function2(V) % Backwards compatibility
                   end,
            class => user_options
           },

      {connectfun, def} =>
          #{default => fun(_,_,_) -> void end,
            chk => fun check_function3/1,
            class => user_options
           },

%%%%% Undocumented
      {infofun, def} =>
          #{default => fun(_,_,_) -> void end,
            chk => fun(V) -> check_function3(V) orelse
                                 check_function2(V) % Backwards compatibility
                   end,
            class => user_options
           }
     };

default(client) ->
    (default(common))
        #{
      {dsa_pass_phrase, def} =>
          #{default => undefined,
            chk => fun check_string/1,
            class => user_options
           },

      {rsa_pass_phrase, def} =>
          #{default => undefined,
            chk => fun check_string/1,
            class => user_options
           },

      {ecdsa_pass_phrase, def} =>
          #{default => undefined,
            chk => fun check_string/1,
            class => user_options
           },

      {silently_accept_hosts, def} =>
          #{default => false,
            chk => fun check_silently_accept_hosts/1,
            class => user_options
           },

      {user_interaction, def} =>
          #{default => true,
            chk => fun erlang:is_boolean/1,
            class => user_options
           },

      {save_accepted_host, def} =>
          #{default => true,
            chk => fun erlang:is_boolean/1,
            class => user_options
           },

      {pref_public_key_algs, def} =>
          #{default => ssh_transport:default_algorithms(public_key),
            chk => fun check_pref_public_key_algs/1,
            class => user_options
           },

      {dh_gex_limits, def} =>
          #{default => {1024, 6144, 8192},      % FIXME: Is this true nowadays?
            chk => fun({Min,I,Max}) ->
                           lists:all(fun check_pos_integer/1,
                                     [Min,I,Max]);
                      (_) -> false
                   end,
            class => user_options
           },

      {connect_timeout, def} =>
          #{default => infinity,
            chk => fun check_timeout/1,
            class => user_options
           },

      {user, def} =>
          #{default => 
                begin
                    Env = case os:type() of
                              {win32, _} -> "USERNAME";
                              {unix, _} -> "LOGNAME"
                          end,
                    case os:getenv(Env) of
                        false ->
                            case os:getenv("USER") of
                                false -> undefined;
                                User -> User
                            end;
                        User ->
                            User
                    end
                end,
            chk => fun check_string/1,
            class => user_options
           },

      {password, def} =>
          #{default => undefined,
            chk => fun check_string/1,
            class => user_options
           },

      {quiet_mode, def} =>
          #{default => false,
            chk => fun erlang:is_boolean/1,
            class => user_options
           },

%%%%% Undocumented
      {keyboard_interact_fun, def} =>
          #{default => undefined,
            chk => fun check_function3/1,
            class => user_options
           }
     };

default(common) ->
    #{
       {user_dir, def} =>
           #{default => false, % FIXME: TBD ~/.ssh at time of call when user is known
             chk => fun(V) -> check_string(V) andalso check_dir(V) end,
             class => user_options
            },

       {preferred_algorithms, def} =>
           #{default => ssh:default_algorithms(),
             chk => fun check_preferred_algorithms/1,
             class => user_options
            },

       %% NOTE: This option is supposed to be used only in this very module (?MODULE). There is
       %% a final stage in handle_options that "merges" the preferred_algorithms option and this one.
       %% The preferred_algorithms is the one to use in the rest of the ssh application!
       {modify_algorithms, def} =>
           #{default => undefined, % signals error if unsupported algo in preferred_algorithms :(
             chk => fun check_modify_algorithms/1,
             class => user_options
            },

       {id_string, def} => 
           #{default => undefined, % FIXME: see ssh_transport:ssh_vsn/0
             chk => fun(random) -> 
                            {true, {random,2,5}}; % 2 - 5 random characters
                       ({random,I1,I2}) -> 
                            %% Undocumented
                            check_pos_integer(I1) andalso
                                check_pos_integer(I2) andalso
                                I1=<I2;
                       (V) ->
                            check_string(V)
                    end,
             class => user_options
            },

       {key_cb, def} =>
           #{default => {ssh_file, []},
             chk => fun({Mod,Opts}) -> is_atom(Mod) andalso is_list(Opts);
                       (Mod) when is_atom(Mod) -> {true, {Mod,[]}};
                       (_) -> false
                    end,
             class => user_options
            },

       {profile, def} =>
           #{default => ?DEFAULT_PROFILE,
             chk => fun erlang:is_atom/1,
             class => user_options
            },

      {idle_time, def} =>
          #{default => infinity,
            chk => fun check_timeout/1,
            class => user_options
           },

       %% This is a "SocketOption"...
       %% {fd, def} =>
       %%     #{default => undefined,
       %%       chk => fun erlang:is_integer/1,
       %%       class => user_options
       %%      },

       {disconnectfun, def} =>
           #{default => fun(_) -> void end,
             chk => fun check_function1/1,
             class => user_options
            },

       {unexpectedfun, def} => 
           #{default => fun(_,_) -> report end,
             chk => fun check_function2/1,
             class => user_options
            },

       {ssh_msg_debug_fun, def} =>
           #{default => fun(_,_,_,_) -> void end,
             chk => fun check_function4/1,
             class => user_options
            },

      {rekey_limit, def} =>                     % FIXME: Why not common?
          #{default => 1024000000,
            chk => fun check_non_neg_integer/1,
            class => user_options
           },

      {auth_methods, def} =>
          #{default => ?SUPPORTED_AUTH_METHODS,
            chk => fun(As) ->
                           try
                               Sup = string:tokens(?SUPPORTED_AUTH_METHODS, ","),
                               New = string:tokens(As, ","),
                               [] == [X || X <- New,
                                           not lists:member(X,Sup)]
                           catch
                               _:_ -> false
                           end
                   end,
            class => user_options
           },

%%%%% Undocumented
       {transport, def} =>
           #{default => ?DEFAULT_TRANSPORT,
             chk => fun({A,B,C}) ->
                            is_atom(A) andalso is_atom(B) andalso is_atom(C)
                    end,
             class => user_options
            },

       {vsn, def} =>
           #{default => {2,0},
             chk => fun({Maj,Min}) -> check_non_neg_integer(Maj) andalso check_non_neg_integer(Min);
                       (_) -> false
                    end,
             class => user_options
            },
    
       {tstflg, def} =>
           #{default => [],
             chk => fun erlang:is_list/1,
             class => user_options
            },

       {user_dir_fun, def} =>
           #{default => undefined,
             chk => fun check_function1/1,
             class => user_options
            },

       {max_random_length_padding, def} =>
           #{default => ?MAX_RND_PADDING_LEN,
             chk => fun check_non_neg_integer/1,
             class => user_options
            },

       {send_ext_info, def} =>
           #{default => true,
             chk => fun erlang:is_boolean/1,
             class => user_options
            },

       {recv_ext_info, def} =>
           #{default => true,
             chk => fun erlang:is_boolean/1,
             class => user_options
            }
     }.


%%%================================================================
%%%================================================================
%%%================================================================

%%%
%%% check_*/1 -> true | false | error({check,Spec})
%%% See error_in_check/2,3
%%%

%%% error_in_check(BadValue) -> error_in_check(BadValue, undefined).

error_in_check(BadValue, Extra) -> error({check,{BadValue,Extra}}).


%%%----------------------------------------------------------------
check_timeout(infinity) -> true;
check_timeout(I) -> check_pos_integer(I).

%%%----------------------------------------------------------------
check_pos_integer(I) -> is_integer(I) andalso I>0.

%%%----------------------------------------------------------------
check_non_neg_integer(I) -> is_integer(I) andalso I>=0.

%%%----------------------------------------------------------------
check_function1(F) -> is_function(F,1).
check_function2(F) -> is_function(F,2).
check_function3(F) -> is_function(F,3).
check_function4(F) -> is_function(F,4).
     
%%%----------------------------------------------------------------
check_pref_public_key_algs(V) -> 
    %% Get the dynamically supported keys, that is, thoose
    %% that are stored
    PKs = ssh_transport:supported_algorithms(public_key),
    CHK = fun(A, Ack) ->
                  case lists:member(A, PKs) of
                      true -> 
                          case lists:member(A,Ack) of
                              false -> [A|Ack];
                              true -> Ack       % Remove duplicates
                          end;
                      false -> error_in_check(A, "Not supported public key")
                  end
          end,
    case lists:foldr(
          fun(ssh_dsa, Ack) -> CHK('ssh-dss', Ack); % compatibility
             (ssh_rsa, Ack) -> CHK('ssh-rsa', Ack); % compatibility
             (X, Ack) -> CHK(X, Ack)
          end, [], V)
    of
        V -> true;
        [] -> false;
        V1 -> {true,V1}
    end.


%%%----------------------------------------------------------------
%% Check that it is a directory and is readable
check_dir(Dir) -> 
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory,
			access = Access}} ->
	    case Access of
		read -> true;
		read_write -> true;
		_ -> error_in_check(Dir, eacces)
	    end;

	{ok, #file_info{}}->
            error_in_check(Dir, enotdir);

	{error, Error} ->
            error_in_check(Dir, Error)
    end.

%%%----------------------------------------------------------------
check_string(S) -> is_list(S).                  % FIXME: stub
                
%%%----------------------------------------------------------------
check_dh_gex_groups({file,File}) when is_list(File) ->
    case file:consult(File) of
        {ok, GroupDefs} ->
            check_dh_gex_groups(GroupDefs);
        {error, Error} ->
            error_in_check({file,File},Error)
    end;

check_dh_gex_groups({ssh_moduli_file,File})  when is_list(File) ->
    case file:open(File,[read]) of
        {ok,D} ->
            try
                read_moduli_file(D, 1, [])
            of
                {ok,Moduli} ->
                    check_dh_gex_groups(Moduli);
                {error,Error} ->
                    error_in_check({ssh_moduli_file,File}, Error)
            catch
                _:_ ->
                    error_in_check({ssh_moduli_file,File}, "Bad format in file "++File)
            after
                file:close(D)
            end;

        {error, Error} ->
            error_in_check({ssh_moduli_file,File}, Error)
    end;

check_dh_gex_groups(L0) when is_list(L0), is_tuple(hd(L0)) ->
    {true,
     collect_per_size(
       lists:foldl(
	 fun({N,G,P}, Acc) when is_integer(N),N>0,
				is_integer(G),G>0,
				is_integer(P),P>0 ->
		 [{N,{G,P}} | Acc];
	    ({N,{G,P}}, Acc) when is_integer(N),N>0,
				  is_integer(G),G>0,
				  is_integer(P),P>0 ->
		 [{N,{G,P}} | Acc];
	    ({N,GPs}, Acc) when is_list(GPs) ->
		 lists:foldr(fun({Gi,Pi}, Acci) when is_integer(Gi),Gi>0,
						     is_integer(Pi),Pi>0 ->
				     [{N,{Gi,Pi}} | Acci]
			     end, Acc, GPs)
	 end, [], L0))};

check_dh_gex_groups(_) ->
    false.



collect_per_size(L) ->
    lists:foldr(
      fun({Sz,GP}, [{Sz,GPs}|Acc]) -> [{Sz,[GP|GPs]}|Acc];
	 ({Sz,GP}, Acc) -> [{Sz,[GP]}|Acc]
      end, [], lists:sort(L)).

read_moduli_file(D, I, Acc) ->
    case io:get_line(D,"") of
	{error,Error} ->
	    {error,Error};
	eof ->
	    {ok, Acc};
	"#" ++ _ -> read_moduli_file(D, I+1, Acc);
	<<"#",_/binary>> ->  read_moduli_file(D, I+1, Acc);
	Data ->
	    Line = if is_binary(Data) -> binary_to_list(Data);
		      is_list(Data) -> Data
		   end,
	    try
		[_Time,_Class,_Tests,_Tries,Size,G,P] = string:tokens(Line," \r\n"),
		M = {list_to_integer(Size),
		     {list_to_integer(G), list_to_integer(P,16)}
		    },
		read_moduli_file(D, I+1, [M|Acc])
	    catch
		_:_ ->
		    read_moduli_file(D, I+1, Acc)
	    end
    end.

%%%----------------------------------------------------------------
-define(SHAs, [md5, sha, sha224, sha256, sha384, sha512]).

check_silently_accept_hosts(B) when is_boolean(B) -> true;
check_silently_accept_hosts(F) when is_function(F,2) -> true;
check_silently_accept_hosts({false,S}) when is_atom(S) -> valid_hash(S);
check_silently_accept_hosts({S,F}) when is_function(F,2) -> valid_hash(S);
check_silently_accept_hosts(_) -> false.


valid_hash(S) -> valid_hash(S, proplists:get_value(hashs,crypto:supports())).

valid_hash(S, Ss) when is_atom(S) -> lists:member(S, ?SHAs) andalso lists:member(S, Ss);
valid_hash(L, Ss) when is_list(L) -> lists:all(fun(S) -> valid_hash(S,Ss) end, L);
valid_hash(X,  _) -> error_in_check(X, "Expect atom or list in fingerprint spec").

%%%----------------------------------------------------------------
check_modify_algorithms(M) when is_list(M) ->
    [error_in_check(Op_KVs, "Bad modify_algorithms")
     || Op_KVs <- M,
        not is_tuple(Op_KVs)
            orelse (size(Op_KVs) =/= 2)
            orelse (not lists:member(element(1,Op_KVs), [append,prepend,rm]))],
    {true, [{Op,normalize_mod_algs(KVs,false)} || {Op,KVs} <- M]};
check_modify_algorithms(_) ->
    error_in_check(modify_algorithms, "Bad option value. List expected.").




normalize_mod_algs(KVs, UseDefaultAlgs) ->
    normalize_mod_algs(ssh_transport:algo_classes(), KVs, [], UseDefaultAlgs).

normalize_mod_algs([K|Ks], KVs0, Acc, UseDefaultAlgs) ->
    %% Pick the expected keys in order and check if they are in the user's list
    {Vs1, KVs} =
        case lists:keytake(K, 1, KVs0) of
            {value, {K,Vs0}, KVs1} ->
                {Vs0, KVs1};
            false ->
                {[], KVs0}
        end,
    Vs = normalize_mod_alg_list(K, Vs1, UseDefaultAlgs),
    normalize_mod_algs(Ks, KVs, [{K,Vs} | Acc], UseDefaultAlgs);
normalize_mod_algs([], [], Acc, _) ->
    %% No values left in the key-value list after removing the expected entries
    %% (thats good)
    lists:reverse(Acc);
normalize_mod_algs([], [{K,_}|_], _, _) ->
    %% Some values left in the key-value list after removing the expected entries
    %% (thats bad)
    case ssh_transport:algo_class(K) of
        true -> error_in_check(K, "Duplicate key");
        false -> error_in_check(K, "Unknown key")
    end;
normalize_mod_algs([], [X|_], _, _) ->
    error_in_check(X, "Bad list element").



%%% Handle the algorithms list
normalize_mod_alg_list(K, Vs, UseDefaultAlgs) ->
    normalize_mod_alg_list(K,
                           ssh_transport:algo_two_spec_class(K),
                           Vs,
                           def_alg(K,UseDefaultAlgs)).


normalize_mod_alg_list(_K, _, [], Default) ->
    Default;

normalize_mod_alg_list(K, true, [{client2server,L1}], [_,{server2client,L2}]) -> 
    [nml1(K,{client2server,L1}),
     {server2client,L2}];

normalize_mod_alg_list(K, true, [{server2client,L2}], [{client2server,L1},_]) -> 
    [{client2server,L1},
     nml1(K,{server2client,L2})];

normalize_mod_alg_list(K, true, [{server2client,L2},{client2server,L1}], _) -> 
    [nml1(K,{client2server,L1}),
     nml1(K,{server2client,L2})];

normalize_mod_alg_list(K, true, [{client2server,L1},{server2client,L2}], _) -> 
    [nml1(K,{client2server,L1}),
     nml1(K,{server2client,L2})];

normalize_mod_alg_list(K, true, L0, _) ->
    L = nml(K,L0), % Throws errors
    [{client2server,L},
     {server2client,L}];

normalize_mod_alg_list(K, false, L, _) ->
    nml(K,L).


nml1(K, {T,V}) when T==client2server ; T==server2client ->
    {T, nml({K,T}, V)}.

nml(K, L) -> 
    [error_in_check(K, "Bad value for this key") % This is a throw
     || V <- L,  
        not is_atom(V)
    ],
    case L -- lists:usort(L) of
        [] -> ok;
        Dups -> error_in_check({K,Dups}, "Duplicates") % This is a throw
    end,
    L.


def_alg(K, false) ->
    case ssh_transport:algo_two_spec_class(K) of
        false -> [];
        true ->  [{client2server,[]}, {server2client,[]}]
    end;
def_alg(K, true) ->
    ssh_transport:default_algorithms(K).

              

check_preferred_algorithms(Algs) when is_list(Algs) ->
    check_input_ok(Algs),
    {true, normalize_mod_algs(Algs, true)};

check_preferred_algorithms(_) ->
    error_in_check(modify_algorithms, "Bad option value. List expected.").


check_input_ok(Algs) ->
    [error_in_check(KVs, "Bad preferred_algorithms")
     || KVs <- Algs,
        not is_tuple(KVs)
            orelse (size(KVs) =/= 2)].

%%%----------------------------------------------------------------
final_preferred_algorithms(Options) ->
    Result =
        case ?GET_OPT(modify_algorithms, Options) of
            undefined ->
                rm_non_supported(true,
                                 ?GET_OPT(preferred_algorithms, Options));
            ModAlgs ->
                rm_non_supported(false,
                                 eval_ops(?GET_OPT(preferred_algorithms, Options),
                                          ModAlgs))
        end,
    error_if_empty(Result), % Throws errors if any value list is empty
    ?PUT_OPT({preferred_algorithms,Result}, Options).
    
eval_ops(PrefAlgs, ModAlgs) ->
    lists:foldl(fun eval_op/2, PrefAlgs, ModAlgs).

eval_op({Op,AlgKVs}, PrefAlgs) ->
    eval_op(Op, AlgKVs, PrefAlgs, []).

eval_op(Op, [{C,L1}|T1], [{C,L2}|T2], Acc) -> 
    eval_op(Op, T1, T2, [{C,eval_op(Op,L1,L2,[])} | Acc]);

eval_op(_,        [],   [], Acc) -> lists:reverse(Acc);
eval_op(rm,      Opt, Pref,  []) when is_list(Opt), is_list(Pref) -> Pref -- Opt;
eval_op(append,  Opt, Pref,  []) when is_list(Opt), is_list(Pref) -> (Pref--Opt) ++ Opt;
eval_op(prepend, Opt, Pref,  []) when is_list(Opt), is_list(Pref) -> Opt ++ (Pref--Opt).


rm_non_supported(UnsupIsErrorFlg, KVs) ->
    [{K,rmns(K,Vs, UnsupIsErrorFlg)} || {K,Vs} <- KVs].

rmns(K, Vs, UnsupIsErrorFlg) ->
    case ssh_transport:algo_two_spec_class(K) of
        false ->
            rm_unsup(Vs, ssh_transport:supported_algorithms(K), UnsupIsErrorFlg, K);
        true ->
            [{C, rm_unsup(Vsx, Sup, UnsupIsErrorFlg, {K,C})} 
             || {{C,Vsx},{C,Sup}} <- lists:zip(Vs,ssh_transport:supported_algorithms(K))
            ]
    end.

rm_unsup(A, B, Flg, ErrInf) ->
    case A--B of
        Unsup=[_|_] when Flg==true -> error({eoptions,
                                             {preferred_algorithms,{ErrInf,Unsup}},
                                             "Unsupported value(s) found"
                                            });
        Unsup -> A -- Unsup
    end.

            
error_if_empty([{K,[]}|_]) ->
    error({eoptions, K, "Empty resulting algorithm list"});
error_if_empty([{K,[{client2server,[]}, {server2client,[]}]}]) ->
    error({eoptions, K, "Empty resulting algorithm list"});
error_if_empty([{K,[{client2server,[]}|_]} | _]) ->
    error({eoptions, {K,client2server}, "Empty resulting algorithm list"});
error_if_empty([{K,[_,{server2client,[]}|_]} | _]) ->
    error({eoptions, {K,server2client}, "Empty resulting algorithm list"});
error_if_empty([_|T]) ->
    error_if_empty(T);
error_if_empty([]) ->
    ok.

%%%----------------------------------------------------------------
forbidden_option(K,V) ->
    Txt = io_lib:format("The option '~s' is used internally. The "
                        "user is not allowed to specify this option.",
                        [K]),
    error({eoptions, {K,V}, Txt}).

%%%----------------------------------------------------------------
