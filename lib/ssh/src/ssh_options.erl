%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
-moduledoc false.

-include("ssh.hrl").
-include_lib("kernel/include/file.hrl").

-export([default/1,
         get_value/5,  get_value/6,
         put_value/5,
         delete_key/5,
         handle_options/2,
         keep_user_options/2,
         keep_set_options/2,
         no_sensitive/2,
         initial_default_algorithms/2,
         check_preferred_algorithms/1,
         merge_options/3
        ]).

-export_type([private_options/0
             ]).

%%%================================================================
%%% Types

-type option_in() :: proplists:property() | proplists:proplist() .

-type option_class() :: internal_options | socket_options | user_options . 

-type option_declaration() :: #{class := user_option | undoc_user_option,
                                chk := fun((any()) -> boolean() | {true,any()}),
                                default => any()
                               }.

-type option_key() :: atom().

-type option_declarations() :: #{ option_key() := option_declaration() }.

-type error() :: {error,{eoptions,any()}} .

-type private_options() :: #{socket_options   := socket_options(),
                             internal_options := internal_options(),
                             option_key()     => any()
                            }.

%%%================================================================
%%%
%%% Get an option
%%%

-spec get_value(option_class(), option_key(), private_options(),
                atom(), non_neg_integer()) -> any() | no_return().

get_value(Class, Key, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    case Class of
        internal_options -> maps:get(Key, maps:get(internal_options,Opts));
        socket_options   -> proplists:get_value(Key, maps:get(socket_options,Opts));
        user_options     -> maps:get(Key, Opts)
    end;
get_value(Class, Key, Opts, _CallerMod, _CallerLine) ->
    error({bad_options,Class, Key, Opts, _CallerMod, _CallerLine}).


-spec get_value(option_class(), option_key(), private_options(), fun(() -> any()),
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

-spec put_value(option_class(), option_in(), private_options(),
                atom(), non_neg_integer()) -> private_options().

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

-spec delete_key(option_class(), option_key(), private_options(),
                 atom(), non_neg_integer()) -> private_options().

delete_key(internal_options, Key, Opts, _CallerMod, _CallerLine) when is_map(Opts) ->
    InternalOpts = maps:get(internal_options,Opts),
    Opts#{internal_options := maps:remove(Key, InternalOpts)}.
        

%%%================================================================
%%%
%%% Replace 0 or more options in an options map
%%%
merge_options(Role, NewPropList, Opts0) when is_list(NewPropList),
                                             is_map(Opts0) ->
    check_and_save(NewPropList, default(Role), Opts0).

%%%================================================================
%%%
%%% Initialize the options
%%%

-spec handle_options(role(), client_options()|daemon_options()) -> private_options() | error() .

handle_options(Role, PropList0) ->
    handle_options(Role, PropList0, #{socket_options   => [],
                                      internal_options => #{},
                                      key_cb_options   => []
                                     }).

handle_options(Role, OptsList0, Opts0) when is_map(Opts0),
                         is_list(OptsList0) ->
    OptsList1 = proplists:unfold(
                  lists:foldr(fun(T,Acc) when tuple_size(T) =/= 2 -> [{special_trpt_args,T} | Acc];
                                 (X,Acc) -> [X|Acc]
                              end,
                              [], OptsList0)),
    try
        OptionDefinitions = default(Role),
        RoleCnfs = application:get_env(ssh, cnf_key(Role), []),
        {InitialMap,OptsList2} =
            maps:fold(
              fun(K, #{default:=Vd}, {M,PL}) ->
                      %% Now set as the default value:
                      %%   1: from erl command list: erl -ssh opt val
                      %%   2: from config file:  {options, [..., {opt,val}, ....]}
                      %%   3: from the hard-coded option values in default/1
                      %% The value in the option list will be handled later in save/3 later
                      case config_val(K, RoleCnfs, OptsList1) of
                          {ok,V1} ->
                              %% A value set in config or options. Replace the current.
                              {M#{K => V1,
                                  key_cb_options => [{K,V1} | maps:get(key_cb_options,M)]},
                               [{K,V1} | PL]
                              };

                          {append,V1} ->
                              %% A value set in config or options, but should be
                              %% appended to the existing value
                              NewVal = maps:get(K,M,[]) ++ V1,
                              {M#{K => NewVal,
                                  key_cb_options => [{K,NewVal} |
                                                     lists:keydelete(K,1,maps:get(key_cb_options,M))]},
                               [{K,NewVal} | lists:keydelete(K,1,PL)]
                              };
                              
                          undefined ->
                              %% Use the default value
                              {M#{K => Vd}, PL}
                      end
                 %%          ;
                 %% (_,_,Acc) ->
                 %%      Acc
              end,
              {Opts0#{key_cb_options => maps:get(key_cb_options,Opts0)},
               [{K,V} || {K,V} <- OptsList1,
                         not maps:is_key(K,Opts0) % Keep socket opts
               ]
              },
              OptionDefinitions),


        %% Enter the user's values into the map; unknown keys are
        %% treated as socket options
        check_and_save(OptsList2, OptionDefinitions, InitialMap)
    catch
        error:{EO, KV, Reason} when EO == eoptions ; EO == eerl_env ->
            if
                Reason == undefined ->
                    {error, {EO,KV}};
                is_list(Reason) ->
                    {error, {EO,{KV,lists:flatten(Reason)}}};
                true ->
                    {error, {EO,{KV,Reason}}}
            end
    end.

check_and_save(OptsList, OptionDefinitions, InitialMap) ->
    final_preferred_algorithms(
      lists:foldl(fun(KV, Vals) ->
                          save(KV, OptionDefinitions, Vals)
                  end, InitialMap, OptsList)).
    

cnf_key(server) -> server_options;
cnf_key(client) -> client_options.


config_val(modify_algorithms=Key, RoleCnfs, Opts) ->
    V = case application:get_env(ssh, Key) of
            {ok,V0} -> V0;
            _ -> []
        end
        ++ proplists:get_value(Key, RoleCnfs, [])
        ++ proplists:get_value(Key, Opts, []),
    case V of
        [] -> undefined;
        _ -> {append,V}
    end;

config_val(Key, RoleCnfs, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {_,V}} ->
            {ok,V};
        false ->
            case lists:keysearch(Key, 1, RoleCnfs) of
                {value, {_,V}} ->
                    {ok,V};
                false ->
                    application:get_env(ssh, Key) % returns {ok,V} | undefined
            end
    end.


check_fun(Key, Defs) ->
    case ssh_connection_handler:prohibited_sock_option(Key) of
        false ->
            #{chk := Fun} = maps:get(Key, Defs),
            Fun;
        true ->
            fun(_,_) -> forbidden end
    end.

%%%================================================================
%%%
%%% Check and save one option
%%%

%%% First compatibility conversions:
save({allow_user_interaction,V}, Opts, Vals) ->
    save({user_interaction,V}, Opts, Vals);

%% Special case for socket options 'inet' and 'inet6'
save(Inet, Defs, OptMap) when Inet==inet ; Inet==inet6 ->
    save({inet,Inet}, Defs, OptMap);

%% Two clauses to prepare for a proplists:unfold
save({Inet,true}, Defs, OptMap) when Inet==inet ; Inet==inet6 ->  save({inet,Inet}, Defs, OptMap);
save({Inet,false}, _Defs, OptMap) when Inet==inet ; Inet==inet6 -> OptMap;

%% There are inet-options that are not a tuple sized 2. They where marked earlier
save({special_trpt_args,T}, _Defs, OptMap) when is_map(OptMap) ->
    OptMap#{socket_options := [T | maps:get(socket_options,OptMap)]};

%% and finally the 'real stuff':
save({Key,Value}, Defs, OptMap) when is_map(OptMap) ->
    try (check_fun(Key,Defs))(Value)
    of
        true ->
            OptMap#{Key := Value};
        {true, ModifiedValue} ->
            OptMap#{Key := ModifiedValue};
        false ->
            error({eoptions, {Key,Value}, "Bad value"});
        forbidden ->
            error({eoptions, {Key,Value}, 
                   io_lib:format("The option '~s' is used internally. The "
                                 "user is not allowed to specify this option.",
                                 [Key])})
    catch
        %% An unknown Key (= not in the definition map) is
        %% regarded as an inet option:
        error:{badkey,inet} ->
            %% atomic (= non-tuple) options 'inet' and 'inet6':
            OptMap#{socket_options := [Value | maps:get(socket_options,OptMap)]};
        error:{badkey,Key} ->
            OptMap#{socket_options := [{Key,Value} | maps:get(socket_options,OptMap)]};

        %% But a Key that is known but the value does not validate
        %% by the check fun will give an error exception:
        error:{check,{BadValue,Extra}} ->
            error({eoptions, {Key,BadValue}, Extra})
    end;
save(Opt, _Defs, OptMap) when is_map(OptMap) ->
    OptMap#{socket_options := [Opt | maps:get(socket_options,OptMap)]}.


%%%================================================================
no_sensitive(rm, #{id_string := _,
                   tstflg := _}) -> '*** removed ***';
no_sensitive(filter, Opts = #{id_string := _,
                              tstflg := _}) -> 
    Sensitive = [password, user_passwords,
                 dsa_pass_phrase, rsa_pass_phrase, ecdsa_pass_phrase,
                 ed25519_pass_phrase, ed448_pass_phrase],
    maps:fold(
      fun(K, _V, Acc) ->
              case lists:member(K, Sensitive) of
                  true -> Acc#{K := '***'};
                  false -> Acc
              end
      end, Opts, Opts);
no_sensitive(Type, L) when is_list(L) ->
    [no_sensitive(Type,E) || E <- L];
no_sensitive(Type, T) when is_tuple(T) ->
    list_to_tuple( no_sensitive(Type, tuple_to_list(T)) );
no_sensitive(_, X) ->
    X.

%%%================================================================
%%%
-spec keep_user_options(client|server, #{}) -> #{}.

keep_user_options(Type, Opts) ->
    Defs = default(Type),
    maps:filter(fun(Key, _Value) ->
                        try
                            #{class := Class} = maps:get(Key,Defs),
                            Class == user_option
                        catch
                            _:_ -> false
                        end
                end, Opts).


-spec keep_set_options(client|server, #{}) -> #{}.

keep_set_options(Type, Opts) ->
    Defs = default(Type),
    maps:filter(fun(Key, Value) ->
                        try
                            #{default := DefVal} = maps:get(Key,Defs),
                            DefVal =/= Value
                        catch
                            _:_ -> false
                        end
                end, Opts).

%%%================================================================
%%%
%%% Default options
%%%

-spec default(role() | common) -> option_declarations() .

default(server) ->
    (default(common))
        #{
      subsystems =>
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
            class => user_option
           },

      shell =>
          #{default => ?DEFAULT_SHELL,
            chk => fun({M,F,A}) -> is_atom(M) andalso is_atom(F) andalso is_list(A);
                      (disabled) -> true;
                      (V) -> check_function1(V) orelse
                                 check_function2(V)
                   end,
            class => user_option
           },

      exec =>
          #{default => undefined,
            chk => fun({direct, V}) ->  check_function1(V) orelse check_function2(V) orelse check_function3(V);
                      (disabled) -> true;
                      %% Compatibility (undocumented):
                      ({M,F,A}) -> is_atom(M) andalso is_atom(F) andalso is_list(A);
                      (V) -> check_function1(V) orelse check_function2(V) orelse check_function3(V)
                   end,
            class => user_option
           },

      ssh_cli =>
          #{default => undefined,
            chk => fun({Cb, As}) -> is_atom(Cb) andalso is_list(As);
                      (V) -> V == no_cli
                   end,
            class => user_option
           },

      tcpip_tunnel_out =>
           #{default => false,
             chk => fun(V) -> erlang:is_boolean(V) end,
             class => user_option
            },

      tcpip_tunnel_in =>
           #{default => false,
             chk => fun(V) -> check_function2(V) orelse erlang:is_boolean(V) end,
             class => user_option
            },

      system_dir =>
          #{default => "/etc/ssh",
            chk => fun(V) -> check_string(V) andalso check_dir(V) end,
            class => user_option
           },

      auth_method_kb_interactive_data =>
          #{default => undefined, % Default value can be constructed when User is known
            chk => fun({S1,S2,S3,B}) ->
                           check_string(S1) andalso
                               check_string(S2) andalso
                               check_string(S3) andalso
                               is_boolean(B);
                      (F) ->
                           check_function3(F) orelse
                               check_function4(F)
                   end,
            class => user_option
           },

      user_passwords =>
          #{default => [],
            chk => fun(V) ->
                           is_list(V) andalso
                               lists:all(fun({S1,S2}) ->
                                                 check_string(S1) andalso 
                                                     check_string(S2)   
                                         end, V)
                   end,
            class => user_option
           },

      no_auth_needed =>
          #{default => false,
            chk => fun(V) -> erlang:is_boolean(V) end,
            class => user_option
           },

      pk_check_user =>
          #{default => false,
            chk => fun(V) -> erlang:is_boolean(V) end,
            class => user_option
           },

      password =>
          #{default => undefined,
            chk => fun(V) -> check_string(V) end,
            class => user_option
           },

      dh_gex_groups =>
          #{default => undefined,
            chk => fun(V) -> check_dh_gex_groups(V) end,
            class => user_option
           },

      dh_gex_limits =>
          #{default => {0, infinity},
            chk => fun({I1,I2}) ->
                           check_pos_integer(I1) andalso
                               check_pos_integer(I2) andalso
                               I1 < I2;
                      (_) ->
                           false
                   end,
            class => user_option
           },

      pwdfun =>
          #{default => undefined,
            chk => fun(V) -> check_function4(V) orelse check_function2(V) end,
            class => user_option
           },

      max_initial_idle_time =>
          #{default => infinity, %% To not break compatibility
            chk => fun(V) -> check_timeout(V) end,
            class => user_option
           },

      negotiation_timeout =>
          #{default => 2*60*1000,
            chk => fun(V) -> check_timeout(V) end,
            class => user_option
           },

      hello_timeout =>
          #{default => 30*1000,
            chk => fun check_timeout/1,
            class => user_option
           },

      max_sessions =>
          #{default => infinity,
            chk => fun(V) -> check_pos_integer(V) end,
            class => user_option
           },

      max_channels =>
          #{default => infinity,
            chk => fun(V) -> check_pos_integer(V) end,
            class => user_option
           },

      parallel_login =>
          #{default => false,
            chk => fun(V) -> erlang:is_boolean(V) end,
            class => user_option
           },

      minimal_remote_max_packet_size =>
          #{default => 0,
            chk => fun(V) -> check_pos_integer(V) end,
            class => user_option
           },

      failfun =>
          #{default => fun(_,_,_) -> void end,
            chk => fun(V) -> check_function3(V) orelse
                                 check_function2(V) % Backwards compatibility
                   end,
            class => user_option
           },

      connectfun =>
          #{default => fun(_,_,_) -> void end,
            chk => fun(V) -> check_function3(V) end,
            class => user_option
           },

      bannerfun =>
          #{default => fun(_) -> <<>> end,
            chk => fun(V) -> check_function1(V) end,
            class => user_option
           },

%%%%% Undocumented
      infofun =>
          #{default => fun(_,_,_) -> void end,
            chk => fun(V) -> check_function3(V) orelse
                                 check_function2(V) % Backwards compatibility
                   end,
            class => undoc_user_option
           }
     };

default(client) ->
    (default(common))
        #{
      dsa_pass_phrase =>
          #{default => undefined,
            chk => fun(V) -> check_string(V) end,
            class => user_option
           },

      rsa_pass_phrase =>
          #{default => undefined,
            chk => fun(V) -> check_string(V) end,
            class => user_option
           },

      ecdsa_pass_phrase =>
          #{default => undefined,
            chk => fun(V) -> check_string(V) end,
            class => user_option
           },

%%% Not yet implemented      ed25519_pass_phrase =>
%%% Not yet implemented          #{default => undefined,
%%% Not yet implemented            chk => fun(V) -> check_string(V) end,
%%% Not yet implemented            class => user_option
%%% Not yet implemented           },
%%% Not yet implemented
%%% Not yet implemented      ed448_pass_phrase =>
%%% Not yet implemented          #{default => undefined,
%%% Not yet implemented            chk => fun(V) -> check_string(V) end,
%%% Not yet implemented            class => user_option
%%% Not yet implemented           },
%%% Not yet implemented
      silently_accept_hosts =>
          #{default => false,
            chk => fun(V) -> check_silently_accept_hosts(V) end,
            class => user_option
           },

      user_interaction =>
          #{default => true,
            chk => fun(V) -> erlang:is_boolean(V) end,
            class => user_option
           },

      save_accepted_host =>
          #{default => true,
            chk => fun(V) -> erlang:is_boolean(V) end,
            class => user_option
           },

      dh_gex_limits =>
          #{default => {1024, 6144, 8192},      % FIXME: Is this true nowadays?
            chk => fun({Min,I,Max}) ->
                           lists:all(fun check_pos_integer/1,
                                     [Min,I,Max]);
                      (_) -> false
                   end,
            class => user_option
           },

      connect_timeout =>
          #{default => infinity,
            chk => fun(V) -> check_timeout(V) end,
            class => user_option
           },

      user =>
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
            chk => fun(V) -> check_string(V) end,
            class => user_option
           },

      password =>
          #{default => undefined,
            chk => fun(V) -> check_string(V) end,
            class => user_option
           },

      quiet_mode =>
          #{default => false,
            chk => fun(V) -> erlang:is_boolean(V) end,
            class => user_option
           },

%%%%% Undocumented
      keyboard_interact_fun =>
          #{default => undefined,
            chk => fun(V) -> check_function3(V) end,
            class => undoc_user_option
           }
     };

default(common) ->
    #{
       user_dir =>
           #{default => false, % FIXME: TBD ~/.ssh at time of call when user is known
             chk => fun(V) -> check_string(V) andalso check_dir(V) end,
             class => user_option
            },

       %% NOTE: This option's default value must be undefined.
       %% In the final stage that "merges" the modify_algorithms and preferred_algorithms,
       %% this option's default values is set.
      pref_public_key_algs =>
          #{default => undefined,
            chk => fun(V) -> check_pref_public_key_algs(V) end,
            class => user_option
           },

       preferred_algorithms =>
           #{default => ssh:default_algorithms(),
             chk => fun(V) -> check_preferred_algorithms(V) end,
             class => user_option
            },

       %% NOTE: This option is supposed to be used only in this very module (?MODULE). There is
       %% a final stage in handle_options that "merges" the preferred_algorithms option and this one.
       %% The preferred_algorithms is the one to use in the rest of the ssh application!
       modify_algorithms =>
           #{default => undefined, % signals error if unsupported algo in preferred_algorithms :(
             chk => fun(V) -> check_modify_algorithms(V) end,
             class => user_option
            },

       id_string => 
           #{default => try {ok, [_|_] = VSN} = application:get_key(ssh, vsn),
                            "Erlang/" ++ VSN
                        catch
                            _:_ -> ""
                        end,
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
             class => user_option
            },

       key_cb =>
           #{default => {ssh_file, []},
             chk => fun({Mod,Opts}) -> is_atom(Mod) andalso is_list(Opts);
                       (Mod) when is_atom(Mod) -> {true, {Mod,[]}};
                       (_) -> false
                    end,
             class => user_option
            },

       profile =>
           #{default => ?DEFAULT_PROFILE,
             chk => fun(V) -> erlang:is_atom(V) end,
             class => user_option
            },

      idle_time =>
          #{default => infinity,
            chk => fun(V) -> check_timeout(V) end,
            class => user_option
           },

       disconnectfun =>
           #{default => fun(_) -> void end,
             chk => fun(V) -> check_function1(V) end,
             class => user_option
            },

       unexpectedfun => 
           #{default => fun(_,_) -> report end,
             chk => fun(V) -> check_function2(V) end,
             class => user_option
            },

       ssh_msg_debug_fun =>
           #{default => fun(_,_,_,_) -> void end,
             chk => fun(V) -> check_function4(V) end,
             class => user_option
            },

       max_log_item_len =>
           #{default => 500,
             chk => fun(infinity) -> true;
                       (I) -> check_non_neg_integer(I)
                    end,
             class => user_option
            },

      rekey_limit =>
          #{default => {3600000, 1024000000}, % {1 hour, 1 GB}
            chk => fun({infinity, infinity}) ->
                           true;
                      ({Mins, infinity}) when is_integer(Mins), Mins>0 ->
                           {true, {Mins*60*1000, infinity}};
                      ({infinity, Bytes}) when is_integer(Bytes), Bytes>=0 ->
                           true;
                      ({Mins, Bytes}) when is_integer(Mins), Mins>0,
                                           is_integer(Bytes), Bytes>=0 ->
                           {true, {Mins*60*1000, Bytes}};
                      (infinity) ->
                           {true, {3600000, infinity}};
                      (Bytes) when is_integer(Bytes), Bytes>=0 ->
                           {true, {3600000, Bytes}};
                      (_) ->
                           false
                   end,
            class => user_option
           },

      auth_methods =>
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
            class => user_option
           },

       send_ext_info =>
           #{default => true,
             chk => fun erlang:is_boolean/1,
             class => user_option
            },

       recv_ext_info =>
           #{default => true,
             chk => fun erlang:is_boolean/1,
             class => user_option
            },

%%%%% Undocumented
       transport =>
           #{default => ?DEFAULT_TRANSPORT,
             chk => fun({A,B,C}) ->
                            is_atom(A) andalso is_atom(B) andalso is_atom(C)
                    end,
             class => undoc_user_option
            },

       vsn =>
           #{default => {2,0},
             chk => fun({Maj,Min}) -> check_non_neg_integer(Maj) andalso check_non_neg_integer(Min);
                       (_) -> false
                    end,
             class => undoc_user_option
            },
    
       tstflg =>
           #{default => [],
             chk => fun(V) -> erlang:is_list(V) end,
             class => undoc_user_option
            },

       user_dir_fun =>
           #{default => undefined,
             chk => fun(V) -> check_function1(V) end,
             class => undoc_user_option
            },

       max_random_length_padding =>
           #{default => ?MAX_RND_PADDING_LEN,
             chk => fun(V) -> check_non_neg_integer(V) end,
             class => undoc_user_option
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
initial_default_algorithms(DefList, ModList) ->
    {true, L0} = check_modify_algorithms(ModList),
    rm_non_supported(false, eval_ops(DefList,L0)).

%%%----------------------------------------------------------------
check_modify_algorithms(M) when is_list(M) ->
    [error_in_check(Op_KVs, "Bad modify_algorithms")
     || Op_KVs <- M,
        not is_tuple(Op_KVs)
            orelse (tuple_size(Op_KVs) =/= 2)
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
    %% (that's good)
    lists:reverse(Acc);
normalize_mod_algs([], [{K,_}|_], _, _) ->
    %% Some values left in the key-value list after removing the expected entries
    %% (that's bad)
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
            orelse (tuple_size(KVs) =/= 2)].

%%%----------------------------------------------------------------
final_preferred_algorithms(Options0) ->
    Result =
        case ?GET_OPT(modify_algorithms, Options0) of
            undefined ->
                rm_non_supported(true,
                                 ?GET_OPT(preferred_algorithms, Options0));
            ModAlgs ->
                rm_non_supported(false,
                                 eval_ops(?GET_OPT(preferred_algorithms, Options0),
                                          ModAlgs))
        end,
    error_if_empty(Result), % Throws errors if any value list is empty
    Options1 = ?PUT_OPT({preferred_algorithms,Result}, Options0),
    case ?GET_OPT(pref_public_key_algs, Options1) of
        undefined ->
            ?PUT_OPT({pref_public_key_algs, proplists:get_value(public_key,Result)}, Options1);
        _ ->
            Options1
    end.
    
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
