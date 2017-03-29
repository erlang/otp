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
         handle_options/2
        ]).

-export_type([options/0
             ]).

%%%================================================================
%%% Types

-type options() :: #{socket_options   := socket_options(),
                     internal_options := internal_options(),
                     option_key()     => any()
                    }.

-type socket_options()   :: proplists:proplist().
-type internal_options() :: #{option_key() => any()}.

-type option_key() :: atom().

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
    io:format("*** Bad Opts GET OPT ~p ~p:~p Key=~p,~n    Opts=~p~n",[Class,_CallerMod,_CallerLine,Key,Opts]),
    error({bad_options,Class, Key, Opts, _CallerMod, _CallerLine}).


-spec get_value(option_class(), option_key(), options(), any(),
                atom(), non_neg_integer()) -> any() | no_return().

get_value(socket_options, Key, Opts, Def, _CallerMod, _CallerLine) when is_map(Opts) ->
    proplists:get_value(Key, maps:get(socket_options,Opts), Def);
get_value(Class, Key, Opts, Def, CallerMod, CallerLine) when is_map(Opts) ->
    try get_value(Class, Key, Opts, CallerMod, CallerLine)
    catch
        error:{badkey,Key} -> Def
    end;
get_value(Class, Key, Opts, _Def, _CallerMod, _CallerLine) ->
    io:format("*** Bad Opts GET OPT ~p ~p:~p Key=~p,~n    Opts=~p~n",[Class,_CallerMod,_CallerLine,Key,Opts]),
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
        lists:foldl(fun(KV, Vals) ->
                            save(KV, OptionDefinitions, Vals)
                    end, InitialMap, PropList1)
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
    end.

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
          #{default => {shell, start, []},
            chk => fun({M,F,A}) -> is_atom(M) andalso is_atom(F) andalso is_list(A);
                      (V) -> check_function1(V) orelse check_function2(V)
                   end,
            class => user_options
           },

      {exec, def} =>                 % FIXME: need some archeology....
          #{default => undefined,
            chk => fun({M,F,_}) -> is_atom(M) andalso is_atom(F);
                      (V) -> is_function(V)
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

      {auth_methods, def} =>
          #{default => ?SUPPORTED_AUTH_METHODS,
            chk => fun check_string/1,
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

      {pref_public_key_algs, def} =>
          #{default => 
                %% Get dynamically supported keys in the order of the ?SUPPORTED_USER_KEYS
                [A || A <- ?SUPPORTED_USER_KEYS,
                      lists:member(A, ssh_transport:supported_algorithms(public_key))],
            chk => 
                fun check_pref_public_key_algs/1,
            class =>
                ssh
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

      {idle_time, def} =>
          #{default => infinity,
            chk => fun check_timeout/1,
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
                          [A|Ack];
                      false -> 
                          %% Check with the documented options, that is,
                          %% the one we can handle
                          case lists:member(A,?SUPPORTED_USER_KEYS) of
                              false ->
                                  %% An algorithm ssh never can handle
                                  error_in_check(A, "Not supported public key");
                              true ->
                                  %% An algorithm ssh can handle, but not in
                                  %% this very call
                                  Ack
                          end
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
check_silently_accept_hosts({S,F}) when is_atom(S),
                                        is_function(F,2) -> 
    lists:member(S, ?SHAs) andalso
        lists:member(S, proplists:get_value(hashs,crypto:supports()));
check_silently_accept_hosts({L,F}) when is_list(L),
                                        is_function(F,2) -> 
    lists:all(fun(S) ->
                      lists:member(S, ?SHAs) andalso
                          lists:member(S, proplists:get_value(hashs,crypto:supports()))
              end, L);
check_silently_accept_hosts(_) -> false.

%%%----------------------------------------------------------------
check_preferred_algorithms(Algs) ->
    try alg_duplicates(Algs, [], [])
    of
	[] ->
	    {true,
	     [try ssh_transport:supported_algorithms(Key)
	      of
		  DefAlgs -> handle_pref_alg(Key,Vals,DefAlgs)
	      catch
		  _:_ -> error_in_check(Key,"Bad preferred_algorithms key")
	      end  || {Key,Vals} <- Algs]
	    };

	Dups ->
	    error_in_check(Dups, "Duplicates")
    catch
	_:_ ->
	    false
    end.

alg_duplicates([{K,V}|KVs], Ks, Dups0) ->
    Dups =
	case lists:member(K,Ks) of
	    true ->  [K|Dups0];
	    false -> Dups0
	end,
    case V--lists:usort(V) of
	[] -> alg_duplicates(KVs, [K|Ks], Dups);
	Ds -> alg_duplicates(KVs, [K|Ks], Dups++Ds)
    end;
alg_duplicates([], _Ks, Dups) ->
    Dups.

handle_pref_alg(Key,
		Vs=[{client2server,C2Ss=[_|_]},{server2client,S2Cs=[_|_]}],
		[{client2server,Sup_C2Ss},{server2client,Sup_S2Cs}]
	       ) ->
    chk_alg_vs(Key, C2Ss, Sup_C2Ss),
    chk_alg_vs(Key, S2Cs, Sup_S2Cs),
    {Key, Vs};

handle_pref_alg(Key,
		Vs=[{server2client,[_|_]},{client2server,[_|_]}],
		Sup=[{client2server,_},{server2client,_}]
	       ) ->
    handle_pref_alg(Key, lists:reverse(Vs), Sup);

handle_pref_alg(Key,
		Vs=[V|_],
		Sup=[{client2server,_},{server2client,_}]
	       ) when is_atom(V) ->
    handle_pref_alg(Key, [{client2server,Vs},{server2client,Vs}], Sup);

handle_pref_alg(Key,
		Vs=[V|_],
		Sup=[S|_]
	       ) when is_atom(V), is_atom(S) ->
    chk_alg_vs(Key, Vs, Sup),
    {Key, Vs};

handle_pref_alg(Key, Vs, _) ->
    error_in_check({Key,Vs}, "Badly formed list").

chk_alg_vs(OptKey, Values, SupportedValues) ->
    case (Values -- SupportedValues) of
	[] -> Values;
	Bad -> error_in_check({OptKey,Bad}, "Unsupported value(s) found")
    end.

%%%----------------------------------------------------------------
forbidden_option(K,V) ->
    Txt = io_lib:format("The option '~s' is used internally. The "
                        "user is not allowed to specify this option.",
                        [K]),
    error({eoptions, {K,V}, Txt}).

%%%----------------------------------------------------------------
