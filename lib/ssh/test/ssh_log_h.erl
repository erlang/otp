-module(ssh_log_h).

-export([log/2
        ]).

-export([add_fun/3,
         sensitive_in_opt/1,
         chk_sensitive/2]).


log(LogEvent, #{?MODULE := #{function := F,
                             data := D}}) ->
    F(LogEvent, D);

log(LogEvent, Config) ->
    io:format("LogEvent = ~p~nConfig = ~p~n", [LogEvent, Config]).

%%%----------------------------------------------------------------
add_fun(Id, Fun, Data) ->
    logger:add_handler(Id, ?MODULE, #{?MODULE => #{function => Fun,
                                                   data => Data}}).
                               
%%%----------------------------------------------------------------
chk_sensitive(LogEvent, _) ->
    case sensitive_in_opt(LogEvent) of
        false ->
            io:format("no sensitive keys are present in the log event!~n", []);
        true ->
            io:format("**** Sensitive keys(s) is(are) present in the log event!!~n", [])
    end.

%%%----------------------------------------------------------------
sensitive_in_opt(LogEvent) ->
    case locate_opts(LogEvent) of
        false ->
            false;

        {ok, O} ->
            Sensitive = [password, user_passwords,
                         dsa_pass_phrase, rsa_pass_phrase, ecdsa_pass_phrase,
                         ed25519_pass_phrase, ed448_pass_phrase],
            lists:any(fun(K) -> lists:member(K, Sensitive) end,
                      maps:keys(O))
    end.



%%%----------------------------------------------------------------
locate_opts(#{id_string := _,
              tstflg := _} = Opts) ->
    {ok, Opts};

locate_opts([H|T]) ->
    case locate_opts(H) of
        {ok, Opts} ->
            {ok, Opts};
        false ->
            locate_opts(T)
    end;

locate_opts(T) when is_tuple(T) ->
    locate_opts( tuple_to_list(T) );

locate_opts(#{} = M) ->
    locate_opts( maps:to_list(M) );

locate_opts(_) ->
    false.


%%%----------------------------------------------------------------

    

