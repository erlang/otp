-module(application_env_cli_provider).
-include("logger.hrl").
-export([load/2]).

load(App, Env) ->
    try
        case init:get_argument(App) of
            {ok, Args} ->
               {ok, lists:foldl(fun conv/2, Env, Args)};
            _ ->
               {ok, []}
        end
    catch
        throw:Error -> Error
    end.

conv([BareKey, BareVal | T], Acc) ->
    Key = make_term(BareKey),
    Val = make_term(BareVal),
    conv(T, [{Key, Val} | lists:keydelete(Key, 1, Acc)]);
conv(_, Acc) ->
    Acc.

make_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens ++ [{dot, erl_anno:new(1)}]) of
                {ok, Term} ->
                    Term;
                {error, {_,M,Reason}} ->
                    handle_make_term_error(M, Reason, Str)
            end;
        {error, {_,M,Reason}, _} ->
            handle_make_term_error(M, Reason, Str)
    end.

handle_make_term_error(Mod, Reason, Str) ->
    ?LOG_ERROR("application_controller: ~ts: ~ts~n",
               [Mod:format_error(Reason), Str],
               #{error_logger=>#{tag=>error}}),
    throw({error, {bad_environment_value, Str}}).
