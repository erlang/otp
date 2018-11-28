-ifndef(LOGGER_HRL).
-define(LOGGER_HRL,true).
-define(LOG_EMERGENCY(A),?DO_LOG(emergency,[A])).
-define(LOG_EMERGENCY(A,B),?DO_LOG(emergency,[A,B])).
-define(LOG_EMERGENCY(A,B,C),?DO_LOG(emergency,[A,B,C])).

-define(LOG_ALERT(A),?DO_LOG(alert,[A])).
-define(LOG_ALERT(A,B),?DO_LOG(alert,[A,B])).
-define(LOG_ALERT(A,B,C),?DO_LOG(alert,[A,B,C])).

-define(LOG_CRITICAL(A),?DO_LOG(critical,[A])).
-define(LOG_CRITICAL(A,B),?DO_LOG(critical,[A,B])).
-define(LOG_CRITICAL(A,B,C),?DO_LOG(critical,[A,B,C])).

-define(LOG_ERROR(A),?DO_LOG(error,[A])).
-define(LOG_ERROR(A,B),?DO_LOG(error,[A,B])).
-define(LOG_ERROR(A,B,C),?DO_LOG(error,[A,B,C])).

-define(LOG_WARNING(A),?DO_LOG(warning,[A])).
-define(LOG_WARNING(A,B),?DO_LOG(warning,[A,B])).
-define(LOG_WARNING(A,B,C),?DO_LOG(warning,[A,B,C])).

-define(LOG_NOTICE(A),?DO_LOG(notice,[A])).
-define(LOG_NOTICE(A,B),?DO_LOG(notice,[A,B])).
-define(LOG_NOTICE(A,B,C),?DO_LOG(notice,[A,B,C])).

-define(LOG_INFO(A),?DO_LOG(info,[A])).
-define(LOG_INFO(A,B),?DO_LOG(info,[A,B])).
-define(LOG_INFO(A,B,C),?DO_LOG(info,[A,B,C])).

-define(LOG_DEBUG(A),?DO_LOG(debug,[A])).
-define(LOG_DEBUG(A,B),?DO_LOG(debug,[A,B])).
-define(LOG_DEBUG(A,B,C),?DO_LOG(debug,[A,B,C])).

-define(LOG(L,A),?DO_LOG(L,[A])).
-define(LOG(L,A,B),?DO_LOG(L,[A,B])).
-define(LOG(L,A,B,C),?DO_LOG(L,[A,B,C])).

-define(LOCATION,#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
                   line=>?LINE,
                   file=>?FILE}).

%%%-----------------------------------------------------------------
%%% Internal, i.e. not intended for direct use in code - use above
%%% macros instead!
-define(DO_LOG(Level,Args),
        case logger:allow(Level,?MODULE) of
            true ->
                apply(logger,macro_log,[?LOCATION,Level|Args]);
            false ->
                ok
        end).
-endif.
