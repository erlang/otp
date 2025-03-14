-module(ssh_eqc_event_handler).

-export([add_report_handler/0,
         get_reports/1,
         log/2]).
-define(HANDLER_ID, ssh_eqc_handler_id).
-behaviour(logger_handler).

add_report_handler() ->
    Ref = make_ref(),
    ok = logger:add_handler(?HANDLER_ID, ?MODULE,
                            #{config =>
                                  #{collector => self(),
                                    ref => Ref}}),
    {ok, Ref}.

get_reports(Ref) ->
    {ok, #{config := #{ref := Ref}}} = logger:get_handler_config(?HANDLER_ID),
    get_reports(Ref, []).

get_reports(Ref, Acc) ->
    receive
	{event_msg, Ref, Msg} ->
            get_reports(Ref, [Msg | Acc])
    after
        100 ->
            ok = logger:remove_handler(?HANDLER_ID),
            {ok, Acc}
    end.

log(E = #{msg := Msg, level := Level}, #{config :=
                                             #{collector := Collector,
                                               ref := Ref}}) ->
    Collector ! {event_msg, Ref, #{msg => Msg, level => Level}}.


