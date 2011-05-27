%% From: Matthias Radestock <matthias@lshift.net>
%% Date: 19 August 2007
%%
%% when I run dialyzer on my code it throws the following error:
%%
%% Analysis failed with error report:
%%         {{case_clause,any},
%%          [{dialyzer_dataflow,bind_guard,5},
%%           {dialyzer_dataflow,bind_guard_case_clauses,6},
%%           {dialyzer_dataflow,bind_guard,5},
%%           {dialyzer_dataflow,bind_guard_case_clauses,6},
%%           {dialyzer_dataflow,bind_guard,5},
%%           {dialyzer_dataflow,bind_eqeq_guard_lit_other,6},
%%           {dialyzer_dataflow,bind_guard,...},
%%           {dialyzer_dataflow,...}]}
%%
%% This is happening with the R11B-5 version of dialyzer when
%% analyzing the attached file.
%%--------------------------------------------------------------------

-module(not_guard_crash).

-export([match_ticket/2]).

-record(ticket, {passive_flag, active_flag, write_flag, read_flag}).

%%--------------------------------------------------------------------

match_ticket(#ticket{passive_flag = PP,
                     active_flag  = PA,
                     write_flag   = PW,
                     read_flag    = PR},
             #ticket{passive_flag = TP,
                     active_flag  = TA,
                     write_flag   = TW,
                     read_flag    = TR}) ->
    if
        %% Matches if either we're not requesting passive access, or
        %% passive access is permitted, and ...
        (not(TP) orelse PP) andalso
        (not(TA) orelse PA) andalso
        (not(TW) orelse PW) andalso
        (not(TR) orelse PR) ->
            match;
        true ->
            no_match
    end.

%%--------------------------------------------------------------------
