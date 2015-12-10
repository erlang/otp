-module(erl_tracer).

-export([enabled/3, trace/6, on_load/0]).

-type tracee() :: port() | pid() | undefined.
-type trace_tag() :: send | send_to_non_existing_process | 'receive' |
                     call | return_to | return_from | exception_from |
                     spawn | exit | link | unlink | getting_linked |
                     getting_unlinked | register | unregister | in | out |
                     gc_start | gc_end.
-type trace_opts() :: #{ match_spec_result => true | term(),
                         scheduler_id => undefined | non_neg_integer(),
                         timestamp => undefined | timestamp | cpu_timestamp |
                                      monotonic | strict_monotonic }.
-type tracer_state() :: term().

on_load() ->
    case erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok -> ok
    end.

%%%
%%% NIF placeholders
%%%

-spec enabled(Tag :: trace_tag() | seq_trace | trace_status,
              TracerState :: tracer_state(),
              Tracee :: tracee()) ->
   trace | discard | remove.
enabled(_, _, _) ->
    erlang:nif_error(nif_not_loaded).

-spec trace(Tag :: trace_tag() | seq_trace,
            TracerState :: tracer_state(),
            Tracee :: tracee(),
            Msg :: term(),
            Extra :: term(),
            Opts :: trace_opts()) -> any().

trace(_, _, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).
