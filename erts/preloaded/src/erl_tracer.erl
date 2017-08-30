-module(erl_tracer).

-export([enabled/3, trace/5, on_load/0]).

-type tracee() :: port() | pid() | undefined.

-type trace_tag_running_ports() :: in | out | in_exiting | out_exiting | out_exited.
-type trace_tag_running_procs() :: in | out | in_exiting | out_exiting | out_exited.
-type trace_tag_send() :: send | send_to_non_existing_process.
-type trace_tag_receive() :: 'receive'.
-type trace_tag_call() :: call | return_to | return_from | exception_from.
-type trace_tag_procs() :: spawn | spawned | exit | link | unlink
                         | getting_linked | getting_unlinked
                         | register | unregister.
-type trace_tag_ports() :: open | closed  | link | unlink
                         | getting_linked | getting_unlinked.
-type trace_tag_gc() :: gc_minor_start | gc_minor_end
                      | gc_major_start | gc_major_end.

-type trace_tag() :: trace_tag_send()
                   | trace_tag_receive()
                   | trace_tag_call()
                   | trace_tag_procs()
                   | trace_tag_ports()
                   | trace_tag_running_procs()
                   | trace_tag_running_ports()
                   | trace_tag_gc().

-type trace_opts() :: #{ extra => term(), match_spec_result => term(),
                         scheduler_id => non_neg_integer(),
                         timestamp => timestamp | cpu_timestamp |
                                      monotonic | strict_monotonic }.
-type tracer_state() :: term().

on_load() ->
    case erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok -> ok
    end.

%%%
%%% NIF placeholders
%%%

%% This suppression is needed as trace_tag gets collapsed to atom()
-dialyzer({no_contracts, enabled/3}).

-spec enabled(Tag :: trace_status,
              TracerState :: tracer_state(),
              Tracee :: tracee()) ->
                     trace | remove;
             (Tag :: trace_tag() | seq_trace,
              TracerState :: tracer_state(),
              Tracee :: tracee()) ->
   trace | discard.
enabled(_, _, _) ->
    erlang:nif_error(nif_not_loaded).

-spec trace(Tag :: trace_tag() | seq_trace,
            TracerState :: tracer_state(),
            Tracee :: tracee(),
            Msg :: term(),
            Opts :: trace_opts()) -> any().

trace(_, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).
