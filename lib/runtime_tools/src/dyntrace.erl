-module(dyntrace).

%%% @doc The Dynamic tracing interface module
%%%
%%% This Dynamic tracing interface module, with the corresponding NIFs, should
%%% work on any operating system platform where user-space DTrace/Systemtap 
%%% (and in the future LttNG UST) probes are supported.
%%%
%%% It is recommended that you use the `dyntrace:p()' function to add
%%% Dynamic trace probes to your Erlang code.  This function can accept up to
%%% four integer arguments and four string arguments; the integer
%%% argument(s) must come before any string argument.
%%%
%%% If using DTrace, enable the dynamic trace probe using the 'dtrace'
%%% command, for example:
%%%
%%%    dtrace -s /your/path/to/lib/runtime_tools-1.8.7/examples/user-probe.d
%%%
%%% Then, back at the Erlang shell, try this example:
%%% ```
%%% 1> dyntrace:put_tag("GGOOOAAALL!!!!!").
%%% true
%%%
%%% 2> dyntrace:p(7, 8, 9, "one", "four").
%%% true
%%% '''
%%%
%%% Output from the example D script `user-probe.d' looks like:
%%% ```
%%% <0.34.0> GGOOOAAALL!!!!! 7 8 9 0 'one' 'four' '' ''
%%% '''
%%%
%%% If the expected type of variable is not present, e.g. integer when
%%% integer() is expected, or an I/O list when iolist() is expected,
%%% then the driver will ignore the user's input and use a default
%%% value of 0 or NULL, respectively.

-export([available/0,
         user_trace_s1/1, % TODO: unify with pid & tag args like user_trace_i4s4
         p/0, p/1, p/2, p/3, p/4, p/5, p/6, p/7, p/8,
         pn/1, pn/2, pn/3, pn/4, pn/5, pn/6, pn/7, pn/8, pn/9]).
-export([put_tag/1, get_tag/0, get_tag_data/0, spread_tag/1, restore_tag/1]).

-export([trace/5,
         trace_procs/5,
         trace_ports/5,
         trace_running_procs/5,
         trace_running_ports/5,
         trace_call/5,
         trace_send/5,
         trace_receive/5,
         trace_garbage_collection/5]).

-export([enabled_procs/3,
         enabled_ports/3,
         enabled_running_procs/3,
         enabled_running_ports/3,
         enabled_call/3,
         enabled_send/3,
         enabled_receive/3,
         enabled_garbage_collection/3,
         enabled/3]).


-export([user_trace_i4s4/9]). % Know what you're doing!
-on_load(on_load/0).

-type probe_arg() :: integer() | iolist().
-type int_p_arg() :: integer() | iolist() | undef.
-type n_probe_label() :: 0..1023.

%% The *_maybe() types use atom() instead of a stricter 'undef'
%% because user_trace_i4s4/9 is exposed to the outside world, and
%% because the driver will allow any atom to be used as a "not
%% present" indication, we'll allow any atom in the types.

-type integer_maybe() :: integer() | atom().
-type iolist_maybe() :: iolist() | atom().

on_load() ->
    PrivDir = code:priv_dir(runtime_tools),
    LibName = "dyntrace",
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, 0) of
                 ok -> ok;
                 {error, {load_failed, _}}=Error1 ->
                     ArchLibDir = 
                         filename:join([PrivDir, "lib", 
                                        erlang:system_info(system_architecture)]),
                     Candidate =
                         filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
                     case Candidate of
                         [] -> Error1;
                         _ ->
                             ArchLib = filename:join([ArchLibDir, LibName]),
                             erlang:load_nif(ArchLib, 0)
                     end;
                 Error1 -> Error1
             end,
    case Status of
        ok -> ok;
        {error, {E, Str}} ->
	    case erlang:system_info(dynamic_trace) of
		none ->
		    ok;
		_ ->
		    error_logger:error_msg("Unable to load dyntrace library. Failed with error:~n
\"~p, ~s\"~n"
					   "Dynamic tracing is enabled but the driver is not built correctly~n",[
														 E,Str]),
		    Status
	    end
    end.

%%%
%%% NIF placeholders
%%%

-spec available() -> true | false.

available() ->
    erlang:nif_error(nif_not_loaded).

-spec user_trace_s1(iolist()) -> true | false | error | badarg.

user_trace_s1(_Message) ->
    erlang:nif_error(nif_not_loaded).

-spec user_trace_i4s4(binary() | undefined,
                      integer_maybe(), integer_maybe(),
                          integer_maybe(), integer_maybe(),
                      iolist_maybe(), iolist_maybe(),
                          iolist_maybe(), iolist_maybe()) ->
      true | false | error | badarg.

user_trace_i4s4(_, _, _, _, _, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

-spec user_trace_n(n_probe_label(), binary() | undefined,
                   integer_maybe(), integer_maybe(),
                   integer_maybe(), integer_maybe(),
                   iolist_maybe(), iolist_maybe(),
                   iolist_maybe(), iolist_maybe()) ->
      true | false | error | badarg.

user_trace_n(_, _, _, _, _, _, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

trace(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_ports(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_running_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_running_ports(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_call(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_send(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_receive(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

trace_garbage_collection(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

enabled(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_procs(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_ports(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_running_procs(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_running_ports(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_call(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_send(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_receive(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

enabled_garbage_collection(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

%%%
%%% Erlang support functions
%%%

-spec p() -> true | false | error | badarg.

p() ->
    user_trace_int(undef, undef, undef, undef, undef, undef, undef, undef).

-spec p(probe_arg()) -> true | false | error | badarg.

p(I1) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, undef, undef, undef, undef);
p(S1) ->
    user_trace_int(undef, undef, undef, undef, S1, undef, undef, undef).

-spec p(probe_arg(), probe_arg()) -> true | false | error | badarg.

p(I1, I2) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, undef, undef, undef, undef);
p(I1, S1) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, undef, undef, undef);
p(S1, S2) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, undef, undef).

-spec p(probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.

p(I1, I2, I3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, undef, undef, undef, undef);
p(I1, I2, S1) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, undef, undef, undef);
p(I1, S1, S2) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, undef, undef);
p(S1, S2, S3) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, S3, undef).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, undef, undef, undef, undef);
p(I1, I2, I3, S1) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, undef, undef, undef);
p(I1, I2, S1, S2) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, undef, undef);
p(I1, S1, S2, S3) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, S3, undef);
p(S1, S2, S3, S4) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, undef, undef, undef);
p(I1, I2, I3, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, undef, undef);
p(I1, I2, S1, S2, S3) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, S3, undef);
p(I1, S1, S2, S3, S4) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, undef, undef);
p(I1, I2, I3, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, S3, undef);
p(I1, I2, S1, S2, S3, S4) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, S3, undef);
p(I1, I2, I3, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, S3, S4).

-spec user_trace_int(int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg(),
                     int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg()) ->
      true | false | error | badarg.

user_trace_int(I1, I2, I3, I4, S1, S2, S3, S4) ->
    UTag = get_tag(),
    try
        user_trace_i4s4(UTag, I1, I2, I3, I4, S1, S2, S3, S4)
    catch
        error:nif_not_loaded ->
            false
    end.

-spec pn(n_probe_label()) -> true | false | error | badarg.

pn(ProbeLabel) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, undef, undef, undef, undef).

-spec pn(n_probe_label(), probe_arg()) -> true | false | error | badarg.

pn(ProbeLabel, I1) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, undef, undef, undef, undef);
pn(ProbeLabel, S1) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, undef, undef, undef).

-spec pn(n_probe_label(), probe_arg(), probe_arg()) -> true | false | error | badarg.

pn(ProbeLabel, I1, I2) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, undef, undef, undef, undef);
pn(ProbeLabel, I1, S1) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, undef, undef, undef);
pn(ProbeLabel, S1, S2) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, S2, undef, undef).

-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, undef, undef, undef, undef);
pn(ProbeLabel, I1, I2, S1) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, undef, undef, undef);
pn(ProbeLabel, I1, S1, S2) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, S2, undef, undef);
pn(ProbeLabel, S1, S2, S3) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, S2, S3, undef).

-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, undef, undef, undef, undef);
pn(ProbeLabel, I1, I2, I3, S1) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, undef, undef, undef);
pn(ProbeLabel, I1, I2, S1, S2) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, S2, undef, undef);
pn(ProbeLabel, I1, S1, S2, S3) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, S2, S3, undef);
pn(ProbeLabel, S1, S2, S3, S4) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, S2, S3, S4).

-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, undef, undef, undef);
pn(ProbeLabel, I1, I2, I3, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, S2, undef, undef);
pn(ProbeLabel, I1, I2, S1, S2, S3) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, S2, S3, undef);
pn(ProbeLabel, I1, S1, S2, S3, S4) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, S2, S3, S4).

-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, undef, undef);
pn(ProbeLabel, I1, I2, I3, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, S2, S3, undef);
pn(ProbeLabel, I1, I2, S1, S2, S3, S4) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, S2, S3, S4).

-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, undef);
pn(ProbeLabel, I1, I2, I3, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, S2, S3, S4).

-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, S4).

-spec user_trace_n_int(n_probe_label(),
                       int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg(),
                       int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg()) ->
      true | false | error | badarg.

user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, S4) ->
    UTag = get_tag(),
    try
        user_trace_n(ProbeLabel, UTag, I1, I2, I3, I4, S1, S2, S3, S4)
    catch
        error:nif_not_loaded ->
            false
    end.

-spec put_tag(undefined | iodata()) -> binary() | undefined.
put_tag(Data) ->
    erlang:dt_put_tag(unicode:characters_to_binary(Data)).

-spec get_tag() -> binary() | undefined.
get_tag() ->
    erlang:dt_get_tag().

-spec get_tag_data() -> binary() | undefined.
%% Gets tag if set, otherwise the spread tag data from last incoming message
get_tag_data() ->
    erlang:dt_get_tag_data().

-spec spread_tag(boolean()) -> true | {non_neg_integer(), binary() | []}.
%% Makes the tag behave as a sequential trace token, will spread with 
%% messages to be picked up by someone using get_tag_data 
spread_tag(B) ->			   
    erlang:dt_spread_tag(B).

-spec restore_tag(true | {non_neg_integer(), binary() | []}) -> true.
restore_tag(T) ->
    erlang:dt_restore_tag(T).
