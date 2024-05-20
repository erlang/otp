%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(supervisor_bridge).
-moduledoc """
Generic supervisor bridge behavior.

This behavior module provides a supervisor bridge, a process that connects a
subsystem not designed according to the OTP design principles to a supervision
tree. The supervisor bridge sits between a supervisor and the subsystem. It
behaves like a real supervisor to its own supervisor, but has a different
interface than a real supervisor to the subsystem. For more information, see
[Supervisor Behaviour](`e:system:sup_princ.md`) in OTP Design Principles.

A supervisor bridge assumes the functions for starting and stopping the
subsystem to be located in a callback module exporting a predefined set of
functions.

The `m:sys` module can be used for debugging a supervisor bridge.

Unless otherwise stated, all functions in this module fail if the specified
supervisor bridge does not exist or if bad arguments are specified.

## See Also

`m:supervisor`, `m:sys`
""".

-behaviour(gen_server).

-include("logger.hrl").

%% External exports
-export([start_link/2, start_link/3]).
%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).
%% logger callback
-export([format_log/1, format_log/2]).

-doc """
Whenever a supervisor bridge is started using
[`start_link/2,3`](`start_link/2`), this function is called by the new process
to start the subsystem and initialize.

`Args` is the `Args` argument provided to the start function.

The function is to return `{ok,Pid,State}`, where `Pid` is the pid of the main
process in the subsystem and `State` is any term.

If later `Pid` terminates with a reason `Reason`, the supervisor bridge
terminates with reason `Reason` as well. If later the supervisor bridge is
stopped by its supervisor with reason `Reason`, it calls
[`Module:terminate(Reason,State)`](`c:terminate/2`) to terminate.

If the initialization fails, the function is to return `{error,Error}`, where
`Error` is any term, or `ignore`.
""".
-callback init(Args :: term()) ->
    {ok, Pid :: pid(), State :: term()} | ignore | {error, Error :: term()}.
-doc """
This function is called by the supervisor bridge when it is about to terminate.
It is to be the opposite of [`Module:init/1`](`c:init/1`) and stop the subsystem
and do any necessary cleaning up. The return value is ignored.

`Reason` is `shutdown` if the supervisor bridge is terminated by its supervisor.
If the supervisor bridge terminates because a a linked process (apart from the
main process of the subsystem) has terminated with reason `Term`, then `Reason`
becomes `Term`.

`State` is taken from the return value of [`Module:init/1`](`c:init/1`).
""".
-callback terminate(Reason :: (shutdown | term()), State :: term()) ->
    Ignored :: term().

%%%-----------------------------------------------------------------
%%% This is a rewrite of supervisor_bridge from BS.3.
%%%
%%% This module is built to function as process code
%%% for a process sitting inbetween a real supervisor
%%% and a not start&recovery complient server/system
%%% The process inbetween simulates start&recovery
%%% behaviour of the server/system below.
%%%
%%% The supervisor_bridge behaviour must export the following
%%% functions:
%%%    init(Args) -> {ok, Pid, State} | {error, Reason} | ignore
%%%       where Pid is the child process
%%%    terminate(Reason, State) -> ok
%%%-----------------------------------------------------------------
-record(state, {mod, pid, child_state, name}).

-doc """
Creates a nameless supervisor bridge process as part of a supervision tree.

Equivalent to `start_link/3` except that the supervisor process is not
[`registered`](`erlang:register/2`).
""".
-spec start_link(Module, Args) -> Result when
      Module :: module(),
      Args :: term(),
      Result :: {ok, Pid} | ignore | {error, Error},
      Error :: {already_started, Pid} | term(),
      Pid :: pid().

start_link(Mod, StartArgs) ->
    gen_server:start_link(supervisor_bridge, [Mod, StartArgs, self], []).

-doc """
Creates a supervisor bridge process, linked to the calling process, which calls
[`Module:init/1`](`c:init/1`) to start the subsystem.

To ensure a synchronized startup procedure, this function does not return until
[`Module:init/1`](`c:init/1`) has returned.

- If `SupBridgeName={local,Name}`, the supervisor bridge is registered locally
  as `Name` using [`register/2`](`register/2`).
- If `SupBridgeName={global,GlobalName}`, the supervisor bridge is registered
  globally as `GlobalName` using `global:register_name/2`.
- If `SupBridgeName={via,Module,ViaName}`, the supervisor bridge is registered
  as `ViaName` using a registry represented by Module. The `Module` callback is
  to export functions `register_name/2`, `unregister_name/1`, and `send/2`,
  which are to behave like the corresponding functions in `m:global`. Thus,
  `{via,global,GlobalName}` is a valid reference.

`Module` is the name of the callback module.

`Args` is an arbitrary term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

- If the supervisor bridge and the subsystem are successfully started, the
  function returns `{ok,Pid}`, where `Pid` is is the pid of the supervisor
  bridge.
- If there already exists a process with the specified `SupBridgeName`, the
  function returns `{error,{already_started,Pid}}`, where `Pid` is the pid of
  that process.
- If [`Module:init/1`](`c:init/1`) returns `ignore`, this function returns
  `ignore` as well and the supervisor bridge terminates with reason `normal`.
- If [`Module:init/1`](`c:init/1`) fails or returns an error tuple or an
  incorrect value, this function returns `{error,Error}`, where `Error` is a
  term with information about the error, and the supervisor bridge terminates
  with reason `Error`.
""".
-spec start_link(SupBridgeName, Module, Args) -> Result when
      SupBridgeName :: {local, Name} | {global, GlobalName} |
		       {via, Module, ViaName},
      Name :: atom(),
      GlobalName :: term(),
      ViaName :: term(),
      Module :: module(),
      Args :: term(),
      Result :: {ok, Pid} | ignore | {error, Error},
      Error :: {already_started, Pid} | term(),
      Pid :: pid().

start_link(Name, Mod, StartArgs) ->
    gen_server:start_link(Name, supervisor_bridge, [Mod, StartArgs, Name], []).

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
-doc false.
init([Mod, StartArgs, Name0]) ->  
    process_flag(trap_exit, true),
    Name = supname(Name0, Mod),
    case Mod:init(StartArgs) of
	{ok, Pid, ChildState} when is_pid(Pid) ->
	    link(Pid),
	    report_progress(Pid, Mod, StartArgs, Name),
	    {ok, #state{mod = Mod, pid = Pid,
			child_state = ChildState, name = Name}};
	ignore ->
	    ignore;
	{error, Reason} ->
	    {stop, Reason}
    end.

supname(self, Mod) -> {self(),Mod};
supname(N, _)      -> N.

%% A supervisor *must* answer the supervisor:which_children call.
-doc false.
handle_call(which_children, _From, State) ->
    {reply, [], State};
handle_call(_Req, _From, State) ->
    {reply, {error, badcall}, State}.

-doc false.
handle_cast(_, State) ->
    {noreply, State}.

-doc false.
handle_info({'EXIT', Pid, Reason}, State) when State#state.pid =:= Pid ->
	case Reason of
	normal ->
	    ok;
	shutdown ->
	    ok;
	{shutdown, _Term} ->
	    ok;
	_ ->
	    report_error(child_terminated, Reason, State)
	end,
    {stop, Reason, State#state{pid = undefined}};
handle_info(_, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, #state{pid = undefined}) ->
    ok;
terminate(Reason, State) ->
    terminate_pid(Reason, State).

-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% This function is supposed to terminate the 'real' server.
terminate_pid(Reason, #state{mod = Mod, child_state = ChildState}) ->
    Mod:terminate(Reason, ChildState).

report_progress(Pid, Mod, StartArgs, SupName) ->
    ?LOG_INFO(#{label=>{supervisor,progress},
                report=>[{supervisor, SupName},
                         {started, [{pid, Pid},
                                    {mfa, {Mod, init, [StartArgs]}}]}]},
              #{domain=>[otp,sasl],
                report_cb=>fun supervisor_bridge:format_log/2,
                logger_formatter=>#{title=>"PROGRESS REPORT"},
                error_logger=>#{tag=>info_report,
                                type=>progress,
                                report_cb=>
                                    fun supervisor_bridge:format_log/1}}).

report_error(Error, Reason, #state{name = Name, pid = Pid, mod = Mod}) ->
    ?LOG_ERROR(#{label=>{supervisor,error},
                 report=>[{supervisor, Name},
                          {errorContext, Error},
                          {reason, Reason},
                          {offender, [{pid, Pid}, {mod, Mod}]}]},
               #{domain=>[otp,sasl],
                 report_cb=>fun supervisor_bridge:format_log/2,
                 logger_formatter=>#{title=>"SUPERVISOR REPORT"},
                 error_logger=>#{tag=>error_report,
                                 type=>supervisor_report,
                                 report_cb=>
                                     fun supervisor_bridge:format_log/1}}).

%% format_log/1 is the report callback used by Logger handler
%% error_logger only. It is kept for backwards compatibility with
%% legacy error_logger event handlers. This function must always
%% return {Format,Args} compatible with the arguments in this module's
%% calls to error_logger prior to OTP-21.0.
-doc false.
format_log(LogReport) ->
    Depth = error_logger:get_format_depth(),
    FormatOpts = #{chars_limit => unlimited,
                   depth => Depth,
                   single_line => false,
                   encoding => utf8},
    format_log_multi(limit_report(LogReport, Depth), FormatOpts).

limit_report(LogReport, unlimited) ->
    LogReport;
limit_report(#{label:={supervisor,progress},
               report:=[{supervisor,_}=Supervisor,{started,Child}]}=LogReport,
             Depth) ->
    LogReport#{report=>[Supervisor,
                        {started,limit_child_report(Child, Depth)}]};
limit_report(#{label:={supervisor,error},
               report:=[{supervisor,_}=Supervisor,{errorContext,Ctxt},
                        {reason,Reason},{offender,Child}]}=LogReport,
             Depth) ->
    LogReport#{report=>[Supervisor,
                        {errorContext,io_lib:limit_term(Ctxt, Depth)},
                        {reason,io_lib:limit_term(Reason, Depth)},
                        {offender,io_lib:limit_term(Child, Depth)}]}.

limit_child_report(ChildReport, Depth) ->
    {mfa,{M,F,[As]}} = lists:keyfind(mfa, 1, ChildReport),
    NewMFAs = {M,F,[io_lib:limit_term(As, Depth)]},
    lists:keyreplace(mfa, 1, ChildReport, {mfa,NewMFAs}).

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
-doc false.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default, FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={supervisor,progress},
                    report:=[{supervisor,SupName},{started,Child}]},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    {ChildFormat,ChildArgs} =
        format_child_log_progress_single(Child, "Started:", FormatOpts),
    Format = "Supervisor: "++P++".",
    Args =
        case Depth of
            unlimited ->
                [SupName];
            _ ->
                [SupName,Depth]
        end,
    {Format++ChildFormat,Args++ChildArgs};
format_log_single(#{label:={supervisor,_Error},
                    report:=[{supervisor,SupName},
                             {errorContext,Ctxt},
                             {reason,Reason},
                             {offender,Child}]},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["Supervisor: ",P,". Context: ",P,
                            ". Reason: ",P,"."]),
    {ChildFormat,ChildArgs} =
        format_child_log_error_single(Child, "Offender:"),
    Args =
        case Depth of
            unlimited ->
                [SupName,Ctxt,Reason];
            _ ->
                [SupName,Depth,Ctxt,Depth,Reason,Depth]
        end,
    {Format++ChildFormat,Args++ChildArgs};
format_log_single(Report, FormatOpts) ->
    format_log_multi(Report, FormatOpts).

format_log_multi(#{label:={supervisor,progress},
                   report:=[{supervisor,SupName},
                            {started,Child}]},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        lists:append(
          ["    supervisor: ",P,"~n",
           "    started: ",P,"~n"]),
    Args =
        case Depth of
            unlimited ->
                [SupName,Child];
            _ ->
                [SupName,Depth,Child,Depth]
        end,
    {Format,Args};
format_log_multi(#{label:={supervisor,_Error},
                   report:=[{supervisor,SupName},
                            {errorContext,Ctxt},
                            {reason,Reason},
                            {offender,Child}]},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        lists:append(
          ["    supervisor: ",P,"~n",
           "    errorContext: ",P,"~n",
           "    reason: ",P,"~n",
           "    offender: ",P,"~n"]),
    Args =
        case Depth of
            unlimited ->
                [SupName,Ctxt,Reason,Child];
            _ ->
                [SupName,Depth,Ctxt,Depth,Reason,Depth,Child,Depth]
        end,
    {Format,Args}.

format_child_log_progress_single(Child, Tag, FormatOpts) ->
    {pid,Pid} = lists:keyfind(pid, 1, Child),
    {mfa,MFAs} = lists:keyfind(mfa, 1, Child),
    Args =
        case maps:get(depth, FormatOpts) of
            unlimited ->
                [MFAs];
            Depth ->
                [MFAs, Depth]
        end,
    {" ~s pid=~w,mfa="++p(FormatOpts)++".",[Tag,Pid]++Args}.

format_child_log_error_single(Child, Tag) ->
    {pid,Pid} = lists:keyfind(pid, 1, Child),
    {mod,Mod} = lists:keyfind(mod, 1, Child),
    {" ~s pid=~w,mod=~w.",[Tag,Pid,Mod]}.

p(#{single_line:=Single,depth:=Depth,encoding:=Enc}) ->
    "~"++single(Single)++mod(Enc)++p(Depth);
p(unlimited) ->
    "p";
p(_Depth) ->
    "P".

single(true) -> "0";
single(false) -> "".

mod(latin1) -> "";
mod(_) -> "t".
