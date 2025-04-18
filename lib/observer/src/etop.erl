%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(etop).
-moduledoc """
Erlang Top is a tool for presenting information about Erlang processes similar
to the information presented by "top" in UNIX.

Start Erlang Top with the provided scripts `etop`. This starts a hidden Erlang
node that connects to the node to be measured. The measured node is specified
with option `-node`. If the measured node has a different cookie than the
default cookie for the user who invokes the script, the cookie must be
explicitly specified with option `-setcookie`.

Under Windows, batch file `etop.bat` can be used.

When executing the `etop` script, configuration parameters can be specified as
command-line options, for example,
`etop -node testnode@myhost -setcookie MyCookie`. The following configuration
parameters exist for the tool:

- **`node`** - The measured node.

  Value: `t:atom/0`

  Mandatory

- **`setcookie`** - Cookie to use for the `etop` node. Must be same as the
  cookie on the measured node.

  Value: `t:atom/0`

- **`lines`** - Number of lines (processes) to display.

  Value: `t:integer/0`

  Default: `10`

- **`interval`** - Time interval (in seconds) between each update of the
  display.

  Value: `t:integer/0`

  Default: `5`

- **`accumulate`** - If `true`, the execution time and reductions are
  accumulated.

  Value: `t:boolean/0`

  Default: `false`

- **`sort`** - Identifies what information to sort by.

  Value: `runtime | reductions | memory | msg_q`

  Default: `runtime` (`reductions` if `tracing=off`)

- **`tracing`** - `etop` uses the Erlang trace facility, and thus no other
  tracing is possible on the measured node while `etop` is running, unless this
  option is set to `off`. Also helpful if the `etop` tracing causes too high
  load on the measured node. With tracing off, runtime is not measured.

  Value: `on | off`

  Default: `on`

For details about Erlang Top, see the [User's Guide](etop_ug.md).
""".
-author('siri@erix.ericsson.se').

-export([start/0, start/1, config/2, stop/0, dump/1, help/0]).
%% Internal
-export([update/1]).
-export([loadinfo/2, meminfo/2, getopt/2]).

-include("etop.hrl").
-include("etop_defs.hrl").

-define(change_at_runtime_config,[lines,interval,sort,accumulate]).

-doc """
Displays the help of `etop` and its options.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec help() -> ok.
help() ->
    io:format(
      "Usage of the Erlang top program~n~n"
      "Options are set as command line parameters as in -node my@host~n"
      "or as parameters to etop:start([{node, my@host}, {...}]).~n~n"
      "Options are:~n"
      "  node        atom       Required   The erlang node to measure ~n"
      "  port        integer    The used port, NOTE: due to a bug this program~n"
      "                         will hang if the port is not available~n"
      "  accumulate  boolean    If true execution time is accumulated ~n"
      "  lines       integer    Number of displayed processes~n"
      "  interval    integer    Display update interval in secs~n"
      "  sort        runtime | reductions | memory | msg_q~n"
      "                         What information to sort by~n"
      "                         Default: runtime (reductions if tracing=off)~n"
      "  tracing     on | off   etop uses the erlang trace facility, and thus~n"
      "                         no other tracing is possible on the node while~n"
      "                         etop is running, unless this option is set to~n"
      "                         'off'. Also helpful if the etop tracing causes~n"
      "                         too high load on the measured node.~n"
      "                         With tracing off, runtime is not measured!~n"
      "  setcookie   string     Only applicable on operating system command~n"
      "                         line. Set cookie for the etop node, must be~n"
      "                         same as the cookie for the measured node.~n"
      "                         This is not an etop parameter~n"
     ).

-doc """
Terminates `etop`.
""".
-spec stop() -> stop | not_started.
stop() ->
    case whereis(etop_server) of
        undefined -> not_started;
        Pid when is_pid(Pid) ->
            Result = etop_server ! stop,
            stop_etop_input_server(),
            Result
    end.

-doc """
Changes the configuration parameters of the tool during runtime. Allowed
parameters are `lines`, `interval`, `accumulate`, and `sort`.
""".
-spec config(Key,Value) -> ok | {error, Reason} when
      Key :: 'lines' | 'interval' | 'accumulate' | 'sort',
      Value :: term(),
      Reason :: term().
config(Key,Value) ->
    case check_runtime_config(Key,Value) of
	ok ->
	    etop_server ! {config,{Key,Value}},
	    ok;
	error -> 
	    {error,illegal_opt}
    end.
check_runtime_config(lines,L) when is_integer(L),L>0 -> ok;
check_runtime_config(interval,I) when is_integer(I),I>0 -> ok;
check_runtime_config(sort,S) when S=:=runtime; 
				  S=:=reductions; 
				  S=:=memory; 
				  S=:=msg_q -> ok;
check_runtime_config(accumulate,A) when A=:=true; A=:=false -> ok;
check_runtime_config(_Key,_Value) -> error.

-doc """
Dumps the current display to a text file.
""".
-spec dump(File) -> ok | {error, Reason} when
      File   :: file:filename_all(),
      Reason :: term().
dump(File) ->
    case file:open(File,[write,{encoding,utf8}]) of
	{ok,Fd} -> etop_server ! {dump,Fd}, ok;
	Error -> Error
    end.

-doc """
Starts `etop`. Notice that `etop` is preferably started with the `etop` script.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec start() -> ok.
start() ->
    start([]).

-doc """
Starts `etop`. To view the possible options, use `help/0`.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec start(Options) -> ok when
      Options :: [{Key,Value}],
      Key :: atom(),
      Value :: term().
start(Opts) ->
    process_flag(trap_exit, true),
    Config1 = handle_args(init:get_arguments() ++ Opts, #opts{}),
    Config2 = Config1#opts{server=self()},

    %% Connect to the node we want to look at
    Node = getopt(node, Config2),
    case net_adm:ping(Node) of
	pang when Node /= node() ->
	    io:format("Error Couldn't connect to node ~p ~n~n", [Node]),
	    help(),
	    exit("connection error");
	_pong ->
	    check_runtime_tools_vsn(Node)
    end,

    %% Maybe set up the tracing
    Config3 = 
	if Config2#opts.tracing == on andalso Node /= node() ->
		%% Cannot trace on current node since the tracer will
		%% trace itself
		etop_tr:setup_tracer(Config2);
	   true -> 
		if Config2#opts.sort == runtime -> 
			Config2#opts{sort=reductions,tracing=off};
		   true -> 
			Config2#opts{tracing=off}
		end
	end,
    AccumTab = ets:new(accum_tab,
		       [set,public,{keypos,#etop_proc_info.pid}]),
    Config4 = Config3#opts{accum_tab=AccumTab},

    %% Switch shell to raw mode if possible
    Config5 =
        case shell:start_interactive({noshell, raw}) of
            ok ->
                spawn_link(fun stop_on_input/0),
                Config4#opts{shell_mode = raw};
            {error, _Other} ->
                Config4
        end,

    %% Start the output server
    Out = spawn_link(Config5#opts.out_mod, init, [Config5]),
    Config6 = Config5#opts{out_proc = Out},
    
    init_data_handler(Config6),
    ok.

stop_on_input() ->
    register(etop_input_server, self()),
    stop_on_input_loop().

stop_on_input_loop() ->
    case io:get_chars("", 1) of
        {error, Reason} ->
            io:fwrite("Cannot get user's input, reason: ~p\r\n", [Reason]),
            ok;
        eof ->
            ok;
        Input ->
            case string:find(Input, "q") of
                nomatch ->
                    stop_on_input_loop();
                _ ->
                    stop()
            end
    end.

stop_etop_input_server() ->
    Self = self(),
    case whereis(etop_input_server) of
        Self ->
            ok;
        InputServer ->
            catch exit(InputServer, stop),
            ok
    end.

check_runtime_tools_vsn(Node) ->
    case rpc:call(Node,observer_backend,vsn,[]) of
	{ok,Vsn} -> check_vsn(Vsn);
        _ -> exit("Faulty version of runtime_tools on remote node")
    end.
check_vsn(_Vsn) -> ok.
%check_vsn(_Vsn) -> exit("Faulty version of runtime_tools on remote node").
    

%% Handle the incoming data

init_data_handler(Config) ->
    register(etop_server,self()),    
    Reader = 
	if Config#opts.tracing == on -> etop_tr:reader(Config);
	   true -> undefined
    end,
    data_handler(Reader, Config).

data_handler(Reader, Opts) ->
    receive
	stop ->
	    stop(Reader, Opts),
	    ok;
	{config,{Key,Value}} ->
	    data_handler(Reader,putopt(Key,Value,Opts));
	{dump,Fd} ->
	    Opts#opts.out_proc ! {dump,Fd},
	    data_handler(Reader,Opts);
	{'EXIT', EPid, Reason} when EPid == Opts#opts.out_proc ->
	    case Reason of
		normal -> ok;
		_ -> io:format("Output server crashed: ~tp~n",[Reason])
	    end,
	    stop(Reader, Opts),
	    out_proc_stopped;
	{'EXIT', Reader, eof} ->
	    io:format("Lost connection to node ~p exiting~n", [Opts#opts.node]),
	    stop(Reader, Opts),
	    connection_lost;
	_ ->
	    data_handler(Reader, Opts)
    end.

stop(Reader, Opts) ->
    (Opts#opts.out_mod):stop(Opts#opts.out_proc),
    case {Reader, Opts#opts.tracing == on} of
        {undefined, true} ->
            etop_tr:stop_tracer(Opts);
        {Pid, true} when is_pid(Pid) ->
            etop_tr:stop_tracer(Opts),
            %% Stop reader process so it doesn't crash on deleted accumulator table
            %% when our process dies.
            exit(Pid, stop);
        _ ->
            ok
    end,
    unregister(etop_server).

-doc false.
update(#opts{store=Store,node=Node,tracing=Tracing,intv=Interval}=Opts) ->
    Pid = spawn_link(Node,observer_backend,etop_collect,[self()]),
    Info = receive {Pid,I} -> I 
	   after Interval ->
                   %% Took more than the update interval to fetch
                   %% data. Either the connection is lost or the
                   %% fetching took too long...
                   io:format("Timeout when waiting for process info from "
                             "node ~p; exiting~n", [Node]),
                   exit(timeout)
	   end,
    #etop_info{procinfo=ProcInfo} = Info,
    ProcInfo1 = 
	if Tracing == on ->
		PI=lists:map(fun(PI=#etop_proc_info{pid=P}) -> 
				     case ets:lookup(Store,P) of
					 [{P,T}] -> PI#etop_proc_info{runtime=T};
					 [] -> PI
				     end
			     end,
			     ProcInfo),
		PI;	   
	   true -> 
		lists:map(fun(PI) -> PI#etop_proc_info{runtime='-'} end,ProcInfo)
	end,
    ProcInfo2 = sort(Opts,ProcInfo1),
    Info#etop_info{procinfo=ProcInfo2}.    

sort(Opts,PI) ->
    Tag = get_tag(Opts#opts.sort),
    PI1 = if Opts#opts.accum -> 
		  PI;
	     true -> 
		  AccumTab = Opts#opts.accum_tab,
		  lists:map(
		    fun(#etop_proc_info{pid=Pid,reds=Reds,runtime=RT}=I) -> 
			    NewI = 
				case ets:lookup(AccumTab,Pid) of
				    [#etop_proc_info{reds=OldReds,
						     runtime='-'}] -> 
					I#etop_proc_info{reds=Reds-OldReds,
							 runtime='-'};
				    [#etop_proc_info{reds=OldReds,
						     runtime=OldRT}] -> 
					I#etop_proc_info{reds=Reds-OldReds,
							 runtime=RT-OldRT};
				    [] -> 
					I
				end,
			    ets:insert(AccumTab,I),
			    NewI
		    end,
		    PI)
	  end,
    PI2 = lists:reverse(lists:keysort(Tag,PI1)),
    lists:sublist(PI2,Opts#opts.lines).

get_tag(runtime) -> #etop_proc_info.runtime;
get_tag(memory) -> #etop_proc_info.mem;
get_tag(reductions) -> #etop_proc_info.reds;
get_tag(msg_q) -> #etop_proc_info.mq.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration Management

-doc false.
getopt(What, Config) when is_record(Config, opts) ->
    case What of 
	node  -> Config#opts.node;
	port  -> Config#opts.port;
	accum -> Config#opts.accum;
	intv  -> Config#opts.intv;
	lines  -> Config#opts.lines;
	sort -> Config#opts.sort;
	width -> Config#opts.width;
	height-> Config#opts.height;

	store -> Config#opts.store;
	host  -> Config#opts.host
    end.

putopt(Key, Value, Config) when is_record(Config, opts) ->
    Config1 = handle_args([{Key,Value}],Config),
    Config1#opts.out_proc ! {config,{Key,Value},Config1},
    Config1.

handle_args([{node, [NodeString]}| R], Config) when is_list(NodeString) ->
    Node = list_to_atom(NodeString),
    NewC = Config#opts{node = Node},
    handle_args(R, NewC);
handle_args([{node, Node} |R], Config) when is_atom(Node) ->
    NewC = Config#opts{node = Node},
    handle_args(R, NewC);
handle_args([{port, Port}| R], Config) when is_integer(Port) ->
    NewC = Config#opts{port=Port},
    handle_args(R, NewC);
handle_args([{port, [Port]}| R], Config) when is_list(Port) ->
    NewC = Config#opts{port= list_to_integer(Port)},
    handle_args(R, NewC);
handle_args([{interval, Time}| R], Config) when is_integer(Time)->
    NewC = Config#opts{intv=Time*1000},
    handle_args(R, NewC);
handle_args([{interval, [Time]}| R], Config) when is_list(Time)->
    NewC = Config#opts{intv=list_to_integer(Time)*1000},
    handle_args(R, NewC);
handle_args([{lines, Lines}| R], Config) when is_integer(Lines) ->
    NewC = Config#opts{lines=Lines},
    handle_args(R, NewC);
handle_args([{lines, [Lines]}| R], Config) when is_list(Lines) ->
    NewC = Config#opts{lines= list_to_integer(Lines)},
    handle_args(R, NewC);
handle_args([{accumulate, Bool}| R], Config) when is_atom(Bool) ->
    NewC = Config#opts{accum=Bool},
    handle_args(R, NewC);
handle_args([{accumulate, [Bool]}| R], Config) when is_list(Bool) ->
    NewC = Config#opts{accum= list_to_atom(Bool)},
    handle_args(R, NewC);
handle_args([{sort, Sort}| R], Config) when is_atom(Sort) ->
    NewC = Config#opts{sort=Sort},
    handle_args(R, NewC);
handle_args([{sort, [Sort]}| R], Config) when is_list(Sort) ->
    NewC = Config#opts{sort= list_to_atom(Sort)},
    handle_args(R, NewC);
handle_args([{output, Output}| R], Config) when is_atom(Output) ->
    NewC = Config#opts{out_mod=output(Output)},
    handle_args(R, NewC);
handle_args([{output, [Output]}| R], Config) when is_list(Output) ->
    NewC = Config#opts{out_mod= output(list_to_atom(Output))},
    handle_args(R, NewC);
handle_args([{tracing, OnOff}| R], Config) when is_atom(OnOff) ->
    NewC = Config#opts{tracing=OnOff},
    handle_args(R, NewC);
handle_args([{tracing, [OnOff]}| R], Config) when is_list(OnOff) ->
    NewC = Config#opts{tracing=list_to_atom(OnOff)},
    handle_args(R, NewC);

handle_args([_| R], C) ->
    handle_args(R, C);
handle_args([], C) ->
    C.

output(graphical) -> exit({deprecated, "Use observer instead"});
output(text) -> etop_txt.


-doc false.
loadinfo(SysI,Prev) ->
    #etop_info{n_procs = Procs, 
	       run_queue = RQ, 
	       now = Now,
	       wall_clock = WC,
	       runtime = RT} = SysI,
    Cpu = calculate_cpu_utilization(WC,RT,Prev#etop_info.runtime),
    Clock = io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
			 tuple_to_list(element(2,calendar:now_to_datetime(Now)))),
    {Cpu,Procs,RQ,Clock}.

calculate_cpu_utilization({_,WC},{_,RT},_) ->
    %% Old version of observer_backend, using statistics(wall_clock)
    %% and statistics(runtime)
    case {WC,RT} of
	{0,0} ->
	    0;
	{0,_} ->
	    100;
	_ ->
	    round(100*RT/WC)
    end;
calculate_cpu_utilization(_,undefined,_) ->
    %% First time collecting - no cpu utilization has been measured
    %% since scheduler_wall_time flag is not yet on
    0;
calculate_cpu_utilization(WC,RTInfo,undefined) ->
    %% Second time collecting - RTInfo shows scheduler_wall_time since
    %% flag was set to true. Faking previous values by setting
    %% everything to zero.
    ZeroRT = [{Id,0,0} || {Id,_,_} <- RTInfo],
    calculate_cpu_utilization(WC,RTInfo,ZeroRT);
calculate_cpu_utilization(_,RTInfo,PrevRTInfo) ->
    %% New version of observer_backend, using statistics(scheduler_wall_time)
    Sum = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}},{AAcc,TAcc}) ->
                              {(A1 - A0)+AAcc,(T1 - T0)+TAcc}
                      end,
		      {0,0},
		      lists:zip(PrevRTInfo,RTInfo)),
    case Sum of
	{0,0} ->
	    0;
	{Active,Total} ->
	    round(100*Active/Total)
    end.

-doc false.
meminfo(MemI, [Tag|Tags]) -> 
    [round(get_mem(Tag, MemI)/1024)|meminfo(MemI, Tags)];
meminfo(_MemI, []) -> [].

get_mem(Tag, MemI) ->
    case lists:keysearch(Tag, 1, MemI) of
	{value, {Tag, I}} -> I;			       %these are in bytes
	_ -> 0
    end.

