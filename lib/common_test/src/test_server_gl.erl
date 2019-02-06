%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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
%% This module implements group leader processes for test cases.
%% Each group leader process handles output to the minor log file for
%% a test case, and calls test_server_io to handle output to the common
%% log files. The group leader processes are created and destroyed
%% through the test_server_io module/process.

-module(test_server_gl).
-export([start_link/1,stop/1,set_minor_fd/3,unset_minor_fd/1,
	 get_tc_supervisor/1,print/4,set_props/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

-record(st, {tc_supervisor :: 'none'|pid(),    %Test case supervisor
	     tc :: mfa() | 'undefined',	       %Current test case MFA
	     minor :: 'none'|pid(),	       %Minor fd
	     minor_monitor,		       %Monitor ref for minor fd
             tsio_monitor,                     %Monitor red for controlling proc
	     capture :: 'none'|pid(),	       %Capture output
	     reject_io :: boolean(),	       %Reject I/O requests...
	     permit_io,			       %... and exceptions
	     auto_nl=true :: boolean(),	       %Automatically add NL
	     levels,			       %{Stdout,Major,Minor}
	     escape_chars=true		       %Switch escaping HTML on/off
	    }).

%% start_link()
%%  Start a new group leader process. Only to be called by
%%  the test_server_io process.

start_link(TSIO) ->
    case gen_server:start_link(?MODULE, [TSIO], []) of
	{ok,Pid} ->
	    {ok,Pid};
	Other ->
	    Other
    end.


%% stop(Pid)
%%  Stop a group leader process. Only to be called by
%%  the test_server_io process.

stop(GL) ->
    gen_server:cast(GL, stop).


%% set_minor_fd(GL, Fd, MFA)
%%  GL = Pid for the group leader process
%%  Fd = file descriptor for the minor log file
%%  MFA = {M,F,A} for the test case owning the minor log file
%%
%%  Register the file descriptor for the minor log file. Subsequent
%%  IO directed to the minor log file will be written to this file.
%%  Also register the currently executing process at the testcase
%%  supervisor corresponding to this group leader process.

set_minor_fd(GL, Fd, MFA) ->
    req(GL, {set_minor_fd,Fd,MFA,self()}).


%% unset_minor_fd(GL, Fd, MFA)
%%  GL = Pid for the group leader process
%%
%%  Unregister the file descriptor for minor log file (typically
%%  because the test case has ended the minor log file is about
%%  to be closed). Subsequent IO (for example, by a process spawned
%%  by the testcase process) will go to the unexpected_io log file.

unset_minor_fd(GL) ->
    req(GL, unset_minor_fd).


%% get_tc_supervisor(GL)
%%  GL = Pid for the group leader process
%%
%%  Return the Pid for the process that supervises the test case
%%  that has this group leader.

get_tc_supervisor(GL) ->
    req(GL, get_tc_supervisor).


%% print(GL, Detail, Format, Args) -> ok
%%  GL = Pid for the group leader process
%%  Detail = integer() | minor | major | html | stdout
%%  Msg = iodata()
%%  Printer = internal | pid()
%%
%%  Print a message to one of the log files. If Detail is an integer,
%%  it will be compared to the levels (set by set_props/2) to
%%  determine which log file(s) that are to receive the output. If
%%  Detail is an atom, the value of the atom will directly determine
%%  which log file to use.  IO to the minor log file will be handled
%%  directly by this group leader process (printing to the file set by
%%  set_minor_fd/3), and all other IO will be handled by calling
%%  test_server_io:print/3.

print(GL, Detail, Msg, Printer) ->
    req(GL, {print,Detail,Msg,Printer}).


%% set_props(GL, [PropertyTuple])
%%  GL = Pid for the group leader process
%%  PropertyTuple = {levels,{Show,Major,Minor}} |
%%                  {auto_nl,boolean()} |
%%                  {reject_io_reqs,boolean()}
%%
%%  Set properties for this group leader process.

set_props(GL, PropList) ->
    req(GL, {set_props,PropList}).

%%% Internal functions.

init([TSIO]) ->
    ct_util:mark_process(group_leader),
    EscChars = case application:get_env(test_server, esc_chars) of
		   {ok,ECBool} -> ECBool;
		   _           -> true
	       end,
    Ref = erlang:monitor(process, TSIO),
    {ok,#st{tc_supervisor=none,
	    minor=none,
	    minor_monitor=none,
            tsio_monitor=Ref,
	    capture=none,
	    reject_io=false,
	    permit_io=gb_sets:empty(),
	    auto_nl=true,
	    levels={1,19,10},
	    escape_chars=EscChars
	   }}.

req(GL, Req) ->
    gen_server:call(GL, Req, infinity).

handle_call(get_tc_supervisor, _From, #st{tc_supervisor=Pid}=St) ->
    {reply,Pid,St};
handle_call({set_minor_fd,Fd,MFA,Supervisor}, _From, St) ->
    Ref = erlang:monitor(process, Fd),
    {reply,ok,St#st{tc=MFA,minor=Fd,minor_monitor=Ref,
		    tc_supervisor=Supervisor}};
handle_call(unset_minor_fd, _From, St) ->
    {reply,ok,St#st{minor=none,tc_supervisor=none}};
handle_call({set_props,PropList}, _From, St) ->
    {reply,ok,do_set_props(PropList, St)};
handle_call({print,Detail,Msg,Printer}, {From,_}, St) ->
    output(Detail, Msg, Printer, From, St),
    {reply,ok,St}.

handle_cast(stop, St) ->
    {stop,normal,St}.

handle_info({'DOWN',Ref,process,_,Reason}=D, #st{minor_monitor=Ref}=St) ->
    case Reason of
	normal -> ok;
	_ ->
	    Data = io_lib:format("=== WARNING === TC: ~tw\n"
				 "Got down from minor Fd ~w: ~tw\n\n",
				 [St#st.tc,St#st.minor,D]),
	    test_server_io:print_unexpected(Data)
    end,
    {noreply,St#st{minor=none,minor_monitor=none}};
handle_info({'DOWN',Ref,process,_,_}, #st{tsio_monitor=Ref}=St) ->
    %% controlling process (test_server_io) terminated, we're done
    {stop,normal,St};
handle_info({permit_io,Pid}, #st{permit_io=P}=St) ->
    {noreply,St#st{permit_io=gb_sets:add(Pid, P)}};
handle_info({capture,Cap0}, St) ->
    Cap = case Cap0 of
	      false -> none;
	      Pid when is_pid(Cap0) -> Pid
	  end,
    {noreply,St#st{capture=Cap}};
handle_info({io_request,From,ReplyAs,Req}=IoReq, St) ->
    _ = try io_req(Req, From, St) of
	passthrough ->
	    group_leader() ! IoReq;
	{EscapeHtml,Data} ->
	    case is_io_permitted(From, St) of
		false ->
		    ok;
		true ->
		    case St of
			#st{capture=none} ->
			    ok;
			#st{capture=CapturePid} ->
			    CapturePid ! {captured,Data},
			    ok
		    end,
		    case EscapeHtml andalso St#st.escape_chars of
			true ->
			    output(minor, test_server_ctrl:escape_chars(Data),
				   From, From, St);
			false ->
			    output(minor, Data, From, From, St)
		    end
	    end,
	    From ! {io_reply,ReplyAs,ok}
    catch
	_:_ ->
	    From ! {io_reply,ReplyAs,{error,arguments}}
    end,
    {noreply,St};
handle_info({structured_io,ClientPid,{Detail,Str}}, St) ->
    output(Detail, Str, ClientPid, ClientPid, St),
    {noreply,St};
handle_info({printout,Detail,["$tc_html",Format],Args}, St) ->
    Str = io_lib:format(Format, Args),
    output(Detail, ["$tc_html",Str], internal, none, St),
    {noreply,St};
handle_info({printout,Detail,Fun}, St) when is_function(Fun)->
    output(Detail, Fun, internal, none, St),
    {noreply,St};
handle_info({printout,Detail,Format,Args}, St) ->
    Str = io_lib:format(Format, Args),
    if not St#st.escape_chars ->
	    output(Detail, ["$tc_html",Str], internal, none, St);
       true ->
	    output(Detail, Str, internal, none, St)
    end,
    {noreply,St};
handle_info(Msg, #st{tc_supervisor=Pid}=St) when is_pid(Pid) ->
    %% The process overseeing the testcase process also used to be
    %% the group leader; thus, it is widely expected that it can be
    %% reached by sending a message to the group leader. Therefore
    %% we'll need to forward any non-recognized messaged to the test
    %% case supervisor.
    Pid ! Msg,
    {noreply,St};
handle_info(_Msg, #st{}=St) ->
    %% There is no known supervisor process. Ignore this message.
    {noreply,St}.

terminate(_, _) ->
    ok.

do_set_props([{levels,Levels}|Ps], St) ->
    do_set_props(Ps, St#st{levels=Levels});
do_set_props([{auto_nl,AutoNL}|Ps], St) ->
    do_set_props(Ps, St#st{auto_nl=AutoNL});
do_set_props([{reject_io_reqs,Bool}|Ps], St) ->
    do_set_props(Ps, St#st{reject_io=Bool});
do_set_props([], St) -> St.

io_req({put_chars,Enc,Str}, _, _) when Enc =:= latin1; Enc =:= unicode  ->
    case Str of
	["$tc_html",Str0] ->
	    {false,unicode:characters_to_list(Str0, Enc)};
	_ ->
	    {true,unicode:characters_to_list(Str, Enc)}
    end;
io_req({put_chars,Encoding,Mod,Func,[Format,Args]}, _, _) ->
    case Format of
	["$tc_html",Format0] ->
	    Str = Mod:Func(Format0, Args),
	    {false,unicode:characters_to_list(Str, Encoding)};
	_ ->
	    Str = Mod:Func(Format, Args),
	    {true,unicode:characters_to_list(Str, Encoding)}
    end;
io_req(_, _, _) -> passthrough.

output(Level, StrOrFun, Sender, From, St) when is_integer(Level) ->
    case selected_by_level(Level, stdout, St) of
	true when hd(StrOrFun) == "$tc_html" ->
	    output(stdout, tl(StrOrFun), Sender, From, St);
	true when is_function(StrOrFun) ->
	    output(stdout, StrOrFun(stdout), Sender, From, St);
	true ->
	    output(stdout, StrOrFun, Sender, From, St);
	false ->
	    ok
    end,
    case selected_by_level(Level, major, St) of
	true when hd(StrOrFun) == "$tc_html" ->
	    output(major, tl(StrOrFun), Sender, From, St);
	true when is_function(StrOrFun) ->
	    output(major, StrOrFun(major), Sender, From, St);
	true ->
	    output(major, StrOrFun, Sender, From, St);
	false ->
	    ok
    end,
    case selected_by_level(Level, minor, St) of
	true when hd(StrOrFun) == "$tc_html" ->
	    output(minor, tl(StrOrFun), Sender, From, St);
	true when is_function(StrOrFun) ->
	    output(minor, StrOrFun(minor), Sender, From, St);
	true ->
	    output(minor, test_server_ctrl:escape_chars(StrOrFun),
		   Sender, From, St);
	false ->
	    ok
    end;
output(stdout, Str, _Sender, From, St) ->
    output_to_file(stdout, Str, From, St);
output(html, Str, _Sender, From, St) ->
    output_to_file(html, Str, From, St);
output(Level, Str, Sender, From, St) when is_atom(Level) ->
    output_to_file(Level, dress_output(Str, Sender, St), From, St).

output_to_file(minor, Data0, From, #st{tc={M,F,A},minor=none}) ->
    Data = [io_lib:format("=== ~w:~tw/~w\n", [M,F,A]),Data0],
    test_server_io:print(From, unexpected_io, Data),
    ok;
output_to_file(minor, Data, From, #st{tc=TC,minor=Fd}) ->
    try
	io:put_chars(Fd, Data)
    catch
	Type:Reason ->
	    Data1 =
		[io_lib:format("=== ERROR === TC: ~tw\n"
			       "Failed to write to minor Fd: ~w\n"
			       "Type: ~w\n"
			       "Reason: ~tw\n",
			       [TC,Fd,Type,Reason]),
		 Data,"\n"],
	    test_server_io:print(From, unexpected_io, Data1)
    end;
output_to_file(Detail, Data, From, _) ->
    test_server_io:print(From, Detail, Data).

is_io_permitted(From, #st{reject_io=true,permit_io=P}) ->
    gb_sets:is_member(From, P);
is_io_permitted(_, #st{reject_io=false}) -> true.

selected_by_level(Level, stdout, #st{levels={Stdout,_,_}}) ->
    Level =< Stdout;
selected_by_level(Level, major, #st{levels={_,Major,_}}) ->
    Level =< Major;
selected_by_level(Level, minor, #st{levels={_,_,Minor}}) ->
    Level >= Minor.

dress_output([$=|_]=Str, internal, _) ->
    [Str,$\n];
dress_output(Str, internal, _) ->
    ["=== ",Str,$\n];
dress_output(Str, _, #st{auto_nl=AutoNL}) ->
    case AutoNL of
	true -> [Str,$\n];
	false -> Str
    end.
