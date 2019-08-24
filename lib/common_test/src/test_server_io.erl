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
%% This module implements a process with the registered name 'test_server_io',
%% which has two main responsibilities:
%%
%%   * Manage group leader processes (see the test_server_gl module)
%%   for test cases. A group_leader process is obtained by calling
%%   get_gl/1. Group leader processes will be kept alive as along as
%%   the 'test_server_io' process is alive.
%%
%%   * Handle output to the common log files (stdout, major, html,
%%   unexpected_io).
%%

-module(test_server_io).
-export([start_link/0,stop/1,get_gl/1,set_fd/2,
	 start_transaction/0,end_transaction/0,
	 print_buffered/1,print/3,print_unexpected/1,
	 set_footer/1,set_job_name/1,set_gl_props/1,
	 reset_state/0,finish/0]).

-export([init/1,handle_call/3,handle_info/2,terminate/2]).

-record(st, {fds,			    % Singleton fds (gb_tree)
	     tags=[],                       % Known tag types   
	     shared_gl :: pid(),	    % Shared group leader
	     gls,			    % Group leaders (gb_set)
	     io_buffering=false,	    % I/O buffering
	     buffered,			    % Buffered I/O requests
	     html_footer,		    % HTML footer
	     job_name,			    % Name of current job.
	     gl_props,			    % Properties for GL
	     phase,                         % Indicates current mode
	     offline_buffer,                % Buffer I/O during startup
	     stopping,                      % Reply to when process stopped
	     pending_ops                    % Perform when process idle 
	    }).

start_link() ->
    case whereis(?MODULE) of
	undefined ->
	    case gen_server:start_link({local,?MODULE}, ?MODULE, [], []) of
		{ok,Pid} ->
		    {ok,Pid};
		Other ->
		    Other
	    end;
	Pid ->
	    %% already running, reset the state
	    reset_state(),
	    {ok,Pid}
    end.

stop(FilesToClose) ->
    OldGL = group_leader(),
    group_leader(self(), self()),
    req({stop,FilesToClose}),
    group_leader(OldGL, self()),
    ok.

finish() ->
    req(finish).

%% get_gl(Shared) -> Pid
%%  Shared = boolean()
%%  Pid = pid()
%%
%%  Return a group leader (a process using the test_server_gl module).
%%  If Shared is true, the shared group leader is returned (suitable for
%%  running sequential test cases), otherwise a new group leader process
%%  is spawned. Group leader processes will live until the
%%  'test_server_io' process is stopped.

get_gl(Shared) when is_boolean(Shared) ->
    req({get_gl,Shared}).

%% set_fd(Tag, Fd) -> ok.
%%  Tag = major | html | unexpected_io
%%  Fd = a file descriptor (as returned by file:open/2)
%%
%%  Associate a file descriptor with the given Tag. This
%%  Tag can later be used in when calling to print/3.

set_fd(Tag, Fd) ->
    req({set_fd,Tag,Fd}).

%% start_transaction()
%%
%%  Subsequent calls to print/3 from the process executing start_transaction/0
%%  will cause the messages to be buffered instead of printed directly.

start_transaction() ->
    req({start_transaction,self()}).

%% end_transaction()
%%
%%  End the transaction started by start_transaction/0. Subsequent calls to
%%  print/3 will cause the message to be printed directly.

end_transaction() ->
    req({end_transaction,self()}).

%% print(From, Tag, Msg)
%%  From = pid()
%%  Tag = stdout, or any tag that has been registered using set_fd/2
%%  Msg = string or iolist
%%
%%  Either print Msg to the file identified by Tag, or buffer the message
%%  start_transaction/0 has been called from the process From.
%%
%%  NOTE: The tags have various special meanings. For example, 'html'
%%  is assumed to be a HTML file.

print(From, Tag, Msg) ->
    req({print,From,Tag,Msg}).

%% print_buffered(Pid)
%%  Pid = pid()
%%
%%  Print all messages buffered in the *first* transaction buffered for Pid.
%%  (If start_transaction/0 and end_transaction/0 has been called N times,
%%  print_buffered/1 must be called N times to print all transactions.)

print_buffered(Pid) ->
    req({print_buffered,Pid}).

%% print_unexpected(Msg)
%%  Msg = string or iolist
%%
%%  Print the given string in the unexpected_io log.

print_unexpected(Msg) ->
    print(xxxFrom,unexpected_io,Msg).

%% set_footer(IoData)
%%
%%  Set a footer for the file associated with the 'html' tag.
%%  It will be used by print/3 to print a footer for the HTML file.

set_footer(Footer) ->
    req({set_footer,Footer}).

%% set_job_name(Name)
%%
%%  Set a name for the currently running job. The name will be used
%%  when printing to 'stdout'.
%%

set_job_name(Name) ->
    req({set_job_name,Name}).

%% set_gl_props(PropList)
%%
%%  Set properties for group leader processes. When a group_leader process
%%  is created, test_server_gl:set_props(PropList) will be called.

set_gl_props(PropList) ->
    req({set_gl_props,PropList}).

%% reset_state
%%
%% Reset the initial state
reset_state() ->
    req(reset_state).

%%% Internal functions.

init([]) ->
    process_flag(trap_exit, true),
    ct_util:mark_process(),
    Empty = gb_trees:empty(),
    {ok,Shared} = test_server_gl:start_link(self()),
    {ok,#st{fds=Empty,shared_gl=Shared,gls=gb_sets:empty(),
	    io_buffering=gb_sets:empty(),
	    buffered=Empty,
	    html_footer="</body>\n</html>\n",
	    job_name="<name not set>",
	    gl_props=[],
	    phase=starting,
	    offline_buffer=[],
	    pending_ops=[]}}.

req(Req) ->
    gen_server:call(?MODULE, Req, infinity).

handle_call({get_gl,false}, _From, #st{gls=Gls,gl_props=Props}=St) ->
    {ok,Pid} = test_server_gl:start_link(self()),
    test_server_gl:set_props(Pid, Props),
    {reply,Pid,St#st{gls=gb_sets:insert(Pid, Gls)}};
handle_call({get_gl,true}, _From, #st{shared_gl=Shared}=St) ->
    {reply,Shared,St};
handle_call({set_fd,Tag,Fd}, _From, #st{fds=Fds0,tags=Tags0,
					offline_buffer=OfflineBuff}=St) ->
    Fds = gb_trees:enter(Tag, Fd, Fds0),
    St1 = St#st{fds=Fds,tags=[Tag|lists:delete(Tag, Tags0)]},
    OfflineBuff1 =
	if OfflineBuff == [] ->
		[];
	   true ->
		%% Fd ready, print anything buffered for associated Tag
		lists:filtermap(fun({T,From,Str}) when T == Tag ->
					_ = output(From, Tag, Str, St1),
					false;
				   (_) ->
					true
				end, lists:reverse(OfflineBuff))
	end,
    {reply,ok,St1#st{phase=started,
		     offline_buffer=lists:reverse(OfflineBuff1)}};
handle_call({start_transaction,Pid}, _From, #st{io_buffering=Buffer0,
						buffered=Buf0}=St) ->
    Buf = case gb_trees:is_defined(Pid, Buf0) of
	      false -> gb_trees:insert(Pid, queue:new(), Buf0);
	      true -> Buf0
	  end,
    Buffer = gb_sets:add(Pid, Buffer0),
    {reply,ok,St#st{io_buffering=Buffer,buffered=Buf}};
handle_call({print,From,Tag,Str}, _From, St0) ->
    St = output(From, Tag, Str, St0),
    {reply,ok,St};
handle_call({end_transaction,Pid}, _From, #st{io_buffering=Buffer0,
					      buffered=Buffered0}=St0) ->
    Q0 = gb_trees:get(Pid, Buffered0),
    Q = queue:in(eot, Q0),
    Buffered = gb_trees:update(Pid, Q, Buffered0),
    Buffer = gb_sets:delete_any(Pid, Buffer0),
    St = St0#st{io_buffering=Buffer,buffered=Buffered},
    {reply,ok,St};
handle_call({print_buffered,Pid}, _From, #st{buffered=Buffered0}=St0) ->
    Q0 = gb_trees:get(Pid, Buffered0),
    Q = do_print_buffered(Q0, St0),
    Buffered = gb_trees:update(Pid, Q, Buffered0),
    St = St0#st{buffered=Buffered},
    {reply,ok,St};
handle_call({set_footer,Footer}, _From, St) ->
    {reply,ok,St#st{html_footer=Footer}};
handle_call({set_job_name,Name}, _From, St) ->
    {reply,ok,St#st{job_name=Name}};
handle_call({set_gl_props,Props}, _From, #st{shared_gl=Shared}=St) ->
    test_server_gl:set_props(Shared, Props),
    {reply,ok,St#st{gl_props=Props}};
handle_call(reset_state, From, #st{phase=stopping,pending_ops=Ops}=St) ->
    %% can't reset during stopping phase, save op for later
    Op = fun(NewSt) -> 
		 {_,Result,NewSt1} = handle_call(reset_state, From, NewSt),
		 {Result,NewSt1}
	 end,
    {noreply,St#st{pending_ops=[{From,Op}|Ops]}};
handle_call(reset_state, _From, #st{fds=Fds,tags=Tags,shared_gl=Shared0,gls=Gls,
				    offline_buffer=OfflineBuff}) ->
    %% close open log files
    lists:foreach(fun(Tag) ->
			  case gb_trees:lookup(Tag, Fds) of
			      none ->
				  ok;
			      {value,Fd} ->
				  file:close(Fd)
			  end
		  end, Tags),
    test_server_gl:stop(Shared0),
    GlList = gb_sets:to_list(Gls),
    _ = [test_server_gl:stop(GL) || GL <- GlList],
    timer:sleep(100),
    case lists:filter(fun(GlPid) -> is_process_alive(GlPid) end, GlList) of
	[] ->
	    ok;
	_ ->
	    timer:sleep(2000),
	    [exit(GL, kill) || GL <- GlList],
	    ok
    end,
    Empty = gb_trees:empty(),
    {ok,Shared} = test_server_gl:start_link(self()),
    {reply,ok,#st{fds=Empty,shared_gl=Shared,gls=gb_sets:empty(),
		  io_buffering=gb_sets:empty(),
		  buffered=Empty,
		  html_footer="</body>\n</html>\n",
		  job_name="<name not set>",
		  gl_props=[],
		  phase=starting,
		  offline_buffer=OfflineBuff,
		  pending_ops=[]}};
handle_call({stop,FdTags}, From, #st{fds=Fds0,tags=Tags0,
				     shared_gl=SGL,gls=Gls0}=St0) ->
    St = St0#st{gls=gb_sets:insert(SGL, Gls0),phase=stopping,stopping=From},
    gc(St),
    %% close open log files
    {Fds1,Tags1} = lists:foldl(fun(Tag, {Fds,Tags}) ->
			       case gb_trees:lookup(Tag, Fds) of
				   none ->
				       {Fds,Tags};
				   {value,Fd} ->
				       _ = file:close(Fd),
				       {gb_trees:delete(Tag, Fds),
					lists:delete(Tag, Tags)}
			       end
		       end, {Fds0,Tags0}, FdTags),
    %% Give the users of the surviving group leaders some
    %% time to finish.
    erlang:send_after(1000, self(), stop_group_leaders),    
    {noreply,St#st{fds=Fds1,tags=Tags1}};
handle_call(finish, From, St) ->
    gen_server:reply(From, ok),
    {stop,normal,St}.

handle_info({'EXIT',Pid,normal}, #st{gls=Gls0,stopping=From}=St) ->
    Gls = gb_sets:delete_any(Pid, Gls0),
    case gb_sets:is_empty(Gls) andalso From =/= undefined of
	true ->
	    %% No more group leaders left.
	    gen_server:reply(From, ok),
	    {noreply,St#st{gls=Gls,phase=stopping,stopping=undefined}};
	false ->
	    %% Wait for more group leaders to finish.
	    {noreply,St#st{gls=Gls,phase=stopping}}
    end;
handle_info({'EXIT',Pid,killed}, #st{gls=Gls0}=St) ->
    %% forced termination of group leader
    {noreply,St#st{gls=gb_sets:delete_any(Pid, Gls0)}};
handle_info({'EXIT',_Pid,Reason}, _St) ->
    exit(Reason);
handle_info(stop_group_leaders, #st{gls=Gls}=St) ->
    %% Stop the remaining group leaders.
    GlPids = gb_sets:to_list(Gls),
    _ = [test_server_gl:stop(GL) || GL <- GlPids],
    timer:sleep(100),
    Wait = 
	case lists:filter(fun(GlPid) -> is_process_alive(GlPid) end, GlPids) of
	    [] -> 0;
	    _  -> 2000
	end,
    erlang:send_after(Wait, self(), kill_group_leaders),
    {noreply,St};
handle_info(kill_group_leaders, #st{gls=Gls,stopping=From,
				    pending_ops=Ops}=St) ->
    _ = [exit(GL, kill) || GL <- gb_sets:to_list(Gls)],
    if From /= undefined ->
	    gen_server:reply(From, ok);
       true ->					% reply has been sent already
	    ok
    end,
    %% we're idle, check if any ops are pending
    St1 = lists:foldr(fun({ReplyTo,Op},NewSt) ->
			      {Result,NewSt1} = Op(NewSt),
			      gen_server:reply(ReplyTo, Result),
			      NewSt1
		      end, St#st{phase=idle,pending_ops=[]}, Ops),
    {noreply,St1};
handle_info(Other, St) ->
    io:format("Ignoring: ~tp\n", [Other]),
    {noreply,St}.

terminate(_, _) ->
    ok.

output(From, Tag, Str, #st{io_buffering=Buffered,buffered=Buf0,
			   phase=Phase,offline_buffer=OfflineBuff}=St) ->
    case gb_sets:is_member(From, Buffered) of
	false ->
	    case do_output(Tag, Str, Phase, St) of
		buffer when length(OfflineBuff)>500 ->
		    %% something's wrong, clear buffer
		    St#st{offline_buffer=[]};
		buffer ->
		    St#st{offline_buffer=[{Tag,From,Str}|OfflineBuff]};
		_ ->
		    St
	    end;
	true ->
	    Q0 = gb_trees:get(From, Buf0),
	    Q = queue:in({Tag,Str}, Q0),
	    Buf = gb_trees:update(From, Q, Buf0),
	    St#st{buffered=Buf}
    end.

do_output(stdout, Str, _, #st{job_name=undefined}) ->
    io:put_chars(Str);
do_output(stdout, Str0, _, #st{job_name=Name}) ->
    Str = io_lib:format("Testing ~ts: ~ts\n", [Name,Str0]),
    io:put_chars(Str);
do_output(Tag, Str, Phase, #st{fds=Fds}=St) ->
    case gb_trees:lookup(Tag, Fds) of
	none when Phase /= started ->
	    buffer;
	none ->
	    S = io_lib:format("\n*** ERROR: ~w, line ~w: No known '~tp' log file\n",
			      [?MODULE,?LINE,Tag]),
	    do_output(stdout, [S,Str], Phase, St);
	{value,Fd} ->
	    try
		io:put_chars(Fd, Str),
		case Tag of
		    html -> finalise_table(Fd, St);
		    _ -> ok
		end
	    catch _:Error ->
		    S = io_lib:format("\n*** ERROR: ~w, line ~w: Error writing to "
				      "log file '~tp': ~tp\n",
				      [?MODULE,?LINE,Tag,Error]),
		    do_output(stdout, [S,Str], Phase, St)
	    end
    end.

finalise_table(Fd, #st{html_footer=Footer}) ->
    case file:position(Fd, {cur,0}) of
	{ok,Pos} ->
	    %% We are writing to a seekable file. Finalise so
	    %% we get complete valid (and viewable) HTML code.
	    %% Then rewind to overwrite the finalising code.
	    io:put_chars(Fd, ["\n</table>\n",Footer]),
	    file:position(Fd, Pos);
	{error,epipe} ->
	    %% The file is not seekable.  We cannot erase what
	    %% we've already written --- so the reader will
	    %% have to wait until we're done.
	    ok
    end.

do_print_buffered(Q0, St) ->
    Item = queue:get(Q0),
    Q = queue:drop(Q0),
    case Item of
	eot ->
	    Q;
	{Tag,Str} ->
	    _ = do_output(Tag, Str, undefined, St),
	    do_print_buffered(Q, St)
    end.

gc(#st{gls=Gls0}) ->
    InUse0 = [begin
		  case process_info(P, group_leader) of
		      {group_leader,GL} -> GL;
		      undefined -> undefined
		  end
	      end || P <- processes()],
    InUse = ordsets:from_list(InUse0),
    Gls = gb_sets:to_list(Gls0),
    NotUsed = ordsets:subtract(Gls, InUse),
    _ = [test_server_gl:stop(Pid) || Pid <- NotUsed],
    ok.
