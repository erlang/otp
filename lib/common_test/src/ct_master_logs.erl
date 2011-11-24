%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% @doc Logging functionality for Common Test Master.
%%%
%%% <p>This module implements a logger for the master
%%% node.</p>
-module(ct_master_logs).

-export([start/2, make_all_runs_index/0, log/3, nodedir/2, stop/0]).

-record(state, {log_fd, start_time, logdir, rundir, 
		nodedir_ix_fd, nodes, nodedirs=[]}).

-define(ct_master_log_name, "ct_master_log.html").
-define(all_runs_name, "master_runs.html").
-define(nodedir_index_name, "index.html").
-define(details_file_name,"details.info").
-define(table_color,"lightblue").

%%%--------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------

start(LogDir,Nodes) ->
    Self = self(),
    Pid = spawn_link(fun() -> init(Self,LogDir,Nodes) end),
    MRef = erlang:monitor(process,Pid),
    receive 
	{started,Pid,Result} -> 
	    erlang:demonitor(MRef, [flush]),
	    {Pid,Result};
	{'DOWN',MRef,process,_,Reason} ->
	    exit({could_not_start_process,?MODULE,Reason})
    end.

log(Heading,Format,Args) ->
    cast({log,self(),[{int_header(),[log_timestamp(now()),Heading]},
		      {Format,Args},
		      {int_footer(),[]}]}),
    ok.

make_all_runs_index() ->
    call(make_all_runs_index).

nodedir(Node,RunDir) ->
    call({nodedir,Node,RunDir}).

stop() ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    MRef = erlang:monitor(process,Pid),
	    ?MODULE ! stop,
	    receive
		{'DOWN',MRef,process,_,_} ->
		    ok
	    end;
	undefined ->
	    ok
    end,
    ok.

%%%--------------------------------------------------------------------
%%% Logger process
%%%--------------------------------------------------------------------

init(Parent,LogDir,Nodes) ->
    register(?MODULE,self()),
    Time = calendar:local_time(),
    RunDir = make_dirname(Time),
    RunDirAbs = filename:join(LogDir,RunDir),
    file:make_dir(RunDirAbs),
    write_details_file(RunDirAbs,{node(),Nodes}),
    make_all_runs_index(LogDir),
    CtLogFd = open_ct_master_log(RunDirAbs),
    NodeStr = 
	lists:flatten(lists:map(fun(N) ->
					atom_to_list(N) ++ " "
				end,Nodes)),

    io:format(CtLogFd,int_header(),[log_timestamp(now()),"Test Nodes\n"]),
    io:format(CtLogFd,"~s\n",[NodeStr]),
    io:format(CtLogFd,int_footer()++"\n",[]),

    NodeDirIxFd = open_nodedir_index(RunDirAbs,Time),
    Parent ! {started,self(),{Time,RunDirAbs}},
    loop(#state{log_fd=CtLogFd,
		start_time=Time,
		logdir=LogDir,
		rundir=RunDirAbs,
		nodedir_ix_fd=NodeDirIxFd,
		nodes=Nodes,
		nodedirs=lists:map(fun(N) ->
					   {N,""}
				   end,Nodes)}).

loop(State) ->
    receive
	{log,_From,List} ->
	    Fd = State#state.log_fd,
	    Fun = 
		fun({Str,Args}) ->
			case catch io:format(Fd,Str++"\n",Args) of
			    {'EXIT',Reason} ->
				io:format(Fd, 
					  "Logging fails! Str: ~p, Args: ~p~n",
					  [Str,Args]),
				exit({logging_failed,Reason}),
				ok;
			    _ ->
				ok
			end
		end,
	    lists:foreach(Fun,List),
	    loop(State);
	{make_all_runs_index,From} ->
	    make_all_runs_index(State#state.logdir),
	    return(From,State#state.logdir),
	    loop(State);	
	{{nodedir,Node,RunDir},From} ->
	    print_nodedir(Node,RunDir,State#state.nodedir_ix_fd),
	    return(From,ok),
	    loop(State);	
	stop ->
	    make_all_runs_index(State#state.logdir),
	    io:format(State#state.log_fd,
		      int_header()++int_footer(),
		      [log_timestamp(now()),"Finished!"]),
	    close_ct_master_log(State#state.log_fd),
	    close_nodedir_index(State#state.nodedir_ix_fd),
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Master Log functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open_ct_master_log(Dir) ->
    FullName = filename:join(Dir,?ct_master_log_name),
    {ok,Fd} = file:open(FullName,[write]),
    io:format(Fd,header("Common Test Master Log"),[]),
    %% maybe add config info here later
    io:format(Fd, config_table([]), []),
    io:format(Fd,
	      "<style>\n"
	      "div.ct_internal { background:lightgrey; color:black }\n"
	      "div.default     { background:lightgreen; color:black }\n"
	      "</style>\n",
	      []),
    io:format(Fd, 
	      "<br><h2>Progress Log</h2>\n"
	      "<pre>\n",[]),
    Fd.

close_ct_master_log(Fd) ->
    io:format(Fd,"</pre>",[]),
    io:format(Fd,footer(),[]),
    file:close(Fd).

config_table(Vars) ->
    [config_table_header()|config_table1(Vars)].

config_table_header() ->
    ["<h2>Configuration</h2>\n",
     "<table border=\"3\" cellpadding=\"5\" bgcolor=\"",?table_color,
     "\"\n",
     "<tr><th>Key</th><th>Value</th></tr>\n"].

%%
%% keep for possible later use
%%
%%config_table1([{Key,Value}|Vars]) ->
%%    ["<tr><td>", atom_to_list(Key), "</td>\n",
%%     "<td><pre>",io_lib:format("~p",[Value]),"</pre></td></tr>\n" | 
%%     config_table1(Vars)];

config_table1([]) ->
    ["</table>\n"].

int_header() ->
    "<div class=\"ct_internal\"><b>*** CT MASTER ~s *** ~s</b>".
int_footer() ->
    "</div>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% NodeDir Index functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open_nodedir_index(Dir,StartTime) ->
    FullName = filename:join(Dir,?nodedir_index_name),
    {ok,Fd} = file:open(FullName,[write]),
    io:format(Fd,nodedir_index_header(StartTime),[]),
    Fd.

print_nodedir(Node,RunDir,Fd) ->
    Index = filename:join(RunDir,"index.html"),
    io:format(Fd,
	      ["<TR>\n"
	       "<TD ALIGN=center>",atom_to_list(Node),"</TD>\n",
	       "<TD ALIGN=left><A HREF=\"",Index,"\">",Index,"</A></TD>\n",
	       "</TR>\n"],[]),
    ok.

close_nodedir_index(Fd) ->
    io:format(Fd,index_footer(),[]),
    file:close(Fd).

nodedir_index_header(StartTime) ->
    [header("Log Files " ++ format_time(StartTime)) |
     ["<CENTER>\n",
      "<P><A HREF=\"",?ct_master_log_name,"\">Common Test Master Log</A></P>",
      "<TABLE border=\"3\" cellpadding=\"5\" ",
      "BGCOLOR=\"",?table_color,"\">\n",
      "<th><B>Node</B></th>\n",
      "<th><B>Log</B></th>\n",
      "\n"]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All Run Index functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_all_runs_index(LogDir) ->
    FullName = filename:join(LogDir,?all_runs_name),
    Match = filename:join(LogDir,logdir_prefix()++"*.*"),
    Dirs = filelib:wildcard(Match),
    DirsSorted = (catch sort_all_runs(Dirs)),
    Header = all_runs_header(),
    Index = [runentry(Dir) || Dir <- DirsSorted],
    Result = file:write_file(FullName,Header++Index++index_footer()),
    Result.    

sort_all_runs(Dirs) ->
    %% sort on time string, always last and on the format:
    %% "YYYY-MM-DD_HH.MM.SS"
    KeyList =
	lists:map(fun(Dir) ->
			  case lists:reverse(string:tokens(Dir,[$.,$_])) of
			      [SS,MM,HH,Date|_] ->
				  {{Date,HH,MM,SS},Dir};
			      _Other ->
				  throw(Dirs)
			  end
		  end,Dirs),
    lists:reverse(lists:map(fun({_,Dir}) ->
				    Dir
			    end,lists:keysort(1,KeyList))).

runentry(Dir) ->
    {MasterStr,NodesStr} = 
	case read_details_file(Dir) of
	    {Master,Nodes} when is_list(Nodes) ->
		[_,Host] = string:tokens(atom_to_list(Master),"@"),
		NodesList = 
		    lists:reverse(lists:map(fun(N) ->
						    atom_to_list(N) ++ ", "
					    end,Nodes)),
		case NodesList of
		    [N|NListR] ->
			N1 = string:sub_string(N,1,length(N)-2),
			{Host,lists:flatten(lists:reverse([N1|NListR]))};
		    [] ->
			{Host,""}
		end;
	    _Error ->
		{"unknown",""}
	end,
    Index = filename:join(Dir,?nodedir_index_name),
    ["<TR>\n"
     "<TD ALIGN=center><A HREF=\"",Index,"\">",timestamp(Dir),"</A></TD>\n",
     "<TD ALIGN=center>",MasterStr,"</TD>\n",
     "<TD ALIGN=center>",NodesStr,"</TD>\n",
     "</TR>\n"].

all_runs_header() ->
    [header("Master Test Runs") |
     ["<CENTER>\n",
      "<TABLE border=\"3\" cellpadding=\"5\" "
      "BGCOLOR=\"",?table_color,"\">\n"
      "<th><B>History</B></th>\n"
      "<th><B>Master Host</B></th>\n"
      "<th><B>Test Nodes</B></th>\n"
      "\n"]].

timestamp(Dir) ->
    [S,Min,H,D,M,Y|_] = lists:reverse(string:tokens(Dir,".-_")),
    [S1,Min1,H1,D1,M1,Y1] = [list_to_integer(N) || N <- [S,Min,H,D,M,Y]],
    format_time({{Y1,M1,D1},{H1,Min1,S1}}).

write_details_file(Dir,Details) ->
    FullName = filename:join(Dir,?details_file_name),
    force_write_file(FullName,term_to_binary(Details)).

read_details_file(Dir) ->
    FullName = filename:join(Dir,?details_file_name),
    case file:read_file(FullName) of
	{ok,Bin} ->
	    binary_to_term(Bin);
	Error ->
	    Error
    end.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

header(Title) ->
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
     "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
     "<HTML>\n",
     "<HEAD>\n",

     "<TITLE>" ++ Title ++ "</TITLE>\n",
     "<META HTTP-EQUIV=\"CACHE-CONTROL\" CONTENT=\"NO-CACHE\">\n",

     "</HEAD>\n",

     body_tag(),

     "<!-- ---- DOCUMENT TITLE  ---- -->\n",

     "<CENTER>\n",
     "<H1>" ++ Title ++ "</H1>\n",
     "</CENTER>\n",

     "<!-- ---- CONTENT ---- -->\n"].

index_footer() ->
    ["</TABLE>\n"
     "</CENTER>\n" | footer()].

footer() ->
     ["<P><CENTER>\n"
     "<HR>\n"
     "<P><FONT SIZE=-1>\n"
     "Copyright &copy; ", year(),
     " <A HREF=\"http://erlang.ericsson.se\">Open Telecom Platform</A><BR>\n"
     "Updated: <!date>", current_time(), "<!/date><BR>\n"
     "</FONT>\n"
     "</CENTER>\n"
     "</body>\n"].

body_tag() ->
    "<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\""
	"vlink=\"#800080\" alink=\"#FF0000\">\n".

current_time() ->
    format_time(calendar:local_time()).

format_time({{Y, Mon, D}, {H, Min, S}}) ->
    Weekday = weekday(calendar:day_of_the_week(Y, Mon, D)),
    lists:flatten(io_lib:format("~s ~s ~p ~w ~2.2.0w:~2.2.0w:~2.2.0w",
				[Weekday, month(Mon), D, Y, H, Min, S])).

weekday(1) -> "Mon";
weekday(2) -> "Tue";
weekday(3) -> "Wed";
weekday(4) -> "Thu";
weekday(5) -> "Fri";
weekday(6) -> "Sat";
weekday(7) -> "Sun".

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

year() ->
    {Y, _, _} = date(),
    integer_to_list(Y).


make_dirname({{YY,MM,DD},{H,M,S}}) -> 
    io_lib:format(logdir_prefix()++".~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w",
		  [YY,MM,DD,H,M,S]).

logdir_prefix() ->
    "ct_master_run".

log_timestamp(Now) ->
    put(log_timestamp,Now),
    {_,{H,M,S}} = calendar:now_to_local_time(Now),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				[H,M,S])).

force_write_file(Name,Contents) ->
    force_delete(Name),
    file:write_file(Name,Contents).

force_delete(Name) ->
    case file:delete(Name) of
	{error,eacces} ->
	    force_rename(Name,Name++".old.",0);
	Other ->
	    Other
    end.

force_rename(From,To,Number) ->
    Dest = [To|integer_to_list(Number)],
    case file:read_file_info(Dest) of
	{ok,_} ->
	    force_rename(From,To,Number+1);
	{error,_} ->
	    file:rename(From,Dest)
    end.

call(Msg) ->
    case whereis(?MODULE) of
	undefined ->
	    {error,does_not_exist};
	Pid ->
	    MRef = erlang:monitor(process,Pid),
	    Ref = make_ref(),
	    ?MODULE ! {Msg,{self(),Ref}},
	    receive
		{Ref, Result} -> 
		    erlang:demonitor(MRef, [flush]),
		    Result;
		{'DOWN',MRef,process,_,Reason}  -> 
		    {error,{process_down,?MODULE,Reason}}
	    end
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result}.

cast(Msg) ->
    case whereis(?MODULE) of
	undefined ->
	    {error,does_not_exist};
	_Pid ->
	    ?MODULE ! Msg
    end.
