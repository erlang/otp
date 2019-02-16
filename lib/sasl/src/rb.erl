%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(rb).

-behaviour(gen_server).

%% External exports
-export([start/0, start/1, stop/0, rescan/0, rescan/1]).
-export([list/0, list/1, log_list/0, log_list/1, show/0, show/1, grep/1, filter/1, filter/2, start_log/1, stop_log/0]).
-export([h/0, help/0]).

%% Internal exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%%%-----------------------------------------------------------------
%%% Report Browser Tool.
%%% Formats Error reports written by log_mf_h
%%%-----------------------------------------------------------------

-record(state, {dir, data, device, max, type, abort, log}).

%%-----------------------------------------------------------------
%% Interface functions.
%% For available options; see print_options().
%%-----------------------------------------------------------------
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup, 
			   {rb_server, {rb, start_link, [Options]},
			    temporary, brutal_kill, worker, [rb]}).

start_link(Options) ->
    gen_server:start_link({local, rb_server}, rb, Options, []).

stop() -> 
    supervisor:terminate_child(sasl_sup, rb_server).

rescan() -> rescan([]).
rescan(Options) ->
    call({rescan, Options}).

list() -> list(all).
list(Type) -> call({list, Type}).

log_list() -> log_list(all).
log_list(Type) -> call({log_list, Type}).

show() -> 
    call(show).

show(Number) when is_integer(Number) -> 
    call({show_number, Number});
show(Type) when is_atom(Type) ->
    call({show_type, Type}).

grep(RegExp) -> call({grep, RegExp}).

filter(Filters) when is_list(Filters) ->
    call({filter, Filters}).

filter(Filters, FDates) when is_list(Filters) andalso is_tuple(FDates) ->
    call({filter, {Filters, FDates}}).

start_log(FileName) -> call({start_log, FileName}).

stop_log() -> call(stop_log).

h() -> help().
help() ->
    io:format("~nReport Browser Tool - usage~n"),
    io:format("===========================~n"),
    io:format("rb:start()         - start the rb_server with default options~n"),
    io:format("rb:start(Options)  - where Options is a list of:~n"),
    print_options(),
    io:format("rb:h()             - print this help~n"),
    io:format("rb:help()          - print this help~n"),
    io:format("rb:list()          - list all reports~n"),
    io:format("rb:list(Type)      - list all reports of type Type~n"),
    io:format("rb:log_list()      - log list of all reports~n"),
    io:format("rb:log_list(Type)  - log list of all reports of type Type~n"),
    io:format("      currently supported types are:~n"),
    print_types(),
    io:format("rb:grep(RegExp)      - print reports containing RegExp.~n"),
    io:format("                     RegExp must be a valid argument for ~n"),
    io:format("                     the function re:run/2 or re:run/3.~n"),
    io:format("rb:filter(Filters) - print reports matching Filters.~n"),
    io:format("                     reports must be proplists.~n"),
    io:format("      Filters is a list of tuples of the following form:~n"),
    print_filters(),
    io:format("rb:filter(Filters, Dates)  -~n"),
    io:format("      same as rb:filter/1 but accepts date ranges to filter reports.~n"),
    io:format("      Dates must be of the following form:~n"),
    print_dates(),
    io:format("rb:rescan()        - rescans the report directory with same~n"),
    io:format("                     options.~n"),
    io:format("rb:rescan(Options) - rescans the report directory with new~n"),
    io:format("                     options. Options is same as in start/1.~n"),
    io:format("rb:show(Number)    - print report no Number~n"),
    io:format("rb:show(Type)      - print all reports of type Type~n"),
    io:format("rb:show()          - print all reports~n"),
    io:format("rb:start_log(File) - redirect all reports to file or io_device~n"),
    io:format("rb:stop_log()      - close the log file and redirect to~n"),
    io:format("                     standard_io~n"),
    io:format("rb:stop            - stop the rb_server~n").

%%-----------------------------------------------------------------
%% Internal functions.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% call(Request) -> Term
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(rb_server, Req, infinity).

%%-----------------------------------------------------------------
%% MAKE SURE THESE TWO FUNCTIONS ARE UPDATED!
%%-----------------------------------------------------------------
print_options() ->
    io:format("      {start_log, FileName}~n"),
    io:format("         - default: standard_io~n"),
    io:format("      {max, MaxNoOfReports}~n"),
    io:format("         - MaxNoOfReports should be an integer or 'all'~n"),
    io:format("         - default: all~n"),
    io:format("      {report_dir, DirString}~n"),
    io:format("         - DirString should be a string without trailing~n"),
    io:format("         - directory delimiter.~n"),
    io:format("         - default: {sasl, error_logger_mf_dir}~n"),
    io:format("      {type, ReportType}~n"),
    io:format("         - ReportType should be a supported type, 'all'~n"),
    io:format("         - or a list of supported types~n"),
    io:format("         - default: all~n"),
    io:format("      {abort_on_error, Bool}~n"),
    io:format("         - Bool: true | false~n"),
    io:format("         - default: false~n").

print_types() ->
    io:format("         - crash_report~n"),
    io:format("         - supervisor_report~n"),
    io:format("         - progress~n"),
    io:format("         - error~n").

print_filters() ->
    io:format("      - {Key, Value}~n"),
    io:format("        includes report containing {Key, Value}~n"),
    io:format("      - {Key, Value, no}~n"),
    io:format("        excludes report containing {Key, Value}~n"),
    io:format("      - {Key, RegExp, re}~n"),
    io:format("        RegExp must be a valid argument for ~n"),
    io:format("        the function re:run/2 or re:run/3.~n"),
    io:format("      - {Key, RegExp, re, no}~n"),
    io:format("        excludes report containing {Key, RegExp}~n").

print_dates() ->
    io:format("      - {StartDate, EndDate}~n"),
    io:format("        StartDate = EndDate = {{Y,M,D},{H,M,S}} ~n"),
    io:format("        prints the reports with date between StartDate and EndDate~n"),
    io:format("      - {StartDate, from}~n"),
    io:format("        prints the reports with date greater than StartDate~n"),
    io:format("      - {EndDate, to}~n"),
    io:format("        prints the reports with date lesser than StartDate~n").

init(Options) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),
    Log = get_option(Options, start_log, standard_io),
    Device = open_log_file(Log),
    Dir = get_report_dir(Options),
    Max = get_option(Options, max, all),
    Type = get_option(Options, type, all),
    Abort = get_option(Options, abort_on_error, false),
    Data = scan_files(Dir ++ "/", Max, Type),
    {ok, #state{dir = Dir ++ "/", data = Data, device = Device,
		max = Max, type = Type, abort = Abort, log = Log}}.

handle_call({rescan, Options}, _From, State) ->
    {Device,Log1} = 
	case get_option(Options, start_log, {undefined}) of
	    {undefined} -> 
		{State#state.device,State#state.log};
	    Log ->
		close_device(State#state.device),
		{open_log_file(Log),Log}
	end,
    Max = get_option(Options, max, State#state.max),
    Type = get_option(Options, type, State#state.type),
    Abort = get_option(Options, abort_on_error, false),
    Data = scan_files(State#state.dir, Max, Type),
    NewState = State#state{data = Data, max = Max, type = Type,
			   device = Device, abort = Abort, log = Log1},
    {reply, ok, NewState};
handle_call(_, _From, #state{data = undefined}) ->
    {reply, {error, no_data}, #state{}};
handle_call({list, Type}, _From, State) ->
    print_list(standard_io, State#state.data, Type),
    {reply, ok, State};
handle_call({log_list, Type}, _From, State) ->
    print_list(State#state.device, State#state.data, Type),
    {reply, ok, State};
handle_call({start_log, FileName}, _From, State) ->
    NewDevice = open_log_file(FileName),
    {reply, ok, State#state{device = NewDevice}};
handle_call(stop_log, _From, State) ->
    close_device(State#state.device),
    {reply, ok, State#state{device = standard_io}};
handle_call({show_number, Number}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_report_by_num(Dir, Data, Number, Device, Abort, Log),
    {reply, ok, State#state{device = NewDevice}};
handle_call({show_type, Type}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_typed_reports(Dir, Data, Type, Device, Abort, Log),
    {reply, ok, State#state{device = NewDevice}};
handle_call(show, _From, State) ->
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_all_reports(Dir, Data, Device, Abort, Log),
    {reply, ok, State#state{device = NewDevice}};
handle_call({grep, RegExp}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    try print_grep_reports(Dir, Data, RegExp, Device, Abort, Log) of
	NewDevice ->
	    {reply, ok, State#state{device = NewDevice}}
    catch
	error:Error ->
	    {reply, {error, Error}, State}
    end;
handle_call({filter, Filters}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    try filter_all_reports(Dir, Data, Filters, Device, Abort, Log) of
	NewDevice ->
	    {reply, ok, State#state{device = NewDevice}}
    catch
	error:Error ->
	    {reply, {error, Error}, State}
    end.

terminate(_Reason, #state{device = Device}) ->
    close_device(Device).

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: open_log_file/1
%% Args: FileName | standard_io
%% Returns: A Device for later use in call to io:format
%%-----------------------------------------------------------------
open_log_file(standard_io) -> standard_io;
open_log_file(Fd) when is_atom(Fd),Fd=/=standard_error -> 
    case whereis(Fd) of
	undefined -> io:format("rb: Registered name not found '~ts'.~n",
		      [Fd]),
	             io:format("rb: Using standard_io~n"),
                     open_log_file(standard_io);
	Pid       -> open_log_file(Pid)	
    end;
open_log_file(Fd) when is_pid(Fd)-> Fd;
open_log_file(FileName) when is_list(FileName) ->
    case file:open(FileName, [write,append,{encoding,utf8}]) of
	{ok, Fd} -> Fd;
	Error -> 
	    io:format("rb: Cannot open file '~ts' (~w).~n",
		      [FileName, Error]),
	    io:format("rb: Using standard_io~n"),
	    standard_io
    end;
open_log_file(standard_error) -> 
	    io:format("rb: Using standard_io~n"),
	    standard_io.

close_device(Fd) when is_pid(Fd) ->
    catch file:close(Fd);
close_device(_) -> ok.

get_option(Options, Key, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

get_report_dir(Options) ->
    case lists:keysearch(report_dir, 1, Options) of
	{value, {_Key, RptDir}} -> RptDir;
	_ ->
	    case catch application:get_env(sasl, error_logger_mf_dir) of
		{ok, Dir} -> Dir;
		_ ->
		    exit("cannot locate report directory")
	    end
    end.

%%-----------------------------------------------------------------
%% Func: scan_files(RptDir, Max, Type)
%% Args: RptDir ::= string().
%%       Max ::= integer() | all, describing how many reports
%5               to read.
%%       Type ::= atom(), describing which reports to read.
%% Purpose: Scan all report files one time, and build a list of
%%          small elements 
%% Returns: Data, where Data is a list of
%%          {Number, Type, ShortDescr, Date, Fname, FilePosition}.
%%-----------------------------------------------------------------
scan_files(RptDir, Max, Type) ->
    case file:open(RptDir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    case catch file:read(Fd, 1) of
		{ok, [LastWritten]} -> 
		    ok = file:close(Fd),
		    Files = make_file_list(RptDir, LastWritten),
		    scan_files(RptDir, Files, Max, Type);		
		_X ->
		    _ = file:close(Fd),
		    exit("cannot read the index file")
	    end;
	_X -> exit("cannot read the index file")
    end.

make_file_list(Dir, FirstFileNo) ->
    case file:list_dir(Dir) of
	{ok, FileNames} ->
	    FileNumbers = lists:zf(fun(Name) ->
					   case catch list_to_integer(Name) of
					       Int when is_integer(Int) ->
						   {true, Int};
					       _ ->
						   false
					   end
				   end,
				   FileNames),
	    shift(lists:sort(FileNumbers), FirstFileNo);
	_ -> exit({bad_directory, Dir})
    end.
					  
shift(List, First) -> 
    shift(List, First, []).

shift([H | T], H, Res) ->
    [H | Res] ++ lists:reverse(T);
shift([H | T], First, Res) ->
    shift(T, First, [H | Res]);
shift([], _, Res) ->
    Res.

%%-----------------------------------------------------------------
%% Func: scan_files(Dir, Files, Max, Type)
%% Args: Files is a list of FileName.
%% Purpose: Scan the report files in the index variable.
%% Returns: {Number, Type, ShortDescr, Date, FileName, FilePosition}
%%-----------------------------------------------------------------
scan_files(Dir, Files, Max, Type) ->
    scan_files(Dir, 1, Files, [], Max, Type).
scan_files(_Dir, _, [], Res, _Max, _Type) -> Res;
scan_files(_Dir, _, _Files, Res, Max, _Type) when Max =< 0 -> Res;
scan_files(Dir, No, [H|T], Res, Max, Type) ->
    Data = get_report_data_from_file(Dir, No, H, Max, Type),
    Len = length(Data),
    NewMax = dec_max(Max, Len),
    NewNo = No + Len,
    NewData = Data ++ Res,
    scan_files(Dir, NewNo, T, NewData, NewMax, Type).

dec_max(all, _) -> all;
dec_max(X,Y) -> X-Y.

get_report_data_from_file(Dir, No, FileNr, Max, Type) ->	
    Fname = integer_to_list(FileNr),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, [read]) of
	{ok, Fd} when is_pid(Fd) -> read_reports(No, Fd, Fname, Max, Type);
	_ -> [{No, unknown, "Can't open file " ++ Fname, "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: read_reports(No, Fd, Fname, Max, Type)
%% Purpose: Read reports from one report file.
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePosition}
%% Note: We have to read all reports, and then check the max-
%%       variable, because the reports are reversed on the file, and
%%       we may need the last ones.
%%-----------------------------------------------------------------
read_reports(No, Fd, Fname, Max, Type) ->
    io:format("rb: reading report..."),
    case catch read_reports(Fd, [], Type) of
	{ok, Res} -> 
	    ok = file:close(Fd),
	    io:format("done.~n"),
	    NewRes = 
		if
		    length(Res) > Max ->
			lists:sublist(Res, 1, Max);
		    true ->
			Res
		end,
	    add_report_data(NewRes, No, Fname);
	{error, [Problem | Res]} ->
	    _ = file:close(Fd),
	    io:format("Error: ~tp~n",[Problem]),
	    io:format("Salvaged ~p entries from corrupt report file ~ts...~n",
		      [length(Res),Fname]),
	    NewRes = 
		if
		    length([Problem|Res]) > Max ->
			lists:sublist([Problem|Res], 1, Max);
		    true ->
			[Problem|Res]
		end,
	    add_report_data(NewRes, No, Fname);
	Else ->
	    io:format("err ~tp~n", [Else]),
	    [{No, unknown, "Can't read reports from file " ++ Fname,
		  "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: add_report_data(Res, No, FName)
%% Args: Res is a list of {Type, ShortDescr, Date, FilePos}
%% Purpose: Convert a list of {Type, ShortDescr, Date, FilePos} to
%%          a list of {No, Type, ShortDescr, Date, FileName, FilePos}
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePos}
%%-----------------------------------------------------------------
add_report_data(Res, No, FName) ->
    add_report_data(Res, No, FName, []).
add_report_data([{Type, ShortDescr, Date, FilePos}|T], No, FName, Res) ->
    add_report_data(T, No+1, FName,
		    [{No, Type, ShortDescr, Date, FName, FilePos}|Res]);
add_report_data([], _No, _FName, Res) -> Res.

read_reports(Fd, Res, Type) ->
    {ok, FilePos} = file:position(Fd, cur),
    case catch read_report(Fd) of
	{ok, Report} -> 
	    RealType = get_type(Report),
	    {ShortDescr, Date} = get_short_descr(Report),
	    Rep = {RealType, ShortDescr, Date, FilePos},
	    if
		Type == all->
		    read_reports(Fd, [Rep | Res], Type);
		RealType == Type ->
		    read_reports(Fd, [Rep | Res], Type);
		is_list(Type) ->
		    case lists:member(RealType, Type) of
			true ->
			    read_reports(Fd, [Rep | Res], Type);
			_ ->
			    read_reports(Fd, Res, Type)
		    end;
		true ->
		    read_reports(Fd, Res, Type)
	    end;
	{error, Error} ->
	    {error, [{unknown, Error, [], FilePos} | Res]};
	eof ->
	    {ok, Res};
	{'EXIT', Reason} ->
	    [{unknown, Reason, [], FilePos} | Res]
    end.

read_report(Fd) ->
    case io:get_chars(Fd,'',2) of
        [Hi,Lo] ->
            Size = get_int16(Hi,Lo),
            case io:get_chars(Fd,'',Size) of
                eof ->
                    {error,"Premature end of file"};
                List ->
                    Bin = list_to_binary(List),
		    Ref = make_ref(),
		    case (catch {Ref,binary_to_term(Bin)}) of
			{'EXIT',_} ->
			    {error, "Incomplete erlang term in log"};
			{Ref,Term} ->
			    {ok, Term}
		    end
	    end;
        eof ->
            eof
    end.
 
get_int16(Hi,Lo) ->
    ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).


%%-----------------------------------------------------------------
%% Update these functions with the reports that should be possible
%% to browse with rb.
%%-----------------------------------------------------------------
get_type({_Time, {error_report, _Pid, {_, crash_report, _}}}) ->
    crash_report;
get_type({_Time, {error_report, _Pid, {_, supervisor_report, _}}}) ->
    supervisor_report;
get_type({_Time, {info_report, _Pid, {_, progress, _}}}) ->
    progress;
get_type({_Time, {Type, _, _}}) -> Type;
get_type(_) -> unknown.

get_short_descr({{Date, Time}, {error_report, Pid, {_, crash_report, Rep}}}) ->
    [OwnRep | _] = Rep,
    Name = 
	case lists:keysearch(registered_name, 1, OwnRep) of
	    {value, {_Key, []}} ->
		case lists:keysearch(initial_call, 1, OwnRep) of
		    {value, {_K, {M,_F,_A}}} -> M;
		    _ -> Pid
		end;
	    {value, {_Key, N}} -> N;
	    _ -> Pid
	end,
    {Name, date_str(Date, Time)};
get_short_descr({{Date, Time}, {error_report, Pid, {_, supervisor_report,Rep}}}) ->
    Name =
	case lists:keysearch(supervisor, 1, Rep) of
	    {value, {_Key, N}} when is_atom(N) -> N;
	    _ -> Pid
	end,
    {Name, date_str(Date,Time)};
get_short_descr({{Date, Time}, {_Type, Pid, _}}) ->
    {Pid, date_str(Date,Time)};
get_short_descr(_) ->
    {'???', "???"}.
    
date_str({Y,Mo,D}=Date,{H,Mi,S}=Time) ->
    case application:get_env(sasl,utc_log) of 
	{ok,true} ->
	    {{YY,MoMo,DD},{HH,MiMi,SS}} = 
		local_time_to_universal_time({Date,Time}),
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w UTC", 
					[YY,MoMo,DD,HH,MiMi,SS]));
	_ ->
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w", 
					[Y,Mo,D,H,Mi,S]))
    end.

local_time_to_universal_time({Date,Time}) ->
    case calendar:local_time_to_universal_time_dst({Date,Time}) of
	[UCT] ->
	    UCT;
	[UCT1,_UCT2] ->
	    UCT1;
	[] -> % should not happen
	    {Date,Time}
    end.


print_list(Fd, Data0, Type) ->
    Modifier = misc_supp:modifier(Fd),
    Header = {"No", "Type", "Process", "Date     Time"},
    {DescrWidth,DateWidth,Data} = find_widths(Data0, Modifier, 7, 13, []),
    Format = lists:concat(["~4s~20s ~", DescrWidth, "s~20s~n"]),
    io:format(Fd, Format, tuple_to_list(Header)),
    io:format(Fd, Format, ["==", "====", "=======", "====     ===="]),
    print_list(Fd, Data, Type, DescrWidth, DateWidth, Modifier).
print_list(_, [], _, _, _, _) -> true;
print_list(Fd, [H|T], Type, Width, DateWidth, Modifier) ->
    print_one_report(Fd, H, Type, Width, DateWidth, Modifier),
    print_list(Fd, T, Type, Width, DateWidth, Modifier).


find_widths([], _Modifier, DescrWidth, DateWidth, Data) ->
    {DescrWidth+1, DateWidth+1, lists:reverse(Data)};
find_widths([H|T], Modifier, DescrWidth, DateWidth, Data) ->
    DescrTerm = element(3,H),
    Descr = io_lib:format("~"++Modifier++"w", [DescrTerm]),
    DescrTry = string:length(Descr),
    NewDescrWidth =
        if
            DescrTry > DescrWidth -> DescrTry;
            true -> DescrWidth
        end,
    DateTry = string:length(element(4, H)),
    NewDateWitdh =
        if
            DateTry > DateWidth -> DateTry;
            true -> DateWidth
        end,
    NewH = setelement(3,H,Descr),
    find_widths(T, Modifier, NewDescrWidth, NewDateWitdh, [NewH|Data]).

print_one_report(Fd, {No, RealType, ShortDescr, Date, _Fname, _FilePos},
		 WantedType,
		 Width, DateWidth, Modifier) ->
    if
	WantedType == all ->
	    print_short_descr(Fd, No, RealType, ShortDescr, Date, Width, 
			      DateWidth, Modifier);
	WantedType == RealType ->
	    print_short_descr(Fd, No, RealType, ShortDescr, Date, Width, 
			      DateWidth, Modifier);
	true -> ok
    end.

print_short_descr(Fd, No, Type, ShortDescr, Date, Width, DateWidth, Modifier) ->
    Format = lists:concat(["~4w~20", Modifier, "w ~", Width, Modifier, "s~",
                           DateWidth, "s~n"]),
    io:format(Fd, Format, [No, Type,  ShortDescr, Date]).

print_report_by_num(Dir, Data, Number, Device, Abort, Log) ->
    {_,Device1} = print_report(Dir, Data, Number, Device, Abort, Log),
    Device1.
    
print_typed_reports(_Dir, [], _Type, Device, _Abort, _Log) ->
    Device;
print_typed_reports(Dir, Data, Type, Device, Abort, Log) ->
    {Next,Device1} = 
	case element(2, hd(Data)) of
	    Type -> 
		print_report(Dir, Data, element(1, hd(Data)), Device, Abort, Log);
	    _ -> 
		{proceed,Device}
	end,
    if Next == abort ->
	    Device1;
       true ->
	    print_typed_reports(Dir, tl(Data), Type, Device1, Abort, Log)
    end.

print_all_reports(_Dir, [], Device, _Abort, _Log) ->
    Device;
print_all_reports(Dir, Data, Device, Abort, Log) ->
    {Next,Device1} = print_report(Dir, Data, element(1, hd(Data)), 
				  Device, Abort, Log),
    if Next == abort ->
	    Device1;
       true ->
	    print_all_reports(Dir, tl(Data), Device1, Abort, Log)
    end.

print_report(Dir, Data, Number, Device, Abort, Log) ->
    case find_report(Data, Number) of
	{Fname, FilePosition} ->
	    FileName = lists:concat([Dir, Fname]),
	    case file:open(FileName, [read]) of
		{ok, Fd} -> 
		    read_rep(Fd, FilePosition, Device, Abort, Log);
		_ -> 
		    io:format("rb: can't open file ~tp~n", [Fname]),
		    {proceed,Device}
	    end;
	no_report ->
	    {proceed,Device}
    end.

find_report([{No, _Type, _Descr, _Date, Fname, FilePosition}|_T], No) ->
    {Fname, FilePosition};
find_report([_H|T], No) -> 
    find_report(T, No);
find_report([], No) ->
    io:format("There is no report with number ~p.~n", [No]),
    no_report.
    
print_grep_reports(_Dir, [], _RegExp, Device, _Abort, _Log) ->
    Device;
print_grep_reports(Dir, Data, RegExp, Device, Abort, Log) ->
    {Next,Device1} = print_grep_report(Dir, Data, element(1, hd(Data)), 
				       Device, RegExp, Abort, Log),
    if Next == abort ->
	    Device1;
       true ->
	    print_grep_reports(Dir, tl(Data), RegExp, Device1, Abort, Log)
    end.

print_grep_report(Dir, Data, Number, Device, RegExp, Abort, Log) ->
    {Fname, FilePosition} = find_report(Data, Number),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, [read]) of
	{ok, Fd} when is_pid(Fd) -> 
	    check_rep(Fd, FilePosition, Device, RegExp, Number, Abort, Log);
	_ -> 
	    io:format("rb: can't open file ~tp~n", [Fname]),
	    {proceed,Device}
    end.

check_rep(Fd, FilePosition, Device, RegExp, Number, Abort, Log) ->
    case read_rep_msg(Fd, FilePosition) of
	{Date, Msg} ->
	    MsgStr = lists:flatten(io_lib:format("~tp",[Msg])),
	    case run_re(MsgStr, RegExp) of
		match ->
		    io:format("Found match in report number ~w~n", [Number]),
		    case catch rb_format_supp:print(Date, Msg, Device) of
			{'EXIT', _} ->
			    handle_bad_form(Date, Msg, Device, Abort, Log);
			_ ->
			    {proceed,Device}
		    end;
		_ ->
		    {proceed,Device}
	    end;
	_ ->
	    io:format("rb: Cannot read from file~n"),
	    {proceed,Device}
    end.

run_re(Subject, {Regexp, Options}) ->
    run_re(Subject, Regexp, Options);
run_re(Subject, Regexp) ->
    run_re(Subject, Regexp, []).

run_re(Subject, Regexp, Options) ->
    case re:run(Subject, Regexp, [unicode|Options--[unicode]]) of
        nomatch ->
            nomatch;
	_ ->
            match
    end.

filter_all_reports(_Dir, [], _Filters, Device, _Abort, _Log) ->
    Device;
filter_all_reports(Dir, Data, Filters, Device, Abort, Log) ->
    {Next,Device1} = filter_report(Dir, Data, Filters, element(1, hd(Data)),
				  Device, Abort, Log),
    if Next == abort ->
	    Device1;
       true ->
	    filter_all_reports(Dir, tl(Data), Filters, Device1, Abort, Log)
    end.

filter_report(Dir, Data, Filters, Number, Device, Abort, Log) ->
    case find_report(Data, Number) of
	{Fname, FilePosition} ->
	    FileName = lists:concat([Dir, Fname]),
	    case file:open(FileName, [read]) of
		{ok, Fd} ->
		    filter_rep(Filters, Fd, FilePosition, Device, Abort, Log);
		_ ->
		    io:format("rb: can't open file ~tp~n", [Fname]),
		    {proceed,Device}
	    end;
	no_report ->
	    {proceed,Device}
    end.

filter_rep({Filters, FDates}, Fd, FilePosition, Device, Abort, Log) ->
    RepMsg = read_rep_msg(Fd, FilePosition),
    case RepMsg of
	{_DateStr, {Date, _Msg}} ->
	    case compare_dates(Date, FDates) of
		true ->
		    print_filter_report(RepMsg, Filters, Device, Abort, Log);
		_ ->
		    {proceed,Device}
	    end;
	_ ->
	    io:format("rb: Cannot read from file~n"),
	    {proceed,Device}
    end;
filter_rep(Filters, Fd, FilePosition, Device, Abort, Log) ->
    RepMsg = read_rep_msg(Fd, FilePosition),
    case RepMsg of
	{Date, Msg} ->
	    print_filter_report({Date, Msg}, Filters, Device, Abort, Log);
	_ ->
	    io:format("rb: Cannot read from file~n"),
	    {proceed,Device}
    end.

filter_report([], _Msg) ->
    true;
filter_report([{Key, Value}|T], Msg) ->
    case proplists:get_value(Key, Msg) of
	Value ->
	    filter_report(T, Msg);
	_ ->
	    false
    end;
filter_report([{Key, Value, no}|T], Msg) ->
    case proplists:get_value(Key, Msg) of
	Value ->
	    false;
	_ ->
	    filter_report(T, Msg)
    end;
filter_report([{Key, RegExp, re}|T], Msg) ->
    case proplists:get_value(Key, Msg) of
	undefined ->
	    false;
	Value ->
	    Subject = lists:flatten(io_lib:format("~tp",[Value])),
	    case run_re(Subject, RegExp) of
		match ->
		    filter_report(T, Msg);
		_ -> false
	    end
    end;
filter_report([{Key, RegExp, re, no}|T], Msg) ->
    case proplists:get_value(Key, Msg) of
	undefined ->
	    true;
	Value ->
	    Subject = lists:flatten(io_lib:format("~tp",[Value])),
	    case run_re(Subject, RegExp) of
		match -> false;
		_ -> filter_report(T, Msg)
	    end
    end.

get_compare_dates(Date, CompareDate) ->
    case application:get_env(sasl, utc_log) of
	{ok, true} ->
	    {local_time_to_universal_time(Date),
	     local_time_to_universal_time(CompareDate)};
	_ ->
	    {Date, CompareDate}
    end.
get_compare_dates(Date, From, To) ->
    case application:get_env(sasl, utc_log) of
	{ok, true} ->
	    {local_time_to_universal_time(Date),
	     local_time_to_universal_time(From),
	     local_time_to_universal_time(To)};
	_ ->
	    {Date, From, To}
    end.

compare_dates(Date, {CompareDate, from}) ->
    {Date2, DateFrom} = get_compare_dates(Date, CompareDate),
    calendar:datetime_to_gregorian_seconds(Date2) >=
	calendar:datetime_to_gregorian_seconds(DateFrom);
compare_dates(Date, {CompareDate, to}) ->
    {Date2, DateTo} = get_compare_dates(Date, CompareDate),
    calendar:datetime_to_gregorian_seconds(Date2) =<
	calendar:datetime_to_gregorian_seconds(DateTo);
compare_dates(Date, {From, To}) ->
    {Date2, DateFrom, DateTo} = get_compare_dates(Date, From, To),
    calendar:datetime_to_gregorian_seconds(Date2) >=
	calendar:datetime_to_gregorian_seconds(DateFrom)
    andalso
    calendar:datetime_to_gregorian_seconds(Date2) =<
	calendar:datetime_to_gregorian_seconds(DateTo).

print_filter_report({Date, Msg}, Filters, Device, Abort, Log) ->
    {_D, M} = Msg,
    {_, _, M2} = M,
    case M2 of
	{_, _, Report} ->
	    case filter_report(Filters, Report) of
		true ->
		    case catch rb_format_supp:print(Date, Msg, Device) of
			{'EXIT', _} ->
			    handle_bad_form(Date, Msg, Device, Abort, Log);
			_ ->
			    {proceed,Device}
		    end;
		_ ->
		    {proceed, Device}
	    end;
	_ ->
	    {proceed,Device}
    end.

read_rep(Fd, FilePosition, Device, Abort, Log) ->
    case read_rep_msg(Fd, FilePosition) of
	{Date, Msg} ->
	    case catch rb_format_supp:print(Date, Msg, Device) of
		{'EXIT', _} ->
		    handle_bad_form(Date, Msg, Device, Abort, Log);
		_ ->
		    {proceed,Device}
	    end;
	_ -> 
	    io:format("rb: Cannot read from file~n"),
	    {proceed,Device}
    end.
    
handle_bad_form(Date, Msg, Device, Abort, Log) ->
    io:format("rb: ERROR! A report on bad form was encountered. " ++
	      "It cannot be printed to the log.~n~n"),
    io:format("Details:~n~p ~tp~n~n", [Date,Msg]),
    case {Abort,Device,open_log_file(Log)} of
	{true,standard_io,standard_io} ->
	    io:format("rb: Logging aborted.~n"),
	    {abort,Device};
	{false,standard_io,standard_io} ->
	    io:format("rb: Logging resumed...~n~n"),
	    {proceed,Device};
	{_,_,standard_io} ->
	    io:format("rb: Can not reopen ~tp. Logging aborted.~n", [Log]),
	    {abort,Device};
	{true,_,NewDevice} ->
	    io:format(NewDevice,
		      "~n~n************************* RB ERROR ************************~n" ++
		      "A report on bad form was encountered here and the logging~n" ++
		      "process was aborted. Note that there may well be remaining~n" ++
		      "reports that haven't yet been logged. Please see the rb~n" ++
		      "manual for more info.~n" ++
		      "***********************************************************~n", []),
	    io:format("rb: Logging aborted.~n"),
	    {abort,NewDevice};
	{false,_,NewDevice} ->
	    io:format(NewDevice, 
		      "~n   ********* RB: UNPRINTABLE REPORT ********~n~n", []),
	    io:format("rb: Logging resumed...~n~n"),	    
	    {proceed,NewDevice}
    end.

read_rep_msg(Fd, FilePosition) ->
    {ok,_} = file:position(Fd, {bof, FilePosition}),
    Res = 
	case catch read_report(Fd) of
	    {ok, Report} ->
		{_ShortDescr, Date} = get_short_descr(Report),
		{Date, Report};
	    _ -> error
	end,
    ok = file:close(Fd),
    Res.
