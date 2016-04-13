%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(c).

%% Utilities to use from shell.

%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([help/0,lc/1,c/1,c/2,nc/1,nc/2, nl/1,l/1,i/0,i/1,ni/0,
         y/1, y/2,
	 lc_batch/0, lc_batch/1,
	 i/3,pid/3,m/0,m/1,
	 bt/1, q/0,
	 erlangrc/0,erlangrc/1,bi/1, flush/0, regs/0, uptime/0,
	 nregs/0,pwd/0,ls/0,ls/1,cd/1,memory/1,memory/0, xm/1]).

-export([display_info/1]).
-export([appcall/4]).

-import(lists, [reverse/1,flatten/1,sublist/3,sort/1,keysort/2,
		concat/1,max/1,min/1,foreach/2,foldl/3,flatmap/2]).
-import(io, [format/1, format/2]).

%%-----------------------------------------------------------------------

-spec help() -> 'ok'.

help() ->
    io:put_chars(<<"bt(Pid)    -- stack backtrace for a process\n"
		   "c(File)    -- compile and load code in <File>\n"
		   "cd(Dir)    -- change working directory\n"
		   "flush()    -- flush any messages sent to the shell\n"
		   "help()     -- help info\n"
		   "i()        -- information about the system\n"
		   "ni()       -- information about the networked system\n"
		   "i(X,Y,Z)   -- information about pid <X,Y,Z>\n"
		   "l(Module)  -- load or reload module\n"
		   "lc([File]) -- compile a list of Erlang modules\n"
		   "ls()       -- list files in the current directory\n"
		   "ls(Dir)    -- list files in directory <Dir>\n"
		   "m()        -- which modules are loaded\n"
		   "m(Mod)     -- information about module <Mod>\n"
		   "memory()   -- memory allocation information\n"
		   "memory(T)  -- memory allocation information of type <T>\n"
		   "nc(File)   -- compile and load code in <File> on all nodes\n"
		   "nl(Module) -- load module on all nodes\n"
		   "pid(X,Y,Z) -- convert X,Y,Z to a Pid\n"
		   "pwd()      -- print working directory\n"
		   "q()        -- quit - shorthand for init:stop()\n"
		   "regs()     -- information about registered processes\n"
		   "nregs()    -- information about all registered processes\n"
		   "uptime()   -- print node uptime\n"
		   "xm(M)      -- cross reference check a module\n"
		   "y(File)    -- generate a Yecc parser\n">>).

%% c(FileName)
%%  Compile a file/module.

-spec c(File) -> {'ok', Module} | 'error' when
      File :: file:name(),
      Module :: module().

c(File) -> c(File, []).

-spec c(File, Options) -> {'ok', Module} | 'error' when
      File :: file:name(),
      Options :: [compile:option()],
      Module :: module().

c(File, Opts0) when is_list(Opts0) ->
    Opts = [report_errors,report_warnings|Opts0],
    case compile:file(File, Opts) of
	{ok,Mod} ->				%Listing file.
	    machine_load(Mod, File, Opts);
	{ok,Mod,_Ws} ->				%Warnings maybe turned on.
	    machine_load(Mod, File, Opts);
	Other ->				%Errors go here
	    Other
    end;
c(File, Opt) -> 
    c(File, [Opt]).

%%% Obtain the 'outdir' option from the argument. Return "." if no
%%% such option was given.
-spec outdir([compile:option()]) -> file:filename().

outdir([]) ->
    ".";
outdir([Opt|Rest]) ->
    case Opt of
	{outdir, D} ->
	    D;
	_ ->
	    outdir(Rest)
    end.

%%% We have compiled File with options Opts. Find out where the
%%% output file went to, and load it.
machine_load(Mod, File, Opts) ->
    Dir = outdir(Opts),
    File2 = filename:join(Dir, filename:basename(File, ".erl")),
    case compile:output_generated(Opts) of
	true ->
	    Base = atom_to_list(Mod),
	    case filename:basename(File, ".erl") of
		Base ->
		    code:purge(Mod),
		    check_load(code:load_abs(File2,Mod), Mod);
		_OtherMod ->
		    format("** Module name '~p' does not match file name '~tp' **~n",
			   [Mod,File]),
		    {error, badfile}
	    end;
	false ->
	    format("** Warning: No object file created - nothing loaded **~n", []),
	    ok
    end.

%%% This function previously warned if the loaded module was
%%% loaded from some other place than current directory.
%%% Now, loading from other than current directory is supposed to work.
%%% so this function does nothing special.
check_load({error, _R} = Error, _) -> Error;
check_load(_, Mod) -> {ok, Mod}.

%% Compile a list of modules
%% enables the nice unix shell cmd
%% erl -s c lc f1 f2 f3 @d c1=v1 @c2 @i IDir @o ODir -s erlang halt
%% to compile files f1.erl , f2.erl ....... from a unix shell
%% with constant c2 defined, c1=v1 (v1 must be a term!), include dir
%% IDir, outdir ODir.

-spec lc(Files) -> 'ok' | 'error' when
      Files :: [File :: erl_compile:cmd_line_arg()].

lc(Args) ->
    case catch split(Args, [], []) of
	error -> error;
	{Opts, Files} ->
	    COpts = [report_errors, report_warnings | reverse(Opts)],
	    foreach(fun(File) -> compile:file(File, COpts) end, reverse(Files))
    end.

%%% lc_batch/1 works like lc/1, but halts afterwards, with appropriate
%%% exit code. This is meant to be called by "erl -compile".

-spec lc_batch() -> no_return().

lc_batch() ->
    io:format("Error: no files to compile~n"),
    halt(1).

-spec lc_batch([erl_compile:cmd_line_arg()]) -> no_return().

lc_batch(Args) ->
    try split(Args, [], []) of
	{Opts, Files} ->
	    COpts = [report_errors, report_warnings | reverse(Opts)],
            Res = [compile:file(File, COpts) || File <- reverse(Files)],
	    case lists:member(error, Res) of
		true ->
		    halt(1);
		false ->
		    halt(0)
	    end
    catch
	throw:error -> halt(1)
    end.

split(['@i', Dir | T], Opts, Files) ->
    split(T, [{i, atom_to_list(Dir)} | Opts], Files);
split(['@o', Dir | T], Opts, Files) ->
    split(T, [{outdir, atom_to_list(Dir)} | Opts], Files);
split(['@d', Def | T], Opts, Files) ->
    split(T, [split_def(atom_to_list(Def), []) | Opts], Files);
split([File | T], Opts, Files) ->
    split(T, Opts, [File | Files]);
split([], Opts, Files) ->
    {Opts, Files}.

split_def([$= | T], Res) -> {d, list_to_atom(reverse(Res)),make_term(T)};
split_def([H | T], Res) -> split_def(T, [H | Res]);
split_def([], Res) -> {d, list_to_atom(reverse(Res))}.

make_term(Str) ->
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} -> Term;
		{error, {_,_,Reason}} ->
		    io:format("~ts: ~ts~n", [Reason, Str]),
		    throw(error)
	    end;
	{error, {_,_,Reason}, _} ->
	    io:format("~ts: ~ts~n", [Reason, Str]),
	    throw(error)
    end.

-spec nc(File) -> {'ok', Module} | 'error' when
      File :: file:name(),
      Module :: module().

nc(File) -> nc(File, []).

-spec nc(File, Options) -> {'ok', Module} | 'error' when
      File :: file:name(),
      Options :: [Option] | Option,
      Option:: compile:option(),
      Module :: module().

nc(File, Opts0) when is_list(Opts0) ->
    Opts = Opts0 ++ [report_errors, report_warnings],
    case compile:file(File, Opts) of
	{ok,Mod} ->
	    Dir = outdir(Opts),
	    Obj = filename:basename(File, ".erl") ++ code:objfile_extension(),
	    Fname = filename:join(Dir, Obj),
	    case file:read_file(Fname) of
		{ok,Bin} ->
		    rpc:eval_everywhere(code,load_binary,[Mod,Fname,Bin]),
		    {ok,Mod};
		Other ->
		    Other
	    end;
	Other ->                                %Errors go here
	    Other
    end;
nc(File, Opt) when is_atom(Opt) -> 
    nc(File, [Opt]).

%% l(Mod)
%%  Reload module Mod from file of same name
-spec l(Module) -> code:load_ret() when
      Module :: module().

l(Mod) ->
    code:purge(Mod),
    code:load_file(Mod).

%% Network version of l/1
-spec nl(Module) -> abcast | error when
      Module :: module().

nl(Mod) ->
    case code:get_object_code(Mod) of
	{_Module, Bin, Fname} ->
            rpc:eval_everywhere(code, load_binary, [Mod, Fname, Bin]);
	Other ->
	    Other
    end.

-spec i() -> 'ok'.

i() -> i(processes()).

-spec ni() -> 'ok'.

ni() -> i(all_procs()).

-spec i([pid()]) -> 'ok'.

i(Ps) ->
    i(Ps, length(Ps)).

-spec i([pid()], non_neg_integer()) -> 'ok'.

i(Ps, N) when N =< 100 ->
    iformat("Pid", "Initial Call", "Heap", "Reds",
	    "Msgs"),
    iformat("Registered", "Current Function", "Stack", "",
	    ""),
    {R,M,H,S} = foldl(fun(Pid, {R0,M0,H0,S0}) ->
			      {A,B,C,D} = display_info(Pid),
			      {R0+A,M0+B,H0+C,S0+D}
		      end, {0,0,0,0}, Ps),
    iformat("Total", "", w(H), w(R), w(M)),
    iformat("", "", w(S), "", "");
i(Ps, N) ->
    iformat("Pid", "Initial Call", "Heap", "Reds",
	    "Msgs"),
    iformat("Registered", "Current Function", "Stack", "",
	    ""),
    paged_i(Ps, {0,0,0,0}, N, 50).

paged_i([], {R,M,H,S}, _, _) ->
    iformat("Total", "", w(H), w(R), w(M)),
    iformat("", "", w(S), "", "");
paged_i(Ps, Acc, N, Page) ->
    {Pids, Rest, N1} =
	if N > Page ->
		{L1,L2} = lists:split(Page, Ps),
		{L1,L2,N-Page};
	   true ->
		{Ps, [], 0}
	end,
    NewAcc = foldl(fun(Pid, {R,M,H,S}) ->
			   {A,B,C,D} = display_info(Pid),
			   {R+A,M+B,H+C,S+D}
		   end, Acc, Pids),
    case Rest of
	[_|_] ->
	    choice(fun() -> paged_i(Rest, NewAcc, N1, Page) end);
	[] ->
	    paged_i([], NewAcc, 0, Page)
    end.

choice(F) ->
    case get_line('(c)ontinue (q)uit -->', "c\n") of
	"c\n" ->
	    F();
	"q\n" ->
	    quit;
	_ ->
	    choice(F)
    end.

get_line(P, Default) ->
    case line_string(io:get_line(P)) of
	"\n" ->
	    Default;
	L ->
	    L
    end.

%% If the standard input is set to binary mode
%% convert it to a list so we can properly match.
line_string(Binary) when is_binary(Binary) -> unicode:characters_to_list(Binary);
line_string(Other) -> Other.

mfa_string(Fun) when is_function(Fun) ->
    {module,M} = erlang:fun_info(Fun, module),
    {name,F} = erlang:fun_info(Fun, name),
    {arity,A} = erlang:fun_info(Fun, arity),
    mfa_string({M,F,A});
mfa_string({M,F,A}) ->
    io_lib:format("~w:~w/~w", [M,F,A]);
mfa_string(X) ->
    w(X).

display_info(Pid) ->
    case pinfo(Pid) of
	undefined -> {0,0,0,0};
	Info ->
	    Call = initial_call(Info),
	    Curr = case fetch(current_function, Info) of
		       {Mod,F,Args} when is_list(Args) ->
			   {Mod,F,length(Args)};
		       Other ->
			   Other
		   end,
	    Reds = fetch(reductions, Info),
	    LM = length(fetch(messages, Info)),
	    HS = fetch(heap_size, Info),
	    SS = fetch(stack_size, Info),
	    iformat(w(Pid), mfa_string(Call),
		    w(HS),
		    w(Reds), w(LM)),
	    iformat(case fetch(registered_name, Info) of
			0 -> "";
			X -> w(X)
		    end,
		    mfa_string(Curr),
		    w(SS),
		    "",
		    ""),
	    {Reds, LM, HS, SS}
    end.

%% We have to do some assumptions about the initial call.
%% If the initial call is proc_lib:init_p/3,5 we can find more information
%% calling the function proc_lib:initial_call/1.

initial_call(Info)  ->
    case fetch(initial_call, Info) of
	{proc_lib, init_p, _} ->
	    proc_lib:translate_initial_call(Info);
	ICall ->
	    ICall
    end.

iformat(A1, A2, A3, A4, A5) ->
    format("~-21s ~-33s ~8s ~8s ~4s~n", [A1,A2,A3,A4,A5]).

all_procs() ->
    case is_alive() of
	true -> flatmap(fun (N) -> rpc:call(N,erlang,processes,[]) end,
			[node()|nodes()]);
	false -> processes()
    end.

pinfo(Pid) ->
    case is_alive() of
	true -> rpc:call(node(Pid), erlang, process_info, [Pid]);
	false -> process_info(Pid)
    end.

fetch(Key, Info) ->
    case lists:keyfind(Key, 1, Info) of
	{_, Val} -> Val;
	false -> 0
    end.

-spec pid(X, Y, Z) -> pid() when
      X :: non_neg_integer(),
      Y :: non_neg_integer(),
      Z :: non_neg_integer().

pid(X, Y, Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
		integer_to_list(Y) ++ "." ++
		integer_to_list(Z) ++ ">").

-spec i(X, Y, Z) -> [{atom(), term()}] when
      X :: non_neg_integer(),
      Y :: non_neg_integer(),
      Z :: non_neg_integer().

i(X, Y, Z) -> pinfo(pid(X, Y, Z)).

-spec q() -> no_return().

q() ->
    init:stop().

-spec bt(Pid) -> 'ok' | 'undefined' when
      Pid :: pid().

bt(Pid) ->
    case catch erlang:process_display(Pid, backtrace) of
	{'EXIT', _} ->
	    undefined;
	_ ->
	    ok
    end.

-spec m() -> 'ok'.

m() ->
    mformat("Module", "File"),
    foreach(fun ({Mod,File}) -> mformat(Mod, File) end, sort(code:all_loaded())).

mformat(A1, A2) ->
    format("~-20s  ~ts\n", [A1,A2]).

%% erlangrc(Home)
%%  Try to run a ".erlang" file, first in the current directory
%%  else in home directory.

erlangrc() ->
    case init:get_argument(home) of
	{ok,[[Home]]} ->
	    erlangrc([Home]);
	_ ->
	    f_p_e(["."], ".erlang")
    end.

erlangrc([Home]) ->
    f_p_e([".",Home], ".erlang").

error(Fmt, Args) ->
    error_logger:error_msg(Fmt, Args).

f_p_e(P, F) ->
    case file:path_eval(P, F) of
	{error, enoent} = Enoent ->
	    Enoent;
	{error, E={Line, _Mod, _Term}} ->
	    error("file:path_eval(~tp,~tp): error on line ~p: ~ts~n",
		  [P, F, Line, file:format_error(E)]),
	    ok;
	{error, E} ->
	    error("file:path_eval(~tp,~tp): ~ts~n",
		  [P, F, file:format_error(E)]),
	    ok;
	Other ->
	    Other
    end.

bi(I) ->
    case erlang:system_info(I) of
	X when is_binary(X) -> io:put_chars(binary_to_list(X));
	X when is_list(X) -> io:put_chars(X);
	X -> format("~w", [X])
    end.

%%
%% Short and nice form of module info
%%
-spec m(Module) -> 'ok' when
      Module :: module().

m(M) ->
    L = M:module_info(),
    {exports,E} = lists:keyfind(exports, 1, L),
    Time = get_compile_time(L),
    COpts = get_compile_options(L),
    format("Module: ~w~n", [M]),
    print_md5(L),
    format("Compiled: "),
    print_time(Time),
    print_object_file(M),
    format("Compiler options:  ~p~n", [COpts]),
    format("Exports: ~n",[]), print_exports(keysort(1, E)).

print_object_file(Mod) ->
    case code:is_loaded(Mod) of
	{file,File} ->
	    format("Object file: ~ts\n", [File]);
	_ ->
	    ignore
    end.

print_md5(L) ->
    case lists:keyfind(md5, 1, L) of
        {md5,<<MD5:128>>} -> io:format("MD5: ~.16b~n",[MD5]);
        _ -> ok
    end.

get_compile_time(L) ->
    case get_compile_info(L, time) of
	{ok,Val} -> Val;
	error -> notime
    end.

get_compile_options(L) ->
    case get_compile_info(L, options) of
	{ok,Val} -> Val;
	error -> []
    end.

get_compile_info(L, Tag) ->
    case lists:keyfind(compile, 1, L) of
	{compile, I} ->
	    case lists:keyfind(Tag, 1, I) of
		{Tag, Val} -> {ok,Val};
		false -> error
	    end;
	false -> error
    end.

print_exports(X) when length(X) > 16 ->
    split_print_exports(X);
print_exports([]) -> ok;
print_exports([{F, A} |Tail]) ->
    format("         ~w/~w~n",[F, A]),
    print_exports(Tail).

split_print_exports(L) ->
    Len = length(L),
    Mid = Len div 2,
    L1 = sublist(L, 1, Mid),
    L2 = sublist(L, Mid +1, Len - Mid + 1),
    split_print_exports(L1, L2).

split_print_exports([], [{F, A}|T]) ->
    Str = " ",
    format("~-30s~w/~w~n", [Str, F, A]),
    split_print_exports([], T);
split_print_exports([{F1, A1}|T1], [{F2, A2} | T2]) ->
    Str = flatten(io_lib:format("~w/~w", [F1, A1])),
    format("~-30s~w/~w~n", [Str, F2, A2]),
    split_print_exports(T1, T2);
split_print_exports([], []) -> ok.

print_time({Year,Month,Day,Hour,Min,_Secs}) ->
    format("~s ~w ~w, ", [month(Month),Day,Year]),
    format("~.2.0w:~.2.0w~n", [Hour,Min]);
print_time(notime) ->
    format("No compile time info available~n",[]).

month(1) -> "January";
month(2) -> "February";
month(3) -> "March";
month(4) -> "April";
month(5) -> "May";
month(6) -> "June";
month(7) -> "July";
month(8) -> "August";
month(9) -> "September";
month(10) -> "October";
month(11) -> "November";
month(12) -> "December".

%% Just because we can't eval receive statements...
-spec flush() -> 'ok'.

flush() ->
    receive
	X ->
            case lists:keyfind(encoding, 1, io:getopts()) of
                {encoding,unicode} ->
                    format("Shell got ~tp~n",[X]);
                _ ->
                    format("Shell got ~p~n",[X])
            end,
	    flush()
    after 0 ->
	    ok
    end.

%% Print formatted info about all registered names in the system
-spec nregs() -> 'ok'.

nregs() ->
    foreach(fun (N) -> print_node_regs(N) end, all_regs()).

-spec regs() -> 'ok'.

regs() ->
    print_node_regs({node(),registered()}).

all_regs() ->
    case is_alive() of
        true -> [{N,rpc:call(N, erlang, registered, [])} ||
                    N <- [node()|nodes()]];
	false -> [{node(),registered()}]
    end.

print_node_regs({N, List}) when is_list(List) ->
    {Pids,Ports,_Dead} = pids_and_ports(N, sort(List), [], [], []),
    %% print process info
    format("~n** Registered procs on node ~w **~n",[N]),
    procformat("Name", "Pid", "Initial Call", "Reds", "Msgs"),
    foreach(fun({Name,PI,Pid}) -> procline(Name, PI, Pid) end, Pids),
    %% print port info
    format("~n** Registered ports on node ~w **~n",[N]),
    portformat("Name", "Id", "Command"),
    foreach(fun({Name,PI,Id}) -> portline(Name, PI, Id) end, Ports).

pids_and_ports(_, [], Pids, Ports, Dead) ->
    {reverse(Pids),reverse(Ports),reverse(Dead)};

pids_and_ports(Node, [Name|Names], Pids, Ports, Dead) ->
    case pwhereis(Node, Name) of
	Pid when is_pid(Pid) ->
	    pids_and_ports(Node, Names, [{Name,pinfo(Pid),Pid}|Pids],
			   Ports, Dead);
	Id when is_port(Id) ->
	    pids_and_ports(Node, Names, Pids, 
			   [{Name,portinfo(Id),Id}|Ports], Dead);
	undefined ->
	    pids_and_ports(Node, Names, Pids, Ports, [Name|Dead])
    end.

pwhereis(Node, Name) ->
    case is_alive() of
	true -> rpc:call(Node, erlang, whereis, [Name]);
	false -> whereis(Name)
    end.

portinfo(Id) ->
    case is_alive() of
	true ->  [ rpc:call(node(Id), erlang, port_info, [Id,name]) ];
	false -> [ erlang:port_info(Id, name) ]
    end.

procline(Name, Info, Pid) ->
    Call = initial_call(Info),
    Reds  = fetch(reductions, Info),
    LM = length(fetch(messages, Info)),
    procformat(io_lib:format("~w",[Name]),
	       io_lib:format("~w",[Pid]),
	       io_lib:format("~s",[mfa_string(Call)]),
	       integer_to_list(Reds), integer_to_list(LM)).

procformat(Name, Pid, Call, Reds, LM) ->
    format("~-21s ~-12s ~-25s ~12s ~4s~n", [Name,Pid,Call,Reds,LM]).

portline(Name, Info, Id) ->
    Cmd = fetch(name, Info),
    portformat(io_lib:format("~w",[Name]), 
	       erlang:port_to_list(Id),
	       Cmd).

portformat(Name, Id, Cmd) ->
    format("~-21s ~-15s ~-40s~n", [Name,Id,Cmd]).

%% pwd()
%% cd(Directory)
%%  These are just wrappers around the file:get/set_cwd functions.

-spec pwd() -> 'ok'.

pwd() ->
    case file:get_cwd() of
	{ok, Str} ->
	    ok = io:format("~ts\n", [Str]);
	{error, _} ->
	    ok = io:format("Cannot determine current directory\n")
    end.

-spec cd(Dir) -> 'ok' when
      Dir :: file:name().

cd(Dir) ->
    _ = file:set_cwd(Dir),
    pwd().

%% ls()
%% ls(Directory)
%%  The strategy is to print in fixed width files.

-spec ls() -> 'ok'.

ls() ->
    ls(".").

-spec ls(Dir) -> 'ok' when
      Dir :: file:name().

ls(Dir) ->
    case file:list_dir(Dir) of
	{ok, Entries} ->
	    ls_print(sort(Entries));
	{error, enotdir} ->
	    ls_print([Dir]);
	{error, Error} ->
	    format("~ts\n", [file:format_error(Error)])
    end.

ls_print([]) -> ok;
ls_print(L) ->
    Width = min([max(lengths(L, [])), 40]) + 5,
    ls_print(L, Width, 0).

ls_print(X, Width, Len) when Width + Len >= 80 ->
    io:nl(),
    ls_print(X, Width, 0);
ls_print([H|T], Width, Len) ->
    io:format("~-*ts",[Width,H]),
    ls_print(T, Width, Len+Width);
ls_print([], _, _) ->
    io:nl().

lengths([H|T], L) -> lengths(T, [length(H)|L]);
lengths([], L)    -> L.

w(X) ->
    io_lib:write(X).

%%
%% memory/[0,1]
%%

-spec memory() -> [{Type, Size}] when
      Type :: atom(),
      Size :: non_neg_integer().

memory() -> erlang:memory().

-spec memory(Type) -> Size when
               Type :: atom(),
               Size :: non_neg_integer()
          ; (Types) -> [{Type, Size}] when
               Types :: [Type],
               Type :: atom(),
               Size :: non_neg_integer().

memory(TypeSpec) -> erlang:memory(TypeSpec).

%%
%% uptime/0
%%

-spec uptime() -> 'ok'.

uptime() ->
    io:format("~s~n", [uptime(get_uptime())]).

uptime({D, {H, M, S}}) ->
    lists:flatten(
      [[ io_lib:format("~p days, ", [D]) || D > 0 ],
       [ io_lib:format("~p hours, ", [H]) || D+H > 0 ],
       [ io_lib:format("~p minutes and ", [M]) || D+H+M > 0 ],
       io_lib:format("~p seconds", [S])]).

get_uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    calendar:seconds_to_daystime(UpTime div 1000).

%%
%% Cross Reference Check
%% 
%%-spec xm(module() | file:filename()) -> xref:m/1 return
xm(M) ->
    appcall(tools, xref, m, [M]).

%%
%% Call yecc 
%% 
%%-spec y(file:name()) -> yecc:file/2 return
y(File) -> y(File, []).

%%-spec y(file:name(), [yecc:option()]) -> yecc:file/2 return
y(File, Opts) ->
    appcall(parsetools, yecc, file, [File, Opts]).


%%
%% Avoid creating strong components in xref and dialyzer by making calls
%% from helper functions to other applications indirect.
%%

appcall(App, M, F, Args) ->
    try
	apply(M, F, Args)
    catch
	error:undef ->
	    case erlang:get_stacktrace() of
		[{M,F,Args,_}|_] ->
		    Arity = length(Args),
		    io:format("Call to ~w:~w/~w in application ~w failed.\n",
			      [M,F,Arity,App]);
		Stk ->
		    erlang:raise(error, undef, Stk)
	    end
    end.
