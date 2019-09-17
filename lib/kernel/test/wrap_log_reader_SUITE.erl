%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2019. All Rights Reserved.
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

-module(wrap_log_reader_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(privdir(_), "./disk_log_SUITE_priv").
-define(config(X,Y), foo).
-define(t,test_server).
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), proplists:get_value(priv_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 no_file/1,
	 one_empty/1, one_filled/1,
	 two_filled/1,
	 four_filled/1,
	 wrap_filled/1,
	 wrapping/1,
	 external/1,
	 error/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [no_file, {group, one}, {group, two}, {group, four},
     {group, wrap}, wrapping, external, error].

groups() -> 
    [{one, [], [one_empty, one_filled]},
     {two, [], [two_filled]}, {four, [], [four_filled]},
     {wrap, [], [wrap_filled]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

%% No log file exists.
no_file(Conf) when is_list(Conf) ->
    code:add_path(proplists:get_value(data_dir,Conf)),
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    wlt ! {open, self(), File},
    rec({error, {index_file_not_found, File}}, ?LINE),
    wlt ! {open, self(), File, 1},
    rec({error, {index_file_not_found, File}}, ?LINE),
    wlt ! {open, self(), File, 4},
    rec({error, {index_file_not_found, File}}, ?LINE),

    stop(),
    delete_files(File),
    ok.


%% One empty index file.
one_empty(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    open(sune, File, ?LINE),
    %% open
    do_chunk([{open,File}, eof], wlt, ?LINE),
    do_chunk([{open,File,1}, eof], wlt, ?LINE),
    wlt ! {open, self(), File, 2},
    rec({error, {file_not_found, add_ext(File, 2)}}, ?LINE),
    close(sune),

    %% closed
    do_chunk([{open,File}, eof], wlt, ?LINE),
    do_chunk([{open,File,1}, eof], wlt, ?LINE),
    wlt ! {open, self(), File, 2},
    rec({error, {file_not_found, add_ext(File, 2)}}, ?LINE),

    stop(),
    delete_files(File),
    ok.

%% One filled index file.
one_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    open(sune, File, ?LINE),
    log_terms(sune, ["first round, one", "first round, two"]),
    sync(sune),
    %% open
    test_one(File),
    close(sune),
    %% closed
    test_one(File),

    stop(),
    delete_files(File),
    ok.

test_one(File) ->
    do_chunk([{open,File},
	      {chunk, ["first round, one", "first round, two"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,1},
	      {chunk, ["first round, one", "first round, two"]},
	      eof], wlt, ?LINE),
    wlt ! {open, self(), File, 2},
    rec({error, {file_not_found, add_ext(File, 2)}}, ?LINE),
    do_chunk([{open,File,1}, {chunk, 1, ["first round, one"]},
	      {chunk, 1, ["first round, two"]}, eof], wlt, ?LINE),
    ok.


%% Two filled index files.
two_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = list_to_atom(join(Dir, "sune.LOG")),
    delete_files(File),
    start(),

    open(sune, File, ?LINE),
    log_terms(sune, ["first round, 11", "first round, 12"]),
    log_terms(sune, ["first round, 21", "first round, 22"]),
    sync(sune),
    %% open
    test_two(File),
    close(sune),
    %% closed
    test_two(File),

    stop(),
    delete_files(File),
    ok.

test_two(File) ->
    do_chunk([{open,File},
	      {chunk, infinity, ["first round, 11", "first round, 12"]},
	      {chunk, ["first round, 21", "first round, 22"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,1},
	      {chunk, ["first round, 11", "first round, 12"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,2},
	      {chunk, ["first round, 21", "first round, 22"]},
	      eof], wlt, ?LINE),
    wlt ! {open, self(), File, 3},
    rec({error, {file_not_found, add_ext(File, 3)}}, ?LINE),
    do_chunk([{open,File,1}, {chunk, 1, ["first round, 11"]},
	      {chunk, 2, ["first round, 12"]}, eof], wlt, ?LINE),
    ok.


%% Four filled index files.
four_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    open(sune, File, ?LINE),
    init_files(0),
    sync(sune),
    %% open
    test_four(File),
    close(sune),
    %% closed
    test_four(File),

    stop(),
    delete_files(File),
    ok.

test_four(File) ->
    do_chunk([{open,File},
	      {chunk, ["first round, 11", "first round, 12"]},
	      {chunk, ["first round, 21", "first round, 22"]},
	      {chunk, ["first round, 31", "first round, 32"]},
	      {chunk, ["first round, 41", "first round, 42"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,1},
	      {chunk, ["first round, 11", "first round, 12"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,4},
	      {chunk, ["first round, 41", "first round, 42"]},
	      eof], wlt, ?LINE),
    wlt ! {open, self(), File, 5},
    rec({error, {file_not_found, add_ext(File, 5)}}, ?LINE),
    do_chunk([{open,File,1}, {chunk, 1, ["first round, 11"]},
	      {chunk, 2, ["first round, 12"]}, eof], wlt, ?LINE),
    do_chunk([{open,File,4}, {chunk, 1, ["first round, 41"]},
	      {chunk, 2, ["first round, 42"]}, eof], wlt, ?LINE),
    ok.


%% First wrap, open, filled index file.
wrap_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    open(sune, File, ?LINE),
    init_files(0),
    log_terms(sune, ["second round, 11", "second round, 12"]),
    sync(sune),
    %% open
    test_wrap(File),
    close(sune),
    %% closed
    test_wrap(File),

    stop(),
    delete_files(File),
    ok.

test_wrap(File) ->
    do_chunk([{open,File},
	      {chunk, ["first round, 21", "first round, 22"]},
	      {chunk, ["first round, 31", "first round, 32"]},
	      {chunk, ["first round, 41", "first round, 42"]},
	      {chunk, ["second round, 11", "second round, 12"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,1},
	      {chunk, ["second round, 11", "second round, 12"]},
	      eof], wlt, ?LINE),
    do_chunk([{open,File,2},
	      {chunk, ["first round, 21", "first round, 22"]},
	      eof], wlt, ?LINE),
    wlt ! {open, self(), File, 5},
    rec({error, {file_not_found, add_ext(File, 5)}}, ?LINE),
    do_chunk([{open,File,1}, {chunk, 1, ["second round, 11"]},
	      {chunk, 2, ["second round, 12"]}, eof], wlt, ?LINE),
    do_chunk([{open,File,4}, {chunk, 1, ["first round, 41"]},
	      {chunk, 2, ["first round, 42"]}, eof], wlt, ?LINE),
    ok.

%% Wrapping at the same time as reading.
wrapping(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    open(sune, File, ?LINE),
    init_files(1100),
    sync(sune),
    C1 =
	do_chunk([{open,File}, {chunk, 1, ["first round, 11"]}], wlt, ?LINE),
    log_terms(sune, ["second round, 11", "second round, 12"]),
    sync(sune),
    do_chunk([{chunk, 1, ["first round, 12"]},
	      %% Here two bad bytes are found.
	      {chunk, ["first round, 21", "first round, 22"]},
	      {chunk, ["first round, 31", "first round, 32"]},
	      {chunk, ["first round, 41", "first round, 42"]}, eof],
	     wlt, ?LINE, C1),
    start(),
    delete_files(File),
    open(sune, File, ?LINE),
    init_files(1100),
    sync(sune),
    C2 =
	do_chunk([{open,File}, {chunk, 1, ["first round, 11"]}], wlt, ?LINE),
    log_terms(sune, ["second round, 11", "second round, 12"]),
    close(sune),
    do_chunk([{chunk, 1, ["first round, 12"]},
	      %% Here two bad bytes are found.
	      {chunk, ["first round, 21", "first round, 22"]},
	      {chunk, ["first round, 31", "first round, 32"]},
	      {chunk, ["first round, 41", "first round, 42"]}, eof],
	     wlt, ?LINE, C2),
    start(),
    delete_files(File),
    open(sune, File, ?LINE),
    init_files(1100),
    sync(sune),
    C3 = do_chunk([{open,File}], wlt, ?LINE),
    log_terms(sune, ["second round, 11"]),
    sync(sune),
    do_chunk([{chunk, 1, ["second round, 11"]},
	      {chunk, 1, ["first round, 21"]},
	      {chunk, 1, ["first round, 22"]},
	      {chunk, ["first round, 31", "first round, 32"]},
	      {chunk, ["first round, 41", "first round, 42"]}, eof],
	     wlt, ?LINE, C3),

    stop(),
    delete_files(File),
    ok.

%% External format.
external(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    open_ext(sune, File, ?FILE),
    init_files_ext(0),
    close(sune),
    P0 = pps(),
    wlt ! {open, self(), File},
    rec({error, {not_a_log_file, add_ext(File, 1)}}, ?LINE),
    check_pps(P0),

    stop(),
    delete_files(File),
    ok.

%% Error situations.
error(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    P0 = pps(),
    wlt ! {open, self(), File, 1},
    rec({error, {index_file_not_found, File}}, ?LINE),
    wlt ! {open, self(), File},
    rec({error, {index_file_not_found, File}}, ?LINE),
    check_pps(P0),

    open(sune, File, ?LINE),
    close(sune),
    P1 = pps(),
    First = add_ext(File, 1),
    ok = file:delete(First),
    wlt ! {open, self(), File},
    rec({error, {not_a_log_file, First}}, ?LINE),
    check_pps(P1),

    delete_files(File),
    open(sune, File, ?LINE),
    init_files(0),
    close(sune),
    P2 = pps(),
    C = do_chunk([{open,File},
		  {chunk, ["first round, 11", "first round, 12"]}],
		 wlt, ?LINE),
    Second = add_ext(File, 2),
    ok = file:delete(Second),
    wlt ! {chunk, self(), C},
    rec({error, {file_error, Second, {error, enoent}}}, ?LINE),
    ok = file:write_file(Second, <<17:(3*8)>>), % three bytes
    wlt ! {chunk, self(), C},
    rec({error, {not_a_log_file, Second}}, ?LINE),
    do_chunk([close], wlt, ?LINE, C),
    check_pps(P2),

    delete_files(File),
    open(sune, File, ?LINE),
    init_files(0),
    close(sune),
    P3 = pps(),
    timer:sleep(1100),
    Now = calendar:local_time(),
    ok = file:change_time(First, Now),
    C2 = do_chunk([{open,File},
		   {chunk, ["first round, 11", "first round, 12"]}],
		  wlt, ?LINE),
    wlt ! {chunk, self(), C2},
    rec({error,{is_wrapped,First}}, ?LINE),
    do_chunk([close], wlt, ?LINE, C2),
    IndexFile = add_ext(File, idx),
    ok = file:write_file(IndexFile, <<17:(3*8)>>),
    wlt ! {open, self(), File, 1},
    rec({error, {index_file_not_found, File}}, ?LINE),
    check_pps(P3),

    stop(),
    delete_files(File),
    ok.

start() ->
    ok = wrap_log_test:stop(),
    dl_wait(),
    ok = wrap_log_test:init().

stop() ->
    ok = wrap_log_test:stop(),
    dl_wait().

%% Give disk logs opened by 'wlr_logger' and 'wlt' time to close after
%% receiving EXIT signals.
dl_wait() ->
    case disk_log:accessible_logs() of
        {[], []} ->
            ok;
        _X ->
            erlang:display(_X),
            timer:sleep(100),
            dl_wait()
    end.

delete_files(File) ->
    file:delete(add_ext(File, idx)),
    file:delete(add_ext(File, siz)),
    file:delete(add_ext(File, 1)),
    file:delete(add_ext(File, 2)),
    file:delete(add_ext(File, 3)),
    file:delete(add_ext(File, 4)),
    ok.

init_files(Delay) ->
    log_terms(sune, ["first round, 11", "first round, 12"]),
    timer:sleep(Delay),
    log_terms(sune, ["first round, 21", "first round, 22"]),
    timer:sleep(Delay),
    log_terms(sune, ["first round, 31", "first round, 32"]),
    timer:sleep(Delay),
    log_terms(sune, ["first round, 41", "first round, 42"]),
    timer:sleep(Delay),
    ok.

init_files_ext(Delay) ->
    blog_terms(sune, ["first round, 11", "first round, 12"]),
    timer:sleep(Delay),
    blog_terms(sune, ["first round, 21", "first round, 22"]),
    timer:sleep(Delay),
    blog_terms(sune, ["first round, 31", "first round, 32"]),
    timer:sleep(Delay),
    blog_terms(sune, ["first round, 41", "first round, 42"]),
    timer:sleep(Delay),
    ok.

join(A, B) ->
    filename:nativename(filename:join(A, B)).

do_chunk(Commands, Server, Where) ->
    do_chunk(Commands, Server, Where, foo).

do_chunk([{open, File, One} | Cs], S, W, _C) ->
    S ! {open, self(), File, One},
    NC = rec1(ok, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([{open, File} | Cs], S, W, _C) ->
    S ! {open, self(), File},
    NC = rec1(ok, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([{chunk, Terms} | Cs], S, W, C) ->
    S ! {chunk, self(), C},
    NC = rec2(Terms, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([{chunk, N, Terms} | Cs], S, W, C) ->
    S ! {chunk, self(), C, N},
    NC = rec2(Terms, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([eof], S, W, C) ->
    S ! {chunk, self(), C},
    C1 = rec2(eof, {W,?LINE}),
    do_chunk([close], S, W, C1);
do_chunk([close], S, W, C) ->
    S ! {close, self(), C},
    rec(ok, {W,?LINE});
do_chunk([], _S, _W, C) ->
    C.

add_ext(Name, Ext) ->
    lists:concat([Name, ".", Ext]).

%% disk_log.
open(Log, File, Where) ->
    wlr_logger ! {open, self(), Log, File},
    rec1(ok, Where).

open_ext(Log, File, Where) ->
    wlr_logger ! {open_ext, self(), Log, File},
    rec1(ok, Where).

close(Log) ->
    wlr_logger ! {close, self(), Log},
    rec(ok, ?LINE).

sync(Log) ->
    wlr_logger ! {sync, self(), Log},
    rec(ok, ?LINE).

log_terms(File, Terms) ->
    wlr_logger ! {log_terms, self(), File, Terms},
    rec(ok, ?LINE).

blog_terms(File, Terms) ->
    wlr_logger ! {blog_terms, self(), File, Terms},
    rec(ok, ?LINE).

rec1(M, Where) ->
    receive 
        {M, C} -> C;
        Else -> ct:fail({error, {Where, Else}})
    after 1000  -> ct:fail({error, {Where, time_out}})
    end.

rec2(M, Where) ->
    receive 
        {C, M} -> C;
        Else -> ct:fail({error, {Where, Else}})
    after 1000  -> ct:fail({error, {Where, time_out}})
    end.

rec(M, Where) ->
    receive 
        M ->
            ok;
        Else -> ct:fail({error, {Where, Else}})
    after 5000 -> ct:fail({error, {Where, time_out}})
    end.

check_pps({Ports0,Procs0} = P0) ->
    case pps() of
        P0 ->
            ok;
        _ ->
            timer:sleep(500),
            case pps() of
                P0 ->
                    ok;
                {Ports1,Procs1} = P1 ->
		    case {Ports1 -- Ports0, Procs1 -- Procs0} of
			{[], []} -> ok;
			{PortsDiff,ProcsDiff} ->
			    io:format("failure, got ~p~n, expected ~p\n", [P1, P0]),
			    show("Old port", Ports0 -- Ports1),
			    show("New port", PortsDiff),
			    show("Old proc", Procs0 -- Procs1),
			    show("New proc", ProcsDiff),
			    ct:fail(failed)
		    end
	    end
    end.

show(_S, []) ->
    ok;
show(S, [{Pid, Name, InitCall}|Pids]) when is_pid(Pid) ->
    io:format("~s: ~w (~w), ~w: ~p~n",
              [S, Pid, proc_reg_name(Name), InitCall,
               erlang:process_info(Pid)]),
    show(S, Pids);
show(S, [{Port, _}|Ports]) when is_port(Port)->
    io:format("~s: ~w: ~p~n", [S, Port, erlang:port_info(Port)]),
    show(S, Ports).

pps() ->
    timer:sleep(100),
    {port_list(), process_list()}.

port_list() ->
    [{P,safe_second_element(erlang:port_info(P, name))} ||
        P <- erlang:ports()].

process_list() ->
    [{P,process_info(P, registered_name),
      safe_second_element(process_info(P, initial_call))} ||
        P <- processes(), erlang:is_process_alive(P)].

proc_reg_name({registered_name, Name}) -> Name;
proc_reg_name([]) -> no_reg_name.

safe_second_element({_,Info}) -> Info;
safe_second_element(Other) -> Other.
