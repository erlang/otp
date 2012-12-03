%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2012. All Rights Reserved.
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

-module(wrap_log_reader_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(privdir(_), "./disk_log_SUITE_priv").
-define(config(X,Y), foo).
-define(t,test_server).
-else.
-include_lib("test_server/include/test_server.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), ?config(priv_dir, Conf)).
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

suite() -> [{ct_hooks,[ts_install_cth]}].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:seconds(60)),
    [{watchdog, Dog} | Config].

end_per_testcase(_Func, _Config) ->
    Dog=?config(watchdog, _Config),
    ?t:timetrap_cancel(Dog).

no_file(suite) -> [];
no_file(doc) -> ["No log file exists"];
no_file(Conf) when is_list(Conf) ->
    ?line code:add_path(?config(data_dir,Conf)),
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    wlt ! {open, self(), File},
    ?line rec({error, {index_file_not_found, File}}, ?LINE),
    wlt ! {open, self(), File, 1},
    ?line rec({error, {index_file_not_found, File}}, ?LINE),
    wlt ! {open, self(), File, 4},
    ?line rec({error, {index_file_not_found, File}}, ?LINE),

    stop(),
    delete_files(File),
    ok.


one_empty(suite) -> [];
one_empty(doc) -> ["One empty index file"];
one_empty(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    ?line open(sune, File, ?LINE),
    %% open
    ?line do_chunk([{open,File}, eof], wlt, ?LINE),
    ?line do_chunk([{open,File,1}, eof], wlt, ?LINE),
    wlt ! {open, self(), File, 2},
    ?line rec({error, {file_not_found, add_ext(File, 2)}}, ?LINE),
    ?line close(sune),

    %% closed
    ?line do_chunk([{open,File}, eof], wlt, ?LINE),
    ?line do_chunk([{open,File,1}, eof], wlt, ?LINE),
    wlt ! {open, self(), File, 2},
    ?line rec({error, {file_not_found, add_ext(File, 2)}}, ?LINE),

    stop(),
    delete_files(File),
    ok.

one_filled(suite) -> [];
one_filled(doc) -> ["One filled index file"];
one_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    ?line open(sune, File, ?LINE),
    ?line log_terms(sune, ["first round, one", "first round, two"]),
    ?line sync(sune),
    %% open
    test_one(File),
    ?line close(sune),
    %% closed
    test_one(File),

    stop(),
    delete_files(File),
    ok.

test_one(File) ->
    ?line do_chunk([{open,File}, 
		    {chunk, ["first round, one", "first round, two"]}, 
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,1}, 
		    {chunk, ["first round, one", "first round, two"]}, 
		    eof], wlt, ?LINE),
    wlt ! {open, self(), File, 2},
    ?line rec({error, {file_not_found, add_ext(File, 2)}}, ?LINE),
    ?line do_chunk([{open,File,1}, {chunk, 1, ["first round, one"]}, 
		    {chunk, 1, ["first round, two"]}, eof], wlt, ?LINE),
    ok.


two_filled(suite) -> [];
two_filled(doc) -> ["Two filled index files"];
two_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = list_to_atom(join(Dir, "sune.LOG")),
    delete_files(File),
    start(),

    ?line open(sune, File, ?LINE),
    ?line log_terms(sune, ["first round, 11", "first round, 12"]),
    ?line log_terms(sune, ["first round, 21", "first round, 22"]),
    ?line sync(sune),
    %% open
    test_two(File),
    ?line close(sune),
    %% closed
    test_two(File),

    stop(),
    delete_files(File),
    ok.

test_two(File) ->
    ?line do_chunk([{open,File}, 
		    {chunk, infinity, ["first round, 11", "first round, 12"]},
		    {chunk, ["first round, 21", "first round, 22"]},
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,1}, 
		    {chunk, ["first round, 11", "first round, 12"]},
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,2}, 
		    {chunk, ["first round, 21", "first round, 22"]},
		    eof], wlt, ?LINE),
    wlt ! {open, self(), File, 3},
    ?line rec({error, {file_not_found, add_ext(File, 3)}}, ?LINE),
    ?line do_chunk([{open,File,1}, {chunk, 1, ["first round, 11"]},
		    {chunk, 2, ["first round, 12"]}, eof], wlt, ?LINE),
    ok.


four_filled(suite) -> [];
four_filled(doc) -> ["Four filled index files"];
four_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    ?line open(sune, File, ?LINE),
    ?line init_files(0),
    ?line sync(sune),
    %% open
    test_four(File),
    ?line close(sune),
    %% closed
    test_four(File),

    stop(),
    delete_files(File),
    ok.

test_four(File) ->
    ?line do_chunk([{open,File}, 
		    {chunk, ["first round, 11", "first round, 12"]},
		    {chunk, ["first round, 21", "first round, 22"]},
		    {chunk, ["first round, 31", "first round, 32"]},
		    {chunk, ["first round, 41", "first round, 42"]},
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,1}, 
		    {chunk, ["first round, 11", "first round, 12"]},
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,4}, 
		    {chunk, ["first round, 41", "first round, 42"]},
		    eof], wlt, ?LINE),
    wlt ! {open, self(), File, 5},
    ?line rec({error, {file_not_found, add_ext(File, 5)}}, ?LINE),
    ?line do_chunk([{open,File,1}, {chunk, 1, ["first round, 11"]},
		    {chunk, 2, ["first round, 12"]}, eof], wlt, ?LINE),
    ?line do_chunk([{open,File,4}, {chunk, 1, ["first round, 41"]},
		    {chunk, 2, ["first round, 42"]}, eof], wlt, ?LINE),
    ok.


wrap_filled(suite) -> [];
wrap_filled(doc) -> ["First wrap, open, filled index file"];
wrap_filled(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    ?line open(sune, File, ?LINE),
    ?line init_files(0),
    ?line log_terms(sune, ["second round, 11", "second round, 12"]),
    ?line sync(sune),
    %% open
    test_wrap(File),
    ?line close(sune),
    %% closed
    test_wrap(File),

    stop(),
    delete_files(File),
    ok.

test_wrap(File) ->
    ?line do_chunk([{open,File}, 
		    {chunk, ["first round, 21", "first round, 22"]},
		    {chunk, ["first round, 31", "first round, 32"]},
		    {chunk, ["first round, 41", "first round, 42"]},
		    {chunk, ["second round, 11", "second round, 12"]},
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,1}, 
		    {chunk, ["second round, 11", "second round, 12"]},
		    eof], wlt, ?LINE),
    ?line do_chunk([{open,File,2}, 
		    {chunk, ["first round, 21", "first round, 22"]},
		    eof], wlt, ?LINE),
    wlt ! {open, self(), File, 5},
    ?line rec({error, {file_not_found, add_ext(File, 5)}}, ?LINE),
    ?line do_chunk([{open,File,1}, {chunk, 1, ["second round, 11"]},
		    {chunk, 2, ["second round, 12"]}, eof], wlt, ?LINE),
    ?line do_chunk([{open,File,4}, {chunk, 1, ["first round, 41"]},
		    {chunk, 2, ["first round, 42"]}, eof], wlt, ?LINE),
    ok.

wrapping(suite) -> [];
wrapping(doc) -> ["Wrapping at the same time as reading"];
wrapping(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    ?line open(sune, File, ?LINE),
    ?line init_files(1100),
    ?line sync(sune),
    ?line C1 = 
	do_chunk([{open,File}, {chunk, 1, ["first round, 11"]}], wlt, ?LINE),
    ?line log_terms(sune, ["second round, 11", "second round, 12"]),
    ?line sync(sune),
    ?line do_chunk([{chunk, 1, ["first round, 12"]}, 
		    %% Here two bad bytes are found.
		    {chunk, ["first round, 21", "first round, 22"]},
		    {chunk, ["first round, 31", "first round, 32"]},
		    {chunk, ["first round, 41", "first round, 42"]}, eof],
		   wlt, ?LINE, C1),
    start(),
    delete_files(File),
    ?line open(sune, File, ?LINE),
    ?line init_files(1100),
    ?line sync(sune),
    ?line C2 = 
	do_chunk([{open,File}, {chunk, 1, ["first round, 11"]}], wlt, ?LINE),
    ?line log_terms(sune, ["second round, 11", "second round, 12"]),
    ?line close(sune),
    ?line do_chunk([{chunk, 1, ["first round, 12"]}, 
		    %% Here two bad bytes are found.
		    {chunk, ["first round, 21", "first round, 22"]},
		    {chunk, ["first round, 31", "first round, 32"]},
		    {chunk, ["first round, 41", "first round, 42"]}, eof],
		   wlt, ?LINE, C2),
    start(),
    delete_files(File),
    ?line open(sune, File, ?LINE),
    ?line init_files(1100),
    ?line sync(sune),
    ?line C3 = do_chunk([{open,File}], wlt, ?LINE),
    ?line log_terms(sune, ["second round, 11"]),
    ?line sync(sune),
    ?line do_chunk([{chunk, 1, ["second round, 11"]}, 
		    {chunk, 1, ["first round, 21"]},
		    {chunk, 1, ["first round, 22"]}, 
		    {chunk, ["first round, 31", "first round, 32"]},
		    {chunk, ["first round, 41", "first round, 42"]}, eof],
		   wlt, ?LINE, C3),

    stop(),
    delete_files(File),
    ok.

external(suite) -> [];
external(doc) -> ["External format"];
external(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    ?line open_ext(sune, File, ?FILE),
    ?line init_files_ext(0),
    ?line close(sune),
    P0 = pps(),
    wlt ! {open, self(), File},
    ?line rec({error, {not_a_log_file, add_ext(File, 1)}}, ?LINE),
    ?line true = (P0 == pps()),    

    stop(),
    delete_files(File),
    ok.

error(suite) -> [];
error(doc) -> ["Error situations"];
error(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = join(Dir, "sune.LOG"),
    delete_files(File),
    start(),

    P0 = pps(),
    wlt ! {open, self(), File, 1},
    ?line rec({error, {index_file_not_found, File}}, ?LINE),
    wlt ! {open, self(), File},
    ?line rec({error, {index_file_not_found, File}}, ?LINE),
    ?line true = (P0 == pps()),

    ?line open(sune, File, ?LINE),
    ?line close(sune),
    P1 = pps(),
    ?line First = add_ext(File, 1),
    ?line ok = file:delete(First),
    wlt ! {open, self(), File},
    ?line rec({error, {not_a_log_file, First}}, ?LINE),
    ?line true = (P1 == pps()),

    delete_files(File),
    ?line open(sune, File, ?LINE),
    ?line init_files(0),
    ?line close(sune),
    P2 = pps(),
    ?line C = do_chunk([{open,File}, 
			{chunk, ["first round, 11", "first round, 12"]}],
		       wlt, ?LINE),
    ?line Second = add_ext(File, 2),
    ?line ok = file:delete(Second),
    wlt ! {chunk, self(), C},
    ?line rec({error, {file_error, Second, {error, enoent}}}, ?LINE),
    ?line ok = file:write_file(Second, <<17:(3*8)>>), % three bytes
    wlt ! {chunk, self(), C},
    ?line rec({error, {not_a_log_file, Second}}, ?LINE),
    ?line do_chunk([close], wlt, ?LINE, C),
    ?line true = (P2 == pps()),

    delete_files(File),
    ?line open(sune, File, ?LINE),
    ?line init_files(0),
    ?line close(sune),
    P3 = pps(),
    timer:sleep(1100),
    Now = calendar:local_time(),
    ?line ok = file:change_time(First, Now),
    ?line C2 = do_chunk([{open,File}, 
			 {chunk, ["first round, 11", "first round, 12"]}],
			wlt, ?LINE),
    wlt ! {chunk, self(), C2},
    ?line rec({error,{is_wrapped,First}}, ?LINE),
    ?line do_chunk([close], wlt, ?LINE, C2),
    IndexFile = add_ext(File, idx),
    ?line ok = file:write_file(IndexFile, <<17:(3*8)>>),
    wlt ! {open, self(), File, 1},
    ?line rec({error, {index_file_not_found, File}}, ?LINE),
    ?line true = (P3 == pps()),

    stop(),
    delete_files(File),
    ok.

start() ->
    ?line ok = wrap_log_test:stop(),
    dl_wait(),
    ?line ok = wrap_log_test:init().

stop() ->
    ?line ok = wrap_log_test:stop(),
    dl_wait().

%% Give disk logs opened by 'logger' and 'wlt' time to close after
%% receiving EXIT signals.
dl_wait() ->
    case disk_log:accessible_logs() of
        {[], []} ->
            ok;
        _ ->
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
    ?line log_terms(sune, ["first round, 11", "first round, 12"]),
    timer:sleep(Delay),
    ?line log_terms(sune, ["first round, 21", "first round, 22"]),
    timer:sleep(Delay),
    ?line log_terms(sune, ["first round, 31", "first round, 32"]),
    timer:sleep(Delay),
    ?line log_terms(sune, ["first round, 41", "first round, 42"]),
    timer:sleep(Delay),
    ok.

init_files_ext(Delay) ->
    ?line blog_terms(sune, ["first round, 11", "first round, 12"]),
    timer:sleep(Delay),
    ?line blog_terms(sune, ["first round, 21", "first round, 22"]),
    timer:sleep(Delay),
    ?line blog_terms(sune, ["first round, 31", "first round, 32"]),
    timer:sleep(Delay),
    ?line blog_terms(sune, ["first round, 41", "first round, 42"]),
    timer:sleep(Delay),
    ok.

join(A, B) ->
    filename:nativename(filename:join(A, B)).

do_chunk(Commands, Server, Where) ->
    do_chunk(Commands, Server, Where, foo).

do_chunk([{open, File, One} | Cs], S, W, _C) ->
    S ! {open, self(), File, One},
    ?line NC = rec1(ok, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([{open, File} | Cs], S, W, _C) ->
    S ! {open, self(), File},
    ?line NC = rec1(ok, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([{chunk, Terms} | Cs], S, W, C) ->
    S ! {chunk, self(), C},
    ?line NC = rec2(Terms, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([{chunk, N, Terms} | Cs], S, W, C) ->
    S ! {chunk, self(), C, N},
    ?line NC = rec2(Terms, {W,?LINE}),
    do_chunk(Cs, S, W, NC);
do_chunk([eof], S, W, C) ->
    S ! {chunk, self(), C},
    ?line C1 = rec2(eof, {W,?LINE}),
    do_chunk([close], S, W, C1);
do_chunk([close], S, W, C) ->
    S ! {close, self(), C},
    ?line rec(ok, {W,?LINE});
do_chunk([], _S, _W, C) ->
    C.

add_ext(Name, Ext) ->
    lists:concat([Name, ".", Ext]).

%% disk_log.
open(Log, File, Where) ->
    logger ! {open, self(), Log, File},
    rec1(ok, Where).

open_ext(Log, File, Where) ->
    logger ! {open_ext, self(), Log, File},
    rec1(ok, Where).

close(Log) ->
    logger ! {close, self(), Log},
    rec(ok, ?LINE).

sync(Log) ->
    logger ! {sync, self(), Log},
    rec(ok, ?LINE).

log_terms(File, Terms) ->
    logger ! {log_terms, self(), File, Terms},
    rec(ok, ?LINE).

blog_terms(File, Terms) ->
    logger ! {blog_terms, self(), File, Terms},
    rec(ok, ?LINE).

rec1(M, Where) ->
    receive 
        {M, C} -> C;
        Else -> test_server:fail({error, {Where, Else}})
    after 1000  -> test_server:fail({error, {Where, time_out}})
    end.

rec2(M, Where) ->
    receive 
        {C, M} -> C;
        Else -> test_server:fail({error, {Where, Else}})
    after 1000  -> test_server:fail({error, {Where, time_out}})
    end.

rec(M, Where) ->
    receive 
        M ->
            ok;
        Else -> ?t:fail({error, {Where, Else}})
    after 5000 -> ?t:fail({error, {Where, time_out}})
    end.
	    
pps() ->
    {erlang:ports(), lists:filter(fun erlang:is_process_alive/1, processes())}.
