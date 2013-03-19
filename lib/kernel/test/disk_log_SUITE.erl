%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
-module(disk_log_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(privdir(_), "./disk_log_SUITE_priv").
-define(datadir(_), "./disk_log_SUITE_data").
-define(config(X,Y), foo).
-define(t,test_server).
-else.
-include_lib("test_server/include/test_server.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), ?config(priv_dir, Conf)).
-define(datadir(Conf), ?config(data_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 

	 halt_int_inf/1, 
	 halt_int_sz_1/1, halt_int_sz_2/1,

	 halt_int_ro/1, halt_ext_ro/1, wrap_int_ro/1, 
	 wrap_ext_ro/1, halt_trunc/1, halt_misc/1, halt_ro_alog/1, 
	 halt_ro_balog/1, halt_ro_crash/1,

	 wrap_int_1/1, wrap_int_2/1, inc_wrap_file/1,

	 halt_ext_inf/1,

	 halt_ext_sz_1/1, halt_ext_sz_2/1,

	 wrap_ext_1/1, wrap_ext_2/1,

	 head_func/1, plain_head/1, one_header/1,

	 wrap_notif/1, full_notif/1, trunc_notif/1, blocked_notif/1,

	 new_idx_vsn/1, 

	 reopen/1, 

	 block_blocked/1, block_queue/1, block_queue2/1,

	 unblock/1,

	 open_overwrite/1, open_size/1, open_truncate/1, open_error/1,

	 close_race/1, close_block/1, close_deadlock/1,

	 error_repair/1, error_log/1, error_index/1,

	 chunk/1, 

	 truncate/1,

	 many_users/1,

	 info_current/1, 

	 change_size_before/1, change_size_during/1, 
	 change_size_after/1, default_size/1, change_size2/1,
	 change_size_truncate/1,

	 change_attribute/1,

	 dist_open/1, dist_error_open/1, dist_notify/1, 
	 dist_terminate/1, dist_accessible/1, dist_deadlock/1,
         dist_open2/1, other_groups/1,

         evil/1,

         otp_6278/1, otp_10131/1]).

-export([head_fun/1, hf/0, lserv/1, 
	 measure/0, init_m/1, xx/0, head_exit/0, slow_header/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([try_unblock/1]).

-export([client/4]).

-define(default_timeout, ?t:minutes(1)).

%% error_logger
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/src/disk_log.hrl").

%% TODO (old):
%%   - global logs
%%   - badarg
%%   - force file:write fail (how?)
%%   - kill logging proc while he is logging
%%   - kill logging node while he is logging
%%   - test chunk_step

%% These are all tests, the list to be returned by all().
-define(ALL_TESTS,
	[halt_int, wrap_int, halt_ext, wrap_ext, read_mode, head,
	 notif, new_idx_vsn, reopen, block, unblock, open, close,
	 error, chunk, truncate, many_users, info, change_size,
	 change_attribute, distribution, evil, otp_6278, otp_10131]).

%% These test cases should be skipped if the VxWorks card is 
%% configured without NFS cache.
-define(SKIP_NO_CACHE,[distribution]).
%% These tests should be skipped if the VxWorks card is configured *with*
%% nfs cache.
-define(SKIP_LARGE_CACHE,[inc_wrap_file, halt_ext, wrap_ext, read_mode, 
			  head, wrap_notif, open_size, error_log, 
                          error_index, chunk, 
			  change_size_before, change_size_during, 
			  change_size_after, default_size]).


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, halt_int}, {group, wrap_int},
     {group, halt_ext}, {group, wrap_ext},
     {group, read_mode}, {group, head}, {group, notif},
     new_idx_vsn, reopen, {group, block}, unblock,
     {group, open}, {group, close}, {group, error}, chunk,
     truncate, many_users, {group, info},
     {group, change_size}, change_attribute,
     {group, distribution}, evil, otp_6278, otp_10131].

groups() -> 
    [{halt_int, [], [halt_int_inf, {group, halt_int_sz}]},
     {halt_int_sz, [], [halt_int_sz_1, halt_int_sz_2]},
     {read_mode, [],
      [halt_int_ro, halt_ext_ro, wrap_int_ro, wrap_ext_ro,
       halt_trunc, halt_misc, halt_ro_alog, halt_ro_balog,
       halt_ro_crash]},
     {wrap_int, [], [wrap_int_1, wrap_int_2, inc_wrap_file]},
     {halt_ext, [], [halt_ext_inf, {group, halt_ext_sz}]},
     {halt_ext_sz, [], [halt_ext_sz_1, halt_ext_sz_2]},
     {wrap_ext, [], [wrap_ext_1, wrap_ext_2]},
     {head, [], [head_func, plain_head, one_header]},
     {notif, [],
      [wrap_notif, full_notif, trunc_notif, blocked_notif]},
     {block, [], [block_blocked, block_queue, block_queue2]},
     {open, [],
      [open_overwrite, open_size, open_truncate, open_error]},
     {close, [], [close_race, close_block, close_deadlock]},
     {error, [], [error_repair, error_log, error_index]},
     {info, [], [info_current]},
     {change_size, [],
      [change_size_before, change_size_during,
       change_size_after, default_size, change_size2,
       change_size_truncate]},
     {distribution, [],
      [dist_open, dist_error_open, dist_notify,
       dist_terminate, dist_accessible, dist_deadlock,
       dist_open2, other_groups]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



init_per_testcase(Case, Config) ->
    case should_skip(Case,Config) of
	true ->
	    CS = check_nfs(Config),
	    {skipped, lists:flatten
	     (io_lib:format
	      ("The test does not work "
	       "with current NFS cache size (~w),"
	       " to get this test to run, "
	       "~s the NFS cache size~n",  
	       [CS, case CS of
			0 ->
			    "enlarge";
			_ ->
			    "zero"
		    end]))};
	_ ->
	    Dog=?t:timetrap(?t:minutes(2)),
	    [{watchdog, Dog}|Config]
    end.

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


halt_int_inf(suite) -> [];
halt_int_inf(doc) -> ["Test simple halt disk log, size infinity"];
halt_int_inf(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    ?line ok = disk_log:start(),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal},
				   {file, File}]),
    ?line simple_log(a),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).


halt_int_sz_1(suite) -> [];
halt_int_sz_1(doc) -> ["Test simple halt disk log, size defined"];
halt_int_sz_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,18000},
				   {format,internal},
				   {file, File}]),
    ?line simple_log(a),
    ?line ok = disk_log:truncate(a),
    ?line [] = get_all_terms(a),
    T1 = mk_bytes(10000),
    T2 = mk_bytes(5000),
    ?line ok = disk_log:log(a, T1),
    ?line case get_all_terms(a) of
	      [T1] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, [T1]})
	  end,
    ?line ok = disk_log:log(a, T2),
    ?line {error, {full, a}} = disk_log:log(a, T1),
    ?line ok = disk_log:alog(a, T1),
    ?line case get_all_terms(a) of
	      [T1, T2] ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, [T1, T2]})
	  end,
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

halt_int_sz_2(suite) -> [];
halt_int_sz_2(doc) -> ["Test simple halt disk log, size ~8192"];
halt_int_sz_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,8191},
				   {format,internal},
				   {file, File1}]),
    ?line {ok, b} = disk_log:open([{name,b}, {type,halt}, {size,8192},
				   {format,internal},
				   {file, File2}]),
    ?line {ok, c} = disk_log:open([{name,c}, {type,halt}, {size,8193},
				   {format,internal},
				   {file, File3}]),
    T1 = mk_bytes(8191-16), % 16 is size of header + magics for 1 item
    T2 = mk_bytes(8192-16),
    T3 = mk_bytes(8193-16),
    ?line ok = disk_log:log(a, T1),
    ?line ok = disk_log:log(b, T2),
    ?line ok = disk_log:log(c, T3),
    ?line case get_all_terms(a) of
	      [T1] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, [T1]})
	  end,
    ?line case get_all_terms(b) of
	      [T2] ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, [T2]})
	  end,
    ?line case get_all_terms(c) of
	      [T3] ->
		  ok;
	      E3 ->
		  test_server_fail({bad_terms, E3, [T3]})
	  end,
    ?line ok = disk_log:truncate(a),
    ?line ok = disk_log:truncate(b),
    ?line {error, {full, a}} = disk_log:log(a, T2),
    ?line {error, {full, b}} = disk_log:log(b, T3),
    ?line [] = get_all_terms(a),
    ?line [] = get_all_terms(b),
    ?line ok = disk_log:close(a),
    ?line ok = disk_log:close(b),
    ?line ok = disk_log:close(c),
    ?line ok = file:delete(File1),
    ?line ok = file:delete(File2),
    ?line ok = file:delete(File3),
    ok.


halt_int_ro(suite) -> [];
halt_int_ro(doc) -> ["Test simple halt disk log, read only, internal"];
halt_int_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),

    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),

    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File},
				   {mode,read_only}]),
    T1 = "not allowed to write",
    ?line {error, {read_only_mode, a}} = disk_log:log(a, T1),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

halt_ext_ro(suite) -> [];
halt_ext_ro(doc) -> ["Test simple halt disk log, read only, external"];
halt_ext_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,external}, {file, File}]),
    xsimple_log(File, a),
    ?line ok = disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,external}, {file, File},
				   {mode,read_only}]),
    T1 = "not allowed to write",
    ?line {error, {read_only_mode, a}}  = disk_log:blog(a, T1),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

wrap_int_ro(suite) -> [];
wrap_int_ro(doc) -> ["Test simple wrap disk log, read only, internal"];
wrap_int_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,internal}, {file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,internal}, {file, File}, {mode,read_only}]),
    T1 = "not allowed to write",
    ?line {error, {read_only_mode, a}} = disk_log:log(a, T1),
    ?line ok = disk_log:close(a),
    ?line del(File, 4).

wrap_ext_ro(suite) -> [];
wrap_ext_ro(doc) -> ["Test simple wrap disk log, read only, external"];
wrap_ext_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,external}, {file, File}]),
    x2simple_log(File ++ ".1", a),
    ?line ok = disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,external}, {file, File},
				   {mode,read_only}]),
    T1 = "not allowed to write",
    ?line {error, {read_only_mode, a}}  = disk_log:blog(a, T1),
    ?line {error, {read_only_mode, a}}  = disk_log:inc_wrap_file(a),
    ?line ok = disk_log:close(a),
    del(File, 4).

halt_trunc(suite) -> [];
halt_trunc(doc) -> ["Test truncation of halt disk log"];
halt_trunc(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),
    ?line {error,{badarg,repair_read_only}} =
	disk_log:open([{name,a}, {type,halt}, {size,infinity},
		       {repair, truncate}, {format,internal},
		       {file, File}, {mode,read_only}]),
    ?line ok = file:delete(File).

halt_misc(suite) -> [];
halt_misc(doc) -> ["Test truncation of halt disk log"];
halt_misc(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File},
				   {mode,read_only}]),
    T1 = "not allowed to write",
    ?line {error, {read_only_mode, a}} = disk_log:log(a, T1),
    ?line {error, {read_only_mode, a}} = disk_log:sync(a),
    ?line {error, {read_only_mode, a}} = disk_log:reopen(a, "b.LOG"),
    ?line {error, {read_only_mode, a}} = 
        disk_log:change_header(a, {head,header}),
    ?line {error, {read_only_mode, a}} = 
        disk_log:change_size(a, infinity),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

halt_ro_alog(suite) -> [];
halt_ro_alog(doc) -> ["Test truncation of halt disk log, read only"];
halt_ro_alog(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {notify,true}, {format,internal},
				   {file, File}, {mode,read_only}]),
    T1 = "not allowed to write",
    ?line ok = disk_log:alog(a, T1),
    ?line ok = halt_ro_alog_wait_notify(a, T1),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

halt_ro_alog_wait_notify(Log, T) ->
    Term = term_to_binary(T),
    receive
	{disk_log, _, Log,{read_only, Term}} ->
	    ok;
	Other ->
	    Other
    after 5000 ->
	    failed
    end.

halt_ro_balog(suite) -> [];
halt_ro_balog(doc) -> ["Test truncation of halt disk log, read only"];
halt_ro_balog(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,internal}, {file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {notify,true}, {format,external},
				   {file, File}, {mode,read_only}]),
    T1 = "not allowed to write",
    ?line ok = disk_log:balog(a, T1),
    ?line ok = halt_ro_balog_wait_notify(a, T1),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

halt_ro_balog_wait_notify(Log, T) ->
    Term = list_to_binary(T),
    receive
	{disk_log, _, Log,{read_only, Term}} ->
	    ok;
	Other ->
	    Other
    after 5000 ->
	    failed
    end.

halt_ro_crash(suite) -> [];
halt_ro_crash(doc) -> ["Test truncation of halt disk log, read only, repair"];
halt_ro_crash(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),

    ?line file:delete(File),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			   {format,internal},{file, File}]),
    simple_log(a),
    ?line ok = disk_log:close(a),
    crash(File, 10),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {notify,true}, {format,internal},
				   {file, File}, {mode,read_only}]),

    ?line Error1 = {error, {read_only_mode, a}} = disk_log:truncate(a),
    ?line "The disk log" ++ _ = format_error(Error1),

    %% crash/1 sets the length of the first item to something big (2.5 kb).
    %% In R6B, binary_to_term accepts garbage at the end of the binary,
    %% which means that the first item is recognized!
    %% This is how it was before R6B:
    %% ?line {C1,T1,15} = disk_log:chunk(a,start),
    %% ?line {C2,T2} = disk_log:chunk(a,C1),
    {C1,_OneItem,7478} = disk_log:chunk(a,start),
    {C2, [], 7} = disk_log:chunk(a,C1),
    ?line eof = disk_log:chunk(a,C2),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).





wrap_int_1(suite) -> [];
wrap_int_1(doc) -> ["Test wrap disk log, internal"];
wrap_int_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,internal},
				   {file, File}]),
    ?line [_] = 
        lists:filter(fun(P) -> disk_log:pid2name(P) =/= undefined end, 
                     erlang:processes()),
    simple_log(a),
    ?line ok = disk_log:close(a),
    del(File, 4),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,internal},
				   {file, File}]),
    ?line [] = get_all_terms(a),
    T1 = mk_bytes(10000), % file 2
    T2 = mk_bytes(5000),  % file 3
    T3 = mk_bytes(4000),  % file 4
    T4 = mk_bytes(2000),  % file 4
    T5 = mk_bytes(5000),  % file 1
    T6 = mk_bytes(5000),  % file 2
    ?line ok = disk_log:log(a, T1),
    ?line ok = disk_log:log(a, T2),
    ?line ok = disk_log:log(a, T3),
    ?line ok = disk_log:log_terms(a, [T4, T5, T6]),
    ?line case get_all_terms(a) of
	      [T2,T3,T4,T5,T6] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, [T2,T3,T4,T5,T6]})
	  end,
    ?line ok = disk_log:close(a),
    del(File, 4).

wrap_int_2(suite) -> [];
wrap_int_2(doc) -> ["Test wrap disk log, internal"];
wrap_int_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8191,3}},
				   {format,internal},
				   {file, File1}]),
    ?line {ok, b} = disk_log:open([{name,b}, {type,wrap}, {size,{8192,3}},
				   {format,internal},
				   {file, File2}]),
    ?line {ok, c} = disk_log:open([{name,c}, {type,wrap}, {size,{8193,3}},
				   {format,internal},
				   {file, File3}]),
    T1 = mk_bytes(8191-16), % 16 is size of header + magics for 1 item
    T2 = mk_bytes(8192-16),
    T3 = mk_bytes(8193-16),
    ?line ok = disk_log:log(a, T1),
    ?line ok = disk_log:log(b, T2),
    ?line ok = disk_log:log(c, T3),
    ?line case get_all_terms(a) of
	      [T1] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, [T1]})
	  end,
    ?line case get_all_terms(b) of
	      [T2] ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, [T2]})
	  end,
    ?line case get_all_terms(c) of
	      [T3] ->
		  ok;
	      E3 ->
		  test_server_fail({bad_terms, E3, [T3]})
	  end,
    ?line ok = disk_log:close(a),
    ?line ok = disk_log:close(b),
    ?line ok = disk_log:close(c),
    del(File1, 3),
    del(File2, 3),
    del(File3, 3).

inc_wrap_file(suite) -> [];
inc_wrap_file(doc) -> ["Test disk log, force a change to next file"];
inc_wrap_file(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),

    %% Test that halt logs gets an error message
    ?line {ok, a} = disk_log:open([{name, a}, {type, halt}, 
				   {format, internal},
				   {file, File1}]),
    ?line ok = disk_log:log(a, "message one"),
    ?line {error, {halt_log, a}} = disk_log:inc_wrap_file(a),

    %% test an internally formatted wrap log file
    ?line {ok, b} = disk_log:open([{name, b}, {type, wrap}, {size, {100,3}},
				   {format, internal}, {head, 'thisisahead'},
				   {file, File2}]),
    ?line ok = disk_log:log(b, "message one"),
    ?line ok = disk_log:inc_wrap_file(b),
    ?line ok = disk_log:log(b, "message two"),
    ?line ok = disk_log:inc_wrap_file(b),
    ?line ok = disk_log:log(b, "message three"),
    ?line ok = disk_log:inc_wrap_file(b),
    ?line ok = disk_log:log(b, "message four"),
    ?line T1 = get_all_terms(b),
    ?line ['thisisahead', "message two", 
	   'thisisahead', "message three", 
	   'thisisahead', "message four"] = T1,

    %% test an externally formatted wrap log file
    ?line {ok, c} = disk_log:open([{name, c}, {type, wrap}, {size, {100,3}},
				   {format,external}, {head,"this is a head "},
				   {file, File3}]),
    ?line ok = disk_log:blog(c, "message one"),
    ?line ok = disk_log:inc_wrap_file(c),
    ?line ok = disk_log:blog(c, "message two"),
    ?line ok = disk_log:inc_wrap_file(c),
    ?line ok = disk_log:blog(c, "message three"),
    ?line ok = disk_log:inc_wrap_file(c),
    ?line ok = disk_log:blog(c, "message four"),
    ?line ok = disk_log:sync(c),
    ?line {ok, Fd31} = file:open(File3 ++ ".1", [read]),
    ?line {ok,"this is a head message four"} = file:read(Fd31, 200),
    ?line {ok, Fd32} = file:open(File3 ++ ".2", [read]),
    ?line {ok,"this is a head message two"} = file:read(Fd32, 200),
    ?line {ok, Fd33} = file:open(File3 ++ ".3", [read]),
    ?line {ok,"this is a head message three"} = file:read(Fd33, 200),
    ?line ok = file:close(Fd31),
    ?line ok = file:close(Fd32),
    ?line ok = file:close(Fd33),

    ?line ok = disk_log:close(a),
    ?line ok = disk_log:close(b),
    ?line ok = disk_log:close(c),
    ?line ok = file:delete(File1),
    del(File2, 3),
    del(File3, 3).




halt_ext_inf(suite) -> [];
halt_ext_inf(doc) -> ["Test halt disk log, external, infinity"];
halt_ext_inf(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
				   {format,external},
				   {file, File}]),
    ?line xsimple_log(File, a),
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).


halt_ext_sz_1(suite) -> [];
halt_ext_sz_1(doc) -> ["Test halt disk log, external, size defined"];
halt_ext_sz_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,18000},
				   {format,external},
				   {file, File}]),
    xsimple_log(File, a),
    ?line ok = disk_log:truncate(a),
    ?line [] = get_list(File, a),
    {B1, T1} = x_mk_bytes(10000),
    {B2, T2} = x_mk_bytes(5000),
    {B3, T3} = x_mk_bytes(1000),
    ?line ok = disk_log:blog(a, B1),
    ?line case get_list(File, a) of
	      T1 ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, T1})
	  end,
    ?line ok = disk_log:blog(a, B2),
    ?line {error, {full, a}} = disk_log:blog_terms(a, [B3,B3,B1]),
    ?line ok = disk_log:balog(a, B1),
    ?line Tmp = T1 ++ T2 ++ T3 ++ T3,
    ?line case get_list(File, a) of
	      Tmp ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, Tmp})
	  end,
    ?line ok = disk_log:close(a),
    ?line ok = file:delete(File).

halt_ext_sz_2(suite) -> [];
halt_ext_sz_2(doc) -> ["Test halt disk log, external, size defined"];
halt_ext_sz_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,8191},
				   {format,external},
				   {file, File1}]),
    ?line {ok, b} = disk_log:open([{name,b}, {type,halt}, {size,8192},
				   {format,external},
				   {file, File2}]),
    ?line {ok, c} = disk_log:open([{name,c}, {type,halt}, {size,8193},
				   {format,external},
				   {file, File3}]),
    {B1, T1} = x_mk_bytes(8191),
    {B2, T2} = x_mk_bytes(8192),
    {B3, T3} = x_mk_bytes(8193),
    ?line ok = disk_log:blog(a, B1),
    ?line ok = disk_log:blog(b, B2),
    ?line ok = disk_log:blog(c, B3),
    ?line case get_list(File1, a) of
	      T1 ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, T1})
	  end,
    ?line case get_list(File2, b) of
	      T2 ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, T2})
	  end,
    ?line case get_list(File3, c) of
	      T3 ->
		  ok;
	      E3 ->
		  test_server_fail({bad_terms, E3, T3})
	  end,
    ?line ok = disk_log:truncate(a),
    ?line ok = disk_log:truncate(b),
    ?line {error, {full, a}} = disk_log:blog(a, B2),
    ?line Error1 = {error, {full, b}} = disk_log:blog(b, B3),
    ?line "The halt log" ++ _ = format_error(Error1),
    ?line true = info(b, full, false),
    ?line [] = get_list(File1, a),
    ?line [] = get_list(File2, b),
    ?line ok = disk_log:close(a),
    ?line ok = disk_log:close(b),
    ?line ok = disk_log:close(c),
    ?line ok = file:delete(File1),
    ?line ok = file:delete(File2),
    ?line ok = file:delete(File3),
    ok.


wrap_ext_1(suite) -> [];
wrap_ext_1(doc) -> ["Test wrap disk log, external, size defined"];
wrap_ext_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,external},
				   {file, File}]),
    x2simple_log(File ++ ".1", a),
    ?line ok = disk_log:close(a),
%    del(File, 4),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
				   {format,external},
				   {file, File}]),
    {B1, _T1} = x_mk_bytes(10000), % file 2
    {B2, T2} = x_mk_bytes(5000),  % file 3
    {B3, T3} = x_mk_bytes(4000),  % file 4
    {B4, T4} = x_mk_bytes(2000),  % file 4
    {B5, T5} = x_mk_bytes(5000),  % file 1
    {B6, T6} = x_mk_bytes(5000),  % file 2
    ?line ok = disk_log:blog(a, B1),
    ?line ok = disk_log:blog(a, B2),
    ?line ok = disk_log:blog(a, B3),
    ?line ok = disk_log:blog_terms(a, [B4, B5, B6]),
    ?line case get_list(File ++ ".3", a) of
	      T2 ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, T2})
	  end,
    ?line T34 = T3 ++ T4,
    ?line case get_list(File ++ ".4", a) of
	      T34 ->
		  ok;
	      E34 ->
		  test_server_fail({bad_terms, E34, T34})
	  end,
    ?line case get_list(File ++ ".1", a) of
	      T5 ->
		  ok;
	      E5 ->
		  test_server_fail({bad_terms, E5, T5})
	  end,
    ?line case get_list(File ++ ".2", a) of
	      T6 ->
		  ok;
	      E6 ->
		  test_server_fail({bad_terms, E6, T6})
	  end,
    ?line ok = disk_log:close(a),
    del(File, 4).

wrap_ext_2(suite) -> [];
wrap_ext_2(doc) -> ["Test wrap disk log, external, size defined"];
wrap_ext_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8191,3}},
				   {format,external},
				   {file, File1}]),
    ?line {ok, b} = disk_log:open([{name,b}, {type,wrap}, {size,{8192,3}},
				   {format,external},
				   {file, File2}]),
    ?line {ok, c} = disk_log:open([{name,c}, {type,wrap}, {size,{8193,3}},
				   {format,external},
				   {file, File3}]),
    {B1, T1} = x_mk_bytes(8191),
    {B2, T2} = x_mk_bytes(8192),
    {B3, T3} = x_mk_bytes(8193),
    ?line ok = disk_log:blog(a, B1),
    ?line ok = disk_log:blog(b, B2),
    ?line ok = disk_log:blog(c, B3),
    ?line case get_list(File1 ++ ".1", a) of
	      T1 ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, T1})
	  end,
    ?line case get_list(File2 ++ ".1", b) of
	      T2 ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, T2})
	  end,
    ?line case get_list(File3 ++ ".1", c) of
	      T3 ->
		  ok;
	      E3 ->
		  test_server_fail({bad_terms, E3, T3})
	  end,
    ?line ok = disk_log:close(a),
    ?line ok = disk_log:close(b),
    ?line ok = disk_log:close(c),
    ?line del(File1, 3),
    ?line del(File2, 3),
    ?line del(File3, 3),
    ok.

simple_log(Log) ->
    T1 = "hej",
    T2 = hopp,
    T3 = {tjena, 12},
    T4 = mk_bytes(10000),
    ?line ok = disk_log:log(Log, T1),
    ?line ok = disk_log:log_terms(Log, [T2, T3]),
    ?line case get_all_terms(Log) of
	      [T1, T2, T3] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, [T1, T2, T3]})
	  end,
    ?line ok = disk_log:log(a, T4),
    ?line case get_all_terms(Log) of
	      [T1, T2, T3, T4] ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, [T1, T2, T3, T4]})
	  end.

xsimple_log(File, Log) ->
    T1 = "hej",
    T2 = list_to_binary("hopp"),
    T3 = list_to_binary(["sena", list_to_binary("sejer")]),
    T4 = list_to_binary(By = mk_bytes(10000)),
    ?line ok = disk_log:blog(Log, T1),
    ?line ok = disk_log:blog_terms(Log, [T2, T3]),
    ?line X = "hejhoppsenasejer",
    ?line X2 = get_list(File, Log),
    ?line case X2 of
	      X -> ok;
	      Z1 -> test_server_fail({bad_terms, Z1, X2})
	  end,
    ?line ok = disk_log:blog(Log, T4),
    ?line Tmp = get_list(File, Log),
    ?line case X ++ By of
	      Tmp -> ok;
	      Z2 -> test_server_fail({bad_terms, Z2, X ++ By})
	  end.

x2simple_log(File, Log) ->
    T1 = "hej",
    T2 = list_to_binary("hopp"),
    T3 = list_to_binary(["sena", list_to_binary("sejer")]),
    T4 = list_to_binary(By = mk_bytes(1000)),
    ?line ok = disk_log:blog(Log, T1),
    ?line ok = disk_log:blog_terms(Log, [T2, T3]),
    ?line X = "hejhoppsenasejer",
    ?line X2 = get_list(File, Log),
    ?line case X2 of
	      X -> ok;
	      Z1 -> test_server_fail({bad_terms, Z1, X2})
	  end,
    ?line ok = disk_log:blog(Log, T4),
    ?line Tmp = get_list(File, Log),
    ?line case X ++ By of
	      Tmp -> ok;
	      Z2 -> test_server_fail({bad_terms, Z2, X ++ By})
	  end.

x_mk_bytes(N) ->
    X = lists:duplicate(N, $a),
    {list_to_binary(X), X}.

mk_bytes(N) when N > 4 ->
    X = lists:duplicate(N-4, $a),
    case byte_size(term_to_binary(X)) of
        N -> X;
        Z -> test_server_fail({bad_terms, Z, N})
    end.

get_list(File, Log) ->
    ?t:format(0, "File ~p~n",[File]),
    ok = disk_log:sync(Log),
    {ok, B} = file:read_file(File),
    binary_to_list(B).


get_all_terms(Log, File, Type) ->
    {ok, _Log} = disk_log:open([{name,Log}, {type,Type}, {size,infinity},
			       {format,internal}, {file, File}, 
				{mode, read_only}]),
    Ts = get_all_terms(Log),
    ok = disk_log:close(Log),
    Ts.

get_all_terms(Log) ->
    get_all_terms1(Log, start, []).

get_all_terms1(Log, Cont, Res) ->
    case disk_log:chunk(Log, Cont) of
	{error, _R} ->
	    test_server_fail({bad_chunk, Log, Cont});
	{Cont2, Terms} ->
	    get_all_terms1(Log, Cont2, Res ++ Terms);
	eof ->
	    Res
    end.

get_all_terms_and_bad(Log, File, Type) ->
    {ok, _Log} = disk_log:open([{name,Log}, {type,Type}, {size,infinity},
			       {format,internal}, {file, File}, 
				{mode, read_only}]),
    Ts = get_all_terms_and_bad(Log),
    ok = disk_log:close(Log),
    Ts.

get_all_terms_and_bad(Log) ->
    ?line read_only = info(Log, mode, foo),
    get_all_terms_and_bad1(Log, start, [], 0).

%% 
get_all_terms_and_bad1(Log, Cont, Res, Bad0) ->
    case disk_log:chunk(Log, Cont) of
	{Cont2, Terms} ->
	    get_all_terms_and_bad1(Log, Cont2, Res ++ Terms, Bad0);
	{Cont2, Terms, Bad} ->
	    get_all_terms_and_bad1(Log, Cont2, Res ++ Terms, Bad0+Bad);
	eof ->
	    {Res, Bad0}
    end.

get_all_binary_terms_and_bad(Log, File, Type) ->
    {ok, _Log} = disk_log:open([{name,Log}, {type,Type}, {size,infinity},
			       {format,internal}, {file, File}, 
				{mode, read_only}]),
    Ts = get_all_binary_terms_and_bad(Log),
    ok = disk_log:close(Log),
    Ts.

get_all_binary_terms_and_bad(Log) ->
    read_only = info(Log, mode, foo),
    get_all_binary_terms_and_bad1(Log, start, [], 0).

%% 
get_all_binary_terms_and_bad1(Log, Cont, Res, Bad0) ->
    case disk_log:bchunk(Log, Cont) of
	{Cont2, BinTerms} ->
	    get_all_binary_terms_and_bad1(Log, Cont2, Res ++ BinTerms, Bad0);
	{Cont2, BinTerms, Bad} ->
	    get_all_binary_terms_and_bad1(Log, Cont2, Res ++ BinTerms, 
                                          Bad0+Bad);
	eof ->
	    {Res, Bad0}
    end.

del(File, 0) ->
    file:delete(File ++ ".siz"),
    file:delete(File ++ ".idx");
del(File, N) ->
    file:delete(File ++ "." ++ integer_to_list(N)),
    del(File, N-1).

test_server_fail(R) ->
    exit({?MODULE, get(line), R}).

xx() ->
    File = "a.LOG",
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    W = xwr(a, 400),
    disk_log:close(a),
%    file:delete(File),
    W.

%% old: 6150
%% new: 5910
xwr(Log, BytesItem) ->
    NoW = 1000,
    Item1 = mk_bytes(BytesItem),
    Item2 = mk_bytes(BytesItem),
    Item3 = mk_bytes(BytesItem),
    Item4 = mk_bytes(BytesItem),
    Item5 = mk_bytes(BytesItem),
    Item6 = mk_bytes(BytesItem),
    Item7 = mk_bytes(BytesItem),
    Item8 = mk_bytes(BytesItem),
    Item9 = mk_bytes(BytesItem),
    Item0 = mk_bytes(BytesItem),
    Term = [Item1,Item2,Item3,Item4,Item5,Item6,Item7,Item8,Item9,Item0],
    {W, _} = timer:tc(?MODULE, wr, [Log, Term, NoW]),
    W/NoW.

measure() ->
    proc_lib:start_link(?MODULE, init_m, [self()]).

init_m(Par) ->
    process_flag(trap_exit, true),
    Res = m(),
    proc_lib:init_ack(Par, Res).

m() ->
    {W10, R10, Rep10, C10} = m_halt_int(10),
    {W11, R11, Rep11, C11} = m_halt_int(100),
    {W12, R12, Rep12, C12} = m_halt_int(400),
    {W13, R13, Rep13, C13} = m_halt_int(1000),
    {W14, R14, Rep14, C14} = m_halt_int(10000),
    {W2, R2, Rep2, C2} = m_wrap_int(400),
    {W3, R3, Rep3, C3} = m_many_halt_int(10, 400),
    {W4, R4, Rep4, C4} = m_many_halt_int(20, 400),
    {W5, R5, Rep5, C5} = m_many_halt_int(10, 1000),
    {W6, R6, Rep6, C6} = m_many_halt_int(10, 10),
    {W7, R7, Rep7, C7} = m_many_halt_int(20, 10),

    io:format("Type of log            mysec/write  mysec/read"
	      "  mysec/repair byte  cpu/write\n"),
    io:format("===========            ===========  =========="
	      "  =================  =========\n"),
    one_line("halt,int.inf. (10)", W10, R10, Rep10, C10),
    one_line("halt,int.inf. (100)", W11, R11, Rep11, C11),
    one_line("halt,int.inf. (400)", W12, R12, Rep12, C12),
    one_line("halt,int.inf. (1000)", W13, R13, Rep13, C13),
    one_line("halt,int.inf. (10000)", W14, R14, Rep14, C14),
    one_line("wrap,int.  4. (400)", W2, R2, Rep2, C2),
    one_line("halt,int.inf. (10,10)", W6, R6, Rep6, C6),
    one_line("halt,int.inf. (20,10)", W7, R7, Rep7, C7),
    one_line("halt,int.inf. (10,400)", W3, R3, Rep3, C3),
    one_line("halt,int.inf. (20,400)", W4, R4, Rep4, C4),
    one_line("halt,int.inf. (10,1000)", W5, R5, Rep5, C5),
    io:format("\n"),
    io:format("\tWrap log time depends on how often the log wraps, as this\n"),
    io:format("\tinvolves opening of new files, which costs alot."),
    io:format("\n").

one_line(Txt, W, R, Rep, C) ->
    io:format("~.22s  ~.10w  ~.10w  ~.17w  ~.9w\n", [Txt, W, R, Rep, C]).

m_halt_int(BytesItem) ->
    File = "a.LOG",
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    {T,W} = wr(a, BytesItem),
    R = r(a),
    [{_,P}] = ets:lookup(?DISK_LOG_NAME_TABLE, a),
    exit(P, kill),
    receive after 100 -> ok end,
    crash(File, 10),
    Sz = file_size(File),
    Start = start_times(),
    {repaired, a, {recovered, Rec}, {badbytes, Bad}} = 
	disk_log:open([{name,a}, {type,halt}, {size,infinity},
		       {format,internal}, {file, File}]),
    {_,Rep} = end_times(Start),
    io:format("m_halt_int: Rep = ~p, Rec = ~p, Bad = ~p~n", [Rep, Rec, Bad]),
    disk_log:close(a),
    file:delete(File),
    {W,R,1000*Rep/Sz,T}.

m_wrap_int(BytesItem) ->
    File = "a.LOG",
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{405*1000, 4}},
			     {format,internal}, {file, File}]),
    {T,W} = wr(a, BytesItem),
    R = r(a),
    [{_,P}] = ets:lookup(?DISK_LOG_NAME_TABLE, a),
    exit(P, kill),
    receive after 100 -> ok end,
    del(File, 4),
    {W,R,'n/a',T}.

m_many_halt_int(NoClients, BytesItem) ->
    Name = 'log.LOG',
    File = "log.LOG",
    {ok, _} = disk_log:open([{name,Name}, {type,halt}, 
			     {size,infinity},
			     {format,internal}, {file,File}]),
    NoW = round(lists:max([lists:min([5000000/BytesItem/NoClients,
				      50000/NoClients]),
			   1000])),
    {T,W} = many_wr(NoClients, Name, NoW, BytesItem),
    ok = disk_log:close(Name),
    file:delete(File),
    {1000*W/NoW/NoClients,'n/a','n/a',1000*T/NoW/NoClients}.

many_wr(NoClients, Log, NoW, BytesItem) ->
    Item = mk_bytes(BytesItem),
    Fun = fun(Name, _Pid, _I) -> disk_log:log(Name, Item) end,
    Start = start_times(),
    Pids = spawn_clients(NoClients, client, [self(), Log, NoW, Fun]),
    check_clients(Pids),
    end_times(Start).

wr(Log, BytesItem) ->
    NoW = round(lists:max([lists:min([5000000/BytesItem,50000]),1000])),
    Item = mk_bytes(BytesItem),
    Start = start_times(),
    wr(Log, Item, NoW),
    {T,W} = end_times(Start),
    {1000*T/NoW, 1000*W/NoW}.

wr(Log, _Item, 0) ->
    disk_log:sync(Log),
    ok;
wr(Log, Item, N) ->
    ok = disk_log:log(Log, Item),
    wr(Log, Item, N-1).

r(_) ->
    nyi.

start_times() ->
    {T1, _} = statistics(runtime),
    {W1, _} = statistics(wall_clock),
    {T1, W1}.

end_times({T1,W1}) ->
    {T2, _} = statistics(runtime),
    {W2, _} = statistics(wall_clock),
    {T2-T1, W2-W1}.


head_func(suite) -> [];
head_func(doc) -> ["Test head parameter"];
head_func(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ets:new(xxx, [named_table, set, public]),
    ets:insert(xxx, {wrapc, 0}),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
				   {size, {100,4}},
				   {head_func, {?MODULE, hf, []}}]),
    ?line B = mk_bytes(60),
    ?line disk_log:log(a, B),
    ?line disk_log:alog(a, B),
    ?line disk_log:alog(a, B),
    ?line disk_log:log(a, B),
    H = [1,2,3],
    ?line [{wrapc, 4}] = ets:lookup(xxx, wrapc),
    ets:delete(xxx),
    ?line case get_all_terms(a) of
	      [H,B,H,B,H,B,H,B] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, 
				    [H,B,H,B,H,B,H,B]})
	  end,
    ?line 8  = no_written_items(a),
    disk_log:close(a),
    del(File, 4),

    % invalid header function
    ?line {error, {invalid_header, {_, {term}}}} = 
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{term}]}}]),
    file:delete(File),

    ?line {error, {invalid_header, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{ok,{term}}]}}]),
    file:delete(File),

    ?line {ok,n} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{ok,<<"head">>}]}}]),
    ?line ok = disk_log:close(n),
    ?line {ok,<<"head">>} = file:read_file(File),
    file:delete(File),

    ?line {ok,n} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{ok,"head"}]}}]),
    ?line ok = disk_log:close(n),
    ?line {ok,<<"head">>} = file:read_file(File),
    file:delete(File),

    ?line Error1 = {error, {badarg, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {head_func, {tjo,hej,san}},{size, {100, 4}}]),
    ?line "The argument " ++ _ = format_error(Error1),
    
    ?line Error2 = {error, {invalid_header, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {head_func, {tjo,hej,[san]}}]),
    ?line "The disk log header" ++ _ = format_error(Error2),
    file:delete(File).


head_fun(H) ->
    H.

hf() ->
    ets:update_counter(xxx, wrapc, 1),
    {ok, [1,2,3]}.

plain_head(suite) -> [];
plain_head(doc) -> ["Test head parameter"];
plain_head(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    H = [1,2,3],
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
				   {size, {100,4}}, {head, H}]),
    %% This one is not "counted".
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
				   {size, {100,4}}, {head, H}]),
    ?line B = mk_bytes(60),
    ?line disk_log:log(a, B),
    ?line disk_log:alog(a, B),
    ?line disk_log:alog(a, B),
    ?line disk_log:log(a, B),
    ?line case get_all_terms(a) of
	      [H,B,H,B,H,B,H,B] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, 
				    [H,B,H,B,H,B,H,B]})
	  end,
    ?line 8 = no_written_items(a),
    ?line ok = disk_log:close(a),
    ?line {error, no_such_log} = disk_log:close(a),
    del(File, 4).



one_header(suite) -> [];
one_header(doc) -> ["Test that a header is just printed once in a log file"];
one_header(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    H = [1,2,3],
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
				   {size, {100,4}}, {head, H}]),
    ?line B = mk_bytes(60),
    ?line ok = disk_log:log(a, B),
    ?line ok = disk_log:alog(a, B),
    ?line ok = disk_log:alog(a, B),
    ?line ok = disk_log:log(a, B),
    ?line case get_all_terms(a) of
	      [H,B,H,B,H,B,H,B] ->
		  ok;
	      E1 ->
		  test_server_fail({bad_terms, E1, 
				    [H,B,H,B,H,B,H,B]})
	  end,
    ?line 8  = no_written_items(a),
    ?line ok = disk_log:close(a),
    del(File, 4),

    Fileb = filename:join(Dir, "b.LOG"),
    ?line {ok, b} = disk_log:open([{name,b}, {file, Fileb}, {head, H}]),
    ?line ok = disk_log:close(b),
    ?line {ok, b} = disk_log:open([{name,b}, {file, Fileb}, {head, H}]),
    ?line ok = disk_log:log(b, "first log"),
    ?line ok = disk_log:alog(b, "second log"),
    ?line ok = disk_log:close(b),
    ?line {ok, b} = disk_log:open([{name,b}, {file, Fileb}, {head, H}]),
    ?line ok = disk_log:alog(b, "3rd log"),
    ?line ok = disk_log:log(b, "4th log"),
    ?line case get_all_terms(b) of
	      [H, "first log", "second log", "3rd log", "4th log"] ->
		  ok;
	      E2 ->
		  test_server_fail({bad_terms, E2, 
				    [H, "first log", "second log", 
				     "3rd log", "4th log"]})
	  end,
    ?line 2  = no_written_items(b),
    ?line ok = disk_log:close(b),
    ?line ok = file:delete(Fileb),

    Filec = filename:join(Dir, "c.LOG"),
    H2 = "this is a header ",
    ?line {ok, c} = disk_log:open([{name,c}, {format, external}, 
				   {file, Filec}, {head, H2}]),
    ?line ok = disk_log:close(c),
    ?line {ok, c} = disk_log:open([{name,c}, {format, external}, 
				   {file, Filec}, {head, H2}]),
    ?line ok = disk_log:blog(c, "first log"),
    ?line ok = disk_log:balog(c, "second log"),
    ?line ok = disk_log:close(c),
    ?line {ok, c} = disk_log:open([{name,c}, {format, external}, 
				   {file, Filec}, {head, H2}]),
    ?line ok = disk_log:balog(c, "3rd log"),
    ?line ok = disk_log:blog(c, "4th log"),
    ?line ok = disk_log:sync(c),
    ?line {ok, Fdc} = file:open(Filec, [read]),
    ?line {ok,"this is a header first logsecond log3rd log4th log"} = 
	file:read(Fdc, 200),
    ?line ok = file:close(Fdc),
    ?line 2  = no_written_items(c),
    ?line disk_log:close(c),
    ?line ok = file:delete(Filec),
    ok.



wrap_notif(suite) -> [];
wrap_notif(doc) -> ["Test notify parameter, wrap"];
wrap_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
				   {size, {100,4}}, {notify, true}]),
    ?line B = mk_bytes(60),
    ?line disk_log:log(a, B),
    ?line disk_log:alog(a, B),
    ?line disk_log:alog(a, B),
    ?line disk_log:log(a, B),
    ?line disk_log:log(a, B),
    ?line rec(3, {disk_log, node(), a, {wrap, 0}}),
    ?line rec(1, {disk_log, node(), a, {wrap, 1}}),
    disk_log:close(a),
    del(File, 4).

full_notif(suite) -> [];
full_notif(doc) -> ["Test notify parameter, wrap, filled file"];
full_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    file:delete(File),

    ?line {ok, a} = disk_log:open([{name, a}, {file, File}, {type, halt},
				   {size, 100}, {notify, true}]),
    ?line B = mk_bytes(60),
    ?line disk_log:log(a, B),
    ?line disk_log:alog(a, B),
    ?line rec(1, {disk_log, node(), a, full}),
    disk_log:close(a),
    file:delete(File).

trunc_notif(suite) -> [];
trunc_notif(doc) -> ["Test notify parameter, wrap, truncated file"];
trunc_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "a.DUMP"),
    ?line {ok, a} = disk_log:open([{name, a}, {file, File}, {type, halt},
				   {size, 100}, {notify, true}]),
    ?line B = mk_bytes(60),
    ?line disk_log:log(a, B),
    ?line disk_log:truncate(a),
    ?line rec(1, {disk_log, node(), a, {truncated, 1}}),
    ?line disk_log:log(a, B),
    ?line ok = disk_log:reopen(a, File2),
    ?line rec(1, {disk_log, node(), a, {truncated, 1}}),
    disk_log:close(a),
    file:delete(File),
    file:delete(File2).

blocked_notif(suite) -> [];
blocked_notif(doc) -> 
    ["Test notify parameters 'format_external' and 'blocked_log"];
blocked_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true}, 
				   {format, external}]),
    ?line B = mk_bytes(60),
    ?line Error1 = {error,{format_external,n}} = disk_log:log(n, B),
    ?line "The requested operation" ++ _ = format_error(Error1),
    ?line ok = disk_log:blog(n, B),
    ?line ok = disk_log:alog(n, B),
    ?line rec(1, {disk_log, node(), n, {format_external, term_to_binary(B)}}),
    ?line ok = disk_log:alog_terms(n, [B,B,B,B]),
    ?line rec(1, {disk_log, node(), n, {format_external, 
				 lists:map(fun term_to_binary/1, [B,B,B,B])}}),
    ?line ok = disk_log:block(n, false),
    ?line ok = disk_log:alog(n, B),
    ?line rec(1, {disk_log, node(), n, {blocked_log, term_to_binary(B)}}),
    ?line ok = disk_log:balog(n, B),
    ?line rec(1, {disk_log, node(), n, {blocked_log, list_to_binary(B)}}),
    ?line ok = disk_log:balog_terms(n, [B,B,B,B]),
    ?line disk_log:close(n),
    ?line rec(1, {disk_log, node(), n, {blocked_log, 
				 lists:map(fun list_to_binary/1, [B,B,B,B])}}),
    ?line del(File, No).


new_idx_vsn(suite) -> [];
new_idx_vsn(doc) -> ["Test the new version of the .idx file"];
new_idx_vsn(Conf) when is_list(Conf) ->
    DataDir = ?datadir(Conf),
    PrivDir = ?privdir(Conf),
    File = filename:join(PrivDir, "new_vsn.LOG"),
    Kurt = filename:join(PrivDir, "kurt.LOG"),
    Kurt2 = filename:join(PrivDir, "kurt2.LOG"),

    %% Test that a wrap log file can have more than 255 files
    ?line {ok, new_vsn} = disk_log:open([{file, File}, {name, new_vsn}, 
					 {type, wrap}, {size, {40, 270}}]),
    ?line ok = log(new_vsn, 280),
    ?line {ok, Bin} = file:read_file(add_ext(File, "idx")),
    ?line <<0,0:32,2,10:32,1:64,1:64,_/binary>> = Bin,
    ?line disk_log:close(new_vsn),
    ?line del(File, 270),

    %% convert a very old version (0) of wrap log file to the new format (2)
    copy_wrap_log("kurt.LOG", 4, DataDir, PrivDir),

    ?line {repaired, kurt, {recovered, 1}, {badbytes, 0}} =
	disk_log:open([{file, Kurt}, {name, kurt}, 
		       {type, wrap}, {size, {40, 4}}]),
    ?line ok = disk_log:log(kurt, "this is a logged message number X"),
    ?line ok = disk_log:log(kurt, "this is a logged message number Y"),
    ?line {ok, BinK} = file:read_file(add_ext(Kurt, "idx")),
    ?line <<0,0:32,2,2:32,1:64,1:64,1:64,1:64>> = BinK,
    ?line {{40,4}, 2} = disk_log_1:read_size_file_version(Kurt),
    disk_log:close(kurt),
    ?line del(Kurt, 4),

    %% keep the old format (1)
    copy_wrap_log("kurt2.LOG", 4, DataDir, PrivDir),

    ?line {repaired, kurt2, {recovered, 1}, {badbytes, 0}} =
	disk_log:open([{file, Kurt2}, {name, kurt2}, 
		       {type, wrap}, {size, {40, 4}}]),
    ?line ok = disk_log:log(kurt2, "this is a logged message number X"),
    ?line ok = disk_log:log(kurt2, "this is a logged message number Y"),
    ?line {ok, BinK2} = file:read_file(add_ext(Kurt2, "idx")),
    ?line <<0,2:32,1:32,1:32,1:32,1:32>> = BinK2,
    ?line {{40,4}, 1} = disk_log_1:read_size_file_version(Kurt2),
    disk_log:close(kurt2),
    ?line del(Kurt2, 4),

    ok.

reopen(suite) -> [];
reopen(doc) -> 
    ["Test reopen/1 on halt and wrap logs."];
reopen(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line NewFile = filename:join(Dir, "nn.LOG"),
    ?line B = mk_bytes(60),
    
    ?line file:delete(File),    % cleanup
    ?line file:delete(NewFile), % cleanup
    ?line Q = qlen(),

    %% External halt log.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {notify, true}, {head, "header"}, 
				   {size, infinity},{format, external}]),
    ?line ok = disk_log:blog(n, B),
    ?line ok = disk_log:breopen(n, NewFile, "head"),
    ?line rec(1, {disk_log, node(), n, {truncated, 2}}),
    ?line ok = disk_log:blog(n, B),
    ?line ok = disk_log:blog(n, B),
    ?line ok = disk_log:breopen(n, NewFile, "head"),
    ?line rec(1, {disk_log, node(), n, {truncated, 3}}),
    ?line ok = disk_log:close(n),
    ?line {ok,BinaryFile} = file:read_file(File),
    ?line "head" = binary_to_list(BinaryFile),
    ?line file:delete(File),
    ?line file:delete(NewFile),

    %% Internal halt log.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {notify, true}, {head, header}, 
				   {size, infinity}]),
    ?line ok = disk_log:log(n, B),
    ?line Error1 = {error, {same_file_name, n}} = disk_log:reopen(n, File),
    ?line "Current and new" ++ _ = format_error(Error1),
    ?line ok = disk_log:reopen(n, NewFile),
    ?line rec(1, {disk_log, node(), n, {truncated, 2}}),
    ?line ok = disk_log:log(n, B),
    ?line ok = disk_log:log(n, B),
    ?line ok = disk_log:reopen(n, NewFile),
    ?line rec(1, {disk_log, node(), n, {truncated, 3}}),
    ?line ok = disk_log:close(n),
    ?line [header, _B, _B] = get_all_terms(nn, NewFile, halt),
    ?line file:delete(File),
    ?line file:delete(NewFile),

    %% Internal wrap log.
    ?line No = 4,
    ?line del(File, No),	% cleanup
    ?line del(NewFile, No),	% cleanup

    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, 
				   {head, header}, {size, {100, No}}]),
    ?line ok = disk_log:log(n, B),
    ?line ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(3, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line ok = disk_log:reopen(n, NewFile, new_header),
    ?line rec(1, {disk_log, node(), n, {truncated, 8}}),
    ?line ok = disk_log:log_terms(n, [B,B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:close(n),
    ?line [header, _, header, _, header, _, header, _] = 
	get_all_terms(nn, NewFile, wrap),
    ?line [new_header, _, header, _, header, _] = get_all_terms(n, File, wrap),

    ?line del(NewFile, No),
    ?line file:delete(File ++ ".2"),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, 
				   {head, header}, {size, {100, No}}]),
    %% One file is missing...
    ?line ok = disk_log:reopen(n, NewFile),
    ?line rec(1, {disk_log, node(), n, {truncated, 6}}),
    ?line ok = disk_log:close(n),

    ?line del(File, No),
    ?line del(NewFile, No),
    ?line Q = qlen(),
    ok.


block_blocked(suite) -> [];
block_blocked(doc) -> 
    ["Test block/1 on external and internal logs."];
block_blocked(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line B = mk_bytes(60),
    Halt = join(Dir, "halt.LOG"),

    % External logs.
    ?line file:delete(Halt), % cleanup
    ?line {ok, halt} = disk_log:open([{name, halt}, {type, halt}, 
				      {format, external}, {file, Halt}]),
    ?line ok = disk_log:sync(halt),
    ?line ok = disk_log:block(halt, false),
    ?line Error1 = {error, {blocked_log, halt}} = disk_log:block(halt),
    ?line "The blocked disk" ++ _ = format_error(Error1),
    ?line {error, {blocked_log, halt}} = disk_log:sync(halt),
    ?line {error, {blocked_log, halt}} = disk_log:truncate(halt),
    ?line {error, {blocked_log, halt}} = disk_log:change_size(halt, infinity),
    ?line {error, {blocked_log, halt}} = 
        disk_log:change_notify(halt, self(), false),
    ?line {error, {blocked_log, halt}} = 
        disk_log:change_header(halt, {head, header}),
    ?line {error, {blocked_log, halt}} = disk_log:reopen(halt, "foo"),
    ?line ok = disk_log:close(halt),

    ?line {ok, halt} = disk_log:open([{name, halt}, {type, halt}, 
				      {format, external}]),
    ?line ok = disk_log:sync(halt),
    ?line ok = disk_log:block(halt, true),
    ?line {error, {blocked_log, halt}} = disk_log:blog(halt, B),
    ?line {error, {blocked_log, halt}} = disk_log:blog(halt, B),
    ?line {error, {blocked_log, halt}} = disk_log:block(halt),
    ?line {error, {blocked_log, halt}} = disk_log:sync(halt),
    ?line {error, {blocked_log, halt}} = disk_log:truncate(halt),
    ?line {error, {blocked_log, halt}} = disk_log:change_size(halt, infinity),
    ?line {error, {blocked_log, halt}} = 
        disk_log:change_notify(halt, self(), false),
    ?line {error, {blocked_log, halt}} = 
        disk_log:change_header(halt, {head, header}),
    ?line {error, {blocked_log, halt}} = disk_log:reopen(halt, "foo"),

    ?line ok = disk_log:unblock(halt),
    ?line ok = disk_log:close(halt),
    ?line file:delete(Halt),

    % Internal logs.
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No), % cleanup
    ?line {ok, halt} = disk_log:open([{name, halt}, {file, File}, {type, wrap},
				      {size, {100, No}}]),
    ?line ok = disk_log:block(halt, true),
    ?line eof = disk_log:chunk(halt, start),
    ?line Error2 = {error, end_of_log} = disk_log:chunk_step(halt, start, 1),
    ?line "An attempt" ++ _ = format_error(Error2),
    ?line {error, {blocked_log, halt}} = disk_log:log(halt, B),
    ?line {error, {blocked_log, halt}} = disk_log:inc_wrap_file(halt),
    ?line ok = disk_log:unblock(halt),
    ?line ok = disk_log:block(halt, false),
    ?line {error, {blocked_log, halt}} = disk_log:log(halt, B),
    ?line {error, {blocked_log, halt}} = disk_log:inc_wrap_file(halt),
    ?line Parent = self(),
    ?line Pid = 
        spawn_link(fun() -> 
                           {error, {blocked_log, halt}} = 
                               disk_log:chunk(halt, start),
                           {error, {blocked_log, halt}} = 
                               disk_log:chunk_step(halt, start, 1),
                           Parent ! {self(), stopped}
                   end),
    ?line receive {Pid,stopped} -> ok end,
    ?line ok = disk_log:close(halt),
    ?line del(File, No).

block_queue(suite) -> [];
block_queue(doc) -> 
    ["Run commands from the queue by unblocking."];
block_queue(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line Q = qlen(),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No), % cleanup
    ?line B = mk_bytes(60),    

    ?line Pid = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid, {open, File}),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {blog, B}),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line 1 = no_written_items(n),
    ?line Error1 = {error,{not_blocked,n}} = disk_log:unblock(n),
    ?line "The disk log" ++ _ = format_error(Error1),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {balog, "one string"}),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line 2 = no_written_items(n),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, sync),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    
    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, truncate),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line 0 = no_items(n),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {block, false}),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line {error, {blocked_log, _}} = disk_log:blog(n, B),
    ?line ok = sync_do(Pid, unblock),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {change_notify, Pid, true}),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line [{_, true}] = owners(n),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {change_notify, Pid, false}),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line [{_, false}] = owners(n),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {change_header, {head, header}}),
    ?line ok = disk_log:unblock(n),
    ?line {error, {badarg, head}} = get_reply(),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {change_size, 17}),
    ?line ok = disk_log:unblock(n),
    ?line {error, {badarg, size}} = get_reply(),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, inc_wrap_file),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),

    ?line ok = sync_do(Pid, close),
    ?line del(File, No),

    ?line _Pid2 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid, {int_open, File}),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {chunk, start}),
    ?line ok = disk_log:unblock(n),
    ?line eof = get_reply(),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {chunk_step, start, 100}),
    ?line ok = disk_log:unblock(n),
    ?line {ok, _Cont} = get_reply(),

    ?line ok = disk_log:block(n, true),
    ?line async_do(Pid, {log,a_term}),
    ?line ok = disk_log:unblock(n),
    ?line ok = get_reply(),
    ?line 1 = no_written_items(n),

    ?line ok = sync_do(Pid, close),
    ?line sync_do(Pid, terminate),
    ?line del(File, No),

    %% Test of the queue. Three processes involved here. Pid1's block
    %% request is queued. Pid2's log requests are put in the queue.
    %% When unblock is executed, Pid1's block request is granted.
    %% Pid2's log requests are executed when Pid1 unblocks.
    %% (This example should show that the pair 'queue' and 'messages' 
    %% in State does the trick - one does not need a "real" queue.)
    ?line P0 = pps(),
    Name = n,
    ?line Pid1 = spawn_link(?MODULE, lserv, [Name]),
    ?line {ok, Name} = sync_do(Pid1, {int_open, File, {1000,2}}),
    ?line Pid2 = spawn_link(?MODULE, lserv, [Name]),
    ?line {ok, Name} = sync_do(Pid2, {int_open, File, {1000,2}}),
    ?line ok = disk_log:block(Name),
    ?line async_do(Pid1, {alog,{1,a}}),
    ?line ok = get_reply(),
    ?line async_do(Pid1, {alog,{2,b}}),
    ?line ok = get_reply(),
    ?line async_do(Pid1, {alog,{3,c}}),
    ?line ok = get_reply(),
    ?line async_do(Pid1, {alog,{4,d}}),
    ?line ok = get_reply(),
    ?line async_do(Pid1, block),
    ?line async_do(Pid2, {alog,{5,e}}),
    ?line ok = get_reply(),
    ?line async_do(Pid2, {alog,{6,f}}),
    ?line ok = get_reply(),
    ?line ok = disk_log:unblock(Name),
    ?line ok = get_reply(),
    ?line async_do(Pid2, {alog,{7,g}}),
    ?line ok = get_reply(),
    ?line async_do(Pid2, {alog,{8,h}}),
    ?line ok = get_reply(),
    ?line async_do(Pid1, unblock),
    ?line ok = get_reply(),
    ?line ok = sync_do(Pid1, close),
    ?line ok = sync_do(Pid2, close),
    ?line sync_do(Pid1, terminate),
    ?line sync_do(Pid2, terminate),
    Terms = get_all_terms(Name, File, wrap),
    ?line true = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g},{8,h}] == Terms,
    del(File, 2),
    ?line Q = qlen(),
    ?line true = (P0 == pps()),
    ok.

block_queue2(suite) -> [];
block_queue2(doc) -> 
    ["OTP-4880. Blocked processes did not get disk_log_stopped message."];
block_queue2(Conf) when is_list(Conf) ->
    ?line Q = qlen(),
    ?line P0 = pps(),
    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,

    %% log requests are queued, and processed when the log is closed
    ?line Pid = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid, {open, File}),
    ?line ok = sync_do(Pid, block),
    %% Asynchronous stuff is ignored.
    ?line ok = disk_log:balog_terms(n, [<<"foo">>,<<"bar">>]),
    ?line ok = disk_log:balog_terms(n, [<<"more">>,<<"terms">>]),
    Parent = self(),
    ?line Fun = 
        fun() ->
                {error,no_such_log} = disk_log:sync(n),
                receive {disk_log, _, {error, disk_log_stopped}} -> ok end,
                Parent ! disk_log_stopped_ok
        end,
    ?line spawn(Fun),
    ?line ok = sync_do(Pid, close),
    ?line receive disk_log_stopped_ok -> ok end,
    ?line sync_do(Pid, terminate),
    ?line {ok,<<>>} = file:read_file(File ++ ".1"),
    ?line del(File, No),    
    ?line Q = qlen(),
    ?line true = (P0 == pps()),
    ok.


unblock(suite) -> [];
unblock(doc) -> 
    ["Test unblock/1."];
unblock(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 1,
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true}, 
				   {format, external}]),
    ?line ok = disk_log:block(n),
    ?line spawn_link(?MODULE, try_unblock, [n]),
    ?line timer:sleep(100),
    ?line disk_log:close(n),
    ?line del(File, No).

try_unblock(Log) ->
    ?line Error = {error, {not_blocked_by_pid, n}} = disk_log:unblock(Log),
    ?line "The disk log" ++ _ = format_error(Error).


open_overwrite(suite) -> [];
open_overwrite(doc) -> 
    ["Test open/1 when old files exist."];
open_overwrite(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No), % cleanup

    % read write
    ?line First = "n.LOG.1",
    ?line make_file(Dir, First, 8),

    ?line Error1 = {error, {not_a_log_file, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No}}]),
    ?line "The file" ++ _ = format_error(Error1),
    ?line del(File, No),

    ?line make_file(Dir, First, 4),

    ?line {error, {not_a_log_file, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No}}]),
    ?line del(File, No),

    ?line make_file(Dir, First, 0),

    ?line {error, {not_a_log_file, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap}, 
		       {format, internal}, {size, {100, No}}]),
    % read only
    ?line make_file(Dir, First, 6),

    ?line {error, {not_a_log_file, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},{mode, read_only},
		       {format, internal}, {size, {100, No}}]),
    ?line del(File, No),

    ?line make_file(Dir, First, 0),

    ?line {error, {not_a_log_file, _}} = 
	disk_log:open([{name, n}, {file, File},{type, wrap}, 
		       {mode, read_only}, {format, internal},
		       {size, {100, No}}]),
    ?line del(File, No),
    
    ?line {error, _} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				      {mode, read_only},
				      {format, internal},{size, {100, No}}]),

    file:delete(File),
    ?line {ok,n} = disk_log:open([{name,n},{file,File},
                                  {mode,read_write},{type,halt}]),
    ?line ok = disk_log:close(n),
    ?line ok = unwritable(File),
    ?line {error, {file_error, File, _}} = 
       disk_log:open([{name,n},{file,File},{mode,read_write},{type,halt}]),
    ?line ok = writable(File),
    file:delete(File),

    ?line {ok,n} = disk_log:open([{name,n},{file,File},{format,external},
                                  {mode,read_write},{type,halt}]),
    ?line ok = disk_log:close(n),
    ?line ok = unwritable(File),
    ?line {error, {file_error, File, _}} = 
        disk_log:open([{name,n},{file,File},{format,external},
                       {mode,read_write},{type,halt}]),
    ?line ok = writable(File),
    file:delete(File),

    ok.


make_file(Dir, File, N) ->
    {ok, F} = file:open(filename:join(Dir, File), 
                        [raw, binary, read, write]),
    ok = file:truncate(F),
    case N of 
	0 ->
	    true;
	_Else ->
	    ok = file:write(F, [lists:seq(1,N)])
    end,
    ok = file:close(F).

open_size(suite) -> [];
open_size(doc) -> 
    ["Test open/1 option size."];
open_size(Conf) when is_list(Conf) ->

    ?line Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),

    ?line No = 4,
    ?line file:delete(File),
    ?line del(File, No),	% cleanup

    %% missing size option
    ?line {error, {badarg, size}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    ?line B = mk_bytes(60),    
    ?line ok = disk_log:log_terms(n, [B, B, B, B]),
    ?line ok = disk_log:sync(n),
    ?line ok = disk_log:block(n),

    %% size option does not match existing size file, read_only
    ?line Error1 = {error, {size_mismatch, _, _}} = 
	disk_log:open([{name, nn}, {file, File}, {type, wrap}, 
		       {mode, read_only}, {format, internal},
		       {size, {100, No + 1}}]),
    ?line "The given size" ++ _ = format_error(Error1),
    ?line {ok, nn} = disk_log:open([{name, nn}, {file, File}, {type, wrap},
			      {mode, read_only},
			      {format, internal},{size, {100, No}}]),
    ?line [_, _, _, _] = get_all_terms1(nn, start, []),
    ?line disk_log:close(nn),

    ?line ok = disk_log:unblock(n),
    ?line ok = disk_log:close(n),

    %% size option does not match existing size file, read_write
    ?line {error, {size_mismatch, _, _}} = 
	disk_log:open([{name, nn}, {file, File}, {type, wrap}, 
		       {format, internal}, {size, {100, No + 1}}]),
    %% size option does not match existing size file, truncating
    ?line {ok, nn} = 
	disk_log:open([{name, nn}, {file, File}, {type, wrap}, 
		       {repair, truncate}, {format, internal}, 
		       {size, {100, No + 1}}]),
    ?line ok = disk_log:close(nn),

    ?line del(File, No),
    ok.


open_truncate(suite) -> [];
open_truncate(doc) -> 
    ["Test open/1 with {repair, truncate}."];
open_truncate(Conf) when is_list(Conf) ->

    ?line Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No),	% cleanup

    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal},{size, {100, No}}]),
    ?line B = mk_bytes(60),    
    ?line ok = disk_log:log_terms(n, [B, B, B, B]),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {repair,truncate},
				   {format, internal},{size, {100, No}}]),
    ?line ok = disk_log:close(n),
    ?line [] = get_all_terms(n, File, wrap),
    ?line del(File, No),
    ok.
    

open_error(suite) -> [];
open_error(doc) -> 
    ["Try some invalid open/1 options."];
open_error(Conf) when is_list(Conf) ->
    ?line Dir = ?privdir(Conf),

    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No),	% cleanup

    ?line {error, {badarg, name}} = disk_log:open([{file, File}]),
    ?line {error, {badarg, file}} = disk_log:open([{name,{foo,bar}}]),
    ?line {error, {badarg, [{foo,bar}]}} = disk_log:open([{foo,bar}]),

    %% external logs, read_only.
    ?line {error, {file_error, _, enoent}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {size, {100,No}},
		       {format, external}, {mode, read_only}]),
    ?line Error5 = {error, {file_error, _, enoent}} = 
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {size, 100},
		       {format, external}, {mode, read_only}]),
    ?line true = lists:prefix("\"" ++ File, format_error(Error5)),

    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    %% Already owner, ignored.
    ?line {ok, n} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, {100, No}}]),
    ?line Error2 = {error, {name_already_open, n}} =
	disk_log:open([{name, n}, {file, another_file}, {type, wrap},
		       {format, external}, {size, {100, No}}]),
    ?line "The disk log" ++ _ = format_error(Error2),
    ?line Error1 = {error, {arg_mismatch, notify, false, true}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, {100, No}}, {notify, true}]),
    ?line "The value" ++ _ = format_error(Error1),
    ?line Error3 = {error, {open_read_write, n}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, 
		       {mode, read_only},
		       {format, external}, {size, {100, No}}]),
    ?line "The disk log" ++ _ = format_error(Error3),
    ?line {error, {badarg, size}} = 
        disk_log:open([{name, n}, {file, File}, {type, halt},
                       {format, external}, {size, {100, No}}]),
    ?line {error, {arg_mismatch, type, wrap, halt}} = 
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external}]),
    ?line {error, {arg_mismatch, format, external, internal}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No}}]),
    ?line {error, {arg_mismatch, repair, true, false}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {repair, false}]),
    ?line {error, {size_mismatch, {100,4}, {1000,4}}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, {1000, No}}]),
    ?line {error, {arg_mismatch, head, none, _}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {head, "header"},
		       {format, external}, {size, {100, No}}]),
    ?line {error, {badarg, size}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, 100}]),

    ?line ok = disk_log:close(n),

    ?line {ok, n} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {mode, read_only},
		       {format, external}, {size, {100, No}}]),
    ?line Error4 = {error, {open_read_only, n}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {mode, read_write},
		       {format, external}, {size, {100, No}}]),
    ?line "The disk log" ++ _ = format_error(Error4),
    ?line ok = disk_log:close(n),

    ?line del(File, No).    


close_race(suite) -> [];
close_race(doc) -> 
    ["Do something quickly after close/1"];
close_race(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 1,
    ?line del(File, No),  % cleanup
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true}, 
				   {format, internal}]),
    ?line ok = disk_log:close(n),
    ?line Error1 = {error, no_such_log} = disk_log:close(n),
    ?line "There is no disk" ++ _ = format_error(Error1),

    % Pid1 blocks, Pid2 closes without being suspended.
    ?line Pid1 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid2 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid1, {open, File}),
    ?line {ok, n} = sync_do(Pid2, {open, File}),
    ?line ok = sync_do(Pid1, block),
    ?line [{_, false}, {_, false}] = sync_do(Pid1, owners),
    ?line ok = sync_do(Pid2, close),
    ?line [{_, false}] = sync_do(Pid1, owners),
    ?line ok = sync_do(Pid1, close),
    ?line sync_do(Pid1, terminate),
    ?line sync_do(Pid2, terminate),
    ?line {error, no_such_log} = disk_log:info(n),

    % Pid3 blocks, Pid3 closes. Pid4 should still be ablo to use log.
    ?line Pid3 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid4 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid3, {open, File}),
    ?line {ok, n} = sync_do(Pid4, {open, File}),
    ?line ok = sync_do(Pid3, block),
    ?line ok = sync_do(Pid3, close),
    ?line [{_Pid4, false}] = sync_do(Pid4, owners),
    ?line sync_do(Pid3, terminate),
    ?line sync_do(Pid4, terminate),
    ?line {error, no_such_log} = disk_log:info(n),

    % Pid5 blocks, Pid5 terminates. Pid6 should still be ablo to use log.
    ?line Pid5 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid6 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid5, {open, File}),
    ?line {ok, n} = sync_do(Pid6, {open, File}),
    ?line ok = sync_do(Pid5, block),
    ?line sync_do(Pid5, terminate),
    ?line [{_Pid6, false}] = sync_do(Pid6, owners),
    ?line sync_do(Pid6, terminate),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line del(File, No),  % cleanup
    ok.

close_block(suite) -> [];
close_block(doc) -> 
    ["Block, unblock, close, terminate."];
close_block(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    No = 1,
    del(File, No),	% cleanup

    P0 = pps(),
    %% One of two owners terminates.
    ?line Pid1 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid2 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid1, {open, File}),
    ?line {ok, n} = sync_do(Pid2, {open, File}),
    ?line [_, _] = sync_do(Pid1, owners),
    ?line [_, _] = sync_do(Pid2, owners),
    ?line 0 = sync_do(Pid1, users),
    ?line 0 = sync_do(Pid2, users),
    ?line sync_do(Pid1, terminate),
    ?line [_] = sync_do(Pid2, owners),
    ?line 0 = sync_do(Pid2, users),
    ?line sync_do(Pid2, terminate),    
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    %% Users terminate (no link...).
    ?line Pid3 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid4 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid3, {open, File, none}),
    ?line {ok, n} = sync_do(Pid4, {open, File, none}),
    ?line [] = sync_do(Pid3, owners),
    ?line [] = sync_do(Pid4, owners),
    ?line 2 = sync_do(Pid3, users),
    ?line 2 = sync_do(Pid4, users),
    ?line sync_do(Pid3, terminate),
    ?line [] = sync_do(Pid4, owners),
    ?line 2 = sync_do(Pid4, users),
    ?line sync_do(Pid4, terminate),    
    ?line disk_log:close(n),
    ?line disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    % Blocking owner terminates.
    ?line Pid5 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {linkto, none},{size, {100,No}},
				   {format, external}]),
    ?line {ok, n} = sync_do(Pid5, {open, File}),
    ?line ok = sync_do(Pid5, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line sync_do(Pid5, terminate),
    ?line ok = status(n),
    ?line [] = owners(n),
    ?line 1 = users(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    % Blocking user terminates.
    ?line Pid6 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {format, external}]),
    ?line {ok, n} = sync_do(Pid6, {open, File, none}),
    ?line ok = sync_do(Pid6, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line sync_do(Pid6, terminate), % very silently...
    ?line ok = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line ok = disk_log:close(n),
    ?line [] = owners(n),
    ?line 1 = users(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    % Blocking owner terminates.
    ?line Pid7 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {linkto, none},
				   {size, {100,No}}, {format, external}]),
    ?line {ok, n} = sync_do(Pid7, {open, File}),
    ?line ok = sync_do(Pid7, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line sync_do(Pid7, terminate),
    ?line ok = status(n),
    ?line [] = owners(n),
    ?line 1 = users(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    %% Two owners, the blocking one terminates.
    ?line Pid8 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid9 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = sync_do(Pid8, {open, File}),
    ?line {ok, n} = sync_do(Pid9, {open, File}),
    ?line ok = sync_do(Pid8, block),
    ?line {blocked, true} = status(n),
    ?line sync_do(Pid8, terminate),
    ?line ok = status(n),
    ?line [_] = sync_do(Pid9, owners),
    ?line 0 = sync_do(Pid9, users),
    ?line sync_do(Pid9, terminate),    
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    % Blocking user closes.
    ?line Pid10 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {format, external}]),
    ?line {ok, n} = sync_do(Pid10, {open, File, none}),
    ?line ok = sync_do(Pid10, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line ok = sync_do(Pid10, close),
    ?line ok = status(n),
    ?line [_] = owners(n),
    ?line 0 = users(n),
    ?line ok = disk_log:close(n),
    ?line sync_do(Pid10, terminate),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line true = (P0 == pps()),    

    % Blocking user unblocks and closes.
    ?line Pid11 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {format, external}]),
    ?line {ok, n} = sync_do(Pid11, {open, File, none}),
    ?line ok = sync_do(Pid11, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line ok = sync_do(Pid11, unblock),
    ?line ok = sync_do(Pid11, close),
    ?line ok = status(n),
    ?line [_] = owners(n),
    ?line 0 = users(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line sync_do(Pid11, terminate),
    ?line true = (P0 == pps()),    

    % Blocking owner closes.
    ?line Pid12 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {linkto, none},
				   {size, {100,No}}, {format, external}]),
    ?line {ok, n} = sync_do(Pid12, {open, File}),
    ?line ok = sync_do(Pid12, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line ok = sync_do(Pid12, close),
    ?line ok = status(n),
    ?line [] = owners(n),
    ?line 1 = users(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line sync_do(Pid12, terminate),
    ?line true = (P0 == pps()),    

    % Blocking owner unblocks and closes.
    ?line Pid13 = spawn_link(?MODULE, lserv, [n]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {linkto, none},
				   {size, {100,No}}, {format, external}]),
    ?line {ok, n} = sync_do(Pid13, {open, File}),
    ?line ok = sync_do(Pid13, block),
    ?line {blocked, true} = status(n),
    ?line [_] = owners(n),
    ?line 1 = users(n),
    ?line ok = sync_do(Pid13, unblock),
    ?line ok = sync_do(Pid13, close),
    ?line ok = status(n),
    ?line [] = owners(n),
    ?line 1 = users(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line sync_do(Pid13, terminate),
    ?line true = (P0 == pps()),

    del(File, No),	% cleanup
    ok.

close_deadlock(suite) -> [];
close_deadlock(doc) -> 
    ["OTP-4745. Deadlock with just an ordinary log could happen."];
close_deadlock(Conf) when is_list(Conf) ->
    ?line true = is_alive(),

    ?line PrivDir = ?privdir(Conf),

    ?line F1 = filename:join(PrivDir, "a.LOG"),
    ?line file:delete(F1),
    Self = self(),

    %% One process opens the log at the same time as another process
    %% closes the log. Used to always cause deadlock before OTP-4745.
    Name = a,
    Fun = fun() -> open_close(Self, Name, F1) end,
    P = spawn(Fun),
    ?line receive {P, Name} -> ok end,
    ?line {ok, L} = disk_log:open([{name,Name},{file,F1}]),
    ?line ok = disk_log:close(L),
    ?line receive {P, done} -> ok end,    
    ?line file:delete(F1),

    %% One process opens the log at the same time as another process
    %% closes the log due to file error while truncating.
    %% This test is time dependent, but does not fail when it does not
    %% "work". When it works, as it seems to do right now :), the
    %% disk_log_server gets {error, no_such_log}, receives the EXIT
    %% message caused by truncate, and tries to open the log again.
    ?line No = 4,
    ?line LDir = F1 ++ ".2",
    ?line file:del_dir(LDir),
    ?line del(F1, No),
    ?line ok = file:make_dir(LDir),
    Fun2 = fun() -> open_truncate(Self, Name, F1, No) end,
    P2 = spawn(Fun2),
    ?line receive {P2, Name} -> ok end,
    ?line {ok, L} = disk_log:open([{name, Name}, {file, F1}, {type, wrap},
				   {format, external}]),
    %% Note: truncate causes the disk log process to terminate. One
    %% cannot say if open above happened before, after, or during the
    %% termination. The link to the owner is removed before termination.
    ?line case disk_log:close(L) of
              ok -> ok;
              {error,no_such_log} -> 
                  ok
          end,
    ?line receive {P2, done} -> ok end,
    ?line del(F1, No),
    ?line file:del_dir(LDir),

    %% To the same thing, this time using distributed logs.
    %% (Does not seem to work very well, unfortunately.)
    FunD = fun() -> open_close_dist(Self, Name, F1) end,
    PD = spawn(FunD),
    receive {PD, Name} -> ok end,
    ?line {[_], []} = disk_log:open([{name,Name},{file,F1},
                                     {distributed,[node()]}]),
    ?line ok = disk_log:close(L),
    receive {PD, done} -> ok end,
    ?line file:delete(F1),

    ok.

open_close(Pid, Name, File) ->
    {ok, L} = disk_log:open([{name,Name},{file,File}]),
    Pid ! {self(), Name},
    ok = disk_log:close(L),
    Pid ! {self(), done}.

open_truncate(Pid, Name, File, No) ->
    {ok, L} = disk_log:open([{name, Name}, {file, File}, {type, wrap},
                             {format, external},{size, {100, No}}]),
    Pid ! {self(), Name},
    {error, {file_error, _, _}} = disk_log:truncate(L),
    %% The file has been closed, the disklog process has terminated.
    Pid ! {self(), done}.

open_close_dist(Pid, Name, File) ->
    {[{_,{ok,L}}], []} = disk_log:open([{name,Name},{file,File},
                                        {distributed,[node()]}]),
    Pid ! {self(), Name},
    ok = disk_log:close(L),
    Pid ! {self(), done}.

async_do(Pid, Req) ->
    Pid ! {self(), Req},
    %% make sure the request is queued
    timer:sleep(100).

get_reply() ->
    receive Reply -> 
	    Reply 
    end.

sync_do(Pid, Req) ->
    Pid ! {self(), Req},
    receive
        Reply when Req =:= terminate ->
            timer:sleep(500),
            Reply;
	Reply ->
	    Reply
    end.

lserv(Log) ->
    ?line receive 
	{From, {open, File}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, {100,1}}, {format, external}]);
	{From, {open, File, LinkTo}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {linkto, LinkTo}, {size, {100,1}}, 
				  {format, external}]);
	{From, {int_open, File}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, {100,1}}]);
	{From, {int_open, File, Size}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, Size}]);
	{From, {dist_open, File, Node}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, {100,1}}, {distributed, [Node]}]);
	{From, {dist_open, File, LinkTo, Node}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {linkto, LinkTo}, {size, {100,1}}, 
				  {distributed, [Node]}]);
	{From, block} ->
	    From ! disk_log:block(Log);
	{From, {block, Bool}} ->
	    From ! disk_log:block(Log, Bool);
	{From, unblock} ->
	    From ! disk_log:unblock(Log);
	{From, close} ->
	    From ! disk_log:close(Log);
	{From, owners} ->
	    From ! owners(Log);
	{From, users} ->
	    From ! users(Log);
	{From, sync} ->
	    From ! disk_log:sync(Log);
	{From, truncate} ->
	    From ! disk_log:truncate(Log);
	{From, terminate} ->
	    From ! terminated,
	    exit(normal);
	{From, {log, B}} ->
	    From ! disk_log:log(Log, B);
	{From, {blog, B}} ->
	    From ! disk_log:blog(Log, B);
	{From, {alog, B}} ->
	    From ! disk_log:alog(Log, B);
	{From, {balog, B}} ->
	    From ! disk_log:balog(Log, B);
	{From, {change_notify, Pid, Bool}} ->
	    From ! disk_log:change_notify(Log, Pid, Bool);
	{From, {change_header, Header}} ->
	    From ! disk_log:change_header(Log, Header);
	{From, {change_size, Size}} ->
	    From ! disk_log:change_size(Log, Size);
	{From, inc_wrap_file} ->
	    From ! disk_log:inc_wrap_file(Log);
	{From, {chunk, Cont}} ->
	    From ! disk_log:chunk(Log, Cont);
	{From, {chunk_step, Cont, N}} ->
	    From ! disk_log:chunk_step(Log, Cont, N);
	Any ->
	    io:format("invalid request ~p~n", [Any]),
	    exit(abnormal)
    end,
    lserv(Log).


error_repair(suite) -> [];
error_repair(doc) -> 
    ["Error while repairing."];
error_repair(Conf) when is_list(Conf) ->
    % not all error situations are covered by this test

    DataDir = ?datadir(Conf),
    PrivDir = ?privdir(Conf),

    ?line File = filename:join(PrivDir, "n.LOG"),
    ?line No = 4,
    ?line file:delete(File),
    ?line del(File, No),	% cleanup

    % kurt.LOG is not closed and has four logged items, one is recovered
    ?line copy_wrap_log("kurt.LOG", "n.LOG", No, DataDir, PrivDir),
    ?line {repaired,n,{recovered,1},{badbytes,0}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, {size,{40,No}}]),
    ?line 1 = cur_cnt(n),
    ?line 53 = curb(n),
    ?line 4 = no_items(n),
    ?line ok = disk_log:close(n),
    
    % temporary repair file cannot be created
    ?line copy_wrap_log("kurt.LOG", "n.LOG", No, DataDir, PrivDir),
    ?line Dir = File ++ ".4" ++ ".TMP",
    ?line ok = file:make_dir(Dir),
    ?line P0 = pps(),
    ?line {error, {file_error, _, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, {size,{40,4}}]),
    ?line true = (P0 == pps()),
    ?line del(File, No),
    ?line ok = file:del_dir(Dir),

    %% repair a file
    ?line P1 = pps(),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {40,No}}]),
    ?line ok = disk_log:log_terms(n, [{this,is}]), % first file full
    ?line ok = disk_log:log_terms(n, [{some,terms}]), % second file full
    ?line ok = disk_log:close(n),
    ?line BadFile = add_ext(File, 2), % current file
    ?line set_opened(BadFile),
    ?line crash(BadFile, 28), % the binary is now invalid
    ?line {repaired,n,{recovered,0},{badbytes,26}} =
	 disk_log:open([{name, n}, {file, File}, {type, wrap},
			{format, internal}, {size, {40,No}}]),
    ?line ok = disk_log:close(n),
    ?line true = (P1 == pps()),
    ?line del(File, No),

    %% yet another repair
    ?line P2 = pps(),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {4000,No}}]),
    ?line ok = disk_log:log_terms(n, [{this,is},{some,terms}]),
    ?line ok = disk_log:close(n),
    ?line BadFile2 = add_ext(File, 1), % current file
    ?line set_opened(BadFile2),
    ?line crash(BadFile2, 51), % the second binary is now invalid
    ?line {repaired,n,{recovered,1},{badbytes,26}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {4000,No}}]),
    ?line ok = disk_log:close(n),
    ?line true = (P2 == pps()),
    ?line del(File, No),

    %% Repair, large term
    ?line Big = term_to_binary(lists:duplicate(66000,$a)),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {40,No}}]),
    ?line ok = disk_log:log_terms(n, [Big]),
    ?line ok = disk_log:close(n),
    ?line set_opened(add_ext(File, 1)),
    ?line {repaired,n,{recovered,1},{badbytes,0}} = 
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {40,No}}]),
    ?line {_, [Got]} = disk_log:chunk(n, start),
    ?line ok = disk_log:close(n),
    ?line Got = Big,
    ?line del(File, No),

    %% A term a little smaller than a chunk, then big terms.
    ?line BigSmall = mk_bytes(1024*64-8-12),
    ?line file:delete(File),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line ok = disk_log:log_terms(n, [BigSmall, Big, Big]),
    ?line ok = disk_log:close(n),    
    ?line set_opened(File),
    ?line FileSize = file_size(File),
    ?line crash(File, FileSize-byte_size(Big)-4),
    ?line Error1 = {error, {need_repair, _}} = 
        disk_log:open([{name, n}, {file, File}, {repair, false},
                       {type, halt}, {format, internal}]),
    ?line "The disk log" ++ _ = format_error(Error1),
    ?line {repaired,n,{recovered,2},{badbytes,132013}} = 
        disk_log:open([{name, n}, {file, File}, {repair, true},
                       {type, halt}, {format, internal}]),
    ?line ok = disk_log:close(n),
    ?line file:delete(File),

    %% The header is recovered.
    ?line {ok,n} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, internal},
		       {head_func, {?MODULE, head_fun, [{ok,"head"}]}}]),
    ?line ok = disk_log:log_terms(n, [list,'of',terms]),
    ?line ["head",list,'of',terms] = get_all_terms(n),
    ?line ok = disk_log:close(n),
    ?line set_opened(File),
    ?line crash(File, 30),
    ?line {repaired,n,{recovered,3},{badbytes,16}} = 
        disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, internal},{repair,true},
		       {head_func, {?MODULE, head_fun, [{ok,"head"}]}}]),
    ?line ["head",'of',terms] = get_all_terms(n),
    ?line ok = disk_log:close(n),

    file:delete(File),

    ok.
    
set_opened(File) ->
    {ok, Fd} = file:open(File, [raw, binary, read, write]),
    ok = file:write(Fd, [?LOGMAGIC, ?OPENED]),
    ok = file:close(Fd).

error_log(suite) -> [];
error_log(doc) -> 
    ["Error while repairing."];
error_log(Conf) when is_list(Conf) ->
    ?line Dir = ?privdir(Conf),

    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line file:delete(File),
    ?line del(File, No),	% cleanup
    ?line LDir = File ++ ".2",

    ?line Q = qlen(),
    % dummy just to get all processes "above" disk_log going
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external},{size, {100, No}}]),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    % inc_wrap_file fails, the external log is not terminated
    ?line P0 = pps(),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external},{size, {100, No}}]),
    ?line ok = file:make_dir(LDir),
    ?line {error, {file_error, _, _}} = disk_log:inc_wrap_file(n),
    ?line timer:sleep(500),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    % inc_wrap_file fails, the internal log is not terminated, ./File.2/ exists
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal},{size, {100, No}}]),
    ?line {error, {file_error, _, _}} = disk_log:inc_wrap_file(n),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    % truncate fails, the log is terminated, ./File.2/ exists
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external},{size, {100, No}}]),
    ?line {error, {file_error, _, _}} = disk_log:truncate(n),
    ?line true = (P0 == pps()),
    ?line del(File, No),

    %% OTP-4880.
    % reopen (rename) fails, the log is terminated, ./File.2/ exists
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, external},{size, 100000}]),
    ?line {error, {file_error, _, eisdir}} = disk_log:reopen(n, LDir),
    ?line true = (P0 == pps()),
    ?line file:delete(File),

    ?line B = mk_bytes(60),

    %% OTP-4880. reopen a wrap log, rename fails
    ?line File2 = filename:join(Dir, "n.LOG2"),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File2}, {type, wrap},
				   {format, external},{size, {100, No}}]),
    ?line ok = disk_log:blog_terms(n, [B,B,B]),
    ?line {error, {file_error, _, eisdir}} = disk_log:reopen(n, File),
    ?line {error, no_such_log} = disk_log:close(n),
    ?line del(File2, No),
    ?line del(File, No),

    % log, external wrap log, ./File.2/ exists
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external},{size, {100, No}}]),
    ?line {error, {file_error, _, _}} = disk_log:blog_terms(n, [B,B,B]),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    % log, internal wrap log, ./File.2/ exists
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal},{size, {100, No}}]),
    ?line {error, {file_error, _, _}} = disk_log:log_terms(n, [B,B,B]),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    ?line ok = file:del_dir(LDir),

    % can't remove file when changing size
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal},{size, {100, No}}]),
    ?line ok = disk_log:log_terms(n, [B,B,B,B]),
    ?line ok = disk_log:change_size(n, {100, No-2}),
    ?line Three = File ++ ".3",
    ?line ok = file:delete(Three),
    ?line ok = file:make_dir(Three),
    ?line {error, {file_error, _, _}} = disk_log:log_terms(n, [B,B,B]),
    ?line timer:sleep(500),
    ?line ok = disk_log:close(n),
    ?line ok = file:del_dir(Three),
    ?line del(File, No),
    ?line Q = qlen(),
    ok.
    
chunk(suite) -> [];
chunk(doc) -> 
    ["Test chunk and chunk_step."];
chunk(Conf) when is_list(Conf) ->
    %% See also halt_ro_crash/1 above.

    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    No = 4,
    ?line B = mk_bytes(60),
    ?line BB = mk_bytes(64000), % 64 kB chunks
    ?line del(File, No),% cleanup

    %% Make sure chunk_step skips the rest of the binary.
    %% OTP-3716. This was a bug...
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {50,No}}]),
    %% 1, 2 and 3 on file one, 4 on file two.
    ?line ok = disk_log:log_terms(n, [1,2,3,4]),
    ?line {I1, [1]} = disk_log:chunk(n, start, 1),
    ?line [{node,Node}] = disk_log:chunk_info(I1),
    ?line Node = node(),
    ?line Error1 = {error, {no_continuation, foobar}} = 
        disk_log:chunk_info(foobar),
    ?line "The term" ++ _ = format_error(Error1),
    ?line {ok, I2} = disk_log:chunk_step(n, I1, 1),
    ?line {error, {badarg, continuation}} = disk_log:chunk_step(n, foobar, 1),
    ?line {I3, [4]} = disk_log:chunk(n, I2, 1),
    ?line {ok, I4} = disk_log:chunk_step(n, I3, -1),
    ?line {_, [1]} = disk_log:chunk(n, I4, 1),
    ?line {error, {badarg, continuation}} = disk_log:bchunk(n, 'begin'),
    ?line {Ib1, [Bin1,Bin2]} = disk_log:bchunk(n, start, 2),
    ?line 1 = binary_to_term(Bin1),
    ?line 2 = binary_to_term(Bin2),
    ?line {ok, Ib2} = disk_log:chunk_step(n, Ib1, 1),
    ?line {Ib3, [Bin3]} = disk_log:bchunk(n, Ib2, 1),
    ?line 4 = binary_to_term(Bin3),
    ?line {ok, Ib4} = disk_log:chunk_step(n, Ib3, -1),
    ?line {_, [Bin4]} = disk_log:bchunk(n, Ib4, 1),
    ?line 1 = binary_to_term(Bin4),
    ?line {Ib5, [Bin1, Bin2, Bin17]} = disk_log:bchunk(n, start),
    ?line 3 = binary_to_term(Bin17),
    ?line {Ib6, [Bin3]} = disk_log:bchunk(n, Ib5, infinity),
    ?line eof = disk_log:bchunk(n, Ib6, infinity),
    ?line ok = disk_log:close(n),
    ?line del(File, No), % cleanup

    %% external log, cannot read chunks
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external}, {size, {100,No}}]),
    ?line {error, {badarg, continuation}} = disk_log:chunk(n, 'begin'),
    ?line {error, {format_external, n}} = disk_log:chunk(n, start),
    ?line Error2 = {error, {not_internal_wrap, n}} = 
        disk_log:chunk_step(n, start, 1),
    ?line "The requested" ++ _ = format_error(Error2),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% wrap, read_write
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {100,No}}]),
    ?line ok = disk_log:log_terms(n, [B,B,B,B]),
    ?line {C1, [_]} = disk_log:chunk(n, start),
    ?line {C2, [_]} = disk_log:chunk(n, C1),
    ?line {C3, [_]} = disk_log:chunk(n, C2),
    ?line {C4, [_]} = disk_log:chunk(n, C3, 1),
    ?line eof = disk_log:chunk(n, C4),
    ?line {C5, [_]} = disk_log:chunk(n, start),
    ?line {ok, C6} = disk_log:chunk_step(n, C5, 1),
    ?line {C7, [_]} = disk_log:chunk(n, C6),
    ?line {ok, C8} = disk_log:chunk_step(n, C7, 1),
    ?line {_, [_]} = disk_log:chunk(n, C8),
    ?line ok = disk_log:close(n),

    %% wrap, read_only
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {mode, read_only},
				   {format, internal}, {size, {100,No}}]),
    ?line {CC1, [_]} = disk_log:chunk(n, start),
    ?line {CC2, [_]} = disk_log:chunk(n, CC1),
    ?line {CC3, [_]} = disk_log:chunk(n, CC2),
    ?line {CC4, [_]} = disk_log:chunk(n, CC3, 1),
    ?line eof = disk_log:chunk(n, CC4),
    ?line {CC5, [_]} = disk_log:chunk(n, start),
    ?line {ok, CC6} = disk_log:chunk_step(n, CC5, 1),
    ?line {CC7, [_]} = disk_log:chunk(n, CC6),
    ?line {ok, CC8} = disk_log:chunk_step(n, CC7, 1),
    ?line {_, [_]} = disk_log:chunk(n, CC8),
    ?line ok = disk_log:close(n),

    %% OTP-3716. A bug: {Error, List} and {Error, List, Bad} could be
    %% returned from chunk/2.
    %% Magic bytes not OK.
    %% File header (8 bytes) OK, item header not OK.
    ?line InvalidFile = add_ext(File, 1),
    ?line crash(InvalidFile, 15),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {mode, read_only},
				   {format, internal}, {size, {100,No}}]),
    ?line {_, [], 61} = disk_log:chunk(n, start),
    ?line ok = disk_log:close(n),
    %% read_write...
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {100,No}}]),
    ?line Error3 = {error, {corrupt_log_file, Culprit}} = 
        disk_log:chunk(n, start),
    ?line "The disk log file" ++ _ = format_error(Error3),
    ?line Culprit = InvalidFile,
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% Two wrap log files, writing the second one, then reading the first
    %% one, where a bogus term resides.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {40,No}}]),
    ?line ok = disk_log:log_terms(n, [{this,is}]), % first file full
    ?line ok = disk_log:log_terms(n, [{some,terms}]), % second file full
    ?line 2 = curf(n),
    ?line BadFile = add_ext(File, 1),
    ?line crash(BadFile, 28), % the _binary_ is now invalid
    ?line {error, {corrupt_log_file, BFile}} = disk_log:chunk(n, start, 1),
    ?line BadFile = BFile,
    ?line ok = disk_log:close(n),
    %% The same, with a halt log.
    ?line file:delete(File), % cleanup
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line ok = disk_log:log_terms(n, [{this,is}]),
    ?line ok = disk_log:sync(n),
    ?line crash(File, 28), % the _binary_ is now invalid
    ?line {error, {corrupt_log_file, File2}} = disk_log:chunk(n, start, 1),
    ?line crash(File, 10),
    ?line {error,{corrupt_log_file,_}} = disk_log:bchunk(n, start, 1),
    ?line true = File == File2,
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% halt, read_write
    ?line file:delete(File), % cleanup
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line ok = disk_log:log_terms(n, [BB,BB,BB,BB]),
    ?line {D1, [Ch1]} = disk_log:chunk(n, start, 1),
    ?line Ch1 = BB,
    ?line {D2, [Ch2]} = disk_log:chunk(n, D1, 1),
    ?line Ch2 = BB,
    ?line {D3, [Ch3]} = disk_log:chunk(n, D2, 1),
    ?line Ch3 = BB,
    ?line {D4, [Ch4]} = disk_log:chunk(n, D3, 1),
    ?line Ch4 = BB,
    ?line eof = disk_log:chunk(n, D4),
    ?line ok = disk_log:close(n),

    %% halt, read_only
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal},{mode,read_only}]),
    ?line {E1, [Ch5]} = disk_log:chunk(n, start, 1),
    ?line Ch5 = BB,
    ?line {E2, [Ch6]} = disk_log:chunk(n, E1, 1),
    ?line Ch6 = BB,
    ?line {E3, [Ch7]} = disk_log:chunk(n, E2, 1),
    ?line Ch7 = BB,
    ?line {E4, [Ch8]} = disk_log:chunk(n, E3, 1),
    ?line Ch8 = BB,
    ?line eof = disk_log:chunk(n, E4),
    ?line ok = disk_log:close(n),
    ?line file:delete(File), % cleanup

    %% More than 64 kB term.
    ?line BBB = term_to_binary(lists:duplicate(66000,$a)),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line ok = disk_log:log_terms(n, [BBB]),
    ?line {F1, [BBB1]} = disk_log:chunk(n, start),
    ?line BBB1 = BBB,
    ?line eof = disk_log:chunk(n, F1),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_only}]),
    ?line {F1r, [BBB2]} = disk_log:chunk(n, start),
    ?line BBB2 = BBB,
    ?line eof = disk_log:chunk(n, F1r),
    ?line ok = disk_log:close(n),

    ?line truncate(File, 8192),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line {error, {corrupt_log_file, _}} = disk_log:chunk(n, start),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_only}]),
    ?line {K1, [], 8176} = disk_log:chunk(n, start),
    ?line eof = disk_log:chunk(n, K1),
    ?line ok = disk_log:close(n),
    ?line file:delete(File), % cleanup

    %% OTP-3716. A bug: eof in the middle of the last element is not ok.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line ok = disk_log:log_terms(n, [B,BB]),
    ?line ok = disk_log:close(n),
    ?line truncate(File, 80),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line {G1, [_]} = disk_log:chunk(n, start, 1),
    ?line {error, {corrupt_log_file, _}} = disk_log:chunk(n, G1, 1),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_only}]),
    ?line {G1r, [_]} = disk_log:chunk(n, start, 1),
    ?line {_, [], 4} = disk_log:chunk(n, G1r, 1),
    ?line ok = disk_log:close(n),
    ?line file:delete(File), % cleanup

    %% Opening a wrap log read-only. The second of four terms is destroyed.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {size, {4000,No}}]),
    ?line ok = disk_log:log_terms(n, 
				  [{this,is},{some,terms},{on,a},{wrap,file}]),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, internal}, {mode, read_only}]),
    ?line CrashFile = add_ext(File, 1),
    ?line crash(CrashFile, 51), % the binary term {some,terms} is now bad
    ?line {H1, [{this,is}], 18} = disk_log:chunk(n, start, 10),
    ?line {H2, [{on,a},{wrap,file}]} = disk_log:chunk(n, H1),
    ?line eof = disk_log:chunk(n, H2),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% The same as last, but with a halt log.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_write}]),
    ?line ok = disk_log:alog_terms(n, [{this,is},{some,terms}]),
    ?line ok = disk_log:log_terms(n, [{on,a},{halt,file}]),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_only}]),
    ?line crash(File, 51), % the binary term {some,terms} is now bad
    ?line {J1, [{this,is}], 18} = disk_log:chunk(n, start, 10),
    ?line {J2, [{on,a},{halt,file}]} = disk_log:chunk(n, J1),
    ?line eof = disk_log:chunk(n, J2),
    ?line ok = disk_log:close(n),
    ?line file:delete(File),

    %% OTP-7641. Same as last one, but the size of the bad term is
    %% less than ?HEADERSz (8) bytes.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_write}]),
    ?line ok = disk_log:alog_terms(n, [{this,is},{s}]),
    ?line ok = disk_log:log_terms(n, [{on,a},{halt,file}]),
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}, {mode, read_only}]),
    ?line crash(File, 44), % the binary term {s} is now bad
    ?line {J11, [{this,is}], 7} = disk_log:chunk(n, start, 10),
    ?line {J21, [{on,a},{halt,file}]} = disk_log:chunk(n, J11),
    ?line eof = disk_log:chunk(n, J21),
    ?line ok = disk_log:close(n),
    ?line file:delete(File),

    %% Minimal MD5-proctected term, and maximal unprotected term.
    %% A chunk ends in the middle of the MD5-sum.
    ?line MD5term = mk_bytes(64*1024-8),
    ?line NotMD5term = mk_bytes((64*1024-8)-1),
    ?line Term2 = mk_bytes((64*1024-8)-16),
    ?line MD5L = [MD5term,NotMD5term,Term2,MD5term,MD5term,NotMD5term],
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line ok = disk_log:log_terms(n, MD5L),
    ?line true = MD5L == get_all_terms(n),
    ?line ok = disk_log:close(n),
    ?line true = MD5L == get_all_terms(n, File, halt),
    ?line crash(File, 21), % the MD5-sum of the first term is now bad
    ?line true = {tl(MD5L),64*1024-8} == get_all_terms_and_bad(n, File, halt),
    ?line {_,64*1024-8} = get_all_binary_terms_and_bad(n, File, halt),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
				   {format, internal}]),
    ?line {error, {corrupt_log_file, _}} = disk_log:chunk(n, start),
    ?line ok = disk_log:close(n),
    ?line file:delete(File),

    %% A file with "old" terms (magic word is MAGICINT).
    DataDir = ?datadir(Conf),
    OldTermsFileOrig = filename:join(DataDir, "old_terms.LOG"),
    OldTermsFile = filename:join(Dir, "old_terms.LOG"),
    ?line copy_file(OldTermsFileOrig, OldTermsFile),
    ?line {[_,_,_,_],0} = get_all_terms_and_bad(n, OldTermsFile, halt),
    ?line {ok, n} = disk_log:open([{name, n}, {file, OldTermsFile}, 
                                   {type, halt}, {format, internal}]),
    ?line [_,_,_,_] = get_all_terms(n),
    ?line ok = disk_log:close(n),
    ?line file:delete(OldTermsFile),

    ok.

error_index(suite) -> [];
error_index(doc) -> 
    ["OTP-5558. Keep the contents of index files after disk crash."];
error_index(Conf) when is_list(Conf) ->
    ?line Dir = ?privdir(Conf),

    ?line File = filename:join(Dir, "n.LOG"),
    ?line IdxFile = File ++ ".idx",
    ?line No = 4,
    ?line file:delete(File),
    ?line del(File, No),	% cleanup

    Args = [{name,n},{type,wrap},{size,{100,No}},{file,File}],
    ?line {ok, n} = disk_log:open(Args),
    ?line ok = disk_log:close(n),
    ?line Q = qlen(),
    P0 = pps(),
    ?line ok = file:write_file(IdxFile, <<"abc">>),
    ?line {error, {invalid_index_file, _}} = disk_log:open(Args),
    ?line {error, {invalid_index_file, _}} = disk_log:open(Args),
    ?line {error, {invalid_index_file, _}} = disk_log:open(Args),

    ?line del(File, No),
    ?line true = (P0 == pps()),
    ?line true = (Q == qlen()),
    ok.
    
truncate(suite) -> [];
truncate(doc) -> 
    ["Test truncate/1 on halt and wrap logs."];
truncate(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),

    ?line Q = qlen(),
    Halt = join(Dir, "halt.LOG"),
    % Halt logs.

    ?line file:delete(Halt), % cleanup
    ?line {ok, halt} = disk_log:open([{name, halt}, {type, halt}, {file, Halt},
				      {head, header}, {notify, true}]),
    ?line infinity = sz(halt),
    ?line ok = disk_log:truncate(halt, tjohej),
    ?line rec(1, {disk_log, node(), halt, {truncated, 1}}),
    ?line ok = disk_log:change_size(halt, 10000),
    ?line 10000 = sz(halt),
    ?line disk_log:close(halt),
    ?line [tjohej] = get_all_terms(halt, Halt, halt),
    ?line file:delete(Halt),

    ?line {ok, halt} = disk_log:open([{name, halt}, {type, halt}, {file, Halt},
				      {head, header}, {notify, true}]),
    ?line ok = disk_log:truncate(halt),
    ?line rec(1, {disk_log, node(), halt, {truncated, 1}}),
    ?line disk_log:close(halt),
    ?line [header] = get_all_terms(halt, Halt, halt),
    ?line file:delete(Halt),
    
    ?line {ok, halt} = disk_log:open([{name, halt}, {type, halt}, 
                                      {file, Halt}, {format, external},
				      {head, "header"}, {notify, false}]),
    ?line ok = disk_log:btruncate(halt, "apa"),
    ?line disk_log:close(halt),
    ?line 3 = file_size(Halt),
    ?line file:delete(Halt),
    
    %% Wrap logs.
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line B = mk_bytes(60),
    ?line del(File, No),	% cleanup

    %% Internal with header.
    ?line Size = {100, No},
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {head, header}, {notify, true}, 
				   {size, Size}]),
    ?line ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:truncate(n, apa),
    ?line rec(1, {disk_log, node(), n, {truncated, 6}}),
    ?line {0, 0} = no_overflows(n),
    ?line 23 = curb(n),
    ?line 1 = curf(n),
    ?line 1 = cur_cnt(n),
    ?line true = (Size == sz(n)),

    ?line ok = disk_log:log_terms(n, [B, B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:close(n),
    ?line [apa, _, header, _] = get_all_terms(n, File, wrap),
    ?line del(File, No),
    
    %% Internal without general header.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, 
				   {size, {100, No}}]),
    ?line ok = disk_log:log_terms(n, [B,B,B]),
    ?line rec(2, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:truncate(n, apa),
    ?line rec(1, {disk_log, node(), n, {truncated, 3}}),
    ?line {0, 0} = no_overflows(n),
    ?line 23 = curb(n),
    ?line 1 = curf(n),
    ?line 1 = cur_cnt(n),
    ?line true = (Size == sz(n)),

    ?line ok = disk_log:log_terms(n, [B, B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:close(n),
    ?line [apa, _, _] = get_all_terms(n, File, wrap),
    ?line del(File, No),

    %% Internal without any header.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, 
				   {size, {100, No}}]),
    ?line ok = disk_log:log_terms(n, [B,B,B]),
    ?line rec(2, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:truncate(n),
    ?line rec(1, {disk_log, node(), n, {truncated, 3}}),
    ?line {0, 0} = no_overflows(n),
    ?line 8 = curb(n),
    ?line 1 = curf(n),
    ?line 0 = cur_cnt(n),
    ?line true = (Size == sz(n)),

    ?line ok = disk_log:log_terms(n, [B, B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:close(n),
    ?line [_, _] = get_all_terms(n, File, wrap),
    ?line del(File, No),
    ?line Q = qlen(),
    ok.


many_users(suite) -> [];
many_users(doc) -> 
    ["Test many users logging and sync:ing at the same time."];
many_users(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    N = 100,
    NoClients = 10,
    Fun1 = fun(Name, Pid, I) -> disk_log:log(Name, {Pid, I}) end,
    Fun2 = fun(Name, Pid, I) -> ok = disk_log:log(Name, {Pid, I}),
				disk_log:sync(Name) end,
    ?line {C1, T1} = many(Fun2, NoClients, N, halt, internal, infinity, Dir),
    ?line true = lists:duplicate(NoClients, ok) == C1,
    ?line true = length(T1) == N*NoClients,
    ?line {C2, T2} = many(Fun1, NoClients, N, halt, internal, 1000, Dir),
    ?line true = lists:duplicate(NoClients, {error, {full,"log.LOG"}}) == C2,
    ?line true = length(T2) > 0,
    ?line {C3, T3} = many(Fun2, NoClients, N, wrap, internal, 
			  {300*NoClients,200}, Dir),
    ?line true = lists:duplicate(NoClients, ok) == C3,
    ?line true = length(T3) == N*NoClients,
    ok.

many(Fun, NoClients, N, Type, Format, Size, Dir) ->
    Name = "log.LOG",
    File = filename:join(Dir, Name),
    del_files(Size, File),
    ?line Q = qlen(),
    ?line {ok, _} = disk_log:open([{name,Name}, {type,Type}, {size,Size}, 
				   {format,Format}, {file,File}]),
    ?line Pids = spawn_clients(NoClients, client, [self(), Name, N, Fun]),
    ?line Checked = check_clients(Pids),
    ?line ok = disk_log:close(Name),
    ?line Terms = get_all_terms(Name, File, Type),
    ?line del_files(Size, File),
    ?line Q = qlen(),
    ?line {Checked, Terms}.

spawn_clients(0, _F, _A) ->
    [];
spawn_clients(I, F, A) ->
    [spawn_link(?MODULE, F, A) | spawn_clients(I-1, F, A)].

check_clients(Pids) ->
    lists:map(fun(Pid) -> receive {Pid, Reply} -> Reply end end, Pids).

client(From, _Name, 0, _Fun) ->
    From ! {self(), ok};
client(From, Name, N, Fun) ->
    %% Fun is called N times.
    case Fun(Name, self(), N) of
	ok -> client(From, Name, N-1, Fun);
	Else -> From ! {self(), Else}
    end.

del_files({_NoBytes,NoFiles}, File) ->
    del(File, NoFiles);
del_files(_Size, File) ->
    file:delete(File).




info_current(suite) -> [];
info_current(doc) -> 
    ["Test no_current_{bytes, items} as returned by info/0."];
info_current(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    No = 4,
    B = mk_bytes(60),
    BB = mk_bytes(160), % bigger than a single wrap log file
    SB = mk_bytes(10),  % much smaller than a single wrap log file
    ?line del(File, No),% cleanup

    ?line Q = qlen(),
    %% Internal with header.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {head, header}, {size, {100,No}}]),
    ?line {26, 1} = {curb(n), cur_cnt(n)},
    ?line {1, 1}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log(n, B),
    ?line {94, 2} = {curb(n), cur_cnt(n)},
    ?line {2, 2}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, 
				   {head, header}, {size, {100,No}}]),
    ?line {94, 2} = {curb(n), cur_cnt(n)},
    ?line {0, 2}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {94, 2} = {curb(n), cur_cnt(n)},
    ?line {2, 4}  = {no_written_items(n), no_items(n)},
    ?line disk_log:inc_wrap_file(n),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {26, 1} = {curb(n), cur_cnt(n)},
    ?line {3, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line {94, 2} = {curb(n), cur_cnt(n)},
    ?line {8, 7}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line {94, 2} = {curb(n), cur_cnt(n)},
    ?line {12, 7}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 2}}),
    ?line {194, 2} = {curb(n), cur_cnt(n)},
    ?line {16, 7}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [SB,SB,SB]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line {80, 4} = {curb(n), cur_cnt(n)},
    ?line {20, 9}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% Internal without header.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    ?line {8, 0} = {curb(n), cur_cnt(n)},
    ?line {0, 0}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log(n, B),
    ?line {76, 1} = {curb(n), cur_cnt(n)},
    ?line {1, 1}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, {size, {100,No}}]),
    ?line {76, 1} = {curb(n), cur_cnt(n)},
    ?line {0, 1}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {76, 1} = {curb(n), cur_cnt(n)},
    ?line {1, 2}  = {no_written_items(n), no_items(n)},
    ?line disk_log:inc_wrap_file(n),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {8, 0} = {curb(n), cur_cnt(n)},
    ?line {1, 2}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line {76, 1} = {curb(n), cur_cnt(n)},
    ?line {4, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line ok = disk_log:log_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line {76, 1} = {curb(n), cur_cnt(n)},
    ?line {6, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 1}}),
    ?line {176, 1} = {curb(n), cur_cnt(n)},
    ?line {8, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:log_terms(n, [SB,SB,SB]),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line {62, 3} = {curb(n), cur_cnt(n)},
    ?line {11, 6}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% External with header.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external}, {head, "header"}, 
				   {size, {100,No}}]),
    ?line {6, 1} = {curb(n), cur_cnt(n)},
    ?line {1, 1}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog(n, B),
    ?line {62, 2} = {curb(n), cur_cnt(n)},
    ?line {2, 2}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external}, {head, "header"}, 
				   {notify, true}, {size, {100,No}}]),
    ?line {62, 2} = {curb(n), cur_cnt(n)},
    ?line {0, 2}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {62, 2} = {curb(n), cur_cnt(n)},
    ?line {2, 4}  = {no_written_items(n), no_items(n)},
    ?line disk_log:inc_wrap_file(n),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {6, 1} = {curb(n), cur_cnt(n)},
    ?line {3, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line {62, 2} = {curb(n), cur_cnt(n)},
    ?line {8, 7}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line ok = disk_log:blog_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line {62, 2} = {curb(n), cur_cnt(n)},
    ?line {12, 7}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 2}}),
    ?line {162, 2} = {curb(n), cur_cnt(n)},
    ?line {16, 7}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [SB,SB,SB]),

    ?line rec(1, {disk_log, node(), n, {wrap, 2}}),
    ?line {24, 4} = {curb(n), cur_cnt(n)},
    ?line {20, 9}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% External without header.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {format, external}, {size, {100,No}}]),
    ?line {0, 0} = {curb(n), cur_cnt(n)},
    ?line {0, 0}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog(n, B),
    ?line {56, 1} = {curb(n), cur_cnt(n)},
    ?line {1, 1}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {notify, true}, 
				   {format, external}, {size, {100,No}}]),
    ?line {56, 1} = {curb(n), cur_cnt(n)},
    ?line {0, 1}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {56, 1} = {curb(n), cur_cnt(n)},
    ?line {1, 2}  = {no_written_items(n), no_items(n)},
    ?line disk_log:inc_wrap_file(n),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line {0, 0} = {curb(n), cur_cnt(n)},
    ?line {1, 2}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line {56, 1} = {curb(n), cur_cnt(n)},
    ?line {4, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line ok = disk_log:blog_terms(n, [B]),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line {56, 1} = {curb(n), cur_cnt(n)},
    ?line {6, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 1}}),
    ?line {156, 1} = {curb(n), cur_cnt(n)},
    ?line {8, 4}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:blog_terms(n, [SB,SB,SB]),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line {18, 3} = {curb(n), cur_cnt(n)},
    ?line {11, 6}  = {no_written_items(n), no_items(n)},
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    ?line Q = qlen(),
    ok.



change_size_before(suite) -> [];
change_size_before(doc) -> 
    ["Change size of a wrap log file before we have reached "
     "to the file index corresponding to the new size"];
change_size_before(Conf) when is_list(Conf) ->

    Log_1_1 = "first log  first message",
    Log_1_2 = "first log  second message",
    Log_2_1 = "second log  first message",
    Log_2_2 = "second log  second message",
    Log_3_1 = "third log  first message",
    Log_3_2 = "third log  second message",
    Log_4_1 = "fourth log  first message",
    Log_4_2 = "fourth log  second message",
    Log_5_1 = "fifth log  first message",
    Log_5_2 = "fifth log  second message",
    Log_1_2_1 = "first log  second round 1",
    Log_1_2_2 = "first log  second round 2",


    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    del(File, 5),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, 
				   {type, wrap}, {size, {100,5}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:change_size(a, {100, 3}),
    ?line [Log_1_1, Log_1_2,
	   Log_2_1, Log_2_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_1_2_1),
    ?line disk_log:log(a, Log_1_2_2),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_1_2_1, Log_1_2_2] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
				   {size, {100,3}}]),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_1_2_1, Log_1_2_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
				   {size, {60,5}}, {format, external}]),
    ?line disk_log:blog(a, Log_1_1),
    ?line disk_log:blog(a, Log_1_2),
    ?line disk_log:blog(a, Log_2_1),
    ?line disk_log:blog(a, Log_2_2),
    ?line disk_log:change_size(a, {60, 3}),
    ?line ok = disk_log:sync(a),
    ?line {ok, Fd1} = file:open(File ++ ".1", [read]),
    ?line Log11_12 = Log_1_1 ++ Log_1_2,
    ?line {ok,Log11_12} = file:read(Fd1, 200),
    ?line ok = file:close(Fd1),
    ?line {ok, Fd2} = file:open(File ++ ".2", [read]),
%    ?t:format(0, "~p~n",[file:read(Fd2, 200)]),
    ?line Log21_22 = Log_2_1 ++ Log_2_2,
    ?line {ok,Log21_22} = file:read(Fd2, 200),
    ?line ok = file:close(Fd2),
    ?line disk_log:blog(a, Log_3_1),
    ?line disk_log:blog(a, Log_3_2),
    ?line disk_log:blog(a, Log_1_2_1),
    ?line disk_log:blog(a, Log_1_2_2),
    ?line ok = disk_log:sync(a),
    ?line {ok, Fd2a} = file:open(File ++ ".2", [read]),
    ?line {ok,Log21_22} = file:read(Fd2a, 200),
    ?line ok = file:close(Fd2a),
    ?line {ok, Fd3a} = file:open(File ++ ".3", [read]),
    ?line Log31_32 = Log_3_1 ++ Log_3_2,
    ?line {ok,Log31_32} = file:read(Fd3a, 200),
    ?line ok = file:close(Fd3a),
    ?line {ok, Fd1a} = file:open(File ++ ".1", [read]),
    ?line Log121_122 = Log_1_2_1 ++ Log_1_2_2,
    ?line {ok,Log121_122} = file:read(Fd1a, 200),
    ?line ok = file:close(Fd1a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
				   {size, {60,3}}, {format, external}]),
    ?line {ok, Fd2b} = file:open(File ++ ".2", [read]),
    ?line {ok,Log21_22} = file:read(Fd2b, 200),
    ?line ok = file:close(Fd2b),
    ?line {ok, Fd3b} = file:open(File ++ ".3", [read]),
    ?line {ok,Log31_32} = file:read(Fd3b, 200),
    ?line ok = file:close(Fd3b),
    ?line {ok, Fd1b} = file:open(File ++ ".1", [read]),
    ?line {ok,Log121_122} = file:read(Fd1b, 200),
    ?line ok = file:close(Fd1b),
    disk_log:close(a),
    del(File, 5),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,5}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:change_size(a, {60, 3}),
    ?line [Log_1_1, Log_1_2,
	   Log_2_1, Log_2_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_1_2_1),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1,
	   Log_1_2_1] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {60,3}}]),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1,
	   Log_1_2_1] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {60, 3}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:change_size(a, {100, 5}),
    ?line [Log_1_1,
	   Log_2_1] = get_all_terms(a),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:log(a, Log_5_1),
    ?line disk_log:log(a, Log_5_2),
    ?line disk_log:log(a, Log_1_2_1),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_5_1, Log_5_2,
	   Log_1_2_1] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100, 5}}]),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_5_1, Log_5_2,
	   Log_1_2_1] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5).



change_size_during(suite) -> [];
change_size_during(doc) -> ["Change size of a wrap log file while logging  "
			    "to a file index between the old and the new size"];
change_size_during(Conf) when is_list(Conf) ->

    Log_1_1 = "first log  first message",
    Log_1_2 = "first log  second message",
    Log_2_1 = "second log  first message",
    Log_2_2 = "second log  second message",
    Log_3_1 = "third log  first message",
    Log_3_2 = "third log  second message",
    Log_4_1 = "fourth log  first message",
    Log_4_2 = "fourth log  second message",
    Log_5_1 = "fifth log  first message",
    Log_5_2 = "fifth log  second message",
    Log_1_2_1 = "first log  second round 1",
    Log_1_2_2 = "first log  second round 2",
    Log_2_2_1 = "second log  second round 1",
    Log_2_2_2 = "second log  second round 2",
    Log_3_2_1 = "third log  second round 1",
    Log_3_2_2 = "third log  second round 2",
    Log_1_3_1 = "first log  third round 1",
    Log_1_3_2 = "first log  third round 2",

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,5}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:log(a, Log_5_1),
    ?line disk_log:log(a, Log_5_2),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:change_size(a, {100, 3}),
    ?line [Log_5_1, Log_5_2,
	   Log_1_1, Log_1_2,
	   Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_4_1, Log_4_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_1_2_1),
    ?line disk_log:log(a, Log_1_2_2),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_1_2_1, Log_1_2_2] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,3}}]),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_1_2_1, Log_1_2_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_2_2_1),
    ?line disk_log:log(a, Log_2_2_2),
    ?line disk_log:log(a, Log_3_2_1),
    ?line disk_log:log(a, Log_3_2_2),
    ?line disk_log:log(a, Log_1_3_1),
    ?line disk_log:log(a, Log_1_3_2),
    ?line [Log_2_2_1, Log_2_2_2,
	   Log_3_2_1, Log_3_2_2,
	   Log_1_3_1, Log_1_3_2] = get_all_terms(a),
    disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,3}}]),
    ?line [Log_2_2_1, Log_2_2_2,
	   Log_3_2_1, Log_3_2_2,
	   Log_1_3_1, Log_1_3_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,5}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:log(a, Log_5_1),
    ?line disk_log:log(a, Log_5_2),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:log(a, Log_5_1),
    ?line disk_log:log(a, Log_5_2),
    ?line disk_log:change_size(a, {100, 3}),
    ?line [Log_1_1, Log_1_2,
	   Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_5_1, Log_5_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_1_2_1),
    ?line disk_log:log(a, Log_1_2_2),
    ?line disk_log:log(a, Log_2_2_1),
    ?line disk_log:log(a, Log_2_2_2),
    ?line disk_log:log(a, Log_3_2_1),
    ?line disk_log:log(a, Log_3_2_2),
    ?line disk_log:log(a, Log_1_3_1),
    ?line disk_log:log(a, Log_1_3_2),
    ?line [Log_2_2_1, Log_2_2_2,
	   Log_3_2_1, Log_3_2_2,
	   Log_1_3_1, Log_1_3_2] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,3}}]),
    ?line [Log_2_2_1, Log_2_2_2,
	   Log_3_2_1, Log_3_2_2,
	   Log_1_3_1, Log_1_3_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5).


change_size_after(suite) -> [];
change_size_after(doc) -> 
    ["Change size of a wrap log file before we have reached "
     "(on the second round) "
     "to the file index corresponding to the new size"];
change_size_after(Conf) when is_list(Conf) ->

    Log_1_1 = "first log  first message",
    Log_1_2 = "first log  second message",
    Log_2_1 = "second log  first message",
    Log_2_2 = "second log  second message",
    Log_3_1 = "third log  first message",
    Log_3_2 = "third log  second message",
    Log_4_1 = "fourth log  first message",
    Log_4_2 = "fourth log  second message",
    Log_5_1 = "fifth log  first message",
    Log_5_2 = "fifth log  second message",
    Log_1_2_1 = "first log  second round 1",
    Log_1_2_2 = "first log  second round 2",

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
                                   {size, {100,5}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:log(a, Log_5_1),
    ?line disk_log:log(a, Log_5_2),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:change_size(a, {100, 3}),
    ?line [Log_3_1,Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_5_1, Log_5_2,
	   Log_1_1, Log_1_2,
	   Log_2_1, Log_2_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_1_2_1),
    ?line disk_log:log(a, Log_1_2_2),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_1_2_1, Log_1_2_2] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
                                   {size, {100,3}}]),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1, Log_3_2,
	   Log_1_2_1, Log_1_2_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
                                   {size, {100,5}}]),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_3_2),
    ?line disk_log:log(a, Log_4_1),
    ?line disk_log:log(a, Log_4_2),
    ?line disk_log:log(a, Log_5_1),
    ?line disk_log:log(a, Log_5_2),
    ?line disk_log:log(a, Log_1_1),
    ?line disk_log:log(a, Log_1_2),
    ?line disk_log:log(a, Log_2_1),
    ?line disk_log:log(a, Log_2_2),
    ?line disk_log:change_size(a, {60, 3}),
    ?line [Log_3_1,Log_3_2,
	   Log_4_1, Log_4_2,
	   Log_5_1, Log_5_2,
	   Log_1_1, Log_1_2,
	   Log_2_1, Log_2_2] = get_all_terms(a),
    ?line disk_log:log(a, Log_3_1),
    ?line disk_log:log(a, Log_1_2_1),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1,
	   Log_1_2_1] = get_all_terms(a),

    ?line disk_log:close(a),
    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
                                   {size, {60,3}}]),
    ?line [Log_2_1, Log_2_2,
	   Log_3_1,
	   Log_1_2_1] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5).



default_size(suite) -> [];
default_size(doc) -> ["Open an existing wrap log without size option "];
default_size(Conf) when is_list(Conf) ->
    ?line Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "a.LOG"),
    ?line {error, {badarg, size}} = disk_log:open([{name,a}, {file, File}, 
                                                   {type, wrap}]),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, 
                                   {size, {100,5}}]),
    ?line disk_log:close(a),

    ?line {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}]),
    ?line {100, 5} = disk_log_1:read_size_file(File),
    ?line ok = disk_log:close(a),
    ?line del(File, 5).

change_size2(suite) -> [];
change_size2(doc) -> ["Testing change_size/2 a bit more..."];
change_size2(Conf) when is_list(Conf) ->
    
    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No),	% cleanup

    %% External halt.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {size, 100000},
                                   {format, external}, {type, halt}]),
    ?line B = mk_bytes(60), % 56 actually...
    ?line ok = disk_log:blog_terms(n, [B,list_to_binary(B),B]),
    ?line Error1 = {error, {new_size_too_small,n,168}} = 
        disk_log:change_size(n, 167),
    ?line "The current size" ++ _ = format_error(Error1),
    ?line ok = disk_log:change_size(n, infinity),
    ?line ok = disk_log:change_size(n, 168),
    ?line ok = disk_log:close(n),
    ?line file:delete(File), % cleanup

    %% External wrap.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true},
				   {format, external}]),
    ?line BB = mk_bytes(160),
    ?line ok = disk_log:blog_terms(n, [BB, BB, BB, BB]), % create all files
    %% Used to be one message, but now one per wrapped file.
    ?line rec(3, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:blog_terms(n, [BB, BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 1}}),
    ?line ok = disk_log:change_size(n, {100, 2}),
    ?line ok = disk_log:change_size(n, {100, 2}),
    ?line {100, 2} = sz(n),
    ?line ok = disk_log:balog_terms(n, [BB, BB]),
    ?line ok = disk_log:balog_terms(n, [BB]),
    ?line ok = disk_log:blog_terms(n, [BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(4, {disk_log, node(), n, {wrap, 1}}),
    ?line ok = disk_log:change_size(n, {100, 4}),
    ?line ok = disk_log:close(n),
    ?line del(File, No),

    %% Internal wrap.
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true},
				   {format, internal}]),
    ?line ok = disk_log:blog_terms(n, [BB, BB, BB, BB]), % create all files
    %% Used to be one message, but now one per wrapped file.
    ?line rec(3, {disk_log, node(), n, {wrap, 0}}),
    ?line ok = disk_log:blog_terms(n, [BB, BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(2, {disk_log, node(), n, {wrap, 1}}),
    ?line ok = disk_log:change_size(n, {100, 2}),
    ?line {100, 2} = sz(n),
    ?line ok = disk_log:blog_terms(n, [BB, BB, BB, BB]),
    %% Used to be one message, but now one per wrapped file.
    ?line rec(4, {disk_log, node(), n, {wrap, 1}}),
    ?line ok = disk_log:close(n),
    ?line del(File, No).

change_size_truncate(suite) -> [];
change_size_truncate(doc) -> ["OTP-3484: truncating index file"];
change_size_truncate(Conf) when is_list(Conf) ->
    
    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "bert.LOG"),
    ?line No = 3,
    ?line B = mk_bytes(60),

    %% The problem here is truncation of the index file. One cannot easily
    %% check that the index file is correctly updated, but print_index_file()
    %% can be used to follow the progress more closely.

    %% Part 1.
    %% Change the size immediately after creating the log, while there
    %% are no log files. This used to write stuff a negative offset
    %% from the beginning of the file.
    ?line del(File, No+1),
    ?line {ok, bert} = disk_log:open([{name,bert}, {type,wrap}, {file, File},
				      {notify, true}, {size,{1000,255}}]),
    ?line ok = disk_log:change_size(bert,{100,No}),
    ?line ok = disk_log:blog(bert, B),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 0}}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 0}}),
    ?line 3 = curf(bert),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line 1 = curf(bert),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),

    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),

    % Three items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),
    ?line 3 = curf(bert),
    ?line ok = disk_log:change_size(bert,{100,1}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    % Three items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    % One item expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),

    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line ok = disk_log:close(bert),
    ?line del(File, No),

    %% Part 2.
    %% Change the size twice, the second time while the the effects of
    %% the first changed have not yet been handled. Finally close before
    %% the index file has been truncated.

    ?line del(File, No),
    ?line {ok, bert} = disk_log:open([{name,bert}, {type,wrap}, {file, File},
				      {notify, true}, {size,{100,No}}]),
    ?line ok = disk_log:blog(bert, B),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 0}}),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 0}}),

    ?line 3 = curf(bert),
    ?line ok = disk_log:change_size(bert,{100,No-1}),

    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),

    ?line 1 = curf(bert),
    ?line ok = disk_log:change_size(bert,{100,No+1}),    

    % Three items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),

    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),

    % Three items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),

    ?line 2 = curf(bert),
    ?line ok = disk_log:change_size(bert,{100,1}),    

    % Three items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),

    ?line ok = disk_log:close(bert),
    
    % State: .siz is 1, current file is 2, index file size is 3...

    ?line {ok, bert} = disk_log:open([{name,bert}, {file, File}, 
				      {type,wrap}, {notify, true}]),
    
    % Three items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),
    
    ?line 2 = curf(bert),
    ?line ok = disk_log:blog(bert, B),
    ?line rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ?line ok = disk_log:close(bert),

    ?line {ok, bert} = disk_log:open([{name,bert}, {file, File}, 
				      {type,wrap}, {notify, true}]),
    
    % Two items expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),
    
    ?line 1 = curf(bert),
    ?line ok = disk_log:blog(bert, B),
    %% Expect {wrap 0}. Nothing lost now, last wrap notification
    %% reported one lost item.
    ?line rec(1, {disk_log, node(), bert, {wrap, 0}}),

    % One item expected.
    % disk_log_1:print_index_file("bert.LOG.idx"),
    ?line ok = disk_log:close(bert),

    ?line del(File, No),
    ok.

change_attribute(suite) -> [];
change_attribute(doc) -> 
    ["Change notify and head"];
change_attribute(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    ?line File = filename:join(Dir, "n.LOG"),
    ?line No = 4,
    ?line del(File, No),	% cleanup
    ?line B = mk_bytes(60),

    ?line Q = qlen(),

    % test change_notify
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    ?line {ok, n} = disk_log:open([{name, n}]), % ignored...
    ?line ok = disk_log:log_terms(n, [B,B]),
    ?line {error, {badarg, notify}} = disk_log:change_notify(n, self(), wrong),
    ?line ok = disk_log:change_notify(n, self(), false),
    ?line ok = disk_log:change_notify(n, self(), true),
    ?line Error1 = {error, {not_owner, _}} = 
        disk_log:change_notify(n, none, true),
    ?line "The pid" ++ _ = format_error(Error1),
    ?line 2 = no_written_items(n),
    ?line 0 = users(n),
    ?line Parent = self(),
    ?line Pid = spawn(fun() -> disk_log:close(n), Parent ! {self(),done} end),
    ?line receive {Pid, done} -> ok end,
    ?line 0 = users(n),
    ?line 1 = length(owners(n)),

    % test change_header
    ?line {error, {badarg, head}} = disk_log:change_header(n, none),
    ?line {error, {badarg, head}} = 
	disk_log:change_header(n, {head_func, {1,2,3}}),
    ?line ok = disk_log:change_header(n, {head, header}),
    ?line ok = disk_log:log(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line 4 = no_written_items(n),
    ?line ok = disk_log:change_header(n, {head, none}),
    ?line ok = disk_log:log(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 0}}),
    ?line 5 = no_written_items(n),
    ?line ok = disk_log:change_header(n, 
			     {head_func, {?MODULE, head_fun, [{ok,header}]}}),
    ?line ok = disk_log:log(n, B),
    ?line rec(1, {disk_log, node(), n, {wrap, 1}}),
    ?line 7 = no_written_items(n),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:close(n),
    ?line del(File, No),
    ?line file:delete(File), % cleanup
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {format, external},
				   {type, halt}]),
    ?line {error, {badarg, head}} = disk_log:change_header(n, {head, header}),
    ?line ok = disk_log:change_header(n, {head, "header"}),
    ?line ok = disk_log:close(n),
    ?line file:delete(File),
    
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    ?line ok = disk_log:change_notify(n, self(), true),
    ?line ok = disk_log:change_header(n, {head, tjolahopp}),
    ?line {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true}]),
    ?line ok = disk_log:close(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line Q = qlen(),
    ?line del(File, No).
    

dist_open(suite) -> [];
dist_open(doc) -> 
    ["Open a distributed log"];
dist_open(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir(Conf),
    ?line true = is_alive(),

    ?line Q = qlen(),
    ?line File = filename:join(PrivDir, "n.LOG"),
    ?line File1 = filename:join(PrivDir, "n1.LOG"),
    ?line No = 3,
    ?line file:delete(File),
    ?line del(File, No),	% cleanup
    ?line del(File1, No),	% cleanup
    ?line B = mk_bytes(60),

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = start_node(disk_log, "-pa " ++ PA),
    ?line wait_for_ready_net(),

    %% open non-distributed on this node:
    ?line {ok,n} = disk_log:open([{name, n}, {file, File}, {type, halt},
                                  {distributed, []}]),

    ?line Error1 = {error, {halt_log, n}} = disk_log:inc_wrap_file(n),
    ?line "The halt log" ++ _ = format_error(Error1),
    ?line ok = disk_log:lclose(n),
    ?line file:delete(File),

    %% open distributed on this node:
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File}, {type, halt},
				    {distributed, [node()]}]),
    %% the error message is ignored:
    ?line ok = disk_log:inc_wrap_file(n),
    ?line ok = disk_log:close(n),
    ?line file:delete(File),

    %% open a wrap log on this node, write something on this node
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [node()]}]),
    ?line ok = disk_log:log(n, B),
    ?line ok = disk_log:close(n),

    %% open a wrap log on this node and aother node, write something
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [node()]}]),
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File1},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [Node]}]),
    ?line ok = disk_log:log(n, B),
    ?line ok = rpc:call(Node, disk_log, log, [n, B]),
    ?line ok = disk_log:close(n),
    ?line del(File, No),
    ?line del(File1, No),
    ?line file:delete(File),

    %% open a wrap log on this node and another node, use lclose
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [node()]}]),
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [node()]},
                                    {linkto,none}]),
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File1},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [Node]}]),
    ?line [_, _] = distributed(n),
    ?line ok = disk_log:lclose(n, Node),
    ?line [_] = distributed(n),
    ?line ok = disk_log:lclose(n),
    ?line ok = disk_log:lclose(n),
    ?line {error, no_such_log} = disk_log:info(n),
    ?line del(File, No),
    ?line del(File1, No),
    ?line file:delete(File),

    % open an invalid log file, and see how error are handled
    ?line First = "n.LOG.1",
    ?line make_file(PrivDir, First, 8),

    ?line {[], [_,_]} = disk_log:open([{name, n}, {file, File},
				       {type, wrap}, {size, {50, No}},
				       {distributed, [Node,node()]}]),
    ?line del(File, No),
    ?line file:delete(File),

    % open a wrap on one other node (not on this node)
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [Node]}]),
    ?line ok = rpc:call(Node, disk_log, log, [n, B]),
    ?line {error, no_such_log} = disk_log:lclose(n),
    ?line ok = disk_log:close(n),

    ?line Q = qlen(),

    ?line {error, no_such_log} = disk_log:info(n),
    ?line del(File, No),
    ?line file:delete(File),
    ?line stop_node(Node),
    ok.
    
dist_error_open(suite) -> [];
dist_error_open(doc) -> 
    ["Open a log distributed and not distributed"];
dist_error_open(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir(Conf),
    ?line true = is_alive(),

    ?line Q = qlen(),
    ?line File = filename:join(PrivDir, "bert.LOG"),
    ?line File1 = filename:join(PrivDir, "bert1.LOG"),
    ?line No = 3,
    ?line file:delete(File),
    ?line del(File, No),	% cleanup
    ?line del(File1, No),	% cleanup

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = start_node(disk_log, "-pa " ++ PA),
    ?line wait_for_ready_net(),

    % open non-distributed on this node:
    ?line {ok,n} = disk_log:open([{name, n}, {file, File},
				  {type, wrap}, {size, {50, No}}]),

    % trying to open distributed on this node (error):
    ?line {[],[Error1={ENode,{error,{node_already_open,n}}}]} = 
	disk_log:open([{name, n}, {file, File},
		       {type, wrap}, {size, {50, No}},
		       {distributed, [node()]}]),
    ?line true = 
        lists:prefix(lists:flatten(io_lib:format("~p: The distribution", 
                                                 [ENode])),
                     format_error(Error1)),
    ?line ok = disk_log:lclose(n),

    % open distributed on this node:
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [node()]}]),
    
    % trying to open non-distributed on this node (error):
    ?line {_,{node_already_open,n}} = 
	disk_log:open([{name, n}, {file, File},
		       {type, wrap}, {size, {50, No}}]),

    ?line ok = disk_log:close(n),
    ?line Q = qlen(),

    ?line del(File, No),
    ?line del(File1, No),
    ?line file:delete(File),
    ?line stop_node(Node),
    ok.

dist_notify(suite) -> [];
dist_notify(doc) -> 
    ["Notification from other node"];
dist_notify(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir(Conf),
    ?line true = is_alive(),
    
    ?line File = filename:join(PrivDir, "bert.LOG"),
    ?line File1 = filename:join(PrivDir, "bert1.LOG"),
    ?line No = 3,
    ?line B = mk_bytes(60),
    ?line file:delete(File),
    ?line file:delete(File1),
    ?line del(File, No),	% cleanup
    ?line del(File1, No),

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = start_node(disk_log, "-pa " ++ PA),
    ?line wait_for_ready_net(),

    % opening distributed on this node:
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File}, {notify, false},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [node()]}]),

    % opening distributed on other node:
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File1}, 
				    {notify, true}, {linkto, self()},
				    {type, wrap}, {size, {50, No}},
				    {distributed, [Node]}]),
    ?line disk_log:alog(n, B),
    ?line disk_log:alog(n, B),
    ?line ok = disk_log:sync(n),
    ?line rec(1, {disk_log, Node, n, {wrap, 0}}),
    ?line ok = disk_log:close(n),

    ?line del(File, No),
    ?line del(File1, No),
    ?line file:delete(File),
    ?line stop_node(Node),
    ok.

dist_terminate(suite) -> [];
dist_terminate(doc) -> 
    ["Terminating nodes with distributed logs"];
dist_terminate(Conf) when is_list(Conf) ->
    ?line Dir = ?privdir(Conf),
    ?line true = is_alive(),

    ?line File = filename:join(Dir, "n.LOG"),
    ?line File1 = filename:join(Dir, "n1.LOG"),
    No = 1,
    del(File, No),	% cleanup
    del(File1, No),	% cleanup

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = start_node(disk_log, "-pa " ++ PA),
    ?line wait_for_ready_net(),

    %% Distributed versions of two of the situations in close_block(/1.

    %% One of two owners terminates.
    ?line Pid1 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid2 = spawn_link(?MODULE, lserv, [n]),
    ?line {[{_, {ok, n}}], []} = sync_do(Pid1, {dist_open, File, node()}),
    ?line {[{_, {ok, n}}], []} = sync_do(Pid2, {dist_open, File1, Node}),
    ?line [_] = sync_do(Pid1, owners),
    ?line [_] = sync_do(Pid2, owners),
    ?line 0 = sync_do(Pid1, users),
    ?line 0 = sync_do(Pid2, users),
    ?line sync_do(Pid1, terminate),
    ?line [_] = sync_do(Pid2, owners),
    ?line 0 = sync_do(Pid2, users),
    ?line sync_do(Pid2, terminate),    
    ?line {error, no_such_log} = disk_log:info(n),

    %% Users terminate (no link...).
    ?line Pid3 = spawn_link(?MODULE, lserv, [n]),
    ?line Pid4 = spawn_link(?MODULE, lserv, [n]),
    ?line {[{_, {ok, n}}], []} = 
	sync_do(Pid3, {dist_open, File, none, node()}),
    ?line {[{_, {ok, n}}], []} = 
	sync_do(Pid4, {dist_open, File1, none, Node}),
    ?line [] = sync_do(Pid3, owners),
    ?line [] = sync_do(Pid4, owners),
    ?line 1 = sync_do(Pid3, users),
    ?line 1 = sync_do(Pid4, users),
    ?line sync_do(Pid3, terminate),
    ?line [] = sync_do(Pid4, owners),
    ?line 1 = sync_do(Pid4, users),
    ?line sync_do(Pid4, terminate),    
    ?line ok = disk_log:close(n), % closing all nodes
    ?line {error, no_such_log} = disk_log:info(n),
    
    ?line del(File, No),
    ?line del(File1, No),
    ?line stop_node(Node),
    ok.

dist_accessible(suite) -> [];
dist_accessible(doc) -> 
    ["Accessible logs on nodes"];
dist_accessible(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir(Conf),

    ?line true = is_alive(),

    ?line F1 = filename:join(PrivDir, "a.LOG"),
    ?line file:delete(F1),
    ?line F2 = filename:join(PrivDir, "b.LOG"),
    ?line file:delete(F2),
    ?line F3 = filename:join(PrivDir, "c.LOG"),
    ?line file:delete(F3),
    ?line F4 = filename:join(PrivDir, "d.LOG"),
    ?line file:delete(F1),
    ?line F5 = filename:join(PrivDir, "e.LOG"),
    ?line file:delete(F2),
    ?line F6 = filename:join(PrivDir, "f.LOG"),
    ?line file:delete(F3),

    ?line {[],[]} = disk_log:accessible_logs(),
    ?line {ok, a} = disk_log:open([{name, a}, {type, halt}, {file, F1}]),
    ?line {[a],[]} = disk_log:accessible_logs(),
    ?line {ok, b} = disk_log:open([{name, b}, {type, halt}, {file, F2}]),
    ?line {[a,b],[]} = disk_log:accessible_logs(),
    ?line {ok, c} = disk_log:open([{name, c}, {type, halt}, {file, F3}]),
    ?line {[a,b,c],[]} = disk_log:accessible_logs(),

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = start_node(disk_log, "-pa " ++ PA),
    ?line wait_for_ready_net(),

    ?line {[_],[]} = disk_log:open([{name, a}, {file, F4}, {type, halt},
				    {distributed, [Node]}]),
    ?line {[a,b,c],[]} = disk_log:accessible_logs(),
    ?line {[],[a]} = rpc:call(Node, disk_log, accessible_logs, []),
    ?line {[_],[]} = disk_log:open([{name, b}, {file, F5}, {type, halt},
				    {distributed, [Node]}]),
    ?line {[],[a,b]} = rpc:call(Node, disk_log, accessible_logs, []),
    ?line {[_],[]} = disk_log:open([{name, c}, {file, F6}, {type, halt},
				    {distributed, [Node]}]),
    ?line {[],[a,b,c]} = rpc:call(Node, disk_log, accessible_logs, []),
    ?line {[a,b,c],[]} = disk_log:accessible_logs(),
    ?line ok = disk_log:close(a),
    ?line {[b,c],[a]} = disk_log:accessible_logs(),
    ?line ok = disk_log:close(b),
    ?line {[c],[a,b]} = disk_log:accessible_logs(),
    ?line ok = disk_log:close(b),
    ?line {[c],[a]} = disk_log:accessible_logs(),
    ?line {[],[a,c]} = rpc:call(Node, disk_log, accessible_logs, []),
    ?line ok = disk_log:close(c),
    ?line {[],[a,c]} = disk_log:accessible_logs(),
    ?line ok = disk_log:close(c),
    ?line {[],[a]} = disk_log:accessible_logs(),
    ?line {[],[a]} = rpc:call(Node, disk_log, accessible_logs, []),
    ?line ok = disk_log:close(a),
    ?line {[],[]} = disk_log:accessible_logs(),
    ?line {[],[]} = rpc:call(Node, disk_log, accessible_logs, []),

    ?line file:delete(F1),
    ?line file:delete(F2),
    ?line file:delete(F3),
    ?line file:delete(F4),
    ?line file:delete(F5),
    ?line file:delete(F6),

    ?line stop_node(Node),
    ok.

dist_deadlock(suite) -> [];
dist_deadlock(doc) -> 
    ["OTP-4405. Deadlock between two nodes could happen."];
dist_deadlock(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir(Conf),

    ?line true = is_alive(),

    ?line F1 = filename:join(PrivDir, "a.LOG"),
    ?line file:delete(F1),
    ?line F2 = filename:join(PrivDir, "b.LOG"),
    ?line file:delete(F2),

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node1} = start_node(disk_log_node1, "-pa " ++ PA),
    ?line {ok, Node2} = start_node(disk_log_node2, "-pa " ++ PA),
    ?line wait_for_ready_net(),

    Self = self(),
    Fun1 = fun() -> dist_dl(Node2, a, F1, Self) end,
    Fun2 = fun() -> dist_dl(Node1, b, F2, Self) end,
    P1 = spawn(Node1, Fun1),
    P2 = spawn(Node2, Fun2),
    receive {P1, a} -> ok end,
    receive {P2, b} -> ok end,

    ?line stop_node(Node1),
    ?line stop_node(Node2),

    ?line file:delete(F1),
    ?line file:delete(F2),
    ok.

dist_dl(Node, Name, File, Pid) ->
    {[{Node,{ok,Log}}], []} =
	disk_log:open([{name,Name},{file,File},{distributed,[Node]}]),
    timer:sleep(50), % give the nodes chance to exchange pg2 information
    ok = disk_log:close(Log),
    Pid ! {self(), Name},
    ok.

dist_open2(suite) -> [];
dist_open2(doc) -> 
    ["OTP-4480. Opening several logs simultaneously."];
dist_open2(Conf) when is_list(Conf) ->
    ?line true = is_alive(),
    ?line {ok, _Pg2} = pg2:start(),

    dist_open2_1(Conf, 0),
    dist_open2_1(Conf, 100),

    dist_open2_2(Conf, 0),
    dist_open2_2(Conf, 100),

    PrivDir = ?privdir(Conf),
    Log = n,

    %% Open a log three times (very fast). Two of the opening
    %% processes will be put on hold (pending). The first one failes
    %% to open the log. The second one succeeds, and the third one is
    %% attached.
    P0 = pps(),
    ?line File0 = "n.LOG",
    ?line File = filename:join(PrivDir, File0),
    ?line make_file(PrivDir, File0, 8),

    Parent = self(),
    F1 = fun() -> R = disk_log:open([{name, Log}, {file, File},
                                     {type, halt}, {format,internal},
                                     {distributed, [node()]}]),
                  Parent ! {self(), R}
         end,
    F2 = fun() -> R = disk_log:open([{name, Log}, {file, File},
                                     {type, halt}, {format,external},
                                     {distributed, [node()]}]),
                  Parent ! {self(), R},
                  timer:sleep(300)
         end,
    ?line Pid1 = spawn(F1),
    timer:sleep(10),
    ?line Pid2 = spawn(F2),
    ?line Pid3 = spawn(F2),

    ?line receive {Pid1,R1} -> {[],[_]} = R1 end,
    ?line receive {Pid2,R2} -> {[_],[]} = R2 end,
    ?line receive {Pid3,R3} -> {[_],[]} = R3 end,

    timer:sleep(500),
    ?line file:delete(File),
    ?line true = (P0 == pps()),

    %% This time the first process has a naughty head_func. This test
    %% does not add very much. Perhaps it should be removed. However,
    %% a head_func like this is why it's necessary to have an separate
    %% process calling disk_log:internal_open: the server cannot wait
    %% for the reply, but the call must be monitored, and this is what
    %% is accomplished by having a proxy process.
    F3 = fun() ->
                 R = disk_log:open([{name,Log},{file,File},
                                    {format,internal},
                                    {head_func,{?MODULE,head_exit,[]}},
                                    {type,halt}, {linkto,none}]),
                 Parent ! {self(), R}
         end,
    F4 = fun() ->
                 R = disk_log:open([{name,Log},{file,File},
                                    {format,internal},
                                    {type,halt}]),
                 Parent ! {self(), R}
         end,
    ?line Pid4 = spawn(F3),
    timer:sleep(10),
    ?line Pid5 = spawn(F4),
    ?line Pid6 = spawn(F4),
    %% The timing is crucial here.
    ?line R = case receive {Pid4,R4} -> R4 end of
                  {error, no_such_log} ->
                      ?line R5 = receive {Pid5, R5a} -> R5a end,
                      ?line R6 = receive {Pid6, R6a} -> R6a end,
                      case {R5, R6} of
                          {{repaired, _, _, _}, {ok, Log}} -> ok;
                          {{ok, Log}, {repaired, _, _, _}} -> ok;
                          _ -> test_server_fail({bad_replies, R5, R6})
                      end,
                      ok;
                  {ok, Log} -> % uninteresting case
                      ?line receive {Pid5,_R5} -> ok end,
                      ?line receive {Pid6,_R6} -> ok end,
                      {comment, 
                       "Timing dependent test did not check anything."}
              end,

    timer:sleep(100),
    ?line {error, no_such_log} = disk_log:close(Log),
    file:delete(File),
    ?line true = (P0 == pps()),

    No = 2,
    Log2 = n2,
    File2 = filename:join(PrivDir, "b.LOG"),
    file:delete(File2),
    del(File, No),
    
    %% If a client takes a long time when writing the header, other
    %% processes should be able to attach to other log without having to
    %% wait.

    ?line {ok,Log} = 
        disk_log:open([{name,Log},{file,File},{type,wrap},{size,{100,No}}]),
    Pid = spawn(fun() -> 
                        receive {HeadPid, start} -> ok end,
                        {ok,Log2} = disk_log:open([{name,Log2},{file,File2},
                                                   {type,halt}]),
                        HeadPid ! {self(), done}
                end),
    HeadFunc = {?MODULE, slow_header, [Pid]},
    ?line ok = disk_log:change_header(Log, {head_func, HeadFunc}),
    ?line ok = disk_log:inc_wrap_file(Log), % header is written

    timer:sleep(100),
    ?line ok = disk_log:close(Log),

    file:delete(File2),
    del(File, No),
    ?line true = (P0 == pps()),

    R.
    
dist_open2_1(Conf, Delay) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    Log = n,

    A0 = [{name,Log},{file,File},{type,halt}],
    ?line create_opened_log(File, A0),
    P0 = pps(),

    Log2 = log2,
    File2 = "log2.LOG",
    ?line file:delete(File2),
    ?line {ok,Log2} = disk_log:open([{name,Log2},{file,File2},{type,halt}]),

    Parent = self(),
    F = fun() -> 
                R = disk_log:open(A0),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    ?line Pid1 = spawn(F),
    timer:sleep(10),
    ?line Pid2 = spawn(F),
    ?line Pid3 = spawn(F),
    ?line {error, no_such_log} = disk_log:log(Log, term), % is repairing now
    ?line 0 = qlen(),

    %% The file is already open, so this will not take long.
    ?line {ok,Log2} = disk_log:open([{name,Log2},{file,File2},{type,halt}]),
    ?line 0 = qlen(), % still repairing
    ?line ok = disk_log:close(Log2),
    ?line {error, no_such_log} = disk_log:close(Log2),
    ?line file:delete(File2),

    ?line receive {Pid1,R1} -> {repaired,_,_,_} = R1 end,
    ?line receive {Pid2,R2} -> {ok,_} = R2 end,
    ?line receive {Pid3,R3} -> {ok,_} = R3 end,
    timer:sleep(500),
    ?line {error, no_such_log} = disk_log:info(Log),

    file:delete(File),
    ?line true = (P0 == pps()),

    ok.

dist_open2_2(Conf, Delay) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    Log = n,

    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node1} = start_node(disk_log_node2, "-pa " ++ PA),
    ?line wait_for_ready_net(),
    P0 = pps(),

    A0 = [{name,Log},{file,File},{type,halt}],
    ?line create_opened_log(File, A0),

    Log2 = log2,
    File2 = "log2.LOG",
    ?line file:delete(File2),
    ?line {[{Node1,{ok,Log2}}],[]} = 
        disk_log:open([{name,Log2},{file,File2},{type,halt},
                       {distributed,[Node1]}]),

    Parent = self(),
    F = fun() ->
                %% It would be nice to slow down the repair. head_func
                %% cannot be used since it is not called when repairing.
                R = disk_log:open([{distributed,[Node1]} | A0]),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    %% And {priority, ...} probably has no effect either.
    ?line Pid1 = spawn_opt(F, [{priority, low}]),
    % timer:sleep(1), % no guarantee that Pid1 will return {repaired, ...}
    ?line Pid2 = spawn_opt(F, [{priority, low}]),
    ?line {error, no_such_log} = 
        disk_log:log(Log, term), % maybe repairing now
    ?line 0 = qlen(),

    %% The file is already open, so this will not take long.
    ?line {[{Node1,{ok,Log2}}],[]} = 
        disk_log:open([{name,Log2},{file,File2},{type,halt},
                       {distributed,[Node1]}]),
    ?line 0 = qlen(), % probably still repairing
    ?line ok = disk_log:close(Log2),
    ?line file:delete(File2),

    ?line receive {Pid1,R1} -> R1 end,
    ?line receive {Pid2,R2} -> R2 end,
    ?line case {R1, R2} of
	      {{[{Node1,{repaired,_,_,_}}],[]}, 
	       {[{Node1,{ok,Log}}],[]}}         -> ok;
	      {{[{Node1,{ok,Log}}],[]}, 
	       {[{Node1,{repaired,_,_,_}}],[]}} -> ok
	  end,

    ?line true = (P0 == pps()),
    ?line stop_node(Node1),
    file:delete(File),
    ok.

head_exit() ->
    process_flag(trap_exit, false), % Don't do like this!
    spawn_link(fun() -> exit(helfel) end),
    {ok,"123"}.

slow_header(Pid) ->
    Pid ! {self(), start},
    receive {Pid, done} -> ok end,
    {ok, <<>>}.

create_opened_log(File, Args) ->
    Log = n,
    file:delete(File),
    {ok, Log} = disk_log:open(Args),
    log_terms(Log, 400000),
    ok = disk_log:close(Log),
    mark(File, ?OPENED),
    ok.

log_terms(_Log, 0) ->
    ok;
log_terms(Log, N) when N > 100 ->
    Terms = [{term,I} || I <- lists:seq(N-99, N)],
    ok = disk_log:log_terms(Log, Terms),
    log_terms(Log, N-100);
log_terms(Log, N) ->
    ok = disk_log:log(Log, {term, N}),
    log_terms(Log, N-1).

other_groups(suite) -> [];
other_groups(doc) -> 
    ["OTP-5810. Cope with pg2 groups that are not disk logs."];
other_groups(Conf) when is_list(Conf) ->
    ?line true = is_alive(),
    ?line PrivDir = ?privdir(Conf),

    ?line File = filename:join(PrivDir, "n.LOG"),
    ?line file:delete(File),

    ?line {[],[]} = disk_log:accessible_logs(),
    ?line {[_],[]} = disk_log:open([{name, n}, {file, File}, {type, halt},
				    {distributed, [node()]}]),
    ?line {[],[n]} = disk_log:accessible_logs(),
    Group = grupp,
    ?line pg2:create(Group),
    ?line ok = pg2:join(Group, self()),
    ?line {[],[n]} = disk_log:accessible_logs(),
    ?line [_] = 
        lists:filter(fun(P) -> disk_log:pid2name(P) =/= undefined end, 
                     erlang:processes()),
    ?line pg2:delete(Group),
    ?line {[],[n]} = disk_log:accessible_logs(),
    ?line ok = disk_log:close(n),
    ?line {[],[]} = disk_log:accessible_logs(),
    ?line file:delete(File),

    ok.

-define(MAX, 16384). % MAX in disk_log_1.erl
evil(suite) -> [];
evil(doc) -> ["Evil cases such as closed file descriptor port."];
evil(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    Log = n,

    %% Not a very thorough test.

    ?line ok = setup_evil_filled_cache_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:log(Log, apa),
    ?line ok = disk_log:close(Log),

    ?line ok = setup_evil_filled_cache_halt(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:truncate(Log, apa),
    ?line ok = stop_evil(Log),

    %% White box test. 
    file:delete(File),
    ?line Ports0 = erlang:ports(),
    ?line {ok, Log} = disk_log:open([{name,Log},{file,File},{type,halt},
                                     {size,?MAX+50},{format,external}]),
    ?line [Fd] = erlang:ports() -- Ports0,
    ?line {B,_} = x_mk_bytes(30),
    ?line ok = disk_log:blog(Log, <<0:(?MAX+1)/unit:8>>),
    ?line exit(Fd, kill),
    ?line {error, {file_error,_,einval}} = disk_log:blog_terms(Log, [B,B]),
    ?line ok= disk_log:close(Log),
    file:delete(File),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:close(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:log(Log, apa),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_halt(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:log(Log, apa),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:reopen(Log, apa),
    ?line {error, {file_error,_,einval}} = disk_log:reopen(Log, apa),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:reopen(Log, apa),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:inc_wrap_file(Log),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:chunk(Log, start),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:truncate(Log),
    ?line ok = stop_evil(Log),

    ?line ok = setup_evil_wrap(Log, Dir),
    ?line {error, {file_error,_,einval}} = disk_log:chunk_step(Log, start, 1),
    ?line ok = stop_evil(Log),

    io:format("messages: ~p~n", [erlang:process_info(self(), messages)]),
    del(File, 2),
    file:delete(File),
    ok.

setup_evil_wrap(Log, Dir) ->
    setup_evil(Log, [{type,wrap},{size,{100,2}}], Dir).

setup_evil_halt(Log, Dir) ->
    setup_evil(Log, [{type,halt},{size,10000}], Dir).

setup_evil(Log, Args, Dir) ->
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),
    file:delete(File),
    del(File, 2),
    ok = disk_log:start(),
    Ports0 = erlang:ports(),
    {ok, Log} = disk_log:open([{name,Log},{file,File} | Args]),
    [Fd] = erlang:ports() -- Ports0,
    exit(Fd, kill),
    ok = disk_log:log_terms(n, [<<0:10/unit:8>>]),
    timer:sleep(2500), % TIMEOUT in disk_log_1.erl is 2000
    ok.

stop_evil(Log) ->
    {error, _} = disk_log:close(Log),
    ok.

setup_evil_filled_cache_wrap(Log, Dir) ->
    setup_evil_filled_cache(Log, [{type,wrap},{size,{?MAX,2}}], Dir).

setup_evil_filled_cache_halt(Log, Dir) ->
    setup_evil_filled_cache(Log, [{type,halt},{size,infinity}], Dir).

%% The cache is filled, and the file descriptor port gone.
setup_evil_filled_cache(Log, Args, Dir) ->
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),
    file:delete(File),
    del(File, 2),
    ok = disk_log:start(),
    Ports0 = erlang:ports(),
    {ok, Log} = disk_log:open([{name,Log},{file,File} | Args]),
    [Fd] = erlang:ports() -- Ports0,
    ok = disk_log:log_terms(n, [<<0:?MAX/unit:8>>]),
    exit(Fd, kill),
    ok.

otp_6278(suite) -> [];
otp_6278(doc) -> ["OTP-6278. open/1 creates no status or crash report."];
otp_6278(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "no_such_dir/no_such_file"),
    ?line error_logger:add_report_handler(?MODULE, self()),
    ?line {error, {file_error, _, _}} = 
        disk_log:open([{name,n},{file,File}]),
    receive
	{crash_report,_Pid,Report} ->
	    ?line io:format("Unexpected: ~p\n", [Report]),
	    ?line ?t:fail()
    after 1000 ->
            ok
    end,
    ?line error_logger:delete_report_handler(?MODULE).

otp_10131(suite) -> [];
otp_10131(doc) -> ["OTP-10131. head_func type."];
otp_10131(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    Log = otp_10131,
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),
    HeadFunc = {?MODULE, head_fun, [{ok,"head"}]},
    {ok, Log} = disk_log:open([{name,Log},{file,File},
                               {head_func, HeadFunc}]),
    HeadFunc = info(Log, head, undef),
    HeadFunc2 = {?MODULE, head_fun, [{ok,"head2"}]},
    ok = disk_log:change_header(Log, {head_func, HeadFunc2}),
    HeadFunc2 = info(Log, head, undef),
    ok = disk_log:close(Log),
    ok.

mark(FileName, What) ->
    {ok,Fd} = file:open(FileName, [raw, binary, read, write]),
    {ok,_} = file:position(Fd, 4),
    ok = file:write(Fd, What),
    ok = file:close(Fd).

crash(File, Where) ->
    {ok, Fd} = file:open(File, [read,write]),
    file:position(Fd, Where),
    ok = file:write(Fd, [10]),
    ok = file:close(Fd).

unwritable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

writable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode bor 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

truncate(File, Where) ->
    {ok, Fd} = file:open(File, [read,write]),
    file:position(Fd, Where),
    ok = file:truncate(Fd),
    ok = file:close(Fd).

file_size(File) ->
    {ok, F} = file:read_file_info(File),
    F#file_info.size.

copy_wrap_log(FromName, N, FromDir, ToDir) ->
    copy_wrap_log(FromName, FromName, N, FromDir, ToDir).

copy_wrap_log(FromName, ToName, N, FromDir, ToDir) ->
    Fun = fun(E) ->
	     From = join(FromDir, io_lib:format("~s.~p", [FromName, E])),
	     To = join(ToDir, io_lib:format("~s.~p", [ToName, E])),
	     case file:read_file_info(From) of
                 {ok, _FileInfo} ->
                     copy_file(From, To);
                 _Else ->
                     ok
             end
	  end,
    Exts = [idx, siz | lists:seq(1, N)],
    lists:foreach(Fun, Exts).

-define(BUFSIZE, 8192).

copy_file(Src, Dest) ->
    % ?t:format("copying from ~p to ~p~n", [Src, Dest]),
    {ok, InFd} = file:open(Src, [raw, binary, read]),
    {ok, OutFd} = file:open(Dest, [raw, binary, write]),
    ok = copy_file1(InFd, OutFd),
    file:close(InFd),
    file:close(OutFd),
    ok = file:change_mode(Dest, 8#0666).

copy_file1(InFd, OutFd) ->
    case file:read(InFd, ?BUFSIZE) of
        {ok, Bin} ->
            ok = file:write(OutFd, Bin),
            copy_file1(InFd, OutFd);
        eof  ->
            ok
    end.


join(A, B) ->
    filename:nativename(filename:join(A, B)).

add_ext(Name, Ext) ->
    lists:concat([Name, ".", Ext]).

log(_Name, 0) ->
    ok;
log(Name, N) ->
    ok = disk_log:log(Name, "this is a logged message number " ++ 
                      integer_to_list(N)),
    log(Name, N-1).

format_error(E) ->
    lists:flatten(disk_log:format_error(E)).

pps() ->
    timer:sleep(100),
    {erlang:ports(), lists:filter(fun(P) -> erlang:is_process_alive(P) end,
				  processes())}.

qlen() ->
    {_, {_, N}} = lists:keysearch(message_queue_len, 1, process_info(self())),
    N.

owners(Log) ->
%%     io:format("owners ~p~n", [info(Log, owners, -1)]),
    info(Log, owners, -1).
users(Log) ->
%%     io:format("users ~p~n", [info(Log, users, -1)]),
    info(Log, users, -1).
status(Log) ->
%%     io:format("status ~p~n", [info(Log, status, -1)]),
    info(Log, status, -1).
distributed(Log) ->
%%     io:format("distributed ~p~n", [info(Log, distributed, -1)]),
    info(Log, distributed, -1).
no_items(Log) ->
%%     io:format("no_items ~p~n", [info(Log, no_items, -1)]),
    info(Log, no_items, -1).
no_written_items(Log) ->
%%     io:format("no_written_items ~p~n", [info(Log, no_written_items, -1)]),
    info(Log, no_written_items, -1).
sz(Log) -> 
%%     io:format("sz ~p~n", [info(Log, size, -1)]),
    info(Log, size, -1).
curb(Log) -> 
%%     io:format("curb ~p~n", [info(Log, no_current_bytes, -1)]),
    info(Log, no_current_bytes, -1).
curf(Log) -> 
%%     io:format("curf ~p~n", [info(Log, current_file, -1)]),
    info(Log, current_file, -1).
cur_cnt(Log) -> 
%%     io:format("cur_cnt ~p~n", [info(Log, no_current_items, -1)]),
    info(Log, no_current_items, -1).
no_overflows(Log) ->
%%     io:format("no_overflows ~p~n", [info(Log, no_overflows, -1)]),
    info(Log, no_overflows, -1).

info(Log, What, Undef) ->
    case lists:keysearch(What, 1, disk_log:info(Log)) of
        {value, {What, Value}} -> Value;
        false -> Undef
    end.

rec(0, _) ->
     ok;
rec(N, Msg) ->
    receive
	Msg ->
	    rec(N-1, Msg)
    after 100 ->
	    test_server_fail({no_msg, N, Msg})
    end.

%% Copied from global_SUITE.erl.
-define(UNTIL(Seq), loop_until_true(fun() -> Seq end)).

loop_until_true(Fun) ->
    case Fun() of
	true ->
	    ok;
	_ ->
	    timer:sleep(1000),
	    loop_until_true(Fun)
    end.

wait_for_ready_net() ->
    Nodes = lists:sort([node() | nodes()]),
    ?UNTIL(begin
               lists:all(fun(N) -> Nodes =:= get_known(N) end, Nodes) and
               lists:all(fun(N) -> 
                                 LNs = rpc:call(N, erlang, nodes, []),
                                 Nodes =:= lists:sort([N | LNs])
                         end, Nodes)
           end).

get_known(Node) ->
    case catch gen_server:call({global_name_server,Node}, get_known) of
        {'EXIT', _} ->
            [list, without, nodenames];
        Known -> 
            lists:sort([Node | Known])
    end.

%% Copied from erl_distribution_SUITE.erl:
start_node(Name, Param) ->
    ?t:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    ?t:stop_node(Node).

%from(H, [H | T]) -> T;
%from(H, [_ | T]) -> from(H, T);
%from(_H, []) -> [].


%% Check for NFS cache size, this is called from init_per_testcase() and
%% makes different tests run depending on the size of the NFS cache on 
%% VxWorks. Possibly this could be adopted to Windows too, but we seldom use
%% NFS when testing on windows, so I can find better things to do.
%% The port program used simply reads the nfsCacheSize variable on the board.
%% If the board is configured without NFS, the port program will fail to load
%% and this will return 0, which may or may not be the wrong thing to do.

check_nfs(_Config) ->
    0.

skip_expand([]) ->
    [];
skip_expand([Case | T]) ->
    case (catch apply(?MODULE, Case, [suite])) of
	{'EXIT', _} ->
	    [Case | skip_expand(T)];
	[] ->
	    [Case | skip_expand(T)];
	Res ->
	    skip_expand(Res) ++ skip_expand(T)
    end.
	    
    
skip_list(Config) ->
    case check_nfs(Config) of
	0 ->
	    skip_expand(?SKIP_NO_CACHE);
	_ ->
	    skip_expand(?SKIP_LARGE_CACHE)
    end.

should_skip(_Test,_Config) ->
    false.

%%-----------------------------------------------------------------
%% The error_logger handler used.
%% (Copied from stdlib/test/proc_lib_SUITE.erl.)
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.
    
handle_event({error_report, _GL, {Pid, crash_report, Report}}, Tester) ->
    Tester ! {crash_report, Pid, Report},
    {ok, Tester};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    State.
