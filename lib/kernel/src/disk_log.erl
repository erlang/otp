%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2020. All Rights Reserved.
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
-module(disk_log).

%% Efficient file based log - process part

-export([start/0, istart_link/1, 
	 log/2, log_terms/2, blog/2, blog_terms/2,
	 alog/2, alog_terms/2, balog/2, balog_terms/2,
	 close/1, lclose/1, lclose/2, sync/1, open/1, 
	 truncate/1, truncate/2, btruncate/2,
	 reopen/2, reopen/3, breopen/3, inc_wrap_file/1, change_size/2,
	 change_notify/3, change_header/2, 
	 chunk/2, chunk/3, bchunk/2, bchunk/3, chunk_step/3, chunk_info/1,
	 block/1, block/2, unblock/1, info/1, format_error/1,
	 accessible_logs/0, all/0]).

%% Internal exports
-export([init/2, internal_open/2,
	 system_continue/3, system_terminate/4, system_code_change/4]).

%% To be used by disk_log_h.erl (not (yet) in Erlang/OTP) only.
-export([ll_open/1, ll_close/1, do_log/2, do_sync/1, do_info/2]).

%% To be used by wrap_log_reader only.
-export([ichunk_end/2]).

%% To be used for debugging only:
-export([pid2name/1]).

-export_type([continuation/0]).

-deprecated([{accessible_logs, 0, "use disk_log:all/0 instead"},
             {lclose, 1, "use disk_log:close/1 instead"},
             {lclose, 2, "use disk_log:close/1 instead"}]).

-type dlog_state_error() :: 'ok' | {'error', term()}.

-record(state, {queue = [],
		messages = [],
		parent,
		server,
		cnt = 0           :: non_neg_integer(),
		args,
		error_status = ok :: dlog_state_error(),
		cache_error = ok     %% cache write error after timeout
	       }).

-include("disk_log.hrl").

-define(failure(Error, Function, Arg), 
	{{failed, Error}, [{?MODULE, Function, Arg}]}).

%%-define(PROFILE(C), C).
-define(PROFILE(C), void).

-compile({inline,[{log_loop,6},{log_end_sync,2},{replies,2},{rflat,1}]}).

%%%----------------------------------------------------------------------
%%% Contract type specifications
%%%----------------------------------------------------------------------

-opaque continuation() :: #continuation{}.

-type file_error()     :: term().  % XXX: refine
-type invalid_header() :: term().  % XXX: refine

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------
%% This module implements the API, and the processes for each log.
%% There is one process per log.
%%-----------------------------------------------------------------      

-type open_error_rsn() :: 'no_such_log'
                        | {'badarg', term()}
                        | {'size_mismatch', CurrentSize :: dlog_size(),
                           NewSize :: dlog_size()}
                        | {'arg_mismatch', OptionName :: dlog_optattr(),
                           CurrentValue :: term(), Value :: term()}
                        | {'name_already_open', Log :: log()}
                        | {'open_read_write', Log :: log()}
                        | {'open_read_only', Log :: log()}
                        | {'need_repair', Log :: log()}
                        | {'not_a_log_file', FileName :: file:filename()}
                        | {'invalid_index_file', FileName :: file:filename()}
                        | {'invalid_header', invalid_header()}
                        | {'file_error', file:filename(), file_error()}
                        | {'node_already_open', Log :: log()}.
-type open_ret()       :: {'ok', Log :: log()}
                        | {'repaired', Log :: log(),
                           {'recovered', Rec :: non_neg_integer()},
                           {'badbytes', Bad :: non_neg_integer()}}
                        | {'error', open_error_rsn()}.

-spec open(ArgL) -> open_ret() when
      ArgL :: dlog_options().
open(A) ->
    disk_log_server:open(check_arg(A, #arg{options = A})).

-type log_error_rsn() :: 'no_such_log' | 'nonode' | {'read_only_mode', log()}
                       | {'format_external', log()} | {'blocked_log', log()}
                       | {'full', log()} | {'invalid_header', invalid_header()}
                       | {'file_error', file:filename(), file_error()}.

-spec log(Log, Term) -> ok | {error, Reason :: log_error_rsn()} when
      Log :: log(),
      Term :: term().
log(Log, Term) -> 
    req(Log, {log, internal, [term_to_binary(Term)]}).

-spec blog(Log, Bytes) -> ok | {error, Reason :: log_error_rsn()} when
      Log :: log(),
      Bytes :: iodata().
blog(Log, Bytes) ->
    req(Log, {log, external, [ensure_binary(Bytes)]}).

-spec log_terms(Log, TermList) -> ok | {error, Reason :: log_error_rsn()} when
      Log :: log(),
      TermList :: [term()].
log_terms(Log, Terms) ->
    Bs = terms2bins(Terms),
    req(Log, {log, internal, Bs}).

-spec blog_terms(Log, BytesList) ->
                        ok | {error, Reason :: log_error_rsn()} when
      Log :: log(),
      BytesList :: [iodata()].
blog_terms(Log, Bytess) ->
    Bs = ensure_binary_list(Bytess),
    req(Log, {log, external, Bs}).

-type notify_ret() :: 'ok' | {'error', 'no_such_log'}.

-spec alog(Log, Term) -> notify_ret() when
      Log :: log(),
      Term :: term().
alog(Log, Term) -> 
    notify(Log, {alog, internal, [term_to_binary(Term)]}).

-spec alog_terms(Log, TermList) -> notify_ret() when
      Log :: log(),
      TermList :: [term()].
alog_terms(Log, Terms) ->
    Bs = terms2bins(Terms),
    notify(Log, {alog, internal, Bs}).

-spec balog(Log, Bytes) -> notify_ret() when
      Log :: log(),
      Bytes :: iodata().
balog(Log, Bytes) ->
    notify(Log, {alog, external, [ensure_binary(Bytes)]}).

-spec balog_terms(Log, ByteList) -> notify_ret() when
      Log :: log(),
      ByteList :: [iodata()].
balog_terms(Log, Bytess) ->
    Bs = ensure_binary_list(Bytess),
    notify(Log, {alog, external, Bs}).

-type close_error_rsn() ::'no_such_log' | 'nonode'
                         | {'file_error', file:filename(), file_error()}.

-spec close(Log) -> 'ok' | {'error', close_error_rsn()} when
      Log :: log().
close(Log) -> 
    req(Log, close).

-type lclose_error_rsn() :: 'no_such_log'
                          | {'file_error', file:filename(), file_error()}.

-spec lclose(Log) -> 'ok' | {'error', lclose_error_rsn()} when
      Log :: log().
lclose(Log) ->
    lclose(Log, node()).

-spec lclose(Log, Node) -> 'ok' | {'error', lclose_error_rsn()} when
      Log :: log(),
      Node :: node().
lclose(Log, Node) when node() =:= Node ->
    req(Log, close);
lclose(_Log, _Node) ->
    {error, no_such_log}.

-type trunc_error_rsn() :: 'no_such_log' | 'nonode'
                         | {'read_only_mode', log()}
                         | {'blocked_log', log()}
                         | {'invalid_header', invalid_header()}
                         | {'file_error', file:filename(), file_error()}.

-spec truncate(Log) -> 'ok' | {'error', trunc_error_rsn()} when
      Log :: log().
truncate(Log) -> 
    req(Log, {truncate, none, truncate, 1}).

-spec truncate(Log, Head) -> 'ok' | {'error', trunc_error_rsn()} when
      Log :: log(),
      Head :: term().
truncate(Log, Head) ->
    req(Log, {truncate, {ok, term_to_binary(Head)}, truncate, 2}).

-spec btruncate(Log, BHead) -> 'ok' | {'error', trunc_error_rsn()} when
      Log :: log(),
      BHead :: iodata().
btruncate(Log, Head) ->
    req(Log, {truncate, {ok, ensure_binary(Head)}, btruncate, 2}).

-type reopen_error_rsn() :: no_such_log
                          | nonode
                          | {read_only_mode, log()}
                          | {blocked_log, log()}
                          | {same_file_name, log()} |
                            {invalid_index_file, file:filename()}
                          | {invalid_header, invalid_header()}
                          | {'file_error', file:filename(), file_error()}.

-spec reopen(Log, File) -> 'ok' | {'error', reopen_error_rsn()} when
      Log :: log(),
      File :: file:filename().
reopen(Log, NewFile) ->
    req(Log, {reopen, NewFile, none, reopen, 2}).

-spec reopen(Log, File, Head) -> 'ok' | {'error', reopen_error_rsn()} when
      Log :: log(),
      File :: file:filename(),
      Head :: term().
reopen(Log, NewFile, NewHead) ->
    req(Log, {reopen, NewFile, {ok, term_to_binary(NewHead)}, reopen, 3}).

-spec breopen(Log, File, BHead) -> 'ok' | {'error', reopen_error_rsn()} when
      Log :: log(),
      File :: file:filename(),
      BHead :: iodata().
breopen(Log, NewFile, NewHead) ->
    req(Log, {reopen, NewFile, {ok, ensure_binary(NewHead)}, breopen, 3}).

-type inc_wrap_error_rsn() :: 'no_such_log' | 'nonode'
                            | {'read_only_mode', log()}
                            | {'blocked_log', log()} | {'halt_log', log()}
                            | {'invalid_header', invalid_header()}
                            | {'file_error', file:filename(), file_error()}.

-spec inc_wrap_file(Log) -> 'ok' | {'error', inc_wrap_error_rsn()} when
      Log :: log().
inc_wrap_file(Log) -> 
    req(Log, inc_wrap_file).

-spec change_size(Log, Size) -> 'ok' | {'error', Reason} when
      Log :: log(),
      Size :: dlog_size(),
      Reason :: no_such_log | nonode | {read_only_mode, Log}
              | {blocked_log, Log}
              | {new_size_too_small, Log, CurrentSize :: pos_integer()}
              | {badarg, size}
              | {file_error, file:filename(), file_error()}.
change_size(Log, NewSize) -> 
    req(Log, {change_size, NewSize}).

-spec change_notify(Log, Owner, Notify) -> 'ok' | {'error', Reason} when
      Log :: log(),
      Owner :: pid(),
      Notify :: boolean(),
      Reason :: no_such_log | nonode | {blocked_log, Log}
              | {badarg, notify} | {not_owner, Owner}.
change_notify(Log, Pid, NewNotify) -> 
    req(Log, {change_notify, Pid, NewNotify}).

-spec change_header(Log, Header) -> 'ok' | {'error', Reason} when
      Log :: log(),
      Header :: {head, dlog_head_opt()}
              | {head_func, MFA :: {atom(), atom(), list()}},
      Reason :: no_such_log | nonode | {read_only_mode, Log}
              | {blocked_log, Log} | {badarg, head}.
change_header(Log, NewHead) ->
    req(Log, {change_header, NewHead}).

-type sync_error_rsn() :: 'no_such_log' | 'nonode' | {'read_only_mode', log()}
                        | {'blocked_log', log()}
                        | {'file_error', file:filename(), file_error()}.

-spec sync(Log) -> 'ok' | {'error', sync_error_rsn()} when
      Log :: log().
sync(Log) -> 
    req(Log, sync).

-type block_error_rsn() :: 'no_such_log' | 'nonode' | {'blocked_log', log()}.

-spec block(Log) -> 'ok' | {'error', block_error_rsn()} when
      Log :: log().
block(Log) -> 
    block(Log, true).

-spec block(Log, QueueLogRecords) -> 'ok' | {'error', block_error_rsn()} when
      Log :: log(),
      QueueLogRecords :: boolean().
block(Log, QueueLogRecords) -> 
    req(Log, {block, QueueLogRecords}).

-type unblock_error_rsn() :: 'no_such_log' | 'nonode'
                           | {'not_blocked', log()}
                           | {'not_blocked_by_pid', log()}.

-spec unblock(Log) -> 'ok' | {'error', unblock_error_rsn()} when
      Log :: log().
unblock(Log) ->
    req(Log, unblock).

-spec format_error(Error) -> io_lib:chars() when
      Error :: term().
format_error(Error) ->
    do_format_error(Error).

-type dlog_info() :: {name, Log :: log()}
                   | {file, File :: file:filename()}
                   | {type, Type :: dlog_type()}
                   | {format, Format :: dlog_format()}
                   | {size, Size :: dlog_size()}
                   | {mode, Mode :: dlog_mode()}
                   | {owners, [{pid(), Notify :: boolean()}]}
                   | {users, Users :: non_neg_integer()}
                   | {status, Status ::
                        ok | {blocked, QueueLogRecords :: boolean()}}
                   | {node, Node :: node()}
                   | {head, Head :: none
                                  | {head, binary()}
                                  | (MFA :: {atom(), atom(), list()})}
                   | {no_written_items, NoWrittenItems ::non_neg_integer()}
                   | {full, Full :: boolean}
                   | {no_current_bytes, non_neg_integer()}
                   | {no_current_items, non_neg_integer()}
                   | {no_items, non_neg_integer()}
                   | {current_file, pos_integer()}
                   | {no_overflows, {SinceLogWasOpened :: non_neg_integer(),
                                     SinceLastInfo :: non_neg_integer()}}.
-spec info(Log) -> InfoList | {'error', no_such_log} when
      Log :: log(),
      InfoList :: [dlog_info()].
info(Log) -> 
    req(Log, info).

-spec pid2name(Pid) -> {'ok', Log} | 'undefined' when
      Pid :: pid(),
      Log :: log().
pid2name(Pid) ->
    disk_log_server:start(),
    case ets:lookup(?DISK_LOG_PID_TABLE, Pid) of
        [] -> undefined;
        [{_Pid, Log}] -> {ok, Log}
    end.

%% This function Takes 3 args, a Log, a Continuation and N.
%% It retuns a {Cont2, ObjList} | eof | {error, Reason}
%% The initial continuation is the atom 'start'

-type chunk_error_rsn() :: no_such_log
                         | {format_external, log()}
                         | {blocked_log, log()}
                         | {badarg, continuation}
                         | {not_internal_wrap, log()}
                         | {corrupt_log_file, FileName :: file:filename()}
                         | {file_error, file:filename(), file_error()}.

-type chunk_ret() :: {Continuation2 :: continuation(), Terms :: [term()]}
                   | {Continuation2 :: continuation(),
                      Terms :: [term()],
                      Badbytes :: non_neg_integer()}
                   | eof
                   | {error, Reason :: chunk_error_rsn()}.

-spec chunk(Log, Continuation) -> chunk_ret() when
      Log :: log(),
      Continuation :: start | continuation().
chunk(Log, Cont) ->
    chunk(Log, Cont, infinity).

-spec chunk(Log, Continuation, N) -> chunk_ret() when
      Log :: log(),
      Continuation :: start | continuation(),
      N :: pos_integer() | infinity.
chunk(Log, Cont, infinity) ->
    %% There cannot be more than ?MAX_CHUNK_SIZE terms in a chunk.
    ichunk(Log, Cont, ?MAX_CHUNK_SIZE);
chunk(Log, Cont, N) when is_integer(N), N > 0 ->
    ichunk(Log, Cont, N).

ichunk(Log, start, N) ->
    R = req(Log, {chunk, 0, [], N}),
    ichunk_end(R, Log);
ichunk(Log, More, N) when is_record(More, continuation) ->
    R = req2(More#continuation.pid, 
	     {chunk, More#continuation.pos, More#continuation.b, N}),
    ichunk_end(R, Log);
ichunk(_Log, _, _) ->
    {error, {badarg, continuation}}.

ichunk_end({C, R}, Log) when is_record(C, continuation) ->
    ichunk_end(R, read_write, Log, C, 0);
ichunk_end({C, R, Bad}, Log) when is_record(C, continuation) ->
    ichunk_end(R, read_only, Log, C, Bad);
ichunk_end(R, _Log) ->
    R.

%% Create the terms on the client's heap, not the server's.
%% The list of binaries is reversed.
ichunk_end(R, Mode, Log, C, Bad) ->
    case catch bins2terms(R, []) of
	{'EXIT', _} ->
	    RR = lists:reverse(R),
	    ichunk_bad_end(RR, Mode, Log, C, Bad, []);
	Ts when Bad > 0 ->
	    {C, Ts, Bad};
	Ts when Bad =:= 0 ->
	    {C, Ts}
    end.

bins2terms([], L) ->
    L;
bins2terms([B | Bs], L) ->
    bins2terms(Bs, [binary_to_term(B) | L]).

ichunk_bad_end([B | Bs], Mode, Log, C, Bad, A) ->
    case catch binary_to_term(B) of
	{'EXIT', _} when read_write =:= Mode ->
	    InfoList = info(Log),
	    {value, {file, FileName}} = lists:keysearch(file, 1, InfoList),
            File = case C#continuation.pos of
		       Pos when is_integer(Pos) -> FileName; % halt log 
		       {FileNo, _} -> add_ext(FileName, FileNo) % wrap log
		   end,
	    {error, {corrupt_log_file, File}};
	{'EXIT', _} when read_only =:= Mode ->
	    Reread = lists:foldl(fun(Bin, Sz) -> 
                                         Sz + byte_size(Bin) + ?HEADERSZ 
                                 end, 0, Bs),
	    NewPos = case C#continuation.pos of
			 Pos when is_integer(Pos) -> Pos-Reread;
			 {FileNo, Pos} -> {FileNo, Pos-Reread}
		     end,
	    NewBad = Bad + byte_size(B),
	    {C#continuation{pos = NewPos, b = []}, lists:reverse(A), NewBad};
	T ->
	    ichunk_bad_end(Bs, Mode, Log, C, Bad, [T | A])
    end.

-type bchunk_ret() :: {Continuation2 :: continuation(),
                       Binaries :: [binary()]}
                    | {Continuation2 :: continuation(),
                       Binaries :: [binary()],
                       Badbytes :: non_neg_integer()}
                    | eof
                    | {error, Reason :: chunk_error_rsn()}.

-spec bchunk(Log, Continuation) -> bchunk_ret() when
      Log :: log(),
      Continuation :: start | continuation().
bchunk(Log, Cont) ->
    bchunk(Log, Cont, infinity).

-spec bchunk(Log, Continuation, N) -> bchunk_ret() when
      Log :: log(),
      Continuation :: start | continuation(),
      N :: pos_integer() | infinity.
bchunk(Log, Cont, infinity) ->
    %% There cannot be more than ?MAX_CHUNK_SIZE terms in a chunk.
    bichunk(Log, Cont, ?MAX_CHUNK_SIZE);
bchunk(Log, Cont, N) when is_integer(N), N > 0 ->
    bichunk(Log, Cont, N).

bichunk(Log, start, N) ->
    R = req(Log, {chunk, 0, [], N}),
    bichunk_end(R);
bichunk(_Log, #continuation{pid = Pid, pos = Pos, b = B}, N) ->
    R = req2(Pid, {chunk, Pos, B, N}),
    bichunk_end(R);
bichunk(_Log, _, _) ->
    {error, {badarg, continuation}}.

bichunk_end({C = #continuation{}, R}) ->
    {C, lists:reverse(R)};
bichunk_end({C = #continuation{}, R, Bad}) ->
    {C, lists:reverse(R), Bad};
bichunk_end(R) ->
    R.

-spec chunk_step(Log, Continuation, Step) ->
                        {'ok', any()} | {'error', Reason} when
      Log :: log(),
      Continuation :: start | continuation(),
      Step :: integer(),
      Reason :: no_such_log | end_of_log | {format_external, Log}
              | {blocked_log, Log}  | {badarg, continuation}
              | {file_error, file:filename(), file_error()}.
chunk_step(Log, Cont, N) when is_integer(N) ->
    ichunk_step(Log, Cont, N).

ichunk_step(Log, start, N) ->
    req(Log, {chunk_step, 0, N});
ichunk_step(_Log, More, N) when is_record(More, continuation) ->
    req2(More#continuation.pid, {chunk_step, More#continuation.pos, N});
ichunk_step(_Log, _, _) ->
    {error, {badarg, continuation}}.

-spec chunk_info(Continuation) -> InfoList | {error, Reason} when
      Continuation :: continuation(),
      InfoList :: [{node, Node :: node()}, ...],
      Reason :: {no_continuation, Continuation}.
chunk_info(More = #continuation{}) ->
   [{node, node(More#continuation.pid)}];
chunk_info(BadCont) ->
   {error, {no_continuation, BadCont}}.

-spec accessible_logs() -> {[Log], []} when
      Log :: log().
accessible_logs() ->
    {disk_log_server:all(), []}.

-spec all() -> [Log] when
      Log :: log().
all() ->
    disk_log_server:all().

istart_link(Server) ->  
    {ok, proc_lib:spawn_link(disk_log, init, [self(), Server])}.

%% Only for backwards compatibility, could probably be removed.
-spec start() -> 'ok'.
start() ->
    disk_log_server:start().

internal_open(Pid, A) ->
    req2(Pid, {internal_open, A}).

%%% ll_open() and ll_close() are used by disk_log_h.erl, a module not
%%% (yet) in Erlang/OTP.

%% -spec ll_open(dlog_options()) -> {'ok', Res :: _, #log{}, Cnt :: _} | Error.
ll_open(A) ->
    case check_arg(A, #arg{options = A}) of
	{ok, L} -> do_open(L);
	Error -> Error
    end.

%% -> closed | throw(Error)
ll_close(Log) ->
    close_disk_log2(Log).

check_arg([], Res) ->
    Ret = case Res#arg.head of
	      none ->
		  {ok, Res};
	      _ ->
		  case check_head(Res#arg.head, Res#arg.format) of
		      {ok, Head} ->
			  {ok, Res#arg{head = Head}};
		      Error ->
			  Error
		  end
	  end,

    if  %% check result
	Res#arg.name =:= 0 -> 
	    {error, {badarg, name}};
	Res#arg.file =:= none ->
	    case catch lists:concat([Res#arg.name, ".LOG"]) of
		{'EXIT',_} -> {error, {badarg, file}};
		FName ->  check_arg([], Res#arg{file = FName})
	    end;
	Res#arg.repair =:= truncate, Res#arg.mode =:= read_only ->
	    {error, {badarg, repair_read_only}};
	Res#arg.type =:= halt, is_tuple(Res#arg.size) ->
	    {error, {badarg, size}};
	Res#arg.type =:= wrap ->
	    {OldSize, Version} = 
		disk_log_1:read_size_file_version(Res#arg.file),
	    check_wrap_arg(Ret, OldSize, Version);
	true ->
	    Ret
    end;
check_arg([{file, F} | Tail], Res) when is_list(F) ->
    check_arg(Tail, Res#arg{file = F});
check_arg([{file, F} | Tail], Res) when is_atom(F) ->
    check_arg(Tail, Res#arg{file = F});
check_arg([{linkto, Pid} |Tail], Res) when is_pid(Pid) ->
    check_arg(Tail, Res#arg{linkto = Pid});
check_arg([{linkto, none} |Tail], Res) ->
    check_arg(Tail, Res#arg{linkto = none});
check_arg([{name, Name}|Tail], Res) ->
    check_arg(Tail, Res#arg{name = Name});
check_arg([{repair, true}|Tail], Res) ->
    check_arg(Tail, Res#arg{repair = true});
check_arg([{repair, false}|Tail], Res) ->
    check_arg(Tail, Res#arg{repair = false});
check_arg([{repair, truncate}|Tail], Res) ->
    check_arg(Tail, Res#arg{repair = truncate});
check_arg([{size, Int}|Tail], Res) when is_integer(Int), Int > 0 ->
    check_arg(Tail, Res#arg{size = Int});
check_arg([{size, infinity}|Tail], Res) ->
    check_arg(Tail, Res#arg{size = infinity});
check_arg([{size, {MaxB,MaxF}}|Tail], Res) when is_integer(MaxB), 
                                                is_integer(MaxF),
						MaxB > 0, MaxB =< ?MAX_BYTES,
						MaxF > 0, MaxF < ?MAX_FILES ->
    check_arg(Tail, Res#arg{size = {MaxB, MaxF}});
check_arg([{type, wrap}|Tail], Res) ->
    check_arg(Tail, Res#arg{type = wrap});
check_arg([{type, halt}|Tail], Res) ->
    check_arg(Tail, Res#arg{type = halt});
check_arg([{format, internal}|Tail], Res) ->
    check_arg(Tail, Res#arg{format = internal});
check_arg([{format, external}|Tail], Res) ->
    check_arg(Tail, Res#arg{format = external});
check_arg([{notify, true}|Tail], Res) ->
    check_arg(Tail, Res#arg{notify = true});
check_arg([{notify, false}|Tail], Res) ->
    check_arg(Tail, Res#arg{notify = false});
check_arg([{head_func, HeadFunc}|Tail], Res)  ->
    check_arg(Tail, Res#arg{head = {head_func, HeadFunc}});
check_arg([{head, Term}|Tail], Res) ->
    check_arg(Tail, Res#arg{head = {head, Term}});
check_arg([{mode, read_only}|Tail], Res) ->
    check_arg(Tail, Res#arg{mode = read_only});
check_arg([{mode, read_write}|Tail], Res) ->
    check_arg(Tail, Res#arg{mode = read_write});
check_arg([{quiet, Boolean}|Tail], Res) when is_boolean(Boolean) ->
    check_arg(Tail, Res#arg{quiet = Boolean});
check_arg(Arg, _) ->
    {error, {badarg, Arg}}.

check_wrap_arg({ok, Res}, {0,0}, _Version) when Res#arg.size =:= infinity ->
    {error, {badarg, size}};
check_wrap_arg({ok, Res}, OldSize, Version) when Res#arg.size =:= infinity ->
    NewRes = Res#arg{size = OldSize},
    check_wrap_arg({ok, NewRes}, OldSize, Version);
check_wrap_arg({ok, Res}, {0,0}, Version) ->
    {ok, Res#arg{version = Version}};
check_wrap_arg({ok, Res}, OldSize, Version) when OldSize =:= Res#arg.size ->
    {ok, Res#arg{version = Version}};
check_wrap_arg({ok, Res}, _OldSize, Version) when Res#arg.repair =:= truncate,
						  is_tuple(Res#arg.size) ->
    {ok, Res#arg{version = Version}};
check_wrap_arg({ok, Res}, OldSize, _Version) when is_tuple(Res#arg.size) ->
    {error, {size_mismatch, OldSize, Res#arg.size}};
check_wrap_arg({ok, _Res}, _OldSize, _Version) ->
    {error, {badarg, size}};
check_wrap_arg(Ret, _OldSize, _Version) ->
    Ret.

%%%-----------------------------------------------------------------
%%% Server functions
%%%-----------------------------------------------------------------
init(Parent, Server) ->
    ?PROFILE(ep:do()),
    process_flag(trap_exit, true),
    loop(#state{parent = Parent, server = Server}).

loop(#state{messages = []}=State) ->
    receive
	Message ->
	    handle(Message, State)
    end;
loop(#state{messages = [M | Ms]}=State) ->
    handle(M, State#state{messages = Ms}).

handle({From, write_cache}, S) when From =:= self() ->
    case catch do_write_cache(get(log)) of
        ok ->
            loop(S);
        Error ->
	    loop(S#state{cache_error = Error})
    end;
handle({From, {log, Format, B}}=Message, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{status = ok, format=external}=L when Format =:= internal ->
	    reply(From, {error, {format_external, L#log.name}}, S);
	#log{status = ok, format=LogFormat} ->
	    log_loop(S, From, [B], [], iolist_size(B), LogFormat);
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({alog, Format, B}=Message, S) ->
    case get(log) of
	#log{mode = read_only} ->
	    notify_owners({read_only,B}),
	    loop(S);
	#log{status = ok, format = external} when Format =:= internal ->
	    notify_owners({format_external, B}),
	    loop(S);
	#log{status = ok, format=LogFormat} ->
	    log_loop(S, [], [B], [], iolist_size(B), LogFormat);
	#log{status = {blocked, false}} ->
	    notify_owners({blocked_log, B}),
	    loop(S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {block, QueueLogRecs}}=Message, S) ->
    case get(log) of
	#log{status = ok}=L ->
	    do_block(From, QueueLogRecs, L),
	    reply(From, ok, S);
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, unblock}, S) ->
    case get(log) of
	#log{status = ok}=L ->
	    reply(From, {error, {not_blocked, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    S2 = do_unblock(L, S),
	    reply(From, ok, S2);
	L ->
	    reply(From, {error, {not_blocked_by_pid, L#log.name}}, S)
    end;
handle({From, sync}=Message, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{status = ok, format=LogFormat} ->
            log_loop(S, [], [], [From], 0, LogFormat);
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {truncate, Head, F, A}}=Message, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{status = ok} when S#state.cache_error =/= ok ->
	    loop(cache_error(S, [From]));
	#log{status = ok}=L ->
	    H = merge_head(Head, L#log.head),
	    case catch do_trunc(L, H) of
		ok ->
		    erase(is_full),
		    notify_owners({truncated, S#state.cnt}),
		    N = if H =:= none -> 0; true -> 1 end,
		    reply(From, ok, (state_ok(S))#state{cnt = N});
		Error ->
		    do_exit(S, From, Error, ?failure(Error, F, A))
	    end;
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {chunk, Pos, B, N}}=Message,  S) ->
    case get(log) of
	#log{status = ok} when S#state.cache_error =/= ok ->
	    loop(cache_error(S, [From]));
	#log{status = ok}=L ->
	    R = do_chunk(L, Pos, B, N),
	    reply(From, R, S);
	#log{blocked_by = From}=L ->
	    R = do_chunk(L, Pos, B, N),
	    reply(From, R, S);
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_L ->
	    enqueue(Message, S)
    end;
handle({From, {chunk_step, Pos, N}}=Message,  S) ->
    case get(log) of
	#log{status = ok} when S#state.cache_error =/= ok ->
	    loop(cache_error(S, [From]));
	#log{status = ok}=L ->
	    R = do_chunk_step(L, Pos, N),
	    reply(From, R, S);
	#log{blocked_by = From}=L ->
	    R = do_chunk_step(L, Pos, N),
	    reply(From, R, S);
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {change_notify, Pid, NewNotify}}=Message, S) ->
    case get(log) of
	#log{status = ok}=L ->
	    case do_change_notify(L, Pid, NewNotify) of
		{ok, L1} ->
		    put(log, L1),
		    reply(From, ok, S);
		Error ->
		    reply(From, Error, S)
	    end;
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {change_header, NewHead}}=Message, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{status = ok, format = Format}=L ->
	    case check_head(NewHead, Format) of
		{ok, Head} ->
		    put(log, L#log{head = mk_head(Head, Format)}),
		    reply(From, ok, S);
		Error ->
		    reply(From, Error, S)
	    end;
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {change_size, NewSize}}=Message, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{status = ok}=L ->
	    case check_size(L#log.type, NewSize) of
		ok ->
		    case catch do_change_size(L, NewSize) of % does the put
			ok ->
			    reply(From, ok, S);
			{big, CurSize} ->
			    reply(From, 
				  {error, 
				   {new_size_too_small, L#log.name, CurSize}},
				  S);
			Else ->
			    reply(From, Else, state_err(S, Else))
		    end;
		not_ok ->
		    reply(From, {error, {badarg, size}}, S)
	    end;
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, inc_wrap_file}=Message, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{type = halt}=L ->
	    reply(From, {error, {halt_log, L#log.name}}, S);
	#log{status = ok} when S#state.cache_error =/= ok ->
	    loop(cache_error(S, [From]));
	#log{status = ok}=L ->
	    case catch do_inc_wrap_file(L) of
		{ok, L2, Lost} ->
		    put(log, L2),
		    notify_owners({wrap, Lost}),
		    reply(From, ok, S#state{cnt = S#state.cnt-Lost});
		{error, Error, L2} ->
		    put(log, L2),		    
		    reply(From, Error, state_err(S, Error))
	    end;
	#log{status = {blocked, false}}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	#log{blocked_by = From}=L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S);
	_ ->
	    enqueue(Message, S)
    end;
handle({From, {reopen, NewFile, Head, F, A}}, S) ->
    case get(log) of
	#log{mode = read_only}=L ->
	    reply(From, {error, {read_only_mode, L#log.name}}, S);
	#log{status = ok} when S#state.cache_error =/= ok ->
	    loop(cache_error(S, [From]));
	#log{status = ok, filename = NewFile}=L ->
	    reply(From, {error, {same_file_name, L#log.name}}, S);
	#log{status = ok}=L ->
	    case catch close_disk_log2(L) of
		closed ->
		    File = L#log.filename,
		    case catch rename_file(File, NewFile, L#log.type) of
			ok ->
			    H = merge_head(Head, L#log.head),
			    case do_open((S#state.args)#arg{name = L#log.name,
							    repair = truncate,
							    head = H,
							    file = File}) of
				{ok, Res, L2, Cnt} ->
				    put(log, L2#log{owners = L#log.owners,
						    head = L#log.head,
						    users = L#log.users}),
				    notify_owners({truncated, S#state.cnt}),
				    erase(is_full),
				    case Res of
					{error, _} ->
					    do_exit(S, From, Res, 
						    ?failure(Res, F, A));
					_ ->
					    reply(From, ok, S#state{cnt = Cnt})
				    end;
				Res ->
				    do_exit(S, From, Res, ?failure(Res, F, A))
			    end;
			Error ->
			    do_exit(S, From, Error, ?failure(Error, reopen, 2))
		    end;
		Error ->
		    do_exit(S, From, Error, ?failure(Error, F, A))
	    end;
	L ->
	    reply(From, {error, {blocked_log, L#log.name}}, S)
    end;
handle({Server, {internal_open, A}}, S) ->
    case get(log) of
	undefined ->
	    case do_open(A) of % does the put
		{ok, Res, L, Cnt} ->
		    put(log, opening_pid(A#arg.linkto, A#arg.notify, L)),
		    reply(Server, Res, S#state{args=A, cnt=Cnt});
		Res ->
		    do_fast_exit(S, Server, Res)
	    end;
	L ->
	    TestH = mk_head(A#arg.head, A#arg.format),
	    case compare_arg(A#arg.options, S#state.args, TestH, L#log.head) of
		ok ->
		    case add_pid(A#arg.linkto, A#arg.notify, L) of
			{ok, L1} ->
			    put(log, L1),
			    reply(Server, {ok, L#log.name}, S);
			Error ->
			    reply(Server, Error, S)
		    end;
		Error ->
		    reply(Server, Error, S)
	    end
    end;
handle({From, close}, S) ->
    case do_close(From, S) of
	{stop, S1} ->
	    do_exit(S1, From, ok, normal);
	{continue, S1} ->
	    reply(From, ok, S1)
    end;
handle({From, info}, S) ->
    reply(From, do_info(get(log), S#state.cnt), S);
handle({'EXIT', From, Reason}, #state{parent=From}=S) ->
    %% Parent orders shutdown.
    _ = do_stop(S),
    exit(Reason);
handle({'EXIT', From, Reason}, #state{server=From}=S) ->
    %% The server is gone.
    _ = do_stop(S),
    exit(Reason);
handle({'EXIT', From, _Reason}, S) ->
    L = get(log),
    case is_owner(From, L) of
	{true, _Notify} ->
	    case close_owner(From, L, S) of
		{stop, S1} ->
		    _ = do_stop(S1),
		    exit(normal);
		{continue, S1} ->
		    loop(S1)
	    end;
	false ->
	    %% 'users' is not decremented.
	    S1 = do_unblock(From, get(log), S),
	    loop(S1)
    end;
handle({system, From, Req}, S) ->
    sys:handle_system_msg(Req, From, S#state.parent, ?MODULE, [], S);
handle(_, S) ->
    loop(S).

enqueue(Message, #state{queue = Queue}=S) ->
    loop(S#state{queue = [Message | Queue]}).

%% Collect further log and sync requests already in the mailbox or queued

-define(MAX_LOOK_AHEAD, 64*1024).

%% Inlined.
log_loop(#state{cache_error = CE}=S, Pids, _Bins, _Sync, _Sz, _F) when CE =/= ok ->
    loop(cache_error(S, Pids));
log_loop(#state{}=S, Pids, Bins, Sync, Sz, _F) when Sz > ?MAX_LOOK_AHEAD ->
    loop(log_end(S, Pids, Bins, Sync, Sz));
log_loop(#state{messages = []}=S, Pids, Bins, Sync, Sz, F) ->
    receive
	Message ->
            log_loop(Message, Pids, Bins, Sync, Sz, F, S)
    after 0 ->
	    loop(log_end(S, Pids, Bins, Sync, Sz))
    end;
log_loop(#state{messages = [M | Ms]}=S, Pids, Bins, Sync, Sz, F) ->
    S1 = S#state{messages = Ms},
    log_loop(M, Pids, Bins, Sync, Sz, F, S1).

%% Items logged after the last sync request found are sync:ed as well.
log_loop({alog, internal, B}, Pids, Bins, Sync, Sz, internal=F, S) ->
    %% alog of terms allowed for the internal format only
    log_loop(S, Pids, [B | Bins], Sync, Sz+iolist_size(B), F);
log_loop({alog, binary, B}, Pids, Bins, Sync, Sz, F, S) ->
    log_loop(S, Pids, [B | Bins], Sync, Sz+iolist_size(B), F);
log_loop({From, {log, internal, B}}, Pids, Bins, Sync, Sz, internal=F, S) ->
    %% log of terms allowed for the internal format only
    log_loop(S, [From | Pids], [B | Bins], Sync, Sz+iolist_size(B), F);
log_loop({From, {log, binary, B}}, Pids, Bins, Sync, Sz, F, S) ->
    log_loop(S, [From | Pids], [B | Bins], Sync, Sz+iolist_size(B), F);
log_loop({From, sync}, Pids, Bins, Sync, Sz, F, S) ->
    log_loop(S, Pids, Bins, [From | Sync], Sz, F);
log_loop(Message, Pids, Bins, Sync, Sz, _F, S) ->
    NS = log_end(S, Pids, Bins, Sync, Sz),
    handle(Message, NS).

log_end(S, [], [], Sync, _Sz) ->
    log_end_sync(S, Sync);
log_end(#state{cnt = Cnt}=S, Pids, Bins, Sync, Sz) ->
    case do_log(get(log), rflat(Bins), Sz) of
	N when is_integer(N) ->
	    ok = replies(Pids, ok),
	    S1 = (state_ok(S))#state{cnt = Cnt + N},
	    log_end_sync(S1, Sync);
        {error, {error, {full, _Name}}, N} when Pids =:= [] ->
            log_end_sync(state_ok(S#state{cnt = Cnt + N}), Sync);
	{error, Error, N} ->
	    ok = replies(Pids, Error),
	    state_err(S#state{cnt = Cnt + N}, Error)
    end.

%% Inlined.
log_end_sync(S, []) ->
    S;
log_end_sync(S, Sync) ->
    Res = do_sync(get(log)),
    ok = replies(Sync, Res),
    state_err(S, Res).

%% Inlined.
rflat([B]) -> B;
rflat(B) -> rflat(B, []).

rflat([B | Bs], L) ->
    rflat(Bs, B ++ L);
rflat([], L) -> L.

%% -> {ok, Log} | {error, Error}
do_change_notify(L, Pid, Notify) ->
    case is_owner(Pid, L) of
	{true, Notify} ->
	    {ok, L};
	{true, _OldNotify} when Notify =/= true, Notify =/= false ->
	    {error, {badarg, notify}};
	{true, _OldNotify} ->
	    Owners = lists:keydelete(Pid, 1, L#log.owners),
	    L1 = L#log{owners = [{Pid, Notify} | Owners]},
	    {ok, L1};
	false ->
	    {error, {not_owner, Pid}}
    end.

%% -> {stop, S} | {continue, S}
do_close(Pid, S) ->
    L = get(log),
    case is_owner(Pid, L) of
	{true, _Notify} ->
	    close_owner(Pid, L, S);
	false ->
	    close_user(Pid, L, S)
    end.

%% -> {stop, S} | {continue, S}
close_owner(Pid, L, S) ->
    L1 = L#log{owners = lists:keydelete(Pid, 1, L#log.owners)},
    put(log, L1),
    S2 = do_unblock(Pid, get(log), S),
    unlink(Pid),
    do_close2(L1, S2).

%% -> {stop, S} | {continue, S}
close_user(Pid, #log{users=Users}=L, S) when Users > 0 ->
    L1 = L#log{users = Users - 1},
    put(log, L1),
    S2 = do_unblock(Pid, get(log), S),
    do_close2(L1, S2);
close_user(_Pid, _L, S) ->
    {continue, S}.

do_close2(#log{users = 0, owners = []}, S) ->
    {stop, S};
do_close2(_L, S) ->
    {continue, S}.

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(_Parent, _, State) ->
    loop(State).

-spec system_terminate(_, _, _, #state{}) -> no_return().
system_terminate(Reason, _Parent, _, State) ->
    _ = do_stop(State),
    exit(Reason).

%%-----------------------------------------------------------------
%% Temporay code for upgrade.
%%-----------------------------------------------------------------
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec do_exit(#state{}, pid(), _, _) -> no_return().
do_exit(S, From, Message0, Reason) ->
    R = do_stop(S),
    Message = case S#state.cache_error of
		  Err when Err =/= ok -> Err;
		  _ when R =:= closed -> Message0;
		  _ when Message0 =:= ok -> R;
		  _ -> Message0
	      end,
    _ = disk_log_server:close(self()),
    ok = replies(From, Message),
    ?PROFILE(ep:done()),
    exit(Reason).

-spec do_fast_exit(#state{}, pid(), _) -> no_return().
do_fast_exit(S, Server, Message) ->
    _ = do_stop(S),
    Server ! {disk_log, self(), Message},
    exit(normal).

%% -> closed | Error
do_stop(S) ->
    proc_q(S#state.queue ++ S#state.messages),
    close_disk_log(get(log)).

proc_q([{From, _R}|Tail]) when is_pid(From) ->
    From ! {disk_log, self(), {error, disk_log_stopped}},
    proc_q(Tail);
proc_q([_|T]) -> %% async stuff 
    proc_q(T);
proc_q([]) ->
    ok.

%% -> log()
opening_pid(Pid, Notify, L) ->
    {ok, L1} = add_pid(Pid, Notify, L),
    L1.

%% -> {ok, log()} | Error
add_pid(Pid, Notify, L) when is_pid(Pid) ->
    case is_owner(Pid, L) of
	false ->
            link(Pid),
	    {ok, L#log{owners = [{Pid, Notify} | L#log.owners]}};
	{true, Notify}  ->
%%	    {error, {pid_already_connected, L#log.name}};
	    {ok, L};
	{true, CurNotify} when Notify =/= CurNotify ->
	    {error, {arg_mismatch, notify, CurNotify, Notify}}
    end;
add_pid(_NotAPid, _Notify, L) ->
    {ok, L#log{users = L#log.users + 1}}.

unblock_pid(#log{blocked_by = none}) ->
    ok;
unblock_pid(#log{blocked_by = Pid}=L) ->
    case is_owner(Pid, L) of
	{true, _Notify} ->
	    ok;
	false ->
	    unlink(Pid)
    end.

%% -> true | false
is_owner(Pid, L) ->
    case lists:keysearch(Pid, 1, L#log.owners) of
	{value, {_Pid, Notify}} ->
	    {true, Notify};
	false ->
	    false
    end.

%% ok | throw(Error)
rename_file(File, NewFile, halt) ->
    case file:rename(File, NewFile) of
        ok ->
            ok;
        Else ->
            file_error(NewFile, Else)
    end;
rename_file(File, NewFile, wrap) ->
    rename_file(wrap_file_extensions(File), File, NewFile, ok).

rename_file([Ext|Exts], File, NewFile0, Res) ->
    NewFile = add_ext(NewFile0, Ext),
    NRes = case file:rename(add_ext(File, Ext), NewFile) of
	       ok ->
		   Res;
	       Else ->
		   file_error(NewFile, Else)
	   end,
    rename_file(Exts, File, NewFile0, NRes);
rename_file([], _File, _NewFiles, Res) -> Res.

file_error(FileName, {error, Error}) ->
    {error, {file_error, FileName, Error}}.

%% "Old" error messages have been kept, arg_mismatch has been added.
%%-spec compare_arg(dlog_options(), #arg{}, 
compare_arg([], _A, none, _OrigHead) ->
    % no header option given
    ok;
compare_arg([], _A, Head, OrigHead) when Head =/= OrigHead ->
    {error, {arg_mismatch, head, OrigHead, Head}};
compare_arg([], _A, _Head, _OrigHead) ->
    ok;
compare_arg([{Attr, Val} | Tail], A, Head, OrigHead) ->
    case compare_arg(Attr, Val, A) of
	{not_ok, OrigVal} -> 
	    {error, {arg_mismatch, Attr, OrigVal, Val}};
	ok -> 
	    compare_arg(Tail, A, Head, OrigHead);
	Error -> 
	    Error
    end.

-spec compare_arg(atom(), _, #arg{}) ->
	     'ok' | {'not_ok', _} | {'error', {atom(), _}}.
compare_arg(file, F, A) when F =/= A#arg.file ->
    {error, {name_already_open, A#arg.name}};
compare_arg(mode, read_only, A) when A#arg.mode =:= read_write ->
    {error, {open_read_write, A#arg.name}};
compare_arg(mode, read_write, A) when A#arg.mode =:= read_only ->
    {error, {open_read_only, A#arg.name}};
compare_arg(type, T, A) when T =/= A#arg.type ->
    {not_ok, A#arg.type};
compare_arg(format, F, A) when F =/= A#arg.format ->
    {not_ok, A#arg.format};
compare_arg(repair, R, A) when R =/= A#arg.repair ->
    %% not used, but check it anyway...
    {not_ok, A#arg.repair};
compare_arg(_Attr, _Val, _A) -> 
    ok.

%% -> {ok, Res, log(), Cnt} | Error
do_open(A) ->
    #arg{type = Type, format = Format, name = Name, head = Head0,
         file = FName, repair = Repair, size = Size, mode = Mode,
         quiet = Quiet, version = V} = A,
    disk_log_1:set_quiet(Quiet),
    Head = mk_head(Head0, Format),
    case do_open2(Type, Format, Name, FName, Repair, Size, Mode, Head, V) of
        {ok, Ret, Extra, FormatType, NoItems} ->
            L = #log{name = Name, type = Type, format = Format,
                     filename = FName, size = Size,
                     format_type = FormatType, head = Head, mode = Mode,
                     version = V, extra = Extra},
            {ok, Ret, L, NoItems};
        Error ->
            Error
    end.

mk_head({head, Term}, internal) -> {ok, term_to_binary(Term)};
mk_head({head, Bytes}, external) -> {ok, ensure_binary(Bytes)};
mk_head(H, _) -> H.

terms2bins([T | Ts]) ->
    [term_to_binary(T) | terms2bins(Ts)];
terms2bins([]) ->
    [].

ensure_binary_list(Bs) ->
    ensure_binary_list(Bs, Bs).

ensure_binary_list([B | Bs], Bs0) when is_binary(B) ->
    ensure_binary_list(Bs, Bs0);
ensure_binary_list([], Bs0) ->
    Bs0;
ensure_binary_list(_, Bs0) ->
    make_binary_list(Bs0).

make_binary_list([B | Bs]) ->
    [ensure_binary(B) | make_binary_list(Bs)];
make_binary_list([]) ->
    [].

ensure_binary(Bytes) ->
    iolist_to_binary(Bytes).

%%-----------------------------------------------------------------
%% Change size of the logs in runtime.
%%-----------------------------------------------------------------
%% -> ok | {big, CurSize} | throw(Error)
do_change_size(#log{type = halt}=L, NewSize) ->
    Halt = L#log.extra,
    CurB = Halt#halt.curB,
    NewLog = L#log{extra = Halt#halt{size = NewSize}},
    if
	NewSize =:= infinity ->
	    erase(is_full),
	    put(log, NewLog),
	    ok;
	CurB =< NewSize ->
	    erase(is_full),
	    put(log, NewLog),
	    ok;
	true ->
	    {big, CurB}
    end;
do_change_size(#log{type = wrap}=L, NewSize) ->
    #log{extra = Extra, version = Version} = L,
    {ok, Handle} = disk_log_1:change_size_wrap(Extra, NewSize, Version),
    erase(is_full),
    put(log, L#log{extra = Handle}),
    ok.

%% -> {ok, Head} | Error; Head = none | {head, H} | {M,F,A}
check_head({head, none}, _Format) ->
    {ok, none};
check_head({head_func, {M, F, A}}, _Format) when is_atom(M), 
                                                 is_atom(F), 
                                                 is_list(A) ->
    {ok, {M, F, A}};
check_head({head, Head}, external) ->
    case catch ensure_binary(Head) of
	{'EXIT', _} ->
	    {error, {badarg, head}};
	_ ->
	    {ok, {head, Head}}
    end;
check_head({head, Term}, internal) ->
    {ok, {head, Term}};
check_head(_Head, _Format) ->
    {error, {badarg, head}}.

check_size(wrap, {NewMaxB,NewMaxF}) when
  is_integer(NewMaxB), is_integer(NewMaxF),
  NewMaxB > 0, NewMaxB =< ?MAX_BYTES, NewMaxF > 0, NewMaxF < ?MAX_FILES ->
    ok;
check_size(halt, NewSize) when is_integer(NewSize), NewSize > 0 ->
    ok;
check_size(halt, infinity) ->
    ok;
check_size(_, _) ->
    not_ok.

%%-----------------------------------------------------------------
%% Increment a wrap log.
%%-----------------------------------------------------------------
%% -> {ok, log(), Lost} | {error, Error, log()}
do_inc_wrap_file(L) ->
    #log{format = Format, extra = Handle} = L,
    case Format of
	internal ->
	    case disk_log_1:mf_int_inc(Handle, L#log.head) of
		{ok, Handle2, Lost} ->
		    {ok, L#log{extra = Handle2}, Lost};
		{error, Error, Handle2} ->
		    {error, Error, L#log{extra = Handle2}}
	    end;
	external ->
	    case disk_log_1:mf_ext_inc(Handle, L#log.head) of
		{ok, Handle2, Lost} ->
		    {ok, L#log{extra = Handle2}, Lost};
		{error, Error, Handle2} ->
		    {error, Error, L#log{extra = Handle2}}
	    end
    end.


%%-----------------------------------------------------------------
%% Open a log file.
%%-----------------------------------------------------------------
%% -> {ok, Reply, log(), Cnt} | Error
%% Note: the header is always written, even if the log size is too small.
do_open2(halt, internal, Name, FName, Repair, Size, Mode, Head, _V) ->
    case catch disk_log_1:int_open(FName, Repair, Mode, Head) of
	{ok, {_Alloc, FdC, {NoItems, _NoBytes}, FileSize}} ->
            Halt = #halt{fdc = FdC, curB = FileSize, size = Size},
	    {ok, {ok, Name}, Halt, halt_int, NoItems};
	{repaired, FdC, Rec, Bad, FileSize} ->
            Halt = #halt{fdc = FdC, curB = FileSize, size = Size},
	    {ok, {repaired, Name, {recovered, Rec}, {badbytes, Bad}},
             Halt, halt_int, Rec};
	Error ->
	    Error
    end;
do_open2(wrap, internal, Name, FName, Repair, Size, Mode, Head, V) ->
    {MaxB, MaxF} = Size,
    case catch 
      disk_log_1:mf_int_open(FName, MaxB, MaxF, Repair, Mode, Head, V) of
	{ok, Handle, Cnt} ->
	    {ok, {ok, Name}, Handle, wrap_int, Cnt};
	{repaired, Handle, Rec, Bad, Cnt} ->
	    {ok, {repaired, Name, {recovered, Rec}, {badbytes, Bad}},
	     Handle, wrap_int, Cnt};
	Error ->
	    Error
    end;
do_open2(halt, external, Name, FName, Repair, Size, Mode, Head, _V) ->
    case catch disk_log_1:ext_open(FName, Repair, Mode, Head) of
	{ok, {_Alloc, FdC, {NoItems, _NoBytes}, FileSize}} ->
            Halt = #halt{fdc = FdC, curB = FileSize, size = Size},
	    {ok, {ok, Name}, Halt, halt_ext, NoItems};
	Error ->
	    Error
    end;
do_open2(wrap, external, Name, FName, Repair, Size, Mode, Head, V) ->
    {MaxB, MaxF} = Size,
    case catch 
      disk_log_1:mf_ext_open(FName, MaxB, MaxF, Repair, Mode, Head, V) of
	{ok, Handle, Cnt} ->
	    {ok, {ok, Name}, Handle, wrap_ext, Cnt};
	Error ->
	    Error
    end.

%% -> closed | Error
close_disk_log(undefined) ->
    closed;
close_disk_log(L) ->
    unblock_pid(L),
    F = fun({Pid, _}) -> 
		unlink(Pid) 
	end,
    lists:foreach(F, L#log.owners),
    R = (catch close_disk_log2(L)),
    erase(log),
    R.

-spec close_disk_log2(#log{}) -> 'closed'. % | throw(Error)

close_disk_log2(L) ->
    case L of
	#log{format_type = halt_int, mode = Mode, extra = Halt} ->
	    disk_log_1:close(Halt#halt.fdc, L#log.filename, Mode);
	#log{format_type = wrap_int, mode = Mode, extra = Handle} ->
	    disk_log_1:mf_int_close(Handle, Mode);
	#log{format_type = halt_ext, extra = Halt} ->
	    disk_log_1:fclose(Halt#halt.fdc, L#log.filename);
	#log{format_type = wrap_ext, mode = Mode, extra = Handle} ->
	    disk_log_1:mf_ext_close(Handle, Mode)
    end,
    closed.

do_format_error({error, Module, Error}) ->
    Module:format_error(Error);
do_format_error({error, Reason}) ->
    do_format_error(Reason);
do_format_error({Node, Error = {error, _Reason}}) ->
    lists:append(io_lib:format("~p: ", [Node]), do_format_error(Error));
do_format_error({badarg, Arg}) ->
    io_lib:format("The argument ~p is missing, not recognized or "
		  "not wellformed~n", [Arg]);
do_format_error({size_mismatch, OldSize, ArgSize}) ->
    io_lib:format("The given size ~p does not match the size ~p found on "
		  "the disk log size file~n", [ArgSize, OldSize]);
do_format_error({read_only_mode, Log}) ->
    io_lib:format("The disk log ~tp has been opened read-only, but the "
		  "requested operation needs read-write access~n", [Log]);
do_format_error({format_external, Log}) ->
    io_lib:format("The requested operation can only be applied on internally "
		  "formatted disk logs, but ~tp is externally formatted~n",
		  [Log]);
do_format_error({blocked_log, Log}) ->
    io_lib:format("The blocked disk log ~tp does not queue requests, or "
		  "the log has been blocked by the calling process~n", [Log]);
do_format_error({full, Log}) ->
    io_lib:format("The halt log ~tp is full~n", [Log]);
do_format_error({not_blocked, Log}) ->
    io_lib:format("The disk log ~tp is not blocked~n", [Log]);
do_format_error({not_owner, Pid}) ->
    io_lib:format("The pid ~tp is not an owner of the disk log~n", [Pid]);
do_format_error({not_blocked_by_pid, Log}) ->
    io_lib:format("The disk log ~tp is blocked, but only the blocking pid "
		  "can unblock a disk log~n", [Log]);
do_format_error({new_size_too_small, Log, CurrentSize}) ->
    io_lib:format("The current size ~p of the halt log ~tp is greater than the "
		  "requested new size~n", [CurrentSize, Log]);
do_format_error({halt_log, Log}) ->
    io_lib:format("The halt log ~tp cannot be wrapped~n", [Log]);
do_format_error({same_file_name, Log}) ->
    io_lib:format("Current and new file name of the disk log ~tp "
		  "are the same~n", [Log]);
do_format_error({arg_mismatch, Option, FirstValue, ArgValue}) ->
    io_lib:format("The value ~tp of the disk log option ~p does not match "
		  "the current value ~tp~n", [ArgValue, Option, FirstValue]);
do_format_error({name_already_open, Log}) ->
    io_lib:format("The disk log ~tp has already opened another file~n", [Log]);
do_format_error({open_read_write, Log}) ->
    io_lib:format("The disk log ~tp has already been opened read-write~n", 
		  [Log]);
do_format_error({open_read_only, Log}) ->
    io_lib:format("The disk log ~tp has already been opened read-only~n", 
		  [Log]);
do_format_error({not_internal_wrap, Log}) ->
    io_lib:format("The requested operation cannot be applied since ~tp is not "
		  "an internally formatted disk log~n", [Log]);
do_format_error(no_such_log) ->
    io_lib:format("There is no disk log with the given name~n", []);
do_format_error(nonode) ->
    io_lib:format("There seems to be no node up that can handle "
		  "the request~n", []);
do_format_error(nodedown) ->
    io_lib:format("There seems to be no node up that can handle "
		  "the request~n", []);
do_format_error({corrupt_log_file, FileName}) ->
    io_lib:format("The disk log file \"~ts\" contains corrupt data~n", 
                  [FileName]);
do_format_error({need_repair, FileName}) ->
    io_lib:format("The disk log file \"~ts\" has not been closed properly and "
		  "needs repair~n", [FileName]);
do_format_error({not_a_log_file, FileName}) ->
    io_lib:format("The file \"~ts\" is not a wrap log file~n", [FileName]);
do_format_error({invalid_header, InvalidHeader}) ->
    io_lib:format("The disk log header is not wellformed: ~p~n", 
		  [InvalidHeader]);
do_format_error(end_of_log) ->
    io_lib:format("An attempt was made to step outside a not yet "
		  "full wrap log~n", []);
do_format_error({invalid_index_file, FileName}) ->
    io_lib:format("The wrap log index file \"~ts\" cannot be used~n",
		  [FileName]);
do_format_error({no_continuation, BadCont}) ->
    io_lib:format("The term ~p is not a chunk continuation~n", [BadCont]);
do_format_error({file_error, FileName, Reason}) ->
    io_lib:format("\"~ts\": ~tp~n", [FileName, file:format_error(Reason)]);
do_format_error(E) ->
    io_lib:format("~tp~n", [E]).

do_info(L, Cnt) ->
    #log{name = Name, type = Type, mode = Mode, filename = File, 
	 extra = Extra, status = Status, owners = Owners, users = Users,
	 format = Format, head = Head} = L,
    Size = case Type of
	       wrap ->
		   disk_log_1:get_wrap_size(Extra);
	       halt ->
		   Extra#halt.size
	   end,
    RW = case Type of
	     wrap when Mode =:= read_write ->
		 #handle{curB = CurB, curF = CurF, 
			 cur_cnt = CurCnt, acc_cnt = AccCnt, 
			 noFull = NoFull, accFull = AccFull} = Extra,
		 NewAccFull = AccFull + NoFull,
		 NewExtra = Extra#handle{noFull = 0, accFull = NewAccFull},
		 put(log, L#log{extra = NewExtra}),
		 [{no_current_bytes, CurB},
		  {no_current_items, CurCnt},
		  {no_items, Cnt},
		  {no_written_items, CurCnt + AccCnt},
		  {current_file, CurF},
		  {no_overflows, {NewAccFull, NoFull}}
		 ];
	     halt when Mode =:= read_write ->
		 IsFull = case get(is_full) of 
			      undefined -> false; 
			      _ -> true 
			  end,
		 [{full, IsFull},
		  {no_written_items, Cnt}
		 ];
	     _ when Mode =:= read_only ->
		 []
	 end,
    HeadL = case Mode of
		read_write ->
		    [{head, case Head of
                                {ok, H} -> H;
                                none -> Head;
                                {_M, _F, _A} -> Head
                            end}];
		read_only ->
		    []
	    end,
    Common = [{name, Name},
	      {file, File},
	      {type, Type},
	      {format, Format},
	      {size, Size},
	      {items, Cnt}, % kept for "backward compatibility" (undocumented)
	      {owners, Owners},
	      {users, Users}] ++
	     HeadL ++
	     [{mode, Mode},
	      {status, Status},
	      {node, node()}
	     ],
    Common ++ RW.

do_block(Pid, QueueLogRecs, L) ->
    L2 = L#log{status = {blocked, QueueLogRecs}, blocked_by = Pid},
    put(log, L2),
    case is_owner(Pid, L2) of
	{true, _Notify} ->
	    ok;
	false ->
	    link(Pid)
    end.

do_unblock(Pid, #log{blocked_by = Pid}=L, S) ->
    do_unblock(L, S);
do_unblock(_Pid, _L, S) ->
    S.

do_unblock(L, S) ->
    unblock_pid(L),
    L2 = L#log{blocked_by = none, status = ok},
    put(log, L2),
    %% Since the block request is synchronous, and the blocking
    %% process is the only process that can unblock, all requests in
    %% 'messages' will have been put in 'queue' before the unblock
    %% request is granted.
    [] = S#state.messages, % assertion
    S#state{queue = [], messages = lists:reverse(S#state.queue)}.

-spec do_log(#log{}, [binary()]) -> integer() | {'error', _, integer()}.

do_log(L, B) ->
    do_log(L, B, iolist_size(B)).

do_log(#log{type = halt}=L, B, BSz) ->
    #log{format = Format, extra = Halt} = L,
    #halt{curB = CurSize, size = Sz} = Halt,
    {Bs, BSize} = logl(B, Format, BSz),
    case get(is_full) of
	true ->
            {error, {error, {full, L#log.name}}, 0};
	undefined when Sz =:= infinity; CurSize + BSize =< Sz ->
	    halt_write(Halt, L, B, Bs, BSize);
	undefined ->
	    halt_write_full(L, B, Format, 0)
    end;
do_log(#log{format_type = wrap_int}=L, B, _BSz) ->
    case disk_log_1:mf_int_log(L#log.extra, B, L#log.head) of
	{ok, Handle, Logged, Lost, Wraps} ->
	    notify_owners_wrap(Wraps),
	    put(log, L#log{extra = Handle}),
	    Logged - Lost;
	{ok, Handle, Logged} ->
	    put(log, L#log{extra = Handle}),
	    Logged;
	{error, Error, Handle, Logged, Lost} ->
	    put(log, L#log{extra = Handle}),
	    {error, Error, Logged - Lost}
    end;
do_log(#log{format_type = wrap_ext}=L, B, _BSz) ->
    case disk_log_1:mf_ext_log(L#log.extra, B, L#log.head) of
	{ok, Handle, Logged, Lost, Wraps} ->
	    notify_owners_wrap(Wraps),
	    put(log, L#log{extra = Handle}),
	    Logged - Lost;
	{ok, Handle, Logged} ->
	    put(log, L#log{extra = Handle}),
	    Logged;
	{error, Error, Handle, Logged, Lost} ->
	    put(log, L#log{extra = Handle}),
	    {error, Error, Logged - Lost}
    end.

logl(B, external, undefined) ->
    {B, iolist_size(B)};
logl(B, external, Sz) ->
    {B, Sz};
logl(B, internal, _Sz) ->
    disk_log_1:logl(B).

halt_write_full(L, [Bin | Bins], Format, N) ->
    B = [Bin],
    {Bs, BSize} = logl(B, Format, undefined),
    Halt = L#log.extra,
    #halt{curB = CurSize, size = Sz} = Halt,
    if 
	CurSize + BSize =< Sz ->
	    case halt_write(Halt, L, B, Bs, BSize) of
		N1 when is_integer(N1) ->
		    halt_write_full(get(log), Bins, Format, N+N1);
		Error ->
		    Error
	    end;
	true ->
	    halt_write_full(L, [], Format, N)
    end;
halt_write_full(L, _Bs, _Format, N) ->
    put(is_full, true),
    notify_owners(full),
    {error, {error, {full, L#log.name}}, N}.

halt_write(Halt, L, B, Bs, BSize) ->
    case disk_log_1:fwrite(Halt#halt.fdc, L#log.filename, Bs, BSize) of
	{ok, NewFdC} ->
	    NCurB = Halt#halt.curB + BSize,
	    NewHalt = Halt#halt{fdc = NewFdC, curB = NCurB},
	    put(log, L#log{extra = NewHalt}),
	    length(B);
	{Error, NewFdC} ->
	    put(log, L#log{extra = Halt#halt{fdc = NewFdC}}),
	    {error, Error, 0}
    end.

%% -> ok | Error
do_write_cache(#log{filename = FName, type = halt, extra = Halt} = Log) ->
    {Reply, NewFdC} = disk_log_1:write_cache(Halt#halt.fdc, FName),
    put(log, Log#log{extra = Halt#halt{fdc = NewFdC}}),
    Reply;
do_write_cache(#log{type = wrap, extra = Handle} = Log) ->
    {Reply, NewHandle} = disk_log_1:mf_write_cache(Handle),
    put(log, Log#log{extra = NewHandle}),
    Reply.

%% -> ok | Error
do_sync(#log{filename = FName, type = halt, extra = Halt} = Log) ->
    {Reply, NewFdC} = disk_log_1:sync(Halt#halt.fdc, FName),
    put(log, Log#log{extra = Halt#halt{fdc = NewFdC}}),
    Reply;
do_sync(#log{type = wrap, extra = Handle} = Log) ->
    {Reply, NewHandle} = disk_log_1:mf_sync(Handle),
    put(log, Log#log{extra = NewHandle}),
    Reply.

%% -> ok | Error | throw(Error)
do_trunc(#log{type = halt}=L, Head) ->
    #log{filename = FName, extra = Halt} = L,
    FdC = Halt#halt.fdc,
    {Reply1, FdC2} = 
        case L#log.format of
            internal ->
                disk_log_1:truncate(FdC, FName, Head);
            external ->
		case disk_log_1:truncate_at(FdC, FName, bof) of
		    {ok, NFdC} when Head =:= none ->
                        {ok, NFdC};
		    {ok, NFdC} ->
			{ok, H} = Head,
                        disk_log_1:fwrite(NFdC, FName, H, byte_size(H));
		    R -> 
			R
		end
        end,
    {Reply, NewHalt} = 
    case disk_log_1:position(FdC2, FName, cur) of
	{ok, NewFdC, FileSize} when Reply1 =:= ok ->
	    {ok, Halt#halt{fdc = NewFdC, curB = FileSize}};
	{Reply2, NewFdC} ->
	    {Reply2, Halt#halt{fdc = NewFdC}};
	{ok, NewFdC, _} ->
	    {Reply1, Halt#halt{fdc = NewFdC}}
    end,
    put(log, L#log{extra = NewHalt}),
    Reply;
do_trunc(#log{type = wrap}=L, Head) ->
    Handle = L#log.extra,
    OldHead = L#log.head,
    {MaxB, MaxF} = disk_log_1:get_wrap_size(Handle),
    ok = do_change_size(L, {MaxB, 1}),
    NewLog = trunc_wrap((get(log))#log{head = Head}),
    %% Just to remove all files with suffix > 1:
    NewLog2 = trunc_wrap(NewLog),
    NewHandle = (NewLog2#log.extra)#handle{noFull = 0, accFull = 0},
    do_change_size(NewLog2#log{extra = NewHandle, head = OldHead}, 
		   {MaxB, MaxF}).

trunc_wrap(L) ->
    case do_inc_wrap_file(L) of
	{ok, L2, _Lost} ->
	    L2;
	{error, Error, _L2} ->
	    throw(Error)
    end.

do_chunk(#log{format_type = halt_int, extra = Halt} = L, Pos, B, N) ->
    FdC = Halt#halt.fdc,
    {NewFdC, Reply} = 
        case L#log.mode of
            read_only ->
                disk_log_1:chunk_read_only(FdC, L#log.filename, Pos, B, N);
            read_write ->
                disk_log_1:chunk(FdC, L#log.filename, Pos, B, N)
        end,
    put(log, L#log{extra = Halt#halt{fdc = NewFdC}}),
    Reply;
do_chunk(#log{format_type = wrap_int, mode = read_only, 
	      extra = Handle} = Log, Pos, B, N) ->
    {NewHandle, Reply} = disk_log_1:mf_int_chunk_read_only(Handle, Pos, B, N),
    put(log, Log#log{extra = NewHandle}),
    Reply;
do_chunk(#log{format_type = wrap_int, extra = Handle} = Log, Pos, B, N) ->
    {NewHandle, Reply} = disk_log_1:mf_int_chunk(Handle, Pos, B, N),
    put(log, Log#log{extra = NewHandle}),
    Reply;
do_chunk(Log, _Pos, _B, _) ->
    {error, {format_external, Log#log.name}}.

do_chunk_step(#log{format_type = wrap_int, extra = Handle}, Pos, N) ->
    disk_log_1:mf_int_chunk_step(Handle, Pos, N);
do_chunk_step(Log, _Pos, _N) ->
    {error, {not_internal_wrap, Log#log.name}}.

%% Inlined.
replies(Pids, Reply) ->
    M = {disk_log, self(), Reply},
    send_reply(Pids, M).

send_reply(Pid, M) when is_pid(Pid) ->
    Pid ! M,
    ok;
send_reply([Pid | Pids], M) ->
    Pid ! M,
    send_reply(Pids, M);
send_reply([], _M) ->
    ok.

reply(To, Reply, S) ->
    To ! {disk_log, self(), Reply},
    loop(S).

req(Log, R) ->
    case disk_log_server:get_log_pid(Log) of
	undefined ->
	    {error, no_such_log};
	Pid ->
	    monitor_request(Pid, R)
    end.

monitor_request(Pid, Req) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Req},
    receive 
	{'DOWN', Ref, process, Pid, _Info} ->
	    {error, no_such_log};
	{disk_log, Pid, Reply} when not is_tuple(Reply) orelse
                                    element(2, Reply) =/= disk_log_stopped ->
	    erlang:demonitor(Ref, [flush]),
	    Reply
    end.

req2(Pid, R) ->
    monitor_request(Pid, R).

merge_head(none, Head) ->
    Head;
merge_head(Head, _) ->
    Head.

%% -> List of extensions of existing files (no dot included) | throw(FileError)
wrap_file_extensions(File) -> 
    {_CurF, _CurFSz, _TotSz, NoOfFiles} =
	disk_log_1:read_index_file(File),
    Fs = if 
	     NoOfFiles >= 1 ->
		 lists:seq(1, NoOfFiles);
	     NoOfFiles =:= 0 ->
		 []
	 end,
    Fun = fun(Ext) ->
		  case file:read_file_info(add_ext(File, Ext)) of
		      {ok, _} ->
			  true;
		      _Else ->
			  false
		  end
	  end,
    lists:filter(Fun, ["idx", "siz" | Fs]).

add_ext(File, Ext) ->
    lists:concat([File, ".", Ext]).

notify(Log, R) ->
    case disk_log_server:get_log_pid(Log) of
	undefined ->
	    {error, no_such_log};
	Pid ->
	    Pid ! R,
	    ok
    end.

notify_owners_wrap([]) ->
    ok;
notify_owners_wrap([N | Wraps]) ->
    notify_owners({wrap, N}),
    notify_owners_wrap(Wraps).

notify_owners(Note) ->
    L = get(log),
    Msg = {disk_log, node(), L#log.name, Note},
    lists:foreach(fun({Pid, true}) -> Pid ! Msg;
		     (_) -> ok
		  end, L#log.owners).

cache_error(#state{cache_error=Error}=S, Pids) ->
    ok = replies(Pids, Error),
    state_err(S#state{cache_error = ok}, Error).

state_ok(S) ->
    state_err(S, ok).

-spec state_err(#state{}, dlog_state_error()) -> #state{}.

state_err(S, Err) when S#state.error_status =:= Err -> S;
state_err(S, Err) ->
    notify_owners({error_status, Err}),
    S#state{error_status = Err}.
