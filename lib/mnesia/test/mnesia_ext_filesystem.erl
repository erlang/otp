%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: the external plugin framework has been developed further, and
%% this plugin is not updated accordingly. Therefore there might be
%% some incompatibilities!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% External filesystem storage backend for mnesia.
%% Usage: mnesia:create_table(Tab, [{external, [
%%                                   {mnesia_ext_filesystem, Nodes}]}, ...]).

-module(mnesia_ext_filesystem).
-behaviour(mnesia_backend_type).

%% -compile(export_all).

-export([register/0,
	 types/0,
	 valid_op/2]).
%%
-export([
	 add_aliases/1,
	 check_definition/4,
	 create_table/3,
	 delete/3,
	 delete_table/2,
	 load_table/4,
	 close_table/2,
	 sync_close_table/2,
	 first/2,
	 fixtable/3,
	 index_is_consistent/3,
	 init_backend/0,
	 info/3,
	 insert/3,
	 is_index_consistent/2,
	 last/2,
	 lookup/3,
	 match_delete/3,
	 next/3,
	 prev/3,
	 real_suffixes/0,
	 remove_aliases/1,
	 repair_continuation/2,
	 select/1,
	 select/3,
	 select/4,
	 semantics/2,
	 slot/3,
	 tmp_suffixes/0,
	 update_counter/4,
	 validate_key/6,
	 validate_record/6
	]).

-export([sender_init/4,
	 sender_handle_info/5,
	 receiver_first_message/4,
	 receive_data/5,
	 receive_done/4
	]).

%% record and key validation

%% table process start and callbacks
-export([start_proc/3,
	 init/1,
	 handle_call/3,
	 handle_info/2,
	 handle_cast/2,
	 terminate/2,
	 code_change/3]).

-include("../src/mnesia.hrl").
-include_lib("kernel/include/file.hrl").
%% -record(info, {tab,
%% 	       key,
%% 	       fullname,
%% 	       recname}).

-record(sel, {alias,
	      tab,
	      type,
	      recname,
	      mp,
	      keypat,
	      ms,
	      limit,
	      key_only = false,
	      direction = forward}).

-record(st, {ets,
	     dets,
	     mp,
	     data_mp,
	     alias,
	     tab}).


register() ->
    mnesia_schema:schema_transaction(
      fun() ->
	      [mnesia_schema:do_add_backend_type(T, M)
	       || {T, M} <- types()]
      end).

types() ->
    [{fs_copies, ?MODULE},
     {raw_fs_copies, ?MODULE}].

semantics(_Alias, storage) -> disc_only_copies;
semantics(_Alias, types  ) -> [set, ordered_set];
semantics(_Alias, index_types) -> [ordered];
semantics(_Alias, _) ->
    undefined.

valid_op(_, _) ->
    true.

init_backend() ->
    ok.

add_aliases(_) ->
    ok.

remove_aliases(_) ->
    ok.

%% ===========================================================
%% Table synch protocol
%% Callbacks are
%% Sender side:
%%  1. sender_init(Alias, Tab, RemoteStorage, ReceiverPid) ->
%%        {InitFun :: fun() -> {Recs, Cont} | '$end_of_table',
%%         ChunkFun :: fun(Cont) -> {Recs, Cont1} | '$end_of_table'}
%%  2. InitFun() is called
%%  3a. ChunkFun(Cont) is called repeatedly until done
%%  3b. sender_handle_info(Msg, Alias, Tab, ReceiverPid, Cont) ->
%%        {ChunkFun, NewCont}
%%
%% Receiver side:
%% 1. receiver_first_message(SenderPid, Msg, Alias, Tab) ->
%%        {Size::integer(), State}
%% 2. receive_data(Data, Alias, Tab, _Sender, State) ->
%%        {more, NewState} | {{more, Msg}, NewState}
%% 3. receive_done(_Alias, _Tab, _Sender, _State) ->
%%        ok
%%
%% The receiver can communicate with the Sender by returning
%% {{more, Msg}, St} from receive_data/4. The sender will be called through
%% sender_handle_info(Msg, ...), where it can adjust its ChunkFun and
%% Continuation. Note that the message from the receiver is sent once the
%% receive_data/4 function returns. This is slightly different from the
%% normal mnesia table synch, where the receiver acks immediately upon
%% reception of a new chunk, then processes the data.
%%

sender_init(Alias, Tab, RemoteStorage, Pid) ->
    %% Need to send a message to the receiver. It will be handled in
    %% receiver_first_message/4 below. There could be a volley of messages...
    {ext, Alias, ?MODULE} = RemoteStorage, % limitation for now
    Pid ! {self(), {first, info(Alias, Tab, size)}},
    receive
	{Pid, Reply} ->
	    io:fwrite("Receiver (~p) replies: ~p~n", [Pid, Reply])
    after 30000 ->
	    erlang:error(timeout_waiting_for_receiver)
    end,
    MP = data_mountpoint(Tab),
    {fun() ->
	     {ok, Fs} = file:list_dir(MP),
	     FI = fetch_files(Fs, MP),
	     {[{[], FI}], {[F || {F,dir} <- FI], MP, MP,
			   fun() -> '$end_of_table' end}}
     end,
     chunk_fun()}.

sender_handle_info(_Msg, _Alias, _Tab, _ReceiverPid, Cont) ->
    %% ignore - we don't expect any message from the receiver
    {chunk_fun(), Cont}.

chunk_fun() ->
    fun(Cont) ->
	    fetch_more_files(Cont)
    end.

fetch_files([F|Fs], Dir) ->
    Fn = filename:join(Dir, F),
    case file:read_file(Fn) of
	{error, eisdir} ->
	    [{F, dir}|fetch_files(Fs, Dir)];
	{ok, Bin} ->
	    [{F, Bin}|fetch_files(Fs, Dir)];
	{error, enoent} ->
	    %% ouch!
	    fetch_files(Fs, Dir)
    end;
fetch_files([], _) ->
    [].

fetch_more_files({[], _, _, C}) ->
    C();
fetch_more_files({[F|Fs], Dir, MP, C}) ->
    D = filename:join(Dir, F),
    case file:list_dir(D) of
	{error, enoent} ->
	    io:fwrite("Not found: ~s~n", [D]),
	    fetch_more_files({Fs, Dir, MP, C});
	{ok, Fs1} ->
	    Fs1i = fetch_files(Fs1, D),
	    {[{remove_top(MP, D), Fs1i}], {[Fx || {Fx,dir} <- Fs1i], D, MP,
			   fun() -> fetch_more_files({Fs, Dir, MP, C}) end}}
    end.

receiver_first_message(Pid, {first, Size} = _Msg, _Alias, _Tab) ->
    io:fwrite("~p:receiver_first_message (~p): ~p~n", [?MODULE, Pid, _Msg]),
    Pid ! {self(), "Hello Joe..."},
    {Size, _State = []}.


receive_data(Data, Alias, Tab, _Sender, State) ->
    MP = data_mountpoint(Tab),
    N = store_data(Data, Alias, Tab, MP, 0),
    call(Alias, Tab, {incr_size, N}),
    {more, State}.

receive_done(_Alias, _Tab, _Sender, _State) ->
    ok.

store_data([], _, _, _, N) ->
    N;
store_data([{Dir, Fs}|T], Alias, Tab, MP, N) ->
    Dirname = filename:join(MP, Dir),
    N1 = case list_dir(Dirname) of
	     {error, enoent} ->
		 fill_empty(Dirname, Fs, N);
	     {error, enotdir} ->
		 N11 = delete_file_or_dir(Dirname, N),
		 store_data([{Dir, Fs}|T], Alias, Tab, MP, N11);
	     {ok, MyFs} ->
		 diff_dirs(MyFs, Fs, Dirname, N)
	 end,
    store_data(T, Alias, Tab, MP, N1).

list_dir(Dirname) ->
    file:list_dir(Dirname).

make_dir(D)     -> file:make_dir(D).
del_dir(D)      -> file:del_dir(D).
delete_file(F)  -> file:delete(F).
write_file(F,B) -> file:write_file(F, B).

fill_empty(Dirname, Fs, N) ->
    make_dir(Dirname),
    lists:foldl(fun({F, dir}, Acc) ->
			file:make_dir(filename:join(Dirname, F)),
			Acc;
		   ({F, Bin}, Acc) when is_binary(Bin) ->
			file:write_file(filename:join(Dirname, F), Bin),
			Acc + 1
		end, N, Fs).

diff_dirs(MyFs, Fs, Dirname, N) ->
    N1 = lists:foldl(
	   fun(F, Acc) ->
		   case lists:keymember(F, 1, Fs) of
		       false ->
			   delete_file_or_dir(filename:join(Dirname,F), Acc);
		       true ->
			   Acc
		   end
	   end, N, MyFs),
    lists:foldl(
      fun({F,dir}, Acc) ->
	      Fname = filename:join(Dirname, F),
	      case make_dir(Fname) of
		  {error, eexist} ->
		      case filelib:is_regular(Fname) of
			  true ->
			      delete_file(Fname),
			      make_dir(Fname),
			      Acc - 1;
			  false ->
			      Acc
		      end;
		  ok ->
		      Acc
	      end;
	 ({F,Bin}, Acc) when is_binary(Bin) ->
	      Fname = filename:join(Dirname, F),
	      case write_file(Fname, Bin) of
		  {error, eisdir} ->
		      N = delete_file_or_dir(Fname),
		      write_file(Fname, Bin),
		      Acc+1-N;
		  ok ->
		      Acc+1
	      end
      end, N1, Fs).


delete_file_or_dir(F) ->
    delete_file_or_dir(F, 0).

delete_file_or_dir(F, N) ->
    case delete_file(F) of
	{error, eisdir} ->
	    {ok, Fs} = list_dir(F),
	    N1 = lists:foldl(fun(F1, Acc) ->
				     Fn = filename:join(F, F1),
				     delete_file_or_dir(Fn, Acc)
			     end, N, Fs),
	    ok = del_dir(F),
	    N1;
	ok ->
	    N+1
    end.

%% End of table synch protocol
%% ===========================================================


validate_key(Alias, Tab, RecName, Arity, Type, Key) ->
    %% RecName, Arity and Type have already been validated
    try begin
	    NewKey = encode_key(Alias, Tab, Key),
	    is_legal_key(Alias, Tab, NewKey),
	    {RecName, Arity, Type}
	end
    catch
	error:_ -> mnesia:abort(bad_type, [Tab, Key])
    end.

is_legal_key(raw_fs_copies, Tab, Key) ->
    MP = data_mountpoint(Tab),
    FN = fullname(Tab, Key, MP),
    case file:read_file_info(FN) of
	{error, enoent} ->
	    case is_valid_dir(FN, MP) of
		ok ->
		    ok;
		Other ->
		    mnesia:abort(Other)
	    end;
	{error, _} = OtherErr ->
	    mnesia:abort(OtherErr);
	{ok, #file_info{type = regular}} ->
	    ok;
	{ok, #file_info{type = directory}} ->
	    mnesia:abort({error, eisdir})
    end;
is_legal_key(_, _, _) ->
    ok.


is_valid_dir(D, D) ->
    ok;
is_valid_dir(F, MP) ->
    D = filename:dirname(F),
    case file:read_file_info(D) of
	{error, enoent} ->
	    is_valid_dir(D, MP);
	{ok, #file_info{type = directory,
			access = Access}} ->
	    if Access == read_write ->
		    ok;
	       true ->
		    {error, eacces}
	    end;
	{ok, #file_info{}} ->
	    {error, enotdir}
    end.

validate_record(Alias, Tab, RecName, Arity, Type, Obj) ->
    %% RecName, Arity and Type have already been validated
    Key = encode_key(Alias, Tab, element(2, Obj)),
    is_legal_key(Alias, Tab, Key),
    {RecName, Arity, Type}.

encode_key(fs_copies, Tab, Key) ->
    split_key(sext_encode(Key));
encode_key(raw_fs_copies, Tab, Key) ->
    if is_binary(Key) ->
	    binary_to_list(Key);
       is_list(Key) ->
	    binary_to_list(list_to_binary(Key));
       is_atom(Key) ->
	    atom_to_list(Key);
       true ->
	    mnesia:abort({bad_type, [Tab, Key]})
    end.

decode_key(Key, fs_copies, _) ->
    sext_decode(unsplit_key(list_to_binary([Key])));
decode_key(Key, _, _) ->
    Key.


sext_encode(K) ->
    mnesia_sext:encode_sb32(K).

sext_decode(K) ->
    mnesia_sext:decode_sb32(K).

split_key(K) when is_binary(K) ->
    binary_to_list(split_key1(K)).

split_key1(<<A,B,C,T/binary>>) ->
    <<A,B,C,$/, (split_key1(T))/binary>>;
split_key1(Bin) ->
    Bin.

unsplit_key(Bin) ->
    << <<C>> || <<C>> <= Bin,
		C =/= $/ >>.


create_table(_Alias, Tab, _Props) ->
    create_mountpoint(Tab),
    ok.

delete_table(_Alias, Tab) ->
    MP = get_mountpoint(Tab),
    assert_proper_mountpoint(Tab, MP),
    os:cmd("rm -r " ++ MP).

assert_proper_mountpoint(_Tab, _MP) ->
    %% not yet implemented. How to verify that the MP var points to the
    %% directory we actually want deleted?
    ok.

prop(K,Props) ->
    proplists:get_value(K, Props).

check_definition(Alias, Tab, Nodes, Props)
  when Alias==fs_copies; Alias==raw_fs_copies ->
    Id = {Alias, Nodes},
    [Rc, Dc, DOc] =
        [proplists:get_value(K,Props,[]) || K <- [ram_copies,
						  disc_copies,
						  disc_only_copies]],
    case {Rc,Dc,DOc} of
	{[],[],[]} ->
	    case prop(type, Props) of
                ordered_set -> ok;
		set -> ok;
                Type -> mnesia:abort({combine_error, Tab, [Id, {type, Type}]})
            end,
	    MP = get_mountpoint(Tab),
	    case dir_exists_or_creatable(MP) of
		true ->
		    ok;
		false ->
		    mnesia:abort({bad_mountpoint, Tab, [MP]})
	    end,
	    case {Alias, prop(attributes, Props)} of
		{raw_fs_copies, [_Key, _Value]} ->
		    ok;
		{fs_copies,  _} ->
		    ok;
		Attrs ->
		    mnesia:abort({invalid_attributes, Tab, Attrs})
	    end;
	_ ->
	    X = fun(_, []) -> [];
		   (T, [_|_]=Ns) -> [{T, Ns}]
		end,
	    mnesia:abort({combine_error, Tab,
			  [Id |
			   X(ram_copies, Rc) ++ X(disc_copies, Dc) ++
			   X(disc_only_copies,DOc)]})
    end.

%%
info_mountpoint(Tab) ->
    Dir = mnesia_monitor:get_env(dir),
    filename:join(Dir, tabname(Tab) ++ ".extfsi").

data_mountpoint(Tab) ->
    get_mountpoint(Tab).

%%
get_mountpoint(Tab) ->
    L = mnesia_monitor:get_env(filesystem_locations),
    case lists:keyfind(Tab, 1, L) of
	false ->
	    default_mountpoint(Tab);
	{_, Loc} ->
	    Loc
    end.

default_mountpoint(Tab) ->
    Dir = mnesia_monitor:get_env(dir),
    filename:join(Dir, tabname(Tab) ++ ".extfs").

real_suffixes() ->
    [".extfs", ".extfsi"].

tmp_suffixes() ->
    [].

%% pos(A, Attrs) ->  pos(A, Attrs, 1).

%% pos(A, [A|_], P) ->  P;
%% pos(A, [_|T], P) ->  pos(A, T, P+1);
%% pos(_, [], _) ->     0.


my_file_info(F) ->
    case file:read_link_info(F) of
	{ok, #file_info{type = symlink}} ->
	    case file:read_link(F) of
		{ok, NewF} ->
		    my_file_info(NewF);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

parent_dir(F) ->
    case filename:split(F) of
	["."] ->
	    parent_dir(filename:absname(F));
	[_] ->
	    F;
	_ ->
	    filename:dirname(F)
    end.

is_writable(D) ->
    case my_file_info(D) of
	{ok, #file_info{type = directory, access = A}}
	  when A == write; A == read_write ->
	    true;
	_ ->
	    false
    end.

dir_exists_or_creatable(Dir) ->
    case my_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    true;
	{error, enoent} ->
	    is_writable(parent_dir(Dir));
	_Other ->
	    false
    end.

create_mountpoint(Tab) ->
    MP = get_mountpoint(Tab),
    file:make_dir(MP),
    file:make_dir(data_mountpoint(Tab)),
    file:make_dir(info_mountpoint(Tab)),
    MP.


load_table(Alias, Tab, _LoadReason, _Opts) ->
    MP = get_mountpoint(Tab),
    {ok, _Pid} =
	mnesia_ext_sup:start_proc(
	  Tab, ?MODULE, start_proc, [Alias, Tab, MP]),
    ok.

close_table(Alias, Tab) ->
    sync_close_table(Alias, Tab).

sync_close_table(Alias, Tab) ->
    try call(Alias, Tab, close_table)
    catch
	error:_ ->
	    ok
    end.

index_is_consistent(Alias, Ix, Bool) ->
    call(Alias, Ix, {write_info, index_consistent, Bool}).

is_index_consistent(Alias, Ix) ->
    case info(Alias, Ix, index_consistent) of
	undefined ->
	    false;
	Bool when is_boolean(Bool) ->
	    Bool
    end.

fullname(Tab, Key) ->
    MP = data_mountpoint(Tab),
    fullname(Tab, Key, MP).

fullname(_Tab, Key0, MP) ->
    Key = if is_binary(Key0) ->
		  binary_to_list(Key0);
	     is_list(Key0) ->
		  lists:flatten(Key0);
	     is_atom(Key0) ->
		  list_to_binary(atom_to_list(Key0))
	  end,
    filename:join([MP, Key]).


info(_Alias, Tab, memory) ->
    try ets:info(tab_name(icache, Tab), memory)
    catch
	error:_ ->
	    0
    end;
info(_Alias, Tab, Item) ->
    try ets:lookup(tab_name(icache, Tab), {info,Item}) of
	[{_, Value}] ->
	    Value;
	[] ->
	    undefined
    catch
	error:Reason ->
	    {error, Reason}
    end.

lookup(Alias, Tab, Key0) ->
    Key = encode_key(Alias, Tab, Key0),
    lookup(Alias, Tab, Key0, fullname(Tab, Key)).

lookup(fs_copies, _Tab, _Key, Fullname) ->
    case file:read_file(Fullname) of
	{ok, Bin} ->
	    [binary_to_term(Bin)];
	{error, _} ->
	    []
    end;
lookup(raw_fs_copies, Tab, Key, Fullname) ->
    Tag = mnesia_lib:val({Tab, record_name}),
    case file:read_file(Fullname) of
	{ok, Bin} ->
	    [{Tag, Key, Bin}];
	{error, enoent} ->
	    []
    end.

insert(Alias, Tab, Obj) ->
    call(Alias, Tab, {insert, Obj}).

%% do_insert/4 -> boolean()
%% server-side end of insert/3.
%% Returns true: new object was added; false: existing object was updated
%%
do_insert(Alias, Tab, Obj, MP) ->
    Key = encode_key(Alias, Tab, element(2, Obj)),
    Fullname = fullname(Tab, Key, MP),
    case filelib:ensure_dir(Fullname) of
	{error, _} = Err ->
	    Err;
	ok ->
	    case file:read_file_info(Fullname) of
		{error, enoent} ->
		    ok = write_object(Alias, Fullname, Obj),
		    true;
		{ok, #file_info{type = regular}} ->
		    ok = write_object(Alias, Fullname, Obj),
		    false;
		{ok, #file_info{type = directory}} ->
		    {error, eisdir}
	    end
    end.

write_object(fs_copies, Fullname, Obj) ->
    file:write_file(Fullname, term_to_binary(Obj, [compressed]));
write_object(raw_fs_copies, Fullname, {_, _K, V}) ->
    file:write_file(Fullname, iolist_to_binary([V])).

select(Alias, Tab, Ms) ->
    MP = data_mountpoint(Tab),
    case do_select(Alias, Tab, MP, Ms, infinity) of
	{Res, Cont} ->
	    '$end_of_table' = Cont(),
	    Res;
	'$end_of_table' ->
	    '$end_of_table'
    end.

select(Alias, Tab, Ms, Limit) when is_integer(Limit) ->
    MP = data_mountpoint(Tab),
    do_select(Alias, Tab, MP, Ms, Limit).

repair_continuation(Cont, _Ms) ->
    Cont.

select(C) ->
    Cont = get_sel_cont(C),
    Cont().

get_sel_cont(C) ->
    Cont = case C of
	       {?MODULE, C1} -> C1;
	       _ -> C
	   end,
    if is_function(Cont, 0) ->
	    case erlang:fun_info(Cont, module) of
		{_, ?MODULE} ->
		    Cont;
		_ ->
		    erlang:error(badarg)
	    end;
       true ->
	    erlang:error(badarg)
    end.

fixtable(_Alias, _Tab, _Bool) ->
    true.

delete(Alias, Tab, Key) ->
    call(Alias, Tab, {delete, Key}).

do_delete(Alias, Tab, Key, MP) ->
    Fullname = fullname(Tab, encode_key(Alias, Tab, Key), MP),
    Deleted = case file:read_file_info(Fullname) of
		  {error, enoent} ->
		      false;
		  {ok, _} ->
		      true
	      end,
    file:delete(Fullname),
    Deleted.

match_delete(Alias, Tab, Pat) ->
    MP = data_mountpoint(Tab),
    Fun = fun(Obj, N) ->
		  Key = element(2, Obj),
		  case do_delete(Alias, Tab, Key, MP) of
		      false -> N;
		      true -> N+1
		  end
	  end,
    Tot = fold(Alias, Tab, Fun, 0, [{Pat,[],['$_']}], 30),
    call(Alias, Tab, {incr_size, -Tot}),
    ok.

first(Alias, Tab) ->
    MP = data_mountpoint(Tab),
    Type = mnesia:table_info(Tab, type),
    case list_dir(MP, Type) of
	{ok, Fs} ->
	    case do_next1(Fs, MP, Type) of
		'$end_of_table' ->
		    '$end_of_table';
		F ->
		    decode_key(remove_top(MP, F), Alias, Tab)
	    end;
	_ ->
	    '$end_of_table'
    end.

last(Alias, Tab) ->
    MP = data_mountpoint(Tab),
    Type = mnesia:table_info(Tab, type),
    case list_dir(MP, Type) of
	{ok, Fs} ->
	    case do_prev1(lists:reverse(Fs), MP, Type) of
		'$end_of_table' ->
		    '$end_of_table';
		F ->
		    decode_key(remove_top(MP, F), Alias, Tab)
	    end;
	_ ->
	    '$end_of_table'
    end.

prev(Alias, Tab, Key) ->
    MP = data_mountpoint(Tab),
    Fullname = fullname(Tab, Key, MP),
    Type = mnesia:table_info(Tab, type),
    case do_prev(Fullname, Type) of
	'$end_of_table' ->
	    '$end_of_table';
	F ->
	    decode_key(remove_top(MP, F), Alias, Tab)
    end.


next(Alias, Tab, Key0) ->
    MP = data_mountpoint(Tab),
    Key = encode_key(Alias, Tab, Key0),
    Fullname = fullname(Tab, Key, MP),
    Type = mnesia:table_info(Tab, type),
    case do_next(Fullname, Type) of
	'$end_of_table' ->
	    '$end_of_table';
	F ->
	    decode_key(remove_top(MP, F), Alias, Tab)
    end.

do_prev(Fullname, Type) ->
    {Dir, Base} = get_parent(Fullname),
    case list_dir(Dir, Type) of
	{ok, Fs} ->
	    case my_lists_split(Base, Fs, rev_type(Type)) of
		{_, []} ->
		    '$end_of_table';
		{_, L} ->
		    do_prev1(L, Dir, Type)
	    end;
	_ ->
	    %% empty, or deleted
	    '$end_of_table'
    end.

do_next(Fullname, Type) ->
    {Dir, Base} = get_parent(Fullname),
    case list_dir(Dir, Type) of
	{ok, Fs} ->
	    case my_lists_split(Base, Fs, Type) of
		{_, []} ->
		    '$end_of_table';
		{_, L} ->
		    do_next1(L, Dir, Type)
	    end;
	_ ->
	    %% empty, or deleted
	    '$end_of_table'
    end.

do_next1([H|T], Dir, Type) ->
    File = filename:join(Dir, H),
    case list_dir(File, Type) of
	{ok, Fs} ->
	    case do_next1(Fs, File, Type) of
		'$end_of_table' ->
		    do_next1(T, Dir, Type);
		Found ->
		    Found
	    end;
	{error, enotdir} ->
	    File
    end;
do_next1([], _, _) ->
    '$end_of_table'.

do_prev1([H|T], Dir, Type) ->
    File = filename:join(Dir, H),
    case list_dir(File, Type) of
	{ok, Fs} ->
	    case do_prev1(lists:reverse(Fs), File, Type) of
		'$end_of_table' ->
		    do_prev1(T, Dir, Type);
		Found ->
		    Found
	    end;
	{error, enotdir} ->
	    File
    end;
do_prev1([], _, _) ->
    '$end_of_table'.

get_parent(F) ->
    Dir = filename:dirname(F),
    Base = filename:basename(F),
    {Dir, Base}.

rev_type(ordered_set) -> reverse_order;
rev_type(T)           -> T.

my_lists_split(X, L, ordered_set) ->
    ordered_split(X, L, []);
my_lists_split(X, L, reverse_order) ->
    rev_ordered_split(X, lists:reverse(L), []);
my_lists_split(X, L, _) ->
    unordered_split(X, L, []).

ordered_split(X, [H|T], Acc) when X >= H ->
    ordered_split(X, T, [H|Acc]);
ordered_split(_X, L, Acc) ->
    {Acc, L}.

rev_ordered_split(X, [H|T], Acc) when X =< H ->
    rev_ordered_split(X, T, [H|Acc]);
rev_ordered_split(_X, L, Acc) ->
    {Acc, L}.

unordered_split(X, [X|T], Acc) ->
    {Acc, T};
unordered_split(X, [H|T], Acc) ->
    unordered_split(X, T, [H|Acc]);
unordered_split(_X, [], Acc) ->
    {Acc, []}.

slot(_Alias, _Tab, _Pos) ->
    ok.

update_counter(_Alias, _Tab, _C, _Val) ->
    ok.

fold(Alias, Tab, Fun, Acc, MS, N) ->
    fold1(select(Alias, Tab, MS, N), Fun, Acc).

fold1('$end_of_table', _, Acc) ->
    Acc;
fold1({L, Cont}, Fun, Acc) ->
    fold1(select(Cont), Fun, lists:foldl(Fun, Acc, L)).

is_key_prefix(File, Fun) when is_function(Fun) ->
    Fun(File);
is_key_prefix(File, {Pfx,_}) ->
    lists:prefix(Pfx, File).
%% is_key_prefix1([], _) ->
%%     true;
%% is_key_prefix1([H|T], [H|T1]) ->
%%     is_key_prefix1(T, T1);
%% is_key_prefix1([_|_], L) when is_list(L) ->
%%     false;
%% is_key_prefix1(_, '_') ->
%%     true;
%% is_key_prefix1(_, V) ->
%%     is_dollar_var(V).

%% is_dollar_var(P) when is_atom(P) ->
%%     case atom_to_list(P) of
%% 	"\$" ++ T ->
%% 	    try begin _ = list_to_integer(T),
%% 		      true
%% 		end
%% 	    catch
%% 		error:_ ->
%% 		    false
%% 	    end;
%% 	_ ->
%% 	    false
%%     end;
%% is_dollar_var(_) ->
%%     false.

keypat(_Alias, _Dir, F) when is_function(F) ->
    fun(File) -> F(keypat, File) end;
keypat(Alias, Dir, [{HeadPat,Gs,_}|_]) when is_tuple(HeadPat) ->
    keypat1(Alias, HeadPat, Dir, Gs);
keypat(_, _, _) ->
    {"", universal_keypat()}.

keypat1(fs_copies, HP, Dir, Gs) ->
    KP = element(2, HP),
    KeyVars = extract_vars(KP),
    Guards = relevant_guards(Gs, KeyVars),
    Pfx = Dir ++ [$/|split_key(mnesia_sext:prefix_sb32(KP))],
    {Pfx, [{KP, Guards, [true]}]};
keypat1(_, HP, Dir, _) ->
    case element(2, HP) of
	L when is_list(L) ->
	    {Dir ++ [$/|plain_key_prefix(L)], [{L,[],[true]}]};
	_ ->
	    {"", universal_keypat()}
    end.

universal_keypat() ->
    [{'_',[],[true]}].

plain_key_prefix([H|T]) when is_integer(H) ->
    [H|plain_key_prefix(T)];
plain_key_prefix(_) ->
    [].



relevant_guards(Gs, Vars) ->
    case Vars -- ['_'] of
	[] ->
	    [];
	Vars1 ->
	    lists:filter(fun(G) ->
				 Vg = extract_vars(G),
				 intersection(Vg, Vars1) =/= []
				     andalso (Vg -- Vars1) == []
			 end, Gs)
    end.


do_select(Alias, Tab, Dir, MS, Limit) ->
    MP = remove_ending_slash(Dir),
    Type = mnesia:table_info(Tab, type),
    RecName = mnesia:table_info(Tab, record_name),
    {Pfx, _} = Keypat = keypat(Alias, MP, MS),
    Sel = #sel{alias = Alias,
	       tab = Tab,
	       type = Type,
	       mp = MP,
	       keypat = Keypat,
	       recname = RecName,
	       ms = MS,
	       key_only = needs_key_only(MS),
	       limit = Limit},
    StartDir = case length(PDir = filename:dirname(Pfx)) of
		   L when L >= length(MP) ->
		       PDir;
		   _ ->
		       MP
	       end,
    do_search_dir(StartDir, Sel, [], Limit).

needs_key_only([{HP,_,Body}]) ->
    BodyVars = lists:flatmap(fun extract_vars/1, Body),
    %% Note that we express the conditions for "needs more than key" and negate.
    not(wild_in_body(BodyVars) orelse
	case bound_in_headpat(HP) of
	    {all,V} -> lists:member(V, BodyVars);
	    none    -> false;
	    Vars    -> any_in_body(lists:keydelete(2,1,Vars), BodyVars)
	end);
needs_key_only(_) ->
    %% don't know
    false.


bound_in_headpat(HP) when is_atom(HP) ->
    {all, HP};
bound_in_headpat(HP) when is_tuple(HP) ->
    [_|T] = tuple_to_list(HP),
    map_vars(T, 2);
bound_in_headpat(_) ->
    %% this is not the place to throw an exception
    none.

map_vars([H|T], P) ->
    case extract_vars(H) of
	[] ->
	    map_vars(T, P+1);
	Vs ->
	    [{P, Vs}|map_vars(T, P+1)]
    end;
map_vars([], _) ->
    [].

any_in_body(Vars, BodyVars) ->
    lists:any(fun({_,Vs}) ->
		      intersection(Vs, BodyVars) =/= []
	      end, Vars).

intersection(A,B) when is_list(A), is_list(B) ->
    A -- (A -- B).

wild_in_body(BodyVars) ->
    intersection(BodyVars, ['$$','$_']) =/= [].


extract_vars([H|T]) ->
    extract_vars(H) ++ extract_vars(T);
extract_vars(T) when is_tuple(T) ->
    extract_vars(tuple_to_list(T));
extract_vars(T) when T=='$$'; T=='$_' ->
    [T];
extract_vars(T) when is_atom(T) ->
    case mnesia_sext:is_wild(T) of
	true ->
	    [T];
	false ->
	    []
    end;
extract_vars(_) ->
    [].

remove_ending_slash(D) ->
    case lists:reverse(D) of
	"/" ++ Dr ->
	    lists:reverse(Dr);
	_ ->
	    D
    end.

do_search_dir(Dir, Sel, Acc, N) ->
    case list_dir(Dir, Sel#sel.type) of
	{ok, Fs} ->
	    do_search(Fs, Dir, Sel, Acc, N,
		    fun(Acc1, _) ->
			    {lists:reverse(Acc1), fun() -> '$end_of_table' end}
		    end);
	{error,_} ->
	    '$end_of_table'
    end.

do_search([], _, _, Acc, N, C) ->
    C(Acc, N);
do_search(Fs, Dir, #sel{limit = Lim} = Sel, Acc, 0, C) ->
    {lists:reverse(Acc), fun() ->
				 do_search(Fs, Dir, Sel, [], Lim, C)
			 end};
do_search([F|Fs], Dir, #sel{keypat = KeyPat} = Sel, Acc, N, C) ->
    Filename = filename:join(Dir, F),
    case is_key_prefix(Filename, KeyPat) of
	true ->
	    follow_file(Filename, Fs, Dir, Sel, Acc, N, C);
	false ->
	    do_search(Fs, Dir, Sel, Acc, N, C)
    end.

follow_file(Filename, Fs, Dir, Sel, Acc, N, C) ->
    case list_dir(Filename, Sel#sel.type) of
	{ok, Fs1} ->
	    C1 = fun(Acc1, N1) ->
			 do_search(Fs, Dir, Sel, Acc1, N1, C)
		 end,
	    do_search(Fs1, Filename, Sel, Acc, N, C1);
	{error, enotdir} ->
	    case match_file(Filename, Sel) of
		{match, Result} ->
		    do_search(Fs, Dir, Sel, [Result|Acc], decr(N), C);
		nomatch ->
		    do_search(Fs, Dir, Sel, Acc, N, C)
	    end;
	{error, enoent} ->
	    do_search(Fs, Dir, Sel, Acc, N, C)
    end.

list_dir(Dir, Type) ->
    case {file:list_dir(Dir), Type} of
	{{ok, Fs}, ordered_set} ->
	    {ok, lists:sort(Fs)};
	{Other, _} ->
	    Other
    end.

match_file(Filename, #sel{ms = F, mp = MP, alias = Alias, tab = Tab,
			  recname = RecName}) when is_function(F) ->
    case F(key, Filename) of
	[] ->
	    nomatch;
	[_] ->
	    RelFilename = remove_top(MP, Filename),
	    Read = read_obj(false, Alias, Tab, RecName, Filename, RelFilename),
	    case F(data, Read) of
		[] ->
		    nomatch;
		[Res] ->
		    {match, Res}
	    end
    end;
match_file(Filename, #sel{mp = MP, keypat = {_,KeyPat}, ms = MS,
			  key_only = KeyOnly,
			  alias = Alias, tab = Tab, recname = RecName}) ->
    RelFilename = remove_top(MP, Filename),
    Key = decode_key(RelFilename, Alias, Tab),
    case match_spec_run([Key], KeyPat) of
	[] ->
	    nomatch;
	[_] ->
	    RelFilename = remove_top(MP, Filename),
	    Read = read_obj(KeyOnly, Alias, Tab, RecName, Filename, RelFilename),
	    case match_spec_run(Read, MS) of
		[] ->
		    nomatch;
		[Res] ->
		    {match, Res}
	    end
    end.

match_spec_run(L, MS) ->
    Compiled = ets:match_spec_compile(MS),
    ets:match_spec_run(L, Compiled).

remove_top([H|T],[H|T1]) ->
    remove_top(T, T1);
remove_top([], "/" ++ T) ->
    remove_top([], T);
remove_top([], T) ->
    T.

read_obj(true, _, Tab, _, _, RelName) ->
    [setelement(2, mnesia:table_info(Tab, wild_pattern), RelName)];
read_obj(false, Alias, _Tab, RecName, Filename, RelName) ->
    case file:read_file(Filename) of
	{ok, Binary} ->
	    case Alias of
		raw_fs_copies ->
		    [{RecName, RelName, Binary}];
		fs_copies ->
		    [binary_to_term(Binary)]
	    end;
	_ ->
	    []
    end.

decr(I) when is_integer(I), I > 0 ->
    I - 1;
decr(I) ->
    I.


%% ==============================================================
%% gen_server part
%% ==============================================================

start_proc(Alias, Tab, MP) ->
    gen_server:start_link({local, mk_proc_name(Alias, Tab)}, ?MODULE,
			  {Alias, Tab, MP}, []).

call(Alias, Tab, Req) ->
    case gen_server:call(proc_name(Alias, Tab), Req, infinity) of
	badarg ->
	    mnesia:abort(badarg);
	{abort, _} = Err ->
	    mnesia:abort(Err);
	Reply ->
	    Reply
    end.

init({Alias, Tab, MP}) ->
    {ok, Dets} = open_dets(Alias, Tab, MP),
    mnesia_lib:set({Tab, mountpoint}, MP),
    Ets = ets:new(mk_tab_name(icache,Tab), [set, protected, named_table]),
    dets:to_ets(Dets, Ets),
    St = #st{ets = Ets,
	     dets = Dets,
	     mp = MP,
	     data_mp = data_mountpoint(Tab),
	     alias = Alias,
	     tab = Tab},
    {ok, update_size_info(St)}.

open_dets(_Alias, Tab, _MP) ->
    Dir = info_mountpoint(Tab),
    Name = mk_tab_name(info, Tab),
    Args = [{file, filename:join([Dir, "info.DAT"])},
	    {keypos, 1},
	    {repair, mnesia_monitor:get_env(auto_repair)},
	    {type, set}],
    mnesia_monitor:open_dets(Name, Args).


handle_call({info, Item}, _From, #st{ets = Ets} = St) ->
    Result = case ets:lookup(Ets, {info, Item}) of
		 [{_, Value}] ->
		     Value;
		 [] ->
		     default_info(Item)
	     end,
    {reply, Result, St};
handle_call({write_info, Key, Value}, _From, St) ->
    {reply, write_info(Key, Value, St), St};
handle_call({incr_size, Incr}, _From, St) ->
    incr_size(St, Incr),
    {reply, ok, St};
handle_call({insert, Obj}, _From, #st{data_mp = MP,
				      alias = Alias, tab = Tab} = St) ->
    case do_insert(Alias, Tab, Obj, MP) of
	true ->
	    incr_size(St, 1),
	    {reply, ok, St};
	false ->
	    {reply, ok, St};
	{error, E} ->
	    {reply, {abort, E}, St}
    end;
handle_call({delete, Key}, _From, #st{data_mp = MP,
				      alias = Alias, tab = Tab} = St) ->
    case do_delete(Alias, Tab, Key, MP) of
	true ->
	    incr_size(St, -1);
	false ->
	    ignore
    end,
    {reply, ok, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

terminate(_Reason, _St) ->
    ok.

default_info(size)   -> 0;
default_info(_) -> undefined.

update_size_info(#st{alias = Alias, tab = Tab, data_mp = MP} = St) ->
    PrevInfo = info(Alias, Tab, size),
    io:fwrite("Updating size info of Tab = ~p (~p)...~n", [Tab, PrevInfo]),
    case should_update_size(Tab) of
	true ->
	    Sz = count_files(Alias, Tab, MP),
	    io:fwrite("Size of ~p is ~p~n", [Tab, Sz]),
	    write_info(size, Sz, St);
	{true, {M,F}} ->
	    case apply(M, F, [Alias, Tab, MP]) of
		Sz1 when is_integer(Sz1), Sz1 >= 0 ->
		    io:fwrite("~p:~p(~p,~p,~p) -> ~p~n",
			      [M,F,Alias,Tab,MP, Sz1]),
		    write_info(size, Sz1, St)
	    end;
	false ->
	    io:fwrite("...not updating; keeping old size~n", []),
	    ignore
    end,
    St.

should_update_size(Tab) ->
    try mnesia:read_table_property(Tab, update_size_on_load) of
	{_, B} when is_boolean(B) ->
	    B;
	{_, {true, {M,F}}} ->
	    {true, {M,F}}
    catch
	error:_ -> true;
	exit:_  -> true
    end.

count_files(Alias, Tab, Dir) ->
    case os:type() of
	{unix,_} ->
	    try begin
		    Ret = os:cmd("find " ++ Dir ++ " -type f | wc -l"),
		    [SzStr] = string:tokens(Ret, " \n"),
		    list_to_integer(SzStr)
		end
	    catch
		error:_ ->
		    safe_count_files(Alias, Tab, Dir)
	    end
    end.

safe_count_files(_Alias, _Tab, Dir) ->
    filelib:fold_files(Dir, ".*", true, fun(_,Acc) -> Acc+1 end, 0).

sum_size({L, Cont}, Acc) ->
    sum_size(select(Cont), lists:sum(L) + Acc);
sum_size('$end_of_table', Acc) ->
    Acc.


incr_size(#st{ets = Ets} = St, Incr) ->
    case ets:lookup(Ets, {info,size}) of
	[] ->
	    write_info(size, Incr, St);
	[{_, Val}] ->
	    write_info(size, Val + Incr, St)
    end.

write_info(Item, Value, #st{ets = Ets, dets = Dets}) ->
    % Order matters. First write to disk
    ok = dets:insert(Dets, {{info,Item}, Value}),
    ets:insert(Ets, {{info,Item}, Value}).

tab_name(icache, Tab) ->
    list_to_existing_atom("mnesia_ext_icache_" ++ tabname(Tab));
tab_name(info, Tab) ->
    list_to_existing_atom("mnesia_ext_info_" ++ tabname(Tab)).

mk_tab_name(icache, Tab) ->
    list_to_atom("mnesia_ext_icache_" ++ tabname(Tab));
mk_tab_name(info, Tab) ->
    list_to_atom("mnesia_ext_info_" ++ tabname(Tab)).

proc_name(_Alias, Tab) ->
    list_to_existing_atom("mnesia_ext_proc_" ++ tabname(Tab)).

mk_proc_name(_Alias, Tab) ->
    list_to_atom("mnesia_ext_proc_" ++ tabname(Tab)).

tabname({Tab, index, {Pos,_}}) ->
    atom_to_list(Tab) ++ "-" ++ integer_to_list(Pos) ++ "-_ix";
tabname({Tab, retainer, Name}) ->
    atom_to_list(Tab) ++ "-" ++ ret_name(Name) ++ "-_RET";
tabname(Tab) when is_atom(Tab) ->
    atom_to_list(Tab) ++ "-_tab".

ret_name(Name) ->
    lists:flatten(io_lib:write(Name)).
