-module(mnesia_ext_be_stub).


%% ----------------------------------------------------------------------------
%% BEHAVIOURS
%% ----------------------------------------------------------------------------

-behaviour(mnesia_backend_type).
-behaviour(gen_server).

%%
%% BACKEND CALLBACKS
%%

%% backend management
-export([init_backend/0,
         add_aliases/1,
         remove_aliases/1]).

%% schema level callbacks
-export([semantics/2,
	 check_definition/4,
	 create_table/3,
	 load_table/4,
	 close_table/2,
	 sync_close_table/2,
	 delete_table/2,
	 info/3]).

%% table synch calls
-export([sender_init/4,
         sender_handle_info/5,
         receiver_first_message/4,
         receive_data/5,
         receive_done/4]).

%% low-level accessor callbacks.
-export([delete/3,
         first/2,
         fixtable/3,
         insert/3,
         last/2,
         lookup/3,
         match_delete/3,
         next/3,
         prev/3,
         repair_continuation/2,
         select/1,
         select/3,
         select/4,
         slot/3,
         update_counter/4]).

%% Index consistency
-export([index_is_consistent/3,
         is_index_consistent/2]).

%% record and key validation
-export([validate_key/6,
         validate_record/6]).

%% file extension callbacks
-export([real_suffixes/0,
         tmp_suffixes/0]).

%% Register
-export([register/0]).

%%
%% GEN SERVER CALLBACKS AND CALLS
%%

-export([start_proc/5,
         init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         terminate/2,
         code_change/3]).


%%
%% Register
%%

register() ->
    register(default_alias()).

register(Alias) ->
    Module = ?MODULE,
    case mnesia:add_backend_type(Alias, Module) of
        {atomic, ok} ->
            {ok, Alias};
        {aborted, {backend_type_already_exists, _}} ->
            {ok, Alias};
        {aborted, Reason} ->
            {error, Reason}
    end.

default_alias() ->
    test_mnesia_be.


%% ----------------------------------------------------------------------------
%% BACKEND CALLBACKS
%% ----------------------------------------------------------------------------

%% backend management

init_backend() ->
    ok.

add_aliases(_Aliases) ->
    ok.

remove_aliases(_Aliases) ->
    ok.

semantics(_Alias, storage) -> disc_only_copies;
semantics(_Alias, types) -> [set, ordered_set, bag];
semantics(_Alias, index_types) -> [ordered];
semantics(_Alias, index_fun) -> fun index_f/4;
semantics(_Alias, _) -> undefined.

index_f(_Alias, _Tab, Pos, Obj) ->
    [element(Pos, Obj)].

is_index_consistent(_Alias, _Info) ->
    true.

index_is_consistent(_Alias, _Info, _Bool) ->
    ok.

check_definition(_Alias, _Tab, _Nodes, Props) ->
    {ok, Props}.

create_table(_Alias, _Tab, _Props) ->
    ok.

load_table(_Alias, _Tab, _LoadReason, _Opts) ->
    ok.

close_table(_Alias, _Tab) ->
    ok.

sync_close_table(_Alias, _Tab) ->
    ok.

delete_table(_Alias, _Tab) ->
    ok.

info(_Alias, _Tab, _) ->
    0.

sender_init(_Alias, _Tab, _RemoteStorage, _Pid) ->
    {standard, fun() -> ok end, fun() -> ok end}.

sender_handle_info(_Msg, _Alias, _Tab, _ReceiverPid, Cont) ->
    {fun() -> ok end, Cont}.

receiver_first_message(_Pid, _Msg, _Alias, _Tab) ->
    {0, []}.

receive_data(_Data, _Alias, _Tab, _Sender, State) ->
    {more, State}.

receive_done(_Alias, _Tab, _Sender, _State) -> 
    ok.

repair_continuation(Continuation, _MatchSpec) -> 
    Continuation.

delete(_Alias, _Tab, _Key) ->
    ok.

first(_Alias, _Tab) ->
    '$end_of_table'.

fixtable(_Alias, _Tab, _Bool) ->
    true.

insert(_Alias, _Tab, _Obj) ->
    ok.

last(_Alias, _Tab) ->
    '$end_of_table'.

lookup(_Alias, _Tab, _Key) ->
    [].

match_delete(_Alias, _Tab, _Pat) ->
    ok.


next(_Alias, _Tab, _Key) ->
    '$end_of_table'.

prev(_Alias, _Tab, _Key) ->
    '$end_of_table'.

select(_Cont) ->
    '$end_of_table'.

select(_Alias, _Tab, _Ms) ->
    '$end_of_table'.

select(_Alias, _Tab, _Ms, _Limit) ->
    '$end_of_table'.

slot(_Alias, _Tab, _Pos) ->
    '$end_of_table'.

update_counter(_Alias, _Tab, _Counter, _Val) -> 
    0.


validate_key(_Alias, _Tab, RecName, Arity, Type, _Key) ->
    {RecName, Arity, Type}.

validate_record(_Alias, _Tab, RecName, Arity, Type, _Obj) ->
    {RecName, Arity, Type}.


real_suffixes() ->
    [".test"].

tmp_suffixes() ->
    [].



%% ----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS AND CALLS
%% ----------------------------------------------------------------------------

start_proc(Alias, Tab, Type, LdbOpts, RecName) ->
    ProcName = proc_name(Alias, Tab),
    gen_server:start_link({local, ProcName}, ?MODULE,
                          {Alias, Tab, Type, LdbOpts, RecName}, []).

init({Alias, Tab, Type, LdbOpts, RecName}) ->
    {ok, #{alias => Alias,
	   tab => Tab,
	   type => Type,
	   ldbopts => LdbOpts,
	   rec_name => RecName
	  }}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _) ->
    ok.


proc_name(_Alias, Tab) ->
    list_to_atom("mnesia_ext_proc_" ++ atom_to_list(Tab)).
