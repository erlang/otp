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
-module(ets).

%% Interface to the Term store BIF's
%% ets == Erlang Term Store

-export([file2tab/1,
	 file2tab/2,
	 filter/3,
	 foldl/3, foldr/3,
	 match_delete/2,
	 tab2file/2,
	 tab2file/3,
	 tabfile_info/1,
	 from_dets/2,
	 to_dets/2,
	 init_table/2,
	 test_ms/2,
	 tab2list/1,
         table/1,
         table/2,
	 fun2ms/1,
	 match_spec_run/2,
	 repair_continuation/2]).

-export([i/0, i/1, i/2, i/3]).

-export_type([tab/0, tid/0, match_spec/0, comp_match_spec/0, match_pattern/0]).

%%-----------------------------------------------------------------------------

-type access()     :: public | protected | private.
-type tab()        :: atom() | tid().
-type type()       :: set | ordered_set | bag | duplicate_bag.
-type continuation() :: '$end_of_table'
                      | {tab(),integer(),integer(),comp_match_spec(),list(),integer()}
                      | {tab(),_,_,integer(),comp_match_spec(),list(),integer(),integer()}.

-opaque tid()      :: reference().

-type match_pattern() :: atom() | tuple().
-type match_spec()    :: [{match_pattern(), [_], [_]}].

%%-----------------------------------------------------------------------------

%%% BIFs

-export([all/0, delete/1, delete/2, delete_all_objects/1,
         delete_object/2, first/1, give_away/3, info/1, info/2,
         insert/2, insert_new/2, is_compiled_ms/1, last/1, lookup/2,
         lookup_element/3, match/1, match/2, match/3, match_object/1,
         match_object/2, match_object/3, match_spec_compile/1,
         match_spec_run_r/3, member/2, new/2, next/2, prev/2,
         rename/2, safe_fixtable/2, select/1, select/2, select/3,
         select_count/2, select_delete/2, select_replace/2, select_reverse/1,
         select_reverse/2, select_reverse/3, setopts/2, slot/2,
         take/2,
         update_counter/3, update_counter/4, update_element/3,
         whereis/1]).

%% internal exports
-export([internal_request_all/0]).

-spec all() -> [Tab] when
      Tab :: tab().

all() ->
    receive_all(ets:internal_request_all(),
                erlang:system_info(schedulers),
                []).

receive_all(_Ref, 0, All) ->
    All;
receive_all(Ref, N, All) ->
    receive
        {Ref, SchedAll} ->
            receive_all(Ref, N-1, SchedAll ++ All)
    end.

-spec internal_request_all() -> reference().

internal_request_all() ->
    erlang:nif_error(undef).

-spec delete(Tab) -> true when
      Tab :: tab().

delete(_) ->
    erlang:nif_error(undef).

-spec delete(Tab, Key) -> true when
      Tab :: tab(),
      Key :: term().

delete(_, _) ->
    erlang:nif_error(undef).

-spec delete_all_objects(Tab) -> true when
      Tab :: tab().

delete_all_objects(_) ->
    erlang:nif_error(undef).

-spec delete_object(Tab, Object) -> true when
      Tab :: tab(),
      Object :: tuple().

delete_object(_, _) ->
    erlang:nif_error(undef).

-spec first(Tab) -> Key | '$end_of_table' when
      Tab :: tab(),
      Key :: term().

first(_) ->
    erlang:nif_error(undef).

-spec give_away(Tab, Pid, GiftData) -> true when
      Tab :: tab(),
      Pid :: pid(),
      GiftData :: term().

give_away(_, _, _) ->
    erlang:nif_error(undef).

-spec info(Tab) -> InfoList | undefined when
      Tab :: tab(),
      InfoList :: [InfoTuple],
      InfoTuple :: {compressed, boolean()}
                 | {heir, pid() | none}
                 | {id, tid()}
                 | {keypos, pos_integer()}
                 | {memory, non_neg_integer()}
                 | {name, atom()}
                 | {named_table, boolean()}
                 | {node, node()}
                 | {owner, pid()}
                 | {protection, access()}
                 | {size, non_neg_integer()}
                 | {type, type()}
		 | {write_concurrency, boolean()}
		 | {read_concurrency, boolean()}.

info(_) ->
    erlang:nif_error(undef).

-spec info(Tab, Item) -> Value | undefined when
      Tab :: tab(),
      Item :: compressed | fixed | heir | id | keypos | memory
            | name | named_table | node | owner | protection
            | safe_fixed | safe_fixed_monotonic_time | size | stats | type
	    | write_concurrency | read_concurrency,
      Value :: term().

info(_, _) ->
    erlang:nif_error(undef).

-spec insert(Tab, ObjectOrObjects) -> true when
      Tab :: tab(),
      ObjectOrObjects :: tuple() | [tuple()].

insert(_, _) ->
    erlang:nif_error(undef).

-spec insert_new(Tab, ObjectOrObjects) -> boolean() when
      Tab :: tab(),
      ObjectOrObjects :: tuple() | [tuple()].

insert_new(_, _) ->
    erlang:nif_error(undef).

-spec is_compiled_ms(Term) -> boolean() when
      Term :: term().

is_compiled_ms(_) ->
    erlang:nif_error(undef).

-spec last(Tab) -> Key | '$end_of_table' when
      Tab :: tab(),
      Key :: term().

last(_) ->
    erlang:nif_error(undef).

-spec lookup(Tab, Key) -> [Object] when
      Tab :: tab(),
      Key :: term(),
      Object :: tuple().

lookup(_, _) ->
    erlang:nif_error(undef).

-spec lookup_element(Tab, Key, Pos) -> Elem when
      Tab :: tab(),
      Key :: term(),
      Pos :: pos_integer(),
      Elem :: term() | [term()].

lookup_element(_, _, _) ->
    erlang:nif_error(undef).

-spec match(Tab, Pattern) -> [Match] when
      Tab :: tab(),
      Pattern :: match_pattern(),
      Match :: [term()].

match(_, _) ->
    erlang:nif_error(undef).

-spec match(Tab, Pattern, Limit) -> {[Match], Continuation} |
                                       '$end_of_table'  when
      Tab :: tab(),
      Pattern :: match_pattern(),
      Limit :: pos_integer(),
      Match :: [term()],
      Continuation :: continuation().

match(_, _, _) ->
    erlang:nif_error(undef).

-spec match(Continuation) -> {[Match], Continuation} |
                                '$end_of_table'  when
      Match :: [term()],
      Continuation :: continuation().

match(_) ->
    erlang:nif_error(undef).

-spec match_object(Tab, Pattern) -> [Object] when
      Tab :: tab(),
      Pattern :: match_pattern(),
      Object :: tuple().

match_object(_, _) ->
    erlang:nif_error(undef).

-spec match_object(Tab, Pattern, Limit) -> {[Object], Continuation} |
                                           '$end_of_table' when
      Tab :: tab(),
      Pattern :: match_pattern(),
      Limit :: pos_integer(),
      Object :: tuple(),
      Continuation :: continuation().

match_object(_, _, _) ->
    erlang:nif_error(undef).

-spec match_object(Continuation) -> {[Object], Continuation} |
                                    '$end_of_table' when
      Object :: tuple(),
      Continuation :: continuation().

match_object(_) ->
    erlang:nif_error(undef).

-spec match_spec_compile(MatchSpec) -> CompiledMatchSpec when
      MatchSpec :: match_spec(),
      CompiledMatchSpec :: comp_match_spec().

match_spec_compile(_) ->
    erlang:nif_error(undef).

-spec match_spec_run_r(List, CompiledMatchSpec, list()) -> list() when
      List :: [term()],
      CompiledMatchSpec :: comp_match_spec().

match_spec_run_r(_, _, _) ->
    erlang:nif_error(undef).

-spec member(Tab, Key) -> boolean() when
      Tab :: tab(),
      Key :: term().

member(_, _) ->
    erlang:nif_error(undef).

-spec new(Name, Options) -> tid() | atom() when
      Name :: atom(),
      Options :: [Option],
      Option :: Type | Access | named_table | {keypos,Pos}
              | {heir, Pid :: pid(), HeirData} | {heir, none} | Tweaks,
      Type :: type(),
      Access :: access(),
      Tweaks :: {write_concurrency, boolean()}
              | {read_concurrency, boolean()}
              | compressed,
      Pos :: pos_integer(),
      HeirData :: term().

new(_, _) ->
    erlang:nif_error(undef).

-spec next(Tab, Key1) -> Key2 | '$end_of_table' when
      Tab :: tab(),
      Key1 :: term(),
      Key2 :: term().

next(_, _) ->
    erlang:nif_error(undef).

-spec prev(Tab, Key1) -> Key2 | '$end_of_table' when
      Tab :: tab(),
      Key1 :: term(),
      Key2 :: term().

prev(_, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: ets:rename/2
-spec rename(Tab, Name) -> Name when
      Tab :: tab(),
      Name :: atom().

rename(_, _) ->
    erlang:nif_error(undef).

-spec safe_fixtable(Tab, Fix) -> true when
      Tab :: tab(),
      Fix :: boolean().

safe_fixtable(_, _) ->
    erlang:nif_error(undef).

-spec select(Tab, MatchSpec) -> [Match] when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      Match :: term().

select(_, _) ->
    erlang:nif_error(undef).

-spec select(Tab, MatchSpec, Limit) -> {[Match],Continuation} |
                                       '$end_of_table' when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      Limit :: pos_integer(),
      Match :: term(),
      Continuation :: continuation().

select(_, _, _) ->
    erlang:nif_error(undef).

-spec select(Continuation) -> {[Match],Continuation} | '$end_of_table' when
      Match :: term(),
      Continuation :: continuation().

select(_) ->
    erlang:nif_error(undef).

-spec select_count(Tab, MatchSpec) -> NumMatched when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      NumMatched :: non_neg_integer().

select_count(_, _) ->
    erlang:nif_error(undef).

-spec select_delete(Tab, MatchSpec) -> NumDeleted when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      NumDeleted :: non_neg_integer().

select_delete(_, _) ->
    erlang:nif_error(undef).

-spec select_replace(Tab, MatchSpec) -> NumReplaced when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      NumReplaced :: non_neg_integer().

select_replace(_, _) ->
    erlang:nif_error(undef).

-spec select_reverse(Tab, MatchSpec) -> [Match] when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      Match :: term().

select_reverse(_, _) ->
    erlang:nif_error(undef).

-spec select_reverse(Tab, MatchSpec, Limit) -> {[Match],Continuation} |
                                               '$end_of_table' when
      Tab :: tab(),
      MatchSpec :: match_spec(),
      Limit :: pos_integer(),
      Match :: term(),
      Continuation :: continuation().

select_reverse(_, _, _) ->
    erlang:nif_error(undef).

-spec select_reverse(Continuation) -> {[Match],Continuation} |
                                      '$end_of_table' when
      Continuation :: continuation(),
      Match :: term().

select_reverse(_) ->
    erlang:nif_error(undef).

-spec setopts(Tab, Opts) -> true when
      Tab :: tab(),
      Opts :: Opt | [Opt],
      Opt :: {heir, pid(), HeirData} | {heir,none},
      HeirData :: term().

setopts(_, _) ->
    erlang:nif_error(undef).

-spec slot(Tab, I) -> [Object] | '$end_of_table' when
      Tab :: tab(),
      I :: non_neg_integer(),
      Object :: tuple().

slot(_, _) ->
    erlang:nif_error(undef).

-spec take(Tab, Key) -> [Object] when
      Tab :: tab(),
      Key :: term(),
      Object :: tuple().

take(_, _) ->
    erlang:nif_error(undef).

-spec update_counter(Tab, Key, UpdateOp) -> Result when
      Tab :: tab(),
      Key :: term(),
      UpdateOp :: {Pos, Incr} | {Pos, Incr, Threshold, SetValue},
      Pos :: integer(),
      Incr :: integer(),
      Threshold :: integer(),
      SetValue :: integer(),
      Result :: integer();
                       (Tab, Key, [UpdateOp]) -> [Result] when
      Tab :: tab(),
      Key :: term(),
      UpdateOp :: {Pos, Incr} | {Pos, Incr, Threshold, SetValue},
      Pos :: integer(),
      Incr :: integer(),
      Threshold :: integer(),
      SetValue :: integer(),
      Result :: integer();
                       (Tab, Key, Incr) -> Result when
      Tab :: tab(),
      Key :: term(),
      Incr :: integer(),
      Result :: integer().

update_counter(_, _, _) ->
    erlang:nif_error(undef).

-spec update_counter(Tab, Key, UpdateOp, Default) -> Result when
                        Tab :: tab(),
                        Key :: term(),
                        UpdateOp :: {Pos, Incr}
                                  | {Pos, Incr, Threshold, SetValue},
                        Pos :: integer(),
                        Incr :: integer(),
                        Threshold :: integer(),
                        SetValue :: integer(),
                        Result :: integer(),
                        Default :: tuple();
                    (Tab, Key, [UpdateOp], Default) -> [Result] when
                        Tab :: tab(),
                        Key :: term(),
                        UpdateOp :: {Pos, Incr}
                                  | {Pos, Incr, Threshold, SetValue},
                        Pos :: integer(),
                        Incr :: integer(),
                        Threshold :: integer(),
                        SetValue :: integer(),
                        Result :: integer(),
                        Default :: tuple();
                    (Tab, Key, Incr, Default) -> Result when
                        Tab :: tab(),
                        Key :: term(),
                        Incr :: integer(),
                        Result :: integer(),
                        Default :: tuple().

update_counter(_, _, _, _) ->
    erlang:nif_error(undef).

-spec update_element(Tab, Key, ElementSpec :: {Pos, Value}) -> boolean() when
      Tab :: tab(),
      Key :: term(),
      Pos :: pos_integer(),
      Value :: term();
                       (Tab, Key, ElementSpec :: [{Pos, Value}]) -> boolean() when
      Tab :: tab(),
      Key :: term(),
      Pos :: pos_integer(),
      Value :: term().

update_element(_, _, _) ->
    erlang:nif_error(undef).

-spec whereis(TableName) -> tid() | undefined when
    TableName :: atom().
whereis(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

-opaque comp_match_spec() :: reference().

-spec match_spec_run(List, CompiledMatchSpec) -> list() when
      List :: [term()],
      CompiledMatchSpec :: comp_match_spec().

match_spec_run(List, CompiledMS) ->
    lists:reverse(ets:match_spec_run_r(List, CompiledMS, [])).

-spec repair_continuation(Continuation, MatchSpec) -> Continuation when
      Continuation :: continuation(),
      MatchSpec :: match_spec().

%% $end_of_table is an allowed continuation in ets...
repair_continuation('$end_of_table', _) ->
    '$end_of_table';
%% ordered_set
repair_continuation(Untouched = {Table,Lastkey,EndCondition,N2,MSRef,L2,N3,N4}, MS)
  when %% (is_atom(Table) or is_integer(Table)),
       is_integer(N2),
      %% is_reference(MSRef),
       is_list(L2),
       is_integer(N3),
       is_integer(N4) ->
    case ets:is_compiled_ms(MSRef) of
	true ->
	    Untouched;
	false ->
	    {Table,Lastkey,EndCondition,N2,ets:match_spec_compile(MS),L2,N3,N4}
    end;
%% set/bag/duplicate_bag
repair_continuation(Untouched = {Table,N1,N2,MSRef,L,N3}, MS)
  when %% (is_atom(Table) or is_integer(Table)),
       is_integer(N1),
       is_integer(N2),
      %% is_reference(MSRef),
       is_list(L),
       is_integer(N3) ->
    case ets:is_compiled_ms(MSRef) of
	true ->
	    Untouched;
	false ->
	    {Table,N1,N2,ets:match_spec_compile(MS),L,N3}
    end.

-spec fun2ms(LiteralFun) -> MatchSpec when
      LiteralFun :: function(),
      MatchSpec :: match_spec().

fun2ms(ShellFun) when is_function(ShellFun) ->
    %% Check that this is really a shell fun...
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   ?MODULE,Clauses,ImportList) of
                {error,[{_,[{_,_,Code}|_]}|_],_} ->
                    io:format("Error: ~ts~n",
                              [ms_transform:format_error(Code)]),
                    {error,transform_error};
                Else ->
                    Else
            end;
        _ ->
            exit({badarg,{?MODULE,fun2ms,
                          [function,called,with,real,'fun',
                           should,be,transformed,with,
                           parse_transform,'or',called,with,
                           a,'fun',generated,in,the,
                           shell]}})
    end.

-spec foldl(Function, Acc0, Tab) -> Acc1 when
      Function :: fun((Element :: term(), AccIn) -> AccOut),
      Tab :: tab(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

foldl(F, Accu, T) ->
    ets:safe_fixtable(T, true),
    First = ets:first(T),
    try
        do_foldl(F, Accu, First, T)
    after
	ets:safe_fixtable(T, false)
    end.

do_foldl(F, Accu0, Key, T) ->
    case Key of
	'$end_of_table' ->
	    Accu0;
	_ ->
	    do_foldl(F,
		     lists:foldl(F, Accu0, ets:lookup(T, Key)),
		     ets:next(T, Key), T)
    end.

-spec foldr(Function, Acc0, Tab) -> Acc1 when
      Function :: fun((Element :: term(), AccIn) -> AccOut),
      Tab :: tab(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

foldr(F, Accu, T) ->
    ets:safe_fixtable(T, true),
    Last = ets:last(T),
    try
        do_foldr(F, Accu, Last, T)
    after 
        ets:safe_fixtable(T, false)
    end.

do_foldr(F, Accu0, Key, T) ->
    case Key of
	'$end_of_table' ->
	    Accu0;
	_ ->
	    do_foldr(F,
		     lists:foldr(F, Accu0, ets:lookup(T, Key)),
		     ets:prev(T, Key), T)
    end.

-spec from_dets(Tab, DetsTab) -> 'true' when
      Tab :: tab(),
      DetsTab :: dets:tab_name().

from_dets(EtsTable, DetsTable) ->
    case (catch dets:to_ets(DetsTable, EtsTable)) of
	{error, Reason} ->
	    erlang:error(Reason, [EtsTable,DetsTable]);
	{'EXIT', {Reason1, _Stack1}} ->
	    erlang:error(Reason1,[EtsTable,DetsTable]);
	{'EXIT', EReason} ->
	    erlang:error(EReason,[EtsTable,DetsTable]);
	EtsTable ->
	    true;
	Unexpected -> %% Dets bug?
	    erlang:error(Unexpected,[EtsTable,DetsTable])
    end.

-spec to_dets(Tab, DetsTab) -> DetsTab when
      Tab :: tab(),
      DetsTab :: dets:tab_name().

to_dets(EtsTable, DetsTable) ->
    case (catch dets:from_ets(DetsTable, EtsTable)) of
	{error, Reason} ->
	    erlang:error(Reason, [EtsTable,DetsTable]);
	{'EXIT', {Reason1, _Stack1}} ->
	    erlang:error(Reason1,[EtsTable,DetsTable]);
	{'EXIT', EReason} ->
	    erlang:error(EReason,[EtsTable,DetsTable]);
	ok ->
	    DetsTable;
	Unexpected -> %% Dets bug?
	    erlang:error(Unexpected,[EtsTable,DetsTable])
    end.

-spec test_ms(Tuple, MatchSpec) -> {'ok', Result} | {'error', Errors} when
      Tuple :: tuple(),
      MatchSpec :: match_spec(),
      Result :: term(),
      Errors :: [{'warning'|'error', string()}].

test_ms(Term, MS) ->
    case erlang:match_spec_test(Term, MS, table) of
	{ok, Result, _Flags, _Messages} ->
	    {ok, Result};
	{error, _Errors} = Error ->
	    Error
    end.

-spec init_table(Tab, InitFun) -> 'true' when
      Tab :: tab(),
      InitFun :: fun((Arg) -> Res),
      Arg :: 'read' | 'close',
      Res :: 'end_of_input' | {Objects :: [term()], InitFun} | term().

init_table(Table, Fun) ->
    ets:delete_all_objects(Table),
    init_table_continue(Table, Fun(read)).

init_table_continue(_Table, end_of_input) ->
    true;
init_table_continue(Table, {List, Fun}) when is_list(List), is_function(Fun) ->
    case (catch init_table_sub(Table, List)) of
	{'EXIT', Reason} ->
	    (catch Fun(close)),
	    exit(Reason);
	true ->
	    init_table_continue(Table, Fun(read))
    end;
init_table_continue(_Table, Error) ->
    exit(Error).

init_table_sub(_Table, []) ->
    true;
init_table_sub(Table, [H|T]) ->
    ets:insert(Table, H),
    init_table_sub(Table, T).

-spec match_delete(Tab, Pattern) -> 'true' when
      Tab :: tab(),
      Pattern :: match_pattern().

match_delete(Table, Pattern) ->
    ets:select_delete(Table, [{Pattern,[],[true]}]),
    true.

%% Produce a list of tuples from a table

-spec tab2list(Tab) -> [Object] when
      Tab :: tab(),
      Object :: tuple().

tab2list(T) ->
    ets:match_object(T, '_').

-spec filter(tab(), function(), [term()]) -> [term()].

filter(Tn, F, A) when is_atom(Tn) ; is_integer(Tn) ->
    do_filter(Tn, ets:first(Tn), F, A, []).

do_filter(_Tab, '$end_of_table', _, _, Ack) -> 
    Ack;
do_filter(Tab, Key, F, A, Ack) ->
    case apply(F, [ets:lookup(Tab, Key)|A]) of
	false ->
	    do_filter(Tab, ets:next(Tab, Key), F, A, Ack);
	true ->
            Ack2 = ets:lookup(Tab, Key) ++ Ack,
	    do_filter(Tab, ets:next(Tab, Key), F, A, Ack2);
	{true, Value} ->
	    do_filter(Tab, ets:next(Tab, Key), F, A, [Value|Ack])
    end.

    
%% Dump a table to a file using the disk_log facility

%% Options := [Option]
%% Option := {extended_info,[ExtInfo]}
%% ExtInfo := object_count | md5sum

-define(MAJOR_F2T_VERSION,1).
-define(MINOR_F2T_VERSION,0).

-record(filetab_options,
	{
	  object_count = false :: boolean(),
	  md5sum       = false :: boolean(),
	  sync         = false :: boolean()
	 }).

-spec tab2file(Tab, Filename) -> 'ok' | {'error', Reason} when
      Tab :: tab(),
      Filename :: file:name(),
      Reason :: term().

tab2file(Tab, File) ->
    tab2file(Tab, File, []).

-spec tab2file(Tab, Filename, Options) -> 'ok' | {'error', Reason} when
      Tab :: tab(),
      Filename :: file:name(),
      Options :: [Option],
      Option :: {'extended_info', [ExtInfo]} | {'sync', boolean()},
      ExtInfo :: 'md5sum' | 'object_count',
      Reason :: term().

tab2file(Tab, File, Options) ->
    try
	{ok, FtOptions} = parse_ft_options(Options),
	_ = file:delete(File),
	case file:read_file_info(File) of
	    {error, enoent} -> ok;
	    _ -> throw(eaccess)
	end,
	Name = make_ref(),
	case disk_log:open([{name, Name}, {file, File}]) of
	    {ok, Name} ->
		ok;
	    {error, Reason} ->
		throw(Reason)
	end,
	try
	    Info0 = case ets:info(Tab) of
		       undefined ->
			   %% erlang:error(badarg, [Tab, File, Options]);
			   throw(badtab);
		       I ->
			   I
	    end,
	    Info = [list_to_tuple(Info0 ++ 
				  [{major_version,?MAJOR_F2T_VERSION},
				   {minor_version,?MINOR_F2T_VERSION},
				   {extended_info, 
				    ft_options_to_list(FtOptions)}])],
	    {LogFun, InitState} = 
	    case FtOptions#filetab_options.md5sum of
		true ->
		    {fun(Oldstate,Termlist) ->
			     {NewState,BinList} = 
				 md5terms(Oldstate,Termlist),
                             case disk_log:blog_terms(Name,BinList) of
                                 ok -> NewState;
                                 {error, Reason2} -> throw(Reason2)
                             end
		     end,
		     erlang:md5_init()};
		false ->
		    {fun(_,Termlist) ->
                             case disk_log:log_terms(Name,Termlist) of
                                 ok -> true;
                                 {error, Reason2} -> throw(Reason2)
                             end
		     end, 
		     true}
	    end,
	    ets:safe_fixtable(Tab,true),
	    {NewState1,Num} = try
				  NewState = LogFun(InitState,Info),
				  dump_file(
				      ets:select(Tab,[{'_',[],['$_']}],100),
				      LogFun, NewState, 0)
			      after
				  (catch ets:safe_fixtable(Tab,false))
			      end,
	    EndInfo = 
	    case  FtOptions#filetab_options.object_count of
		true ->
		    [{count,Num}];
		false ->
		    []
	    end ++
	    case  FtOptions#filetab_options.md5sum of
		true ->
		    [{md5,erlang:md5_final(NewState1)}];
		false ->
		    []
	    end,
	    case EndInfo of
		[] ->
		    ok;
		List ->
		    LogFun(NewState1,[['$end_of_table',List]])
	    end,
	    case FtOptions#filetab_options.sync of
	        true ->
		    case disk_log:sync(Name) of
		        ok -> ok;
			{error, Reason2} -> throw(Reason2)
		    end;
                false ->
		    ok
            end,
	    disk_log:close(Name)
	catch
	    throw:TReason ->
		_ = disk_log:close(Name),
		_ = file:delete(File),
		throw(TReason);
	    exit:ExReason ->
		_ = disk_log:close(Name),
		_ = file:delete(File),
		exit(ExReason);
	    error:ErReason:StackTrace ->
		_ = disk_log:close(Name),
		_ = file:delete(File),
	        erlang:raise(error,ErReason,StackTrace)
	end
    catch
	throw:TReason2 ->
	    {error,TReason2};
	exit:ExReason2 ->
	    {error,ExReason2}
    end.

dump_file('$end_of_table', _LogFun, State, Num) ->
    {State,Num};
dump_file({Terms, Context}, LogFun, State, Num) ->
    Count = length(Terms),
    NewState = LogFun(State, Terms),
    dump_file(ets:select(Context), LogFun, NewState, Num + Count).

ft_options_to_list(#filetab_options{md5sum = MD5, object_count = PS}) ->
    case PS of
	true ->
	    [object_count]; 
	_ ->
	    []
    end ++
	case MD5 of
	    true ->
		[md5sum]; 
	    _ ->
		[]
	end.

md5terms(State, []) ->
    {State, []};
md5terms(State, [H|T]) ->
    B = term_to_binary(H),
    NewState = erlang:md5_update(State, B),
    {FinState, TL} = md5terms(NewState, T),
    {FinState, [B|TL]}.

parse_ft_options(Options) when is_list(Options) ->
    {ok, parse_ft_options(Options, #filetab_options{}, false)}.

parse_ft_options([], FtOpt, _) ->
    FtOpt;
parse_ft_options([{sync,true} | Rest], FtOpt, EI) ->
    parse_ft_options(Rest, FtOpt#filetab_options{sync = true}, EI);
parse_ft_options([{sync,false} | Rest], FtOpt, EI) ->
    parse_ft_options(Rest, FtOpt, EI);
parse_ft_options([{extended_info,L} | Rest], FtOpt0, false) ->
    FtOpt1 = parse_ft_info_options(FtOpt0, L),
    parse_ft_options(Rest, FtOpt1, true);
parse_ft_options([Other | _], _, _) ->
    throw({unknown_option, Other});
parse_ft_options(Malformed, _, _) ->
    throw({malformed_option, Malformed}).

parse_ft_info_options(FtOpt,[]) ->
    FtOpt;
parse_ft_info_options(FtOpt,[object_count | T]) ->
    parse_ft_info_options(FtOpt#filetab_options{object_count = true}, T);
parse_ft_info_options(FtOpt,[md5sum | T]) ->
    parse_ft_info_options(FtOpt#filetab_options{md5sum = true}, T);
parse_ft_info_options(_,[Unexpected | _]) ->
    throw({unknown_option,[{extended_info,[Unexpected]}]});
parse_ft_info_options(_,Malformed) ->
    throw({malformed_option,Malformed}).
		     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read a dumped file from disk and create a corresponding table
%% Opts := [Opt]
%% Opt := {verify,boolean()}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec file2tab(Filename) -> {'ok', Tab} | {'error', Reason} when
      Filename :: file:name(),
      Tab :: tab(),
      Reason :: term().

file2tab(File) ->
    file2tab(File, []).

-spec file2tab(Filename, Options) -> {'ok', Tab} | {'error', Reason} when
      Filename :: file:name(),
      Tab :: tab(),
      Options :: [Option],
      Option :: {'verify', boolean()},
      Reason :: term().

file2tab(File, Opts) ->
    try
	{ok,Verify,TabArg} = parse_f2t_opts(Opts,false,[]),
	Name = make_ref(),
        {ok, Name} =
	    case disk_log:open([{name, Name}, 
				{file, File}, 
				{mode, read_only}]) of
		{ok, Name} ->
                    {ok, Name};
		{repaired, Name, _,_} -> %Uh? cannot happen?
		    case Verify of
			true ->
			    _ = disk_log:close(Name),
			    throw(badfile);
			false ->
                            {ok, Name}
		    end;
		{error, Other1} ->
		    throw({read_error, Other1});
		Other2 ->
		    throw(Other2)
	    end,
	{ok, Major, Minor, FtOptions, MD5State, FullHeader, DLContext} =
            try get_header_data(Name, Verify)
            catch
                badfile ->
                    _ = disk_log:close(Name),
                    throw(badfile)
            end,
	try
	    if  
		Major > ?MAJOR_F2T_VERSION -> 
		    throw({unsupported_file_version,{Major,Minor}});
		true ->
		    ok
	    end,
	    {ok, Tab, HeadCount} = create_tab(FullHeader, TabArg),
	    StrippedOptions = 				   
	        case Verify of
		    true ->
			FtOptions;
		    false ->
			#filetab_options{}
		end,
	    {ReadFun,InitState} = 
	        case StrippedOptions#filetab_options.md5sum of
		    true ->
			{fun({OldMD5State,OldCount,_OL,ODLContext} = OS) ->
				 case wrap_bchunk(Name,ODLContext,100,Verify) of
				     eof ->
					 {OS,[]};
				     {NDLContext,Blist} ->
					 {Termlist, NewMD5State, 
					  NewCount,NewLast} =
					     md5_and_convert(Blist,
							     OldMD5State,
							     OldCount),
					 {{NewMD5State, NewCount,
					   NewLast,NDLContext},
					  Termlist}
				 end
			 end,
			 {MD5State,0,[],DLContext}};
		    false ->
			{fun({_,OldCount,_OL,ODLContext} = OS) ->
				 case wrap_chunk(Name,ODLContext,100,Verify) of
				     eof ->
					 {OS,[]};
				     {NDLContext,List} ->
					 {NewLast,NewCount,NewList} = 
					     scan_for_endinfo(List, OldCount),
					 {{false,NewCount,NewLast,NDLContext},
					  NewList}
				 end
			 end,
			 {false,0,[],DLContext}}
		end,
	    try
		do_read_and_verify(ReadFun,InitState,Tab,
				   StrippedOptions,HeadCount,Verify)
	    catch
		throw:TReason ->
		    ets:delete(Tab),    
		    throw(TReason);
		exit:ExReason ->
		    ets:delete(Tab),
		    exit(ExReason);
		error:ErReason:StackTrace ->
		    ets:delete(Tab),
		    erlang:raise(error,ErReason,StackTrace)
	    end
	after
	    _ = disk_log:close(Name)
	end
    catch
	throw:TReason2 ->
	    {error,TReason2};
	exit:ExReason2 ->
	    {error,ExReason2}
    end.

do_read_and_verify(ReadFun,InitState,Tab,FtOptions,HeadCount,Verify) ->
    case load_table(ReadFun,InitState,Tab) of
	{ok,{_,FinalCount,[],_}} ->
	    case {FtOptions#filetab_options.md5sum,
		  FtOptions#filetab_options.object_count} of
		{false,false} ->
		    case Verify of
			false ->
			    ok;
			true ->
			    case FinalCount of
				HeadCount ->
				    ok;
				_ ->
				    throw(invalid_object_count)
			    end
		    end;
		_ ->
		    throw(badfile)
	    end,
	    {ok,Tab};
	{ok,{FinalMD5State,FinalCount,['$end_of_table',LastInfo],_}} ->
	    ECount = case lists:keyfind(count,1,LastInfo) of
			 {count,N} ->
			     N;
			 _ ->
			     false
		     end,
	    EMD5 = case lists:keyfind(md5,1,LastInfo) of
			 {md5,M} ->
			     M;
			 _ ->
			     false
		     end,
	    case FtOptions#filetab_options.md5sum of
		true ->
		    case erlang:md5_final(FinalMD5State) of
			EMD5 ->
			    ok;
			_MD5MisM ->
			    throw(checksum_error)
		    end;
		false ->
		    ok
	    end,
	    case FtOptions#filetab_options.object_count of
		true ->
		    case FinalCount of
			ECount ->
			    ok;
			_Other ->
			    throw(invalid_object_count)
		    end;
		false ->
		    %% Only use header count if no extended info
		    %% at all is present and verification is requested.
		    case {Verify,FtOptions#filetab_options.md5sum} of
			{true,false} ->
			    case FinalCount of
				HeadCount ->
				    ok;
				_Other2 ->
				     throw(invalid_object_count)
			    end;
			_ ->
			    ok
		    end
	    end,
	    {ok,Tab}
    end.

parse_f2t_opts([],Verify,Tab) ->
    {ok,Verify,Tab};
parse_f2t_opts([{verify, true}|T],_OV,Tab) ->
    parse_f2t_opts(T,true,Tab);
parse_f2t_opts([{verify,false}|T],OV,Tab) ->
    parse_f2t_opts(T,OV,Tab);
parse_f2t_opts([{table,Tab}|T],OV,[]) ->
    parse_f2t_opts(T,OV,Tab);
parse_f2t_opts([Unexpected|_],_,_) ->
    throw({unknown_option,Unexpected});
parse_f2t_opts(Malformed,_,_) ->
    throw({malformed_option,Malformed}).
			   
count_mandatory([]) ->
    0;
count_mandatory([{Tag,_}|T]) when Tag =:= name;
				  Tag =:= type;
				  Tag =:= protection;
				  Tag =:= named_table;
				  Tag =:= keypos;
				  Tag =:= size ->
    1+count_mandatory(T);
count_mandatory([_|T]) ->
    count_mandatory(T).
				   
verify_header_mandatory(L) ->						 
    count_mandatory(L) =:= 6.

wrap_bchunk(Name,C,N,true) ->
    case disk_log:bchunk(Name,C,N) of
	{_,_,X} when X > 0 ->
	    throw(badfile);
	{NC,Bin,_} ->
	    {NC,Bin};
	Y ->
	    Y
    end;
wrap_bchunk(Name,C,N,false) ->
    case disk_log:bchunk(Name,C,N) of
	{NC,Bin,_} ->
	    {NC,Bin};
	Y ->
	    Y
    end.
					  
wrap_chunk(Name,C,N,true) ->
    case disk_log:chunk(Name,C,N) of
	{_,_,X} when X > 0 ->
	    throw(badfile);
	{NC,TL,_} ->
	    {NC,TL};
	Y ->
	    Y
    end;
wrap_chunk(Name,C,N,false) ->
    case disk_log:chunk(Name,C,N) of
	{NC,TL,_} ->
	    {NC,TL};
	Y ->
	    Y
    end.

get_header_data(Name,true) ->
    case wrap_bchunk(Name,start,1,true) of
	{C,[Bin]} when is_binary(Bin) ->
	    T = binary_to_term(Bin),
	    case T of
		Tup when is_tuple(Tup) ->
		    L = tuple_to_list(Tup),
		    case verify_header_mandatory(L) of
			false ->
			    throw(badfile);
			true ->
			    Major = case lists:keyfind(major,1,L) of
					{major,Maj} ->
					    Maj;
					_ ->
					    0
				    end,
			    Minor = case lists:keyfind(minor,1,L) of
					{minor,Min} ->
					    Min;
					_ ->
					    0
				    end,
			    FtOptions = 
				case lists:keyfind(extended_info,1,L) of
				    {extended_info,I} when is_list(I) ->
					#filetab_options
					    {
					    object_count = 
					      lists:member(object_count,I),
					    md5sum = 
					      lists:member(md5sum,I)
					    };
				    _ ->
					#filetab_options{}
				end,
			    MD5Initial = 
				case FtOptions#filetab_options.md5sum of
				    true ->
					X = erlang:md5_init(),
					erlang:md5_update(X,Bin);
				    false ->
					false
				end,
			    {ok, Major, Minor, FtOptions, MD5Initial, L, C}
		    end;
		_X ->
		    throw(badfile)
	    end;
	_Y ->
	    throw(badfile)
    end;

get_header_data(Name, false) ->
   case wrap_chunk(Name, start, 1, false) of
       {C,[Tup]} when is_tuple(Tup) ->
	   L = tuple_to_list(Tup),
	   case verify_header_mandatory(L) of
	       false ->
		   throw(badfile);
	       true ->
		   Major = case lists:keyfind(major_version, 1, L) of
			       {major_version, Maj} ->
				   Maj;
			       _ ->
				   0
			   end,
		   Minor = case lists:keyfind(minor_version, 1, L) of
			       {minor_version, Min} ->
				   Min;
			       _ ->
				   0
			   end,
		   FtOptions = 
		       case lists:keyfind(extended_info, 1, L) of
			   {extended_info, I} when is_list(I) ->
			       #filetab_options
					 {
					 object_count = 
					 lists:member(object_count,I),
					 md5sum = 
					 lists:member(md5sum,I)
					};
			   _ ->
			       #filetab_options{}
		       end,
		   {ok, Major, Minor, FtOptions, false, L, C}
	   end;
       _ ->
	   throw(badfile)
    end.

md5_and_convert([], MD5State, Count) ->
    {[],MD5State,Count,[]};
md5_and_convert([H|T], MD5State, Count) when is_binary(H) ->
    case (catch binary_to_term(H)) of
	{'EXIT', _} ->
	    md5_and_convert(T,MD5State,Count);
	['$end_of_table',_Dat] = L ->
	   {[],MD5State,Count,L};
	Term ->
	    X = erlang:md5_update(MD5State, H),
	    {Rest,NewMD5,NewCount,NewLast} = md5_and_convert(T, X, Count+1),
	    {[Term | Rest],NewMD5,NewCount,NewLast}
    end.

scan_for_endinfo([], Count) ->
    {[],Count,[]};
scan_for_endinfo([['$end_of_table',Dat]], Count) ->
    {['$end_of_table',Dat],Count,[]};
scan_for_endinfo([Term|T], Count) ->
    {NewLast,NCount,Rest} = scan_for_endinfo(T, Count+1),
    {NewLast,NCount,[Term | Rest]}.

load_table(ReadFun, State, Tab) ->
    {NewState,NewData} = ReadFun(State),
    case NewData of
	[] ->
	    {ok,NewState};
	List ->
	    ets:insert(Tab, List),
	    load_table(ReadFun, NewState, Tab)
    end.

create_tab(I, TabArg) ->
    {name, Name} = lists:keyfind(name, 1, I),
    {type, Type} = lists:keyfind(type, 1, I),
    {protection, P} = lists:keyfind(protection, 1, I),
    {keypos, _Kp} = Keypos = lists:keyfind(keypos, 1, I),
    {size, Sz} = lists:keyfind(size, 1, I),
    L1 = [Type, P, Keypos],
    L2 = case lists:keyfind(named_table, 1, I) of
             {named_table, true} -> [named_table | L1];
	     {named_table, false} -> L1
	 end,
    L3 = case lists:keyfind(compressed, 1, I) of
	     {compressed, true} -> [compressed | L2];
	     {compressed, false} -> L2;
	     false -> L2
	 end,
    L4 = case lists:keyfind(write_concurrency, 1, I) of
	     {write_concurrency, _}=Wcc -> [Wcc | L3];
	     _ -> L3
	 end,
    L5 = case lists:keyfind(read_concurrency, 1, I) of
	     {read_concurrency, _}=Rcc -> [Rcc | L4];
	     false -> L4
	 end,
    case TabArg of
        [] ->
	    try
		Tab = ets:new(Name, L5),
		{ok, Tab, Sz}
	    catch _:_ ->
		throw(cannot_create_table)
            end;
        _ ->
            {ok, TabArg, Sz}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tabfile_info/1 reads the head information in an ets table dumped to
%% disk by means of file2tab and returns a list of the relevant table
%% information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tabfile_info(Filename) -> {'ok', TableInfo} | {'error', Reason} when
      Filename :: file:name(),
      TableInfo :: [InfoItem],
      InfoItem :: {'name', atom()}
                | {'type', Type}
                | {'protection', Protection}
                | {'named_table', boolean()}
                | {'keypos', non_neg_integer()}
                | {'size', non_neg_integer()}
                | {'extended_info', [ExtInfo]}
                | {'version', {Major :: non_neg_integer(),
                               Minor :: non_neg_integer()}},
      ExtInfo :: 'md5sum' | 'object_count',
      Type :: 'bag' | 'duplicate_bag' | 'ordered_set' | 'set',
      Protection :: 'private' | 'protected' | 'public',
      Reason :: term().

tabfile_info(File) when is_list(File) ; is_atom(File) ->
    try
	Name = make_ref(),
        {ok, Name} =
	    case disk_log:open([{name, Name}, 
				{file, File}, 
				{mode, read_only}]) of
		{ok, Name} ->
                    {ok, Name};
		{repaired, Name, _,_} -> %Uh? cannot happen?
		    {ok, Name};
		{error, Other1} ->
		    throw({read_error, Other1});
		Other2 ->
		    throw(Other2)
	    end,
	{ok, Major, Minor, _FtOptions, _MD5State, FullHeader, _DLContext} =
            try get_header_data(Name, false)
            catch
                badfile ->
                    _ = disk_log:close(Name),
                    throw(badfile)
            end,
        case disk_log:close(Name) of
            ok -> ok;
            {error, Reason} -> throw(Reason)
        end,
	{value, N} = lists:keysearch(name, 1, FullHeader),
	{value, Type} = lists:keysearch(type, 1, FullHeader),
	{value, P} = lists:keysearch(protection, 1, FullHeader),
	{value, Val} = lists:keysearch(named_table, 1, FullHeader),
	{value, Kp} = lists:keysearch(keypos, 1, FullHeader),
	{value, Sz} = lists:keysearch(size, 1, FullHeader),
	Ei = case lists:keyfind(extended_info, 1, FullHeader) of
		 false -> {extended_info, []};
		 Ei0 -> Ei0
	     end,
	{ok, [N,Type,P,Val,Kp,Sz,Ei,{version,{Major,Minor}}]}
    catch
	throw:TReason ->
	    {error,TReason};
	exit:ExReason ->
	    {error,ExReason}
    end.

-spec table(Tab) -> QueryHandle when
      Tab :: tab(),
      QueryHandle :: qlc:query_handle().

table(Tab) ->
    table(Tab, []).

-spec table(Tab, Options) -> QueryHandle when
      Tab :: tab(),
      QueryHandle :: qlc:query_handle(),
      Options :: [Option] | Option,
      Option :: {'n_objects', NObjects}
              | {'traverse', TraverseMethod},
      NObjects :: 'default' | pos_integer(),
      TraverseMethod :: 'first_next' | 'last_prev'
                      | 'select' | {'select', MatchSpec :: match_spec()}.

table(Tab, Opts) ->
    case options(Opts, [traverse, n_objects]) of
        {badarg,_} ->
            erlang:error(badarg, [Tab, Opts]);
        [[Traverse, NObjs], QlcOptions] ->
            TF = case Traverse of
                     first_next -> 
                         fun() -> qlc_next(Tab, ets:first(Tab)) end;
                     last_prev -> 
                         fun() -> qlc_prev(Tab, ets:last(Tab)) end;
                     select -> 
                         fun(MS) -> qlc_select(ets:select(Tab, MS, NObjs)) end;
                     {select, MS} ->
                         fun() -> qlc_select(ets:select(Tab, MS, NObjs)) end
                 end,
            PreFun = fun(_) -> ets:safe_fixtable(Tab, true) end,
            PostFun = fun() -> ets:safe_fixtable(Tab, false) end,
            InfoFun = fun(Tag) -> table_info(Tab, Tag) end,
            KeyEquality = case ets:info(Tab, type) of
                              ordered_set -> '==';
                              _ -> '=:='
                          end,
            LookupFun = 
                case Traverse of 
                    {select, _MS} ->
                        undefined;
                    _ -> 
                        fun(_Pos, [K]) ->
                                ets:lookup(Tab, K);
                           (_Pos, Ks) -> 
                                lists:flatmap(fun(K) -> ets:lookup(Tab, K) 
                                              end, Ks) 
                        end
                end,
            FormatFun = 
                fun({all, _NElements, _ElementFun}) ->
                        As = [Tab | [Opts || _ <- [[]], Opts =/= []]],
                        {?MODULE, table, As};
                   ({match_spec, MS}) ->
                        {?MODULE, table, 
                         [Tab, [{traverse, {select, MS}} | 
                                listify(Opts)]]};
                   ({lookup, _KeyPos, [Value], _NElements, ElementFun}) ->
                        io_lib:format("~w:lookup(~w, ~w)", 
                                      [?MODULE, Tab, ElementFun(Value)]);
                   ({lookup, _KeyPos, Values, _NElements, ElementFun}) ->
                        Vals = [ElementFun(V) || V <- Values],
                        io_lib:format("lists:flatmap(fun(V) -> "
                                      "~w:lookup(~w, V) end, ~w)", 
                                      [?MODULE, Tab, Vals])
                end,
            qlc:table(TF, [{pre_fun, PreFun}, {post_fun, PostFun}, 
                           {info_fun, InfoFun}, {format_fun, FormatFun},
                           {key_equality, KeyEquality},
                           {lookup_fun, LookupFun}] ++ QlcOptions)
    end.
         
table_info(Tab, num_of_objects) ->
    ets:info(Tab, size);
table_info(Tab, keypos) ->
    ets:info(Tab, keypos);
table_info(Tab, is_unique_objects) ->
    ets:info(Tab, type) =/= duplicate_bag;
table_info(Tab, is_sorted_key) ->
    ets:info(Tab, type) =:= ordered_set;
table_info(_Tab, _) ->
    undefined.

qlc_next(_Tab, '$end_of_table') ->
    [];
qlc_next(Tab, Key) ->
    ets:lookup(Tab, Key) ++ fun() -> qlc_next(Tab, ets:next(Tab, Key)) end.

qlc_prev(_Tab, '$end_of_table') ->
    [];
qlc_prev(Tab, Key) ->
    ets:lookup(Tab, Key) ++ fun() -> qlc_prev(Tab, ets:prev(Tab, Key)) end.

qlc_select('$end_of_table') -> 
    [];
qlc_select({Objects, Cont}) -> 
    Objects ++ fun() -> qlc_select(ets:select(Cont)) end.

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options, [Key | Keys], L) when is_list(Options) ->
    V = case lists:keyfind(Key, 1, Options) of
            {n_objects, default} ->
                {ok, default_option(Key)};
            {n_objects, NObjs} when is_integer(NObjs), NObjs >= 1 ->
                {ok, NObjs};
            {traverse, select} ->
                {ok, select};
            {traverse, {select, _MS} = Select} ->
                {ok, Select};
            {traverse, first_next} ->
                {ok, first_next};
            {traverse, last_prev} ->
                {ok, last_prev};
	    {Key, _} ->
		badarg;
	    false ->
		Default = default_option(Key),
		{ok, Default}
	end,
    case V of
	badarg ->
	    {badarg, Key};
	{ok,Value} ->
	    NewOptions = lists:keydelete(Key, 1, Options),
	    options(NewOptions, Keys, [Value | L])
    end;
options(Options, [], L) ->
    [lists:reverse(L), Options].

default_option(traverse) -> select;
default_option(n_objects) -> 100.

listify(L) when is_list(L) ->
    L;
listify(T) ->
    [T].

%% End of table/2.

%% Print info about all tabs on the tty
-spec i() -> 'ok'.

i() ->
    hform('id', 'name', 'type', 'size', 'mem', 'owner'),
    io:format(" -------------------------------------"
	      "---------------------------------------\n"),
    lists:foreach(fun prinfo/1, tabs()),
    ok.

tabs() ->
    lists:sort(ets:all()).

prinfo(Tab) ->
    case catch prinfo2(Tab) of
	{'EXIT', _} ->
	    io:format("~-10s ... unreadable \n", [to_string(Tab)]);
	ok -> 
	    ok
    end.
prinfo2(Tab) ->
    Name = ets:info(Tab, name),
    Type = ets:info(Tab, type),
    Size = ets:info(Tab, size),
    Mem = ets:info(Tab, memory),
    Owner = ets:info(Tab, owner),
    hform(Tab, Name, Type, Size, Mem, is_reg(Owner)).

is_reg(Owner) ->
    case process_info(Owner, registered_name) of
	{registered_name, Name} -> Name;
	_ -> Owner
    end.

%%% Arndt: this code used to truncate over-sized fields. Now it
%%% pushes the remaining entries to the right instead, rather than
%%% losing information.
hform(A0, B0, C0, D0, E0, F0) ->
    [A,B,C,D,E,F] = [to_string(T) || T <- [A0,B0,C0,D0,E0,F0]],
    A1 = pad_right(A, 15),
    B1 = pad_right(B, 17),
    C1 = pad_right(C, 5),
    D1 = pad_right(D, 6),
    E1 = pad_right(E, 8),
    %% no need to pad the last entry on the line
    io:format(" ~s ~s ~s ~s ~s ~s\n", [A1,B1,C1,D1,E1,F]).

pad_right(String, Len) ->
    if
	length(String) >= Len ->
	    String;
	true ->
	    [Space] = " ",
	    String ++ lists:duplicate(Len - length(String), Space)
    end.

to_string(X) ->
    lists:flatten(io_lib:format("~p", [X])).

%% view a specific table
-spec i(Tab) -> 'ok' when
      Tab :: tab().

i(Tab) ->
    i(Tab, 40).

-spec i(tab(), pos_integer()) -> 'ok'.

i(Tab, Height) ->
    i(Tab, Height, 80).

-spec i(tab(), pos_integer(), pos_integer()) -> 'ok'.

i(Tab, Height, Width) ->
    First = ets:first(Tab),
    display_items(Height, Width, Tab, First, 1, 1).

display_items(Height, Width, Tab, '$end_of_table', Turn, Opos) -> 
    P = 'EOT  (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, eot, Tab, '$end_of_table', Turn, Opos);
display_items(Height, Width, Tab, Key, Turn, Opos) when Turn < Height ->
    do_display(Height, Width, Tab, Key, Turn, Opos);
display_items(Height, Width, Tab, Key, Turn, Opos) when Turn >=  Height ->
    P = '(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, normal, Tab, Key, Turn, Opos).

choice(Height, Width, P, Mode, Tab, Key, Turn, Opos) ->
    case get_line(P, "c\n") of
	"c\n" when Mode =:= normal ->
	    do_display(Height, Width, Tab, Key, 1, Opos);
	"c\n" when is_tuple(Mode), element(1, Mode) =:= re ->
	    {re, Re} = Mode,
	    re_search(Height, Width, Tab, Key, Re, 1, Opos);
	"q\n" ->
	    ok;
	"k\n" ->
	    ets:delete(Tab),
	    ok;
	[$p|Digs]  ->
	    catch case catch list_to_integer(nonl(Digs)) of
		      {'EXIT', _} ->
			  io:put_chars("Bad digits\n");
		      Number when Mode =:= normal ->
			  print_number(Tab, ets:first(Tab), Number);
		      Number when Mode =:= eot ->
			  print_number(Tab, ets:first(Tab), Number);
		      Number -> %% regexp
			  {re, Re} = Mode,
			  print_re_num(Tab, ets:first(Tab), Number, Re)
		  end,
	    choice(Height, Width, P, Mode, Tab, Key, Turn, Opos);
	[$/|Regexp]   -> %% from regexp
	    case re:compile(nonl(Regexp),[unicode]) of
		{ok,Re} ->
		    re_search(Height, Width, Tab, ets:first(Tab), Re, 1, 1);
		{error,{ErrorString,_Pos}} ->
		    io:format("~ts\n", [ErrorString]),
		    choice(Height, Width, P, Mode, Tab, Key, Turn, Opos)
	    end;
        eof ->
            ok;
	_  ->
	    choice(Height, Width, P, Mode, Tab, Key, Turn, Opos)
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

nonl(S) -> string:trim(S, trailing, "$\n").

print_number(Tab, Key, Num) ->
    Os = ets:lookup(Tab, Key),
    Len = length(Os),
    if 
	(Num - Len) < 1 ->
	    O = lists:nth(Num, Os),
	    io:format("~p~n", [O]); %% use ppterm here instead
	true ->
	    print_number(Tab, ets:next(Tab, Key), Num - Len)
    end.

do_display(Height, Width, Tab, Key, Turn, Opos) ->
    Objs = ets:lookup(Tab, Key),
    do_display_items(Height, Width, Objs, Opos),
    Len = length(Objs),
    display_items(Height, Width, Tab, ets:next(Tab, Key), Turn+Len, Opos+Len).

do_display_items(Height, Width, [Obj|Tail], Opos) ->
    do_display_item(Height, Width, Obj, Opos),
    do_display_items(Height, Width, Tail, Opos+1);
do_display_items(_Height, _Width, [], Opos) ->
    Opos.

do_display_item(_Height, Width, I, Opos)  ->
    L = to_string(I),
    L2 = if
	     length(L) > Width - 8 ->
                 string:slice(L, 0, Width-13) ++ "  ...";
	     true ->
		 L
	 end,
    io:format("<~-4w> ~s~n", [Opos,L2]).

re_search(Height, Width, Tab, '$end_of_table', Re, Turn, Opos) ->
    P = 'EOT  (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, {re, Re}, Tab, '$end_of_table', Turn, Opos);
re_search(Height, Width, Tab, Key, Re, Turn, Opos) when Turn < Height ->
    re_display(Height, Width, Tab, Key, ets:lookup(Tab, Key), Re, Turn, Opos);
re_search(Height, Width, Tab, Key, Re, Turn, Opos)  ->
    P = '(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, {re, Re}, Tab, Key, Turn, Opos).

re_display(Height, Width, Tab, Key, [], Re, Turn, Opos) ->
    re_search(Height, Width, Tab, ets:next(Tab, Key), Re, Turn, Opos);
re_display(Height, Width, Tab, Key, [H|T], Re, Turn, Opos) ->
    Str = to_string(H),
    case re:run(Str, Re, [{capture,none}]) of
	match ->
	    do_display_item(Height, Width, H, Opos),
	    re_display(Height, Width, Tab, Key, T, Re, Turn+1, Opos+1);
	nomatch ->
	    re_display(Height, Width, Tab, Key, T, Re, Turn, Opos)
    end.

print_re_num(_,'$end_of_table',_,_) -> ok;
print_re_num(Tab, Key, Num, Re) ->
    Os = re_match(ets:lookup(Tab, Key), Re),
    Len = length(Os),
    if 
	(Num - Len) < 1 ->
	    O = lists:nth(Num, Os),
	    io:format("~p~n", [O]); %% use ppterm here instead
	true ->
	    print_re_num(Tab, ets:next(Tab, Key), Num - Len, Re)
    end.

re_match([], _) -> [];
re_match([H|T], Re) ->
    case re:run(to_string(H), Re, [{capture,none}]) of
	match -> 
	    [H|re_match(T,Re)];
	nomatch ->
	    re_match(T, Re)
    end.
