%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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
-module(qlc).

%%% Purpose: Main API module qlc. Functions for evaluation.
%%% Other files:
%%% qlc_pt. Implements the parse transform.

%% External exports 

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([parse_transform/2, transform_from_evaluator/2]).

-export([q/1, q/2]).

-export([eval/1, e/1, eval/2, e/2, fold/3, fold/4]).
-export([cursor/1, cursor/2, 
         next_answers/1, next_answers/2, 
         delete_cursor/1]).
-export([append/1, append/2]).

-export([sort/1, sort/2, keysort/2, keysort/3]).

-export([table/2]).

-export([info/1, info/2]).

-export([string_to_handle/1, string_to_handle/2, string_to_handle/3]).

-export([format_error/1]).

%% Exported to qlc_pt.erl only:
-export([template_state/0, aux_name/3, name_suffix/2, vars/1,
         var_ufold/2, var_fold/3, all_selections/1]).

-dialyzer(no_improper_lists).

%% When cache=list lists bigger than ?MAX_LIST_SIZE bytes are put on
%% file. Also used when merge join finds big equivalence classes.
-define(MAX_LIST_SIZE, 512*1024).

-record(qlc_append, % qlc:append/1,2
        {hl
        }).

-record(qlc_table,   % qlc:table/2
        {trav_fun,   % traverse fun
         trav_MS,    % boolean(); true iff traverse fun takes a match spec
         pre_fun,
         post_fun,
         info_fun,
         format_fun,
         lookup_fun,
         parent_fun,
         key_equality, % '==' | '=:=' | undefined (--R12B-5)
         lu_vals,    % undefined | {Position,Values}; values to be looked up
         ms = no_match_spec
                     % match specification; [T || P <- Tab, Fs]
        }).

-record(qlc_sort,    % qlc:sort/1,2 and qlc:keysort/2,3
        {h,
         keypos,     % sort | {keysort, KeyPos}
         unique,
         compressed, % [] | [compressed]
         order,
         fs_opts,    % file_sorter options
         tmpdir_usage = allowed, % allowed | not_allowed 
                                 %  | warning_msg | error_msg | info_msg
         tmpdir
        }).

%% Also in qlc_pt.erl.
-record(qlc_lc,     % qlc:q/1,2
        {lc,
         opt        % #qlc_opt
        }).

-record(qlc_list,   % a prepared list
        {l,
         ms = no_match_spec
        }).

-record(qlc_join,        % a prepared join
        {kind,           % {merge, KeyEquality} |
                         % {lookup, KeyEquality, LookupFun}
         opt,            % #qlc_opt from q/2.
         h1, q1, c1,     % to be traversed by "lookup join"
         h2, q2, c2      % to be looked up by "lookup join"
        }).

%%% A query cursor is a tuple {qlc_cursor, Cursor} where Cursor is a pair
%%% {CursorPid, OwnerPid}.

-record(qlc_cursor, {c}).

-record(qlc_opt,
        {unique = false,      % boolean()
         cache = false,       % boolean() | list (true~ets, false~no)
         max_lookup = -1,     % int() >= 0 | -1 (represents infinity)
         join = any,          % any | nested_loop | merge | lookup
         tmpdir = "",         % global tmpdir
         lookup = any,        % any | boolean()
         max_list = ?MAX_LIST_SIZE,  % int() >= 0
         tmpdir_usage = allowed    % allowed | not_allowed 
                                   %  | warning_msg | error_msg | info_msg
        }).

-record(setup, {parent}).

-define(THROWN_ERROR, {?MODULE, throw_error, _, _}).

-export_type([query_cursor/0, query_handle/0]).

%%% A query handle is a tuple {qlc_handle, Handle} where Handle is one
%%% of #qlc_append, #qlc_table, #qlc_sort, and #qlc_lc.

-record(qlc_handle, {h}).

get_handle(#qlc_handle{h = #qlc_lc{opt = {qlc_opt, U, C, M}}=H}) ->
    %% R11B-0.
    H#qlc_lc{opt = #qlc_opt{unique = U, cache = C, max_lookup = M}};
get_handle(#qlc_handle{h = H}) ->
    H;
get_handle(L) when is_list(L) ->
    L;
get_handle(_) ->
    badarg.

%%%
%%% Exported functions
%%%

-type(query_list_comprehension() :: term()).
-opaque(query_cursor() :: {qlc_cursor, term()}).
-opaque(query_handle() :: {qlc_handle, term()}).
-type(query_handle_or_list() :: query_handle() | list()).
-type(answers() :: [answer()]).
-type(answer() :: term()).
-type(abstract_expr() :: erl_parse:abstract_expr()).
-type(match_expression() :: ets:match_spec()).
-type(spawn_options() :: default | [proc_lib:spawn_option()]).
-type(sort_options() :: [sort_option()] | sort_option()).
-type(sort_option() :: {compressed, boolean()}
                     | {no_files, no_files()}
                     | {order, order()}
                     | {size, pos_integer()}
                     | {tmpdir, tmp_directory()}
                     | {unique, boolean()}).
-type(order() :: ascending | descending | order_fun()).
-type(order_fun() :: fun((term(), term()) -> boolean())).
-type(tmp_directory() :: [] | file:name()).
-type(no_files() :: pos_integer()). % > 1
-type(key_pos() :: pos_integer() | [pos_integer()]).
-type(max_list_size() :: non_neg_integer()).
-type(cache() :: ets | list | no).
-type(tmp_file_usage() :: allowed | not_allowed | info_msg
                        | warning_msg  | error_msg).

-spec(append(QHL) -> QH when
      QHL :: [query_handle_or_list()],
      QH :: query_handle()).
append(QHs) ->
    Hs = [case get_handle(QH) of
              badarg -> erlang:error(badarg, [QHs]);
              H -> H
          end || QH <- QHs],
    #qlc_handle{h = #qlc_append{hl = Hs}}.

-spec(append(QH1, QH2) -> QH3 when
      QH1 :: query_handle_or_list(),
      QH2 :: query_handle_or_list(),
      QH3 :: query_handle()).
append(QH1, QH2) ->
    Hs = [case get_handle(QH) of
              badarg -> erlang:error(badarg, [QH1, QH2]);
              H -> H
          end || QH <- [QH1, QH2]],
    #qlc_handle{h = #qlc_append{hl = Hs}}.

-spec(cursor(QH) -> Cursor when
      QH :: query_handle_or_list(),
      Cursor :: query_cursor()).
cursor(QH) ->
    cursor(QH, []).

-spec(cursor(QH, Options) -> Cursor when
      QH :: query_handle_or_list(),
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {spawn_options, spawn_options()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all,
      Cursor :: query_cursor()).
cursor(QH, Options) ->
    case {options(Options, [unique_all, cache_all, tmpdir, 
                            spawn_options, max_list_size, 
                            tmpdir_usage]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[GUnique, GCache, TmpDir, SpawnOptions0, MaxList, TmpUsage], H} ->
            SpawnOptions = spawn_options(SpawnOptions0),
            case cursor_process(H, GUnique, GCache, TmpDir, 
                                SpawnOptions, MaxList, TmpUsage) of
                Pid when is_pid(Pid) ->
                    #qlc_cursor{c = {Pid, self()}};
                Error ->
                    Error
            end
    end.

-spec(delete_cursor(QueryCursor) -> ok when
      QueryCursor :: query_cursor()).
delete_cursor(#qlc_cursor{c = {_, Owner}}=C) when Owner =/= self() ->
    erlang:error(not_cursor_owner, [C]);
delete_cursor(#qlc_cursor{c = {Pid, _}}) ->
    stop_cursor(Pid);
delete_cursor(T) ->
    erlang:error(badarg, [T]).

-spec(e(QH) -> Answers | Error when
      QH :: query_handle_or_list(),
      Answers :: answers(),
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
e(QH) ->
    eval(QH, []).

-spec(e(QH, Options) -> Answers | Error when
      QH :: query_handle_or_list(),
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all,
      Answers :: answers(),
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
e(QH, Options) ->
    eval(QH, Options).

-spec(eval(QH) -> Answers | Error when
      QH :: query_handle_or_list(),
      Answers :: answers(),
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
eval(QH) ->
    eval(QH, []).

-spec(eval(QH, Options) -> Answers | Error when
      QH :: query_handle_or_list(),
      Answers :: answers(),
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all,
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
eval(QH, Options) ->
    case {options(Options, [unique_all, cache_all, tmpdir, max_list_size,
                            tmpdir_usage]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[GUnique, GCache, TmpDir, MaxList, TmpUsage], Handle} ->
            try 
                Prep = prepare_qlc(Handle, [], GUnique, GCache, 
                                   TmpDir, MaxList, TmpUsage),
                case setup_qlc(Prep, #setup{parent = self()}) of
                    {L, Post, _LocalPost} when is_list(L) ->
                        post_funs(Post),
                        L;
                    {Objs, Post, _LocalPost} when is_function(Objs) ->
                        try
                            collect(Objs)
                        after
                            post_funs(Post)
                        end
                end
            catch throw:Term:Stacktrace ->
                case Stacktrace of
                    [?THROWN_ERROR | _] ->
                        Term;
                    _ ->
                        erlang:raise(throw, Term, Stacktrace)
                end
            end
    end.

-spec(fold(Function, Acc0, QH) ->
               Acc1 | Error when
      QH :: query_handle_or_list(),
      Function :: fun((answer(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
fold(Fun, Acc0, QH) ->
    fold(Fun, Acc0, QH, []).

-spec(fold(Function, Acc0, QH, Options) ->
               Acc1 | Error when
      QH :: query_handle_or_list(),
      Function :: fun((answer(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Options :: [Option] | Option,
      Option :: {cache_all, cache()} | cache_all
              | {max_list_size, max_list_size()}
              | {tmpdir_usage, tmp_file_usage()}
              | {tmpdir, tmp_directory()}
              | {unique_all, boolean()} | unique_all,
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
fold(Fun, Acc0, QH, Options) ->
    case {options(Options, [unique_all, cache_all, tmpdir, max_list_size,
                            tmpdir_usage]), 
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [Fun, Acc0, QH, Options]);
        {[GUnique, GCache, TmpDir, MaxList, TmpUsage], Handle} ->
            try
                Prep = prepare_qlc(Handle, not_a_list, GUnique, GCache,
                                   TmpDir, MaxList, TmpUsage),
                case setup_qlc(Prep, #setup{parent = self()}) of
                    {Objs, Post, _LocalPost} when is_function(Objs); 
                                                  is_list(Objs) ->
                        try
                            fold_loop(Fun, Objs, Acc0)
                        after
                            post_funs(Post)
                        end
                end
            catch throw:Term:Stacktrace ->
                case Stacktrace of
                    [?THROWN_ERROR | _] ->
                        Term;
                    _ ->
                        erlang:raise(throw, Term, Stacktrace)
                end
            end
    end.

-spec(format_error(Error) -> Chars when
      Error :: {error, module(), term()},
      Chars :: io_lib:chars()).
format_error(not_a_query_list_comprehension) ->
    io_lib:format("argument is not a query list comprehension", []);
format_error({used_generator_variable, V}) ->
    io_lib:format("generated variable ~w must not be used in list expression",
                  [V]);
format_error(binary_generator) ->
    io_lib:format("cannot handle binary generators", []);
format_error(too_complex_join) ->
    io_lib:format("cannot handle join of three or more generators efficiently",
                  []);
format_error(too_many_joins) ->
    io_lib:format("cannot handle more than one join efficiently", []);
format_error(nomatch_pattern) ->
    io_lib:format("pattern cannot possibly match", []);
format_error(nomatch_filter) ->
    io_lib:format("filter evaluates to 'false'", []);
format_error({Line, Mod, Reason}) when is_integer(Line) ->
    io_lib:format("~p: ~ts~n", 
                  [Line, lists:flatten(Mod:format_error(Reason))]);
%% file_sorter errors
format_error({bad_object, FileName}) ->
    io_lib:format("the temporary file \"~ts\" holding answers is corrupt",
                 [FileName]);
format_error(bad_object) ->
    io_lib:format("the keys could not be extracted from some term", []);
format_error({file_error, FileName, Reason}) ->
    io_lib:format("\"~ts\": ~tp~n",[FileName, file:format_error(Reason)]);
format_error({premature_eof, FileName}) ->
    io_lib:format("\"~ts\": end-of-file was encountered inside some binary term", 
                  [FileName]);
format_error({tmpdir_usage, Why}) ->
    io_lib:format("temporary file was needed for ~w~n", [Why]);
format_error({error, Module, Reason}) ->
    Module:format_error(Reason);
format_error(E) ->
    io_lib:format("~tp~n", [E]).

-spec(info(QH) -> Info when
      QH :: query_handle_or_list(),
      Info :: abstract_expr() | string()).
info(QH) ->
    info(QH, []).

-spec(info(QH, Options) -> Info when
      QH :: query_handle_or_list(),
      Options :: [Option] | Option,
      Option :: EvalOption | ReturnOption,
      EvalOption :: {cache_all, cache()} | cache_all
                  | {max_list_size, max_list_size()}
                  | {tmpdir_usage, tmp_file_usage()}
                  | {tmpdir, tmp_directory()}
                  | {unique_all, boolean()} | unique_all,
      ReturnOption :: {depth, Depth}
                    | {flat, boolean()}
                    | {format, Format}
                    | {n_elements, NElements},
      Depth :: infinity | non_neg_integer(),
      Format :: abstract_code | string,
      NElements :: infinity | pos_integer(),
      Info :: abstract_expr() | string()).
info(QH, Options) ->
    case {options(Options, [unique_all, cache_all, flat, format, n_elements, 
                            depth, tmpdir, max_list_size, tmpdir_usage]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[GUnique, GCache, Flat, Format, NElements, 
          Depth, TmpDir, MaxList, TmpUsage],
         H} ->
            try
                Prep = prepare_qlc(H, [], GUnique, GCache, 
                                   TmpDir, MaxList, TmpUsage),
                Info = le_info(Prep, {NElements,Depth}),
                AbstractCode = abstract(Info, Flat, NElements, Depth),
                case Format of
                    abstract_code ->
                        abstract_code(AbstractCode);
                    string ->
                        Hook = fun({special, _Line, String}, _I, _P, _F) ->
                                       String
                               end,
                        lists:flatten(erl_pp:expr(AbstractCode, 0, Hook));
                    debug -> % Not documented. Intended for testing only.
                        Info
                end
            catch throw:Term:Stacktrace ->
                case Stacktrace of
                    [?THROWN_ERROR | _] ->
                        Term;
                    _ ->
                        erlang:raise(throw, Term, Stacktrace)
                end
            end
    end.

-spec(keysort(KeyPos, QH1) -> QH2 when
      KeyPos :: key_pos(),
      QH1 :: query_handle_or_list(),
      QH2 :: query_handle()).
keysort(KeyPos, QH) ->
    keysort(KeyPos, QH, []).

-spec(keysort(KeyPos, QH1, SortOptions) -> QH2 when
      KeyPos :: key_pos(),
      SortOptions :: sort_options(),
      QH1 :: query_handle_or_list(),
      QH2 :: query_handle()).
keysort(KeyPos, QH, Options) ->
    case {is_keypos(KeyPos), 
          options(Options, [tmpdir, order, unique, compressed, 
                            size, no_files]),
          get_handle(QH)} of
        {true, [TmpDir, Order, Unique,Compressed | _], H} when H =/= badarg ->
            #qlc_handle{h = #qlc_sort{h = H, keypos = {keysort,KeyPos}, 
                                      unique = Unique, 
                                      compressed = Compressed,
                                      order = Order, 
                                      fs_opts = listify(Options), 
                                      tmpdir = TmpDir}};
        _ ->
            erlang:error(badarg, [KeyPos, QH, Options])
    end.

-define(DEFAULT_NUM_OF_ANSWERS, 10).

-spec(next_answers(QueryCursor) ->
            Answers | Error when
      QueryCursor :: query_cursor(),
      Answers :: answers(),
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
next_answers(C) ->
    next_answers(C, ?DEFAULT_NUM_OF_ANSWERS).

-spec(next_answers(QueryCursor, NumberOfAnswers) ->
            Answers | Error when
      QueryCursor :: query_cursor(),
      Answers :: answers(),
      NumberOfAnswers :: all_remaining | pos_integer(),
      Error :: {error, module(), Reason},
      Reason :: file_sorter:reason()).
next_answers(#qlc_cursor{c = {_, Owner}}=C, 
             NumOfAnswers) when Owner =/= self() ->
    erlang:error(not_cursor_owner, [C, NumOfAnswers]);
next_answers(#qlc_cursor{c = {Pid, _}}=C, NumOfAnswers) ->
    N = case NumOfAnswers of
            all_remaining -> -1;
            _ when is_integer(NumOfAnswers), NumOfAnswers > 0 -> NumOfAnswers;
            _ -> erlang:error(badarg, [C, NumOfAnswers])
        end,
    next_loop(Pid, [], N);
next_answers(T1, T2) ->
    erlang:error(badarg, [T1, T2]).

-spec(parse_transform(Forms, Options) -> Forms2 when
      Forms :: [erl_parse:abstract_form()],
      Forms2 :: [erl_parse:abstract_form()],
      Options :: [Option],
      Option :: type_checker | compile:option()).

parse_transform(Forms, Options) ->
    qlc_pt:parse_transform(Forms, Options).

%% The funcspecs qlc:q/1 and qlc:q/2 are known by erl_eval.erl and
%% erl_lint.erl.
-spec(q(QLC) -> QH when
      QLC :: query_list_comprehension(),
      QH :: query_handle()).
q(QLC_lc) ->
    q(QLC_lc, []).

-spec(q(QLC, Options) -> QH when
      QH :: query_handle(),
      Options :: [Option] | Option,
      Option :: {max_lookup, MaxLookup}
              | {cache, cache()} | cache
              | {join, Join}
              | {lookup, Lookup}
              | {unique, boolean()} | unique,
      MaxLookup :: non_neg_integer() | infinity,
      Join :: any | lookup | merge | nested_loop,
      Lookup :: boolean() | any,
      QLC :: query_list_comprehension()).
q(#qlc_lc{}=QLC_lc, Options) ->
    case options(Options, [unique, cache, max_lookup, join, lookup]) of
        [Unique, Cache, Max, Join, Lookup] ->
            Opt = #qlc_opt{unique = Unique, cache = Cache, 
                           max_lookup = Max, join = Join, lookup = Lookup},
            #qlc_handle{h = QLC_lc#qlc_lc{opt = Opt}};
        _ ->
            erlang:error(badarg, [QLC_lc, Options])
    end;
q(T1, T2) ->
    erlang:error(badarg, [T1, T2]).

-spec(sort(QH1) -> QH2 when
      QH1 :: query_handle_or_list(),
      QH2 :: query_handle()).
sort(QH) ->
    sort(QH, []).

-spec(sort(QH1, SortOptions) -> QH2 when
      SortOptions :: sort_options(),
      QH1 :: query_handle_or_list(),
      QH2 :: query_handle()).
sort(QH, Options) ->
    case {options(Options, [tmpdir, order, unique, compressed, 
                            size, no_files]), get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[TD, Order, Unique, Compressed | _], H} ->
            #qlc_handle{h = #qlc_sort{h = H, keypos = sort, unique = Unique, 
                                      compressed = Compressed, order = Order,
                                      fs_opts = listify(Options), 
                                      tmpdir = TD}}
    end.

%% Note that the generated code is evaluated by (the slow) erl_eval.
-spec(string_to_handle(QueryString) -> QH | Error when
      QueryString :: string(),
      QH :: query_handle(),
      Error :: {error, module(), Reason},
      Reason :: erl_parse:error_info() | erl_scan:error_info()).
string_to_handle(Str) ->
    string_to_handle(Str, []).

-spec(string_to_handle(QueryString, Options) -> QH | Error when
      QueryString :: string(),
      Options :: [Option] | Option,
      Option :: {max_lookup, MaxLookup}
              | {cache, cache()} | cache
              | {join, Join}
              | {lookup, Lookup}
              | {unique, boolean()} | unique,
      MaxLookup :: non_neg_integer() | infinity,
      Join :: any | lookup | merge | nested_loop,
      Lookup :: boolean() | any,
      QH :: query_handle(),
      Error :: {error, module(), Reason},
      Reason :: erl_parse:error_info() | erl_scan:error_info()).
string_to_handle(Str, Options) ->
    string_to_handle(Str, Options, erl_eval:new_bindings()).

-spec(string_to_handle(QueryString, Options, Bindings) -> QH | Error when
      QueryString :: string(),
      Options :: [Option] | Option,
      Option :: {max_lookup, MaxLookup}
              | {cache, cache()} | cache
              | {join, Join}
              | {lookup, Lookup}
              | {unique, boolean()} | unique,
      MaxLookup :: non_neg_integer() | infinity,
      Join :: any | lookup | merge | nested_loop,
      Lookup :: boolean() | any,
      Bindings :: erl_eval:binding_struct(),
      QH :: query_handle(),
      Error :: {error, module(), Reason},
      Reason :: erl_parse:error_info() | erl_scan:error_info()).
string_to_handle(Str, Options, Bindings) when is_list(Str) ->
    case options(Options, [unique, cache, max_lookup, join, lookup]) of
        badarg ->
            erlang:error(badarg, [Str, Options, Bindings]);
        [Unique, Cache, MaxLookup, Join, Lookup] ->
            case erl_scan:string(Str, 1, [text]) of
                {ok, Tokens, _} ->
                    ScanRes =
                        case lib:extended_parse_exprs(Tokens) of
                            {ok, [Expr0], SBs} ->
                                {ok, Expr0, SBs};
                            {ok, _ExprList, _SBs} ->
                                erlang:error(badarg,
                                             [Str, Options, Bindings]);
                            E ->
                                E
                        end,
                    case ScanRes of
                        {ok, Expr, XBs} ->
                            Bs1 = merge_binding_structs(Bindings, XBs),
                            case qlc_pt:transform_expression(Expr, Bs1) of
                                {ok, {call, _, _QlcQ,  Handle}} ->
                                    {value, QLC_lc, _} = 
                                        erl_eval:exprs(Handle, Bs1),
                                    O = #qlc_opt{unique = Unique, 
                                                 cache = Cache,
                                                 max_lookup = MaxLookup, 
                                                 join = Join,
                                                 lookup = Lookup},
                                    #qlc_handle{h = QLC_lc#qlc_lc{opt = O}};
                                {not_ok, [{error, Error} | _]} ->
                                    error(Error)
                            end;
                        {error, ErrorInfo} ->
                            error(ErrorInfo)
                    end;
                {error, ErrorInfo, _EndLine} ->
                    error(ErrorInfo)
            end
    end;
string_to_handle(T1, T2, T3) ->    
    erlang:error(badarg, [T1, T2, T3]).

-spec(table(TraverseFun, Options) -> QH when
      TraverseFun :: TraverseFun0 | TraverseFun1,
      TraverseFun0 :: fun(() -> TraverseResult),
      TraverseFun1 :: fun((match_expression()) -> TraverseResult),
      TraverseResult :: Objects | term(),
      Objects :: [] | [term() | ObjectList],
      ObjectList :: TraverseFun0 | Objects,
      Options :: [Option] | Option,
      Option :: {format_fun, FormatFun}
              | {info_fun, InfoFun}
              | {lookup_fun, LookupFun}
              | {parent_fun, ParentFun}
              | {post_fun, PostFun}
              | {pre_fun, PreFun}
              | {key_equality, KeyComparison},
      FormatFun :: undefined  | fun((SelectedObjects) -> FormatedTable),
      SelectedObjects :: all
                       | {all, NElements, DepthFun}
                       | {match_spec, match_expression()}
                       | {lookup, Position, Keys}
                       | {lookup, Position, Keys, NElements, DepthFun},
      NElements :: infinity | pos_integer(),
      DepthFun :: fun((term()) -> term()),
      FormatedTable :: {Mod, Fun, Args}
                     | abstract_expr()
                     | string(),
      InfoFun :: undefined  | fun((InfoTag) -> InfoValue),
      InfoTag :: indices | is_unique_objects | keypos | num_of_objects,
      InfoValue :: undefined  | term(),
      LookupFun :: undefined  | fun((Position, Keys) -> LookupResult),
      LookupResult :: [term()] | term(),
      ParentFun :: undefined  | fun(() -> ParentFunValue),
      PostFun :: undefined  | fun(() -> term()),
      PreFun :: undefined  | fun((PreArgs) -> term()),
      PreArgs :: [PreArg],
      PreArg :: {parent_value, ParentFunValue}  | {stop_fun, StopFun},
      ParentFunValue :: undefined  | term(),
      StopFun :: undefined  | fun(() -> term()),
      KeyComparison :: '=:=' | '==',
      Position :: pos_integer(),
      Keys :: [term()],
      Mod :: atom(),
      Fun :: atom(),
      Args :: [term()],
      QH :: query_handle()).
table(TraverseFun, Options) when is_function(TraverseFun) ->
    case {is_function(TraverseFun, 0), 
          IsFun1 = is_function(TraverseFun, 1)} of
        {false, false} ->
            erlang:error(badarg, [TraverseFun, Options]);
        _ ->
            case options(Options, [pre_fun, post_fun, info_fun, format_fun, 
                                   lookup_fun, parent_fun, key_equality]) of
                [PreFun, PostFun, InfoFun, FormatFun, LookupFun, ParentFun,
                 KeyEquality] ->
                    T = #qlc_table{trav_fun = TraverseFun, pre_fun = PreFun,
                                   post_fun = PostFun, info_fun = InfoFun, 
                                   parent_fun = ParentFun,
                                   trav_MS = IsFun1,
                                   format_fun = FormatFun, 
                                   lookup_fun = LookupFun,
                                   key_equality = KeyEquality},
                    #qlc_handle{h = T};
                badarg ->
                    erlang:error(badarg, [TraverseFun, Options])
            end
    end;
table(T1, T2) ->
    erlang:error(badarg, [T1, T2]).

-spec(transform_from_evaluator(LC, Bs) -> Return when
      LC :: abstract_expr(),
      Bs :: erl_eval:binding_struct(),
      Return :: {ok, abstract_expr()}
              | {not_ok, {error, module(), Reason :: term()}}).

transform_from_evaluator(LC, Bs0) ->
    qlc_pt:transform_from_evaluator(LC, Bs0).

-define(TEMPLATE_STATE, 1).

template_state() ->
    ?TEMPLATE_STATE.

aux_name(Name, N, AllNames) ->
    {VN, _} = aux_name1(Name, N, AllNames),
    VN.

name_suffix(A, Suff) ->
    list_to_atom(lists:concat([A, Suff])).

vars(E) ->
    var_ufold(fun({var,_L,V}) -> V end, E).

var_ufold(F, E) ->
    ordsets:from_list(var_fold(F, [], E)).

all_selections([]) ->
    [[]];
all_selections([{I,Cs} | ICs]) ->
    [[{I,C} | L] || C <- Cs, L <- all_selections(ICs)].

%%%
%%% Local functions
%%%

merge_binding_structs(Bs1, Bs2) ->
    lists:foldl(fun({N, V}, Bs) -> erl_eval:add_binding(N, V, Bs)
                end, Bs1, erl_eval:bindings(Bs2)).

aux_name1(Name, N, AllNames) ->
    SN = name_suffix(Name, N),
    case sets:is_element(SN, AllNames) of
        true -> aux_name1(Name, N + 1, AllNames);
        false -> {SN, N}
    end.

var_fold(F, A, {var,_,V}=Var) when V =/= '_' ->
    [F(Var) | A];
var_fold(F, A, T) when is_tuple(T) ->
    var_fold(F, A, tuple_to_list(T));
var_fold(F, A, [E | Es]) ->
    var_fold(F, var_fold(F, A, E), Es);
var_fold(_F, A, _T) ->
    A.

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options0, [Key | Keys], L) when is_list(Options0) ->
    Options = case lists:member(Key, Options0) of
                  true -> 
                      [atom_option(Key) | lists:delete(Key, Options0)];
                  false ->
                      Options0
              end,
    V = case lists:keyfind(Key, 1, Options) of
            {format_fun, U=undefined} ->
                {ok, U};
            {info_fun, U=undefined} ->
                {ok, U};
            {lookup_fun, U=undefined} ->
                {ok, U};
            {parent_fun, U=undefined} ->
                {ok, U};
            {post_fun, U=undefined} ->
                {ok, U};
            {pre_fun, U=undefined} ->
                {ok, U};
            {info_fun, Fun} when is_function(Fun, 1) ->
                {ok, Fun};
            {pre_fun, Fun} when is_function(Fun, 1) ->
                {ok, Fun};
            {post_fun, Fun} when is_function(Fun, 0) ->
                {ok, Fun};
            {lookup_fun, Fun} when is_function(Fun, 2) ->
                {ok, Fun};
            {max_lookup, Max} when is_integer(Max), Max >= 0 ->
                {ok, Max};
            {max_lookup, infinity} ->
                {ok, -1};
            {format_fun, Fun} when is_function(Fun, 1) ->
                {ok, Fun};
            {parent_fun, Fun} when is_function(Fun, 0) ->
                {ok, Fun};
            {key_equality, KE='=='} ->
                {ok, KE};
            {key_equality, KE='=:='} ->
                {ok, KE};
            {join, J=any} ->
                {ok, J};
            {join, J=nested_loop} ->
                {ok, J};
            {join, J=merge} ->
                {ok, J};
            {join, J=lookup} ->
                {ok, J};
            {lookup, LookUp} when is_boolean(LookUp); LookUp =:= any ->
                {ok, LookUp};
            {max_list_size, Max} when is_integer(Max), Max >= 0 ->
                {ok, Max};
            {tmpdir_usage, TmpUsage} when TmpUsage =:= allowed;
                                          TmpUsage =:= not_allowed;
                                          TmpUsage =:= info_msg;
                                          TmpUsage =:= warning_msg;
                                          TmpUsage =:= error_msg ->
                {ok, TmpUsage};
            {unique, Unique} when is_boolean(Unique) ->
                {ok, Unique};
            {cache, Cache} when is_boolean(Cache); Cache =:= list ->
                {ok, Cache};
            {cache, ets} ->
                {ok, true};
            {cache, no} ->
                {ok, false};
            {unique_all, UniqueAll} when is_boolean(UniqueAll) ->
                {ok, UniqueAll};
            {cache_all, CacheAll} when is_boolean(CacheAll);
                                       CacheAll =:= list ->
                {ok, CacheAll};
            {cache_all, ets} ->
                {ok, true};
            {cache_all, no} ->
                {ok, false};
            {spawn_options, default} ->
                {ok, default};
            {spawn_options, SpawnOptions} ->
                case is_proper_list(SpawnOptions) of
                    true -> 
                        {ok, SpawnOptions};
                    false ->
                        badarg
                end;
            {flat, Flat} when is_boolean(Flat) ->
                {ok, Flat};
            {format, Format} when Format =:= string;
                                  Format =:= abstract_code;
                                  Format =:= debug ->
                {ok, Format};
            {n_elements, NElements} when NElements =:= infinity;
                                         is_integer(NElements),
                                         NElements > 0 ->
                {ok, NElements};
            {depth, Depth} when Depth =:= infinity;
                                is_integer(Depth), Depth >= 0 ->
                {ok, Depth};
            {order, Order} when is_function(Order, 2);
                                (Order =:= ascending);
                                (Order =:= descending) ->
                {ok, Order};
            {compressed, Comp} when Comp ->
                {ok, [compressed]};
            {compressed, Comp} when not Comp ->
                {ok, []};
            {tmpdir, T} ->
                {ok, T};
            {size, Size} when is_integer(Size), Size > 0 ->
                {ok, Size};
            {no_files, NoFiles} when is_integer(NoFiles), NoFiles > 1 ->
                {ok, NoFiles};
            {Key, _} ->
                badarg;
            false ->
                Default = default_option(Key),
                {ok, Default}
        end,
    case V of
        badarg ->
            badarg;
        {ok, Value} ->
            NewOptions = lists:keydelete(Key, 1, Options),
            options(NewOptions, Keys, [Value | L])
    end;
options([], [], L) ->
    lists:reverse(L);
options(_Options, _, _L) ->
    badarg.

default_option(pre_fun) -> undefined;
default_option(post_fun) -> undefined;
default_option(info_fun) -> undefined;
default_option(format_fun) -> undefined;
default_option(lookup_fun) -> undefined;
default_option(max_lookup) -> -1;
default_option(join) -> any;
default_option(lookup) -> any;
default_option(parent_fun) -> undefined;
default_option(key_equality) -> '=:=';
default_option(spawn_options) -> default;
default_option(flat) -> true;
default_option(format) -> string;
default_option(n_elements) -> infinity;
default_option(depth) -> infinity;
default_option(max_list_size) -> ?MAX_LIST_SIZE;
default_option(tmpdir_usage) -> allowed;
default_option(cache) -> false;
default_option(cache_all) -> false;
default_option(unique) -> false;
default_option(unique_all) -> false;
default_option(order) -> ascending; % default values from file_sorter.erl
default_option(compressed) -> [];
default_option(tmpdir) -> "";
default_option(size) -> 524288;
default_option(no_files) -> 16.

atom_option(cache) -> {cache, true};
atom_option(unique) -> {unique, true};
atom_option(cache_all) -> {cache_all, true};
atom_option(unique_all) -> {unique_all, true};
atom_option(lookup) -> {lookup, true};
atom_option(flat) -> {flat, true};
atom_option(Key) -> Key.

is_proper_list([_ | L]) ->
    is_proper_list(L);
is_proper_list(L) ->
    L =:= [].

spawn_options(default) ->
    [link];
spawn_options(SpawnOptions) ->
    lists:delete(monitor, 
                 case lists:member(link, SpawnOptions) of
                     true -> 
                         SpawnOptions;
                     false ->
                         [link | SpawnOptions]
                 end).

is_keypos(Keypos) when is_integer(Keypos), Keypos > 0 ->
    true;
is_keypos([]) ->
    false;
is_keypos(L) ->
    is_keyposs(L).

is_keyposs([Kp | Kps]) when is_integer(Kp), Kp > 0 ->
    is_keyposs(Kps);
is_keyposs(Kps) ->
    Kps =:= [].

listify(L) when is_list(L) ->
    L;
listify(T) ->
    [T].

%% Optimizations to be carried out.
-record(optz,
        {unique = false,    % boolean()
         cache = false,     % boolean() | list
         join_option = any, % constraint set by the 'join' option
         fast_join = no,    % no | #qlc_join. 'no' means nested loop.
         opt                % #qlc_opt
        }).

%% Prepared #qlc_lc.
-record(qlc,
        {lcf,       % fun() -> Val
         codef,
         qdata,     % with evaluated list expressions
         init_value,
         optz       % #optz
        }).

%% Prepared simple #qlc_lc.
-record(simple_qlc,
        {p,         % atom(), pattern variable
         le,
         line :: erl_anno:anno(),
         init_value,
         optz       % #optz
         }).

-record(prepared,
        {qh,     % #qlc_append | #qlc_table | #qlc | #simple_qlc | 
                 % #qlc_sort | list()
         sorted = no,  % yes | no | ascending | descending
         sort_info = [], % 
         sort_info2 = [], % 'sort_info' updated with pattern info; qh is LE
         lu_skip_quals = [], % qualifiers to skip due to lookup
         join = {[],[]},  % {Lookup, Merge}
         n_objs = undefined,   % for join (not used yet)
         is_unique_objects = false, % boolean()
         is_cached = false          % boolean() (true means 'ets' or 'list')
        }).

%%% Cursor process functions.

cursor_process(H, GUnique, GCache, TmpDir, SpawnOptions, MaxList, TmpUsage) ->
    Parent = self(),
    Setup = #setup{parent = Parent},
    CF = fun() -> 
                 %% Unless exit/2 is trapped no cleanup can be done.
                 %% The user is assumed not to set the flag to false.
                 process_flag(trap_exit, true),
                 MonRef = erlang:monitor(process, Parent),
                 {Objs, Post, _LocalPost} = 
                     try 
                         Prep = prepare_qlc(H, not_a_list, GUnique, GCache, 
                                            TmpDir, MaxList, TmpUsage),
                         setup_qlc(Prep, Setup)
                     catch Class:Reason:Stacktrace ->
                           Parent ! {self(),
                                     {caught, Class, Reason, Stacktrace}},
                           exit(normal)
                     end,
                 Parent ! {self(), ok},
                 wait_for_request(Parent, MonRef, Post), 
                 reply(Parent, MonRef, Post, Objs)
         end,
    Pid = spawn_opt(CF, SpawnOptions),
    parent_fun(Pid, Parent).

%% Expect calls from tables calling the parent_fun and finally an 'ok'.
parent_fun(Pid, Parent) ->
    receive 
        {Pid, ok} -> Pid;
        {TPid, {parent_fun, Fun}} ->
            V = try 
                    {value, Fun()}
                catch Class:Reason:Stacktrace ->
                    {parent_fun_caught, Class, Reason, Stacktrace}
            end,
            TPid ! {Parent, V},
            parent_fun(Pid, Parent);
        {Pid, {caught, throw, Error, [?THROWN_ERROR | _]}} ->
            Error;
        {Pid, {caught, Class, Reason, Stacktrace}} ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

reply(Parent, MonRef, Post, []) ->
    no_more(Parent, MonRef, Post);
reply(Parent, MonRef, Post, [Answer | Cont]) ->
    Parent ! {self(), {answer, Answer}},    
    wait_for_request(Parent, MonRef, Post),
    reply(Parent, MonRef, Post, Cont);
reply(Parent, MonRef, Post, Cont) ->
    Reply = try 
                if 
                    is_function(Cont) ->
                        Cont();
                    true ->
                        throw_error(Cont)
                end
            catch 
                Class:Reason:Stacktrace ->
                   post_funs(Post),
                   Message = {caught, Class, Reason, Stacktrace},
                   Parent ! {self(), Message},
                   exit(normal)
            end,
    reply(Parent, MonRef, Post, Reply).

no_more(Parent, MonRef, Post) ->
    Parent ! {self(), no_more},
    wait_for_request(Parent, MonRef, Post),
    no_more(Parent, MonRef, Post).

wait_for_request(Parent, MonRef, Post) ->
    receive 
        {Parent, stop} ->
            post_funs(Post),
            exit(normal);
        {Parent, more} ->
            ok;
        {'EXIT', Parent, _Reason} ->
            post_funs(Post),
            exit(normal);
        {'DOWN', MonRef, process, Parent, _Info} ->
            post_funs(Post),
            exit(normal);
        {'EXIT', Pid, _Reason} when Pid =:= self() ->
            %% Trapped signal. The cursor ignores it...
            wait_for_request(Parent, MonRef, Post);
        Other ->
            error_logger:error_msg(
              "The qlc cursor ~w received an unexpected message:\n~tp\n",
              [self(), Other]),
            wait_for_request(Parent, MonRef, Post)
    end.

%%% End of cursor process functions.

abstract_code({special, Line, String}) ->
    {string, Line, String};
abstract_code(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(abstract_code(tuple_to_list(Tuple)));
abstract_code([H | T]) ->
    [abstract_code(H) | abstract_code(T)];
abstract_code(Term) ->
    Term.

%% Also in qlc_pt.erl.
-define(Q, q).
-define(QLC_Q(L1, L2, L3, L4, LC, Os), 
        {call,L1,{remote,L2,{atom,L3,?MODULE},{atom,L4,?Q}},[LC | Os]}).

abstract(Info, false=_Flat, NElements, Depth) ->
    abstract(Info, NElements, Depth);
abstract(Info, true=_Flat, NElements, Depth) ->
    Abstract = abstract(Info, NElements, Depth),
    Vars = abstract_vars(Abstract),
    {_, Body0, Expr} = flatten_abstr(Abstract, 1, Vars, []),
    case Body0 of
        [] -> 
            Expr;
        [{match,_,Expr,Q}] ->
            Q;
        [{match,_,Expr,Q} | Body] ->
            {block, anno0(), lists:reverse(Body, [Q])};
        _ ->
            {block, anno0(), lists:reverse(Body0, [Expr])}
    end.

abstract(Info, NElements, Depth) ->
    abstract1(Info, NElements, Depth, anno1()).

abstract1({qlc, E0, Qs0, Opt}, NElements, Depth, A) ->
    Qs = lists:map(fun({generate, P, LE}) ->
                           {generate, A, binary_to_term(P),
                            abstract1(LE, NElements, Depth, A)};
                      (F) -> 
                           binary_to_term(F)
                   end, Qs0),
    E = binary_to_term(E0),
    Os = case Opt of
             [] -> [];
             _ -> [abstract_term(Opt, 1)]
         end,
    ?QLC_Q(A, A, A, A, {lc,A,E,Qs}, Os);
abstract1({table, {M, F, As0}}, _NElements, _Depth, Anno)
                          when is_atom(M), is_atom(F), is_list(As0) ->
    As = [abstract_term(A, 1) || A <- As0],
    {call, Anno, {remote, Anno, {atom, Anno, M}, {atom, Anno, F}}, As};
abstract1({table, TableDesc}, _NElements, _Depth, _A) ->
    case io_lib:deep_char_list(TableDesc) of
        true ->
            {ok, Tokens, _} =
                erl_scan:string(lists:flatten(TableDesc++"."), 1, [text]),
            {ok, Es, Bs} =
                lib:extended_parse_exprs(Tokens),
            [Expr] = lib:subst_values_for_vars(Es, Bs),
            special(Expr);
        false -> % abstract expression
            TableDesc
    end;
abstract1({append, Infos}, NElements, Depth, A) ->
    As = lists:foldr(fun(Info, As0) -> 
                             {cons,A,abstract1(Info, NElements, Depth, A),
                              As0}
                     end, {nil, A}, Infos),
    {call, A, {remote, A, {atom, A, ?MODULE}, {atom, A, append}}, [As]};
abstract1({sort, Info, SortOptions}, NElements, Depth, A) ->
    {call, A, {remote, A, {atom, A, ?MODULE}, {atom, A, sort}},
     [abstract1(Info, NElements, Depth, A), abstract_term(SortOptions, 1)]};
abstract1({keysort, Info, Kp, SortOptions}, NElements, Depth, A) ->
    {call, A, {remote, A, {atom, A, ?MODULE}, {atom, A, keysort}},
     [abstract_term(Kp, 1), abstract1(Info, NElements, Depth, A),
      abstract_term(SortOptions, 1)]};
abstract1({list,L,MS}, NElements, Depth, A) ->
    {call, A, {remote, A, {atom, A, ets}, {atom, A, match_spec_run}},
     [abstract1(L, NElements, Depth, A),
      {call, A, {remote, A, {atom, A, ets}, {atom, A, match_spec_compile}},
       [abstract_term(depth(MS, Depth), 1)]}]};
abstract1({list, L}, NElements, Depth, _A) when NElements =:= infinity;
                                                NElements >= length(L) ->
    abstract_term(depth(L, Depth), 1);
abstract1({list, L}, NElements, Depth, _A) ->
    abstract_term(depth(lists:sublist(L, NElements), Depth) ++ '...', 1).

special({value, _, Thing}) ->
    abstract_term(Thing);
special(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(special(tuple_to_list(Tuple)));
special([E|Es]) ->
    [special(E)|special(Es)];
special(Expr) ->
    Expr.

depth(List, infinity) ->
    List;
depth(List, Depth) ->
    [depth1(E, Depth) || E <- List].

depth_fun(infinity = _Depth) ->
    fun(E) -> E end;
depth_fun(Depth) ->
    fun(E) -> depth1(E, Depth) end.

depth1([]=L, _D) ->
    L;
depth1(_Term, 0) ->
    '...';
depth1(Tuple, D) when is_tuple(Tuple) ->
    depth_tuple(Tuple, tuple_size(Tuple), 1, D - 1, []);
depth1(List, D) when is_list(List) ->
    if 
        D =:= 1 ->
            ['...'];
        true -> 
            depth_list(List, D - 1)
    end;
depth1(Binary, D) when byte_size(Binary) > D - 1 ->
    D1 = D - 1,
    <<Bin:D1/bytes,_/bytes>> = Binary,
    <<Bin/bytes,"...">>;
depth1(T, _Depth) ->
    T.

depth_list([]=L, _D) ->
    L;
depth_list(_L, 0) ->
    '...';
depth_list([E | Es], D) ->
    [depth1(E, D) | depth_list(Es, D - 1)].

depth_tuple(_Tuple, Sz, I, _D, L) when I > Sz ->
    list_to_tuple(lists:reverse(L));
depth_tuple(_L, _Sz, _I, 0, L) ->
    list_to_tuple(lists:reverse(L, ['...']));
depth_tuple(Tuple, Sz, I, D, L) ->
    E = depth1(element(I, Tuple), D),
    depth_tuple(Tuple, Sz, I + 1, D - 1, [E | L]).

abstract_term(Term) ->
    abstract_term(Term, 0).

abstract_term(Term, Line) ->
    abstr_term(Term, anno(Line)).

abstr_term(Tuple, Line) when is_tuple(Tuple) ->
    {tuple,Line,[abstr_term(E, Line) || E <- tuple_to_list(Tuple)]};
abstr_term([_ | _]=L, Line) ->
    case io_lib:char_list(L) of
        true ->
            erl_parse:abstract(L, erl_anno:line(Line));
        false ->
            abstr_list(L, Line)
    end;
abstr_term(Fun, Line) when is_function(Fun) ->
    case erl_eval:fun_data(Fun) of
        {fun_data, _Bs, Cs} ->
            {'fun', Line, {clauses, Cs}};
        {named_fun_data, _Bs, Name, Cs} ->
            {named_fun, Line, Name, Cs};
        false ->
            {name, Name} = erlang:fun_info(Fun, name),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            case erlang:fun_info(Fun, type) of
                {type, external} ->
                    {module, Module} = erlang:fun_info(Fun, module),
                    {'fun', Line, {function,
				   {atom,Line,Module},
				   {atom,Line,Name},
				   {integer,Line,Arity}}};
                {type, local} ->
                    {'fun', Line, {function,Name,Arity}}
            end
    end;
abstr_term(PPR, Line) when is_pid(PPR); is_port(PPR); is_reference(PPR) ->
    {special, Line, lists:flatten(io_lib:write(PPR))};    
abstr_term(Map, Line) when is_map(Map) ->
    {map,Line,
     [{map_field_assoc,Line,abstr_term(K, Line),abstr_term(V, Line)} ||
         {K,V} <- maps:to_list(Map)]};
abstr_term(Simple, Line) ->
    erl_parse:abstract(Simple, erl_anno:line(Line)).

abstr_list([H | T], Line) ->
    {cons, Line, abstr_term(H, Line), abstr_list(T, Line)};
abstr_list(T, Line) ->
    abstr_term(T, Line).

%% Since generator pattern variables cannot be used in list
%% expressions, it is OK to flatten out QLCs using temporary
%% variables.
flatten_abstr(?QLC_Q(L1, L2, L3, L4, LC0, Os), VN0, Vars, Body0) ->
    {lc,L,E,Qs0} = LC0,
    F = fun({generate,Ln,P,LE0}, {VN1,Body1}) ->
                {VN2,Body2,LE} = flatten_abstr(LE0, VN1, Vars, Body1),
                {{generate,Ln,P,LE}, {VN2,Body2}};
           (Fil, VN_Body) ->
                {Fil, VN_Body}
        end,
    {Qs, {VN3,Body}} = lists:mapfoldl(F, {VN0,Body0}, Qs0),
    LC = {lc,L,E,Qs},
    {V, VN} = aux_name1('V', VN3, Vars),
    Var = {var, L1, V},
    QLC = ?QLC_Q(L1, L2, L3, L4, LC, Os),
    {VN + 1, [{match, L1, Var, QLC} | Body], Var};
flatten_abstr(T0, VN0, Vars, Body0) when is_tuple(T0) ->
    {VN, Body, L} = flatten_abstr(tuple_to_list(T0), VN0, Vars, Body0),
    {VN, Body, list_to_tuple(L)};
flatten_abstr([E0 | Es0], VN0, Vars, Body0) ->
    {VN1, Body1, E} = flatten_abstr(E0, VN0, Vars, Body0),
    {VN, Body, Es} = flatten_abstr(Es0, VN1, Vars, Body1),
    {VN, Body, [E | Es]};
flatten_abstr(E, VN, _Vars, Body) ->
    {VN, Body, E}.

abstract_vars(Abstract) ->
    sets:from_list(ordsets:to_list(vars(Abstract))).

collect([]=L) ->
    L;
collect([Answer | Cont]) ->
    [Answer | collect(Cont)];
collect(Cont) ->
    case Cont() of
        Answers when is_list(Answers) ->
            collect(Answers);
        Term ->
            throw_error(Term)
    end.

fold_loop(Fun, [Obj | Cont], Acc) ->
    fold_loop(Fun, Cont, Fun(Obj, Acc));
fold_loop(_Fun, [], Acc) ->
    Acc;
fold_loop(Fun, Cont, Acc) ->
    case Cont() of
        Objects when is_list(Objects) ->
            fold_loop(Fun, Objects, Acc);
        Term -> 
            Term
    end.

next_loop(Pid, L, N) when N =/= 0 ->
    case monitor_request(Pid, more) of
        no_more ->
            lists:reverse(L);
        {answer, Answer} ->
            next_loop(Pid, [Answer | L], N - 1);
        {caught, throw, Error, [?THROWN_ERROR | _]} ->
            Error;
        {caught, Class, Reason, Stacktrace} ->
            {current_stacktrace, CurrentStacktrace} =
                erlang:process_info(self(), current_stacktrace),
            erlang:raise(Class, Reason, Stacktrace ++ CurrentStacktrace);
        error ->
            erlang:error({qlc_cursor_pid_no_longer_exists, Pid})
    end;
next_loop(_Pid, L, _N) ->
    lists:reverse(L).

stop_cursor(Pid) ->
    erlang:monitor(process, Pid),
    unlink(Pid),
    receive
        {'EXIT',Pid,_Reason} -> % Simply ignore the error.
            receive 
                {'DOWN',_,process,Pid,_} -> ok
            end
    after 0 -> 
            Pid ! {self(),stop},
            receive
                {'DOWN',_,process,Pid,_} -> ok
            end
    end.

monitor_request(Pid, Req) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Req},
    receive 
        {'DOWN', Ref, process, Pid, _Info} ->
            receive
                {'EXIT', Pid, _Reason} -> ok
            after 1 -> ok end,
            error;
        {'EXIT', Pid, _Reason} ->
            receive 
                {'DOWN', _, process, Pid, _} -> error
            end;
        {Pid, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply
    end.

%% Marker for skipped filter or unused generator.
-define(SKIP, (-1)).

%% Qual = {gen, LE} | fil
-define(qual_data(QNum, GoToIndex, State, Qual), 
        {QNum, GoToIndex, State, Qual}).

-record(join, % generated by qlc_pt
        {op, q1, q2, wh1, wh2, cs_fun}). % op is unused

%% le_info/1 returns an intermediate information format only used for
%% testing purposes. Changes will happen without notice.
%%
%% QueryDesc = {qlc, TemplateDesc, [QualDesc], [QueryOpt]} 
%%           | {table, TableDesc}
%%           | {append, [QueryDesc]}
%%           | {sort, QueryDesc, [sort_option()]}
%%           | {keysort, key_pos(), QueryDesc, [sort_option()]}
%%           | {list, list()}
%%           | {list, QueryDesc, match_expression()}
%% TableDesc = {Mod, Fun, Args}
%%           | erl_parse:abstract_expr()
%%           | string()
%% Mod = module()
%% Fun = atom()
%% Args = [term()]
%% QualDesc = FilterDesc
%%          | {generate, PatternDesc, QueryDesc}
%% QueryOpt = {cache, boolean()} | cache
%%          | {unique, boolean()} | unique
%% FilterDesc = PatternDesc = TemplateDesc = binary()

le_info(#prepared{qh = #simple_qlc{le = LE, p = P, line = L, optz = Optz}},
        InfOpt) ->
    QVar = term_to_binary({var, L, P}),
    {qlc, QVar, [{generate, QVar, le_info(LE, InfOpt)}], opt_info(Optz)};
le_info(#prepared{qh = #qlc{codef = CodeF, qdata = Qdata, optz = Optz}},
        InfOpt) ->
    Code = CodeF(),
    TemplateState = template_state(),
    E = element(TemplateState, Code),
    QualInfo0 = qual_info(Qdata, Code, InfOpt),
    QualInfo1 = case Optz#optz.fast_join of
                    #qlc_join{} = Join ->
                        join_info(Join, QualInfo0, Qdata, Code);
                    no ->
                        QualInfo0
                end,
    QualInfo = [I || I <- QualInfo1, I =/= skip],
    {qlc, E, QualInfo, opt_info(Optz)};
le_info(#prepared{qh = #qlc_table{format_fun = FormatFun, trav_MS = TravMS, 
                                  ms = MS, lu_vals = LuVals}}, InfOpt) ->
    {NElements, Depth} = InfOpt,
    %% The 'depth' option applies to match specifications as well.
    %% This is for limiting imported variables (parameters).
    DepthFun = depth_fun(Depth),
    case LuVals of
        _ when FormatFun =:= undefined ->
            {table, {'$MOD', '$FUN', []}};
        {Pos, Vals} ->
            Formated = try FormatFun({lookup, Pos, Vals, NElements, DepthFun})
                       catch _:_ -> FormatFun({lookup, Pos, Vals})
                       end,
            if
                MS =:= no_match_spec ->
                    {table, Formated};
                true ->
                    {list, {table, Formated}, depth(MS, Depth)}
            end;
        _ when TravMS, is_list(MS) ->
            {table, FormatFun({match_spec, depth(MS, Depth)})};
        _ when MS =:= no_match_spec ->
            try {table, FormatFun({all, NElements, DepthFun})}
            catch _:_ -> {table, FormatFun(all)}
            end
    end;
le_info(#prepared{qh = #qlc_append{hl = HL}}, InfOpt) ->
    {append, [le_info(H, InfOpt) || H <- HL]};
le_info(#prepared{qh = #qlc_sort{h = H, keypos = sort, 
                                 fs_opts = SortOptions0, tmpdir = TmpDir}},
        InfOpt) ->
    SortOptions = sort_options_global_tmp(SortOptions0, TmpDir),
    {sort, le_info(H, InfOpt), SortOptions};
le_info(#prepared{qh = #qlc_sort{h = H, keypos = {keysort, Kp}, 
                                 fs_opts = SortOptions0, tmpdir = TmpDir}},
        InfOpt) ->
    SortOptions = sort_options_global_tmp(SortOptions0, TmpDir),
    {keysort, le_info(H, InfOpt), Kp, SortOptions};
le_info(#prepared{qh = #qlc_list{l = L, ms = no_match_spec}}, _InfOpt) ->
    {list, L};
le_info(#prepared{qh = #qlc_list{l = L, ms = MS}},_InfOpt) when is_list(L) ->
    {list, {list, L}, MS};
le_info(#prepared{qh = #qlc_list{l = L, ms = MS}}, InfOpt) ->
    {list, le_info(L, InfOpt), MS}.

qual_info([?qual_data(_QNum, _GoI, ?SKIP, fil) | Qdata], Code, InfOpt) ->
    %% see skip_lookup_filters()
    [skip | qual_info(Qdata, Code, InfOpt)];
qual_info([?qual_data(QNum, _GoI, _SI, fil) | Qdata], Code, InfOpt) ->
    [element(QNum + 1, Code) | qual_info(Qdata, Code, InfOpt)];
qual_info([?qual_data(_QNum, _GoI, _SI, {gen,#join{}}) | Qdata], 
          Code, InfOpt) ->
    [skip | qual_info(Qdata, Code, InfOpt)];
qual_info([?qual_data(QNum, _GoI, _SI, {gen,LE}) | Qdata], Code, InfOpt) ->
    [{generate,element(QNum + 1, Code),le_info(LE, InfOpt)} | 
     qual_info(Qdata, Code, InfOpt)];
qual_info([], _Code, _InfOpt) ->
    [].

join_info(Join, QInfo, Qdata, Code) ->
    #qlc_join{kind = Kind, q1 = QNum1a, c1 = C1, q2 = QNum2a, c2 = C2, 
              opt = Opt} = Join,
    {?qual_data(JQNum,_,_,_), Rev, QNum1, QNum2, _WH1, _WH2, CsFun} = 
        find_join_data(Qdata, QNum1a, QNum2a),
    {Cs1_0, Cs2_0, Compat} = CsFun(),
    [Cs1, Cs2] = case Compat of
                     [] -> % --R12B-5
                         [[{C,[{V,'=:='} || V <- Vs]} || {C,Vs} <- CVs] || 
                             CVs <- [Cs1_0, Cs2_0]];
                     _ -> % 'v1', R13A --
                         %% Only compared constants (==).
                         [Cs1_0, Cs2_0]
                 end,
    L = anno0(),
    G1_0 = {var,L,'G1'}, G2_0 = {var,L,'G2'},
    JP = element(JQNum + 1, Code),
    %% Create code for wh1 and wh2 in #join{}:
    {{I1,G1}, {I2,G2}, QInfoL} = 
        case Kind of
            {merge, _} -> 
                {JG1,QInfo1} = join_merge_info(QNum1, QInfo, Code, G1_0, Cs1),
                {JG2,QInfo2} = join_merge_info(QNum2, QInfo, Code, G2_0, Cs2),
                {JG1, JG2, QInfo1 ++ QInfo2};
            _ when Rev ->
                {JG2,QInfo2} = join_merge_info(QNum2, QInfo, Code, G2_0, Cs2),
                {J1, QInfo1} = join_lookup_info(QNum1, QInfo, G1_0),
                {{J1,G1_0}, JG2, QInfo2 ++ [QInfo1]};
            _ ->
                {JG1,QInfo1} = join_merge_info(QNum1, QInfo, Code, G1_0, Cs1),
                {J2, QInfo2} = join_lookup_info(QNum2, QInfo, G2_0),
                {JG1, {J2,G2_0}, QInfo1 ++ [QInfo2]}
        end,
    {JOptVal, JOp} = kind2op(Kind),
    JOpt = [{join, JOptVal}] ++ opt_info(join_unique_cache(Opt)),
    JFil = term_to_binary({op,L,JOp,
                           {call,L,{atom,L,element},[{integer,L,C1},G1]},
                           {call,L,{atom,L,element},[{integer,L,C2},G2]}}),
    P = term_to_binary({cons, L, G1, G2}),
    JInfo = {generate, JP, {qlc, P, QInfoL ++ [JFil], JOpt}},
    {Before, [I1 | After]} = lists:split(QNum1 - 1, QInfo),
    Before ++ [JInfo] ++ lists:delete(I2, After).

kind2op({merge, _KE}) -> {merge, '=='};
kind2op({lookup, KE, _LU_fun}) -> {lookup, KE}.

%% qlc:q(P0 || P0 = Pattern <- H1, ConstFilters), 
%% where "P0" is a fresh variable and ConstFilters are filters that
%% test constant values of pattern columns.
join_merge_info(QNum, QInfo, Code, G, ExtraConstants) ->
    {generate, _, LEInfo}=I = lists:nth(QNum, QInfo),
    P = binary_to_term(element(QNum + 1, Code)),
    case {P, ExtraConstants} of
        {{var, _, _}, []} ->
            %% No need to introduce a QLC.
            TP = term_to_binary(G),
            I2 = {generate, TP, LEInfo},
            {{I,G}, [I2]};
        _ -> 
            {EPV, M} = 
                case P of
                    {var, _, _} -> 
                        %% No need to introduce a pattern variable.
                        {P, P};
                    _ -> 
                        {PV, _} = aux_name1('P', 0, abstract_vars(P)),
                        L = erl_anno:new(0),
                        V = {var, L, PV},
                        {V, {match, L, V, P}}
                 end,
            DQP = term_to_binary(EPV),
            LEI = {generate, term_to_binary(M), LEInfo},
            TP = term_to_binary(G),
            CFs = [begin
                       A = anno0(),
                       Call = {call,A,{atom,A,element},[{integer,A,Col},EPV]},
                       F = list2op([{op,A,Op,abstract_term(Con),Call}
                                      || {Con,Op} <- ConstOps], 'or', A),
                       term_to_binary(F)
                   end ||
                      {Col,ConstOps} <- ExtraConstants],
            {{I,G}, [{generate, TP, {qlc, DQP, [LEI | CFs], []}}]}
    end.

list2op([E], _Op, _Anno) ->
    E;
list2op([E | Es], Op, Anno) ->
    {op,Anno,Op,E,list2op(Es, Op, Anno)}.

join_lookup_info(QNum, QInfo, G) ->
    {generate, _, LEInfo}=I = lists:nth(QNum, QInfo),
    TP = term_to_binary(G),
    {I, {generate, TP, LEInfo}}.

opt_info(#optz{unique = Unique, cache = Cache0, join_option = JoinOption}) ->
    %% No 'nested_loop' options are added here, even if there are
    %% nested loops to carry out, unless a 'nested_loop' was given as
    %% option. The reason is that the qlc module does not know about
    %% all instances of nested loops.
    Cache = if
                Cache0 -> ets;
                true -> Cache0
            end,
    [{T,V} || {T,V} <- [{cache,Cache},{unique,Unique}],
              V =/= default_option(T)] ++
    [{T,V} || {T,V} <- [{join,JoinOption}], V =:= nested_loop].

prepare_qlc(H, InitialValue, GUnique, GCache, TmpDir, MaxList, TmpUsage) ->
    GOpt = #qlc_opt{unique = GUnique, cache = GCache, 
                    tmpdir = TmpDir, max_list = MaxList, 
                    tmpdir_usage = TmpUsage},
    case opt_le(prep_le(H, GOpt), 1) of
        #prepared{qh = #qlc{} = QLC}=Prep ->
            Prep#prepared{qh = QLC#qlc{init_value = InitialValue}};
        #prepared{qh = #simple_qlc{}=SimpleQLC}=Prep ->
            Prep#prepared{qh = SimpleQLC#simple_qlc{init_value = InitialValue}};
        Prep ->
            Prep
    end.

%%% The options given to append, q and table (unique and cache) as well
%%% as the type of expression (list, table, append, qlc...) are
%%% analyzed by prep_le. The results are is_unique_objects and
%%% is_cached. It is checked that the evaluation (in the Erlang sense)
%%% of list expressions yields qlc handles.

prep_le(#qlc_lc{lc = LC_fun, opt = #qlc_opt{} = Opt0}=H, GOpt) ->
    #qlc_opt{unique = GUnique, cache = GCache, 
             tmpdir = TmpDir, max_list = MaxList, 
             tmpdir_usage = TmpUsage} = GOpt,
    Unique = Opt0#qlc_opt.unique or GUnique,
    Cache = if
                not GCache -> Opt0#qlc_opt.cache;
                true -> GCache
            end,
    Opt = Opt0#qlc_opt{unique = Unique, cache = Cache, 
                       tmpdir = TmpDir, max_list = MaxList, 
                       tmpdir_usage = TmpUsage},
    prep_qlc_lc(LC_fun(), Opt, GOpt, H);
prep_le(#qlc_table{info_fun = IF}=T, GOpt) ->
    {SortInfo, Sorted} = table_sort_info(T),
    IsUnique = grd(IF, is_unique_objects),
    Prep = #prepared{qh = T, sort_info = SortInfo, sorted = Sorted,
                     is_unique_objects = IsUnique},
    Opt = if 
              IsUnique or not GOpt#qlc_opt.unique,
              T#qlc_table.ms =:= no_match_spec -> 
                  GOpt#qlc_opt{cache = false};
              true ->
                  GOpt
          end,
    may_create_simple(Opt, Prep);
prep_le(#qlc_append{hl = HL}, GOpt) ->
    case lists:flatmap(fun(#prepared{qh = #qlc_list{l = []}}) -> [];
                          (#prepared{qh = #qlc_append{hl = HL1}}) -> HL1;
                          (H) -> [H] end, 
                       [prep_le(H, GOpt) || H <- HL]) of
        []=Nil -> 
            short_list(Nil);
        [Prep] -> 
            Prep;
        PrepL -> 
            Cache = lists:all(fun(#prepared{is_cached = IsC}) -> IsC =/= false
                              end, PrepL),
            %% The handles in hl are replaced by prepared handles:
            Prep = #prepared{qh = #qlc_append{hl = PrepL}, is_cached = Cache},
            may_create_simple(GOpt, Prep)
    end;
prep_le(#qlc_sort{h = H0}=Q0, GOpt) ->
    %% The handle h is replaced by a prepared handle:
    Q = Q0#qlc_sort{h = prep_le(H0, GOpt)},
    prep_sort(Q, GOpt);
prep_le([_, _ | _]=L, GOpt) ->
    Prep = #prepared{qh = #qlc_list{l = L}, is_cached = true},
    Opt = if 
              not GOpt#qlc_opt.unique -> 
                  GOpt#qlc_opt{cache = false}; 
             true -> GOpt 
          end,
    may_create_simple(Opt, Prep);
prep_le(L, _GOpt) when is_list(L) ->
    short_list(L);
prep_le(T, _GOpt) ->
    erlang:error({unsupported_qlc_handle, #qlc_handle{h = T}}).

eval_le(LE_fun, GOpt) ->
    case LE_fun() of
        {error, ?MODULE, _} = Error ->
            throw_error(Error);
        R -> 
            case get_handle(R) of
                badarg ->
                    erlang:error(badarg, [R]);
                H -> 
                    prep_le(H, GOpt)
            end
    end.

prep_qlc_lc({simple_v1, PVar, LE_fun, L}, Opt, GOpt, _H) ->
    check_lookup_option(Opt, false),
    prep_simple_qlc(PVar, anno(L), eval_le(LE_fun, GOpt), Opt);
prep_qlc_lc({qlc_v1, QFun, CodeF, Qdata0, QOpt}, Opt, GOpt, _H) ->
    F = fun(?qual_data(_QNum, _GoI, _SI, fil)=QualData, ModGens) -> 
                {QualData, ModGens};
           (?qual_data(_QNum, _GoI, _SI, {gen, #join{}})=QualData, ModGens) ->
                {QualData, ModGens};
           (?qual_data(QNum, GoI, SI, {gen, LE_fun}), ModGens0) -> 
                Prep1 = eval_le(LE_fun, GOpt),
                {Prep, ModGens} = 
                    prep_generator(QNum, Prep1, QOpt, Opt, ModGens0),
                {?qual_data(QNum, GoI, SI, {gen, Prep}), ModGens}
        end,
    {Qdata, ModGens} = lists:mapfoldl(F, [], Qdata0),
    SomeLookUp = lists:keymember(true, 2, ModGens),
    check_lookup_option(Opt, SomeLookUp),
    case ModGens of
        [{_QNum, _LookUp, all, OnePrep}] ->
            check_join_option(Opt),
            OnePrep;
        _ -> 
            Prep0 = prep_qlc(QFun, CodeF, Qdata, QOpt, Opt),
            LU_SkipQuals = 
                lists:flatmap(fun({QNum,_LookUp,Fs,_Prep}) -> [{QNum,Fs}]
                              end, ModGens),        
            Prep1 = Prep0#prepared{lu_skip_quals = LU_SkipQuals},
            prep_join(Prep1, QOpt, Opt)
    end;
prep_qlc_lc(_, _Opt, _GOpt, H) ->
    erlang:error({unsupported_qlc_handle, #qlc_handle{h = H}}).

prep_generator(QNum, Prep0, QOpt, Opt, ModGens) ->
    PosFun = fun(KeyEquality) -> pos_fun(KeyEquality, QOpt, QNum) end,
    MSFs = case match_specs(QOpt, QNum) of
               undefined ->
                   {no_match_spec, []};
               {_, _}=MSFs0 ->
                   MSFs0
         end,
    #prepared{qh = LE} = Prep0,
    case prep_gen(LE, Prep0, PosFun, MSFs, Opt) of
        {replace, Fs, LookUp, Prep} ->
            {Prep, [{QNum,LookUp,Fs,Prep} | ModGens]};
        {skip, SkipFils, LookUp, Prep} ->
            {Prep, [{QNum,LookUp,SkipFils,Prep} | ModGens]};
        {no, _Fs, _LookUp, Prep} ->
            {Prep, ModGens}
    end.

pos_fun(undefined, QOpt, QNum) ->
    {'=:=', constants(QOpt, QNum)}; %% --R12B-5
pos_fun('=:=', QOpt, QNum) ->
    {'=:=', constants(QOpt, QNum)};
pos_fun('==', QOpt, QNum) ->    
    try {'==', equal_constants(QOpt, QNum)} % R13A--
    catch _:_ -> {'=:=', constants(QOpt, QNum)}
    end.

prep_gen(#qlc_table{lu_vals = LuV0, ms = MS0, trav_MS = TravMS,
                    info_fun = IF, lookup_fun = LU_fun,
                    key_equality = KeyEquality}=LE0,
         Prep0, PosFun0, {MS, Fs}, Opt) ->
    PosFun = PosFun0(KeyEquality),
    {LuV, {STag,SkipFils}} = find_const_positions(IF, LU_fun, PosFun, Opt),
    LU = LuV =/= false,
    if
        LuV0 =/= undefined; MS0 =/= no_match_spec ->
            {no, [], false, Prep0};
        MS =/= no_match_spec, LU ->
            MS1 = if 
                      Fs =:= SkipFils; STag =:= Fs ->
                          %% The guard of the match specification 
                          %% is covered by the lookup.
                          case MS of
                              [{'$1',_Guard,['$1']}] -> % no transformation
                                  no_match_spec;
                              [{Head,_Guard,Body}] ->
                                  [{Head,[],Body}] % true guard
                          end;
                      true ->
                          MS
                  end,
            Prep = Prep0#prepared{qh = LE0#qlc_table{lu_vals = LuV,ms = MS1}},
            {replace, Fs, LU, Prep};
        LU ->
            Prep = Prep0#prepared{qh = LE0#qlc_table{lu_vals = LuV}},
            {skip, SkipFils, LU, Prep};
        TravMS, MS =/= no_match_spec ->
            Prep = Prep0#prepared{qh = LE0#qlc_table{ms = MS},
                                  is_unique_objects = false},
            {replace, Fs, false, may_create_simple(Opt, Prep)};
        true -> 
            {no, [], false, Prep0}
    end;
prep_gen(#qlc_list{l = []}, Prep0, _PosFun, {_MS, Fs}, _Opt) ->
    %% unique and cached
    {replace, Fs, false, Prep0};
prep_gen(#qlc_list{ms = no_match_spec}=LE0, Prep0, _PosFun, {MS, Fs}, Opt) 
                                  when MS =/= no_match_spec ->
    Prep = Prep0#prepared{qh = LE0#qlc_list{ms = MS}, 
                          is_cached = false},
    {replace, Fs, false, may_create_simple(Opt, Prep)};
prep_gen(#qlc_list{}, Prep0, _PosFun, {MS, Fs}, Opt) 
                                  when MS =/= no_match_spec ->
    ListMS = #qlc_list{l = Prep0, ms = MS},
    LE = #prepared{qh = ListMS, is_cached = false},
    {replace, Fs, false, may_create_simple(Opt, LE)};
prep_gen(_LE0, Prep0, _PosFun, _MSFs, _Opt) ->
    {no, [], false, Prep0}.

-define(SIMPLE_QVAR, 'SQV').

may_create_simple(#qlc_opt{unique = Unique, cache = Cache} = Opt, 
                  #prepared{is_cached = IsCached, 
                            is_unique_objects = IsUnique} = Prep) ->
    if 
        Unique and not IsUnique; 
        (Cache =/= false) and not IsCached ->
            prep_simple_qlc(?SIMPLE_QVAR, anno(1), Prep, Opt);
        true ->
            Prep
    end.

prep_simple_qlc(PVar, Line, LE, Opt) ->
    check_join_option(Opt),
    #prepared{is_cached = IsCached, 
              sort_info = SortInfo, sorted = Sorted,
              is_unique_objects = IsUnique} = LE,
    #qlc_opt{unique = Unique, cache = Cache} = Opt,
    Cachez = if
                 Unique -> Cache;
                 not IsCached -> Cache;
                 true -> false
             end,
    Optz = #optz{unique = Unique and not IsUnique, 
                 cache = Cachez, opt = Opt},
    QLC = #simple_qlc{p = PVar, le = LE, line = Line, 
                      init_value = not_a_list, optz = Optz},
    %% LE#prepared.join is not copied
    #prepared{qh = QLC, is_unique_objects = IsUnique or Unique, 
              sort_info = SortInfo, sorted = Sorted,
              is_cached = IsCached or (Cachez =/= false)}.

prep_sort(#qlc_sort{h = #prepared{sorted = yes}=Prep}, _GOpt) ->
    Prep;
prep_sort(#qlc_sort{h = #prepared{is_unique_objects = IsUniqueObjs}}=Q, 
          GOpt) ->
    S1 = sort_unique(IsUniqueObjs, Q),
    S2 = sort_tmpdir(S1, GOpt),
    S = S2#qlc_sort{tmpdir_usage = GOpt#qlc_opt.tmpdir_usage},
    {SortInfo, Sorted} = sort_sort_info(S),
    #prepared{qh = S, is_cached = true, sort_info = SortInfo,
              sorted = Sorted,
              is_unique_objects = S#qlc_sort.unique or IsUniqueObjs}.

prep_qlc(QFun, CodeF, Qdata0, QOpt, Opt) ->
    #qlc_opt{unique = Unique, cache = Cache, join = Join} = Opt,
    Optz = #optz{unique = Unique, cache = Cache, 
                 join_option = Join, opt = Opt},
    {Qdata, SortInfo} = qlc_sort_info(Qdata0, QOpt),
    QLC = #qlc{lcf = QFun, codef = CodeF, qdata = Qdata, 
               init_value = not_a_list, optz = Optz},
    #prepared{qh = QLC, sort_info = SortInfo,
              is_unique_objects = Unique, 
              is_cached = Cache =/= false}.

%% 'sorted', 'sorted_info', and 'sorted_info2' are used to avoid
%% sorting on a key when there is no need to sort on the key. 'sorted'
%% is set by qlc:sort() only; its purpose is to assure that if columns
%% 1 to i are constant, then column i+1 is key-sorted (always true if
%% the tuples are sorted). Note: the implementation is (too?) simple.
%% For instance, each column is annotated with 'ascending' or
%% 'descending' (not yet). More exact would be, as examples, 'always
%% ascending' and 'ascending if all preceding columns are constant'.
%%
%% The 'size' of the template is not used (size_of_qualifier(QOpt, 0)).

qlc_sort_info(Qdata0, QOpt) ->
    F = fun(?qual_data(_QNum, _GoI, _SI, fil)=Qd, Info) -> 
                {Qd, Info};
           (?qual_data(_QNum, _GoI, _SI, {gen, #join{}})=Qd, Info) ->
                {Qd, Info};
           (?qual_data(QNum, GoI, SI, {gen, PrepLE0}), Info) -> 
                PrepLE = sort_info(PrepLE0, QNum, QOpt),
                Qd = ?qual_data(QNum, GoI, SI, {gen, PrepLE}),
                I = [{{Column,Order}, [{traverse,QNum,C}]} ||
                        {{C,Order},What} <- PrepLE#prepared.sort_info2,
                        What =:= [], % Something else later...
                        Column <- equal_template_columns(QOpt, {QNum,C})],
                {Qd, [I | Info]}
        end,
    {Qdata, SortInfoL} = lists:mapfoldl(F, [], Qdata0),
    SortInfo0 = [{{Pos,Ord}, [template]} || 
                    Pos <- constant_columns(QOpt, 0), 
                    Ord <- orders(yes)]
           ++ lists:append(SortInfoL),
    SortInfo = family_union(SortInfo0),
    {Qdata, SortInfo}.

sort_info(#prepared{sort_info = SI, sorted = S} = Prep, QNum, QOpt) ->
    SI1 = [{{C,Ord},[]} || 
              S =/= no, 
              is_integer(Sz = size_of_qualifier(QOpt, QNum)), 
              Sz > 0, % the size of the pattern
              (NConstCols = size_of_constant_prefix(QOpt, QNum)) < Sz, 
              C <- [NConstCols+1],
              Ord <- orders(S)]
       ++ [{{Pos,Ord},[]} || Pos <- constant_columns(QOpt, QNum),
                             Ord <- orders(yes)]
       ++ [{PosOrd,[]} || {PosOrd,_} <- SI],
    SI2 = lists:usort(SI1),
    Prep#prepared{sort_info2 = SI2}. 

%orders(descending=O) ->
%    [O];
orders(ascending=O) ->
    [O];
orders(yes) ->
    [ascending
%   ,descending
    ].

sort_unique(true, #qlc_sort{fs_opts = SortOptions, keypos = sort}=Sort) ->
    Sort#qlc_sort{unique = false, 
                  fs_opts = 
                      lists:keydelete(unique, 1, 
                                      lists:delete(unique, SortOptions))};
sort_unique(_, Sort) ->
    Sort.

sort_tmpdir(S, #qlc_opt{tmpdir = ""}) ->
    S;
sort_tmpdir(S, Opt) ->
    S#qlc_sort{tmpdir = Opt#qlc_opt.tmpdir}.

short_list(L) ->
    %% length(L) < 2: all elements are known be equal
    #prepared{qh = #qlc_list{l = L}, sorted = yes, is_unique_objects = true, 
              is_cached = true}.
    
find_const_positions(IF, LU_fun, {KeyEquality, PosFun}, 
                     #qlc_opt{max_lookup = Max, lookup = Lookup}) 
           when is_function(LU_fun), is_function(PosFun), is_function(IF),
                Lookup =/= false ->
    case call(IF, keypos, undefined, [])  of
        undefined ->
            Indices = call(IF, indices, undefined, []),
            find_const_position_idx(Indices, KeyEquality, PosFun, Max, []);
        KeyPos ->
            case pos_vals(KeyPos, KeyEquality, PosFun(KeyPos), Max) of
                false ->
                    find_const_position_idx(IF(indices), KeyEquality,
                                            PosFun, Max, []);
                PosValuesSkip ->
                    PosValuesSkip
            end
    end;
find_const_positions(_IF, _LU_fun, _KE_PosFun, _Opt0) ->
    {false, {some,[]}}.

find_const_position_idx([I | Is], KeyEquality, PosFun, Max, L0) ->
    case pos_vals(I, KeyEquality, PosFun(I), Max) of
        false ->
            find_const_position_idx(Is, KeyEquality, PosFun, Max, L0);
        {{_Pos, Values}, _SkipFils}=PosValuesFils ->
            L = [{length(Values), PosValuesFils} | L0],
            find_const_position_idx(Is, KeyEquality, PosFun, Max, L)
    end;
find_const_position_idx(_, _KeyEquality, _PosFun, _Max, []) ->
    {false, {some,[]}};
find_const_position_idx(_, _KeyEquality, _PosFun, _Max, L) ->
    [{_,PVF} | _] = lists:sort(L),
    PVF.

pos_vals(Pos, '==', {usort_needed, Values, SkipFils}, Max) ->
    pos_vals_max(Pos, lists:usort(Values), SkipFils, Max);
pos_vals(Pos, '=:=', {usort_needed, Values, SkipFils}, Max) ->
    pos_vals_max(Pos, lists:sort(nub(Values)), SkipFils, Max);
pos_vals(Pos, _KeyEquality, {values, Values, SkipFils}, Max) ->
    pos_vals_max(Pos, Values, SkipFils, Max);
pos_vals(_Pos, _KeyEquality, _T, _Max) ->
    false.

nub([]) ->
    [];
nub([E | L]) ->
    case lists:member(E, Es=nub(L)) of
        true ->
            Es;
        false ->
            [E | Es]
    end.

%% length(Values) >= 1
pos_vals_max(Pos, Values, Skip, Max) when Max =:= -1; Max >= length(Values) ->
    {{Pos, Values}, Skip};
pos_vals_max(_Pos, _Value, _Skip, _Max) ->
    false.

prep_join(Prep, QOpt, Opt) ->
    case join_opt(QOpt) of
        undefined ->
            check_join_option(Opt),
            Prep;
        EqualMatch ->
            {Ix, M} = case EqualMatch of
                          {NEqual, NMatch} ->
                              pref_join(NEqual, NMatch, Prep, QOpt, Opt);
                          EM ->
                              pref_join(EM, EM, Prep, QOpt, Opt)
                      end,
            SI = family_union(Prep#prepared.sort_info ++ M),
            Prep#prepared{join = {Ix, M}, sort_info = SI}
    end.

%% The parse transform ensures that only two tables are involved.
pref_join(Equal, Match, Prep, QOpt, #qlc_opt{join = JoinOpt}) ->
    JQs = [{KeyEquality, QCs} ||
              {KeyEquality, QCsL} <- [{'==',Equal}, {'=:=',Match}],
              QCs <- QCsL],
    IxL = [pref_lookup_join(KE, QCs, Prep, QOpt) || 
              JoinOpt =:= any orelse JoinOpt =:= lookup,
              {KE, QCs} <- JQs],
    ML = [pref_merge_join(KE, QCs, Prep, QOpt) ||
             JoinOpt =:= any orelse JoinOpt =:= merge,
             {KE, QCs} <- JQs],
    {lists:usort(lists:append(IxL)), lists:usort(lists:append(ML))}.

pref_lookup_join(KeyEquality, {[{Q1,C1},{Q2,C2}],Skip}, Prep, QOpt) 
                                       when is_integer(C1), is_integer(C2) ->
    #prepared{qh = #qlc{qdata = QData}} = Prep,
    Is1 = lookup_qual_data(QData, Q1, KeyEquality),
    Lu2 = [pref_lookup_join2(Q2, C2, Q1, C1, Skip, QOpt, KeyEquality) ||
              IC1 <- Is1, IC1 =:= C1],
    Is2 = lookup_qual_data(QData, Q2, KeyEquality),
    Lu1 = [pref_lookup_join2(Q1, C1, Q2, C2, Skip, QOpt, KeyEquality) ||
              IC2 <- Is2, IC2 =:= C2],
    family(Lu1 ++ Lu2);
pref_lookup_join(KE, [{_,Cs1},{_,Cs2}]=L, Prep, QOpt) when is_list(Cs1), 
                                                           is_list(Cs2) ->
    %% --R12B-5
    lists:append([pref_lookup_join(KE, QC,Prep,QOpt) || 
                     QC <- selections_no_skip(L)]).

lookup_qual_data(QData, QNum, KeyEquality) ->
    case lists:keysearch(QNum, 1, QData) of
        {value, ?qual_data(QNum, _, _, {gen, PrepLE})} ->
            join_indices(PrepLE, KeyEquality)
    end.

%% If the table has a match specification (ms =/= no_match_spec) that
%% has _not_ been derived from a filter but from a query handle then
%% the lookup join cannot be done. This particular case has not been
%% excluded here but is taken care of in opt_join().
join_indices(#prepared{qh = #qlc_table{info_fun = IF, 
                                       lookup_fun = LU_fun,
                                       key_equality = KeyEquality,
                                       lu_vals = undefined}},
             KE) when is_function(LU_fun),
                      KE =:= KeyEquality orelse
                      KE =:= '=:=' andalso 
                            KeyEquality =:= undefined -> % --R12B-5
    KpL = case call(IF, keypos, undefined, []) of
              undefined -> [];
              Kp -> [Kp]
          end,
    case call(IF, indices, undefined, []) of
        undefined -> KpL;
        Is0 -> lists:usort(KpL ++ Is0)
    end;
join_indices(_Prep, _KeyEquality) ->
    [].

pref_lookup_join2(Q1, C1, Q2, C2, Skip, QOpt, KeyEquality) ->
    TemplCols = compared_template_columns(QOpt, {Q1,C1}, KeyEquality),
    {{Q1,C1,Q2,C2},{lookup_join,TemplCols,KeyEquality,Skip}}.

pref_merge_join(KE, {[{Q1,C1},{Q2,C2}],Skip}, Prep, QOpt) 
                                       when is_integer(C1), is_integer(C2) ->
    #prepared{qh = #qlc{qdata = QData}} = Prep,
    Sort1 = merge_qual_data(QData, Q1),
    Sort2 = merge_qual_data(QData, Q2),
    Merge = pref_merge(KE, Q1, C1, Q2, C2, Skip, Sort1, Sort2, QOpt),
    family_union(Merge);
pref_merge_join(KE, [{_,Cs1},{_,Cs2}]=L, Prep, QOpt) when is_list(Cs1), 
                                                      is_list(Cs2) ->
    %% --R12B-5
    lists:append([pref_merge_join(KE, QC, Prep, QOpt) || 
                     QC <- selections_no_skip(L)]).

selections_no_skip(L) ->
    [{C,{some,[]}} || C <- all_selections(L)].

merge_qual_data(QData, QNum) ->
    case lists:keysearch(QNum, 1, QData) of
        {value, ?qual_data(QNum, _, _, {gen, PrepLE})} ->
            #prepared{sort_info2 = SortInfo} = PrepLE,
            SortInfo
    end.

pref_merge(KE, Q1, C1, Q2, C2, Skip, Sort1, Sort2, QOpt) ->
    Col1 = {Q1,C1},
    Col2 = {Q2,C2},
    DoSort = [QC || {{_QNum,Col}=QC,SortL} <- [{Col1,Sort1}, {Col2,Sort2}],
                    lists:keymember({Col, ascending}, 1, SortL) =:= false],
    J = [{{Q1,C1,Q2,C2}, {merge_join,DoSort,KE,Skip}}],
    %% true = (QOpt(template))(Col1, '==') =:= (QOpt(template))(Col2, '==')
    [{{Column, ascending}, J} || 
        Column <- equal_template_columns(QOpt, Col1)] ++ [{other, J}].

table_sort_info(#qlc_table{info_fun = IF}) ->
    case call(IF, is_sorted_key, undefined, []) of
        undefined -> 
            {[], no};
        false -> 
            {[], no};
        true -> 
            case call(IF, keypos, undefined, []) of
                undefined -> % strange
                    {[], no};
                KeyPos ->
                    {[{{KeyPos,ascending},[]}], no}
            end
    end.

sort_sort_info(#qlc_sort{keypos = sort, order = Ord0}) ->
    {[], sort_order(Ord0)};
sort_sort_info(#qlc_sort{keypos = {keysort,Kp0}, order = Ord0}) ->
    Kp = case Kp0 of
             [Pos | _] -> Pos;
             _ -> Kp0
         end,
    {[{{Kp,sort_order(Ord0)},[]}], no}.

sort_order(F) when is_function(F) ->
    no;
sort_order(Order) ->
    Order.

check_join_option(#qlc_opt{join = any}) ->
    ok;
check_join_option(#qlc_opt{join = Join}) ->
    erlang:error(no_join_to_carry_out, [{join,Join}]).

check_lookup_option(#qlc_opt{lookup = true}, false) ->
    erlang:error(no_lookup_to_carry_out, [{lookup,true}]);
check_lookup_option(_QOpt, _LuV) ->
    ok.

compared_template_columns(QOpt, QNumColumn, KeyEquality) ->
    (QOpt(template))(QNumColumn, KeyEquality).

equal_template_columns(QOpt, QNumColumn) ->
    (QOpt(template))(QNumColumn, '==').

%eq_template_columns(QOpt, QNumColumn) ->
%    (QOpt(template))(QNumColumn, '=:=').

size_of_constant_prefix(QOpt, QNum) ->
    (QOpt(n_leading_constant_columns))(QNum).

constants(QOpt, QNum) ->
    (QOpt(constants))(QNum).

equal_constants(QOpt, QNum) ->
    (QOpt(equal_constants))(QNum).

join_opt(QOpt) ->
    QOpt(join).

match_specs(QOpt, QNum) ->
    (QOpt(match_specs))(QNum).

constant_columns(QOpt, QNum) ->
    (QOpt(constant_columns))(QNum).

size_of_qualifier(QOpt, QNum) ->
    (QOpt(size))(QNum).

%% Two optimizations are carried out:
%% 1. The first generator is never cached if the QLC itself is cached.
%% Since the answers do not need to be cached, the top-most QLC is
%% never cached either. Simple QLCs not holding any options are
%% removed. Simple QLCs are coalesced when possible.
%% 2. Merge join and lookup join is done if possible.

opt_le(#prepared{qh = #simple_qlc{le = LE0, optz = Optz0}=QLC}=Prep0, 
       GenNum) ->
    case LE0 of
        #prepared{qh = #simple_qlc{p = LE_Pvar, le = LE2, optz = Optz2}} ->
            %% Coalesce two simple QLCs.
            Cachez = case Optz2#optz.cache of
                         false -> Optz0#optz.cache;
                         Cache2 -> Cache2
                     end,
            Optz = Optz0#optz{cache = Cachez,
                              unique = Optz0#optz.unique or Optz2#optz.unique},
            PVar = if 
                       LE_Pvar =:= ?SIMPLE_QVAR -> QLC#simple_qlc.p;
                       true -> LE_Pvar
                   end,
            Prep = Prep0#prepared{qh = QLC#simple_qlc{p = PVar, le = LE2,
                                                      optz = Optz}},
            opt_le(Prep, GenNum);
        _ ->
            Optz1 = no_cache_of_first_generator(Optz0, GenNum),
            case {opt_le(LE0, 1), Optz1} of
                {LE, #optz{unique = false, cache = false}} ->
                    LE;
                {LE, _} ->
                    Prep0#prepared{qh = QLC#simple_qlc{le = LE, optz = Optz1}}
            end
    end;
opt_le(#prepared{qh = #qlc{}, lu_skip_quals = LU_SkipQuals0}=Prep0, GenNum) ->
    #prepared{qh = #qlc{qdata = Qdata0, optz = Optz0}=QLC} = Prep0,
    #optz{join_option = JoinOption, opt = Opt} = Optz0,
    JoinOption = Optz0#optz.join_option,
    {LU_QNum, Join, JoinSkipFs, DoSort} = 
        opt_join(Prep0#prepared.join, JoinOption, Qdata0, Opt, LU_SkipQuals0),
    {LU_Skip, LU_SkipQuals} = 
        lists:partition(fun({QNum,_Fs}) -> QNum =:= LU_QNum end,
                        LU_SkipQuals0),
    LU_SkipFs = lists:flatmap(fun({_QNum,Fs}) -> Fs end, LU_SkipQuals),
    %% If LU_QNum has a match spec it must be applied _after_ the
    %% lookup join (the filter must not be skipped!). 
    Qdata1 = if 
                 LU_Skip =:= [] -> Qdata0;
                 true -> activate_join_lookup_filter(LU_QNum, Qdata0)
             end,
    Qdata2 = skip_lookup_filters(Qdata1, LU_SkipFs ++ JoinSkipFs),
    F = fun(?qual_data(QNum, GoI, SI, {gen, #prepared{}=PrepLE}), GenNum1) ->
                NewPrepLE = maybe_sort(PrepLE, QNum, DoSort, Opt),
                {?qual_data(QNum, GoI, SI, {gen, opt_le(NewPrepLE, GenNum1)}),
                 GenNum1 + 1};
           (Qd, GenNum1) ->
                {Qd, GenNum1}
        end,
    {Qdata, _} = lists:mapfoldl(F, 1, Qdata2),
    Optz1 = no_cache_of_first_generator(Optz0, GenNum),
    Optz = Optz1#optz{fast_join = Join},
    Prep0#prepared{qh = QLC#qlc{qdata = Qdata, optz = Optz}};
opt_le(#prepared{qh = #qlc_append{hl = HL}}=Prep, GenNum) ->
    Hs = [opt_le(H, GenNum) || H <- HL],
    Prep#prepared{qh = #qlc_append{hl = Hs}};
opt_le(#prepared{qh = #qlc_sort{h = H}=Sort}=Prep, GenNum) ->
    Prep#prepared{qh = Sort#qlc_sort{h = opt_le(H, GenNum)}};
opt_le(Prep, _GenNum) ->
    Prep.

no_cache_of_first_generator(Optz, GenNum) when GenNum > 1 ->
    Optz;
no_cache_of_first_generator(Optz, 1) ->
    Optz#optz{cache = false}.

maybe_sort(LE, QNum, DoSort, Opt) ->
    case lists:keyfind(QNum, 1, DoSort) of
        {QNum, Col} ->
            #qlc_opt{tmpdir = TmpDir, tmpdir_usage = TmpUsage} = Opt,
            SortOpts = [{tmpdir,Dir} || Dir <- [TmpDir], Dir =/= ""],
            Sort = #qlc_sort{h = LE, keypos = {keysort, Col}, unique = false,
                             compressed = [], order = ascending,
                             fs_opts = SortOpts, tmpdir_usage = TmpUsage,
                             tmpdir = TmpDir},
            #prepared{qh = Sort, sorted = no, join = no};
        false ->
            LE
    end.

skip_lookup_filters(Qdata, []) ->
    Qdata;
skip_lookup_filters(Qdata0, LU_SkipFs) ->
    [case lists:member(QNum, LU_SkipFs) of
         true ->
             ?qual_data(QNum, GoI, ?SKIP, fil);
         false ->
             Qd
     end || ?qual_data(QNum, GoI, _, _)=Qd <- Qdata0].

%% If the qualifier used for lookup by the join (QNum) has a match
%% specification it must be applied _after_ the lookup join (the
%% filter must not be skipped!).
activate_join_lookup_filter(QNum, Qdata) ->
    {_,GoI2,SI2,{gen,Prep2}} = lists:keyfind(QNum, 1, Qdata),
    Table2 = Prep2#prepared.qh,
    NPrep2 = Prep2#prepared{qh = Table2#qlc_table{ms = no_match_spec}},
    %% Table2#qlc_table.ms has been reset; the filter will be run.
    lists:keyreplace(QNum, 1, Qdata, ?qual_data(QNum,GoI2,SI2,{gen,NPrep2})).

opt_join(Join, JoinOption, Qdata, Opt, LU_SkipQuals) ->
    %% prep_qlc_lc() assures that no unwanted join is carried out
    {Ix0, M0} = Join,
    Ix1 = opt_join_lu(Ix0, Qdata, LU_SkipQuals),
    Ix = lists:reverse(lists:keysort(2, Ix1)), % prefer to skip
    case Ix of 
        [{{Q1,C1,Q2,C2},Skip,KE,LU_fun} | _] ->
            J = #qlc_join{kind = {lookup, KE, LU_fun}, q1 = Q1,
                          c1 = C1, q2 = Q2, c2 = C2, opt = Opt},
            {Q2, J, Skip, []};
        [] ->
            M = opt_join_merge(M0),
            case M of
                [{{Q1,C1,Q2,C2},{merge_join,DoSort,KE,Skip}}|_] ->
                    J = #qlc_join{kind = {merge, KE}, opt = Opt,
                                  q1 = Q1, c1 = C1, q2 = Q2, c2 = C2},
                    {not_a_qnum, J, Skip, DoSort};
                [] when JoinOption =:= nested_loop ->
                    {not_a_qnum, no, [], []};
                _ when JoinOption =/= any ->
                    erlang:error(cannot_carry_out_join, [JoinOption]);
                _ ->
                    {not_a_qnum, no, [], []}
            end
    end.

opt_join_lu([{{_Q1,_C1,Q2,_C2}=J,[{lookup_join,_KEols,JKE,Skip0} | _]} | LJ], 
            Qdata, LU_SkipQuals) ->
    {Q2,_,_,{gen,Prep2}} = lists:keyfind(Q2, 1, Qdata),
    #qlc_table{ms = MS, key_equality = KE, 
               lookup_fun = LU_fun} = Prep2#prepared.qh,
    %% If there is no filter to skip (the match spec was derived 
    %% from a query handle) then the lookup join cannot be done.
    case 
        MS =/= no_match_spec andalso 
        lists:keymember(Q2, 1, LU_SkipQuals) =:= false
    of 
        true ->
            opt_join_lu(LJ, Qdata, LU_SkipQuals);
        false ->
            %% The join is preferred before evaluating the match spec
            %% (if there is one).
            Skip = skip_if_possible(JKE, KE, Skip0),
            [{J,Skip,KE,LU_fun} | opt_join_lu(LJ, Qdata, LU_SkipQuals)]
    end;
opt_join_lu([], _Qdata, _LU_SkipQuals) ->
    [].

opt_join_merge(M) ->
    %% Prefer not to sort arguments. Prefer to skip join filter.
    L = [{-length(DoSort),length(Skip),
         {QCs,{merge_join,DoSort,KE,Skip}}} ||
            {_KpOrder_or_other,MJ} <- M,
            {QCs,{merge_join,DoSort,KE,Skip0}} <- MJ,
            Skip <- [skip_if_possible(KE, '==', Skip0)]],
    lists:reverse([J || {_,_,J} <- lists:sort(L)]).

%% Cannot skip the join filter the join operator is '=:=' and the join
%% is performed using '=='. Note: the tag 'some'/'all' is not used.
skip_if_possible('=:=', '==', _) ->
    [];
skip_if_possible(_, _, {_SkipTag, Skip}) ->
    Skip.

%% -> {Objects, Post, LocalPost} | throw()
%% Post is a list of funs (closures) to run afterwards.
%% LocalPost should be run when all objects have been found (optimization).
%% LocalPost will always be a subset of Post.
%% List expressions are evaluated, resulting in lists of objects kept in
%% RAM or on disk.
%% An error term is thrown as soon as cleanup according Post has been
%% done. (This is opposed to errors during evaluation; such errors are
%% returned as terms.)
setup_qlc(Prep, Setup) ->
    Post0 = [],
    setup_le(Prep, Post0, Setup).

setup_le(#prepared{qh = #simple_qlc{le = LE, optz = Optz}}, Post0, Setup) ->
    {Objs, Post, LocalPost} = setup_le(LE, Post0, Setup),
    unique_cache(Objs, Post, LocalPost, Optz);
setup_le(#prepared{qh = #qlc{lcf = QFun, qdata = Qdata, init_value = V, 
                             optz = Optz}}, Post0, Setup) ->
    {GoTo, FirstState, Post, LocalPost} = 
        setup_quals(Qdata, Post0, Setup, Optz),
    Objs = fun() -> QFun(FirstState, V, GoTo) end,
    unique_cache(Objs, Post, LocalPost, Optz);
setup_le(#prepared{qh = #qlc_table{post_fun = PostFun}=Table}, Post, Setup) ->
    H = table_handle(Table, Post, Setup),
    %% The pre fun has been called from table_handle():
    {H, [PostFun | Post], []};
setup_le(#prepared{qh = #qlc_append{hl = PrepL}}, Post0, Setup) ->
    F = fun(Prep, {Post1, LPost1}) -> 
                {Objs, Post2, LPost2} = setup_le(Prep, Post1, Setup),
                {Objs, {Post2, LPost1++LPost2}}
        end,
    {ObjsL, {Post, LocalPost}} = lists:mapfoldl(F, {Post0,[]}, PrepL),
    {fun() -> append_loop(ObjsL, 0) end, Post, LocalPost};
setup_le(#prepared{qh = #qlc_sort{h = Prep, keypos = Kp, 
                                  unique = Unique, compressed = Compressed,
                                  order = Order, fs_opts = SortOptions0, 
                                  tmpdir_usage = TmpUsage,tmpdir = TmpDir}},
         Post0, Setup) ->
    SortOptions = sort_options_global_tmp(SortOptions0, TmpDir),
    LF = fun(Objs) -> 
                 sort_list(Objs, Order, Unique, Kp, SortOptions, Post0)
         end,
    case setup_le(Prep, Post0, Setup) of
        {L, Post, LocalPost} when is_list(L) ->
            {LF(L), Post, LocalPost};
        {Objs, Post, LocalPost} ->
            FF = fun(Objs1) ->
                         file_sort_handle(Objs1, Kp, SortOptions, TmpDir, 
                                          Compressed, Post, LocalPost)
                 end,
            sort_handle(Objs, LF, FF, SortOptions, Post, LocalPost, 
                        {TmpUsage, sorting})
    end;
setup_le(#prepared{qh = #qlc_list{l = L, ms = MS}}, Post, _Setup) 
              when (no_match_spec =:= MS); L =:= [] ->
    {L, Post, []};
setup_le(#prepared{qh = #qlc_list{l = L, ms = MS}}, Post, _Setup) 
                                   when is_list(L) ->
    {ets:match_spec_run(L, ets:match_spec_compile(MS)), Post, []};
setup_le(#prepared{qh = #qlc_list{l = H0, ms = MS}}, Post0, Setup) ->
    {Objs0, Post, LocalPost} = setup_le(H0, Post0, Setup),
    Objs = ets:match_spec_run(Objs0, ets:match_spec_compile(MS)),
    {Objs, Post, LocalPost}.

%% The goto table (a tuple) is created at runtime. It is accessed by
%% the generated code in order to find next clause to execute. For
%% generators there is also a fun; calling the fun runs the list
%% expression of the generator. There are two elements for a filter:
%% the first one is the state to go when the filter is false; the
%% other the state when the filter is true. There are three elements
%% for a generator G: the first one is the state of the generator
%% before G (or the stop state if there is no generator); the second
%% one is the state of the qualifier following the generator (or the
%% template if there is no next generator); the third one is the list
%% expression fun. 
%% There are also join generators which are "activated" when it is 
%% possbible to do a join.

setup_quals(Qdata, Post0, Setup, Optz) ->
    {GoTo0, Post1, LocalPost0} = 
        setup_quals(0, Qdata, [], Post0, [], Setup),
    GoTo1 = lists:keysort(1, GoTo0),
    FirstState0 = next_state(Qdata),
    {GoTo2, FirstState, Post, LocalPost1} =
        case Optz#optz.fast_join of
            #qlc_join{kind = {merge,_KE}, c1 = C1, c2 = C2, opt = Opt} = MJ ->
                MF = fun(_Rev, {H1, WH1}, {H2, WH2}) -> 
                             fun() -> 
                                  merge_join(WH1(H1), C1, WH2(H2), C2, Opt) 
                             end
                     end,
                setup_join(MJ, Qdata, GoTo1, FirstState0, MF, Post1);
            #qlc_join{kind = {lookup,_KE,LuF}, c1 = C1, c2 = C2} = LJ ->
                LF = fun(Rev, {H1, WH1}, {H2, WH2}) ->
                             {H, W} = if 
                                          Rev -> {H2, WH2};
                                          true -> {H1, WH1} 
                                      end,
                             fun() -> 
                                     lookup_join(W(H), C1, LuF, C2, Rev)
                             end
                     end,
                setup_join(LJ, Qdata, GoTo1, FirstState0, LF, Post1);
            no ->
                {flat_goto(GoTo1), FirstState0, Post1, []}
        end,
    GoTo = list_to_tuple(GoTo2),
    {GoTo, FirstState, Post, LocalPost0 ++ LocalPost1}.

setup_quals(GenLoopS, [?qual_data(_QNum,GoI,?SKIP,fil) | Qdata], 
            Gs, P, LP, Setup) ->
    %% ?SKIP causes runtime error. See also skip_lookup_filters().
    setup_quals(GenLoopS, Qdata, [{GoI,[?SKIP,?SKIP]} | Gs], P, LP, Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,_SI,fil) | Qdata], 
            Gs, P, LP, Setup) ->
    setup_quals(GenLoopS, Qdata, [{GoI,[GenLoopS,next_state(Qdata)]} | Gs], 
                P, LP, Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,_SI, {gen,#join{}}) | Qdata],
            Gs, P, LP, Setup) ->
    setup_quals(GenLoopS, Qdata, [{GoI,[?SKIP,?SKIP,?SKIP]} | Gs],P,LP,Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,SI,{gen,LE}) | Qdata], 
            Gs, P, LP, Setup) ->
    {V, NP, LP1} = setup_le(LE, P, Setup),
    setup_quals(SI + 1, Qdata, [{GoI, [GenLoopS,next_state(Qdata),V]} | Gs], 
                NP, LP ++ LP1, Setup);
setup_quals(GenLoopS, [], Gs, P, LP, _Setup) ->
    {[{1,[GenLoopS]} | Gs], P, LP}.

%% Finds the qualifier in Qdata that performs the join between Q1 and
%% Q2, and sets it up using the handles already set up for Q1 and Q2.
%% Removes Q1 and Q2 from GoTo0 and updates the join qualifier in GoTo0.
%% Note: the parse transform has given each generator three slots
%% in the GoTo table. The position of these slots within the GoTo table
%% is fixed (at runtime).
%% (Assumes there is only one join-generator in Qdata.)
setup_join(J, Qdata, GoTo0, FirstState0, JoinFun, Post0) ->
    #qlc_join{q1 = QNum1a, q2 = QNum2a, opt = Opt} = J,
    {?qual_data(_QN,JGoI,JSI,_), Rev, QNum1, QNum2, WH1, WH2, _CsFun} = 
        find_join_data(Qdata, QNum1a, QNum2a),
    [{GoI1,SI1}] = [{GoI,SI} ||
                       ?qual_data(QNum,GoI,SI,_) <- Qdata, QNum =:= QNum1],
    [{GoI2,SI2}] = [{GoI,SI} ||
                       ?qual_data(QNum,GoI,SI,_) <- Qdata, QNum =:= QNum2],

    [H1] = [H || {GoI,[_Back,_Forth,H]} <- GoTo0, GoI =:= GoI1],
    [{BackH2,H2}] = 
           [{Back,H} || {GoI,[Back,_Forth,H]} <- GoTo0, GoI =:= GoI2],
    H0 = JoinFun(Rev, {H1,WH1}, {H2,WH2}),
    %% The qlc expression options apply to the introduced qlc expr as well.
    {H, Post, LocalPost} = 
         unique_cache(H0, Post0, [], join_unique_cache(Opt)),
    [JBack] = [Back || {GoI,[Back,_,_]} <- GoTo0, GoI =:= GoI1],
    JForth = next_after(Qdata, SI1, QNum2),
    GoTo1 = lists:map(fun({GoI,_}) when GoI =:= JGoI -> 
                              {JGoI, [JBack, JForth, H]};
                         ({GoI,_}) when GoI =:= GoI1; GoI =:= GoI2 ->
                              {GoI, [?SKIP,?SKIP,?SKIP]}; % not necessary
                         (Go) ->
                              Go
                      end, GoTo0),
    GoTo = lists:map(fun(S) when S =:= SI1 -> 
                             JSI;
                        (S) when S =:= SI2 -> 
                             next_after(Qdata, S, QNum2);
                        (S) when S =:= SI1+1 -> 
                             JSI+1;
                        (S) when S =:= SI2+1, SI1 + 1 =:= BackH2  -> 
                             JSI+1;
                        (S) when S =:= SI2+1 -> 
                             BackH2;
                        (S) -> S
                     end, flat_goto(GoTo1)),
    FirstState = if
                     SI1 =:= FirstState0 -> JSI;
                     true -> FirstState0
                 end,
    {GoTo, FirstState, Post, LocalPost}.

join_unique_cache(#qlc_opt{cache = Cache, unique = Unique}=Opt) ->
    #optz{cache = Cache, unique = Unique, opt = Opt}.

flat_goto(GoTo) ->
    lists:flatmap(fun({_,L}) -> L end, GoTo).

next_after([?qual_data(_, _, S, _) | Qdata], S, QNum2) ->
    case Qdata of
        [?qual_data(QNum2, _, _, _) | Qdata1] ->
            next_state(Qdata1);
        _ -> 
            next_state(Qdata)
    end;
next_after([_ | Qdata], S, QNum2) ->
    next_after(Qdata, S, QNum2).

next_state([?qual_data(_,_,_,{gen,#join{}}) | Qdata]) ->
    next_state(Qdata);
next_state([?qual_data(_,_,?SKIP,fil) | Qdata]) ->
    %% see skip_lookup_filters()
    next_state(Qdata);
next_state([?qual_data(_,_,S,_) | _]) ->
    S;
next_state([]) ->
    template_state().

find_join_data(Qdata, QNum1, QNum2) ->
    [QRev] = [{Q,Rev,QN1,QN2,H1,H2,CsF} || 
                 ?qual_data(_QN,_GoI,_SI,
                            {gen,#join{q1 = QN1,q2 = QN2, 
                                       wh1 = H1, wh2 = H2,
                                       cs_fun = CsF}})= Q <- Qdata,
                 if 
                     QN1 =:= QNum1, QN2 =:= QNum2 ->
                         not (Rev = false);
                     QN1 =:= QNum2, QN2 =:= QNum1 ->
                         Rev = true;
                     true -> 
                         Rev = false
                 end],
    QRev.

table_handle(#qlc_table{trav_fun = TraverseFun, trav_MS = TravMS, 
                        pre_fun = PreFun, lookup_fun = LuF, 
                        parent_fun = ParentFun, lu_vals = LuVals, ms = MS}, 
             Post, Setup) ->
    #setup{parent = Parent} = Setup,
    ParentValue = 
        if 
            ParentFun =:= undefined ->
                undefined;
            Parent =:= self() ->
                try
                    ParentFun() 
                catch Class:Reason:Stacktrace ->
                    post_funs(Post),
                    erlang:raise(Class, Reason, Stacktrace)
                end;
            true ->
                case monitor_request(Parent, {parent_fun, ParentFun}) of
                    error -> % parent has died
                        post_funs(Post),
                        exit(normal);
                    {value, Value} ->
                        Value;
                    {parent_fun_caught, Class, Reason, Stacktrace} ->
                        %% No use augmenting Stacktrace here.
                        post_funs(Post),
                        erlang:raise(Class, Reason, Stacktrace)
                end
        end,
    StopFun = 
        if 
            Parent =:= self() ->
                undefined;
            true ->
                Cursor = #qlc_cursor{c = {self(), Parent}},
                fun() -> delete_cursor(Cursor) end
        end,
    PreFunArgs = [{parent_value, ParentValue}, {stop_fun, StopFun}],
    _ = call(PreFun, PreFunArgs, ok, Post),
    case LuVals of
        {Pos, Vals} when MS =:= no_match_spec ->
            LuF(Pos, Vals);
        {Pos, Vals} ->
            case LuF(Pos, Vals) of
                [] -> 
                    [];
                Objs when is_list(Objs) -> 
                    ets:match_spec_run(Objs, 
                                       ets:match_spec_compile(MS));
                Error ->
                    post_funs(Post),
                    throw_error(Error)
            end;
        _ when not TravMS ->
            MS = no_match_spec, % assertion
            TraverseFun;
        _ when MS =:= no_match_spec ->
            fun() -> TraverseFun([{'$1',[],['$1']}]) end;
        _ ->
            fun() -> TraverseFun(MS) end
    end.

-define(CHUNK_SIZE, 64*1024).

open_file(FileName, Extra, Post) ->
    case file:open(FileName, [read, raw, binary | Extra]) of
        {ok, Fd} ->
            {fun() -> 
                 case file:position(Fd, bof) of
                     {ok, 0} -> 
                         TF = fun([], _) -> 
                                      [];
                                 (Ts, C) when is_list(Ts) -> 
                                      lists:reverse(Ts, C) 
                              end,
                         file_loop_read(<<>>, ?CHUNK_SIZE, {Fd,FileName}, TF);
                     Error -> 
                         file_error(FileName, Error)
                 end
             end, Fd};
        Error ->
            post_funs(Post),
            throw_file_error(FileName, Error)
    end.

file_loop(Bin0, Fd_FName, Ts0, TF) ->
    case 
        try file_loop2(Bin0, Ts0) 
        catch _:_ ->        
            {_Fd, FileName} = Fd_FName,
            error({bad_object, FileName})
        end
    of
        {terms, <<Size:4/unit:8, B/bytes>>=Bin, []} ->
            file_loop_read(Bin, Size - byte_size(B) + 4, Fd_FName, TF);
        {terms, <<Size:4/unit:8, _/bytes>>=Bin, Ts} ->
            C = fun() -> file_loop_read(Bin, Size+4, Fd_FName, TF) end,
            TF(Ts, C);
        {terms, B, Ts} ->
            C = fun() -> file_loop_read(B, ?CHUNK_SIZE, Fd_FName, TF) end,
            TF(Ts, C);
        Error ->
            Error
    end.

file_loop2(<<Size:4/unit:8, B:Size/bytes, Bin/bytes>>, Ts) ->
    file_loop2(Bin, [binary_to_term(B) | Ts]);
file_loop2(Bin, Ts) ->
    {terms, Bin, Ts}.

%% After power failures (and only then) files with corrupted Size
%% fields have been observed in a disk_log file. If file:read/2 is
%% asked to read a huge amount of data the emulator may crash. Nothing
%% has been done here to prevent such crashes (by inspecting
%% BytesToRead in some way) since temporary files will never be read
%% after a power failure.
file_loop_read(B, MinBytesToRead, {Fd, FileName}=Fd_FName, TF) ->
    BytesToRead = erlang:max(?CHUNK_SIZE, MinBytesToRead),
    case file:read(Fd, BytesToRead) of
        {ok, Bin} when byte_size(B) =:= 0 ->
            file_loop(Bin, Fd_FName, [], TF);
        {ok, Bin} ->
            case B of 
                <<Size:4/unit:8, Tl/bytes>> 
                                when byte_size(Bin) + byte_size(Tl) >= Size ->
                    {B1, B2} = split_binary(Bin, Size - byte_size(Tl)),
                    Foo = fun([T], Fun) -> [T | Fun] end,
                    %% TF should be applied exactly once.
                    case 
                        file_loop(list_to_binary([B, B1]), Fd_FName, [], Foo) 
                    of
                        [T | Fun] -> 
                            true = is_function(Fun),
                            file_loop(B2, Fd_FName, [T], TF);
                        Error ->
                            Error
                    end;
                _ ->
                    file_loop(list_to_binary([B, Bin]), Fd_FName, [], TF)
            end;
        eof when byte_size(B) =:= 0 ->
            TF([], foo);
        eof ->
            error({bad_object, FileName});
        Error ->
            file_error(FileName, Error)
    end.

sort_cursor_input(H, NoObjects) ->
    fun(close) ->
            ok;
       (read) ->
            sort_cursor_input_read(H, NoObjects)
    end.
                    
sort_cursor_list_output(TmpDir, Z, Unique) ->
    fun(close) ->
            {terms, []};
       ({value, NoObjects}) ->
            fun(BTerms) when Unique; length(BTerms) =:= NoObjects ->
                    fun(close) ->
                            {terms, BTerms};
                       (BTerms1) ->
                            sort_cursor_file(BTerms ++ BTerms1, TmpDir, Z)
                    end;
               (BTerms) ->
                    sort_cursor_file(BTerms, TmpDir, Z)
            end
    end.

sort_cursor_file(BTerms, TmpDir, Z) ->
    FName = tmp_filename(TmpDir),
    case file:open(FName, [write, raw, binary | Z]) of
        {ok, Fd} ->
            WFun = write_terms(FName, Fd),
            WFun(BTerms);
        Error ->
            throw_file_error(FName, Error)
    end.

sort_options_global_tmp(S, "") ->
    S;
sort_options_global_tmp(S, TmpDir) ->
    [{tmpdir,TmpDir} | lists:keydelete(tmpdir, 1, S)].

tmp_filename(TmpDirOpt) ->
    U = "_",
    Node = node(),
    Pid = os:getpid(),
    Unique = erlang:unique_integer(),
    F = lists:concat([?MODULE,U,Node,U,Pid,U,Unique]),
    TmpDir = case TmpDirOpt of
                 "" ->
                     {ok, CurDir} = file:get_cwd(),
                     CurDir;
                 TDir ->
                     TDir
             end,
    filename:join(filename:absname(TmpDir), F).

write_terms(FileName, Fd) ->
    fun(close) ->
            _ = file:close(Fd),
            {file, FileName};
       (BTerms) ->
            case file:write(Fd, size_bin(BTerms, [])) of
                ok ->
                    write_terms(FileName, Fd);
                Error ->
                    _ = file:close(Fd),
                    throw_file_error(FileName, Error)
            end
    end.

size_bin([], L) ->
    L;
size_bin([BinTerm | BinTerms], L) ->
    size_bin(BinTerms, [L, <<(byte_size(BinTerm)):4/unit:8>> | BinTerm]).

sort_cursor_input_read([], NoObjects) ->
    {end_of_input, NoObjects};
sort_cursor_input_read([Object | Cont], NoObjects) ->
    {[term_to_binary(Object)], sort_cursor_input(Cont, NoObjects + 1)};
sort_cursor_input_read(F, NoObjects) ->
    case F() of
        Objects when is_list(Objects) ->
            sort_cursor_input_read(Objects, NoObjects);
        Term ->
            throw_error(Term)
    end.

unique_cache(L, Post, LocalPost, Optz) when is_list(L) ->
    case Optz#optz.unique of
        true -> 
            {unique_sort_list(L), Post, LocalPost};
        false ->
            %% If Optz#optz.cache then an ETS table could be used. 
            {L, Post, LocalPost}
    end;
unique_cache(H, Post, LocalPost, #optz{unique = false, cache = false}) ->
    {H, Post, LocalPost};
unique_cache(H, Post, LocalPost, #optz{unique = true, cache = false}) ->
    E = ets:new(qlc, [set, private]),
    {fun() -> no_dups(H, E) end, [del_table(E) | Post], LocalPost};
unique_cache(H, Post, LocalPost, #optz{unique = false, cache = true}) ->
    E = ets:new(qlc, [set, private]),
    {L, P} = unique_cache_post(E),
    {fun() -> cache(H, E, LocalPost) end, [P | Post], [L]};
unique_cache(H, Post, LocalPost, #optz{unique = true, cache = true}) ->
    UT = ets:new(qlc, [bag, private]),
    MT = ets:new(qlc, [set, private]),
    {L1, P1} = unique_cache_post(UT),
    {L2, P2} = unique_cache_post(MT),
    {fun() -> ucache(H, UT, MT, LocalPost) end, [P1, P2 | Post], [L1, L2]};
unique_cache(H, Post, LocalPost, #optz{unique = false, cache = list}=Optz) ->
    Ref = make_ref(),
    F = del_lcache(Ref),
    #qlc_opt{tmpdir = TmpDir, max_list = MaxList, tmpdir_usage = TmpUsage} =
        Optz#optz.opt,
    {fun() -> lcache(H, Ref, LocalPost, TmpDir, MaxList, TmpUsage) end, 
     [F | Post], [F]};
unique_cache(H, Post0, LocalPost0, #optz{unique = true, cache = list}=Optz) ->
    #qlc_opt{tmpdir = TmpDir, max_list = MaxList, tmpdir_usage = TmpUsage} =
        Optz#optz.opt,
    Size = if
               MaxList >= 1 bsl 31 -> (1 bsl 31) - 1;
               MaxList =:= 0 -> 1;
               true ->  MaxList
           end,
    SortOptions = [{size, Size}, {tmpdir, TmpDir}],
    USortOptions = [{unique, true} | SortOptions],
    TmpUsageM = {TmpUsage, caching},
    LF1 = fun(Objs) -> lists:ukeysort(1, Objs) end,
    FF1 = fun(Objs) ->
                  file_sort_handle(Objs, {keysort, 1}, USortOptions, 
                                   TmpDir, [], Post0, LocalPost0)
          end,
    {UH, Post1, LocalPost1} = sort_handle(tag_objects(H, 1), LF1, FF1, 
                                          USortOptions, Post0, LocalPost0, 
                                          TmpUsageM),
    LF2 = fun(Objs) -> lists:keysort(2, Objs) end,
    FF2 = fun(Objs) ->
                  file_sort_handle(Objs, {keysort, 2}, SortOptions, TmpDir,
                                   [], Post1, LocalPost1)
          end,
    {SH, Post, LocalPost} = 
        sort_handle(UH, LF2, FF2, SortOptions, Post1, LocalPost1, TmpUsageM),
    if
        is_list(SH) ->
            %% Remove the tag once and for all.
            {untag_objects2(SH), Post, LocalPost};
        true ->
            %% Every traversal untags the objects...
            {fun() -> untag_objects(SH) end, Post, LocalPost}
    end.

unique_cache_post(E) ->
    {empty_table(E), del_table(E)}.

unique_sort_list(L) ->
    E = ets:new(qlc, [set, private]),
    unique_list(L, E).

unique_list([], E) ->
    true = ets:delete(E),
    [];
unique_list([Object | Objects], E) ->
    case ets:member(E, Object) of
        false ->
            true = ets:insert(E, {Object}),
            [Object | unique_list(Objects, E)];
        true ->
            unique_list(Objects, E)
    end.

sort_list(L, CFun, true, sort, _SortOptions, _Post) when is_function(CFun) ->
    lists:usort(CFun, L);
sort_list(L, CFun, false, sort, _SortOptions, _Post) when is_function(CFun) ->
    lists:sort(CFun, L);
sort_list(L, ascending, true, sort, _SortOptions, _Post) ->
    lists:usort(L);
sort_list(L, descending, true, sort, _SortOptions, _Post) ->
    lists:reverse(lists:usort(L));
sort_list(L, ascending, false, sort, _SortOptions, _Post) ->
    lists:sort(L);
sort_list(L, descending, false, sort, _SortOptions, _Post) ->
    lists:reverse(lists:sort(L));
sort_list(L, Order, Unique, {keysort, Kp}, _SortOptions, _Post) 
           when is_integer(Kp), is_atom(Order) ->
    case {Order, Unique} of
        {ascending, true} ->
            lists:ukeysort(Kp, L);
        {ascending, false} ->
            lists:keysort(Kp, L);
        {descending, true} ->
            lists:reverse(lists:ukeysort(Kp, L));
        {descending, false} ->
            lists:reverse(lists:keysort(Kp, L))
    end;
sort_list(L, _Order, _Unique, Sort, SortOptions, Post) ->
    In = fun(_) -> {L, fun(_) -> end_of_input end} end,
    Out = sort_list_output([]),
    TSortOptions = [{format,term} | SortOptions],
    do_sort(In, Out, Sort, TSortOptions, Post).

sort_list_output(L) ->
    fun(close) ->
            lists:append(lists:reverse(L));
       (Terms) when is_list(Terms) ->
            sort_list_output([Terms | L])
    end.

%% Don't use the file_sorter unless it is known that objects will be
%% put on a temporary file (optimization).
sort_handle(H, ListFun, FileFun, SortOptions, Post, LocalPost, TmpUsageM) ->
    Size = case lists:keyfind(size, 1, SortOptions) of
               {size, Size0} -> Size0;
               false -> default_option(size)
           end,
    sort_cache(H, [], Size, {ListFun, FileFun, Post, LocalPost, TmpUsageM}).

sort_cache([], CL, _Sz, {LF, _FF, Post, LocalPost, _TmpUsageM}) ->
    {LF(lists:reverse(CL)), Post, LocalPost};
sort_cache(Objs, CL, Sz, C) when Sz < 0 ->
    sort_cache2(Objs, CL, false, C);
sort_cache([Object | Cont], CL, Sz0, C) ->
    Sz = decr_list_size(Sz0, Object),
    sort_cache(Cont, [Object | CL], Sz, C);
sort_cache(F, CL, Sz, C) ->
    case F() of
        Objects when is_list(Objects) ->
            sort_cache(Objects, CL, Sz, C);
        Term -> 
            {_LF, _FF, Post, _LocalPost, _TmpUsageM} = C,
            post_funs(Post),
            throw_error(Term)
    end.

sort_cache2([], CL, _X, {LF, _FF, Post, LocalPost, _TmpUsageM}) ->
    {LF(lists:reverse(CL)), Post, LocalPost};
sort_cache2([Object | Cont], CL, _, C) ->
    sort_cache2(Cont, [Object | CL], true, C);
sort_cache2(F, CL, false, C) ->
    %% Find one extra object to be sure that temporary file(s) will be
    %% used when calling the file_sorter. This works even if
    %% duplicates are removed.
    case F() of
        Objects when is_list(Objects) ->
            sort_cache2(Objects, CL, true, C);
        Term -> 
            {_LF, _FF, Post, _LocalPost, _TmpUsageM} = C,
            post_funs(Post),
            throw_error(Term)
    end;
sort_cache2(_Cont, _CL, true, {_LF,_FF,Post,_LocalPost, {not_allowed,M}}) ->
    post_funs(Post),
    throw_reason({tmpdir_usage, M});
sort_cache2(Cont, CL, true, {_LF, FF, _Post, _LocalPost, {TmpUsage, M}}) ->
    maybe_error_logger(TmpUsage, M),
    FF(lists:reverse(CL, Cont)).

file_sort_handle(H, Kp, SortOptions, TmpDir, Compressed, Post, LocalPost) ->
    In = sort_cursor_input(H, 0),
    Unique = lists:member(unique, SortOptions) 
             orelse
             lists:keymember(unique, 1, SortOptions),
    Out = sort_cursor_list_output(TmpDir, Compressed, Unique),
    Reply = do_sort(In, Out, Kp, SortOptions, Post),
    case Reply of
        {file, FileName} ->
            {F, Fd} = open_file(FileName, Compressed, Post),
            P = fun() -> _ = file:close(Fd),
                         _ = file:delete(FileName)
                end,
            {F, [P | Post], LocalPost};
        {terms, BTerms} ->
            try 
                {[binary_to_term(B) || B <- BTerms], Post, LocalPost}
            catch Class:Reason:Stacktrace ->
                post_funs(Post),
                erlang:raise(Class, Reason, Stacktrace)
            end
    end.

do_sort(In, Out, Sort, SortOptions, Post) ->
    try
        case do_sort(In, Out, Sort, SortOptions) of
            {error, Reason} -> throw_reason(Reason);
            Reply -> Reply
        end
    catch Class:Term:Stacktrace ->
        post_funs(Post),
        erlang:raise(Class, Term, Stacktrace)
    end.

do_sort(In, Out, sort, SortOptions) ->
    file_sorter:sort(In, Out, SortOptions);
do_sort(In, Out, {keysort, KeyPos}, SortOptions) ->
    file_sorter:keysort(KeyPos, In, Out, SortOptions).

del_table(Ets) ->
    fun() -> true = ets:delete(Ets) end.

empty_table(Ets) ->
    fun() -> true = ets:delete_all_objects(Ets) end.

append_loop([[_ | _]=L], _N) ->
    L;
append_loop([F], _N) ->
    F();
append_loop([L | Hs], N) ->
    append_loop(L, N, Hs).

append_loop([], N, Hs) ->
    append_loop(Hs, N);
append_loop([Object | Cont], N, Hs) ->
    [Object | append_loop(Cont, N + 1, Hs)];
append_loop(F, 0, Hs) ->
    case F() of
        [] ->
            append_loop(Hs, 0);
        [Object | Cont] ->
            [Object | append_loop(Cont, 1, Hs)];
        Term ->
            Term
    end;
append_loop(F, _N, Hs) -> % when _N > 0
    fun() -> append_loop(F, 0, Hs) end.

no_dups([]=Cont, UTab) ->
    true = ets:delete_all_objects(UTab),
    Cont;
no_dups([Object | Cont], UTab) ->
    case ets:member(UTab, Object)  of
        false ->
            true = ets:insert(UTab, {Object}),
            %% A fun is created here, even if Cont is a list; objects
            %% will not be copied to the ETS table unless requested.
            [Object | fun() -> no_dups(Cont, UTab) end];
        true ->
            no_dups(Cont, UTab)
    end;
no_dups(F, UTab) ->
    case F() of
        Objects when is_list(Objects) ->
            no_dups(Objects, UTab);
        Term ->
            Term
    end.

%% When all objects have been returned from a cached QLC, the
%% generators of the expression will never be called again, and so the
%% tables used by the generators (LocalPost) can be emptied.

cache(H, MTab, LocalPost) ->
    case ets:member(MTab, 0) of
        false ->
            true = ets:insert(MTab, {0}),
            cache(H, MTab, 1, LocalPost);
        true ->
            cache_recall(MTab, 1)
    end.

cache([]=Cont, _MTab, _SeqNo, LocalPost) ->
    local_post(LocalPost),
    Cont;
cache([Object | Cont], MTab, SeqNo, LocalPost) ->
    true = ets:insert(MTab, {SeqNo, Object}),
    %% A fun is created here, even if Cont is a list; objects
    %% will not be copied to the ETS table unless requested.
    [Object | fun() -> cache(Cont, MTab, SeqNo + 1, LocalPost) end];
cache(F, MTab, SeqNo, LocalPost) ->
    case F() of
        Objects when is_list(Objects) ->
            cache(Objects, MTab, SeqNo, LocalPost);
        Term -> 
            Term
    end.

cache_recall(MTab, SeqNo) ->
    case ets:lookup(MTab, SeqNo) of
        []=Cont ->
            Cont;
        [{SeqNo, Object}] ->
            [Object | fun() -> cache_recall(MTab, SeqNo + 1) end]
    end.

ucache(H, UTab, MTab, LocalPost) ->
    case ets:member(MTab, 0) of
        false ->
            true = ets:insert(MTab, {0}),
            ucache(H, UTab, MTab, 1, LocalPost);
        true ->
            ucache_recall(UTab, MTab, 1)
    end.

ucache([]=Cont, _UTab, _MTab, _SeqNo, LocalPost) ->
    local_post(LocalPost),
    Cont;
ucache([Object | Cont], UTab, MTab, SeqNo, LocalPost) ->
    %% Always using 28 bits hash value...
    Hash = erlang:phash2(Object),
    case ets:lookup(UTab, Hash) of
        [] ->
            ucache3(Object, Cont, Hash, UTab, MTab, SeqNo, LocalPost);
        HashSeqObjects ->
            case lists:keymember(Object, 3, HashSeqObjects) of
                true -> 
                    ucache(Cont, UTab, MTab, SeqNo, LocalPost);
                false -> 
                    ucache3(Object, Cont, Hash, UTab, MTab, SeqNo, LocalPost)
            end
    end;
ucache(F, UTab, MTab, SeqNo, LocalPost) ->
    case F() of
        Objects when is_list(Objects) ->
            ucache(Objects, UTab, MTab, SeqNo, LocalPost);
        Term ->
            Term
    end.

ucache3(Object, Cont, Hash, UTab, MTab, SeqNo, LocalPost) ->
    true = ets:insert(UTab, {Hash, SeqNo, Object}),
    true = ets:insert(MTab, {SeqNo, Hash}),
    %% A fun is created here, even if Cont is a list; objects
    %% will not be copied to the ETS table unless requested.
    [Object | fun() -> ucache(Cont, UTab, MTab, SeqNo+1, LocalPost) end].

ucache_recall(UTab, MTab, SeqNo) ->
    case ets:lookup(MTab, SeqNo) of
        []=Cont ->
            Cont;
        [{SeqNo, Hash}] ->
            Object = case ets:lookup(UTab, Hash) of
                         [{Hash, SeqNo, Object0}] -> Object0;
                         HashSeqObjects -> 
                             {Hash, SeqNo, Object0} =
                                 lists:keyfind(SeqNo, 2, HashSeqObjects),
                             Object0
                     end,
            [Object | fun() -> ucache_recall(UTab, MTab, SeqNo + 1) end]
    end.

-define(LCACHE_FILE(Ref), {Ref, '$_qlc_cache_tmpfiles_'}).

lcache(H, Ref, LocalPost, TmpDir, MaxList, TmpUsage) ->
    Key = ?LCACHE_FILE(Ref),
    case get(Key) of
        undefined ->
            lcache1(H, {Key, LocalPost, TmpDir, MaxList, TmpUsage}, 
                    MaxList, []);
        {file, _Fd, _TmpFile, F} ->
            F();
        L when is_list(L) ->
            L
    end.
    
lcache1([]=Cont, {Key, LocalPost, _TmpDir, _MaxList, _TmpUsage}, _Sz, Acc) ->
    local_post(LocalPost),
    case get(Key) of
        undefined -> 
            put(Key, lists:reverse(Acc)),
            Cont;
        {file, Fd, TmpFile, _F} ->
            case lcache_write(Fd, TmpFile, Acc) of
                ok ->
                    Cont;
                Error ->
                    Error
            end
    end;
lcache1(H, State, Sz, Acc) when Sz < 0 ->
    {Key, LocalPost, TmpDir, MaxList, TmpUsage} = State,
    GetFile = 
        case get(Key) of
            {file, Fd0, TmpFile, _F} ->
                {TmpFile, Fd0};
            undefined when TmpUsage =:= not_allowed ->
                error({tmpdir_usage, caching});
            undefined ->
                maybe_error_logger(TmpUsage, caching),
                FName = tmp_filename(TmpDir),
                {F, Fd0} = open_file(FName, [write], LocalPost),
                put(Key, {file, Fd0, FName, F}),
                {FName, Fd0}
        end,
    case GetFile of
        {FileName, Fd} ->
            case lcache_write(Fd, FileName, Acc) of
                ok -> 
                    lcache1(H, State, MaxList, []);
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
lcache1([Object | Cont], State, Sz0, Acc) ->
    Sz = decr_list_size(Sz0, Object),
    [Object | lcache2(Cont, State, Sz, [Object | Acc])];
lcache1(F, State, Sz, Acc) ->
    case F() of
        Objects when is_list(Objects) ->
            lcache1(Objects, State, Sz, Acc);
        Term ->
            Term
    end.

lcache2([Object | Cont], State, Sz0, Acc) when Sz0 >= 0 ->
    Sz = decr_list_size(Sz0, Object),
    [Object | lcache2(Cont, State, Sz, [Object | Acc])];
lcache2(Cont, State, Sz, Acc) ->
    fun() -> lcache1(Cont, State, Sz, Acc) end.

lcache_write(Fd, FileName, L) ->
    write_binary_terms(t2b(L, []), Fd, FileName).

t2b([], Bs) ->
    Bs;
t2b([T | Ts], Bs) ->
    t2b(Ts, [term_to_binary(T) | Bs]).

del_lcache(Ref) ->
    fun() -> 
            Key = ?LCACHE_FILE(Ref),
            case get(Key) of
                undefined ->
                    ok;
                {file, Fd, TmpFile, _F} ->
                    _ = file:close(Fd),
                    _ = file:delete(TmpFile),
                    erase(Key);
                _L -> 
                    erase(Key)
            end
    end.

tag_objects([Object | Cont], T) ->
    [{Object, T} | tag_objects2(Cont, T + 1)];
tag_objects([]=Cont, _T) ->
    Cont;
tag_objects(F, T) ->
    case F() of
        Objects when is_list(Objects) ->
            tag_objects(Objects, T);
        Term ->
            Term
    end.

tag_objects2([Object | Cont], T) ->
    [{Object, T} | tag_objects2(Cont, T + 1)];
tag_objects2(Objects, T) ->
    fun() -> tag_objects(Objects, T) end.

untag_objects([]=Objs) ->
    Objs;
untag_objects([{Object, _N} | Cont]) ->
    [Object | untag_objects2(Cont)];
untag_objects(F) ->
    case F() of
        Objects when is_list(Objects) ->
            untag_objects(Objects);
        Term -> % Cannot happen
            Term
    end.

untag_objects2([{Object, _N} | Cont]) ->
    [Object | untag_objects2(Cont)];
untag_objects2([]=Cont) ->
    Cont;
untag_objects2(Objects) ->
    fun() -> untag_objects(Objects) end.

%%% Merge join.
%%% Temporary files are used when many objects have the same key.

-define(JWRAP(E1, E2), [E1 | E2]).

-record(m, {id, tmpdir, max_list, tmp_usage}).

merge_join([]=Cont, _C1, _T2, _C2, _Opt) ->
    Cont;
merge_join([E1 | L1], C1, L2, C2, Opt) ->
    #qlc_opt{tmpdir = TmpDir, max_list = MaxList, 
             tmpdir_usage = TmpUsage} = Opt,
    M = #m{id = merge_join_id(), tmpdir = TmpDir, max_list = MaxList, 
           tmp_usage = TmpUsage},
    merge_join2(E1, element(C1, E1), L1, C1, L2, C2, M);
merge_join(F1, C1, L2, C2, Opt) ->
    case F1() of
        L1 when is_list(L1) ->
            merge_join(L1, C1, L2, C2, Opt);
        T1 ->
            T1
    end.

merge_join1(_E2, _K2, []=Cont, _C1, _L2, _C2, M) ->
    end_merge_join(Cont, M);
merge_join1(E2, K2, [E1 | L1], C1, L2, C2, M) ->
    K1 = element(C1, E1),
    if 
        K1 == K2 ->
            same_keys2(E1, K1, L1, C1, L2, C2, E2, M);
        K1 > K2 ->
            merge_join2(E1, K1, L1, C1, L2, C2, M);
        true -> % K1 < K2
            merge_join1(E2, K2, L1, C1, L2, C2, M)
    end;
merge_join1(E2, K2, F1, C1, L2, C2, M) ->
    case F1() of
        L1 when is_list(L1) ->
            merge_join1(E2, K2, L1, C1, L2, C2, M);
        T1 ->
            T1
    end.

merge_join2(_E1, _K1, _L1, _C1, []=Cont, _C2, M) ->
    end_merge_join(Cont, M);
merge_join2(E1, K1, L1, C1, [E2 | L2], C2, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            same_keys2(E1, K1, L1, C1, L2, C2, E2, M);
        K1 > K2 ->
            merge_join2(E1, K1, L1, C1, L2, C2, M);
        true -> % K1 < K2
            merge_join1(E2, K2, L1, C1, L2, C2, M)
    end;
merge_join2(E1, K1, L1, C1, F2, C2, M) ->
    case F2() of
        L2 when is_list(L2) ->
            merge_join2(E1, K1, L1, C1, L2, C2, M);
        T2 ->
            T2
    end.

%% element(C2, E2_0) == K1
same_keys2(E1, K1, L1, C1, [], _C2, E2_0, M) ->
    Cont = fun(_L1b) -> end_merge_join([], M) end,
    loop_same_keys(E1, K1, L1, C1, [E2_0], Cont, M);
same_keys2(E1, K1, L1, C1, [E2 | L2]=L2_0, C2, E2_0, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            same_keys1(E1, K1, L1, C1, E2, C2, E2_0, L2, M);
        K1 < K2 ->
            [?JWRAP(E1, E2_0) |
             fun() -> same_loop1(L1, K1, C1, E2_0, L2_0, C2, M) end]
    end;
same_keys2(E1, K1, L1, C1, F2, C2, E2_0, M) ->
    case F2() of
        L2 when is_list(L2) ->
            same_keys2(E1, K1, L1, C1, L2, C2, E2_0, M);
        T2 ->
            Cont = fun(_L1b) -> T2 end,
            loop_same_keys(E1, K1, L1, C1, [E2_0], Cont, M)
    end.

same_loop1([], _K1_0, _C1, _E2_0, _L2, _C2, M) ->
    end_merge_join([], M);
same_loop1([E1 | L1], K1_0, C1, E2_0, L2, C2, M) ->
    K1 = element(C1, E1),
    if
        K1 == K1_0 ->
            [?JWRAP(E1, E2_0) | 
             fun() -> same_loop1(L1, K1_0, C1, E2_0, L2, C2, M) end];
        K1_0 < K1 ->
            merge_join2(E1, K1, L1, C1, L2, C2, M)
    end;
same_loop1(F1, K1_0, C1, E2_0, L2, C2, M) ->
    case F1() of
        L1 when is_list(L1) ->
            same_loop1(L1, K1_0, C1, E2_0, L2, C2, M);
        T1 ->
            T1
    end.

%% element(C2, E2_0) == K1, element(C2, E2) == K1_0
same_keys1(E1_0, K1_0, []=L1, C1, E2, C2, E2_0, L2, M) ->
    [?JWRAP(E1_0, E2_0), ?JWRAP(E1_0, E2) |
     fun() -> same_keys(K1_0, E1_0, L1, C1, L2, C2, M) end];
same_keys1(E1_0, K1_0, [E1 | _]=L1, C1, E2, C2, E2_0, L2, M) ->
    K1 = element(C1, E1),
    if 
        K1_0 == K1 ->
            E2s = [E2, E2_0],
            Sz0 = decr_list_size(M#m.max_list, E2s),
            same_keys_cache(E1_0, K1_0, L1, C1, L2, C2, E2s, Sz0, M);
        K1_0 < K1  ->
            [?JWRAP(E1_0, E2_0), ?JWRAP(E1_0, E2) |
             fun() -> same_keys(K1_0, E1_0, L1, C1, L2, C2, M) end]
    end;
same_keys1(E1_0, K1_0, F1, C1, E2, C2, E2_0, L2, M) ->
    case F1() of
        L1 when is_list(L1) ->
            same_keys1(E1_0, K1_0, L1, C1, E2, C2, E2_0, L2, M);
        T1 ->
            Cont = fun() -> T1 end,
            loop_same(E1_0, [E2, E2_0], Cont)
    end.

%% There is no such element E in L1 such that element(C1, E) == K1.
same_keys(_K1, _E1, _L1, _C1, []=Cont, _C2, M) ->
    end_merge_join(Cont, M);
same_keys(K1, E1, L1, C1, [E2 | L2], C2, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            [?JWRAP(E1, E2) | 
             fun() -> same_keys(K1, E1, L1, C1, L2, C2, M) end];
        K1 < K2 ->
            merge_join1(E2, K2, L1, C1, L2, C2, M)
    end;
same_keys(K1, E1, L1, C1, F2, C2, M) ->
    case F2() of
        L2 when is_list(L2) ->
            same_keys(K1, E1, L1, C1, L2, C2, M);
        T2 ->
            T2
    end.

%% There are at least two elements in [E1 | L1] that are to be combined
%% with the elements in E2s (length(E2s) > 1). This loop covers the case
%% when all elements in E2 with key K1 can be kept in RAM.
same_keys_cache(E1, K1, L1, C1, [], _C2, E2s, _Sz, M) ->
    Cont = fun(_L1b) -> end_merge_join([], M) end,
    loop_same_keys(E1, K1, L1, C1, E2s, Cont, M);
same_keys_cache(E1, K1, L1, C1, L2, C2, E2s, Sz0, M) when Sz0 < 0 ->
    case init_merge_join(M) of
        ok -> 
            Sz = M#m.max_list,
            C = fun() -> 
                        same_keys_file(E1, K1, L1, C1, L2, C2, [], Sz, M) 
                end,
            write_same_keys(E1, E2s, M, C);
        Error ->
            Error
    end;
same_keys_cache(E1, K1, L1, C1, [E2 | L2], C2, E2s, Sz0, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            Sz = decr_list_size(Sz0, E2),
            same_keys_cache(E1, K1, L1, C1, L2, C2, [E2 | E2s], Sz, M);
        K1 < K2 ->
            Cont = fun(L1b) -> merge_join1(E2, K2, L1b, C1, L2, C2, M) end,
            loop_same_keys(E1, K1, L1, C1, E2s, Cont, M)
    end;
same_keys_cache(E1, K1, L1, C1, F2, C2, E2s, Sz, M) ->
    case F2() of
        L2 when is_list(L2) ->
            same_keys_cache(E1, K1, L1, C1, L2, C2, E2s, Sz, M);
        T2 ->
            Cont = fun(_L1b) -> T2 end,
            loop_same_keys(E1, K1, L1, C1, E2s, Cont, M)
    end.

%% E2s holds all elements E2 in L2 such that element(E2, C2) == K1.
loop_same_keys(E1, _K1, [], _C1, E2s, _Cont, M) ->
    end_merge_join(loop_same(E1, E2s, []), M);
loop_same_keys(E1, K1, L1, C1, E2s, Cont, M) ->
    loop_same(E1, E2s, fun() -> loop_keys(K1, L1, C1, E2s, Cont, M) end).

loop_same(_E1, [], L) ->
    L;
loop_same(E1, [E2 | E2s], L) ->
    loop_same(E1, E2s, [?JWRAP(E1, E2) | L]).

loop_keys(K, [E1 | L1]=L1_0, C1, E2s, Cont, M) ->
    K1 = element(C1, E1),
    if 
        K1 == K ->
            loop_same_keys(E1, K1, L1, C1, E2s, Cont, M);
        K1 > K -> 
            Cont(L1_0)
    end;
loop_keys(_K, []=L1, _C1, _Es2, Cont, _M) ->
    Cont(L1);
loop_keys(K, F1, C1, E2s, Cont, M) ->
    case F1() of
        L1 when is_list(L1) ->
            loop_keys(K, L1, C1, E2s, Cont, M);
        T1 ->
            T1
    end.

%% This is for the case when a temporary file has to be used.
same_keys_file(E1, K1, L1, C1, [], _C2, E2s, _Sz, M) ->
    Cont = fun(_L1b) -> end_merge_join([], M) end,
    same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont);    
same_keys_file(E1, K1, L1, C1, L2, C2, E2s, Sz0, M) when Sz0 < 0 ->
    Sz = M#m.max_list,
    C = fun() -> same_keys_file(E1, K1, L1, C1, L2, C2, [], Sz, M) end,
    write_same_keys(E1, E2s, M, C);
same_keys_file(E1, K1, L1, C1, [E2 | L2], C2, E2s, Sz0, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            Sz = decr_list_size(Sz0, E2),
            same_keys_file(E1, K1, L1, C1, L2, C2, [E2 | E2s], Sz, M);
        K1 < K2 ->
            Cont = fun(L1b) ->
                           %% The temporary file could be truncated here.
                           merge_join1(E2, K2, L1b, C1, L2, C2, M)
                   end,
            same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont)
    end;
same_keys_file(E1, K1, L1, C1, F2, C2, E2s, Sz, M) ->
    case F2() of
        L2 when is_list(L2) ->
            same_keys_file(E1, K1, L1, C1, L2, C2, E2s, Sz, M);
        T2 ->
            Cont = fun(_L1b) -> T2 end,
            same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont)
    end.

same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont) ->
    C = fun() -> loop_keys_file(K1, L1, C1, Cont, M) end,
    write_same_keys(E1, E2s, M, C).

write_same_keys(_E1, [], _M, Cont) ->
    Cont();
write_same_keys(E1, Es2, M, Cont) ->
    write_same_keys(E1, Es2, M, [], Cont).

%% Avoids one (the first) traversal of the temporary file.
write_same_keys(_E1, [], M, E2s, Objs) ->
    case write_merge_join(M, E2s) of
        ok -> Objs;
        Error -> Error
    end;
write_same_keys(E1, [E2 | E2s0], M, E2s, Objs) ->
    BE2 = term_to_binary(E2),
    write_same_keys(E1, E2s0, M, [BE2 | E2s], [?JWRAP(E1, E2) | Objs]).

loop_keys_file(K, [E1 | L1]=L1_0, C1, Cont, M) ->
    K1 = element(C1, E1),
    if
        K1 == K ->
            C = fun() -> loop_keys_file(K1, L1, C1, Cont, M) end,
            read_merge_join(M, E1, C);
        K1 > K ->
            Cont(L1_0)
    end;
loop_keys_file(_K, []=L1, _C1, Cont, _M) ->
    Cont(L1);
loop_keys_file(K, F1, C1, Cont, M) ->
    case F1() of
        L1 when is_list(L1) ->
            loop_keys_file(K, L1, C1, Cont, M);
        T1 ->
            T1
    end.

end_merge_join(Reply, M) ->
    end_merge_join(M),
    Reply.

%% Normally post_funs() cleans up temporary files by calling funs in
%% Post. It seems impossible to do that with the temporary file(s)
%% used when many objects have the same key--such a file is created
%% after the setup when Post is prepared. There seems to be no real
%% alternative to using the process dictionary, at least as things
%% have been implemented so far. Probably all of Post could have been
%% put in the process dictionary...

-define(MERGE_JOIN_FILE, '$_qlc_merge_join_tmpfiles_').

init_merge_join(#m{id = MergeId, tmpdir = TmpDir, tmp_usage = TmpUsage}) ->
    case tmp_merge_file(MergeId) of
        {Fd, FileName} ->
            case file:position(Fd, bof) of
                {ok, 0} ->
                    case file:truncate(Fd) of
                        ok ->
                            ok;
                        Error ->
                            file_error(FileName, Error)
                    end;
                Error ->
                    file_error(FileName, Error)
            end;
        none when TmpUsage =:= not_allowed ->
            error({tmpdir_usage, joining});
        none ->
            maybe_error_logger(TmpUsage, joining),
            FName = tmp_filename(TmpDir),
            case file:open(FName, [raw, binary, read, write]) of
                {ok, Fd} ->
                    TmpFiles = get(?MERGE_JOIN_FILE),
                    put(?MERGE_JOIN_FILE, [{MergeId, Fd, FName} | TmpFiles]),
                    ok;
                Error ->
                    file_error(FName, Error)
            end
    end.

write_merge_join(#m{id = MergeId}, BTerms) ->
    {Fd, FileName} = tmp_merge_file(MergeId),
    write_binary_terms(BTerms, Fd, FileName).

read_merge_join(#m{id = MergeId}, E1, Cont) ->
    {Fd, FileName} = tmp_merge_file(MergeId),
    case file:position(Fd, bof) of
        {ok, 0} -> 
            Fun = fun([], _) ->
                          Cont();
                     (Ts, C) when is_list(Ts) ->
                          join_read_terms(E1, Ts, C)
                  end,
            file_loop_read(<<>>, ?CHUNK_SIZE, {Fd, FileName}, Fun);
        Error -> 
            file_error(FileName, Error)
    end.

join_read_terms(_E1, [], Objs) ->
    Objs;
join_read_terms(E1, [E2 | E2s], Objs) ->
    join_read_terms(E1, E2s, [?JWRAP(E1, E2) | Objs]).

end_merge_join(#m{id = MergeId}) ->
    case tmp_merge_file(MergeId) of
        none -> 
            ok;
        {Fd, FileName}  ->
            _ = file:close(Fd),
            _ = file:delete(FileName),
            put(?MERGE_JOIN_FILE, 
                lists:keydelete(MergeId, 1, get(?MERGE_JOIN_FILE)))
    end.

end_all_merge_joins() ->
    lists:foreach(
      fun(Id) -> end_merge_join(#m{id = Id}) end, 
      [Id || {Id, _Fd, _FileName} <- lists:flatten([get(?MERGE_JOIN_FILE)])]),
    erase(?MERGE_JOIN_FILE).

merge_join_id() ->
    case get(?MERGE_JOIN_FILE) of
        undefined ->
            put(?MERGE_JOIN_FILE, []);
        _ -> 
            ok
    end,
    make_ref().

tmp_merge_file(MergeId) ->
    TmpFiles = get(?MERGE_JOIN_FILE),
    case lists:keyfind(MergeId, 1, TmpFiles) of
        {MergeId, Fd, FileName} ->
            {Fd, FileName};
        false ->
            none
    end.

decr_list_size(Sz0, E) when is_integer(Sz0) ->
    Sz0 - erlang:external_size(E).

%%% End of merge join.    

lookup_join([E1 | L1], C1, LuF, C2, Rev) ->
    K1 = element(C1, E1),
    case LuF(C2, [K1]) of
        [] ->
            lookup_join(L1, C1, LuF, C2, Rev);
        [E2] when Rev ->
            [?JWRAP(E2, E1) | fun() -> lookup_join(L1, C1, LuF, C2, Rev) end];
        [E2] ->
            [?JWRAP(E1, E2) | fun() -> lookup_join(L1, C1, LuF, C2, Rev) end];
        E2s when is_list(E2s), Rev ->
            [?JWRAP(E2, E1) || E2 <- E2s] ++
                fun() -> lookup_join(L1, C1, LuF, C2, Rev) end;
        E2s when is_list(E2s) ->
            [?JWRAP(E1, E2) || E2 <- E2s] ++
                fun() -> lookup_join(L1, C1, LuF, C2, Rev) end;
        Term ->
            Term
    end;
lookup_join([]=Cont, _C1, _LuF, _C2, _Rev) ->
    Cont;
lookup_join(F1, C1, LuF, C2, Rev) ->
    case F1() of
        L1 when is_list(L1) ->
            lookup_join(L1, C1, LuF, C2, Rev);
        T1 ->
            T1
    end.

maybe_error_logger(allowed, _) ->
    ok;
maybe_error_logger(Name, Why) ->
    [_, _, {?MODULE,maybe_error_logger,_,_} | Stacktrace] =
	expand_stacktrace(),
    Trimmer = fun(M, _F, _A) -> M =:= erl_eval end,
    Formater = fun(Term, I) -> io_lib:print(Term, I, 80, -1) end,
    X = lib:format_stacktrace(1, Stacktrace, Trimmer, Formater),
    error_logger:Name("qlc: temporary file was needed for ~w\n~ts\n",
                      [Why, lists:flatten(X)]).

expand_stacktrace() ->
    D = erlang:system_flag(backtrace_depth, 8),
    try
        %% Compensate for a bug in erlang:system_flag/2:
        expand_stacktrace(erlang:max(1, D))
    after
        erlang:system_flag(backtrace_depth, D)
    end.

expand_stacktrace(D) ->
    _ = erlang:system_flag(backtrace_depth, D),
    {'EXIT', {foo, Stacktrace}} = (catch erlang:error(foo)),
    L = lists:takewhile(fun({M,_,_,_}) -> M =/= ?MODULE
                        end, lists:reverse(Stacktrace)),
    if
        length(L) < 3 andalso length(Stacktrace) =:= D ->
            expand_stacktrace(D + 5);
        true ->
            Stacktrace
    end.

write_binary_terms(BTerms, Fd, FileName) ->
    case file:write(Fd, size_bin(BTerms, [])) of
        ok ->
            ok;
        Error ->
            file_error(FileName, Error)
    end.

post_funs(L) ->
    end_all_merge_joins(),
    local_post(L).

local_post(L) ->
    lists:foreach(fun(undefined) -> ok;
                     (F) -> catch (F)() 
                  end, L).

call(undefined, _Arg, Default, _Post) ->
    Default;
call(Fun, Arg, _Default, Post) ->
    try
        Fun(Arg) 
    catch Class:Reason:Stacktrace ->
        post_funs(Post),
        erlang:raise(Class, Reason, Stacktrace)
    end.

grd(undefined, _Arg) ->
    false;
grd(Fun, Arg) ->
    case Fun(Arg) of
        true -> 
            true;
        _ ->
            false
    end.

anno0() ->
    anno(0).

anno1() ->
    anno(1).

anno(L) ->
    erl_anno:new(L).

family(L) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(L))).

family_union(L) ->
    R = sofs:relation(L,[{atom,[atom]}]),
    sofs:to_external(sofs:family_union(sofs:relation_to_family(R))).

file_error(File, {error, Reason}) ->
    error({file_error, File, Reason}).

-spec throw_file_error(string(), {'error',atom()}) -> no_return().

throw_file_error(File, {error, Reason}) ->
    throw_reason({file_error, File, Reason}).

-spec throw_reason(term()) -> no_return().

throw_reason(Reason) ->
    throw_error(error(Reason)).

-spec throw_error(term()) -> no_return().

throw_error(Error) ->
    throw(Error).

error(Reason) ->
    {error, ?MODULE, Reason}.
