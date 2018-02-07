%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(file_sorter).

%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([sort/1, sort/2, sort/3, 
         keysort/2, keysort/3, keysort/4,
         merge/2, merge/3, 
         keymerge/3, keymerge/4,
         check/1, check/2, 
         keycheck/2, keycheck/3]).

-dialyzer(no_improper_lists).

-include_lib("kernel/include/file.hrl").

-define(CHUNKSIZE, 16384).
-define(RUNSIZE, 524288).
-define(NOMERGE, 16).
-define(MERGESIZE, ?CHUNKSIZE).

-define(MAXSIZE, (1 bsl 31)).

-record(w, {keypos, runs = [[]], seq = 1, in, out, fun_out, prefix, temp = [],
            format, runsize, no_files, order, chunksize, wfd, ref, z, unique,
            hdlen, inout_value}).

-record(opts, {format = binary_term_fun(), size = ?RUNSIZE, 
               no_files = ?NOMERGE, tmpdir = default, order = ascending, 
               compressed = false, unique = false, header = 4}).

-compile({inline, [{badarg, 2}, {make_key,2}, {make_stable_key,3}, {cfun,3}]}).

%%%
%%% Exported functions
%%%

-export_type([reason/0]).

-type(file_name() :: file:name()).
-type(file_names() :: [file:name()]).
-type(i_command() :: read | close).
-type(i_reply() :: end_of_input | {end_of_input, value()}
                 | {[object()], infun()} | input_reply()).
-type(infun() :: fun((i_command()) -> i_reply())).
-type(input() :: file_names() | infun()).
-type(input_reply() :: term()).
-type(o_command() :: {value, value()} | [object()] | close).
-type(o_reply() :: outfun() | output_reply()).
-type(object() :: term() | binary()).
-type(outfun() :: fun((o_command()) -> o_reply())).
-type(output() :: file_name() | outfun()).
-type(output_reply() :: term()).
-type(value() :: term()).

-type(options() :: [option()] | option()).
-type(option() :: {compressed, boolean()}
                | {header, header_length()}
                | {format, format()}
                | {no_files, no_files()}
                | {order, order()}
                | {size, size()}
                | {tmpdir, tmp_directory()}
                | {unique, boolean()}).
-type(format() :: binary_term | term | binary | format_fun()).
-type(format_fun() :: fun((binary()) -> term())).
-type(header_length() :: pos_integer()).
-type(key_pos() :: pos_integer() | [pos_integer()]).
-type(no_files() :: pos_integer()). % > 1
-type(order() :: ascending | descending | order_fun()).
-type(order_fun() :: fun((term(), term()) -> boolean())).
-type(size() :: non_neg_integer()).
-type(tmp_directory() :: [] | file:name()).

-type(reason() :: bad_object
                | {bad_object, file_name()}
                | {bad_term, file_name()}
                | {file_error, file_name(),
                   file:posix() | badarg | system_limit}
                | {premature_eof, file_name()}).

-spec(sort(FileName) -> Reply when
      FileName :: file_name(),
      Reply :: ok | {error, reason()} | input_reply() | output_reply()).
sort(FileName) ->
    sort([FileName], FileName).

-spec(sort(Input, Output) -> Reply when
      Input :: input(),
      Output :: output(),
      Reply :: ok | {error, reason()} | input_reply() | output_reply()).
sort(Input, Output) ->
    sort(Input, Output, []).

-spec(sort(Input, Output, Options) -> Reply when
      Input :: input(),
      Output :: output(),
      Options :: options(),
      Reply :: ok | {error, reason()} | input_reply() | output_reply()).
sort(Input0, Output0, Options) ->
    case {is_input(Input0), maybe_output(Output0), options(Options)}  of
        {{true,Input}, {true,Output}, #opts{}=Opts} -> 
            do_sort(0, Input, Output, Opts, sort);
        T -> 
            badarg(culprit(tuple_to_list(T)), [Input0, Output0, Options])
    end.

-spec(keysort(KeyPos, FileName) -> Reply when
      KeyPos :: key_pos(),
      FileName :: file_name(),
      Reply :: ok | {error, reason()} | input_reply() | output_reply()).
keysort(KeyPos, FileName) ->
    keysort(KeyPos, [FileName], FileName).

-spec(keysort(KeyPos, Input, Output) -> Reply when
      KeyPos :: key_pos(),
      Input :: input(),
      Output :: output(),
      Reply :: ok | {error, reason()} | input_reply() | output_reply()).
keysort(KeyPos, Input, Output) ->
    keysort(KeyPos, Input, Output, []).

-spec(keysort(KeyPos, Input, Output, Options) -> Reply when
      KeyPos :: key_pos(),
      Input :: input(),
      Output :: output(),
      Options :: options(),
      Reply :: ok | {error, reason()} | input_reply() | output_reply()).
keysort(KeyPos, Input0, Output0, Options) ->
    R = case {is_keypos(KeyPos), is_input(Input0), 
              maybe_output(Output0), options(Options)} of
            {_, _, _, #opts{format = binary}} -> 
                {Input0,Output0,[{badarg,format}]};
            {_, _, _, #opts{order = Order}} when is_function(Order) -> 
                {Input0,Output0,[{badarg,order}]};
            {true, {true,In}, {true,Out}, #opts{}=Opts} -> 
                {In,Out,Opts};
            T -> 
                {Input0,Output0,tuple_to_list(T)}
        end,
    case R of
        {Input,Output,#opts{}=O} ->
            do_sort(KeyPos, Input, Output, O, sort);
        {_,_,O} ->
            badarg(culprit(O), [KeyPos, Input0, Output0, Options])
    end.

-spec(merge(FileNames, Output) -> Reply when
      FileNames :: file_names(),
      Output :: output(),
      Reply :: ok | {error, reason()} | output_reply()).
merge(Files, Output) ->
    merge(Files, Output, []).

-spec(merge(FileNames, Output, Options) -> Reply when
      FileNames :: file_names(),
      Output :: output(),
      Options :: options(),
      Reply :: ok | {error, reason()} | output_reply()).
merge(Files0, Output0, Options) ->
    case {is_files(Files0), maybe_output(Output0), options(Options)} of
        %% size not used
        {{true,Files}, {true,Output}, #opts{}=Opts} ->
            do_sort(0, Files, Output, Opts, merge);
        T -> 
            badarg(culprit(tuple_to_list(T)), [Files0, Output0, Options])
    end.

-spec(keymerge(KeyPos, FileNames, Output) -> Reply when
      KeyPos :: key_pos(),
      FileNames :: file_names(),
      Output :: output(),
      Reply :: ok | {error, reason()} | output_reply()).
keymerge(KeyPos, Files, Output) ->
    keymerge(KeyPos, Files, Output, []).

-spec(keymerge(KeyPos, FileNames, Output, Options) -> Reply when
      KeyPos :: key_pos(),
      FileNames :: file_names(),
      Output :: output(),
      Options :: options(),
      Reply :: ok | {error, reason()} | output_reply()).
keymerge(KeyPos, Files0, Output0, Options) ->
    R = case {is_keypos(KeyPos), is_files(Files0), 
              maybe_output(Output0), options(Options)} of
            {_, _, _, #opts{format = binary}} -> 
                {Files0,Output0,[{badarg,format}]};
            {_, _, _, #opts{order = Order}} when is_function(Order) -> 
                {Files0,Output0,[{badarg,order}]};
            {true, {true,Fs}, {true,Out}, #opts{}=Opts} -> 
                {Fs,Out,Opts};
            T -> 
                {Files0,Output0,tuple_to_list(T)}
        end,
    case R of
        {Files,Output,#opts{}=O} -> 
            do_sort(KeyPos, Files, Output, O, merge);
        {_,_,O} -> 
            badarg(culprit(O), [KeyPos, Files0, Output0, Options])
    end.

-spec(check(FileName) -> Reply when
      FileName :: file_name(),
      Reply :: {ok, [Result]} | {error, reason()},
      Result :: {FileName, TermPosition, term()},
      TermPosition :: pos_integer()).
check(FileName) ->
    check([FileName], []).

-spec(check(FileNames, Options) -> Reply when
      FileNames :: file_names(),
      Options :: options(),
      Reply :: {ok, [Result]} | {error, reason()},
      Result :: {FileName, TermPosition, term()},
      FileName :: file_name(),
      TermPosition :: pos_integer()).
check(Files0, Options) ->
    case {is_files(Files0), options(Options)} of
        {{true,Files}, #opts{}=Opts} ->
            do_sort(0, Files, undefined, Opts, check);
        T ->
            badarg(culprit(tuple_to_list(T)), [Files0, Options])
    end.

-spec(keycheck(KeyPos, FileName) -> Reply when
      KeyPos :: key_pos(),
      FileName :: file_name(),
      Reply :: {ok, [Result]} | {error, reason()},
      Result :: {FileName, TermPosition, term()},
      TermPosition :: pos_integer()).
keycheck(KeyPos, FileName) ->
    keycheck(KeyPos, [FileName], []).

-spec(keycheck(KeyPos, FileNames, Options) -> Reply when
      KeyPos :: key_pos(),
      FileNames :: file_names(),
      Options :: options(),
      Reply :: {ok, [Result]} | {error, reason()},
      Result :: {FileName, TermPosition, term()},
      FileName :: file_name(),
      TermPosition :: pos_integer()).
keycheck(KeyPos, Files0, Options) ->
    R = case {is_keypos(KeyPos), is_files(Files0), options(Options)} of
            {_, _, #opts{format = binary}} -> 
                {Files0,[{badarg,format}]};
            {_, _, #opts{order = Order}} when is_function(Order) -> 
                {Files0,[{badarg,order}]};
            {true, {true,Fs}, #opts{}=Opts} -> 
                {Fs,Opts};
            T -> 
                {Files0,tuple_to_list(T)}
        end,
    case R of
        {Files,#opts{}=O} -> 
            do_sort(KeyPos, Files, undefined, O, check);
        {_,O} -> 
            badarg(culprit(O), [KeyPos, Files0, Options])
    end.

%%%
%%% Local functions
%%%

%%-define(debug, true).

-ifdef(debug).
-define(DEBUG(S, A), io:format(S, A)).
-else.
-define(DEBUG(S, A), ok).
-endif.

culprit([{error, _} = E | _]) ->
    E;
culprit([{badarg, _} = B | _]) ->
    B;
culprit([_ | B]) -> 
    culprit(B).

%% Inlined.
badarg({error, _} = E, _Args) ->
    E;
badarg({badarg, _} = B, Args) ->
    erlang:error(B, Args).

options(Options) when is_list(Options) ->
    options(Options, #opts{});
options(Option) ->
    options([Option]).

options([{format, Format} | L], Opts) when Format =:= binary; 
                                           Format =:= term;
                                           is_function(Format, 1) ->
    options(L, Opts#opts{format = Format});
options([{format, binary_term} | L], Opts) ->
    options(L, Opts#opts{format = binary_term_fun()});
options([{size, Size} | L], Opts) when is_integer(Size), Size >= 0 ->
    options(L, Opts#opts{size = erlang:max(Size, 1)});
options([{no_files, NoFiles} | L], Opts) when is_integer(NoFiles), 
                                              NoFiles > 1 ->
    options(L, Opts#opts{no_files = NoFiles});
options([{tmpdir, ""} | L], Opts) ->
    options(L, Opts#opts{tmpdir = default});
options([{tmpdir, Dir} | L],  Opts) ->
    case catch filename:absname(Dir) of
        {'EXIT', _} ->
            {badarg, Dir};
        FileName -> 
            options(L, Opts#opts{tmpdir = {dir, FileName}})
    end;
options([{order, Fun} | L], Opts) when is_function(Fun, 2) ->
    options(L, Opts#opts{order = Fun});
options([{order, Order} | L], Opts) when Order =:= ascending; 
                                         Order =:= descending ->
    options(L, Opts#opts{order = Order});
options([{compressed, Bool} | L], Opts) when is_boolean(Bool) ->
    options(L, Opts#opts{compressed = Bool});
options([{unique, Bool} | L], Opts) when is_boolean(Bool) ->
    options(L, Opts#opts{unique = Bool});
options([{header, Len} | L], Opts) 
                when is_integer(Len), Len > 0, Len < ?MAXSIZE ->
    options(L, Opts#opts{header = Len});
options([], Opts) ->
    if 
        Opts#opts.format =:= term, Opts#opts.header =/= 4 ->
            {badarg, header};
        true ->
            Opts
    end;
options([Bad | _], _Opts) ->
    {badarg, Bad};
options(Bad, _Opts) ->
    {badarg, Bad}.

-define(OBJ(X, Y), {X, Y}).
-define(SK(T, I), [T | I]). % stable key

do_sort(KeyPos0, Input0, Output0, Opts, Do) ->
    #opts{format = Format0, size = Size, no_files = NoFiles,
          tmpdir = TmpDir, order = Order, compressed = Compressed,
          unique = Unique, header = HdLen} = Opts,
    Prefix = tmp_prefix(Output0, TmpDir),
    ChunkSize = ?CHUNKSIZE,
    Ref = make_ref(),
    KeyPos = case KeyPos0 of [Kp] -> Kp; _ -> KeyPos0 end,
    {Format, Input} = wrap_input(Format0, Do, Input0),
    Z = if Compressed -> [compressed]; true -> [] end,
    {Output, FunOut} = wrap_output_terms(Format0, Output0, Z),
    W = #w{keypos = KeyPos, out = Output, fun_out = FunOut,
           prefix = Prefix, format = Format, runsize = Size, 
           no_files = NoFiles, order = Order, chunksize = ChunkSize, 
           ref = Ref, z = Z, unique = Unique, hdlen = HdLen, 
           inout_value = no_value},
    try
        doit(Do, Input, W)
    catch {Ref,Error} ->
        Error
    end.
    
doit(sort, Input, W) ->
    files(1, [], 0, W, Input);
doit(merge, Input, W) ->
    last_merge(Input, W);
doit(check, Input, W) ->
    check_files(Input, W, []).

wrap_input(term, check, Files) ->
    Fun = fun(File) -> 
                  Fn = merge_terms_fun(file_rterms(no_file, [File])),
                  {fn, Fn, File}
          end,
    {binary_term_fun(), [Fun(F) || F <- Files]};
wrap_input(Format, check, Files) ->
    {Format, Files};
wrap_input(term, merge, Files) ->
    Fun = fun(File) -> merge_terms_fun(file_rterms(no_file, [File])) end,
    Input = lists:reverse([Fun(F) || F <- Files]),
    {binary_term_fun(), Input};
wrap_input(Format, merge, Files) ->
    Input = lists:reverse([merge_bins_fun(F) || F <- Files]),
    {Format, Input};
wrap_input(term, sort, InFun) when is_function(InFun, 1) ->
    {binary_term_fun(), fun_rterms(InFun)};
wrap_input(term, sort, Files) ->
    {binary_term_fun(), file_rterms(no_file, Files)};
wrap_input(Format, sort, Input) ->
    {Format, Input}.

merge_terms_fun(RFun) ->
    fun(close) ->
            RFun(close);
       ({I, [], _LSz, W}) ->
            case RFun(read) of
                end_of_input ->
                    eof;
                {Objs, NRFun} when is_function(NRFun, 1) ->
                    {_, [], Ts, _} = fun_objs(Objs, [], 0, ?MAXSIZE, I, W),
                    {{I, Ts, ?CHUNKSIZE}, merge_terms_fun(NRFun)};
                Error ->
                    error(Error, W)
            end
    end.

merge_bins_fun(FileName) ->
    fun(close) ->
            ok;
       ({_I, _L, _LSz, W} = A) ->
            Fun = read_fun(FileName, user, W),
            Fun(A)
    end.

wrap_output_terms(term, OutFun, _Z) when is_function(OutFun, 1) ->
    {fun_wterms(OutFun), true};
wrap_output_terms(term, File, Z) when File =/= undefined ->
    {file_wterms(name, File, Z++[write]), false};
wrap_output_terms(_Format, Output, _Z) ->
    {Output, is_function(Output, 1)}.

binary_term_fun() ->
    fun binary_to_term/1.

check_files([], _W, L) ->
    {ok, lists:reverse(L)};
check_files([FN | FNs], W, L) ->
    {IFun, FileName} = 
        case FN of
            {fn, Fun, File} ->
                {Fun, File};
            File ->
                {read_fun(File, user, W), File}
        end,
    NW = W#w{in = IFun},
    check_run(IFun, FileName, FNs, NW, L, 2, nolast).

check_run(IFun, F, FNs, W, L, I, Last) ->
    case IFun({{merge,I}, [], 0, W}) of
        {{_I, Objs, _LSz}, IFun1} ->
            NW = W#w{in = IFun1},
            check_objs0(IFun1, F, FNs, NW, L, I, Last, lists:reverse(Objs));
        eof ->
            NW = W#w{in = undefined},
            check_files(FNs, NW, L)
    end.

check_objs0(IFun, F, FNs, W, L, I, nolast, [?OBJ(T,_BT) | Os]) ->
    check_objs1(IFun, F, FNs, W, L, I, T, Os);
check_objs0(IFun, F, FNs, W, L, I, Last, []) ->
    check_run(IFun, F, FNs, W, L, I, Last);
check_objs0(IFun, F, FNs, W, L, I, {last, Last}, Os) ->
    check_objs1(IFun, F, FNs, W, L, I, Last, Os).

check_objs1(IFun, F, FNs, W, L, I, LastT, Os) ->
    case W of
        #w{order = ascending, unique = true} ->
            ucheck_objs(IFun, F, FNs, W, L, I, LastT, Os);
        #w{order = ascending, unique = false} ->
            check_objs(IFun, F, FNs, W, L, I, LastT, Os);
        #w{order = descending, unique = true} ->
            rucheck_objs(IFun, F, FNs, W, L, I, LastT, Os);
        #w{order = descending, unique = false} ->
            rcheck_objs(IFun, F, FNs, W, L, I, LastT, Os);
        #w{order = CF, unique = true} ->
            uccheck_objs(IFun, F, FNs, W, L, I, LastT, Os, CF);
        #w{order = CF, unique = false} ->
            ccheck_objs(IFun, F, FNs, W, L, I, LastT, Os, CF)
    end.

check_objs(IFun, F, FNs, W, L, I, Last, [?OBJ(T,_BT) | Os]) when T >= Last ->
    check_objs(IFun, F, FNs, W, L, I+1, T, Os);
check_objs(IFun, F, FNs, W, L, I, _Last, [?OBJ(_T,BT) | _]) ->
    culprit_found(IFun, F, FNs, W, L, I, BT);
check_objs(IFun, F, FNs, W, L, I, Last, []) ->
    check_run(IFun, F, FNs, W, L, I, {last, Last}).

rcheck_objs(IFun, F, FNs, W, L, I, Last, [?OBJ(T,_BT) | Os]) when T =< Last ->
    rcheck_objs(IFun, F, FNs, W, L, I+1, T, Os);
rcheck_objs(IFun, F, FNs, W, L, I, _Last, [?OBJ(_T,BT) | _]) ->
    culprit_found(IFun, F, FNs, W, L, I, BT);
rcheck_objs(IFun, F, FNs, W, L, I, Last, []) ->
    check_run(IFun, F, FNs, W, L, I, {last, Last}).

ucheck_objs(IFun, F, FNs, W, L, I, LT, [?OBJ(T,_BT) | Os]) when T > LT ->
    ucheck_objs(IFun, F, FNs, W, L, I+1, T, Os);
ucheck_objs(IFun, F, FNs, W, L, I, _LT, [?OBJ(_T,BT) | _]) ->
    culprit_found(IFun, F, FNs, W, L, I, BT);
ucheck_objs(IFun, F, FNs, W, L, I, LT, []) ->
    check_run(IFun, F, FNs, W, L, I, {last, LT}).

rucheck_objs(IFun, F, FNs, W, L, I, LT, [?OBJ(T,_BT) | Os]) when T < LT ->
    rucheck_objs(IFun, F, FNs, W, L, I+1, T, Os);
rucheck_objs(IFun, F, FNs, W, L, I, _LT, [?OBJ(_T,BT) | _]) ->
    culprit_found(IFun, F, FNs, W, L, I, BT);
rucheck_objs(IFun, F, FNs, W, L, I, LT, []) ->
    check_run(IFun, F, FNs, W, L, I, {last, LT}).

ccheck_objs(IFun, F, FNs, W, L, I, LT, [?OBJ(T,BT) | Os], CF) ->
    case CF(LT, T) of
        true -> % LT =< T
            ccheck_objs(IFun, F, FNs, W, L, I+1, T, Os, CF);
        false -> % LT > T
            culprit_found(IFun, F, FNs, W, L, I, BT)
    end;
ccheck_objs(IFun, F, FNs, W, L, I, LT, [], _CF) ->
    check_run(IFun, F, FNs, W, L, I, {last, LT}).

uccheck_objs(IFun, F, FNs, W, L, I, LT, [?OBJ(T,BT) | Os], CF) ->
    case CF(LT, T) of
        true -> % LT =< T
            case CF(T, LT) of
                true -> % T equal to LT
                    culprit_found(IFun, F, FNs, W, L, I, BT);
                false -> % LT < T
                    uccheck_objs(IFun, F, FNs, W, L, I+1, T, Os, CF)
            end;
        false -> % LT > T
            culprit_found(IFun, F, FNs, W, L, I, BT)
    end;
uccheck_objs(IFun, F, FNs, W, L, I, LT, [], _CF) ->
    check_run(IFun, F, FNs, W, L, I, {last, LT}).

culprit_found(IFun, F, FNs, W, L, I, [_Size | BT]) ->
    IFun(close),
    check_files(FNs, W, [{F,I,binary_to_term(BT)} | L]).

files(_I, L, _LSz, #w{seq = 1, out = Out}=W, []) ->
    %% No temporary files created, everything in L.
    case Out of
        Fun when is_function(Fun) ->
            SL = internal_sort(L, W),
            W1 = outfun(binterm_objects(SL, []), W),
            NW = close_input(W1),
            outfun(close, NW);
        Out ->
            _ = write_run(L, W, Out),
            ok
    end;
files(_I, L, _LSz, W, []) ->
    W1 = write_run(L, W),
    last_merge(lists:append(W1#w.runs), W1);
files(I, L, LSz, W, Fun) when is_function(Fun) ->
    NW = W#w{in = Fun},
    fun_run(I, L, LSz, NW, []);
files(I, L, LSz, W, [FileName | FileNames]) ->
    InFun = read_fun(FileName, user, W),
    NW = W#w{in = InFun},
    file_run(InFun, FileNames, I, L, LSz, NW).

file_run(InFun, FileNames, I, L, LSz, W) when LSz < W#w.runsize ->
    case InFun({I, L, LSz, W}) of
        {{I1, L1, LSz1}, InFun1} ->
            NW = W#w{in = InFun1},
            file_run(InFun1, FileNames, I1, L1, LSz1, NW);
        eof ->
            NW = W#w{in = undefined},
            files(I, L, LSz, NW, FileNames)
    end;
file_run(InFun, FileNames, I, L, _LSz, W) ->
    NW = write_run(L, W),
    file_run(InFun, FileNames, I, [], 0, NW).

fun_run(I, L, LSz, W, []) ->
    case infun(W) of
        {end_of_input, NW} ->
            files(I, L, LSz, NW, []);
        {cont, NW, Objs} ->
            fun_run(I, L, LSz, NW, Objs)
    end;
fun_run(I, L, LSz, #w{runsize = Runsize}=W, Objs) when LSz < Runsize ->
    {NI, NObjs, NL, NLSz} = fun_objs(Objs, L, LSz, Runsize, I, W),
    fun_run(NI, NL, NLSz, W, NObjs);
fun_run(I, L, _LSz, W, Objs) ->
    NW = write_run(L, W),
    fun_run(I, [], 0, NW, Objs).

write_run([], W) ->
    W;
write_run(L, W) ->
    {W1, Temp} = next_temp(W),
    NW = write_run(L, W1, Temp),
    [R | Rs] = NW#w.runs,
    merge_runs([[Temp | R] | Rs], [], NW).

write_run(L, W, FileName) ->
    SL = internal_sort(L, W),
    BTs = binterms(SL, []), 
    {Fd, W1} = open_file(FileName, W),
    write(Fd, FileName, BTs, W1),
    close_file(Fd, W1).

%% Returns a list in reversed order.
internal_sort([]=L, _W) ->
    L;
internal_sort(L, #w{order = CFun, unique = Unique}) when is_function(CFun) ->
    Fun = fun(?OBJ(T1, _), ?OBJ(T2, _)) -> CFun(T1, T2) end,
    RL = lists:reverse(L),
    lists:reverse(if
                      Unique -> 
                          lists:usort(Fun, RL);
                      true -> 
                          lists:sort(Fun, RL)
                  end);
internal_sort(L, #w{unique = true, keypos = 0}=W) ->
    rev(lists:usort(L), W);
internal_sort(L, #w{unique = false, keypos = 0}=W) ->
    rev(lists:sort(L), W);
internal_sort(L, #w{unique = true}=W) ->
    rev(lists:ukeysort(1, lists:reverse(L)), W);
internal_sort(L, #w{unique = false}=W) ->
    rev(lists:keysort(1, lists:reverse(L)), W).

rev(L, #w{order = ascending}) ->
    lists:reverse(L);
rev(L, #w{order = descending}) ->
    L.

last_merge(R, W) when length(R) =< W#w.no_files ->
    case W#w.out of
        Fun when is_function(Fun) ->
            {Fs, W1} = init_merge(lists:reverse(R), 1, [], W),
            ?DEBUG("merging ~tp~n", [lists:reverse(R)]),
            W2 = merge_files(Fs, [], 0, nolast, W1),
            NW = close_input(W2),
            outfun(close, NW);
        Out ->
            _ = merge_files(R, W, Out),
            ok
    end;
last_merge(R, W) ->
    L = lists:sublist(R, W#w.no_files),
    {M, NW} = merge_files(L, W),
    last_merge([M | lists:nthtail(W#w.no_files, R)], NW).

merge_runs([R | Rs], NRs0, W) when length(R) < W#w.no_files ->
    NRs = lists:reverse(NRs0) ++ [R | Rs],
    W#w{runs = NRs};    
merge_runs([R], NRs0, W) ->
    {M, NW} = merge_files(R, W),
    NRs = [[] | lists:reverse([[M] | NRs0])],
    NW#w{runs = NRs};    
merge_runs([R, R1 | Rs], NRs0, W) ->
    {M, NW} = merge_files(R, W),
    merge_runs([[M | R1] | Rs], [[] | NRs0], NW).

merge_files(R, W) ->
    {W1, Temp} = next_temp(W),
    ?DEBUG("merging ~tp~nto ~tp~n", [lists:reverse(R), Temp]),
    {Temp, merge_files(R, W1, Temp)}.
    
merge_files(R, W, FileName) ->
    {Fs, W1} = init_merge(lists:reverse(R), 1, [], W),
    {Fd, W2} = open_file(FileName, W1),
    W3 = W2#w{wfd = {Fd, FileName}},
    W4 = merge_files(Fs, [], 0, nolast, W3),
    NW = W4#w{wfd = undefined},
    close_file(Fd, NW).

%% A file number, I, is used for making the merge phase stable.
init_merge([FN | FNs], I, Fs, W) ->
    IFun = case FN of
               _ when is_function(FN) ->
                   %% When and only when merge/2,3 or keymerge/3,4 was called.
                   FN;
               _ ->
                   read_fun(FN, fsort, W)
           end,
    W1 = W#w{temp = [IFun | lists:delete(FN, W#w.temp)]},
    case read_more(IFun, I, 0, W1) of
        {Ts, _LSz, NIFun, NW} ->
            InEtc = {I, NIFun},
            init_merge(FNs, I+1, [[Ts | InEtc] | Fs], NW);
        {eof, NW} -> % can only happen when merging files
            init_merge(FNs, I+1, Fs, NW)
    end;
init_merge([], _I, Fs0, #w{order = ascending}=W) ->
    {lists:sort(Fs0), W};
init_merge([], _I, Fs0, #w{order = descending}=W) ->
    {lists:reverse(lists:sort(Fs0)), W};
init_merge([], _I, Fs0, #w{order = Order}=W) when is_function(Order) ->
    {lists:sort(cfun_files(W#w.order), lists:reverse(Fs0)), W}.

cfun_files(CFun) ->
    fun(F1, F2) ->
            [[?OBJ(T1,_) | _] | _] = F1,
            [[?OBJ(T2,_) | _] | _] = F2,
            CFun(T1, T2)
    end.

%% The argument Last is used when unique = true. It is the last kept
%% element.
%% LSz is not the sum of the sizes of objects in L. Instead it is
%% the number of bytes read. After init_merge it is set to 0, which
%% means that the first chunk written may be quite large (it may take
%% a while before buffers are exhausted).
merge_files([F1, F2 | Fs], L0, LSz, Last0, W) when LSz < ?MERGESIZE ->
    [Ts0 | InEtc] = F1,
    Kind = merge_kind(W),
    {Last, L, Ts} = case {Last0, Kind} of
                        {{last, Lst}, Kind} -> 
                            {Lst, L0, Ts0};
                        {nolast, {ukmerge, _Kp}} -> 
                            [?OBJ(?SK(T, _I), BT) | Ts1] = Ts0,
                            {T, [BT], Ts1};
                        {nolast, {rukmerge, _Kp}} -> 
                            [?OBJ(?SK(T, _I), BT) | Ts1] = Ts0,
                            {{T, BT}, [], Ts1};
                        {nolast, _} ->
                            [?OBJ(T, BT) | Ts1] = Ts0,
                            {T, [BT], Ts1}
                    end,
    [[?OBJ(T2, BT2) | Ts2T] = Ts2 | InEtc2] = F2,
    {NInEtc, NFs, NL, NLast} = 
       case Kind of 
           umerge ->
               umerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Last);
           {ukmerge, Kp} ->
               ukmerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Kp, Last);
           merge ->
               merge_files(L, F2, Fs, InEtc2, BT2, Ts2T, Ts, InEtc, T2);
           rumerge ->
               rumerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Last);
           {rukmerge, Kp} ->
               {Lt, LtBT} = Last,
               rukmerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Kp, 
                              Lt, LtBT);
           rmerge ->
               rmerge_files(L, F2, Fs, InEtc2, BT2, Ts2T, Ts, InEtc, T2);
           {ucmerge, CF} ->
               {I2, _} = InEtc2,
               {I, _} = InEtc,
               ucmerge_files(L, F2, Fs, InEtc2, Ts2, I2, Ts, I, InEtc, T2, CF,
                             Last);
           {cmerge, CF} ->
               {I2, _} = InEtc2,
               {I, _} = InEtc,
               cmerge_files(L, F2, Fs, InEtc2, BT2, Ts2T, I2, Ts, I, InEtc, T2,
                            CF)
       end,
    read_chunk(NInEtc, NFs, NL, LSz, NLast, W);
merge_files([F1], L, LSz, Last, W) when LSz < ?MERGESIZE ->
    [Ts | InEtc] = F1,
    NL = last_file(Ts, L, Last, merge_kind(W), W),
    read_chunk(InEtc, [], NL, LSz, nolast, W);
merge_files([], [], 0, nolast, W) ->
    %% When merging files, ensure that the output fun (if there is
    %% one) is called at least once before closing.
    merge_write(W, []);
merge_files([], L, _LSz, Last, W) ->
    Last = nolast,
    merge_write(W, L);
merge_files(Fs, L, _LSz, Last, W) ->
    NW = merge_write(W, L),
    merge_files(Fs, [], 0, Last, NW).

merge_kind(#w{order = ascending, unique = true, keypos = 0}) ->
    umerge;
merge_kind(#w{order = ascending, unique = true, keypos = Kp}) ->
    {ukmerge, Kp};
merge_kind(#w{order = ascending, unique = false}) ->
    merge;
merge_kind(#w{order = descending, unique = true, keypos = 0}) ->
    rumerge;
merge_kind(#w{order = descending, unique = true, keypos = Kp}) ->
    {rukmerge, Kp};
merge_kind(#w{order = descending, unique = false}) ->
    rmerge;
merge_kind(#w{order = CF, unique = true}) ->
    {ucmerge, CF};
merge_kind(#w{order = CF, unique = false}) ->
    {cmerge, CF}.

merge_write(W, L) ->
     case {W#w.wfd, W#w.out} of
         {undefined, Fun} when is_function(Fun) ->
             outfun(objects(L, []), W);
         {{Fd, FileName}, _} ->
             write(Fd, FileName, lists:reverse(L), W),
             W
     end.

umerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(T, _BT) | Ts], InEtc, T2, Last) 
            when T == Last ->
    umerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Last);
umerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(T, BT) | Ts], InEtc, T2, _Last) 
            when T =< T2 ->
    umerge_files([BT | L], F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, T);
umerge_files(L, F2, Fs, _InEtc2, _Ts2, [], InEtc, _T2, Last) ->
    {InEtc, [F2 | Fs], L, {last, Last}};
umerge_files(L, _F2, Fs, InEtc2, Ts2, Ts, InEtc, _T2, Last) ->
    [F3 | NFs] = insert([Ts | InEtc], Fs),
    [[?OBJ(T3,_BT3) | _] = Ts3 | InEtc3] = F3,
    umerge_files(L, F3, NFs, InEtc3, Ts3, Ts2, InEtc2, T3, Last).

rumerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(T, _BT) | Ts], InEtc, T2, Last) 
            when T == Last ->
    rumerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Last);
rumerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(T, BT) | Ts], InEtc, T2, _Last) 
            when T >= T2 ->
    rumerge_files([BT | L], F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, T);
rumerge_files(L, F2, Fs, _InEtc2, _Ts2, [], InEtc, _T2, Last) ->
    {InEtc, [F2 | Fs], L, {last, Last}};
rumerge_files(L, _F2, Fs, InEtc2, Ts2, Ts, InEtc, _T2, Last) ->
    [F3 | NFs] = rinsert([Ts | InEtc], Fs),
    [[?OBJ(T3,_BT3) | _] = Ts3 | InEtc3] = F3,
    rumerge_files(L, F3, NFs, InEtc3, Ts3, Ts2, InEtc2, T3, Last).

merge_files(L, F2, Fs, InEtc2, BT2, Ts2, [?OBJ(T, BT) | Ts], InEtc, T2) 
            when T =< T2 ->
    merge_files([BT | L], F2, Fs, InEtc2, BT2, Ts2, Ts, InEtc, T2);
merge_files(L, F2, Fs, _InEtc2, _BT2, _Ts2, [], InEtc, _T2) ->
    {InEtc, [F2 | Fs], L, {last, foo}};
merge_files(L, _F2, Fs, InEtc2, BT2, Ts2, Ts, InEtc, _T2) ->
    L1 = [BT2 | L],
    [F3 | NFs] = insert([Ts | InEtc], Fs),
    [[?OBJ(T3,BT3) | Ts3] | InEtc3] = F3,
    merge_files(L1, F3, NFs, InEtc3, BT3, Ts3, Ts2, InEtc2, T3).

rmerge_files(L, F2, Fs, InEtc2, BT2, Ts2, [?OBJ(T, BT) | Ts], InEtc, T2) 
            when T >= T2 ->
    rmerge_files([BT | L], F2, Fs, InEtc2, BT2, Ts2, Ts, InEtc, T2);
rmerge_files(L, F2, Fs, _InEtc2, _BT2, _Ts2, [], InEtc, _T2) ->
    {InEtc, [F2 | Fs], L, {last, foo}};
rmerge_files(L, _F2, Fs, InEtc2, BT2, Ts2, Ts, InEtc, _T2) ->
    L1 = [BT2 | L],
    [F3 | NFs] = rinsert([Ts | InEtc], Fs),
    [[?OBJ(T3,BT3) | Ts3] | InEtc3] = F3,
    rmerge_files(L1, F3, NFs, InEtc3, BT3, Ts3, Ts2, InEtc2, T3).

ukmerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(?SK(T, _I),_BT) | Ts], InEtc, 
              T2, Kp, Last) when T == Last ->
    ukmerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Kp, Last);
ukmerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(?SK(T0,_I)=T,BT) | Ts], InEtc, 
              T2, Kp, _Last) when T =< T2 ->
    ukmerge_files([BT | L], F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Kp, T0);
ukmerge_files(L, F2, Fs, _InEtc2, _Ts2, [], InEtc, _T2, _Kp, Last) ->
    {InEtc, [F2 | Fs], L, {last, Last}};
ukmerge_files(L, _F2, Fs, InEtc2, Ts2, Ts, InEtc, _T2, Kp, Last) ->
    [F3 | NFs] = insert([Ts | InEtc], Fs),
    [[?OBJ(T3,_BT3) | _] = Ts3 | InEtc3] = F3,
    ukmerge_files(L, F3, NFs, InEtc3, Ts3, Ts2, InEtc2, T3, Kp, Last).

rukmerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(?SK(T, _I), BT) | Ts], InEtc, 
               T2, Kp, Last, _LastBT) when T == Last ->
    rukmerge_files(L, F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Kp, T, BT);
rukmerge_files(L, F2, Fs, InEtc2, Ts2, [?OBJ(?SK(T0, _I)=T, BT) | Ts], InEtc, 
               T2, Kp, _Last, LastBT) when T >= T2 ->
    rukmerge_files([LastBT|L], F2, Fs, InEtc2, Ts2, Ts, InEtc, T2, Kp, T0,BT);
rukmerge_files(L, F2, Fs, _InEtc2, _Ts2, [], InEtc, _T2, _Kp, Last, LastBT) ->
    {InEtc, [F2 | Fs], L, {last, {Last, LastBT}}};
rukmerge_files(L, _F2, Fs, InEtc2, Ts2, Ts, InEtc, _T2, Kp, Last, LastBT) ->
    [F3 | NFs] = rinsert([Ts | InEtc], Fs),
    [[?OBJ(T3,_BT3) | _] = Ts3 | InEtc3] = F3,
    rukmerge_files(L, F3, NFs, InEtc3, Ts3, Ts2, InEtc2, T3, Kp, Last,LastBT).

ucmerge_files(L, F2, Fs, InEtc2, Ts2, I2, [?OBJ(T, BT) | Ts] = Ts0, I,
              InEtc, T2, CF, Last) when I < I2 ->
    case CF(T, T2) of
        true -> % T =< T2
            case CF(T, Last) of
                true ->
                    ucmerge_files(L, F2, Fs, InEtc2, Ts2, I2, Ts, I, InEtc, T2,
                                  CF, Last);
                false ->
                    ucmerge_files([BT | L], F2, Fs, InEtc2, Ts2, I2, Ts, I,
                                  InEtc, T2, CF, T)
            end;
       false -> % T > T2
          [F3 | NFs] = cinsert([Ts0 | InEtc], Fs, CF),
          [[?OBJ(T3,_BT3) | _] = Ts3 | {I3,_} = InEtc3] = F3,
          ucmerge_files(L, F3, NFs, InEtc3, Ts3, I3, Ts2, I2, InEtc2, T3, CF, Last)
    end;
ucmerge_files(L, F2, Fs, InEtc2, Ts2, I2, [?OBJ(T, BT) | Ts] = Ts0, I,
              InEtc, T2, CF, Last) -> % when I2 < I
    case CF(T2, T) of
        true -> % T2 =< T
            [F3 | NFs] = cinsert([Ts0 | InEtc], Fs, CF),
            [[?OBJ(T3,_BT3) | _] = Ts3 | {I3,_} = InEtc3] = F3,
            ucmerge_files(L, F3, NFs, InEtc3, Ts3, I3, Ts2, I2, InEtc2, T3, 
                          CF, Last);
       false -> % T < T2
            case CF(T, Last) of
                true ->
                    ucmerge_files(L, F2, Fs, InEtc2, Ts2, I2, Ts, I, InEtc, T2,
                                  CF, Last);
                false ->
                    ucmerge_files([BT | L], F2, Fs, InEtc2, Ts2, I2, Ts, I,
                                  InEtc, T2, CF, T)
            end
    end;
ucmerge_files(L, F2, Fs, _InEtc2, _Ts2, _I2, [], _I, InEtc, _T2, _CF, Last) ->
    {InEtc, [F2 | Fs], L, {last, Last}}.

cmerge_files(L, F2, Fs, InEtc2, BT2, Ts2, I2, [?OBJ(T, BT) | Ts] = Ts0, I,
             InEtc, T2, CF) when I < I2 ->
    case CF(T, T2) of
       true -> % T =< T2
          cmerge_files([BT|L], F2, Fs, InEtc2, BT2, Ts2, I2, Ts, I, InEtc, T2, CF);
       false -> % T > T2
          L1 = [BT2 | L],
          [F3 | NFs] = cinsert([Ts0 | InEtc], Fs, CF),
          [[?OBJ(T3,BT3) | Ts3] | {I3,_} = InEtc3] = F3,
          cmerge_files(L1, F3, NFs, InEtc3, BT3, Ts3, I3, Ts2, I2, InEtc2, T3, CF)
    end;
cmerge_files(L, F2, Fs, InEtc2, BT2, Ts2, I2, [?OBJ(T, BT) | Ts] = Ts0, I,
             InEtc, T2, CF) -> % when I2 < I
    case CF(T2, T) of
       true -> % T2 =< T
          L1 = [BT2 | L],
          [F3 | NFs] = cinsert([Ts0 | InEtc], Fs, CF),
          [[?OBJ(T3,BT3) | Ts3] | {I3,_} = InEtc3] = F3,
          cmerge_files(L1, F3, NFs, InEtc3, BT3, Ts3, I3, Ts2, I2, InEtc2, T3, CF);
       false -> % T < T2
          cmerge_files([BT|L], F2, Fs, InEtc2, BT2, Ts2, I2, Ts, I, InEtc, T2, CF)
    end;
cmerge_files(L, F2, Fs, _InEtc2, _BT2, _Ts2, _I2, [], _I, InEtc, _T2, _CF) ->
    {InEtc, [F2 | Fs], L, {last, foo}}.

last_file(Ts, L, {last, T}, {ukmerge,_}, _W) ->
    kulast_file(Ts, T, L);
last_file(Ts, L, {last, {T,BT}}, {rukmerge,_}, _W) ->
    ruklast_file(Ts, T, BT, L);
last_file(Ts, L, {last, T}, {ucmerge,CF}, _W) ->
    uclast_file(Ts, T, CF, L);
last_file(Ts, L, {last, T}, _Kind, #w{unique = true}) ->
    ulast_file(Ts, T, L);
last_file(Ts, L, _Last, _Kind, _W) ->
    last_file(Ts, L).

ulast_file([?OBJ(T, _BT) | Ts], Last, L) when Last == T ->
    last_file(Ts, L);
ulast_file(Ts, _Last, L) ->
    last_file(Ts, L).
    
kulast_file([?OBJ(?SK(T, _I), _BT) | Ts], Last, L) when Last == T ->
    last_file(Ts, L);
kulast_file(Ts, _Last, L) ->
    last_file(Ts, L).

ruklast_file([?OBJ(?SK(T, _I), BT) | Ts], Last, _LastBT, L) when Last == T ->
    last_file(Ts, [BT | L]);
ruklast_file(Ts, _Last, LastBT, L) ->
    last_file(Ts, [LastBT | L]).

uclast_file([?OBJ(T, BT) | Ts], Last, CF, L) ->
    case CF(T, Last) of
        true ->
            last_file(Ts, L);
        false ->
            last_file(Ts, [BT | L])
    end.

last_file([?OBJ(_Ta, BTa), ?OBJ(_Tb, BTb) | Ts], L) ->
    last_file(Ts, [BTb, BTa | L]);
last_file([?OBJ(_T, BT) | Ts], L) ->
    last_file(Ts, [BT | L]);
last_file([], L) ->
    L.
    
%% OK for 16 files.
insert(A, [X1, X2, X3, X4 | Xs]) when A > X4 ->
    [X1, X2, X3, X4 | insert(A, Xs)];
insert(A, [X1, X2, X3 | T]) when A > X3 ->
    [X1, X2, X3, A | T];
insert(A, [X1, X2 | Xs]) when A > X2 ->
    [X1, X2, A | Xs];
insert(A, [X1 | T]) when A > X1 ->
    [X1, A | T];
insert(A, Xs) ->
    [A | Xs].

rinsert(A, [X1, X2, X3, X4 | Xs]) when A < X4 ->
    [X1, X2, X3, X4 | rinsert(A, Xs)];
rinsert(A, [X1, X2, X3 | T]) when A < X3 ->
    [X1, X2, X3, A | T];
rinsert(A, [X1, X2 | Xs]) when A < X2 ->
    [X1, X2, A | Xs];
rinsert(A, [X1 | T]) when A < X1 ->
    [X1, A | T];
rinsert(A, Xs) ->
    [A | Xs].

-define(CINSERT(F, A, T1, T2),
        case cfun(CF, F, A) of
            true -> [F, A | T2];
            false -> [A | T1]
        end).

cinsert(A, [F1 | [F2 | [F3 | [F4 | Fs]=T4]=T3]=T2]=T1, CF) ->
    case cfun(CF, F4, A) of
        true -> [F1, F2, F3, F4 | cinsert(A, Fs, CF)];
        false -> 
            case cfun(CF, F2, A) of
                true -> [F1, F2 | ?CINSERT(F3, A, T3, T4)];
                false -> ?CINSERT(F1, A, T1, T2)
            end
    end;
cinsert(A, [F1 | [F2 | Fs]=T2]=T1, CF) ->
    case cfun(CF, F2, A) of
        true -> [F1, F2 | cinsert(A, Fs, CF)];
        false -> ?CINSERT(F1, A, T1, T2)
    end;
cinsert(A, [F | Fs]=T, CF) ->
    ?CINSERT(F, A, T, Fs);
cinsert(A, _, _CF) ->
    [A].

%% Inlined.
cfun(CF, F1, F2) ->
    [[?OBJ(T1,_) | _] | {I1,_}] = F1,
    [[?OBJ(T2,_) | _] | {I2,_}] = F2,
    if 
        I1 < I2 ->
            CF(T1, T2);
        true -> % I2 < I1
            not CF(T2, T1)
    end.

binterm_objects([?OBJ(_T, [_Sz | BT]) | Ts], L) ->
    binterm_objects(Ts, [BT | L]);
binterm_objects([], L) ->
    L.

objects([[_Sz | BT] | Ts], L) ->
    objects(Ts, [BT | L]);
objects([], L) ->
    L.

binterms([?OBJ(_T1, BT1), ?OBJ(_T2, BT2) | Ts], L) ->
    binterms(Ts, [BT2, BT1 | L]);
binterms([?OBJ(_T, BT) | Ts], L) ->
    binterms(Ts, [BT | L]);
binterms([], L) ->
    L.

read_chunk(InEtc, Fs, L, LSz, Last, W) ->
    {I, IFun} = InEtc,
    case read_more(IFun, I, LSz, W) of
        {Ts, NLSz, NIFun, #w{order = ascending}=NW} ->
            NInEtc = {I, NIFun},
            NFs = insert([Ts | NInEtc], Fs),
            merge_files(NFs, L, NLSz, Last, NW);
        {Ts, NLSz, NIFun, #w{order = descending}=NW} ->
            NInEtc = {I, NIFun},
            NFs = rinsert([Ts | NInEtc], Fs),
            merge_files(NFs, L, NLSz, Last, NW);
        {Ts, NLSz, NIFun, NW} ->
            NInEtc = {I, NIFun},
            NFs = cinsert([Ts | NInEtc], Fs, NW#w.order),
            merge_files(NFs, L, NLSz, Last, NW);
        {eof, NW} ->
            merge_files(Fs, L, LSz, Last, NW)
    end.

%% -> {[{term() | binary()}], NewLSz, NewIFun, NewW} | eof | throw(Error)
read_more(IFun, I, LSz, W) ->
    case IFun({{merge, I}, [], LSz, W}) of
        {{_, [], NLSz}, NIFun} ->
            read_more(NIFun, I, NLSz, W);
        {{_, L, NLSz}, NInFun} ->
            NW = case lists:member(IFun, W#w.temp) of
                     true ->
                         %% temporary file
                         W#w{temp = [NInFun | lists:delete(IFun, W#w.temp)]};
                     false ->
                         %% input file
                         W
                 end,
            {lists:reverse(L), NLSz, NInFun, NW};
        eof ->
            %% already closed.
            NW = W#w{temp = lists:delete(IFun, W#w.temp)},
            {eof, NW}
    end.

read_fun(FileName, Owner, W) ->
    case file:open(FileName, [raw, binary, read, compressed]) of
        {ok, Fd} ->
            read_fun2(Fd, <<>>, 0, FileName, Owner);
        Error ->
            file_error(FileName, Error, W)
    end.

read_fun2(Fd, Bin, Size, FileName, Owner) ->
    fun(close) ->
            close_read_fun(Fd, FileName, Owner);
       ({I, L, LSz, W}) ->
            case read_objs(Fd, FileName, I, L, Bin, Size, LSz, W) of
                {{I1, L1, Bin1, Size1}, LSz1} ->
                    NIFun = read_fun2(Fd, Bin1, Size1, FileName, Owner),
                    {{I1, L1, LSz1}, NIFun};
                eof ->
                    close_read_fun(Fd, FileName, Owner),
                    eof
            end
    end.
       
close_read_fun(Fd, _FileName, user) ->
    _ = file:close(Fd),
    ok;
close_read_fun(Fd, FileName, fsort) ->
    _ = file:close(Fd),
    _ = file:delete(FileName),
    ok.

read_objs(Fd, FileName, I, L, Bin0, Size0, LSz, W) ->
    Max = erlang:max(Size0, ?CHUNKSIZE),
    BSz0 = byte_size(Bin0),
    Min = Size0 - BSz0 + W#w.hdlen, % Min > 0
    NoBytes = erlang:max(Min, Max),
    case read(Fd, FileName, NoBytes, W) of
        {ok, Bin} ->
            BSz = byte_size(Bin),
            NLSz = LSz + BSz,
            case catch file_loop(L, I, Bin0, Bin, Size0, BSz0, BSz, Min, W)
                of
                {'EXIT', _R} ->
                    error({error, {bad_object, FileName}}, W);
                Reply ->
                    {Reply, NLSz}
            end;
        eof when byte_size(Bin0) =:= 0 ->
            eof;
        eof ->
            error({error, {premature_eof, FileName}}, W)
    end.            

file_loop(L, I, _B1, B2, Sz, 0, _B2Sz, _Min, W) ->
    file_loop(L, I, B2, Sz, W);
file_loop(L, I, B1, B2, Sz, _B1Sz, B2Sz, Min, W) when B2Sz > Min ->
    {B3, B4} = split_binary(B2, Min),
    {I1, L1, <<>>, Sz1} = file_loop(L, I, list_to_binary([B1, B3]), Sz, W),
    file_loop(L1, I1, B4, Sz1, W);
file_loop(L, I, B1, B2, Sz, _B1Sz, _B2Sz, _Min, W) ->
    file_loop(L, I, list_to_binary([B1, B2]), Sz, W).

file_loop(L, I, B, Sz, W) ->
    #w{keypos = Kp, format = Format, hdlen = HdLen} = W,
    file_loop1(L, I, B, Sz, Kp, Format, HdLen).

file_loop1(L, I, HB, 0, Kp, F, HdLen) ->
    <<Size:HdLen/unit:8, B/binary>> = HB,
    file_loop2(L, I, B, Size, <<Size:HdLen/unit:8>>, Kp, F, HdLen);
file_loop1(L, I, B, Sz, Kp, F, HdLen) ->
    file_loop2(L, I, B, Sz, <<Sz:HdLen/unit:8>>, Kp, F, HdLen).

file_loop2(L, _I, B, Sz, SzB, 0, binary, HdLen) ->
    {NL, NB, NSz, NSzB} = file_binloop(L, Sz, SzB, B, HdLen),
    if 
        byte_size(NB) =:= NSz ->
            <<Bin:NSz/binary>> = NB,
            {0, [?OBJ(Bin, [NSzB | Bin]) | NL], <<>>, 0};
        true ->
            {0, NL, NB, NSz}
    end;
file_loop2(L, _I, B, Sz, SzB, 0, Fun, HdLen) ->
    file_binterm_loop(L, Sz, SzB, B, Fun, HdLen);
file_loop2(L, {merge, I}, B, Sz, SzB, Kp, Fun, HdLen) -> % when Kp =/= 0
    merge_loop(Kp, I, L, Sz, SzB, B, Fun, HdLen);
file_loop2(L, I, B, Sz, SzB, Kp, Fun, HdLen) when is_integer(I) ->
    key_loop(Kp, I, L, Sz, SzB, B, Fun, HdLen).

file_binloop(L, Size, SizeB, B, HL) ->
    case B of
        <<Bin:Size/binary, NSizeB:HL/binary, R/binary>> ->
            <<NSize:HL/unit:8>> = NSizeB,
            file_binloop([?OBJ(Bin, [SizeB | Bin]) | L], NSize, NSizeB, R, HL);
        _ ->
            {L, B, Size, SizeB}
    end.

file_binterm_loop(L, Size, SizeB, B, Fun, HL) ->
    case B of
        <<BinTerm:Size/binary, NSizeB:HL/binary, R/binary>> ->
            <<NSize:HL/unit:8>> = NSizeB,
            BT = [SizeB | BinTerm],
            Term = Fun(BinTerm),
            file_binterm_loop([?OBJ(Term, BT) | L], NSize, NSizeB, R, Fun, HL);
        <<BinTerm:Size/binary>> ->
            Term = Fun(BinTerm),
            NL = [?OBJ(Term, [SizeB | BinTerm]) | L], 
            {0, NL, <<>>, 0};
        _ ->
            {0, L, B, Size}
    end.

key_loop(KeyPos, I, L, Size, SizeB, B, Fun, HL) ->
    case B of
        <<BinTerm:Size/binary, NSizeB:HL/binary, R/binary>> ->
            <<NSize:HL/unit:8>> = NSizeB,
            BT = [SizeB | BinTerm],
            UniqueKey = make_key(KeyPos, Fun(BinTerm)),
            E = ?OBJ(UniqueKey, BT),
            key_loop(KeyPos, I+1, [E | L], NSize, NSizeB, R, Fun, HL);
        <<BinTerm:Size/binary>> ->
            UniqueKey = make_key(KeyPos, Fun(BinTerm)),
            NL = [?OBJ(UniqueKey, [SizeB | BinTerm]) | L], 
            {I+1, NL, <<>>, 0};
        _ ->
            {I, L, B, Size}
    end.

merge_loop(KeyPos, I, L, Size, SizeB, B, Fun, HL) ->
    case B of
        <<BinTerm:Size/binary, NSizeB:HL/binary, R/binary>> ->
            <<NSize:HL/unit:8>> = NSizeB,
            BT = [SizeB | BinTerm],
            UniqueKey = make_stable_key(KeyPos, I, Fun(BinTerm)),
            E = ?OBJ(UniqueKey, BT),
            merge_loop(KeyPos, I, [E | L], NSize, NSizeB, R, Fun, HL);
        <<BinTerm:Size/binary>> ->
            UniqueKey = make_stable_key(KeyPos, I, Fun(BinTerm)),
            NL = [?OBJ(UniqueKey, [SizeB | BinTerm]) | L], 
            {{merge, I}, NL, <<>>, 0};
        _ ->
            {{merge, I}, L, B, Size}
    end.

fun_objs(Objs, L, LSz, NoBytes, I, W) ->
    #w{keypos = Keypos, format = Format, hdlen = HL} = W,
    case catch fun_loop(Objs, L, LSz, NoBytes, I, Keypos, Format, HL) of
        {'EXIT', _R} ->
            error({error, bad_object}, W);
        Reply ->
            Reply
    end.

fun_loop(Objs, L, LSz, RunSize, _I, 0, binary, HdLen) ->
    fun_binloop(Objs, L, LSz, RunSize, HdLen);
fun_loop(Objs, L, LSz, RunSize, _I, 0, Fun, HdLen) ->
    fun_loop(Objs, L, LSz, RunSize, Fun, HdLen);
fun_loop(Objs, L, LSz, RunSize, {merge, I}, Keypos, Fun, HdLen) ->
    fun_mergeloop(Objs, L, LSz, RunSize, I, Keypos, Fun, HdLen);
fun_loop(Objs, L, LSz, RunSize, I, Keypos, Fun, HdLen) when is_integer(I) ->
    fun_keyloop(Objs, L, LSz, RunSize, I, Keypos, Fun, HdLen).

fun_binloop([B | Bs], L, LSz, RunSize, HL) when LSz < RunSize ->
    Size = byte_size(B),
    Obj = ?OBJ(B, [<<Size:HL/unit:8>> | B]),
    fun_binloop(Bs, [Obj | L], LSz+Size, RunSize, HL);
fun_binloop(Bs, L, LSz, _RunSize, _HL) ->
    {0, Bs, L, LSz}.

fun_loop([B | Bs], L, LSz, RunSize, Fun, HL) when LSz < RunSize ->
    Size = byte_size(B),
    Obj = ?OBJ(Fun(B), [<<Size:HL/unit:8>> | B]),
    fun_loop(Bs, [Obj | L], LSz+Size, RunSize, Fun, HL);
fun_loop(Bs, L, LSz, _RunSize, _Fun, _HL) ->
    {0, Bs, L, LSz}.

fun_keyloop([B | Bs], L, LSz, RunSize, I, Kp, Fun, HL) when LSz < RunSize ->
    Size = byte_size(B),
    UniqueKey = make_key(Kp, Fun(B)),
    E = ?OBJ(UniqueKey, [<<Size:HL/unit:8>> | B]),
    fun_keyloop(Bs, [E | L], LSz+Size, RunSize, I+1, Kp, Fun, HL);
fun_keyloop(Bs, L, LSz, _RunSize, I, _Kp, _Fun, _HL) ->
    {I, Bs, L, LSz}.

fun_mergeloop([B | Bs], L, LSz, RunSize, I, Kp, Fun, HL) when LSz < RunSize ->
    Size = byte_size(B),
    UniqueKey = make_stable_key(Kp, I, Fun(B)),
    E = ?OBJ(UniqueKey, [<<Size:HL/unit:8>> | B]),
    fun_mergeloop(Bs, [E | L], LSz+Size, RunSize, I, Kp, Fun, HL);
fun_mergeloop(Bs, L, LSz, _RunSize, I, _Kp, _Fun, _HL) ->
    {{merge, I}, Bs, L, LSz}. % any I would do

%% Inlined.
make_key(Kp, T) when is_integer(Kp) ->
    element(Kp, T);
make_key([Kp1, Kp2], T) ->
    [element(Kp1, T), element(Kp2, T)];
make_key([Kp1, Kp2 | Kps], T) ->
    [element(Kp1, T), element(Kp2, T) | make_key2(Kps, T)].

%% Inlined.
%% A sequence number (I) is used for making the internal sort stable.
%% I is ordering number of the file from which T was read.
make_stable_key(Kp, I, T) when is_integer(Kp) ->
    ?SK(element(Kp, T), I);
make_stable_key([Kp1, Kp2], I, T) ->
    ?SK([element(Kp1, T) | element(Kp2, T)], I);
make_stable_key([Kp1, Kp2 | Kps], I, T) ->
    ?SK([element(Kp1, T), element(Kp2, T) | make_key2(Kps, T)], I).

make_key2([Kp], T) ->
    [element(Kp, T)];
make_key2([Kp | Kps], T) ->
    [element(Kp, T) | make_key2(Kps, T)].

infun(W) ->
    W1 = W#w{in = undefined},
    try (W#w.in)(read) of
        end_of_input ->
            {end_of_input, W1};
        {end_of_input, Value} ->
            {end_of_input, W1#w{inout_value = {value, Value}}};
        {Objs, NFun} when is_function(NFun, 1),
                          is_list(Objs) ->
            {cont, W#w{in = NFun}, Objs};
        Error ->
            error(Error, W1)
    catch Class:Reason:Stacktrace ->
        cleanup(W1),
        erlang:raise(Class, Reason, Stacktrace)
    end.

outfun(A, #w{inout_value = Val} = W) when Val =/= no_value ->
    W1 = W#w{inout_value = no_value},
    W2 = if 
             W1#w.fun_out ->
                 outfun(Val, W1);
             true -> W1
         end,
    outfun(A, W2);
outfun(A, W) ->
    W1 = W#w{out = undefined},
    try (W#w.out)(A) of
        Reply when A =:= close ->
            Reply;
        NF when is_function(NF, 1) ->
            W#w{out = NF};
        Error ->
            error(Error, W1)
    catch Class:Reason:Stacktrace ->
        cleanup(W1),
        erlang:raise(Class, Reason, Stacktrace)
    end.

is_keypos(Keypos) when is_integer(Keypos), Keypos > 0 ->
    true;
is_keypos([]) ->
    {badarg, []};
is_keypos(L) ->
    is_keyposs(L).

is_keyposs([Kp | Kps]) when is_integer(Kp), Kp > 0 ->
    is_keyposs(Kps);
is_keyposs([]) ->
    true;
is_keyposs([Bad | _]) ->
    {badarg, Bad};
is_keyposs(Bad) ->
    {badarg, Bad}.

is_input(Fun) when is_function(Fun, 1) ->
    {true, Fun};
is_input(Files) ->
    is_files(Files).

is_files(Fs) ->
    is_files(Fs, []).

is_files([F | Fs], L) ->
    case read_file_info(F) of
        {ok, File, _FI} ->
            is_files(Fs, [File | L]);
        Error ->
            Error
    end;
is_files([], L) ->
    {true, lists:reverse(L)};
is_files(Bad, _L) ->
    {badarg, Bad}.

maybe_output(Fun) when is_function(Fun, 1) ->
    {true, Fun};
maybe_output(File) ->
    case read_file_info(File) of
        {badarg, _File} = Badarg ->
            Badarg;
        {ok, FileName, _FileInfo} ->
            {true, FileName};
        {error, {file_error, FileName, _Reason}} ->
            {true, FileName}
    end.

read_file_info(File) ->
    %% Absolute names in case some process should call file:set_cwd/1.
    case catch filename:absname(File) of
        {'EXIT', _} ->
            {badarg, File};
        FileName ->
            case file:read_file_info(FileName) of
                {ok, FileInfo} ->
                    {ok, FileName, FileInfo};
                {error, einval} ->
                    {badarg, File};
                {error, Reason} ->
                    {error, {file_error, FileName, Reason}}
            end
    end.

%% No attempt is made to avoid overwriting existing files.
next_temp(W) ->
    Seq = W#w.seq,
    NW = W#w{seq = Seq + 1},
    Temp = lists:concat([W#w.prefix, Seq]),
    {NW, Temp}.

%% Would use the temporary directory (TMP|TEMP|TMPDIR), were it
%% readily accessible.
tmp_prefix(F, TmpDirOpt) when is_function(F); F =:= undefined ->
    {ok, CurDir} = file:get_cwd(),
    tmp_prefix1(CurDir, TmpDirOpt);
tmp_prefix(OutFile, TmpDirOpt) ->
    Dir = filename:dirname(OutFile),
    tmp_prefix1(Dir, TmpDirOpt).

tmp_prefix1(Dir, TmpDirOpt) ->
    U = "_",
    Node = node(),
    Pid = os:getpid(),
    Unique = erlang:unique_integer([positive]),
    F = lists:concat(["fs_",Node,U,Pid,U,Unique,"."]),
    TmpDir = case TmpDirOpt of
                 default ->
                     Dir;
                 {dir, TDir} ->
                     TDir
             end,
    filename:join(filename:absname(TmpDir), F).

%% -> {Fd, NewW} | throw(Error)
open_file(FileName, W) ->
    case file:open(FileName, W#w.z ++ [raw, binary, write]) of
        {ok, Fd}  ->
            {Fd, W#w{temp = [{Fd,FileName} | W#w.temp]}};
        Error ->
            file_error(FileName, Error, W)
    end.

read(Fd, FileName, N, W) ->
    case file:read(Fd, N) of
        {ok, Bin} ->
            {ok, Bin};
        eof ->
            eof;
        {error, enomem} ->
            %% Bad N
            error({error, {bad_object, FileName}}, W);
        {error, einval} ->
            %% Bad N
            error({error, {bad_object, FileName}}, W);
        Error ->
            file_error(FileName, Error, W)
    end.

write(Fd, FileName, B, W) ->
    case file:write(Fd, B) of
        ok ->
            ok;
        Error ->
            file_error(FileName, Error, W)
    end.

-spec file_error(_, {'error',atom()}, #w{}) -> no_return().

file_error(File, {error, Reason}, W) ->
    error({error, {file_error, File, Reason}}, W).

error(Error, W) ->
    cleanup(W),
    throw({W#w.ref, Error}).

cleanup(W) ->
    close_out(W),
    W1 = close_input(W),
    F = fun(IFun) when is_function(IFun) -> 
                IFun(close);
           ({Fd,FileName}) ->
                _ = file:close(Fd),
                _= file:delete(FileName);
           (FileName) -> 
                _= file:delete(FileName)
        end,
    lists:foreach(F, W1#w.temp).

close_input(#w{in = In}=W) when is_function(In) ->
    catch In(close),
    W#w{in = undefined};
close_input(#w{in = undefined}=W) ->
    W.

close_out(#w{out = Out}) when is_function(Out) ->
    catch Out(close);
close_out(_) -> 
    ok.

close_file(Fd, W) ->
    {Fd, FileName} = lists:keyfind(Fd, 1, W#w.temp),
    ?DEBUG("closing ~tp~n", [FileName]),
    case file:close(Fd) of
        ok ->
            W#w{temp = [FileName | lists:keydelete(Fd, 1, W#w.temp)]};
        Error ->
            file_error(FileName, Error, W)
    end.

%%%
%%% Format 'term'.
%%%

file_rterms(no_file, Files) ->
    fun(close) ->
            ok;
       (read) when Files =:= [] ->
            end_of_input;
       (read) ->
            [F | Fs] = Files,
            case file:open(F, [read, compressed]) of
                {ok, Fd} ->
                    file_rterms2(Fd, [], 0, F, Fs);
                {error, Reason} ->
                    {error, {file_error, F, Reason}}
            end
    end;
file_rterms({Fd, FileName}, Files) ->
    fun(close) ->
            file:close(Fd);
       (read) ->
            file_rterms2(Fd, [], 0, FileName, Files)
    end.

file_rterms2(Fd, L, LSz, FileName, Files) when LSz < ?CHUNKSIZE ->
    case io:read(Fd, '') of
        {ok, Term} ->
            B = term_to_binary(Term),
            file_rterms2(Fd, [B | L], LSz + byte_size(B), FileName, Files);
        eof ->
            _ = file:close(Fd),
            {lists:reverse(L), file_rterms(no_file, Files)};
        _Error ->
            _ = file:close(Fd),
            {error, {bad_term, FileName}}
    end;
file_rterms2(Fd, L, _LSz, FileName, Files) ->
    {lists:reverse(L), file_rterms({Fd, FileName}, Files)}.

file_wterms(W, F, Args) ->
    fun(close) when W =:= name ->
            ok;
       (close) ->
            {fd, Fd} = W,
            file:close(Fd);
       (L) when W =:= name ->
            case file:open(F, Args) of
                {ok, Fd} ->
                    write_terms(Fd, F, L, Args);
                {error, Reason} ->
                    {error, {file_error, F, Reason}}
            end;
       (L) ->
            {fd, Fd} = W,
            write_terms(Fd, F, L, Args)
    end.

write_terms(Fd, F, [B | Bs], Args) ->
    case io:request(Fd, {format, "~p.~n", [binary_to_term(B)]}) of
        ok -> 
            write_terms(Fd, F, Bs, Args);
        {error, Reason} ->
            _ = file:close(Fd),
            {error, {file_error, F, Reason}}
    end;
write_terms(Fd, F, [], Args) ->
    file_wterms({fd, Fd}, F, Args).

fun_rterms(InFun) ->
    fun(close) ->
            InFun(close);
       (read) ->
            case InFun(read) of
                {Ts, NInFun} when is_list(Ts), 
                                  is_function(NInFun, 1) ->
                    {to_bin(Ts, []), fun_rterms(NInFun)};
                Else ->
                    Else
            end
    end.

fun_wterms(OutFun) ->
    fun(close) ->
            OutFun(close);
       (L) ->
            case OutFun(wterms_arg(L)) of
                NOutFun when is_function(NOutFun, 1) ->
                    fun_wterms(NOutFun);
                Else ->
                    Else
            end
    end.

to_bin([E | Es], L) ->
    to_bin(Es, [term_to_binary(E) | L]);
to_bin([], L) ->
    lists:reverse(L).

wterms_arg(L) when is_list(L) ->
    to_term(L, []);
wterms_arg(Value) ->
    Value.

to_term([B | Bs], L) ->
    to_term(Bs, [binary_to_term(B) | L]);
to_term([], L) ->
    lists:reverse(L).
