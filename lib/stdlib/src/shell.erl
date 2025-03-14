%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(shell).
-moduledoc({file, "../doc/src/shell.md"}).

-compile(nowarn_deprecated_catch).

-export([start/0, start/1, start/2, server/1, server/2, history/1, results/1]).
-export([get_state/0, get_function/2]).
-export([start_restricted/1, stop_restricted/0]).
-export([local_func/0, local_func/1, local_allowed/3, non_local_allowed/3]).
-export([catch_exception/1, prompt_func/1, multiline_prompt_func/1, strings/1]).
-export([format_shell_func/1, erl_pp_format_func/1]).
-export([start_interactive/0, start_interactive/1]).
-export([read_and_add_records/5]).
-export([default_multiline_prompt/1, inverted_space_prompt/1]).
-export([prompt_width/1, prompt_width/2]).
-export([help/0,whereis/0]).

-define(LINEMAX, 30).
-define(CHAR_MAX, 60).
-define(DEF_HISTORY, 20).
-define(DEF_RESULTS, 20).
-define(DEF_CATCH_EXCEPTION, false).
-define(DEF_PROMPT_FUNC, default).
-define(DEF_STRINGS, true).

-define(RECORDS, shell_records).

-define(MAXSIZE_HEAPBINARY, 64).
-record(shell_state,{
                     bindings = [],
                     records = [],
                     functions = []
                    }).
%% When used as the fallback restricted shell callback module...
-doc false.
local_allowed(q,[],State) ->
    {true,State};
local_allowed(_,_,State) ->
    {false,State}.

-doc false.
non_local_allowed({init,stop},[],State) ->
    {true,State};
non_local_allowed(_,_,State) ->
    {false,State}.

-doc """
Starts the interactive shell if it has not already been started. It can be used
to programatically start the shell from an escript or when erl is started with
the -noinput or -noshell flags.

Calling this function will start a remote shell if `-remsh` is given on the
command line or a local shell if not.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec start_interactive() -> ok | {error, already_started}.
start_interactive() ->
    user_drv:start_shell().
-doc """
Starts the interactive shell if it has not already been started. It can be used
to programatically start the shell from an [`escript`](`e:erts:escript_cmd.md`)
or when [`erl`](`e:erts:erl_cmd.md`) is started with the
[`-noinput`](`e:erts:erl_cmd.md#noinput`) or
[`-noshell`](`e:erts:erl_cmd.md#noshell`) flags. The following options are
allowed:

- **noshell | {noshell, Mode}**{: #noshell_raw } - Starts the interactive shell
  as if [`-noshell`](`e:erts:erl_cmd.md#noshell`) was given to
  [`erl`](`e:erts:erl_cmd.md`).

  It is possible to give a `Mode` indicating if the input should be set
  in `cooked` or `raw` mode. `Mode` only has en effect if `t:io:user/0` is a tty.
  If no `Mode` is given, it defaults is `cooked`.

  When in `raw` mode all key presses are passed to `t:io:user/0` as they are
  typed when they are typed and the characters are not echoed to the terminal.
  It is possible to set the `echo` to `true` using `io:setopts/2` to enabling
  echoing again.

  When in `cooked` mode the OS will handle the line editing and all data is
  passed to `t:io:user/0` when a newline is entered.

- **[mfa()](`t:erlang:mfa/0`)** - Starts the interactive shell using
  [`mfa()`](`t:erlang:mfa/0`) as the default shell. The `t:mfa/0` should
  return the `t:pid/0` of the created shell process.

- **\{[node()](`t:erlang:node/0`), [mfa()](`t:erlang:mfa/0`)\}** - Starts the
  interactive shell using [`mfa()`](`t:erlang:mfa/0`) on
  [`node()`](`t:erlang:node/0`) as the default shell. The `t:mfa/0` should
  return the `t:pid/0` of the created shell process.

- **\{remote, [`string()`](`t:erlang:string/0`)\}** - Starts the interactive
  shell using as if [`-remsh`](`e:erts:erl_cmd.md#remsh`) was given to
  [`erl`](`e:erts:erl_cmd.md`).

- **\{remote, [`string()`](`t:erlang:string/0`),
  [`mfa()`](`t:erlang:mfa/0`)\}** - Starts the interactive shell using as if
  [`-remsh`](`e:erts:erl_cmd.md#remsh`) was given to
  [`erl`](`e:erts:erl_cmd.md`) but with an alternative shell implementation.

On error this function will return:

- **already_started** - if an interactive shell is already started.

- **noconnection** - if a remote shell was requested but it could not be
  connected to.

- **badfile | nofile | on_load_failure** - if a remote shell was requested with
  a custom [mfa()](`t:erlang:mfa/0`), but the module could not be loaded. See
  [Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
  description of the error reasons.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec start_interactive(noshell | {noshell, raw | cooked} | {module(), atom(), [term()]}) ->
          ok | {error, already_started};
                       ({remote, string()}) ->
          ok | {error, already_started | noconnection};
                       ({node(), {module(), atom(), [term()]}} | {remote, string(), {module(), atom(), [term()]}}) ->
          ok | {error, already_started | noconnection | badfile | nofile | on_load_failure}.
start_interactive({Node, {M, F, A}}) ->
    user_drv:start_shell(#{ initial_shell => {Node, M, F ,A} });
start_interactive(noshell) ->
    start_interactive({noshell, cooked});
start_interactive({noshell, Type}) when Type =:= raw; Type =:= cooked ->
    user_drv:start_shell(#{ initial_shell => noshell, input => Type });
start_interactive(InitialShell) ->
    user_drv:start_shell(#{ initial_shell => InitialShell }).

-doc """
Returns the current shell process on the node where the calling process'
group_leader is located. If that node has no shell this function will return
undefined.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec whereis() -> pid() | undefined.
whereis() ->
    group:whereis_shell().

-doc false.
-spec start() -> pid().

start() ->
    start(false, false).

-doc false.
start(init) ->
    start(false, true);
start(NoCtrlG) ->
    start(NoCtrlG, false).

-doc false.
start(NoCtrlG, StartSync) ->
    _ = code:ensure_loaded(user_default),
    Ancestors = [self() | case get('$ancestors') of
                              undefined -> [];
                              Anc -> Anc
                          end],
    spawn(fun() ->
                  put('$ancestors', Ancestors),
                  server(NoCtrlG, StartSync)
          end).

%% Call this function to start a user restricted shell
%% from a normal shell session.
-doc """
Exits a normal shell and starts a restricted shell. `Module` specifies the
callback module for the functions `local_allowed/3` and `non_local_allowed/3`.
The function is meant to be called from the shell.

If the callback module cannot be loaded, an error tuple is returned. The
`Reason` in the error tuple is the one returned by the code loader when trying
to load the code of the callback module.
""".
-spec start_restricted(Module) -> {'error', Reason} when
      Module :: module(),
      Reason :: code:load_error_rsn().

start_restricted(RShMod) when is_atom(RShMod) ->
    case code:ensure_loaded(RShMod) of
        {module,RShMod} ->
            application:set_env(stdlib, restricted_shell, RShMod),
            exit(restricted_shell_started);
        {error,What} = Error ->
            error_logger:error_report(
              lists:flatten(
                io_lib:fwrite(
                  "Restricted shell module ~w not found: ~tp\n",
                  [RShMod,What]))),
            Error
    end.

-doc """
Exits a restricted shell and starts a normal shell. The function is meant to be
called from the shell.
""".
-spec stop_restricted() -> no_return().

stop_restricted() ->
    application:unset_env(stdlib, restricted_shell),
    exit(restricted_shell_stopped).

-doc false.
-spec server(boolean(), boolean()) -> 'terminated'.

server(NoCtrlG, StartSync) ->
    put(no_control_g, NoCtrlG),
    server(StartSync).


%%% The shell should not start until the system is up and running.
%%% We subscribe with init to get a notification of when.

%%% In older releases we didn't syncronize the shell with init, but let it
%%% start in parallell with other system processes. This was bad since
%%% accessing the shell too early could interfere with the boot procedure.
%%% Still, by means of a flag, we make it possible to start the shell the
%%% old way (for backwards compatibility reasons). This should however not
%%% be used unless for very special reasons necessary.

-doc false.
-spec server(boolean()) -> 'terminated'.

server(StartSync) ->
    case init:get_argument(async_shell_start) of
        {ok,_} ->
            ok;                                 % no sync with init
        _ when not StartSync ->
            ok;
        _ ->
            case init:notify_when_started(self()) of
                started ->
                    ok;
                _ ->
                    init:wait_until_started()
            end
    end,

    %% We disable line history for all commands. We will explicitly enable
    %% it only for the commands that we want.
    _ = io:setopts([{line_history, false}]),

    %% Our spawner has fixed the process groups.
    Bs = erl_eval:new_bindings(),

    %% Use an Ets table for record definitions. It takes too long to
    %% send a huge term to and from the evaluator. Ets makes it
    %% possible to have thousands of record definitions.
    RT = ets:new(?RECORDS, [public,ordered_set]),
    _ = initiate_records(Bs, RT),
    process_flag(trap_exit, true),
    %% Store local definitions of functions, records and types in an ets table.
    %% {function_def, {M,F,A}},string()} i.e. "func(X) -> X."
    %% {function, {M,F,A}},fun()} i.e. rewrite of the function_def with the same MFA into a fun "fun(X)-> X end."
    %% {function_type_def, {M,F,A}},string()} i.e. "-spec func(X :: integer()) -> X."
    %% {{function_type, {M,F,A}}, attr_form()} i.e. The intermediate representation of the spec, used for type traversal in edlin_type_suggestion.erl
    %% {type_def, {M,F,A}},string()} i.e. "-type foo() :: bar() | baz()"
    %% {type, {M,F,A}}, attr_form()} i.e. The intermediate representation of the type, used for type traversal in edlin_type_suggestion.erl
    %% {record_def, {M,F,A}},string()} i.e. "-record(foo, {bar :: baz()})."
    %% The attr_form() of the record is saved in the RT table.
    FT = ets:new(user_functions, [public,ordered_set]),

    %% Check if we're in user restricted mode.
    RShErr =
        case application:get_env(stdlib, restricted_shell) of
            {ok,RShMod} when is_atom(RShMod) ->
                io:fwrite(<<"Restricted ">>, []),
                case code:ensure_loaded(RShMod) of
                    {module,RShMod} ->
                        undefined;
                    {error,What} ->
                        {RShMod,What}
                end;
            {ok, Term} ->
                {Term,not_an_atom};
            undefined ->
                undefined
        end,

    JCL =
        case get(no_control_g) of
            true -> " (type help(). for help)";
            _ -> " (press Ctrl+G to abort, type help(). for help)"
        end,
    DefaultSessionSlogan =
        io_lib:format(<<"Eshell V~s">>, [erlang:system_info(version)]),
    SessionSlogan =
        case application:get_env(stdlib, shell_session_slogan, DefaultSessionSlogan) of
            SloganFun when is_function(SloganFun, 0) ->
                SloganFun();
            Slogan ->
                Slogan
        end,
    try
        io:fwrite("~ts~ts\n",[unicode:characters_to_list(SessionSlogan),JCL])
    catch _:_ ->
            io:fwrite("Warning! The slogan \"~p\" could not be printed.\n",[SessionSlogan])
    end,
    erase(no_control_g),

    case RShErr of
        undefined ->
            ok;
        {RShMod2,What2} ->
            io:fwrite(
                ("Warning! Restricted shell module ~w not found: ~tp.\n"
                 "Only the commands q() and init:stop() will be allowed!\n"),
              [RShMod2,What2]),
            application:set_env(stdlib, restricted_shell, ?MODULE)
    end,

    {History,Results} = check_and_get_history_and_results(),
    server_loop(0, start_eval(Bs, RT, FT, []), Bs, RT, FT, [], History, Results).

server_loop(N0, Eval_0, Bs00, RT, FT, Ds00, History0, Results0) ->
    N = N0 + 1,
    {Eval_1,Bs0,Ds0,Prompt} = prompt(N, Eval_0, Bs00, RT, FT, Ds00),
    {Res,Eval0} = get_command(Prompt, Eval_1, Bs0, RT, FT, Ds0),

    case Res of
        {ok,Es0} ->
            case expand_hist(Es0, N) of
                {ok,Es} ->
                    {V,Eval,Bs,Ds} = shell_cmd(Es, Eval0, Bs0, RT, FT, Ds0, cmd),
                    {History,Results} = check_and_get_history_and_results(),
                    add_cmd(N, Es, V),
                    HB1 = del_cmd(command, N - History, N - History0, false),
                    HB = del_cmd(result, N - Results, N - Results0, HB1),
                    %% The following test makes sure that large binaries
                    %% (outside of the heap) are garbage collected as soon
                    %% as possible.
                    if
                        HB ->
                            garb(self());
                        Results < Results0 ->
                            garb(self());
                        true ->
                            ok
                    end,
                    server_loop(N, Eval, Bs, RT, FT, Ds, History, Results);
                {error,E} ->
                    fwrite_severity(benign, <<"~ts">>, [E]),
                    server_loop(N0, Eval0, Bs0, RT, FT, Ds0, History0, Results0)
            end;
        {error,{Location,Mod,What}} ->
            fwrite_severity(benign, <<"~s: ~ts">>,
                            [pos(Location), Mod:format_error(What)]),
            server_loop(N0, Eval0, Bs0, RT, FT, Ds0, History0, Results0);
        {error,terminated} ->                   %Io process terminated
            exit(Eval0, kill),
            terminated;
        {error,interrupted} ->                  %Io process interrupted us
            exit(Eval0, kill),
            {_,Eval,_,_} = shell_rep(Eval0, Bs0, RT, FT, Ds0),
            server_loop(N0, Eval, Bs0, RT, FT, Ds0, History0, Results0);
        {error,tokens} ->                       %Most probably character > 255
            fwrite_severity(benign, <<"~w: Invalid tokens.">>,
                            [N]),
            server_loop(N0, Eval0, Bs0, RT, FT, Ds0, History0, Results0);
        eof ->
            catch fwrite_severity(fatal, <<"Terminating erlang (~w)">>, [node()]),
            halt()
    end.

get_command(Prompt, Eval, Bs, RT, FT, Ds) ->
    Ancestors = [self() | get('$ancestors')],
    ResWordFun = fun erl_scan:reserved_word/1,
    Parse =
        fun() ->
                put('$ancestors', Ancestors),
                PreviousHistory = proplists:get_value(line_history, io:getopts()),
                [ok = io:setopts([{line_history, true}]) || PreviousHistory =/= undefined],
                Res = io:scan_erl_exprs(group_leader(), Prompt, {1,1},
                                        [text,{reserved_word_fun,ResWordFun}]),
                _ = [io:setopts([{line_history, PreviousHistory}]) || PreviousHistory =/= undefined],
                exit(
                  case Res of
                      {ok,Toks,_EndPos} ->
                          %% NOTE: we can handle function definitions, records and type declarations
                          %% but this cannot be handled by the function which only expects erl_parse:abstract_expressions()
                          %% for now just pattern match against those types and pass the string to shell local func.
                          case Toks of
                              [{'-', _}, {atom, _, Atom}|_] ->
                                  SpecialCase = fun(LocalFunc) ->
                                                        FakeLine = begin
                                                                       case erl_parse:parse_form(Toks) of
                                                                           {ok, Def} -> lists:flatten(escape_quotes(lists:flatten(erl_pp:form(Def))));
                                                                           E ->
                                                                            exit(E)
                                                                       end
                                                                   end,
                                                        {done, {ok, FakeResult, _}, _} = erl_scan:tokens(
                                                                                           [], atom_to_list(LocalFunc) ++ "(\""++FakeLine++"\").\n",
                                                                                           {1,1}, [text,{reserved_word_fun,fun erl_scan:reserved_word/1}]),
                                                        erl_eval:extended_parse_exprs(FakeResult)
                                                end,
                                  case Atom of
                                      record -> SpecialCase(rd);
                                      spec -> SpecialCase(ft);
                                      type -> SpecialCase(td);
                                      nominal -> SpecialCase(td);
                                      _ -> erl_eval:extended_parse_exprs(Toks)
                                  end;
                              [{atom, _, FunName}, {'(', _}|_] ->
                                  case erl_parse:parse_form(Toks) of
                                      {ok, FunDef} ->
                                          FunName1 = lists:flatten(io_lib:fwrite("~tw",[FunName])),
                                          case {edlin_expand:shell_default_or_bif(FunName1), shell:local_func(FunName1)} of
                                              {"user_defined", false} ->
                                                FunDef1 = lists:flatten(escape_quotes(lists:flatten(erl_pp:form(FunDef)))),
                                                FakeLine = reconstruct(FunDef, FunName),
                                                  {done, {ok, FakeResult, _}, _} = erl_scan:tokens(
                                                                                     [], "fd("++ FunName1 ++ ", " ++ FakeLine ++ ", \"" ++ FunDef1 ++ "\").\n",
                                                                                     {1,1}, [text,{reserved_word_fun,fun erl_scan:reserved_word/1}]),
                                                  erl_eval:extended_parse_exprs(FakeResult);
                                              _ -> erl_eval:extended_parse_exprs(Toks)
                                          end;
                                      _ -> erl_eval:extended_parse_exprs(Toks)
                                  end;
                              _ ->
                                  erl_eval:extended_parse_exprs(Toks)
                          end;
                      {eof,_EndPos} ->
                          eof;
                      {error,ErrorInfo,_EndPos} ->
                          %% Skip the rest of the line:
                          Opts = io:getopts(),
                          TmpOpts = lists:keyreplace(echo, 1, Opts,
                                                     {echo, false}),
                          _ = io:setopts(TmpOpts),
                          _ = io:get_line(''),
                          _ = io:setopts(Opts),
                          {error,ErrorInfo};
                      Else ->
                          Else
                  end
                 )
        end,
    Pid = spawn_link(Parse),
    get_command1(Pid, Eval, Bs, RT, FT, Ds).
escape_quotes(String) -> escape_quotes(String, []).

escape_quotes([], Acc) ->
    % When we've processed all characters, reverse the accumulator
    % because we've been prepending for efficiency reasons.
    lists:reverse(Acc);

escape_quotes([$\\, $\" | Rest], Acc) ->
    % If we find an escaped quote (\"),
    % we escape the backslash and the quote (\\\") and continue.
    escape_quotes(Rest, [$\", $\\, $\\, $\\ | Acc]);

escape_quotes([$\" | Rest], Acc) ->
    % If we find a quote ("),
    % we escape it (\\") and continue.
    escape_quotes(Rest, [$\", $\\ | Acc]);

escape_quotes([Char | Rest], Acc) ->
    % In case of any other character, we keep it as is.
    escape_quotes(Rest, [Char | Acc]).
reconstruct(Fun, Name) ->
    lists:flatten(erl_pp:expr(reconstruct1(Fun, Name))).
reconstruct1({function, Anno, Name, Arity, Clauses}, Name) ->
    {named_fun, Anno, 'RecursiveFuncVar', reconstruct1(Clauses, Name, Arity)}.
reconstruct1([{call, Anno, {atom, Anno1, Name}, Args}|Body], Name, Arity) when length(Args) =:= Arity ->
    [{call, Anno, {var, Anno1, 'RecursiveFuncVar'}, reconstruct1(Args, Name, Arity)}| reconstruct1(Body, Name, Arity)];
reconstruct1([{call, Anno, {atom, Anno1, Name}, Args}|Body], Name, Arity) -> % arity not the same
    [{call, Anno, {remote, Anno1, {atom, Anno1, shell_default}, {atom, Anno1, Name}}, reconstruct1(Args, Name, Arity)}|
     reconstruct1(Body, Name, Arity)];
reconstruct1([{call, Anno, {atom, Anno1, Fun}, Args}|Body], Name, Arity) -> % Name not the same
    case {edlin_expand:shell_default_or_bif(atom_to_list(Fun)), shell:local_func(Fun)} of
        {"user_defined", false} ->
            [{call, Anno, {remote, Anno1, {atom, Anno1, shell_default}, {atom, Anno1, Fun}}, reconstruct1(Args, Name, Arity)}|
             reconstruct1(Body, Name, Arity)];
        {"shell_default", false} ->
            [{call, Anno, {remote, Anno1, {atom, Anno1, shell_default}, {atom, Anno1, Fun}}, reconstruct1(Args, Name, Arity)}| reconstruct1(Body, Name, Arity)];
        {"erlang", false} ->
            [{call, Anno, {remote, Anno1, {atom, Anno1, erlang}, {atom, Anno1, Fun}}, reconstruct1(Args, Name, Arity)}| reconstruct1(Body, Name, Arity)];
        {_, true} ->
            [{call, Anno, {atom, Anno1, Fun}, reconstruct1(Args, Name, Arity)}| reconstruct1(Body, Name, Arity)]
    end;
reconstruct1([E|Body], Name, Arity) when is_tuple(E) ->
    [list_to_tuple(reconstruct1(tuple_to_list(E), Name, Arity))|reconstruct1(Body, Name, Arity)];
reconstruct1([E|Body], Name, Arity) when is_list(E) ->
    [reconstruct1(E, Name, Arity)|reconstruct1(Body, Name, Arity)];
reconstruct1([E|Body], Name, Arity) ->
    [E|reconstruct1(Body, Name, Arity)];
reconstruct1([], _, _) -> [].

get_command1(Pid, Eval, Bs, RT, FT, Ds) ->
    receive
        {shell_state, From} ->
            From ! {shell_state, Bs, RT, FT},
            get_command1(Pid, Eval, Bs, RT, FT, Ds);
        {'EXIT', Pid, Res} ->
            {Res, Eval};
        {'EXIT', Eval, {Reason,Stacktrace}} ->
            report_exception(error, {Reason,Stacktrace}, RT),
            get_command1(Pid, start_eval(Bs, RT, FT, Ds), Bs, RT, FT, Ds);
        {'EXIT', Eval, Reason} ->
            report_exception(error, {Reason,[]}, RT),
            get_command1(Pid, start_eval(Bs, RT, FT, Ds), Bs, RT, FT, Ds)
    end.

prompt(N, Eval0, Bs0, RT, FT, Ds0) ->
    case get_prompt_func() of
        {M,F} ->
            A = erl_anno:new(1),
            L = {cons,A,{tuple,A,[{atom,A,history},{integer,A,N}]},{nil,A}},
            C = {call,A,{remote,A,{atom,A,M},{atom,A,F}},[L]},
            {V,Eval,Bs,Ds} = shell_cmd([C], Eval0, Bs0, RT, FT, Ds0, pmt),
            {Eval,Bs,Ds,case V of
                            {pmt,Val} ->
                                Val;
                            _ ->
                                bad_prompt_func({M,F}),
                                default_prompt(N)
                        end};
        default ->
            {Eval0,Bs0,Ds0,default_prompt(N)}
    end.

get_prompt_func() ->
    case application:get_env(stdlib, shell_prompt_func, default) of
        {M,F}=PromptFunc when is_atom(M), is_atom(F) ->
            PromptFunc;
        default=Default ->
            Default;
        Term ->
            bad_prompt_func(Term),
            default
    end.

bad_prompt_func(M) ->
    fwrite_severity(benign, "Bad prompt function: ~tp", [M]).

default_prompt(N) ->
    %% Don't bother flattening the list irrespective of what the
    %% I/O-protocol states.
    case is_alive() of
        true  -> io_lib:format(<<"(~s)~w> ">>, [node(), N]);
        false -> io_lib:format(<<"~w> ">>, [N])
    end.

%% expand_hist(Expressions, CommandNumber)
%%  Preprocess the expression list replacing all history list commands
%%  with their expansions.

expand_hist(Es, C) ->
    catch {ok,expand_exprs(Es, C)}.

expand_exprs([E|Es], C) ->
    [expand_expr(E, C)|expand_exprs(Es, C)];
expand_exprs([], _C) ->
    [].

expand_expr({cons,A,H,T}, C) ->
    {cons,A,expand_expr(H, C),expand_expr(T, C)};
expand_expr({lc,A,E,Qs}, C) ->
    {lc,A,expand_expr(E, C),expand_quals(Qs, C)};
expand_expr({bc,A,E,Qs}, C) ->
    {bc,A,expand_expr(E, C),expand_quals(Qs, C)};
expand_expr({tuple,A,Elts}, C) ->
    {tuple,A,expand_exprs(Elts, C)};
expand_expr({map,A,Es}, C) ->
    {map,A,expand_exprs(Es, C)};
expand_expr({map,A,Arg,Es}, C) ->
    {map,A,expand_expr(Arg, C),expand_exprs(Es, C)};
expand_expr({map_field_assoc,A,K,V}, C) ->
    {map_field_assoc,A,expand_expr(K, C),expand_expr(V, C)};
expand_expr({map_field_exact,A,K,V}, C) ->
    {map_field_exact,A,expand_expr(K, C),expand_expr(V, C)};
expand_expr({record_index,A,Name,F}, C) ->
    {record_index,A,Name,expand_expr(F, C)};
expand_expr({record,A,Name,Is}, C) ->
    {record,A,Name,expand_fields(Is, C)};
expand_expr({record_field,A,R,Name,F}, C) ->
    {record_field,A,expand_expr(R, C),Name,expand_expr(F, C)};
expand_expr({record,A,R,Name,Ups}, C) ->
    {record,A,expand_expr(R, C),Name,expand_fields(Ups, C)};
expand_expr({record_field,A,R,F}, C) ->         %This is really illegal!
    {record_field,A,expand_expr(R, C),expand_expr(F, C)};
expand_expr({block,A,Es}, C) ->
    {block,A,expand_exprs(Es, C)};
expand_expr({'if',A,Cs}, C) ->
    {'if',A,expand_cs(Cs, C)};
expand_expr({'case',A,E,Cs}, C) ->
    {'case',A,expand_expr(E, C),expand_cs(Cs, C)};
expand_expr({'try',A,Es,Scs,Ccs,As}, C) ->
    {'try',A,expand_exprs(Es, C),expand_cs(Scs, C),
     expand_cs(Ccs, C),expand_exprs(As, C)};
expand_expr({'receive',A,Cs}, C) ->
    {'receive',A,expand_cs(Cs, C)};
expand_expr({'receive',A,Cs,To,ToEs}, C) ->
    {'receive',A,expand_cs(Cs, C), expand_expr(To, C), expand_exprs(ToEs, C)};
expand_expr({call,A,{atom,_,e},[N]}, C) ->
    case get_cmd(N, C) of
        {undefined,_,_} ->
            no_command(N);
        {[Ce],_V,_CommandN} ->
            Ce;
        {Ces,_V,_CommandN} when is_list(Ces) ->
            {block,A,Ces}
    end;
expand_expr({call,CA,{atom,VA,v},[N]}, C) ->
    case get_cmd(N, C) of
        {undefined,_,_} ->
            no_command(N);
        {Ces,_V,CommandN} when is_list(Ces) ->
            {call,CA,{atom,VA,v},[{integer,VA,CommandN}]}
    end;
expand_expr({call,A,F,Args}, C) ->
    {call,A,expand_expr(F, C),expand_exprs(Args, C)};
expand_expr({'catch',A,E}, C) ->
    {'catch',A,expand_expr(E, C)};
expand_expr({match,A,Lhs,Rhs}, C) ->
    {match,A,Lhs,expand_expr(Rhs, C)};
expand_expr({op,A,Op,Arg}, C) ->
    {op,A,Op,expand_expr(Arg, C)};
expand_expr({op,A,Op,Larg,Rarg}, C) ->
    {op,A,Op,expand_expr(Larg, C),expand_expr(Rarg, C)};
expand_expr({remote,A,M,F}, C) ->
    {remote,A,expand_expr(M, C),expand_expr(F, C)};
expand_expr({'fun',A,{clauses,Cs}}, C) ->
    {'fun',A,{clauses,expand_exprs(Cs, C)}};
expand_expr({named_fun,A,Name,Cs}, C) ->
    {named_fun,A,Name,expand_exprs(Cs, C)};
expand_expr({clause,A,H,G,B}, C) ->
    %% Could expand H and G, but then erl_eval has to be changed as well.
    {clause,A,H, G, expand_exprs(B, C)};
expand_expr({bin,A,Fs}, C) ->
    {bin,A,expand_bin_elements(Fs, C)};
                                                %expand_expr({'-'})
expand_expr(E, _C) ->    % Constants.
    E.

expand_cs([{clause,A,P,G,B}|Cs], C) ->
    [{clause,A,P,G,expand_exprs(B, C)}|expand_cs(Cs, C)];
expand_cs([], _C) ->
    [].

expand_fields([{record_field,A,F,V}|Fs], C) ->
    [{record_field,A,expand_expr(F, C),expand_expr(V, C)}|
     expand_fields(Fs, C)];
expand_fields([], _C) -> [].

expand_quals([{generate,A,P,E}|Qs], C) ->
    [{generate,A,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([{generate_strict,A,P,E}|Qs], C) ->
    [{generate_strict,A,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([{b_generate,A,P,E}|Qs], C) ->
    [{b_generate,A,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([{b_generate_strict,A,P,E}|Qs], C) ->
    [{b_generate_strict,A,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([{m_generate,A,P,E}|Qs], C) ->
    [{m_generate,A,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([{m_generate_strict,A,P,E}|Qs], C) ->
    [{m_generate_strict,A,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([E|Qs], C) ->
    [expand_expr(E, C)|expand_quals(Qs, C)];
expand_quals([], _C) -> [].

expand_bin_elements([], _C) ->
    [];
expand_bin_elements([{bin_element,A,E,Sz,Ts}|Fs], C) ->
    [{bin_element,A,expand_expr(E, C),Sz,Ts}|expand_bin_elements(Fs, C)].

no_command(N) ->
    throw({error,
           io_lib:fwrite(<<"~ts: command not found">>,
                         [erl_pp:expr(N, enc())])}).

%% add_cmd(Number, Expressions, Value)
%% get_cmd(Number, CurrentCommand)
%% del_cmd(Number, NewN, OldN, HasBin0) -> bool()

add_cmd(N, Es, V) ->
    put({command,N}, Es),
    put({result,N}, V).

getc(N) ->
    {get({command,N}), get({result,N}), N}.

get_cmd(Num, C) ->
    case catch erl_eval:expr(Num, erl_eval:new_bindings()) of
        {value,N,_} when N < 0 -> getc(C+N);
        {value,N,_} -> getc(N);
        _Other -> {undefined,undefined,undefined}
    end.

del_cmd(_Type, N, N0, HasBin) when N < N0 ->
    HasBin;
del_cmd(Type, N, N0, HasBin0) ->
    T = erase({Type,N}),
    HasBin = HasBin0 orelse has_binary(T),
    del_cmd(Type, N-1, N0, HasBin).

has_binary(T) ->
    try has_bin(T), false
    catch true=Thrown -> Thrown
    end.

has_bin(T) when is_tuple(T) ->
    has_bin(T, tuple_size(T));
has_bin([E | Es]) ->
    has_bin(E),
    has_bin(Es);
has_bin(B) when byte_size(B) > ?MAXSIZE_HEAPBINARY ->
    throw(true);
has_bin(T) ->
    T.

has_bin(T, 0) ->
    T;
has_bin(T, I) ->
    has_bin(element(I, T)),
    has_bin(T, I - 1).

-doc false.
get_state() ->
    whereis() ! {shell_state, self()},
    receive
        {shell_state, Bs, RT, FT} ->
            #shell_state{bindings = Bs, records = ets:tab2list(RT), functions = ets:tab2list(FT)}
    end.

-doc false.
get_function(Func, Arity) ->
    {shell_state, _Bs, _RT, FT} = get_state(),
    try
        {value, {_, Fun}} = lists:keysearch({function, {shell_default,Func,Arity}}, 1, FT),
        Fun
    catch _:_ ->
        undefined
    end.

%% shell_cmd(Sequence, Evaluator, Bindings, RecordTable, Dictionary, What)
%% shell_rep(Evaluator, Bindings, RecordTable, Dictionary) ->
%%      {Value,Evaluator,Bindings,Dictionary}
%%  Send a command to the evaluator and wait for the reply. Start a new
%%  evaluator if necessary.
%%  What = pmt | cmd. When evaluating a prompt ('pmt') the evaluated value
%%  must not be displayed, and it has to be returned.

shell_cmd(Es, Eval, Bs, RT, FT, Ds, W) ->
    Eval ! {shell_cmd,self(),{eval,Es}, W},
    shell_rep(Eval, Bs, RT, FT, Ds).

shell_rep(Ev, Bs0, RT, FT, Ds0) ->
    shell_rep(Ev, Bs0, RT, FT, Ds0, 5000).
shell_rep(Ev, Bs0, RT, FT, Ds0, Timeout) ->
    receive
        {shell_rep,Ev,{value,V,Bs,Ds}} ->
            {V,Ev,Bs,Ds};
        {shell_rep,Ev,{command_error,{Location,M,Error}}} ->
            fwrite_severity(benign, <<"~s: ~ts">>,
                            [pos(Location), M:format_error(Error)]),
            {{'EXIT',Error},Ev,Bs0,Ds0};
        {shell_req,Ev,{get_cmd,N}} ->
            Ev ! {shell_rep,self(),getc(N)},
            shell_rep(Ev, Bs0, RT, FT, Ds0);
        {shell_req,Ev,get_cmd} ->
            Ev ! {shell_rep,self(),get()},
            shell_rep(Ev, Bs0, RT, FT, Ds0);
        {shell_req,Ev,exit} ->
            Ev ! {shell_rep,self(),exit},
            exit(normal);
        {shell_req,Ev,{update_dict,Ds}} ->      % Update dictionary
            Ev ! {shell_rep,self(),ok},
            shell_rep(Ev, Bs0, RT, FT, Ds);
        {shell_state, From} ->
            From ! {shell_state, Bs0, RT, FT},
            shell_rep(Ev, Bs0, RT, FT, Ds0);
        {ev_exit,{Ev,Class,Reason0}} ->         % It has exited unnaturally
            receive {'EXIT',Ev,normal} -> ok end,
            report_exception(Class, Reason0, RT),
            Reason = nocatch(Class, Reason0),
            {{'EXIT',Reason},start_eval(Bs0, RT, FT, Ds0), Bs0, Ds0};
        {ev_caught,{Ev,Class,Reason0}} ->       % catch_exception is in effect
            report_exception(Class, benign, Reason0, RT),
            Reason = nocatch(Class, Reason0),
            {{'EXIT',Reason},Ev,Bs0,Ds0};
        {'EXIT',_Id,interrupt} ->               % Someone interrupted us
            exit(Ev, kill),
            shell_rep(Ev, Bs0, RT, FT, Ds0);
        {'EXIT',Ev,{Reason,Stacktrace}} ->
            report_exception(exit, {Reason,Stacktrace}, RT),
            {{'EXIT',Reason},start_eval(Bs0, RT, FT, Ds0), Bs0, Ds0};
        {'EXIT',Ev,Reason} ->
            report_exception(exit, {Reason,[]}, RT),
            {{'EXIT',Reason},start_eval(Bs0, RT, FT, Ds0), Bs0, Ds0};
        {'EXIT',_Id,R} ->
            exit(Ev, R),
            exit(R);
        _Other ->                               % Ignore everything else
            io:format("Throwing ~p~n", [_Other]),
            shell_rep(Ev, Bs0, RT, FT, Ds0)
        after Timeout ->
            io:format("Command is taking a long time, type Ctrl+G, then enter 'i' to interrupt~n"),
            shell_rep(Ev, Bs0, RT, FT, Ds0, infinity)
    end.

nocatch(throw, {Term,Stack}) ->
    {{nocatch,Term},Stack};
nocatch(error, Reason) ->
    Reason;
nocatch(exit, Reason) ->
    Reason.

report_exception(Class, Reason, RT) ->
    report_exception(Class, serious, Reason, RT).

report_exception(Class, Severity, {Reason,Stacktrace}, RT) ->
    Tag = severity_tag(Severity),
    I = iolist_size(Tag) + 1,
    PF = fun(Term, I1) -> pp(Term, I1, RT) end,
    SF = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    Enc = encoding(),
    Str = erl_error:format_exception(I, Class, Reason, Stacktrace, SF, PF, Enc),
    io:requests([{put_chars, latin1, Tag},
                 {put_chars, unicode, Str},
                 nl]).

start_eval(Bs, RT, FT, Ds) ->
    Self = self(),
    Ancestors = [self() | get('$ancestors')],
    Eval = spawn_link(fun() ->
                              put('$ancestors', Ancestors),
                              evaluator(Self, Bs, RT, FT, Ds)
                      end),
    put(evaluator, Eval),
    Eval.

%% evaluator(Shell, Bindings, RecordTable, ProcessDictionary)
%%  Evaluate expressions from the shell. Use the "old" variable bindings
%%  and dictionary.

evaluator(Shell, Bs, RT, FT, Ds) ->
    init_dict(Ds),
    case application:get_env(stdlib, restricted_shell) of
        undefined ->
            eval_loop(Shell, Bs, RT, FT);
        {ok,RShMod} ->
            case get(restricted_shell_state) of
                undefined -> put(restricted_shell_state, []);
                _ -> ok
            end,
            put(restricted_expr_state, []),
            restricted_eval_loop(Shell, Bs, RT, FT, RShMod)
    end.

eval_loop(Shell, Bs0, RT, FT) ->
    receive
        {shell_cmd,Shell,{eval,Es},W} ->
            Ef = {value,
                  fun(MForFun, As) -> apply_fun(MForFun, As, Shell) end},
            Lf = local_func_handler(Shell, RT, FT, Ef),
            Bs = eval_exprs(Es, Shell, Bs0, RT, Lf, Ef, W),
            eval_loop(Shell, Bs, RT, FT)
    end.

restricted_eval_loop(Shell, Bs0, RT, FT, RShMod) ->
    receive
        {shell_cmd,Shell,{eval,Es}, W} ->
            {LFH,NLFH} = restrict_handlers(RShMod, Shell, RT,  FT),
            put(restricted_expr_state, []),
            Bs = eval_exprs(Es, Shell, Bs0, RT, {eval,LFH}, {value,NLFH}, W),
            restricted_eval_loop(Shell, Bs, RT, FT, RShMod)
    end.

eval_exprs(Es, Shell, Bs0, RT, Lf, Ef, W) ->
    try
        {R,Bs2} = exprs(Es, Bs0, RT, Lf, Ef, W),
        Shell ! {shell_rep,self(),R},
        Bs2
    catch
        exit:normal ->
            exit(normal);
        Class:Reason:Stacktrace ->
            M = {self(),Class,{Reason,Stacktrace}},
            case do_catch(Class, Reason) of
                true ->
                    Shell ! {ev_caught,M},
                    Bs0;
                false ->
                    %% We don't want the ERROR REPORT generated by the
                    %% emulator. Note: exit(kill) needs nothing special.
                    {links,LPs} = process_info(self(), links),
                    ER = nocatch(Class, {Reason,Stacktrace}),
                    lists:foreach(fun(P) -> exit(P, ER) end, LPs--[Shell]),
                    Shell ! {ev_exit,M},
                    exit(normal)
            end
    end.

do_catch(exit, restricted_shell_stopped) ->
    false;
do_catch(exit, restricted_shell_started) ->
    false;
do_catch(_Class, _Reason) ->
    case application:get_env(stdlib, shell_catch_exception, false) of
        true ->
            true;
        _ ->
            false
    end.

exprs(Es, Bs0, RT, Lf, Ef, W) ->
    exprs(Es, Bs0, RT, Lf, Ef, Bs0, W).

exprs([E0|Es], Bs1, RT, Lf, Ef, Bs0, W) ->
    UsedRecords = used_record_defs(E0, RT),
    RBs = record_bindings(UsedRecords, Bs1),
    case check_command(prep_check([E0]), RBs) of
        ok ->
            E1 = expand_records(UsedRecords, E0),
            {value,V0,Bs2} = expr(E1, Bs1, Lf, Ef),
            Bs = orddict:from_list([VV || {X,_}=VV <- erl_eval:bindings(Bs2),
                                          not is_expand_variable(X)]),
            if
                Es =:= [] ->
                    VS = pp(V0, 1, RT),
                    case W of
                        cmd -> io:requests([{put_chars, unicode, VS}, nl]);
                        pmt -> ok
                    end,
                    %% Don't send the result back if it will be
                    %% discarded anyway.
                    V = if
                            W =:= pmt ->
                                {W,V0};
                            true -> case result_will_be_saved() of
                                        true -> V0;
                                        false ->
                                            erlang:garbage_collect(),
                                            ignored
                                    end
                        end,
                    {{value,V,Bs,get()},Bs};
                true ->
                    exprs(Es, Bs, RT, Lf, Ef, Bs0, W)
            end;
        {error,Error} ->
            {{command_error,Error},Bs0}
    end.

is_expand_variable(V) ->
    case catch atom_to_list(V) of
        "rec" ++ _Integer -> true;
        _ -> false
    end.

result_will_be_saved() ->
    case get_history_and_results() of
        {_, 0} -> false;
        _ -> true
    end.

used_record_defs(E, RT) ->
    %% Be careful to return a list where used records come before
    %% records that use them. The linter wants them ordered that way.
    UR = case used_records(E, [], RT, []) of
             [] ->
                 [];
             L0 ->
                 L1 = lists:zip(L0, lists:seq(1, length(L0))),
                 L2 = lists:keysort(2, lists:ukeysort(1, L1)),
                 [R || {R, _} <- L2]
         end,
    record_defs(RT, UR).

used_records(E, U0, RT, Skip) ->
    case used_records(E) of
        {name,Name,E1} ->
            U = case lists:member(Name, Skip) of
                    true ->
                        U0;
                    false ->
                        R = ets:lookup(RT, Name),
                        used_records(R, [Name | U0], RT, [Name | Skip])
                end,
            used_records(E1, U, RT, Skip);
        {expr,[E1 | Es]} ->
            used_records(Es, used_records(E1, U0, RT, Skip), RT, Skip);
        _ ->
            U0
    end.

used_records({record_index,_,Name,F}) ->
    {name, Name, F};
used_records({record,_,Name,Is}) ->
    {name, Name, Is};
used_records({record_field,_,R,Name,F}) ->
    {name, Name, [R | F]};
used_records({record,_,R,Name,Ups}) ->
    {name, Name, [R | Ups]};
used_records({record_field,_,R,F}) -> % illegal
    {expr, [R | F]};
used_records({call,_,{atom,_,record},[A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,_,{atom,_,is_record},[A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,_,{remote,_,{atom,_,erlang},{atom,_,is_record}},
              [A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,_,{atom,_,record_info},[A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,A,{tuple,_,[M,F]},As}) ->
    used_records({call,A,{remote,A,M,F},As});
used_records({type,_,record,[{atom,_,Name}|Fs]}) ->
    {name, Name, Fs};
used_records(T) when is_tuple(T) ->
    {expr, tuple_to_list(T)};
used_records(E) ->
    {expr, E}.

fwrite_severity(Severity, S, As) ->
    io:fwrite(<<"~ts\n">>, [format_severity(Severity, S, As)]).

format_severity(Severity, S, As) ->
    add_severity(Severity, io_lib:fwrite(S, As)).

add_severity(Severity, S) ->
    [severity_tag(Severity), S].

severity_tag(fatal)   -> <<"*** ">>;
severity_tag(serious) -> <<"** ">>;
severity_tag(benign)  -> <<"* ">>.

restrict_handlers(RShMod, Shell, RT, FT) ->
    { fun(F,As,Binds) ->
              local_allowed(F, As, RShMod, Binds, Shell, RT, FT)
      end,
      fun(MF,As) ->
              non_local_allowed(MF, As, RShMod, Shell)
      end }.

-define(BAD_RETURN(M, F, V),
        try erlang:error(reason)
        catch _:_:S -> erlang:raise(exit, {restricted_shell_bad_return,V},
                                    [{M,F,3} | S])
        end).

local_allowed(F, As, RShMod, Bs, Shell, RT,  FT) when is_atom(F) ->
    {LFH,NLFH} = restrict_handlers(RShMod, Shell, RT, FT),
    case not_restricted(F, As) of % Not restricted is the same as builtin.
                                                % variable and record manipulations local
                                                % to the shell process. Those are never
                                                % restricted.
        true ->
            local_func(F, As, Bs, Shell, RT, FT, {eval,LFH}, {value,NLFH});
        false ->
            {AsEv,Bs1} = expr_list(As, Bs, {eval,LFH}, {value,NLFH}),
            case RShMod:local_allowed(F, AsEv, {get(restricted_shell_state),
                                                get(restricted_expr_state)}) of
                {Result,{RShShSt,RShExprSt}} ->
                    put(restricted_shell_state, RShShSt),
                    put(restricted_expr_state, RShExprSt),
                    if not Result ->
                            shell_req(Shell, {update_dict,get()}),
                            exit({restricted_shell_disallowed,{F,AsEv}});
                       true -> % This is never a builtin,
                                                % those are handled above.
                            non_builtin_local_func(F,AsEv,Bs1, FT)
                    end;
                Unexpected ->  % The user supplied non conforming module
                    ?BAD_RETURN(RShMod, local_allowed, Unexpected)
            end
    end.

non_local_allowed(MForFun, As, RShMod, Shell) ->
    case RShMod:non_local_allowed(MForFun, As, {get(restricted_shell_state),
                                                get(restricted_expr_state)}) of
        {Result,{RShShSt,RShExprSt}} ->
            put(restricted_shell_state, RShShSt),
            put(restricted_expr_state, RShExprSt),
            case Result of
                false ->
                    shell_req(Shell, {update_dict,get()}),
                    exit({restricted_shell_disallowed,{MForFun,As}});
                {redirect, NewMForFun, NewAs} ->
                    apply_fun(NewMForFun, NewAs, Shell);
                _ ->
                    apply_fun(MForFun, As, Shell)
            end;
        Unexpected ->  % The user supplied non conforming module
            ?BAD_RETURN(RShMod, non_local_allowed, Unexpected)
    end.

%% The commands implemented in shell should not be checked if allowed
%% This *has* to correspond to the function local_func/7!
%% (especially true for f/1, the argument must not be evaluated).
not_restricted(f, []) ->
    true;
not_restricted(f, [_]) ->
    true;
not_restricted(h, []) ->
    true;
not_restricted(b, []) ->
    true;
not_restricted(history, [_]) ->
    true;
not_restricted(results, [_]) ->
    true;
not_restricted(catch_exception, [_]) ->
    true;
not_restricted(exit, []) ->
    true;
not_restricted(fl, []) ->
    true;
not_restricted(fd, [_,_]) ->
    true;
not_restricted(ft, [_]) ->
    true;
not_restricted(td, [_]) ->
    true;
not_restricted(rd, [_]) ->
    true;
not_restricted(rd, [_,_]) ->
    true;
not_restricted(rf, []) ->
    true;
not_restricted(rf, [_]) ->
    true;
not_restricted(rl, []) ->
    true;
not_restricted(rl, [_]) ->
    true;
not_restricted(rp, [_]) ->
    true;
not_restricted(rr, [_]) ->
    true;
not_restricted(rr, [_,_]) ->
    true;
not_restricted(rr, [_,_,_]) ->
    true;
not_restricted(v, [_]) ->
    true;
not_restricted(_, _) ->
    false.

%% When erlang:garbage_collect() is called from the shell,
%% the shell process process that spawned the evaluating
%% process is garbage collected as well.
%% To garbage collect the evaluating process only the command
%% garbage_collect(self()). can be used.
apply_fun({erlang,garbage_collect}, [], Shell) ->
    garb(Shell);
apply_fun({M,F}, As, _Shell) ->
    apply(M, F, As);
apply_fun(MForFun, As, _Shell) ->
    apply(MForFun, As).

prep_check({call,Anno,{atom,_,f},[{var,_,_Name}]}) ->
    %% Do not emit a warning for f(V) when V is unbound.
    {atom,Anno,ok};
prep_check({value,_CommandN,_Val}) ->
    %% erl_lint cannot handle the history expansion {value,_,_}.
    {atom,a0(),ok};
prep_check(T) when is_tuple(T) ->
    list_to_tuple(prep_check(tuple_to_list(T)));
prep_check([E | Es]) ->
    [prep_check(E) | prep_check(Es)];
prep_check(E) ->
    E.

expand_records([], E0) ->
    E0;
expand_records(UsedRecords, E0) ->
    RecordDefs = [Def || {_Name,Def} <- UsedRecords],
    A = erl_anno:new(1),
    E = prep_rec(E0),
    Forms0 = RecordDefs ++ [{function,A,foo,0,[{clause,A,[],[],[E]}]}],
    Forms = erl_expand_records:module(Forms0, [strict_record_tests]),
    {function,A,foo,0,[{clause,A,[],[],[NE]}]} = lists:last(Forms),
    prep_rec(NE).

prep_rec({value,_CommandN,_V}=Value) ->
    %% erl_expand_records cannot handle the history expansion {value,_,_}.
    {atom,Value,ok};
prep_rec({atom,{value,_CommandN,_V}=Value,ok}) ->
    %% Undo the effect of the previous clause...
    Value;
prep_rec(T) when is_tuple(T) -> list_to_tuple(prep_rec(tuple_to_list(T)));
prep_rec([E | Es]) -> [prep_rec(E) | prep_rec(Es)];
prep_rec(E) -> E.

init_dict([{K,V}|Ds]) ->
    put(K, V),
    init_dict(Ds);
init_dict([]) -> true.

-doc false.
help() ->
    S = ~"""
         ** shell internal commands **
         b()        -- display all variable bindings
         e(N)       -- repeat the expression in query <N>
         exit()     -- terminate the shell instance
         f()        -- forget all variable bindings
         f(X)       -- forget the binding of variable X
         ff()       -- forget all locally defined functions
         ff(F,A)    -- forget locally defined function named as atom F and arity A
         fl()       -- forget all locally defined functions, types and records
         h()        -- history
         h(Mod)     -- help about module
         h(Mod,Func) -- help about function in module
         h(Mod,Func,Arity) -- help about function with arity in module
         lf()       -- list locally defined functions
         lr()       -- list locally defined records
         lt()       -- list locally defined types
         rd(R,D)    -- define a record
         rf()       -- remove all record information
         rf(R)      -- remove record information about R
         rl()       -- display all record information
         rl(R)      -- display record information about R
         rp(Term)   -- display Term using the shell's record information
         rr(File)   -- read record information from File (wildcards allowed)
         rr(F,R)    -- read selected record information from file(s)
         rr(F,R,O)  -- read selected record information with options
         tf()       -- forget all locally defined types
         tf(T)      -- forget locally defined type named as atom T
         v(N)       -- use the value of query <N>
         catch_exception(B) -- how exceptions are handled
         history(N) -- set how many previous commands to keep
         results(N) -- set how many previous command results to keep
         save_module(FilePath) -- save all locally defined functions, types and records to a file
         """,
    io:put_chars(S),
    io:nl(),
    true.


%% local_func(Function, Args, Bindings, Shell, RecordTable,
%%            LocalFuncHandler, ExternalFuncHandler) -> {value,Val,Bs}
%%  Evaluate local functions, including shell commands.
%%
%% Note that the predicate not_restricted/2 has to correspond to what's
%% handled internally - it should return 'true' for all local functions
%% handled in this module (i.e. those that are not eventually handled by
%% non_builtin_local_func/3 (user_default/shell_default).
%% fd, ft and td should not be exposed to the user
-doc false.
local_func() -> [v,h,b,f,fd,ff,fl,lf,lr,lt,rd,rf,rl,rp,rr,tf,save_module,history,results,catch_exception].
-doc false.
local_func(Func) ->
    lists:member(Func, local_func()).
local_func(v, [{integer,_,V}], Bs, Shell, _RT, _FT, _Lf, _Ef) ->
    %% This command is validated and expanded prior.
    {_Ces,Value,_N} = shell_req(Shell, {get_cmd, V}),
    {value,Value,Bs};
local_func(h, [], Bs, Shell, RT, _FT, _Lf, _Ef) ->
    Cs = shell_req(Shell, get_cmd),
    Cs1 = lists:filter(fun({{command, _},_}) -> true;
                          ({{result, _},_}) -> true;
                          (_) -> false
                       end,
                       Cs),
    Cs2 = lists:map(fun({{T, N}, V}) -> {{N, T}, V} end,
                    Cs1),
    Cs3 = lists:keysort(1, Cs2),
    {value,list_commands(Cs3, RT),Bs};
local_func(b, [], Bs, _Shell, RT, _FT, _Lf, _Ef) ->
    {value,list_bindings(erl_eval:bindings(Bs), RT),Bs};
local_func(f, [], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    {value,ok,erl_eval:new_bindings()};
local_func(f, [{var,_,Name}], Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    {value,ok,erl_eval:del_binding(Name, Bs)};
local_func(f, [_Other], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,f,1}]);
%% Output local functions (with specs)
local_func(lf, [], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    Output = local_functions_and_specs(FT),
    io:requests([{put_chars, unicode, Output++"\n"}, nl]),
    {value, ok, Bs};
%% Output local types
local_func(lt, [], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    Output = local_types(FT),
    io:requests([{put_chars, unicode, Output}, nl]),
    {value, ok, Bs};
%% Output local records
local_func(lr, [], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    Output = local_records(FT),
    io:requests([{put_chars, unicode, Output}, nl]),
    {value, ok, Bs};

%% Undocumented function that outputs contents of FT to a module file
%% compiles it and loads it, its still in experimentation phase
%% In theory, you may want to be able to load a module in to local table
%% edit them, and then save it back to the file system.
%% You may also want to be able to save a test module.
local_func(save_module, [{string,_,PathToFile}], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    [_Path, FileName] = string:split("/"++PathToFile, "/", trailing),
    [Module, _] = string:split(FileName, ".", leading),
    Module1 = io_lib:fwrite("~tw",[list_to_atom(Module)]),
    Exports = [lists:flatten(io_lib:fwrite("~tw",[F]))++"/"++integer_to_list(A)||{F,A}<-local_defined_functions(FT)],
    Output = (
        "-module("++Module1++").\n\n" ++
        "-export(["++lists:join(",",Exports)++"]).\n\n"++
        local_types(FT) ++
        local_records(FT) ++
        local_functions(FT)
    ),
    Ret = case filelib:is_file(PathToFile) of
        false ->
            write_and_compile_module(PathToFile, Output);
        true ->
            case io:get_line("File exists, do you want to overwrite it? (y/N)\n") of
                "y\n" ->
                    write_and_compile_module(PathToFile, Output);
                _ ->
                    io:format("Aborting save~n"),
                    ok
            end
    end,
    {value, Ret, Bs};
local_func(save_module, [_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,save_module,1}]);
local_func(fd, [{atom,_,FunName}, FunExpr, {string, _, FunDef}], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    {value, Fun, []} = erl_eval:expr(FunExpr, []),
    {arity, Arity} = erlang:fun_info(Fun, arity),
    %% unescape_quotes(FunDef)
    ets:insert(FT, [{{function, {shell_default, FunName, Arity}}, Fun}]),
    ets:insert(FT, [{{function_def, {shell_default, FunName, Arity}}, FunDef}]),
    {value, ok, Bs};
local_func(fd, [_,_,_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell, fd, 3}]);
local_func(ft, [{string, _, TypeDef}], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    case erl_scan:tokens([], TypeDef, {1,1}, [text,{reserved_word_fun,fun erl_scan:reserved_word/1}]) of
        {done, {ok, Toks, _}, _} ->
            case erl_parse:parse_form(Toks) of
                {ok, {attribute,_,spec,{{FunName, Arity},_}}=AttrForm} ->
                    ets:insert(FT, [{{function_type, {shell_default, FunName, Arity}}, AttrForm}]),
                    ets:insert(FT, [{{function_type_def, {shell_default, FunName, Arity}}, TypeDef}]),
                    {value, ok, Bs};
                {error,{_Location,M,ErrDesc}} ->
                    ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
                    exit(lists:flatten(ErrStr))
            end;
        {done, {error,{_Location, M, ErrDesc}, _}, _} ->
            ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
            exit(lists:flatten(ErrStr))
    end;
local_func(ft, [_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell, ft, 1}]);
local_func(fl, [], Bs, _Shell, RT, FT, _Lf, _Ef) ->
    LocalRecords = [R || {{record_def, R},_} <- ets:tab2list(FT)],
    true = ets:delete_all_objects(FT),
    lists:foreach(fun(Name) -> ets:delete(RT, Name) end, LocalRecords),
    {value,ok,Bs};
local_func(ff, [], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    ets:select_delete(FT, [{{{function_def, '_'},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{function, '_'},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{function_type_def, '_'},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{function_type, '_'},'_'}, [],[true]}]),
    {value,ok,Bs};
local_func(ff, [{atom, _, F}, {integer,_,A}], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    M = shell_default,
    ets:select_delete(FT, [{{{function_def, {M, F, A}},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{function, {M, F, A}},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{function_type_def, {M, F, A}},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{function_type, {M, F, A}},'_'}, [],[true]}]),
    {value,ok,Bs};
local_func(ff, [_,_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,ff,2}]);
local_func(td, [{string, _, TypeDef}], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    case erl_scan:tokens([], TypeDef, {1,1}, [text,{reserved_word_fun,fun erl_scan:reserved_word/1}]) of
        {done, {ok, Toks, _}, _} ->
            case erl_parse:parse_form(Toks) of
                {ok, {attribute,_,type,{TypeName, _, _}}=AttrForm} ->
                    true = ets:insert(FT, [{{type, TypeName}, AttrForm}]),
                    true = ets:insert(FT, [{{type_def, TypeName}, TypeDef}]),
                    {value, ok, Bs};
                {ok, {attribute,_,nominal,{TypeName, _, _}}=AttrForm} ->
                    true = ets:insert(FT, [{{type, TypeName}, AttrForm}]),
                    true = ets:insert(FT, [{{type_def, TypeName}, TypeDef}]),
                    {value, ok, Bs};
                {error,{_Location,M,ErrDesc}} ->
                    ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
                    exit(lists:flatten(ErrStr))
            end;
        {done, {error,{_Location, M, ErrDesc}, _}, _} ->
            ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
            exit(lists:flatten(ErrStr))
    end;
local_func(td, [_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell, td, 1}]);
local_func(tf, [], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    ets:select_delete(FT, [{{{type_def, '_'}, '_'}, [],[true]}]),
    ets:select_delete(FT, [{{{type, '_'}, '_'}, [],[true]}]),
    {value,ok,Bs};
local_func(tf, [{atom,_,A}], Bs, _Shell, _RT, FT, _Lf, _Ef) ->
    ets:select_delete(FT, [{{{type_def, A},'_'}, [],[true]}]),
    ets:select_delete(FT, [{{{type, A},'_'}, [],[true]}]),
    {value,ok,Bs};
local_func(tf, [_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,tf,1}]);
local_func(rd, [{string, _, TypeDef}], Bs, _Shell, RT, FT, _Lf, _Ef) ->
    case erl_scan:tokens([], TypeDef, {1,1}, [text,{reserved_word_fun,fun erl_scan:reserved_word/1}]) of
        {done, {ok, Toks, _}, _} ->
            case erl_parse:parse_form(Toks) of
                {ok,{attribute,_,_,{TypeName,_}}=AttrForm} ->
                    [_] = add_records([AttrForm], Bs, RT),
                    true = ets:insert(FT, [{{record_def, TypeName}, TypeDef}]),
                    {value,ok,Bs};
                {error,{_Location,M,ErrDesc}} ->
                    ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
                    exit(lists:flatten(ErrStr))
            end;
        {done, {error,{_Location, M, ErrDesc}, _}, _}  ->
            ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
            exit(lists:flatten(ErrStr))
    end;
local_func(rd, [_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell, rd, 1}]);
local_func(rd, [{atom,_,RecName0},RecDef0], Bs, _Shell, RT, FT, _Lf, _Ef) ->
    RecDef = expand_value(RecDef0),
    RDs = lists:flatten(erl_pp:expr(RecDef)),
    RecName = io_lib:write_atom_as_latin1(RecName0),
    Attr = lists:concat(["-record(", RecName, ",", RDs, ")."]),
    {ok, Tokens, _} = erl_scan:string(Attr),
    case erl_parse:parse_form(Tokens) of
        {ok,{attribute,_,_,{TypeName,_}}=AttrForm} ->
            [RN] = add_records([AttrForm], Bs, RT),
            true = ets:insert(FT, [{{record_def, TypeName}, RecDef}]),
            {value,RN,Bs};
        {error,{_Location,M,ErrDesc}} ->
            ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
            exit(lists:flatten(ErrStr))
    end;
local_func(rd, [_,_], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,rd,2}]);
local_func(rf, [], Bs, _Shell, RT, FT, _Lf, _Ef) ->
    ets:select_delete(FT, [{{{record_def, '_'},'_'}, [],[true]}]),
    true = ets:delete_all_objects(RT),
    {value,initiate_records(Bs, RT),Bs};
local_func(rf, [A], Bs0, _Shell, RT, FT, Lf, Ef) ->
    {[Recs],Bs} = expr_list([A], Bs0, Lf, Ef),
    if '_' =:= Recs ->
            ets:select_delete(FT, [{{{record_def, '_'},'_'}, [],[true]}]),
            true = ets:delete_all_objects(RT);
       true ->
            lists:foreach(fun(Name) ->
                            true = ets:delete(RT, Name),
                            true = ets:delete(FT, {record_def, Name})
                          end, listify(Recs))
    end,
    {value,ok,Bs};
local_func(rl, [], Bs, _Shell, RT, _FT, _Lf, _Ef) ->
    {value,list_records(ets:tab2list(RT)),Bs};
local_func(rl, [A], Bs0, _Shell, RT, _FT, Lf, Ef) ->
    {[Recs],Bs} = expr_list([A], Bs0, Lf, Ef),
    {value,list_records(record_defs(RT, listify(Recs))),Bs};
local_func(rp, [A], Bs0, _Shell, RT, _FT, Lf, Ef) ->
    {[V],Bs} = expr_list([A], Bs0, Lf, Ef),
    Cs = pp(V, _Column=1, _Depth=-1, RT),
    io:requests([{put_chars, unicode, Cs}, nl]),
    {value,ok,Bs};
local_func(rr, [A], Bs0, _Shell, RT, _FT, Lf, Ef) ->
    {[File],Bs} = expr_list([A], Bs0, Lf, Ef),
    {value,read_and_add_records(File, '_', [], Bs, RT),Bs};
local_func(rr, [_,_]=As0, Bs0, _Shell, RT, _FT, Lf, Ef) ->
    {[File,Sel],Bs} = expr_list(As0, Bs0, Lf, Ef),
    {value,read_and_add_records(File, Sel, [], Bs, RT),Bs};
local_func(rr, [_,_,_]=As0, Bs0, _Shell, RT, _FT, Lf, Ef) ->
    {[File,Sel,Options],Bs} = expr_list(As0, Bs0, Lf, Ef),
    {value,read_and_add_records(File, Sel, Options, Bs, RT),Bs};
local_func(history, [{integer,_,N}], Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    {value,history(N),Bs};
local_func(history, [_Other], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,history,1}]);
local_func(results, [{integer,_,N}], Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    {value,results(N),Bs};
local_func(results, [_Other], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,results,1}]);
local_func(catch_exception, [{atom,_,Bool}], Bs, _Shell, _RT, _FT, _Lf, _Ef)
  when Bool; not Bool ->
    {value,catch_exception(Bool),Bs};
local_func(catch_exception, [_Other], _Bs, _Shell, _RT, _FT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,catch_exception,1}]);
local_func(exit, [], _Bs, Shell, _RT, _FT, _Lf, _Ef) ->
    shell_req(Shell, exit),                     %This terminates us
    exit(normal);
local_func(F, As0, Bs0, _Shell, _RT, FT, Lf, Ef) when is_atom(F) ->
    {As,Bs} = expr_list(As0, Bs0, Lf, Ef),
    non_builtin_local_func(F,As,Bs, FT).

local_functions_and_specs(FT) ->
    Output_functions = maps:from_list(
        [{{FunNameAtom, Arity}, lists:flatten(FunDef)} ||{{function_def,{_, FunNameAtom, Arity}},FunDef} <- ets:tab2list(FT)]),
    Output_function_specs = maps:from_list(
        [{{FunNameAtom, Arity}, FunSpec}|| {{function_type_def, {_, FunNameAtom, Arity}}, FunSpec} <- ets:tab2list(FT)]),
    Keys1 = maps:keys(Output_functions),
    Keys2 = maps:keys(Output_function_specs),
    Keys = lists:uniq(Keys1 ++ Keys2),
    string:trim(lists:join($\n,lists:map(fun(Key) ->
        Spec = maps:get(Key, Output_function_specs, nospec),
        Def = maps:get(Key, Output_functions, nodef),
        case {Spec, Def} of
            {nospec, _} -> Def;
            {_, nodef} ->
                {FunName, Arity} = Key,
                Spec ++ "%% " ++ lists:flatten(io_lib:fwrite("~tw",[FunName]))++ "/" ++ integer_to_list(Arity) ++ " not implemented";
            {_, _} -> Spec ++ Def
        end
    end,
    Keys))).
local_defined_functions(FT) ->
    [{F, A} ||{{function_def,{_, F, A}},_} <- ets:tab2list(FT)].
local_functions(FT) ->
    local_functions(local_defined_functions(FT), FT).
local_functions(Keys, FT) ->
    lists:join($\n,
        [begin
            Spec = case ets:lookup(FT, {function_type_def, {shell_default, F, A}}) of
                [{{function_type_def, {shell_default, F, A}}, Spec1}] -> Spec1;
                [] -> ""
            end,
            [{{function_def, {shell_default, F, A}}, Def}] = ets:lookup(FT, {function_def, {shell_default, F, A}}),
            Spec++Def
        end || {F, A} <- Keys]).
%% Output local types
local_types(FT) ->
    lists:join($\n,
        [TypeDef||{{type_def, _},TypeDef} <- ets:tab2list(FT)]).
%% Output local records
local_records(FT) ->
    lists:join($\n,
        [RecDef||{{record_def, _},RecDef} <- ets:tab2list(FT)]).
write_and_compile_module(PathToFile, Output) ->
    case file:write_file(PathToFile, unicode:characters_to_binary(Output)) of
        ok -> c:c(PathToFile);
        Error -> Error
    end.
non_builtin_local_func(F,As,Bs, FT) ->
    Arity = length(As),
    case erlang:function_exported(user_default, F, Arity) of
        true ->
            {eval,erlang:make_fun(user_default, F, Arity),As,Bs};
        false ->
            shell_default(F,As,Bs, FT)
    end.

shell_default(F,As,Bs, FT) ->
    M = shell_default,
    A = length(As),
    case code:ensure_loaded(M) of
        {module, _} ->
            case erlang:function_exported(M,F,A) of
                true ->
                    {eval,erlang:make_fun(M, F, A),As,Bs};
                false ->
                    shell_default_local_func(F,As, Bs, FT)
            end;
        {error, _} ->
            shell_default_local_func(F,As, Bs, FT)
    end.

shell_default_local_func(F, As, Bs, FT) ->
    case ets:lookup(FT, {function, {shell_default, F, length(As)}}) of
        [] -> shell_undef(F, length(As));
        [{_, Fun}] -> {eval, Fun, As, Bs}
    end.

shell_undef(F,A) ->
    erlang:error({shell_undef,F,A,[]}).

local_func_handler(Shell, RT, FT, Ef) ->
    H = fun(Lf) ->
                fun(F, As, Bs) ->
                        local_func(F, As, Bs, Shell, RT, FT, {eval,Lf(Lf)}, Ef)
                end
        end,
    {eval,H(H)}.

record_print_fun(RT) ->
    fun(Tag, NoFields) ->
            case ets:lookup(RT, Tag) of
                [{_,{attribute,_,record,{Tag,Fields}}}]
                  when length(Fields) =:= NoFields ->
                    record_fields(Fields);
                _ ->
                    no
            end
    end.

record_fields([{record_field,_,{atom,_,Field}} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([{record_field,_,{atom,_,Field},_} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([{typed_record_field,Field,_Type} | Fs]) ->
    record_fields([Field | Fs]);
record_fields([]) ->
    [].

initiate_records(Bs, RT) ->
    RNs1 = init_rec(shell_default, Bs, RT),
    RNs2 = case code:is_loaded(user_default) of
               {file,_File} ->
                   init_rec(user_default, Bs, RT);
               false ->
                   []
           end,
    lists:usort(RNs1 ++ RNs2).

init_rec(Module, Bs, RT) ->
    case read_records(Module, []) of
        RAs when is_list(RAs) ->
            case catch add_records(RAs, Bs, RT) of
                {'EXIT',_} ->
                    [];
                RNs ->
                    RNs
            end;
        _Error ->
            []
    end.

-doc false.
read_and_add_records(File, Selected, Options, Bs, RT) ->
    case read_records(File, Selected, Options) of
        RAs when is_list(RAs) ->
            add_records(RAs, Bs, RT);
        Error ->
            Error
    end.

read_records(File, Selected, Options) ->
    case read_records(File, listify(Options)) of
        Error when is_tuple(Error) ->
            Error;
        RAs when Selected =:= '_' ->
            RAs;
        RAs ->
            Sel = listify(Selected),
            [RA || {attribute,_,_,{Name,_}}=RA <- RAs,
                   lists:member(Name, Sel)]
    end.

add_records(RAs, Bs0, RT) ->
    %% TODO store File name to support type completion
    Recs = [{Name,D} || {attribute,_,_,{Name,_}}=D <- RAs],
    Bs1 = record_bindings(Recs, Bs0),
    case check_command([], Bs1) of
        {error,{_Location,M,ErrDesc}} ->
            %% A source file that has not been compiled.
            ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
            exit(lists:flatten(ErrStr));
        ok ->
            true = ets:insert(RT, Recs),
            lists:usort([Name || {Name,_} <- Recs])
    end.

listify(L) when is_list(L) ->
    L;
listify(E) ->
    [E].

check_command(Es, Bs) ->
    erl_eval:check_command(Es, Bs).

expr(E, Bs, Lf, Ef) ->
    erl_eval:expr(E, Bs, Lf, Ef).

expr_list(Es, Bs, Lf, Ef) ->
    erl_eval:expr_list(Es, Bs, Lf, Ef).

%% Note that a sequence number is used here to make sure that if a
%% record is used by another record, then the first record is parsed
%% before the second record. (erl_eval:check_command() calls the
%% linter which needs the records in a proper order.)
record_bindings([], Bs) ->
    Bs;
record_bindings(Recs0, Bs0) ->
    {Recs1, _} = lists:mapfoldl(fun ({Name,Def}, I) -> {{Name,I,Def},I+1}
                                end, 0, Recs0),
    Recs2 = lists:keysort(2, lists:ukeysort(1, Recs1)),
    lists:foldl(fun ({Name,I,Def}, Bs) ->
                        erl_eval:add_binding({record,I,Name}, Def, Bs)
                end, Bs0, Recs2).

%%% Read record information from file(s)

read_records(FileOrModule, Opts0) ->
    Opts = lists:delete(report_warnings, Opts0),
    case find_file(FileOrModule) of
        {beam, Beam, File} ->
            read_records_from_beam(Beam, File);
        {files,[File]} ->
            read_file_records(File, Opts);
        {files,Files} ->
            lists:flatmap(fun(File) ->
                                  case read_file_records(File, Opts) of
                                      RAs when is_list(RAs) -> RAs;
                                      _ -> []
                                  end
                          end, Files);
        Error ->
            Error
    end.

-include_lib("kernel/include/file.hrl").

find_file(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        File when is_list(File) ->
            %% Special cases:
            %% - Modules not in the code path (loaded with code:load_abs/1):
            %%   code:get_object_code/1 only searches in the code path
            %%   but code:which/1 finds all loaded modules
            %% - File can also be a file in an archive,
            %%   beam_lib:chunks/2 cannot handle such paths but
            %%   erl_prim_loader:read_file/1 can
            case erl_prim_loader:read_file(File) of
                {ok, Beam} ->
                    {beam, Beam, File};
                error ->
                    {error, nofile}
            end;
        preloaded ->
            {_M, Beam, File} = code:get_object_code(Mod),
            {beam, Beam, File};
        _Else -> % non_existing, interpreted, cover_compiled
            {error,nofile}
    end;
find_file(File) ->
    case catch filelib:wildcard(File) of
        {'EXIT',_} ->
            {error,invalid_filename};
        Files ->
            {files,Files}
    end.

read_file_records(File, Opts) ->
    case filename:extension(File) of
        ".beam" ->
            read_records_from_beam(File, File);
        _ ->
            parse_file(File, Opts)
    end.

read_records_from_beam(Beam, File) ->
    case beam_lib:chunks(Beam, [abstract_code,"CInf"]) of
        {ok,{_Mod,[{abstract_code,{Version,Forms}},{"CInf",CB}]}} ->
            case record_attrs(Forms) of
                [] when Version =:= raw_abstract_v1 ->
                    [];
                [] ->
                    %% If the version is raw_X, then this test
                    %% is unnecessary.
                    try_source(File, CB);
                Records ->
                    Records
            end;
        {ok,{_Mod,[{abstract_code,no_abstract_code},{"CInf",CB}]}} ->
            try_source(File, CB);
        Error ->
            %% Could be that the "Abst" chunk is missing (pre R6).
            Error
    end.

%% This is how the debugger searches for source files. See int.erl.
try_source(Beam, RawCB) ->
    EbinDir = filename:dirname(Beam),
    CB = binary_to_term(RawCB),
    Os = proplists:get_value(options,CB, []),
    Src0 = filename:rootname(Beam) ++ ".erl",
    Src1 = filename:join([filename:dirname(EbinDir), "src",
                          filename:basename(Src0)]),
    Src2 = proplists:get_value(source, CB, []),
    try_sources([Src0,Src1,Src2], Os).

try_sources([], _) ->
    {error, nofile};
try_sources([Src|Rest], Os) ->
    case is_file(Src) of
        true -> parse_file(Src, Os);
        false -> try_sources(Rest, Os)
    end.

is_file(Name) ->
    case filelib:is_file(Name) of
        true ->
            not filelib:is_dir(Name);
        false ->
            false
    end.

parse_file(File, Opts) ->
    Cwd = ".",
    Dir = filename:dirname(File),
    IncludePath = [Cwd,Dir|inc_paths(Opts)],
    case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
        {ok,Forms} ->
            record_attrs(Forms);
        Error ->
            Error
    end.

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [P || {i,P} <- Opts, is_list(P)].

record_attrs(Forms) ->
    [A || A = {attribute,_,record,_D} <- Forms].

%%% End of reading record information from file(s)

shell_req(Shell, Req) ->
    Shell ! {shell_req,self(),Req},
    receive
        {shell_rep,Shell,Rep} -> Rep
    end.

list_commands([{{N,command},Es0}, {{N,result}, V} |Ds], RT) ->
    Es = prep_list_commands(Es0),
    VS = pp(V, 4, RT),
    Ns = io_lib:fwrite(<<"~w: ">>, [N]),
    I = iolist_size(Ns),
    io:requests([{put_chars, latin1, Ns},
                 {format,<<"~ts\n">>,[erl_pp:exprs(Es, I, enc())]},
                 {format,<<"-> ">>,[]},
                 {put_chars, unicode, VS},
                 nl]),
    list_commands(Ds, RT);
list_commands([{{N,command},Es0} |Ds], RT) ->
    Es = prep_list_commands(Es0),
    Ns = io_lib:fwrite(<<"~w: ">>, [N]),
    I = iolist_size(Ns),
    io:requests([{put_chars, latin1, Ns},
                 {format,<<"~ts\n">>,[erl_pp:exprs(Es, I, enc())]}]),
    list_commands(Ds, RT);
list_commands([_D|Ds], RT) ->
    list_commands(Ds, RT);
list_commands([], _RT) -> ok.

list_bindings([{Name,Val}|Bs], RT) ->
    case erl_eval:fun_data(Val) of
        {fun_data,_FBs,FCs0} ->
            FCs = expand_value(FCs0), % looks nicer
            A = a0(),
            F = {'fun',A,{clauses,FCs}},
            M = {match,A,{var,A,Name},F},
            io:fwrite(<<"~ts\n">>, [erl_pp:expr(M, enc())]);
        {named_fun_data,_FBs,FName,FCs0} ->
            FCs = expand_value(FCs0), % looks nicer
            A = a0(),
            F = {named_fun,A,FName,FCs},
            M = {match,A,{var,A,Name},F},
            io:fwrite(<<"~ts\n">>, [erl_pp:expr(M, enc())]);
        false ->
            Namel = io_lib:fwrite(<<"~s = ">>, [Name]),
            Nl = iolist_size(Namel)+1,
            ValS = pp(Val, Nl, RT),
            io:requests([{put_chars, latin1, Namel},
                         {put_chars, unicode, ValS},
                         nl])
    end,
    list_bindings(Bs, RT);
list_bindings([], _RT) ->
    ok.

list_records(Records) ->
    lists:foreach(fun({_Name,Attr}) ->
                          io:fwrite(<<"~ts">>, [erl_pp:attribute(Attr, enc())])
                  end, Records).

record_defs(RT, Names) ->
    lists:flatmap(fun(Name) -> ets:lookup(RT, Name)
                  end, Names).

expand_value(E) ->
    substitute_v1(fun({value,CommandN,V}) -> try_abstract(V, CommandN)
                  end, E).

%% There is no abstract representation of funs.
try_abstract(V, CommandN) ->
    try erl_parse:abstract(V)
    catch
        _:_ ->
            A = a0(),
            {call,A,{atom,A,v},[{integer,A,CommandN}]}
    end.

%% Rather than listing possibly huge results the calls to v/1 are shown.
prep_list_commands(E) ->
    A = a0(),
    substitute_v1(fun({value,Anno,_V}) ->
                          CommandN = erl_anno:line(Anno),
                          {call,A,{atom,A,v},[{integer,A,CommandN}]}
                  end, E).

substitute_v1(F, {value,_,_}=Value) ->
    F(Value);
substitute_v1(F, T) when is_tuple(T) ->
    list_to_tuple(substitute_v1(F, tuple_to_list(T)));
substitute_v1(F, [E | Es]) ->
    [substitute_v1(F, E) | substitute_v1(F, Es)];
substitute_v1(_F, E) ->
    E.

a0() ->
    erl_anno:new(0).

pos({Line,Col}) ->
    io_lib:format("~w:~w", [Line,Col]);
pos(Line) ->
    io_lib:format("~w", [Line]).

check_and_get_history_and_results() ->
    check_env(shell_history_length),
    check_env(shell_saved_results),
    get_history_and_results().

get_history_and_results() ->
    History = get_env(shell_history_length, ?DEF_HISTORY),
    Results = get_env(shell_saved_results, ?DEF_RESULTS),
    {History, erlang:min(Results, History)}.

pp(V, I, RT) ->
    pp(V, I, _Depth=?LINEMAX, RT).

pp(V, I, D, RT) ->
    Strings = application:get_env(stdlib, shell_strings, true) =/= false,
    io_lib_pretty:print(V, ([{column, I}, {line_length, columns()},
                             {depth, D}, {line_max_chars, ?CHAR_MAX},
                             {strings, Strings},
                             {record_print_fun, record_print_fun(RT)}]
                            ++ enc())).

columns() ->
    case io:columns() of
        {ok,N} -> N;
        _ -> 80
    end.

encoding() ->
    [{encoding, Encoding}] = enc(),
    Encoding.

enc() ->
    case lists:keyfind(encoding, 1, io:getopts()) of
        false -> [{encoding,latin1}]; % should never happen
        Enc -> [Enc]
    end.

garb(Shell) ->
    erlang:garbage_collect(Shell),
    catch erlang:garbage_collect(whereis(user)),
    catch erlang:garbage_collect(group_leader()),
    erlang:garbage_collect().

get_env(V, Def) ->
    case application:get_env(stdlib, V, Def) of
        Val when is_integer(Val), Val >= 0 ->
            Val;
        _ ->
            Def
    end.

check_env(V) ->
    case application:get_env(stdlib, V, 0) of
        Val when is_integer(Val), Val >= 0 ->
            ok;
        Val ->
            Txt = io_lib:fwrite
                    ("Invalid value of STDLIB configuration parameter"
                     "~tw: ~tp\n", [V, Val]),
            error_logger:info_report(lists:flatten(Txt))
    end.

set_env(App, Name, Val, Default) ->
    Prev = case application:get_env(App, Name) of
               undefined ->
                   Default;
               {ok, Old} ->
                   Old
           end,
    application_controller:set_env(App, Name, Val),
    Prev.

-doc """
Sets the number of previous commands to keep in the history list to `N`. The
previous number is returned. Defaults to 20.
""".
-spec history(N) -> non_neg_integer() when
      N :: non_neg_integer().

history(L) when is_integer(L), L >= 0 ->
    set_env(stdlib, shell_history_length, L, ?DEF_HISTORY).

-doc """
Sets the number of results from previous commands to keep in the history list to
`N`. The previous number is returned. Defaults to 20.
""".
-spec results(N) -> non_neg_integer() when
      N :: non_neg_integer().

results(L) when is_integer(L), L >= 0 ->
    set_env(stdlib, shell_saved_results, L, ?DEF_RESULTS).

-doc """
Sets the exception handling of the evaluator process. The previous exception
handling is returned. The default (`false`) is to kill the evaluator process
when an exception occurs, which causes the shell to create a new evaluator
process. When the exception handling is set to `true`, the evaluator process
lives on, which means that, for example, ports and ETS tables as well as
processes linked to the evaluator process survive the exception.
""".
-spec catch_exception(Bool) -> boolean() when
      Bool :: boolean().

catch_exception(Bool) ->
        set_env(stdlib, shell_catch_exception, Bool, ?DEF_CATCH_EXCEPTION).

-doc """
Sets the shell prompt function to `PromptFunc`. The previous prompt function is
returned.
""".
-doc(#{since => <<"OTP R13B04">>}).
-spec prompt_func(PromptFunc) -> PromptFunc2 when
      PromptFunc :: 'default' | {module(),atom()},
      PromptFunc2 :: 'default' | {module(),atom()}.

prompt_func(PromptFunc) ->
    set_env(stdlib, shell_prompt_func, PromptFunc, ?DEF_PROMPT_FUNC).

-doc """
Sets the shell multiline prompt function to `PromptFunc`. The previous prompt
function is returned.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec multiline_prompt_func(PromptFunc) -> PromptFunc2 when
      PromptFunc :: 'default' | {module(),function()} | string(),
      PromptFunc2 :: 'default' | {module(),function()} | string().

multiline_prompt_func(PromptFunc) ->
    set_env(stdlib, shell_multiline_prompt, PromptFunc, ?DEF_PROMPT_FUNC).

-doc """
Can be used to set the formatting of the Erlang shell output.

This has an effect on commands that have been submitted, and how it is saved in history.
Or if the formatting hotkey is pressed while editing an expression (Alt+R by default). You
can specify a `Mod:Func/1` that expects the whole expression as a string and
returns a formatted expressions as a string. See
[`stdlib app config`](stdlib_app.md#format_shell_func) for how to set it before
shell started.

If instead a string is provided, it will be used as a shell command. Your
command must include `${file}` somewhere in the string, for the shell to know
where the file goes in the command.

```erlang
shell:format_shell_func("\"emacs -batch \${file} -l ~/erlang-format/emacs-format-file -f emacs-format-function\"").
```

```erlang
shell:format_shell_func({shell, erl_pp_format_func}).
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec format_shell_func(ShellFormatFunc) -> ShellFormatFunc2 when
      ShellFormatFunc :: 'default' | {module(),function()} | string(),
      ShellFormatFunc2 :: 'default' | {module(),function()} | string().
format_shell_func(ShellFormatFunc) ->
    set_env(stdlib, format_shell_func, ShellFormatFunc, default).

-doc """
A formatting function that can be set with `format_shell_func/1` that will make
expressions submitted to the shell prettier.

> #### Note {: .info }
>
> This formatting function filter comments away from the expressions.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec erl_pp_format_func(String) -> String2 when
      String :: string(),
      String2 :: string().
erl_pp_format_func(String) ->
    %% A simple pretty printer function of shell expressions.
    %%
    %% Comments will be filtered.
    %% If you add return_comments to the option list,
    %% parsing will fail, and we will end up with the original string.
    Options = [text,{reserved_word_fun,fun erl_scan:reserved_word/1}],
    case erl_scan:tokens([], string:trim(String) ++ "\n", {1,1}, Options) of
        {done, {ok, Toks, _}, _} ->
            try
                case erl_parse:parse_form(Toks) of
                    {ok, Def} -> lists:flatten(erl_pp:form(Def))
                end
            catch
                _:_ -> case erl_parse:parse_exprs(Toks) of
                          {ok, Def1} -> lists:flatten(erl_pp:exprs(Def1))++".";
                          _ -> String
                      end
            end;
        _ -> String
    end.

-doc """
Sets pretty printing of lists to `Strings`. The previous value of the flag is
returned.

The flag can also be set by the STDLIB application variable `shell_strings`.
Defaults to `true`, which means that lists of integers are printed using the
string syntax, when possible. Value `false` means that no lists are printed
using the string syntax.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec strings(Strings) -> Strings2 when
      Strings :: boolean(),
      Strings2 :: boolean().

strings(Strings) ->
    set_env(stdlib, shell_strings, Strings, ?DEF_STRINGS).

-doc """
Equivalent to `prompt_width/2` with `Encoding` set to the encoding used by
`t:io:user/0`.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec prompt_width(unicode:chardata()) -> non_neg_integer().
prompt_width(String) ->
    Encoding =
        case whereis(user_drv) =:= self() of
            %% When in JCL mode edlin can be called by
            %% user_drv, which in turn calls this function.
            %% Since JCL is very rudimentary we can assume
            %% that it uses latin1.
            true -> latin1;
            false ->
                proplists:get_value(encoding, io:getopts(user))
        end,
    prompt_width(String, Encoding).

-doc """
It receives a prompt and computes its width, considering its Unicode characters
and ANSI escapes.

Useful for creating custom multiline prompts.

Example:

```erlang
1> shell:prompt_width("olá> ", unicode).
5
%% "olá> " is printed as "ol\341> " on a latin1 systems
2> shell:prompt_width("olá> ", latin1).
8
%% Ansi escapes are ignored
3> shell:prompt_width("\e[32molá\e[0m> ", unicode).
5
%% Double width characters count as 2
4> shell:prompt_width("😀> ", unicode).
4
%% "😀> " is printed as "\x{1F600}> " on latin1 systems
5> shell:prompt_width("😀> ", latin1).
11
```
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec prompt_width(unicode:chardata(), unicode | latin1) -> non_neg_integer().
prompt_width(String, Encoding) ->
    case string:next_grapheme(String) of
        [] -> 0;
        [$\e | Rest] ->
            case re:run(String, prim_tty:ansi_regexp(), [unicode]) of
                {match, [{0, N}]} ->
                    <<_Ansi:N/binary, AnsiRest/binary>> = unicode:characters_to_binary(String),
                    prompt_width(AnsiRest, Encoding);
                _ ->
                    prim_tty:npwcwidth($\e, Encoding) + prompt_width(Rest, Encoding)
            end;
        [H|Rest] when is_list(H)-> lists:sum([prim_tty:npwcwidth(A, Encoding)||A<-H]) + prompt_width(Rest, Encoding);
        [H|Rest] -> prim_tty:npwcwidth(H, Encoding) + prompt_width(Rest, Encoding)
    end.

-doc """
Configures the multiline prompt as two trailing dots. This is the default
function but it may also be set explicitly as
`-stdlib shell_multiline_prompt {shell, default_multiline_prompt}`.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec default_multiline_prompt(unicode:chardata()) ->
      unicode:chardata().

default_multiline_prompt(Pbs) ->
    lists:duplicate(max(0, prompt_width(Pbs) - 3), $\s) ++ ".. ".

-doc """
Configures the multiline prompt as inverted space. It may be set explicitly as
`-stdlib shell_multiline_prompt {shell, inverted_space_prompt}` or calling
`multiline_prompt_func({shell, inverted_space_prompt}).`
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec inverted_space_prompt(unicode:chardata()) ->
      unicode:chardata().

inverted_space_prompt(Pbs) ->
    "\e[7m" ++ lists:duplicate(prompt_width(Pbs) - 1, $\s) ++ "\e[27m ".
