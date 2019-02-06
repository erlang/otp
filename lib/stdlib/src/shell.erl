%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

-export([start/0, start/1, start/2, server/1, server/2, history/1, results/1]).
-export([whereis_evaluator/0, whereis_evaluator/1]).
-export([start_restricted/1, stop_restricted/0]).
-export([local_allowed/3, non_local_allowed/3]).
-export([catch_exception/1, prompt_func/1, strings/1]).

-define(LINEMAX, 30).
-define(CHAR_MAX, 60).
-define(DEF_HISTORY, 20).
-define(DEF_RESULTS, 20).
-define(DEF_CATCH_EXCEPTION, false).
-define(DEF_PROMPT_FUNC, default).
-define(DEF_STRINGS, true).

-define(RECORDS, shell_records).

-define(MAXSIZE_HEAPBINARY, 64).

%% When used as the fallback restricted shell callback module...
local_allowed(q,[],State) ->
    {true,State};
local_allowed(_,_,State) ->
    {false,State}.

non_local_allowed({init,stop},[],State) ->
    {true,State};
non_local_allowed(_,_,State) ->
    {false,State}.

-spec start() -> pid().

start() ->
    start(false, false).

start(init) ->
    start(false, true);
start(NoCtrlG) ->
    start(NoCtrlG, false).

start(NoCtrlG, StartSync) ->
    _ = code:ensure_loaded(user_default),
    spawn(fun() -> server(NoCtrlG, StartSync) end).

%% Find the pid of the current evaluator process.
-spec whereis_evaluator() -> 'undefined' | pid().

whereis_evaluator() ->
    %% locate top group leader, always registered as user
    %% can be implemented by group (normally) or user 
    %% (if oldshell or noshell)
    case whereis(user) of
	undefined ->
	    undefined;
	User ->
	    %% get user_drv pid from group, or shell pid from user
	    case group:interfaces(User) of
		[] ->				% old- or noshell
		    case user:interfaces(User) of
			[] ->
			    undefined;
			[{shell,Shell}] ->
			    whereis_evaluator(Shell)
		    end;
		[{user_drv,UserDrv}] ->
		    %% get current group pid from user_drv
		    case user_drv:interfaces(UserDrv) of
			[] ->
			    undefined;
			[{current_group,Group}] ->
			    %% get shell pid from group
			    GrIfs = group:interfaces(Group),
			    case lists:keyfind(shell, 1, GrIfs) of
				{shell, Shell} ->
				    whereis_evaluator(Shell);
				false ->
				    undefined
			    end
		    end
	    end
    end.

-spec whereis_evaluator(pid()) -> 'undefined' | pid().

whereis_evaluator(Shell) ->
    case process_info(Shell, dictionary) of
	{dictionary,Dict} ->
	    case lists:keyfind(evaluator, 1, Dict) of
		{_, Eval} when is_pid(Eval) ->
		    Eval;
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

%% Call this function to start a user restricted shell 
%% from a normal shell session.
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

-spec stop_restricted() -> no_return().

stop_restricted() ->
    application:unset_env(stdlib, restricted_shell),
    exit(restricted_shell_stopped).

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

-spec server(boolean()) -> 'terminated'.

server(StartSync) ->
    case init:get_argument(async_shell_start) of
	{ok,_} -> 
	    ok;					% no sync with init
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
    %% Our spawner has fixed the process groups.
    Bs = erl_eval:new_bindings(),

    %% Use an Ets table for record definitions. It takes too long to
    %% send a huge term to and from the evaluator. Ets makes it
    %% possible to have thousands of record definitions.
    RT = ets:new(?RECORDS, [public,ordered_set]),
    _ = initiate_records(Bs, RT),
    process_flag(trap_exit, true),

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

    case get(no_control_g) of
	true ->
	    io:fwrite(<<"Eshell V~s\n">>, [erlang:system_info(version)]);
	_undefined_or_false ->
	    io:fwrite(<<"Eshell V~s  (abort with ^G)\n">>,
		      [erlang:system_info(version)])
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
    server_loop(0, start_eval(Bs, RT, []), Bs, RT, [], History, Results).

server_loop(N0, Eval_0, Bs00, RT, Ds00, History0, Results0) ->
    N = N0 + 1,
    {Eval_1,Bs0,Ds0,Prompt} = prompt(N, Eval_0, Bs00, RT, Ds00),
    {Res,Eval0} = get_command(Prompt, Eval_1, Bs0, RT, Ds0),
    case Res of 
	{ok,Es0,XBs} ->
            Es1 = erl_eval:subst_values_for_vars(Es0, XBs),
            case expand_hist(Es1, N) of
                {ok,Es} ->
                    {V,Eval,Bs,Ds} = shell_cmd(Es, Eval0, Bs0, RT, Ds0, cmd),
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
                        true ->
                            ok
                    end,
                    server_loop(N, Eval, Bs, RT, Ds, History, Results);
                {error,E} ->
                    fwrite_severity(benign, <<"~ts">>, [E]),
                    server_loop(N0, Eval0, Bs0, RT, Ds0, History0, Results0)
            end;
	{error,{Line,Mod,What}} ->
            fwrite_severity(benign, <<"~w: ~ts">>,
                            [Line, Mod:format_error(What)]),
	    server_loop(N0, Eval0, Bs0, RT, Ds0, History0, Results0);
	{error,terminated} ->			%Io process terminated
	    exit(Eval0, kill),
	    terminated;
	{error,interrupted} ->			%Io process interrupted us
	    exit(Eval0, kill),
	    {_,Eval,_,_} = shell_rep(Eval0, Bs0, RT, Ds0),
	    server_loop(N0, Eval, Bs0, RT, Ds0, History0, Results0);
	{error,tokens} ->			%Most probably character > 255
            fwrite_severity(benign, <<"~w: Invalid tokens.">>, 
                            [N]),
	    server_loop(N0, Eval0, Bs0, RT, Ds0, History0, Results0);
	eof ->
            fwrite_severity(fatal, <<"Terminating erlang (~w)">>, [node()]),
	    halt()
    end.

get_command(Prompt, Eval, Bs, RT, Ds) ->
    Parse =
        fun() ->
                exit(
                  case
                      io:scan_erl_exprs(group_leader(), Prompt, 1, [text])
                  of
                      {ok,Toks,_EndPos} ->
                          erl_eval:extended_parse_exprs(Toks);
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
    get_command1(Pid, Eval, Bs, RT, Ds).

get_command1(Pid, Eval, Bs, RT, Ds) ->
    receive
	{'EXIT', Pid, Res} ->
	    {Res, Eval};
	{'EXIT', Eval, {Reason,Stacktrace}} ->
            report_exception(error, {Reason,Stacktrace}, RT),
	    get_command1(Pid, start_eval(Bs, RT, Ds), Bs, RT, Ds);
	{'EXIT', Eval, Reason} ->
            report_exception(error, {Reason,[]}, RT),
	    get_command1(Pid, start_eval(Bs, RT, Ds), Bs, RT, Ds)
    end.

prompt(N, Eval0, Bs0, RT, Ds0) ->
    case get_prompt_func() of
        {M,F} ->
            L = [{history,N}],
            A = erl_anno:new(1),
            C = {call,A,{remote,A,{atom,A,M},{atom,A,F}},[{value,A,L}]},
            {V,Eval,Bs,Ds} = shell_cmd([C], Eval0, Bs0, RT, Ds0, pmt),
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
    case application:get_env(stdlib, shell_prompt_func) of
        {ok,{M,F}=PromptFunc} when is_atom(M), is_atom(F) ->
            PromptFunc;
        {ok,default=Default} ->
            Default;
        {ok,Term} ->
            bad_prompt_func(Term),
            default;
        undefined ->
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

expand_expr({cons,L,H,T}, C) ->
    {cons,L,expand_expr(H, C),expand_expr(T, C)};
expand_expr({lc,L,E,Qs}, C) ->
    {lc,L,expand_expr(E, C),expand_quals(Qs, C)};
expand_expr({bc,L,E,Qs}, C) ->
    {bc,L,expand_expr(E, C),expand_quals(Qs, C)};
expand_expr({tuple,L,Elts}, C) ->
    {tuple,L,expand_exprs(Elts, C)};
expand_expr({map,L,Es}, C) ->
    {map,L,expand_exprs(Es, C)};
expand_expr({map,L,Arg,Es}, C) ->
    {map,L,expand_expr(Arg, C),expand_exprs(Es, C)};
expand_expr({map_field_assoc,L,K,V}, C) ->
    {map_field_assoc,L,expand_expr(K, C),expand_expr(V, C)};
expand_expr({map_field_exact,L,K,V}, C) ->
    {map_field_exact,L,expand_expr(K, C),expand_expr(V, C)};
expand_expr({record_index,L,Name,F}, C) ->
    {record_index,L,Name,expand_expr(F, C)};
expand_expr({record,L,Name,Is}, C) ->
    {record,L,Name,expand_fields(Is, C)};
expand_expr({record_field,L,R,Name,F}, C) ->
    {record_field,L,expand_expr(R, C),Name,expand_expr(F, C)};
expand_expr({record,L,R,Name,Ups}, C) ->
    {record,L,expand_expr(R, C),Name,expand_fields(Ups, C)};
expand_expr({record_field,L,R,F}, C) ->		%This is really illegal!
    {record_field,L,expand_expr(R, C),expand_expr(F, C)};
expand_expr({block,L,Es}, C) ->
    {block,L,expand_exprs(Es, C)};
expand_expr({'if',L,Cs}, C) ->
    {'if',L,expand_cs(Cs, C)};
expand_expr({'case',L,E,Cs}, C) ->
    {'case',L,expand_expr(E, C),expand_cs(Cs, C)};
expand_expr({'try',L,Es,Scs,Ccs,As}, C) ->
    {'try',L,expand_exprs(Es, C),expand_cs(Scs, C),
     expand_cs(Ccs, C),expand_exprs(As, C)};
expand_expr({'receive',L,Cs}, C) ->
    {'receive',L,expand_cs(Cs, C)};
expand_expr({'receive',L,Cs,To,ToEs}, C) ->
    {'receive',L,expand_cs(Cs, C), expand_expr(To, C), expand_exprs(ToEs, C)};
expand_expr({call,L,{atom,_,e},[N]}, C) ->
    case get_cmd(N, C) of
        {undefined,_,_} ->
	    no_command(N);
	{[Ce],_V,_CommandN} ->
	    Ce;
	{Ces,_V,_CommandN} when is_list(Ces) ->
	    {block,L,Ces}
    end;
expand_expr({call,_L,{atom,_,v},[N]}, C) ->
    case get_cmd(N, C) of
        {_,undefined,_} ->
	    no_command(N);
	{Ces,V,CommandN} when is_list(Ces) ->
            {value,erl_anno:new(CommandN),V}
    end;
expand_expr({call,L,F,Args}, C) ->
    {call,L,expand_expr(F, C),expand_exprs(Args, C)};
expand_expr({'catch',L,E}, C) ->
    {'catch',L,expand_expr(E, C)};
expand_expr({match,L,Lhs,Rhs}, C) ->
    {match,L,Lhs,expand_expr(Rhs, C)};
expand_expr({op,L,Op,Arg}, C) ->
    {op,L,Op,expand_expr(Arg, C)};
expand_expr({op,L,Op,Larg,Rarg}, C) ->
    {op,L,Op,expand_expr(Larg, C),expand_expr(Rarg, C)};
expand_expr({remote,L,M,F}, C) ->
    {remote,L,expand_expr(M, C),expand_expr(F, C)};
expand_expr({'fun',L,{clauses,Cs}}, C) ->
    {'fun',L,{clauses,expand_exprs(Cs, C)}};
expand_expr({named_fun,L,Name,Cs}, C) ->
    {named_fun,L,Name,expand_exprs(Cs, C)};
expand_expr({clause,L,H,G,B}, C) ->
    %% Could expand H and G, but then erl_eval has to be changed as well.
    {clause,L,H, G, expand_exprs(B, C)};
expand_expr({bin,L,Fs}, C) ->
    {bin,L,expand_bin_elements(Fs, C)};
expand_expr(E, _C) ->	 % Constants.
    E.

expand_cs([{clause,L,P,G,B}|Cs], C) ->
    [{clause,L,P,G,expand_exprs(B, C)}|expand_cs(Cs, C)];
expand_cs([], _C) ->
    [].

expand_fields([{record_field,L,F,V}|Fs], C) ->
    [{record_field,L,expand_expr(F, C),expand_expr(V, C)}|
     expand_fields(Fs, C)];
expand_fields([], _C) -> [].

expand_quals([{generate,L,P,E}|Qs], C) ->
    [{generate,L,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([{b_generate,L,P,E}|Qs], C) ->
    [{b_generate,L,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([E|Qs], C) ->
    [expand_expr(E, C)|expand_quals(Qs, C)];
expand_quals([], _C) -> [].

expand_bin_elements([], _C) ->
    [];
expand_bin_elements([{bin_element,L,E,Sz,Ts}|Fs], C) ->
    [{bin_element,L,expand_expr(E, C),Sz,Ts}|expand_bin_elements(Fs, C)].

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

%% shell_cmd(Sequence, Evaluator, Bindings, RecordTable, Dictionary, What)
%% shell_rep(Evaluator, Bindings, RecordTable, Dictionary) ->
%%	{Value,Evaluator,Bindings,Dictionary}
%%  Send a command to the evaluator and wait for the reply. Start a new
%%  evaluator if necessary.
%%  What = pmt | cmd. When evaluating a prompt ('pmt') the evaluated value
%%  must not be displayed, and it has to be returned.

shell_cmd(Es, Eval, Bs, RT, Ds, W) ->
    Eval ! {shell_cmd,self(),{eval,Es}, W},
    shell_rep(Eval, Bs, RT, Ds).

shell_rep(Ev, Bs0, RT, Ds0) ->
    receive
	{shell_rep,Ev,{value,V,Bs,Ds}} ->
	    {V,Ev,Bs,Ds};
        {shell_rep,Ev,{command_error,{Line,M,Error}}} -> 
            fwrite_severity(benign, <<"~w: ~ts">>,
                            [Line, M:format_error(Error)]),
            {{'EXIT',Error},Ev,Bs0,Ds0};
	{shell_req,Ev,get_cmd} ->
	    Ev ! {shell_rep,self(),get()},
	    shell_rep(Ev, Bs0, RT, Ds0);
	{shell_req,Ev,exit} ->
	    Ev ! {shell_rep,self(),exit},
	    exit(normal);
	{shell_req,Ev,{update_dict,Ds}} ->	% Update dictionary
	    Ev ! {shell_rep,self(),ok},
	    shell_rep(Ev, Bs0, RT, Ds);
        {ev_exit,{Ev,Class,Reason0}} ->         % It has exited unnaturally
            receive {'EXIT',Ev,normal} -> ok end,
	    report_exception(Class, Reason0, RT),
            Reason = nocatch(Class, Reason0),
	    {{'EXIT',Reason},start_eval(Bs0, RT, Ds0), Bs0, Ds0};
        {ev_caught,{Ev,Class,Reason0}} ->       % catch_exception is in effect
	    report_exception(Class, benign, Reason0, RT),
            Reason = nocatch(Class, Reason0),
            {{'EXIT',Reason},Ev,Bs0,Ds0};
	{'EXIT',_Id,interrupt} ->		% Someone interrupted us
	    exit(Ev, kill),
	    shell_rep(Ev, Bs0, RT, Ds0);
        {'EXIT',Ev,{Reason,Stacktrace}} ->
            report_exception(exit, {Reason,Stacktrace}, RT),
	    {{'EXIT',Reason},start_eval(Bs0, RT, Ds0), Bs0, Ds0};
        {'EXIT',Ev,Reason} ->
            report_exception(exit, {Reason,[]}, RT),
	    {{'EXIT',Reason},start_eval(Bs0, RT, Ds0), Bs0, Ds0};
	{'EXIT',_Id,R} ->
	    exit(Ev, R),
	    exit(R);
	_Other ->				% Ignore everything else
	    shell_rep(Ev, Bs0, RT, Ds0)
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

start_eval(Bs, RT, Ds) ->
    Self = self(),
    Eval = spawn_link(fun() -> evaluator(Self, Bs, RT, Ds) end),
    put(evaluator, Eval),
    Eval.

%% evaluator(Shell, Bindings, RecordTable, ProcessDictionary)
%%  Evaluate expressions from the shell. Use the "old" variable bindings
%%  and dictionary.

evaluator(Shell, Bs, RT, Ds) ->
    init_dict(Ds),
    case application:get_env(stdlib, restricted_shell) of
	undefined ->
	    eval_loop(Shell, Bs, RT);
	{ok,RShMod} ->
	    case get(restricted_shell_state) of
		undefined -> put(restricted_shell_state, []);
		_ -> ok
	    end,
	    put(restricted_expr_state, []),
	    restricted_eval_loop(Shell, Bs, RT, RShMod)
    end.

eval_loop(Shell, Bs0, RT) ->
    receive
	{shell_cmd,Shell,{eval,Es},W} ->
            Ef = {value, 
                  fun(MForFun, As) -> apply_fun(MForFun, As, Shell) end},
            Lf = local_func_handler(Shell, RT, Ef),
            Bs = eval_exprs(Es, Shell, Bs0, RT, Lf, Ef, W),
	    eval_loop(Shell, Bs, RT)
    end.

restricted_eval_loop(Shell, Bs0, RT, RShMod) ->
    receive
	{shell_cmd,Shell,{eval,Es}, W} ->
            {LFH,NLFH} = restrict_handlers(RShMod, Shell, RT),
            put(restricted_expr_state, []),
            Bs = eval_exprs(Es, Shell, Bs0, RT, {eval,LFH}, {value,NLFH}, W),
	    restricted_eval_loop(Shell, Bs, RT, RShMod)
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
    case application:get_env(stdlib, shell_catch_exception) of
        {ok, true} ->
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
used_records({call,Line,{tuple,_,[M,F]},As}) ->
    used_records({call,Line,{remote,Line,M,F},As});
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

restrict_handlers(RShMod, Shell, RT) ->
    { fun(F,As,Binds) -> 
	      local_allowed(F, As, RShMod, Binds, Shell, RT) 
      end,
      fun(MF,As) -> 
	      non_local_allowed(MF, As, RShMod, Shell) 
      end }.

-define(BAD_RETURN(M, F, V),
        try erlang:error(reason)
        catch _:_:S -> erlang:raise(exit, {restricted_shell_bad_return,V}, 
                                    [{M,F,3} | S])
        end).

local_allowed(F, As, RShMod, Bs, Shell, RT) when is_atom(F) ->
    {LFH,NLFH} = restrict_handlers(RShMod, Shell, RT),
    case not_restricted(F, As) of % Not restricted is the same as builtin. 
				  % variable and record manipulations local 
	                          % to the shell process. Those are never
	                          % restricted.
	true ->
	    local_func(F, As, Bs, Shell, RT, {eval,LFH}, {value,NLFH});
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
			    non_builtin_local_func(F,AsEv,Bs1)
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

prep_check({call,Line,{atom,_,f},[{var,_,_Name}]}) ->
    %% Do not emit a warning for f(V) when V is unbound.
    {atom,Line,ok};
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
    L = erl_anno:new(1),
    E = prep_rec(E0),
    Forms0 = RecordDefs ++ [{function,L,foo,0,[{clause,L,[],[],[E]}]}],
    Forms = erl_expand_records:module(Forms0, [strict_record_tests]),
    {function,L,foo,0,[{clause,L,[],[],[NE]}]} = lists:last(Forms),
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

%% local_func(Function, Args, Bindings, Shell, RecordTable, 
%%            LocalFuncHandler, ExternalFuncHandler) -> {value,Val,Bs}
%%  Evaluate local functions, including shell commands.
%%
%% Note that the predicate not_restricted/2 has to correspond to what's 
%% handled internally - it should return 'true' for all local functions 
%% handled in this module (i.e. those that are not eventually handled by 
%% non_builtin_local_func/3 (user_default/shell_default).

local_func(h, [], Bs, Shell, RT, _Lf, _Ef) ->
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
local_func(b, [], Bs, _Shell, RT, _Lf, _Ef) ->
    {value,list_bindings(erl_eval:bindings(Bs), RT),Bs};
local_func(f, [], _Bs, _Shell, _RT, _Lf, _Ef) ->
    {value,ok,erl_eval:new_bindings()};
local_func(f, [{var,_,Name}], Bs, _Shell, _RT, _Lf, _Ef) ->
    {value,ok,erl_eval:del_binding(Name, Bs)};
local_func(f, [_Other], _Bs, _Shell, _RT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,f,1}]);
local_func(rd, [{atom,_,RecName0},RecDef0], Bs, _Shell, RT, _Lf, _Ef) ->
    RecDef = expand_value(RecDef0),
    RDs = lists:flatten(erl_pp:expr(RecDef)),
    RecName = io_lib:write_atom_as_latin1(RecName0),
    Attr = lists:concat(["-record(", RecName, ",", RDs, ")."]),
    {ok, Tokens, _} = erl_scan:string(Attr),
    case erl_parse:parse_form(Tokens) of
        {ok,AttrForm} ->
            [RN] = add_records([AttrForm], Bs, RT),
            {value,RN,Bs};
        {error,{_Line,M,ErrDesc}} ->
            ErrStr = io_lib:fwrite(<<"~ts">>, [M:format_error(ErrDesc)]),
            exit(lists:flatten(ErrStr))
    end;
local_func(rd, [_,_], _Bs, _Shell, _RT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,rd,2}]);
local_func(rf, [], Bs, _Shell, RT, _Lf, _Ef) ->
    true = ets:delete_all_objects(RT),
    {value,initiate_records(Bs, RT),Bs};
local_func(rf, [A], Bs0, _Shell, RT, Lf, Ef) ->
    {[Recs],Bs} = expr_list([A], Bs0, Lf, Ef),
    if '_' =:= Recs ->
            true = ets:delete_all_objects(RT);
       true -> 
            lists:foreach(fun(Name) -> true = ets:delete(RT, Name)
                          end, listify(Recs))
    end,
    {value,ok,Bs};
local_func(rl, [], Bs, _Shell, RT, _Lf, _Ef) ->
    {value,list_records(ets:tab2list(RT)),Bs};
local_func(rl, [A], Bs0, _Shell, RT, Lf, Ef) ->
    {[Recs],Bs} = expr_list([A], Bs0, Lf, Ef),
    {value,list_records(record_defs(RT, listify(Recs))),Bs};
local_func(rp, [A], Bs0, _Shell, RT, Lf, Ef) ->
    {[V],Bs} = expr_list([A], Bs0, Lf, Ef),
    Cs = pp(V, _Column=1, _Depth=-1, RT),
    io:requests([{put_chars, unicode, Cs}, nl]),
    {value,ok,Bs};
local_func(rr, [A], Bs0, _Shell, RT, Lf, Ef) ->
    {[File],Bs} = expr_list([A], Bs0, Lf, Ef),
    {value,read_and_add_records(File, '_', [], Bs, RT),Bs};
local_func(rr, [_,_]=As0, Bs0, _Shell, RT, Lf, Ef) ->
    {[File,Sel],Bs} = expr_list(As0, Bs0, Lf, Ef),
    {value,read_and_add_records(File, Sel, [], Bs, RT),Bs};
local_func(rr, [_,_,_]=As0, Bs0, _Shell, RT, Lf, Ef) ->
    {[File,Sel,Options],Bs} = expr_list(As0, Bs0, Lf, Ef),
    {value,read_and_add_records(File, Sel, Options, Bs, RT),Bs};
local_func(history, [{integer,_,N}], Bs, _Shell, _RT, _Lf, _Ef) ->
    {value,history(N),Bs};
local_func(history, [_Other], _Bs, _Shell, _RT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,history,1}]);
local_func(results, [{integer,_,N}], Bs, _Shell, _RT, _Lf, _Ef) ->
    {value,results(N),Bs};
local_func(results, [_Other], _Bs, _Shell, _RT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,results,1}]);
local_func(catch_exception, [{atom,_,Bool}], Bs, _Shell, _RT, _Lf, _Ef) 
                             when Bool; not Bool ->
    {value,catch_exception(Bool),Bs};
local_func(catch_exception, [_Other], _Bs, _Shell, _RT, _Lf, _Ef) ->
    erlang:raise(error, function_clause, [{shell,catch_exception,1}]);
local_func(exit, [], _Bs, Shell, _RT, _Lf, _Ef) ->
    shell_req(Shell, exit),			%This terminates us
    exit(normal);
local_func(F, As0, Bs0, _Shell, _RT, Lf, Ef) when is_atom(F) ->
    {As,Bs} = expr_list(As0, Bs0, Lf, Ef),
    non_builtin_local_func(F,As,Bs).

non_builtin_local_func(F,As,Bs) ->
    Arity = length(As),
    case erlang:function_exported(user_default, F, Arity) of
	true ->
            {eval,erlang:make_fun(user_default, F, Arity),As,Bs};
	false ->
	    shell_default(F,As,Bs)
    end.

shell_default(F,As,Bs) ->
    M = shell_default,
    A = length(As),
    case code:ensure_loaded(M) of
	{module, _} ->
	    case erlang:function_exported(M,F,A) of
		true ->
		    {eval,erlang:make_fun(M, F, A),As,Bs};
		false ->
		    shell_undef(F,A)
	    end;
	{error, _} ->
	    shell_undef(F,A)
    end.

shell_undef(F,A) ->
    erlang:error({shell_undef,F,A,[]}).

local_func_handler(Shell, RT, Ef) ->
    H = fun(Lf) -> 
                fun(F, As, Bs) -> 
                        local_func(F, As, Bs, Shell, RT, {eval,Lf(Lf)}, Ef) 
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
    Recs = [{Name,D} || {attribute,_,_,{Name,_}}=D <- RAs],
    Bs1 = record_bindings(Recs, Bs0),
    case check_command([], Bs1) of
        {error,{_Line,M,ErrDesc}} ->
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
	    {files,[File]};
	preloaded ->
	    {_M,_Bin,File} = code:get_object_code(Mod),
            {files,[File]};
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
            case beam_lib:chunks(File, [abstract_code,"CInf"]) of
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
            end;
        _ ->
            parse_file(File, Opts)
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
    Strings =
        case application:get_env(stdlib, shell_strings) of
            {ok, false} ->
                false;
            _ ->
                true
        end,
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
    case application:get_env(stdlib, V) of
	{ok, Val} when is_integer(Val), Val >= 0 ->
	    Val;
	_ ->
	    Def
    end.
	    
check_env(V) ->
    case application:get_env(stdlib, V) of
	undefined ->
	    ok;
	{ok, Val} when is_integer(Val), Val >= 0 ->
	    ok;
        {ok, Val} ->
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

-spec history(N) -> non_neg_integer() when
      N :: non_neg_integer().

history(L) when is_integer(L), L >= 0 ->
    set_env(stdlib, shell_history_length, L, ?DEF_HISTORY).

-spec results(N) -> non_neg_integer() when
      N :: non_neg_integer().

results(L) when is_integer(L), L >= 0 ->
    set_env(stdlib, shell_saved_results, L, ?DEF_RESULTS).

-spec catch_exception(Bool) -> boolean() when
      Bool :: boolean().

catch_exception(Bool) ->
    set_env(stdlib, shell_catch_exception, Bool, ?DEF_CATCH_EXCEPTION).

-spec prompt_func(PromptFunc) -> PromptFunc2 when
      PromptFunc :: 'default' | {module(),atom()},
      PromptFunc2 :: 'default' | {module(),atom()}.

prompt_func(String) ->
    set_env(stdlib, shell_prompt_func, String, ?DEF_PROMPT_FUNC).

-spec strings(Strings) -> Strings2 when
      Strings :: boolean(),
      Strings2 :: boolean().

strings(Strings) ->
    set_env(stdlib, shell_strings, Strings, ?DEF_STRINGS).
