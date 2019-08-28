%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-module(erl_compile_server).
-behaviour(gen_server).
-export([start_link/0, compile/1]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(COMPILE_SERVER, erl_compile_server).
-define(IDLE_TIMEOUT, (10*1000)).
-define(WRONG_TIMEOUT, 1).

-type client() :: {pid(), term()}.
-type job_map() :: #{{pid(),reference()} := client()}.

-record(st, {
          cwd=[] :: file:filename(),
          config :: term(),
          timeout=?IDLE_TIMEOUT :: non_neg_integer(),
          jobs=#{} :: job_map()
         }).

-type state() :: #st{}.

-spec start_link() -> {'ok', pid()} | {'error', term()}.

start_link() ->
    gen_server:start_link({local, ?COMPILE_SERVER}, ?MODULE, [], []).

-spec init(Arg :: []) -> {'ok', any(), timeout()}.

init([]) ->
    %% We don't want the current directory in the code path.
    %% Remove it.
    Path = [D || D <- code:get_path(), D =/= "."],
    true = code:set_path(Path),
    Config = init_config(),
    {ok, #st{config=Config}, ?IDLE_TIMEOUT}.

-spec compile(term()) -> term().

compile(Parameters) ->
    gen_server:call(?COMPILE_SERVER, {compile, Parameters}, infinity).

-spec handle_call(any(), _, state()) -> {'noreply', state()}.

handle_call({compile, Parameters}, From, #st{jobs=Jobs}=St0) ->
    {ErlcArgs, PathArgs} = parse_command_line(Parameters),
    case verify_context(PathArgs, Parameters, St0) of
        {ok, St1} ->
            #{cwd := Cwd, encoding := Enc} = Parameters,
            PidRef = spawn_monitor(fun() -> exit(do_compile(ErlcArgs, Cwd, Enc)) end),
            St = St1#st{jobs=Jobs#{PidRef => From}},
            {noreply, St#st{timeout=?IDLE_TIMEOUT}};
        wrong_config ->
            case map_size(Jobs) of
                0 ->
                    %% Wrong configuration and no outstanding jobs.
                    %% Terminate immediately.
                    halt();
                _ ->
                    {reply, wrong_config, St0#st{timeout=?WRONG_TIMEOUT}, ?WRONG_TIMEOUT}
            end
    end.

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast(_, St) ->
    {noreply, St}.

-spec handle_info(term(), state()) -> {'noreply', state(), timeout()}.

handle_info({'DOWN',Ref,process,Pid,Reason}, #st{jobs=Jobs0}=St0) ->
    Key = {Pid, Ref},
    Client = map_get(Key, Jobs0),
    Jobs = maps:remove(Key, Jobs0),
    St = St0#st{jobs=Jobs},
    gen_server:reply(Client, Reason),
    case map_size(Jobs) =:= 0 of
        true ->
            {noreply, St, St#st.timeout};
        false ->
            {noreply, St}
    end;
handle_info(timeout, #st{jobs=Jobs}) when map_size(Jobs) =:= 0 ->
    halt();
handle_info(_, #st{timeout=Timeout}=St) ->
    %% There are still outstanding jobs.
    {noreply, St, Timeout}.

%%%
%%% Local functions.
%%%

verify_context(PathArgs, #{env := Env}=Parameters, St0) ->
    case ensure_cwd(Parameters, St0) of
        {ok, #st{config=Config}=St} ->
            case make_config(PathArgs, Env) of
                Config ->
                    {ok, St};
                _ ->
                    wrong_config
            end;
        wrong_config ->
            wrong_config
    end.

ensure_cwd(#{cwd := Cwd}, #st{cwd=Cwd}=St) ->
    {ok, St};
ensure_cwd(#{cwd := NewCwd}, #st{jobs=Jobs}=St) when map_size(Jobs) =:= 0 ->
    ok = file:set_cwd(NewCwd),
    {ok, St#st{cwd=NewCwd}};
ensure_cwd(#{}, #st{}) ->
    wrong_config.

do_compile(ErlcArgs, Cwd, Enc) ->
    GL = create_gl(),
    group_leader(GL, self()),
    Result = erl_compile:compile(ErlcArgs, Cwd),
    StdOutput = ensure_enc(gl_get_output(GL), Enc),
    case Result of
        ok ->
            {ok, StdOutput};
        {error, StdErrorOutput0} ->
            StdErrorOutput = ensure_enc(StdErrorOutput0, Enc),
            {error, StdOutput, StdErrorOutput}
    end.

parse_command_line(#{command_line := CmdLine, cwd := Cwd}) ->
    parse_command_line_1(CmdLine, Cwd, [], []).

parse_command_line_1(["-pa", Pa|T], Cwd, PaAcc, PzAcc) ->
    parse_command_line_1(T, Cwd, [Pa|PaAcc], PzAcc);
parse_command_line_1(["-pz", Pz|T], Cwd, PaAcc, PzAcc) ->
    parse_command_line_1(T, Cwd, PaAcc, [Pz|PzAcc]);
parse_command_line_1(["-extra"|ErlcArgs], Cwd, PaAcc, PzAcc) ->
    PaArgs = clean_path_args(lists:reverse(PaAcc), Cwd),
    PzArgs = clean_path_args(lists:reverse(PzAcc), Cwd),
    {ErlcArgs, [{pa, PaArgs}, {pz, PzArgs}]};
parse_command_line_1([_|T], Cwd, PaAcc, PzAcc) ->
    parse_command_line_1(T, Cwd, PaAcc, PzAcc).

ensure_enc(Chars, latin1) ->
    L = unicode:characters_to_list(Chars, unicode),
    unicode:characters_to_binary(
      [ case X of
            High when High > 255 ->
                ["\\x{", erlang:integer_to_list(X, 16), $}];
            Low ->
                Low
        end || X <- L ], unicode, latin1);
ensure_enc(Chars, _Enc) -> Chars.

init_config() ->
    EnvVars = ["ERL_AFLAGS", "ERL_FLAGS", "ERL_ZFLAGS",
               "ERL_COMPILER_OPTIONS",
               "ERL_LIBS",
               "ERLC_CONFIGURATION"],
    Env0 = [{Name, os:getenv(Name)} || Name <- EnvVars],
    Env = [P || {_, Val}=P <- Env0, Val =/= false],
    {ok, Cwd} = file:get_cwd(),
    make_config([get_path_arg(pa, Cwd), get_path_arg(pz, Cwd)], Env).

get_path_arg(PathArg, Cwd) ->
    case init:get_argument(PathArg) of
        error ->
            {PathArg, []};
        {ok, Paths0} ->
            Paths1 = lists:append(Paths0),
            Paths = clean_path_args(Paths1, Cwd),
            {PathArg, Paths}
    end.

clean_path_args(PathArgs, Cwd) ->
    [filename:absname(P, Cwd) || P <- PathArgs].

make_config(PathArgs, Env0) ->
    Env = lists:sort(Env0),
    PathArgs ++ [iolist_to_binary([[Name,$=,Val,$\n] || {Name,Val} <- Env])].

%%%
%%% A group leader that will capture all output to the group leader.
%%%

create_gl() ->
    spawn_link(fun() -> gl_loop([]) end).

gl_get_output(GL) ->
    GL ! {self(), get_output},
    receive
        {GL, Output} -> Output
    end.

gl_loop(State0) ->
    receive
	{io_request, From, ReplyAs, Request} ->
            {_Tag, Reply, State} = gl_request(Request, State0),
            gl_reply(From, ReplyAs, Reply),
            gl_loop(State);
	{From, get_output} ->
            Output = iolist_to_binary(State0),
	    From ! {self(), Output},
	    gl_loop(State0);
	_Unknown ->
	    gl_loop(State0)
    end.

gl_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply},
    ok.

gl_request({put_chars, Encoding, Chars}, State) ->
    gl_put_chars(unicode:characters_to_binary(Chars, Encoding), State);
gl_request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
	gl_request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch
	_:_ ->
	    {{error,Function}, State}
    end;
gl_request({requests, Reqs}, State) ->
    gl_multi_request(Reqs, {ok, State});
gl_request(_Other, State) ->
    {error, {error, request}, State}.

gl_multi_request([R|Rs], {ok, State}) ->
    gl_multi_request(Rs, gl_request(R, State));
gl_multi_request([_|_], Error) ->
    Error;
gl_multi_request([], Result) ->
    Result.

gl_put_chars(Chars, Output) ->
    {ok, ok, [Output,Chars]}.
