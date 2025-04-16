%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
-module(user_drv).
-moduledoc false.

%% Basic interface to stdin/stdout.
%%
%% This is responsible for a couple of things:
%%   - Dispatching I/O messages when erl is running
%%     The messages are listed in the type message/0.
%%   - Any data received from the terminal is sent to the current group like this:
%%     `{DrvPid :: pid(), {data, UnicodeCharacters :: list()}}`
%%   - It serves as the job control manager (i.e. what happens when you type ^G)
%%   - Starts potential -remsh sessions to other nodes
%%
-type message() ::
        %% I/O requests that modify the terminal
        {Sender :: pid(), request()} |
        %% Query the server of the current dimensions of the terminal.
        %% `Sender` will be sent the message:
        %%   `{DrvPid :: pid(), tty_geometry, {Width :: integer(), Height :: integer()}}`
        {Sender :: pid(), tty_geometry} |
        %% Query the server if it supports unicode characters
        %% `Sender` will be sent the message:
        %%   `{DrvPid :: pid(), get_unicode_state, SupportUnicode :: boolean()}`
        {Sender :: pid(), get_unicode_state} |
        %% Change whether the server supports unicode characters or not. The reply
        %% contains the previous unicode state.
        %% `Sender` will be sent the message:
        %%   `{DrvPid :: pid(), set_unicode_state, SupportedUnicode :: boolean()}`
        {Sender :: pid(), set_unicode_state, boolean()}.
-type request() ::
        %% Put characters above the prompt
        {put_chars, unicode, binary()} |
        %% Same as put_chars/3, but sends Reply to From when the characters are
        %% guaranteed to have been written to the terminal
        {put_chars_sync, unicode, binary(), {From :: pid(), Reply :: term()}} |
        %% Output raw binary, should only be called if output mode is set to raw
        %% and encoding set to latin1.
        {put_chars_sync, latin1, binary(), {From :: pid(), Reply :: term()}} |
        %% Put text in expansion area
        {put_expand, unicode, binary(), integer()} |
        {move_expand, -32768..32767} |
        %% Move the cursor X characters left or right (negative is left)
        {move_rel, -32768..32767} |
        %% Move the cursor Y rows up or down (negative is up)
        {move_line, -32768..32767} |
        %% Move combo, helper to simplify some move operations
        {move_combo, -32768..32767, -32768..32767, -32768..32767} |
        %% Insert characters at current cursor position moving any
        %% characters after the cursor.
        {insert_chars, unicode, binary()} |
        %% Put characters at current cursor position,
        %% overwriting any characters it encounters.
        {insert_chars_over, unicode, binary()} |
        %% Delete X chars before or after the cursor adjusting any test remaining
        %% to the right of the cursor.
        {delete_chars, -32768..32767} |
        %% Deletes the current prompt and expression
        delete_line |
        %% Delete after the cursor
        delete_after_cursor |
        %% Trigger a terminal "bell"
        beep |
        %% Clears the screen
        clear |
        %% Execute multiple request() actions
        {requests, [request()]} |
        %% Open external editor
        {open_editor, string()} |
        %% Redraws the current prompt and expression
        redraw_prompt |
        {redraw_prompt, string(), string(), tuple()} |
        %% Clears the state, not touching the characters
        new_prompt.

-export_type([message/0, request/0]).
-export([start/0, start/1, start_shell/0, start_shell/1, whereis_group/0]).

%% gen_statem state callbacks
-behaviour(gen_statem).
-export([init/3,server/3,switch_loop/3]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).

-include_lib("kernel/include/logger.hrl").

-record(editor, { port :: port(), file :: file:name(), requester :: pid() }).
-record(state, { tty :: prim_tty:state() | undefined,
                 write :: reference() | undefined,
                 read :: reference() | undefined,
                 terminal_mode :: raw | cooked | disabled,
                 shell_started = new :: new | old | false,
                 editor :: #editor{} | undefined,
                 user :: pid(),
                 current_group :: pid() | undefined,
                 groups, queue }).

-type shell() :: {module(), atom(), [term()]} | {node(), module(), atom(), [term()]}.
-type arguments() ::
        #{ initial_shell => noshell | shell() |
           {remote, unicode:charlist()} | {remote, unicode:charlist(), {module(), atom(), [term()]}},
           input => cooked | raw | disabled }.

%% Default line editing shell
-spec start() -> pid().
start() ->
    case init:get_argument(remsh) of
        {ok,[[Node]]} ->
            start(#{ initial_shell => {remote, Node} });
        {ok,[[Node]|_]} ->
            ?LOG_WARNING("Multiple -remsh given to erl, using the first, ~p", [Node]),
            start(#{ initial_shell => {remote, Node} });
        E when E =:= error ; E =:= {ok,[[]]} ->
            start(#{ })
    end.

-spec start_shell() -> ok | {error, Reason :: term()}.
start_shell() ->
    start_shell(#{ }).
-spec start_shell(arguments()) -> ok | {error, already_started}.
start_shell(Args) ->
    gen_statem:call(?MODULE, {start_shell, Args}).

-spec whereis_group() -> pid() | undefined.
whereis_group() ->
    {dictionary, Dict} =
        erlang:process_info(whereis(?MODULE), dictionary),
    proplists:get_value(current_group, Dict).

%% Backwards compatibility with pre OTP-26 for Elixir/LFE etc
-spec start(['tty_sl -c -e'| shell()]) -> pid();
           (arguments()) -> pid().
start(['tty_sl -c -e', Shell]) ->
    start(#{ initial_shell => Shell });
start(Args) when is_map(Args) ->
    case gen_statem:start({local, ?MODULE}, ?MODULE, Args, []) of
        {ok, Pid} -> Pid;
        {error, Reason} ->
            {error, Reason}
    end.

callback_mode() -> state_functions.

-spec init(arguments()) -> gen_statem:init_result(init).
init(Args) ->
    process_flag(trap_exit, true),

    ok = prim_tty:load(),

    IsTTY = prim_tty:isatty(stdin) =:= true andalso prim_tty:isatty(stdout) =:= true,
    StartShell = maps:get(initial_shell, Args, undefined) =/= noshell,
    OldShell = maps:get(initial_shell, Args, undefined) =:= oldshell,

    try
        if
            not IsTTY andalso StartShell; OldShell ->
                error(enotsup);
            IsTTY, StartShell ->
                TTYState = prim_tty:init(#{ input => raw,
                                            output => cooked }),
                init_standard_error(TTYState, true),
                {ok, init, {Args, #state{ terminal_mode = raw, user = start_user() } },
                 {next_event, internal, TTYState}};
            true ->
                TTYState = prim_tty:init(
                             #{ input => maps:get(input, Args), output => raw }),
                init_standard_error(TTYState, false),
                {ok, init, {Args,#state{ terminal_mode = maps:get(input, Args), user = start_user() } },
                 {next_event, internal, TTYState}}
        end
    catch error:enotsup ->
            %% This is thrown by prim_tty:init when
            %% it could not start the terminal,
            %% probably because TERM=dumb was set.
            %%
            %% The oldshell mode is important as it is
            %% the mode used when running erlang in an
            %% emacs buffer.
            CatchTTYState = prim_tty:init(#{ input => cooked, output => raw }),
            init_standard_error(CatchTTYState, false),
            {ok, init, {Args,#state{ terminal_mode = cooked, shell_started = old, user = start_user() } },
             {next_event, internal, CatchTTYState}}
    end.

%% Initialize standard_error
init_standard_error(TTY, NewlineCarriageReturn) ->
    Encoding = case prim_tty:unicode(TTY) of
                   true -> unicode;
                   false -> latin1
               end,
    ok = io:setopts(standard_error, [{encoding, Encoding},
                                     {onlcr, NewlineCarriageReturn}]).

init(internal, TTYState, {Args, State = #state{ user = User }}) ->

    %% Cleanup ancestors so that observer looks nice
    put('$ancestors',[User|get('$ancestors')]),

    #{ read := ReadHandle, write := WriteHandle } = prim_tty:handles(TTYState),

    NewState = State#state{ tty = TTYState,
                            read = ReadHandle, write = WriteHandle,
                            user = User, queue = {false, queue:new()},
                            groups = gr_add_cur(gr_new(), User, {})
                          },

    case Args of
        #{ initial_shell := noshell } ->
            init_noshell(NewState);
        #{ initial_shell := {remote, Node} } ->
            InitialShell = {shell,start,[]},
            exit_on_remote_shell_error(
              Node, InitialShell, init_remote_shell(NewState, Node, InitialShell));
        #{ initial_shell := {remote, Node, InitialShell} } ->
            exit_on_remote_shell_error(
              Node, InitialShell, init_remote_shell(NewState, Node, InitialShell));
        #{ initial_shell := oldshell } ->
            old = State#state.shell_started,
            init_local_shell(NewState, {shell,start,[]});
        #{ initial_shell := InitialShell } ->
            init_local_shell(NewState, InitialShell);
        _ ->
            init_local_shell(NewState, {shell,start,[init]})
    end.

exit_on_remote_shell_error(RemoteNode, _, {error, noconnection}) ->
    io:format(standard_error, "Could not connect to ~p\n", [RemoteNode]),
    erlang:halt(1);
exit_on_remote_shell_error(RemoteNode, {M, _, _}, {error, Reason}) ->
    io:format(standard_error, "Could not load ~p on ~p (~p)\n", [RemoteNode, M, Reason]),
    erlang:halt(1);
exit_on_remote_shell_error(_, _, Result) ->
    Result.

%% We have been started with -noshell. In this mode the current_group is
%% the `user` group process.
init_noshell(State) ->
    State#state.user ! {self(), terminal_mode, State#state.terminal_mode},
    init_shell(State#state{ shell_started = false }, "").

init_remote_shell(State, Node, {M, F, A}) ->

    case net_kernel:get_state() of
        #{ started := no } ->
            {ok, _} = net_kernel:start([undefined, shortnames]),
            ok;
        _ ->
            ok
    end,

    LocalNode =
        case net_kernel:get_state() of
            #{ name_type := dynamic } ->
                net_kernel:nodename();
            #{ name_type := static } ->
                node()
        end,

    RemoteNode =
        case string:find(Node,"@") of
            nomatch ->
                list_to_atom(Node ++ string:find(atom_to_list(LocalNode),"@"));
            _ ->
                list_to_atom(Node)
        end,

    case net_kernel:connect_node(RemoteNode) of
        true ->

            case erpc:call(RemoteNode, code, ensure_loaded, [M]) of
                {error, Reason} when Reason =/= embedded ->
                    {error, Reason};
                _ ->

                    %% Setup correct net tick times
                    case erpc:call(RemoteNode, net_kernel, get_net_ticktime, []) of
                        {ongoing_change_to, NetTickTime} ->
                            _ = net_kernel:set_net_ticktime(NetTickTime),
                            ok;
                        NetTickTime ->
                            _ = net_kernel:set_net_ticktime(NetTickTime),
                            ok
                    end,

                    RShell = {RemoteNode, M, F, A},

                    %% We fetch the shell slogan from the remote node
                    Slogan =
                        case erpc:call(RemoteNode, application, get_env,
                                       [stdlib, shell_slogan,
                                        erpc:call(RemoteNode, erlang, system_info, [system_version])]) of
                            Fun when is_function(Fun, 0) ->
                                erpc:call(RemoteNode, Fun);
                            SloganEnv ->
                                SloganEnv
                        end,

                    Group = group:start(self(), RShell,
                                        [{dumb, State#state.shell_started =/= new}] ++
                                            group_opts(RemoteNode)),

                    Gr = gr_add_cur(State#state.groups, Group, RShell),

                    init_shell(State#state{ groups = Gr }, [Slogan,$\n])
            end;
        false ->
            {error, noconnection}
    end.

init_local_shell(State, InitialShell) ->
  Slogan =
      case application:get_env(
             stdlib, shell_slogan,
             fun() -> erlang:system_info(system_version) end) of
          Fun when is_function(Fun, 0) ->
              Fun();
          SloganEnv ->
              SloganEnv
      end,

    Gr = gr_add_cur(State#state.groups,
                    group:start(self(), InitialShell,
                                group_opts() ++ [{dumb,State#state.shell_started =/= new}]),
                    InitialShell),

    init_shell(State#state{ groups = Gr }, [Slogan,$\n]).

init_shell(State, Slogan) ->

    init_standard_error(State#state.tty, State#state.terminal_mode =:= raw),

    %% Tell the reader to read greedily if there is a shell
    [prim_tty:read(State#state.tty) || State#state.shell_started =/= false],

    Curr = gr_cur_pid(State#state.groups),
    put(current_group, Curr),
    {next_state, server, State#state{ current_group = gr_cur_pid(State#state.groups) },
     {next_event, info,
      {gr_cur_pid(State#state.groups),
       {put_chars, unicode,
        unicode:characters_to_binary(io_lib:format("~ts", [Slogan]))}}}}.

%% start_user()
%%  Start a group leader process and register it as 'user', unless,
%%  of course, a 'user' already exists.
start_user() ->
    case whereis(user) of
	undefined ->
	    group:start(self(), noshell, [{name, user}]);
	User ->
	    User
    end.

server({call, From}, {start_shell, Args},
       State = #state{ tty = TTY, shell_started = false }) ->
    IsTTY = prim_tty:isatty(stdin) =:= true andalso prim_tty:isatty(stdout) =:= true,
    StartShell = maps:get(initial_shell, Args, undefined) =/= noshell,
    OldShell = maps:get(initial_shell, Args, undefined) =:= oldshell,
    NewState =
        try
            if
                not IsTTY andalso StartShell; OldShell ->
                    error(enotsup);
                IsTTY, StartShell ->
                    NewTTY = prim_tty:reinit(TTY, #{ input => raw, output => cooked }),
                    State#state{ tty = NewTTY, terminal_mode = raw, shell_started = new };
                not StartShell ->
                    Input = maps:get(input, Args),
                    if not IsTTY andalso Input =:= raw ->
                            error(enotsup);
                       true ->
                            State#state{
                              terminal_mode = Input,
                              tty = prim_tty:reinit(TTY, #{ input => Input,
                                                            output => raw }),
                              shell_started = false }
                    end
            end
        catch error:enotsup ->
                NewTTYState = prim_tty:reinit(TTY, #{ input => cooked, output => raw }),
                Shell = if StartShell ->
                                old;
                           true ->
                                false
                        end,
                State#state{ terminal_mode = cooked, tty = NewTTYState, shell_started = Shell }
        end,
    #{ read := ReadHandle, write := WriteHandle } = prim_tty:handles(NewState#state.tty),
    NewHandleState = NewState#state {
                       read = ReadHandle,
                       write = WriteHandle
                      },
    {Result, Reply}
        = case maps:get(initial_shell, Args, undefined) of
              noshell ->
                  case maps:get(input, Args) =:= NewHandleState#state.terminal_mode of
                    true ->
                        {init_noshell(NewHandleState), ok};
                    false ->
                        {init_noshell(NewHandleState), {error, enotsup}}
                    end;
              {remote, Node} ->
                  case init_remote_shell(NewHandleState, Node, {shell, start, []}) of
                      {error, _} = Error ->
                          {init_noshell(NewHandleState), Error};
                      R ->
                          {R, ok}
                  end;
              {remote, Node, InitialShell} ->
                  case init_remote_shell(NewHandleState, Node, InitialShell) of
                      {error, _} = Error ->
                          {init_noshell(NewHandleState), Error};
                      R ->
                          {R, ok}
                  end;
              undefined ->
                  case NewHandleState#state.shell_started of
                      old ->
                          {init_local_shell(NewHandleState, {shell,start,[]}), ok};
                      new ->
                          {init_local_shell(NewHandleState, {shell,start,[init]}), ok};
                      false ->
                          %% This can never happen, but dialyzer complains so we add
                          %% this clause.
                          {keep_state_and_data, ok}
                  end;
              InitialShell ->
                  {init_local_shell(NewHandleState, InitialShell), ok}
        end,
    gen_statem:reply(From, Reply),
    Result;
server({call, From}, {start_shell, _Args}, _State) ->
    gen_statem:reply(From, {error, already_started}),
    keep_state_and_data;
server(info, {ReadHandle,{data,UTF8Binary}}, State = #state{ read = ReadHandle })
  when State#state.current_group =:= State#state.user ->
    State#state.current_group ! {self(), {data,UTF8Binary}},
    keep_state_and_data;
server(info, {ReadHandle,{data,UTF8Binary}}, State = #state{ read = ReadHandle }) ->
    case contains_ctrl_g_or_ctrl_c(UTF8Binary) of
        ctrl_g -> {next_state, switch_loop, State, {next_event, internal, init}};
        ctrl_c ->
            case gr_get_info(State#state.groups, State#state.current_group) of
                undefined -> ok;
                _ -> exit(State#state.current_group, interrupt)
            end,
            keep_state_and_data;
        none ->
            State#state.current_group !
                {self(), {data, UTF8Binary}},
            keep_state_and_data
    end;
server(info, {ReadHandle,eof}, State = #state{ read = ReadHandle }) ->
    State#state.current_group ! {self(), eof},
    {keep_state, State#state{ read = undefined }};
server(info,{ReadHandle,{signal,Signal}}, State = #state{ tty = TTYState, read = ReadHandle }) ->
    {keep_state, State#state{ tty = prim_tty:handle_signal(TTYState, Signal) }};

server(info, {Requester, read, N}, State = #state{ tty = TTYState })
  when Requester =:= State#state.current_group ->
    %% Only allowed when current_group == user
    true = State#state.current_group =:= State#state.user,
    ok = prim_tty:read(TTYState, N),
    keep_state_and_data;

server(info, {Requester, read, _N}, _State) ->
    Requester ! {self(), {error, enotsup}},
    keep_state_and_data;

server(info, {Requester, tty_geometry}, #state{ tty = TTYState }) ->
    case prim_tty:window_size(TTYState) of
        {ok, Geometry} ->
            Requester ! {self(), tty_geometry, Geometry},
            ok;
        Error ->
            Requester ! {self(), tty_geometry, Error},
            ok
    end,
    keep_state_and_data;
server(info, {Requester, get_unicode_state}, #state{ tty = TTYState }) ->
    Requester ! {self(), get_unicode_state, prim_tty:unicode(TTYState) },
    keep_state_and_data;
server(info, {Requester, set_unicode_state, Bool}, #state{ tty = TTYState } = State) ->
    OldUnicode = prim_tty:unicode(TTYState),
    NewTTYState = prim_tty:unicode(TTYState, Bool),
    ok = io:setopts(standard_error,[{encoding, if Bool -> unicode; true -> latin1 end}]),
    Requester ! {self(), set_unicode_state, OldUnicode},
    {keep_state, State#state{ tty = NewTTYState }};
server(info, {Requester, get_terminal_state}, _State) ->
    Requester ! {self(), get_terminal_state, #{ stdin => prim_tty:isatty(stdin),
                                                stdout => prim_tty:isatty(stdout),
                                                stderr => prim_tty:isatty(stderr) } },
    keep_state_and_data;
server(info, {Requester, {open_editor, Buffer}}, #state{tty = TTYState } = State) ->
    case open_editor(TTYState, Buffer) of
        false ->
            Requester ! {self(), {editor_data, Buffer}},
            keep_state_and_data;
        {EditorPort, TmpPath} ->
            {keep_state, State#state{ editor = #editor{ port = EditorPort,
                                                        file = TmpPath,
                                                        requester = Requester }}}
    end;
server(info, Req, State = #state{ user = User, current_group = Curr, editor = undefined })
  when element(1,Req) =:= User orelse element(1,Req) =:= Curr,
       tuple_size(Req) =:= 2 orelse tuple_size(Req) =:= 3 ->
    %% We match {User|Curr,_}|{User|Curr,_,_}
    {NewTTYState, NewQueue} = handle_req(Req, State#state.tty, State#state.queue),
    {keep_state, State#state{ tty = NewTTYState, queue = NewQueue }};
server(info, {WriteRef, ok}, State = #state{ write = WriteRef,
                                             queue = {{Origin, MonitorRef, Reply}, IOQ} }) ->
    %% We get this ok from the user_drv_writer, in io_request we store
    %% info about where to send reply at head of queue
    Origin ! {reply, Reply, ok},
    erlang:demonitor(MonitorRef, [flush]),
    {NewTTYState, NewQueue} = handle_req(next, State#state.tty, {false, IOQ}),
    {keep_state, State#state{ tty = NewTTYState, queue = NewQueue }};
server(info, {'DOWN', MonitorRef, _, _, Reason},
       #state{ queue = {{Origin, MonitorRef, Reply}, _IOQ} }) ->
    %% The writer process died, we send the correct error to the caller and
    %% then stop this process. This will bring down all linked groups (including 'user').
    %% All writes from now on will throw badarg terminated.
    Origin ! {reply, Reply, {error, Reason}},
    ?LOG_INFO("Failed to write to standard out (~p)", [Reason]),
    stop;
server(info,{Requester, {put_chars_sync, _, _, Reply}}, _State) ->
    %% This is a sync request from an unknown or inactive group.
    %% We need to ack the Req otherwise originating process will hang forever.
    %% We discard the output to non visible shells
    Requester ! {reply, Reply, ok},
    keep_state_and_data;

server(info,{'EXIT',User, shutdown}, #state{ user = User }) ->
    keep_state_and_data;
server(info,{'EXIT',User, _Reason}, State = #state{ user = User }) ->
    NewUser = start_user(),
    {keep_state, State#state{ user = NewUser,
                              groups = gr_set_num(State#state.groups, 1, NewUser, {})}};
server(info, {'EXIT', EditorPort, _R},
       State = #state{tty = TTYState,
                      editor = #editor{ requester = Requester,
                                        port = EditorPort,
                                        file = PathTmp}}) ->
    {ok, Content} = file:read_file(PathTmp),
    _ = file:del_dir_r(PathTmp),
    Unicode = case unicode:characters_to_list(Content,unicode) of
                  {error, _, _} -> unicode:characters_to_list(
                                     unicode:characters_to_list(Content,latin1), unicode);
                  U -> U
              end,
    Requester ! {self(), {editor_data, string:chomp(Unicode)}},
    ok = prim_tty:enable_reader(TTYState),
    {keep_state, State#state{editor = undefined}};
server(info,{'EXIT', Group, Reason}, State) ->
    case gr_get_info(State#state.groups, Group) of
        undefined when Reason =:= normal ->
            keep_state_and_data;
        undefined ->
            Rdr = [?LOG_ERROR("Reader crashed (~p)", [Reason]) || prim_tty:is_reader(State#state.tty, Group)],
            Wrt = [?LOG_ERROR("Writer crashed (~p)", [Reason]) || prim_tty:is_writer(State#state.tty, Group)],
            case Rdr ++ Wrt of
                [] ->
                    keep_state_and_data;
                _ ->
                    stop
            end;
        GroupInfo ->  % shell and group leader exit
            case gr_cur_pid(State#state.groups) of
                Group when Reason =/= die, Reason =/= terminated  ->	% current shell exited
                    Reqs = [if
                                Reason =/= normal ->
                                    {put_chars,unicode,<<"*** ERROR: ">>};
                                true -> % exit not caused by error
                                    {put_chars,unicode,<<"*** ">>}
                            end,
                            {put_chars,unicode,<<"Shell process terminated! ">>}],
                    Gr1 = gr_del_pid(State#state.groups, Group),
                    case GroupInfo of
                        {Ix,{shell,start,Params}} -> % 3-tuple == local shell
                            NewTTyState = io_requests(Reqs ++ [{put_chars,unicode,<<"***\n">>}],
                                                      State#state.tty),
                            %% restart group leader and shell, same index
                            NewGroup = group:start(self(), {shell,start,Params}),
                            {ok,Gr2} = gr_set_cur(gr_set_num(Gr1, Ix, NewGroup,
                                                             {shell,start,Params}), Ix),
                            {keep_state, State#state{ tty = NewTTyState,
                                                      current_group = NewGroup,
                                                      groups = Gr2 }};
                        _ -> % remote shell
                            NewTTYState = io_requests(
                                            Reqs ++ [{put_chars,unicode,<<"(^G to start new job) ***\n">>}],
                                            State#state.tty),
                            {keep_state, State#state{ tty = NewTTYState, groups = Gr1 }}
                    end;
                _ ->
                    {keep_state, State#state{ groups = gr_del_pid(State#state.groups, Group) }}
            end
    end;
server(_, _, _) ->
    keep_state_and_data.

contains_ctrl_g_or_ctrl_c(<<$\^G,_/binary>>) ->
    ctrl_g;
contains_ctrl_g_or_ctrl_c(<<$\^C,_/binary>>) ->
    ctrl_c;
contains_ctrl_g_or_ctrl_c(<<_/utf8,T/binary>>) ->
    contains_ctrl_g_or_ctrl_c(T);
contains_ctrl_g_or_ctrl_c(<<>>) ->
    none.

switch_loop(internal, init, State) ->
    case application:get_env(stdlib, shell_esc, jcl) of
	abort ->
	    CurrGroup = gr_cur_pid(State#state.groups),
	    exit(CurrGroup, die),
	    Gr1 =
		case gr_get_info(State#state.groups, CurrGroup) of
		    {_Ix,{}} ->	% no shell
			State#state.groups;
		    _ ->
			receive {'EXIT',CurrGroup,_} ->
				gr_del_pid(State#state.groups, CurrGroup)
			after 1000 ->
				State#state.groups
			end
		end,
	    NewGroup = group:start(self(), {shell,start,[]}),
            NewTTYState = io_requests([{insert_chars,unicode,<<"\n">>}], State#state.tty),
            {next_state, server,
             State#state{ tty = NewTTYState,
                          groups = gr_add_cur(Gr1, NewGroup, {shell,start,[]})}};
	jcl ->
            NewTTYState =
                io_requests([{insert_chars,unicode,<<"\nUser switch command (enter 'h' for help)\n">>}],
                            State#state.tty),
	    %% init edlin used by switch command and have it copy the
	    %% text buffer from current group process
	    edlin:init(gr_cur_pid(State#state.groups)),
            {keep_state, State#state{ tty = NewTTYState },
             {next_event, internal, line}}
    end;
switch_loop(internal, line, State) ->
    {more_chars, Cont, Rs} = edlin:start(" --> "),
    {keep_state, {Cont, State#state{ tty = io_requests(Rs, State#state.tty) }}};
switch_loop(internal, {line, Line}, State) ->
    case erl_scan:string(Line) of
        {ok, Tokens, _} ->
            case switch_cmd(Tokens, State#state.groups, State#state.shell_started =/= new) of
                {ok, Groups} ->
                    Curr = gr_cur_pid(Groups),
                    put(current_group, Curr),
                    Curr ! {self(), activate},
                    {next_state, server,
                        State#state{ current_group = Curr, groups = Groups,
                                     tty = io_requests([{insert_chars, unicode, <<"\n">>},new_prompt], State#state.tty)}};
                {retry, Requests} ->
                    {keep_state, State#state{ tty = io_requests([{insert_chars, unicode, <<"\n">>},new_prompt|Requests], State#state.tty) },
                     {next_event, internal, line}};
                {retry, Requests, Groups} ->
                    Curr = gr_cur_pid(Groups),
                    put(current_group, Curr),
                    {keep_state, State#state{
                                   tty = io_requests([{insert_chars, unicode, <<"\n">>},new_prompt|Requests], State#state.tty),
                                   current_group = Curr,
                                   groups = Groups },
                     {next_event, internal, line}}
            end;
        {error, _, _} ->
            NewTTYState =
                io_requests([{insert_chars,unicode,<<"Illegal input\n">>}], State#state.tty),
            {keep_state, State#state{ tty = NewTTYState },
             {next_event, internal, line}}
    end;
switch_loop(info,{ReadHandle,{data,Cs}}, {Cont, #state{ read = ReadHandle } = State}) ->
    case edlin:edit_line(unicode:characters_to_list(Cs), Cont) of
        {done,{[Line],_,_},_Rest, Rs} ->
            {keep_state, State#state{ tty = io_requests(Rs, State#state.tty) },
             {next_event, internal, {line, Line}}};
        {more_chars,NewCont,Rs} ->
            {keep_state,
             {NewCont, State#state{ tty = io_requests(Rs, State#state.tty)}}};
        {blink,NewCont,Rs} ->
            {keep_state,
             {NewCont, State#state{ tty = io_requests(Rs, State#state.tty)}},
             1000};
        {_What, _Cs, _NewCont, _Rs}  ->
            keep_state_and_data;
        {_What, _Bef, _Cs, _NewCont, _Rs}  ->
            keep_state_and_data
    end;
switch_loop(info, {Requester, get_unicode_state}, {_Cont, #state{ tty = TTYState }}) ->
    Requester ! {self(), get_unicode_state, prim_tty:unicode(TTYState) },
    keep_state_and_data;
switch_loop(info, {Requester, get_terminal_state}, _State) ->
    Requester ! {self(), get_terminal_state, #{ stdin => prim_tty:isatty(stdin),
                                                stdout => prim_tty:isatty(stdout),
                                                stderr => prim_tty:isatty(stderr) } },
    keep_state_and_data;
switch_loop(info, {Requester, tty_geometry}, {_Cont, #state{ tty = TTYState }}) ->
    case prim_tty:window_size(TTYState) of
        {ok, Geometry} ->
            Requester ! {self(), tty_geometry, Geometry},
            ok;
        Error ->
            Requester ! {self(), tty_geometry, Error},
            ok
    end,
    keep_state_and_data;
switch_loop(timeout, _, {_Cont, State}) ->
    {keep_state_and_data,
     {next_event, info, {State#state.read,{data,[]}}}};
switch_loop(info, _Unknown, _State) ->
    {keep_state_and_data, postpone}.

switch_cmd([{atom,_,Key},{Type,_,Value}], Gr, Dumb)
  when Type =:= atom; Type =:= integer ->
    switch_cmd({Key, Value}, Gr, Dumb);
switch_cmd([{atom,_,Key},{atom,_,V1},{atom,_,V2}], Gr, Dumb) ->
    switch_cmd({Key, V1, V2}, Gr, Dumb);
switch_cmd([{atom,_,Key}], Gr, Dumb) ->
    switch_cmd(Key, Gr, Dumb);
switch_cmd([{'?',_}], Gr, Dumb) ->
    switch_cmd(h, Gr, Dumb);

switch_cmd(Cmd, Gr, Dumb) when Cmd =:= c; Cmd =:= i; Cmd =:= k ->
    switch_cmd({Cmd, gr_cur_index(Gr)}, Gr, Dumb);
switch_cmd({c, I}, Gr0, _Dumb) ->
    case gr_set_cur(Gr0, I) of
	{ok,Gr} -> {ok, Gr};
	undefined -> unknown_group()
    end;
switch_cmd({i, I}, Gr, _Dumb) ->
    case gr_get_num(Gr, I) of
        {pid,Pid} ->
            exit(Pid, interrupt),

            {retry, [{put_chars, unicode,
                      unicode:characters_to_binary(
                        io_lib:format("Interrupted job ~p, enter 'c' to resume.~n",[I]))}]};
        undefined ->
            unknown_group()
    end;
switch_cmd({k, I}, Gr, _Dumb) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, die),
	    case gr_get_info(Gr, Pid) of
		{_Ix,{}} ->			% no shell
		    retry;
		_ ->
                    receive {'EXIT',Pid,_} ->
                            {retry,[],gr_del_pid(Gr, Pid)}
                    after 1000 ->
                            {retry,[],Gr}
                    end
	    end;
	undefined ->
	    unknown_group()
    end;
switch_cmd(j, Gr, _Dumb) ->
    {retry, gr_list(Gr)};
switch_cmd({s, Shell}, Gr0, Dumb) when is_atom(Shell) ->
    Pid = group:start(self(), {Shell,start,[]}, [{dumb, Dumb} | group_opts()]),
    Gr = gr_add_cur(Gr0, Pid, {Shell,start,[]}),
    {retry, [], Gr};
switch_cmd(s, Gr, Dumb) ->
    switch_cmd({s, shell}, Gr, Dumb);
switch_cmd(r, Gr0, _Dumb) ->
    case is_alive() of
	true ->
	    Node = pool:get_node(),
	    Pid = group:start(self(), {Node,shell,start,[]}, group_opts(Node)),
	    Gr = gr_add_cur(Gr0, Pid, {Node,shell,start,[]}),
	    {retry, [], Gr};
	false ->
	    {retry, [{put_chars,unicode,<<"Node is not alive\n">>}]}
    end;
switch_cmd({r, Node}, Gr, Dumb) when is_atom(Node)->
    switch_cmd({r, Node, shell}, Gr, Dumb);
switch_cmd({r,Node,Shell}, Gr0, Dumb) when is_atom(Node), is_atom(Shell) ->
    case is_alive() of
	true ->
            case net_kernel:connect_node(Node) of
                true ->
                    Pid = group:start(self(), {Node,Shell,start,[]}, [{dumb, Dumb} | group_opts(Node)]),
                    Gr = gr_add_cur(Gr0, Pid, {Node,Shell,start,[]}),
                    {retry, [], Gr};
                false ->
                    {retry, [{put_chars,unicode,<<"Could not connect to node\n">>}]}
            end;
        false ->
            {retry, [{put_chars,unicode,"Node is not alive\n"}]}
    end;

switch_cmd(q, _Gr, _Dumb) ->
    case erlang:system_info(break_ignored) of
	true ->					% noop
	    {retry, [{put_chars,unicode,<<"Unknown command\n">>}]};
	false ->
	    halt()
    end;
switch_cmd(h, _Gr, _Dumb) ->
    {retry, list_commands()};
switch_cmd([], _Gr, _Dumb) ->
    {retry,[]};
switch_cmd(_Ts, _Gr, _Dumb) ->
    {retry, [{put_chars,unicode,<<"Unknown command\n">>}]}.

unknown_group() ->
    {retry,[{put_chars,unicode,<<"Unknown job\n">>}]}.

list_commands() ->
    QuitReq = case erlang:system_info(break_ignored) of
		  true ->
		      [];
		  false ->
		      [{put_chars, unicode,<<"  q                 - quit erlang\n">>}]
	      end,
    [{put_chars, unicode,<<"  c [nn]            - connect to job\n">>},
     {put_chars, unicode,<<"  i [nn]            - interrupt job\n">>},
     {put_chars, unicode,<<"  k [nn]            - kill job\n">>},
     {put_chars, unicode,<<"  j                 - list all jobs\n">>},
     {put_chars, unicode,<<"  s [shell]         - start local shell\n">>},
     {put_chars, unicode,<<"  r [node [shell]]  - start remote shell\n">>}] ++
        QuitReq ++
        [{put_chars, unicode,<<"  ? | h             - this message\n">>}].

group_opts(Node) ->
    VersionString = erpc:call(Node, erlang, system_info, [otp_release]),
    Version = list_to_integer(VersionString),
    ExpandFun =
        case Version > 25 of
            true -> [{expand_fun,fun(B, Opts)-> erpc:call(Node,edlin_expand,expand,[B, Opts]) end}];
            false -> [{expand_fun,fun(B, _)-> erpc:call(Node,edlin_expand,expand,[B]) end}]
        end,
    group_opts() ++ ExpandFun.
group_opts() ->
    [{expand_below, application:get_env(stdlib, shell_expand_location, below) =:= below}].

-spec io_request(request(), prim_tty:state()) -> {noreply, prim_tty:state()} |
          {term(), reference(), prim_tty:state()} | {term(), {error, term()}}.
io_request({requests,Rs}, TTY) ->
    {noreply, io_requests(Rs, TTY)};
io_request(redraw_prompt, TTY) ->
    write(prim_tty:handle_request(TTY, redraw_prompt));
io_request({redraw_prompt, Pbs, Pbs2, LineState}, TTY) ->
    write(prim_tty:handle_request(TTY, {redraw_prompt, Pbs, Pbs2, LineState}));
io_request(new_prompt, TTY) ->
    write(prim_tty:handle_request(TTY, new_prompt));
io_request(delete_after_cursor, TTY) ->
    write(prim_tty:handle_request(TTY, delete_after_cursor));
io_request(delete_line, TTY) ->
    write(prim_tty:handle_request(TTY, delete_line));
io_request({put_chars, unicode, Chars}, TTY) ->
    write(prim_tty:handle_request(TTY, {putc, unicode:characters_to_binary(Chars)}));
io_request({put_chars_sync, latin1, Chars, Reply}, TTY) ->
    try
        case {prim_tty:unicode(TTY), prim_tty:output_mode(TTY)} of
            {false, raw} ->
                Bin = if is_binary(Chars) -> Chars;
                    true -> list_to_binary(Chars)
                end,
                {Output, NewTTY} = prim_tty:handle_request(TTY, {putc_raw, Bin}),
                {ok, MonitorRef} = prim_tty:write(NewTTY, Output, self()),
                {Reply, MonitorRef, NewTTY};
            _ ->
                io_request({put_chars_sync, unicode, unicode:characters_to_binary(Chars,latin1), Reply}, TTY)
        end
    catch
        _:_ -> {Reply, {error, {put_chars, latin1, Chars}}}
    end;
io_request({put_chars_sync, unicode, Chars, Reply}, TTY) ->
    {Output, NewTTY} = prim_tty:handle_request(TTY, {putc, unicode:characters_to_binary(Chars)}),
    {ok, MonitorRef} = prim_tty:write(NewTTY, Output, self()),
    {Reply, MonitorRef, NewTTY};
io_request({put_expand, unicode, Chars, N}, TTY) ->
    write(prim_tty:handle_request(TTY, {expand, unicode:characters_to_binary(Chars), N}));
io_request({move_expand, N}, TTY) ->
    write(prim_tty:handle_request(TTY, {move_expand, N}));
io_request({move_rel, N}, TTY) ->
    write(prim_tty:handle_request(TTY, {move, N}));
io_request({move_line, R}, TTY) ->
    write(prim_tty:handle_request(TTY, {move_line, R}));
io_request({move_combo, V1, R, V2}, TTY) ->
    write(prim_tty:handle_request(TTY, {move_combo, V1, R, V2}));
io_request({insert_chars, unicode, Chars}, TTY) ->
    write(prim_tty:handle_request(TTY, {insert, unicode:characters_to_binary(Chars)}));
io_request({insert_chars_over, unicode, Chars}, TTY) ->
    write(prim_tty:handle_request(TTY, {insert_over, unicode:characters_to_binary(Chars)}));
io_request({delete_chars, N}, TTY) ->
    write(prim_tty:handle_request(TTY, {delete, N}));
io_request(clear, TTY) ->
    write(prim_tty:handle_request(TTY, clear));
io_request(beep, TTY) ->
    write(prim_tty:handle_request(TTY, beep)).

write({Output, TTY}) ->
    ok = prim_tty:write(TTY, Output),
    {noreply, TTY}.

io_requests([{insert_chars, unicode, C1},{insert_chars, unicode, C2}|Rs], TTY) ->
    io_requests([{insert_chars, unicode, [C1,C2]}|Rs], TTY);
io_requests([{put_chars, unicode, C1},{put_chars, unicode, C2}|Rs], TTY) ->
    io_requests([{put_chars, unicode, [C1,C2]}|Rs], TTY);
io_requests([{move_rel, N}, {move_line, R}, {move_rel, M}|Rs], TTY) ->
    io_requests([{move_combo, N, R, M}|Rs], TTY);
io_requests([{move_rel, N}, {move_line, R}|Rs], TTY) ->
    io_requests([{move_combo, N, R, 0}|Rs], TTY);
io_requests([{move_line, R}, {move_rel, M}|Rs], TTY) ->
    io_requests([{move_combo, 0, R, M}|Rs], TTY);
io_requests([R|Rs], TTY) ->
    {noreply, NewTTY} = io_request(R, TTY),
    io_requests(Rs, NewTTY);
io_requests([], TTY) ->
    TTY.

open_editor(TTY, Buffer) ->
    DefaultEditor =
        case os:type() of
            {win32, _} -> "notepad";
            {unix, _} -> "nano"
        end,
    Editor = os:getenv("VISUAL", os:getenv("EDITOR", DefaultEditor)),
    TmpFile = string:chomp(mktemp()) ++ ".erl",
    _ = file:write_file(TmpFile, unicode:characters_to_binary(Buffer, unicode)),
    case filelib:is_file(TmpFile) of
        true ->
            ok = prim_tty:disable_reader(TTY),
            try
                EditorPort =
                    case os:type() of
                        {win32, _} ->
                            [Cmd | Args] = string:split(Editor," ", all),
                            open_port({spawn_executable, os:find_executable(Cmd)},
                                      [{args,Args ++ [TmpFile]}, nouse_stdio]);
                        {unix, _ } ->
                            open_port({spawn, Editor ++ " " ++ TmpFile}, [nouse_stdio])
                    end,
                {EditorPort, TmpFile}
            catch error:enoent ->
                    ok = prim_tty:enable_reader(TTY),
                    io:format(standard_error, "Could not find EDITOR '~ts'.~n", [Editor]),
                    false
            end;
        false ->
            io:format(standard_error,
                      "Could not find create temp file '~ts'.~n",
                      [TmpFile]),
            false
    end.

mktemp() ->
    case os:type() of
        {win32, _} ->
            os:cmd("powershell \"write-host (& New-TemporaryFile | Select-Object -ExpandProperty FullName)\"");
        {unix,_} ->
            os:cmd("mktemp")
    end.

handle_req(next, TTYState, {false, IOQ} = IOQueue) ->
    case queue:out(IOQ) of
        {empty, _} ->
            {TTYState, IOQueue};
        {{value, {Origin, Req}}, ExecQ} ->
            case io_request(Req, TTYState) of
                {noreply, NewTTYState} ->
                    handle_req(next, NewTTYState, {false, ExecQ});
                {Reply, MonitorRef, NewTTYState} ->
                    {NewTTYState, {{Origin, MonitorRef, Reply}, ExecQ}};
                {Reply, {error, Reason}} ->
                    Origin ! {reply, Reply, {error, Reason}},
                    handle_req(next, TTYState, {false, ExecQ})
            end
    end;
handle_req(Msg, TTYState, {false, IOQ} = IOQueue) ->
    empty = queue:peek(IOQ),
    {Origin, Req} = Msg,
    case io_request(Req, TTYState) of
        {noreply, NewTTYState} ->
            {NewTTYState, IOQueue};
        {Reply, MonitorRef, NewTTYState} ->
            {NewTTYState, {{Origin, MonitorRef, Reply}, IOQ}};
        {Reply, {error, Reason}} ->
            Origin ! {reply, Reply, {error, Reason}},
            {TTYState, IOQueue}
    end;
handle_req(Msg,TTYState,{Resp, IOQ}) ->
    %% All requests are queued when we have outstanding sync put_chars
    {TTYState, {Resp, queue:in(Msg,IOQ)}}.

%% gr_new()
%% gr_get_num(Group, Index)
%% gr_get_info(Group, Pid)
%% gr_add_cur(Group, Pid, Shell)
%% gr_set_cur(Group, Index)
%% gr_cur_pid(Group)
%% gr_cur_index(Group)
%% gr_del_pid(Group, Pid)
%%  Manage the group list. The group structure has the form:
%%	{NextIndex,CurrIndex,CurrPid,GroupList}
%%
%%  where each element in the group list is:
%%	{Index,GroupPid,Shell}
-record(group, { index, pid, shell }).
-record(gr, { next = 0, current = 0, pid = none, groups = []}).
gr_new() ->
    #gr{}.
gr_new_group(I, P, S) ->
    #group{ index = I, pid = P, shell = S }.

gr_get_num(#gr{ groups = Gs }, I) ->
    case lists:keyfind(I, #group.index, Gs) of
        false -> undefined;
        #group{ shell = {} } ->
            undefined;
        #group{ pid = Pid } ->
            {pid, Pid}
    end.

gr_get_info(#gr{ groups = Gs }, Pid) ->
    case lists:keyfind(Pid, #group.pid, Gs) of
        false -> undefined;
        #group{ index = I, shell = S } ->
            {I, S}
    end.

gr_add_cur(#gr{ next = Next, groups = Gs}, Pid, Shell) ->
    put(current_group, Pid),
    #gr{ next = Next + 1, current = Next, pid = Pid,
         groups = Gs ++ [gr_new_group(Next, Pid, Shell)]
       }.

gr_set_cur(Gr, I) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
            put(current_group, Pid),
            {ok, Gr#gr{ current = I, pid = Pid }};
	undefined -> undefined
    end.

gr_set_num(Gr = #gr{ groups = Groups }, I, Pid, Shell) ->
    NewGroups = lists:keystore(I, #group.index, Groups, gr_new_group(I,Pid,Shell)),
    Gr#gr{ groups = NewGroups }.


gr_del_pid(Gr = #gr{ groups = Groups }, Pid) ->
    Gr#gr{ groups = lists:keydelete(Pid, #group.pid, Groups) }.


gr_cur_pid(#gr{ pid = Pid }) ->
    Pid.
gr_cur_index(#gr{ current = Index }) ->
    Index.

gr_list(#gr{ current = Current, groups = Groups}) ->
    lists:flatmap(
      fun(#group{ shell = {} }) ->
              [];
         (#group{ index = I, shell = S }) ->
              Marker = ["*" || Current =:= I],
              [{put_chars, unicode,
                unicode:characters_to_binary(
                  io_lib:format("~4w~.1ts ~w\n", [I,Marker,S]))}]
      end, Groups).
