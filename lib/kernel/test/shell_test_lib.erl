%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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

-module(shell_test_lib).

-include("shell_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").

-export([start_tmux/1,
         stop_tmux/1,
         setup_tty/1,
         stop_tty/1,
         tmux/1,
         rpc/2,
         rpc/4,
         set_tty_prompt/2,
         prompt/1,
         tty_name/1,
         send_tty/2,
         send_stdin/2,
         check_location/2,
         check_location/3,
         get_location/1,
         get_window_size/1,
         check_not_in_content/2,
         check_not_in_content/4,
         check_content/2,
         check_content/3,
         check_content/4,
         get_content/1,
         get_content/2]).

%% Put this in init_per_xxx in the test you're writing
start_tmux(Config) ->
    case string:split(shell_test_lib:tmux("-V")," ") of
        ["tmux",[Num,$.|_]] when Num >= $3, Num =< $9 ->
            shell_test_lib:tmux("kill-session"),
            W = proplists:get_value(width, Config, 50),
            H = proplists:get_value(height, Config, 60),
            "" = shell_test_lib:tmux("-u new-session -x " ++integer_to_list(W)++" -y "
                                     ++integer_to_list(H)++" -d"),
            ["" = shell_test_lib:tmux(["set-environment '",Name,"' '",Value,"'"])
             || {Name,Value} <- os:env()],
            Config;
        ["tmux", Vsn] ->
            {skip, "invalid tmux version " ++ Vsn ++ ". Need vsn 3 or later"};
        Error ->
            {skip, "tmux not installed " ++ Error}
    end.

stop_tmux(_Config) ->
    Windows = string:split(shell_test_lib:tmux("list-windows"), "\n", all),
    lists:foreach(
      fun(W) ->
              case string:split(W, " ", all) of
                  ["0:" | _] -> ok;
                  [No, _Name | _] ->
                      "" = os:cmd(["tmux select-window -t ", string:split(No,":")]),
                      ct:log("~ts~n~ts",[W, os:cmd(lists:concat(["tmux capture-pane -p -e"]))])
              end
      end, Windows),
%    "" = os:cmd("tmux kill-session")
    ok.

%% Setup a TTY, or a ssh server and client but do not type anything in terminal (except password)
setup_tty(Config) ->
    ClientName = maps:get(name,proplists:get_value(peer, Config, #{}),
                    peer:random_name(proplists:get_value(tc_path, Config))),
    
    IsSsh = proplists:get_value(ssh, Config, false),

    Name = if IsSsh ->
            ClientName ++ "_ssh";
        true ->
            ClientName
    end,

    Envs = lists:flatmap(fun({Key,Value}) ->
                                 ["-env",Key,Value]
                         end, proplists:get_value(env,Config,[])),

    ExtraArgs = proplists:get_value(args,Config,[]),

    ExecArgs = case os:getenv("TMUX_DEBUG") of
                   "strace" ->
                       STraceLog = filename:join(proplists:get_value(priv_dir,Config),
                                                 Name++".strace"),
                       ct:log("Link to strace: file://~ts", [STraceLog]),
                       [os:find_executable("strace"),"-f",
                        "-o",STraceLog,
                        "-e","trace=all",
                        "-e","read=0,1,2",
                        "-e","write=0,1,2"
                       ] ++ string:split(ct:get_progname()," ",all);
                   "rr" ->
                       [os:find_executable("cerl"),"-rr"];
                   _ ->
                       string:split(ct:get_progname()," ",all)
               end,
    DefaultPeerArgs = #{ name => Name,
                         exec =>
                             {os:find_executable("tmux"),
                              ["new-window","-n",Name,"-d","--"] ++ ExecArgs },

                         args => ["-pz",filename:dirname(code:which(?MODULE)),
                                  "-connect_all","false",
%                                  "-kernel","logger_level","all",
                                  "-kernel","shell_history","disabled",
                                  "-kernel","prevent_overlapping_partitions","false",
                                  "-eval","shell:prompt_func({shell_test_lib,prompt})."
                                 ] ++ Envs ++ ExtraArgs,
                         wait_boot => 60_000,
                         detached => false
                       },

    {ok, Peer, Node} =
        ?CT_PEER(maps:merge(proplists:get_value(peer,Config,#{}),
                            DefaultPeerArgs)),

    Self = self(),

    %% By default peer links with the starter. For these TCs we however only
    %% want the peer to die if we die, so we create a "unidirection link" using
    %% monitors.
    spawn(fun() ->
                  TCRef = erlang:monitor(process, Self),
                  PeerRef = erlang:monitor(process, Peer),
                  receive
                      {'DOWN',TCRef,_,_,Reason} ->
                          exit(Peer, Reason);
                      {'DOWN',PeerRef,_,_,_} ->
                          ok
                  end
          end),
    unlink(Peer),

    "" = tmux(["set-option -t ",Name," remain-on-exit on"]),

    %% We start tracing on the remote node in order to help debugging
    TraceLog = filename:join(proplists:get_value(priv_dir,Config),Name++".trace"),
    ct:log("Link to trace: file://~ts",[TraceLog]),

    spawn(Node,
          fun() ->
                  {ok, _} = dbg:tracer(file,TraceLog),
                  %% dbg:p(whereis(user_drv),[c,m]),
                  %% dbg:p(whereis(user_drv_writer),[c,m]),
                  %% dbg:p(whereis(user_drv_reader),[c,m]),
                  %% dbg:tp(user_drv,x),
                  %% dbg:tp(prim_tty,x),
                  %% dbg:tpl(prim_tty,write_nif,x),
                  %% dbg:tpl(prim_tty,read_nif,x),
                  monitor(process, Self),
                  receive _ -> ok end
          end),
    Tmux = #tmux{ peer = Peer, node = Node, name = ClientName },
    if IsSsh ->
            rpc(Tmux, fun() ->
                ssh:start(),
                PrivDir = filename:join(proplists:get_value(priv_dir, Config), "nopubkey"),
                file:make_dir(PrivDir),
                SysDir = proplists:get_value(data_dir, Config),
                {ok, _Sshd} = ssh:daemon(8989, [{system_dir, SysDir},
                                    {user_dir, PrivDir},
                                    {password, "bar"}])
            end),
            os:cmd(os:find_executable("tmux") ++ " new-window -n " ++ ClientName ++ " -d -- "++
                "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost -p 8989 -l foo"),
            "" = tmux(["set-option -t ",ClientName," remain-on-exit on"]),

            timer:sleep(2000),
            check_content(Tmux,"Enter password for \"foo\""),
            "" = tmux("send -t " ++ ClientName ++ " bar Enter"),
            timer:sleep(1000),
            check_content(Tmux,"\\d+\n?>"),
            Tmux#tmux{ ssh_server_name = Name };
        true ->
            Tmux
    end.

stop_tty(Term) ->
    catch peer:stop(Term#tmux.peer),
    ct:log("~ts",[get_content(Term, "-e")]),
    [ct:log("~ts",[get_content(Term#tmux{ name = Term#tmux.ssh_server_name }, "-e")])
        || Term#tmux.ssh_server_name =/= undefined],
%    "" = tmux("kill-window -t " ++ Term#tmux.name),
    ok.

tmux([Cmd|_] = Command) when is_list(Cmd) ->
    tmux(lists:concat(Command));
tmux(Command) ->
    string:trim(os:cmd(["tmux ",Command])).

rpc(#tmux{ node = N }, Fun) ->
    erpc:call(N, Fun).
rpc(#tmux{ node = N }, M, F, A) ->
    erpc:call(N, M, F, A).

set_tty_prompt(Term, Config) ->
    Prompt = fun() -> ["\e[94m",54620,44397,50612,47,51312,49440,47568,"\e[0m"] end,
    Prompt1 = proplists:get_value(shell_prompt_func_test, Config, Prompt),

    erpc:call(Term#tmux.node, application, set_env,
        [stdlib, shell_prompt_func_test, Prompt1]).

prompt(L) ->
    N = proplists:get_value(history, L, 0),
    Fun = application:get_env(stdlib, shell_prompt_func_test,
                                fun() -> atom_to_list(node()) end),
    io_lib:format("(~ts)~w> ",[Fun(),N]).

tty_name(Term) ->
    Term#tmux.name.

send_tty(Term, "Home") ->
    %% https://stackoverflow.com/a/55616731
    send_tty(Term,"Escape"),
    send_tty(Term,"OH");
send_tty(Term, "End") ->
    send_tty(Term,"Escape"),
    send_tty(Term,"OF");
send_tty(#tmux{ name = Name } = _Term,Value) ->
    [Head | Quotes] = string:split(Value, "'", all),
    "" = tmux("send -t " ++ Name ++ " '" ++ Head ++ "'"),
    [begin
            "" = tmux("send -t " ++ Name ++ " \"'\""),
            "" = tmux("send -t " ++ Name ++ " '" ++ V ++ "'")
        end || V <- Quotes].

%% We use send_stdin for testing of things that we cannot sent via
%% the tmux send command, such as invalid unicode
send_stdin(Term, Chars) when is_binary(Chars) ->
    rpc(Term,erlang,display_string,[stdin,Chars]);
send_stdin(Term, Chars) ->
    send_stdin(Term, iolist_to_binary(unicode:characters_to_binary(Chars))).

check_location(Term, Where) ->
    check_location(Term, Where, 5).
check_location(Term, Where, Attempt) when is_tuple(Where) ->
    check_location(Term, [Where], Attempt);
check_location(#tmux{ orig_location = {OrigRow, OrigCol} = Orig } = Term,
                Where, Attempt) ->
    NewLocation = get_location(Term),
    case lists:any(fun({AdjRow, AdjCol}) ->
                            {OrigRow+AdjRow,OrigCol+AdjCol} =:= NewLocation
                    end, Where) of
        true -> NewLocation;
        false when Attempt =:= 0 ->
            {NewRow, NewCol} = NewLocation,
            ct:fail({wrong_location, {expected,Where},
                        {got,{NewRow - OrigRow, NewCol - OrigCol},
                        {NewLocation, Orig}}});
        false ->
            timer:sleep(50),
            check_location(Term, Where, Attempt -1)
    end.

get_location(Term) ->
    RowAndCol = tmux("display -pF '#{cursor_y} #{cursor_x}' -t "++Term#tmux.name),
    [Row, Col] = string:lexemes(string:trim(RowAndCol,both)," "),
    {list_to_integer(Row), list_to_integer(Col)}.

get_window_size(Term) ->
    RowAndCol = tmux("display -pF '#{window_height} #{window_width}' -t "++Term#tmux.name),
    [Row, Col] = string:lexemes(string:trim(RowAndCol,both)," "),
    {list_to_integer(Row), list_to_integer(Col)}.

check_not_in_content(Term, NegativeMatch) ->
    check_not_in_content(Term, NegativeMatch, #{}, 5).
check_not_in_content(Term, NegativeMatch, Opts, Attempt) ->
    Opts = #{},
    OrigContent = case Term of
        #tmux{} -> get_content(Term);
        Fun when is_function(Fun,0) -> Fun()
    end,
    Content = case maps:find(replace, Opts) of
                {ok, {RE,Repl} } ->
                    re:replace(OrigContent, RE, Repl, [global]);
                error ->
                    OrigContent
                end,
    case re:run(string:trim(Content, both), lists:flatten(NegativeMatch), [unicode]) of
        {match,_} ->
            io:format("Failed, found '~ts' in ~n'~ts'~n",
            [unicode:characters_to_binary(NegativeMatch), Content]),
            io:format("Failed, found '~w' in ~n'~w'~n",
                        [unicode:characters_to_binary(NegativeMatch), Content]),
            ct:fail(match);
        _ when Attempt =:= 0 ->
            ok;
        _ ->
            timer:sleep(500),
            check_not_in_content(Term, NegativeMatch, Opts, Attempt - 1)
    end.
check_content(Term, Match) ->
    check_content(Term, Match, #{}).
check_content(Term, Match, Opts) when is_map(Opts) ->
    check_content(Term, Match, Opts, 5).
check_content(Term, Match, Opts, Attempt) ->
    OrigContent = case Term of
                    #tmux{} -> get_content(Term);
                    Fun when is_function(Fun,0) -> Fun()
                end,
    Content = case maps:find(replace, Opts) of
                    {ok, {RE,Repl} } ->
                        re:replace(OrigContent, RE, Repl, [global]);
                    error ->
                        OrigContent
                end,
    case re:run(string:trim(Content, both), lists:flatten(Match), [unicode]) of
        {match,_} ->
            ok;
        _ when Attempt =:= 0 ->
            io:format("Failed to find '~ts' in ~n'~ts'~n",
                        [unicode:characters_to_binary(Match), Content]),
            io:format("Failed to find '~w' in ~n'~w'~n",
                        [unicode:characters_to_binary(Match), Content]),
            ct:fail(nomatch);
        _ ->
            timer:sleep(500),
            check_content(Term, Match, Opts, Attempt - 1)
    end.

get_content(Term) ->
    get_content(Term, "").
get_content(#tmux{ name = Name }, Args) ->
    Content = unicode:characters_to_binary(tmux("capture-pane -p " ++ Args ++ " -t " ++ Name)),
    case string:split(Content,"a.\na") of
        [_Ignore,C] ->
            C;
        [C] ->
            C
    end.
