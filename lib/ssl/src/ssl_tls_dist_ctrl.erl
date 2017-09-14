%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(ssl_tls_dist_ctrl).

-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").
-include_lib("kernel/include/net_address.hrl").

-export([start/1, get_socket/1, set_supervisor/2, hs_data_common/1]).

%%% ------------------------------------------------------------

%% In order to avoid issues with lingering signal binaries
%% we enable off-heap message queue data as well as fullsweep
%% after 0. The fullsweeps will be cheap since we have more
%% or less no live data.
common_spawn_opts() ->
    [{message_queue_data, off_heap},
     {fullsweep_after, 0}].

%%% ------------------------------------------------------------

start(SslSocket) ->
    spawn_opt(
      fun () ->
              setup(SslSocket)
      end,
      [{priority, max}] ++ common_spawn_opts()).

get_socket(DistCtrl) ->
    call(DistCtrl, get_socket).

set_supervisor(DistCtrl, Pid) ->
    call(DistCtrl, {set_supervisor, Pid}).

hs_data_common(DistCtrl) ->
    TickHandler = call(DistCtrl, tick_handler),
    SslSocket = get_socket(DistCtrl),
    #hs_data{
       f_send =
           fun (Ctrl, Packet) when Ctrl == DistCtrl ->
                   call(Ctrl, {send, Packet})
           end,
       f_recv =
           fun (Ctrl, Length, Timeout) when Ctrl == DistCtrl ->
                   case call(Ctrl, {recv, Length, Timeout}) of
                       {ok, Bin} when is_binary(Bin) ->
                           {ok, binary_to_list(Bin)};
                       Other ->
                           Other
                   end
           end,
       f_setopts_pre_nodeup =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   call(Ctrl, pre_nodeup)
           end,
       f_setopts_post_nodeup =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   call(Ctrl, post_nodeup)
           end,
       f_getll =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   call(Ctrl, getll)
           end,
       f_handshake_complete =
           fun (Ctrl, Node, DHandle) when Ctrl == DistCtrl ->
                   call(Ctrl, {handshake_complete, Node, DHandle})
           end,
       f_address =
           fun (Ctrl, Node) when Ctrl == DistCtrl ->
                   case call(Ctrl, {check_address, Node}) of
                       {error, no_node} ->
                           %% No '@' or more than one '@' in node name.
                           ?shutdown(no_node);
                       Res ->
                           Res
                   end
           end,
       mf_setopts =
           fun (Ctrl, Opts) when Ctrl == DistCtrl ->
                   case setopts_filter(Opts) of
                       [] ->
                           ssl:setopts(SslSocket, Opts);
                       Opts1 ->
                           {error, {badopts,Opts1}}
                   end
           end,
       mf_getopts =
           fun (Ctrl, Opts) when Ctrl == DistCtrl ->
                   ssl:getopts(SslSocket, Opts)
           end,
       mf_getstat =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   case ssl:getstat(
                          SslSocket, [recv_cnt, send_cnt, send_pend]) of
                       {ok, Stat} ->
                           split_stat(Stat,0,0,0);
                       Error ->
                           Error
                   end
           end,
       mf_tick =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   TickHandler ! tick
           end}.

%%% ------------------------------------------------------------

call(DistCtrl, Msg) ->
    Ref = erlang:monitor(process, DistCtrl),
    DistCtrl ! {Ref, self(), Msg},
    receive
        {Ref, Res} ->
            erlang:demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, DistCtrl, Reason} ->
            exit({dist_controller_exit, Reason})
    end.

setopts_filter(Opts) ->
    [Opt || {K,_} = Opt <- Opts,
            K =:= active orelse K =:= deliver orelse K =:= packet].

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

%%% ------------------------------------------------------------
%%% Distribution controller processes
%%% ------------------------------------------------------------

%%
%% There will be five parties working together when the
%% connection is up:
%% - The SSL socket. Providing a TLS connection
%%   to the other node.
%% - The output handler. It will dispatch all outgoing
%%   traffic from the VM to the gen_tcp socket. This
%%   process is registered as distribution controller
%%   for this channel with the VM.
%% - The input handler. It will dispatch all incoming
%%   traffic from the gen_tcp socket to the VM. This
%%   process is also the socket owner and receives
%%   incoming traffic using active-N.
%% - The tick handler. Dispatches asynchronous tick
%%   requests to the socket. It executes on max priority
%%   since it is important to get ticks through to the
%%   other end.
%% - The channel supervisor (provided by dist_util). It
%%   monitors traffic. Issue tick requests to the tick
%%   handler when no outgoing traffic is seen and bring
%%   the connection down if no incoming traffic is seen.
%%   This process also executes on max priority.
%%
%%   These parties are linked togheter so should one
%%   of them fail, all of them are terminated and the
%%   connection is taken down.
%%


%%
%% The tick handler process writes a tick to the
%% socket when it receives a 'tick' message from
%% the connection supervisor.
%%
%% We are not allowed to block the connection
%% superviser when writing a tick and we also want
%% the tick to go through even during a heavily
%% loaded system. gen_tcp does not have a
%% non-blocking send operation exposed in its API
%% and we don't want to run the distribution
%% controller under high priority. Therefore this
%% separate process with max prio that dispatches
%% ticks.
%%
tick_handler(SslSocket) ->
    receive
        tick ->
            %% May block due to busy port...
            sock_send(SslSocket, ""),
            flush_ticks(SslSocket);
        _ ->
            tick_handler(SslSocket)
    end.

flush_ticks(SslSocket) ->
    receive
        tick ->
            flush_ticks(SslSocket)
    after 0 ->
            tick_handler(SslSocket)
    end.

setup(SslSocket) ->
    TickHandler =
        spawn_opt(
          fun () ->
                  tick_handler(SslSocket)
          end,
          [link, {priority, max}] ++ common_spawn_opts()),
    setup_loop(SslSocket, TickHandler, undefined).

%%
%% During the handshake phase we loop in setup().
%% When the connection is up we spawn an input handler and
%% continue as output handler.
%%
setup_loop(SslSocket, TickHandler, Sup) ->
    receive
        {ssl_closed, SslSocket} ->
            exit(connection_closed);
        {ssl_error, SslSocket} ->
            exit(connection_closed);

        {Ref, From, {set_supervisor, Pid}} ->
            Res = link(Pid),
            From ! {Ref, Res},
            setup_loop(SslSocket, TickHandler, Pid);

        {Ref, From, tick_handler} ->
            From ! {Ref, TickHandler},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, get_socket} ->
            From ! {Ref, SslSocket},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, {send, Packet}} ->
            Res = ssl:send(SslSocket, Packet),
            From ! {Ref, Res},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, {recv, Length, Timeout}} ->
            Res = ssl:recv(SslSocket, Length, Timeout),
            From ! {Ref, Res},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, {check_address, Node}} ->
            Res =
                case ssl:peername(SslSocket) of
                    {ok, Address} ->
                        case inet_tls_dist:split_node(Node) of
                            false ->
                                {error, no_node};
                            Host ->
                                #net_address{
                                   address=Address, host=Host,
                                   protocol=tls, family=inet}
                        end
                end,
            From ! {Ref, Res},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, pre_nodeup} ->
            Res =
                ssl:setopts(
                  SslSocket,
                  [{packet, 4}, inet_tls_dist:nodelay()]),
            From ! {Ref, Res},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, post_nodeup} ->
            Res =
                ssl:setopts(
                  SslSocket,
                  [{packet, 4}, inet_tls_dist:nodelay()]),
            From ! {Ref, Res},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, getll} ->
            From ! {Ref, {ok, self()}},
            setup_loop(SslSocket, TickHandler, Sup);

        {Ref, From, {handshake_complete, _Node, DHandle}} ->
            From ! {Ref, ok},
            %% Handshake complete! Begin dispatching traffic...
            %%
            %% Use a dedicated input process to push the
            %% input-output-flow-control-deadlock problem
            %% to the SSL implementation.
            InputHandler =
                spawn_opt(
                  fun () ->
                          link(Sup),
                          ssl:setopts(SslSocket, [{active, once}]),
                          receive
                              DHandle ->
                                  input_loop(DHandle, SslSocket)
                          end
                  end,
                  [link] ++ common_spawn_opts()),
            ok = ssl:controlling_process(SslSocket, InputHandler),
            ok = erlang:dist_ctrl_input_handler(DHandle, InputHandler),
            InputHandler ! DHandle,
            %%
            %% From now on we execute on normal priority
            process_flag(priority, normal),
            erlang:dist_ctrl_get_data_notification(DHandle),
            output_loop(DHandle, SslSocket)
    end.

input_loop(DHandle, SslSocket) ->
    receive
        {ssl_closed, SslSocket} ->
            %% Connection to remote node terminated...
            exit(connection_closed);
        {ssl_error, SslSocket, _Reason} ->
            %% Connection to remote node terminated...
            exit(connection_closed);
        {ssl, SslSocket, Data} ->
            %% Incoming data from remote node...
            ok = ssl:setopts(SslSocket, [{active, once}]),
            try erlang:dist_ctrl_put_data(DHandle, Data)
            catch _:_ -> death_row()
            end,
            input_loop(DHandle, SslSocket);
        _ ->
            %% Drop garbage message...
            input_loop(DHandle, SslSocket)
    end.

output_loop(DHandle, SslSocket) ->
    receive
        dist_data ->
            %% Outgoing data from this node...
            try send_data(DHandle, SslSocket)
            catch _ : _ -> death_row()
            end,
            output_loop(DHandle, SslSocket);
        {send, From, Ref, Data} ->
            %% This is for testing only!
            %%
            %% Needed by some OTP distribution
            %% test suites...
            sock_send(SslSocket, Data),
            From ! {Ref, ok},
            output_loop(DHandle, SslSocket);
        _ ->
            %% Drop garbage message...
            output_loop(DHandle, SslSocket)
    end.

send_data(DHandle, SslSocket) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle);
        Data ->
            sock_send(SslSocket, Data),
            send_data(DHandle, SslSocket)
    end.

sock_send(SslSocket, Data) ->
    try ssl:send(SslSocket, Data) of
        ok -> ok;
        {error, Reason} ->
            death_row({send_error, Reason})
    catch
        Type:Reason ->
            death_row({send_error, {Type, Reason}})
    end.

death_row() ->
    death_row(connection_closed).

death_row(normal) ->
    %% We do not want to exit with normal
    %% exit reason since it wont bring down
    %% linked processes...
    death_row();
death_row(Reason) ->
    %% When the connection is on its way down operations
    %% begin to fail. We catch the failures and call
    %% this function waiting for termination. We should
    %% be terminated by one of our links to the other
    %% involved parties that began bringing the
    %% connection down. By waiting for termination we
    %% avoid altering the exit reason for the connection
    %% teardown. We however limit the wait to 5 seconds
    %% and bring down the connection ourselves if not
    %% terminated...
    receive after 5000 -> exit(Reason) end.
