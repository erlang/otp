%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

%%
%% Common library routines in the SSH application
%% 

-module(ssh_lib).
-moduledoc false.

-export([
         format_address_port/2, format_address_port/1,
         format_address/1,
         format_time_ms/1,
         comp/2,
         set_label/1,
         set_label/2
        ]).

-include("ssh.hrl").

%%%----------------------------------------------------------------
format_address_port({IP,Port}) when is_integer(Port) ->
    format_address_port(IP, Port);
format_address_port(X) ->
    io_lib:format("~p", [X]).

%%%----------------------------------------------------------------
format_address_port(Address, Port) ->
    try lists:concat([format_address(Address), ":", Port])
    catch
        _:_ -> io_lib:format("~p:~p",[Address,Port])
    end.

%%%----------------------------------------------------------------
format_address(#address{address=A, port=P}) ->
    format_address_port(A,P);
format_address(A) ->
    try inet:ntoa(A)
    catch
        _:_ when is_list(A) -> A;
        _:_ -> io_lib:format('~p',[A])
    end.

%%%----------------------------------------------------------------
format_time_ms(T) when is_integer(T) ->
    if
        T < 60000 -> io_lib:format("~.3f sec", [T/1000]);
        true -> io_lib:format("~p min ~s", [T div 60000, format_time_ms(T rem 60000)])
    end.

            
%%%----------------------------------------------------------------

comp(X1, X2) ->
    comp(X1, X2, true).

%%% yes - very far from best implementation
comp(<<B1,R1/binary>>, <<B2,R2/binary>>, Truth) ->
    comp(R1, R2, Truth and (B1 == B2));
comp(<<_,R1/binary>>, <<>>, Truth) ->
    comp(R1, <<>>, Truth and false);
comp(<<>>, <<>>, Truth) ->
    Truth;

comp([H1|T1], [H2|T2], Truth) ->
    comp(T1, T2, Truth and (H1 == H2));
comp([_|T1], [], Truth) ->
    comp(T1, [], Truth and false);
comp([], [], Truth) ->
    Truth;

comp(_, _, _) ->
    false.

set_label(Details) ->
    proc_lib:set_label({ssh, Details}).

set_label(client, Details) ->
    proc_lib:set_label({sshc, Details});
set_label(server, Details) ->
    proc_lib:set_label({sshd, Details}).
