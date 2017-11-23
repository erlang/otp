%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(runner).

-export([test/1, test/2,
	 start/1, send_term/2, finish/1, send_eot/1, recv_eot/1,
	 get_term/1, get_term/2]).

-define(default_timeout, 5000).

%% Executes a test case in a C program.
%%
%% This function is useful for test cases written in C which requires
%% no further input, and only returns a result by calling report().

test(Tc) ->
    test(Tc, ?default_timeout).

test(Tc, Timeout) ->
    Port = start(Tc),
    
    case get_term(Port, Timeout) of
	eot ->
	    ok;
	Other ->
	    io:format("In this test case, a success/failure result was"),
	    io:format("expected from the C program.\n"),
	    io:format("Received: ~p", [Other]),
	    ct:fail(badresult)
    end.

%% Executes a test case in a C program.  Returns the port.
%%
%% Use get_term/1,2.
%%
%% Returns: {ok, Port}

start({Prog, Tc}) when is_list(Prog), is_integer(Tc) ->
    Port = open_port({spawn, Prog}, [{packet, 4}, exit_status]),
    Command = [Tc div 256, Tc rem 256],
    Port ! {self(), {command, Command}},
    Port.

%% Finishes a test case by send an 'eot' message to the C program
%% and waiting for an 'eot'.
%%
%% If the C program doesn't require an 'eot', use recv_eot/1 instead.

finish(Port) when is_port(Port) ->
    send_eot(Port),
    ok = recv_eot(Port),
    0 = receive
            {Port,{exit_status,Status}} ->
                Status
        end,
    ok.

%% Sends an Erlang term to a C program.

send_term(Port, Term) when is_port(Port) ->
    Port ! {self(), {command, [$t, term_to_binary(Term)]}}.

%% Sends an 'eot' (end-of-test) indication to a C progrm.

send_eot(Port) when is_port(Port) ->
    Port ! {self(), {command, [$e]}}.

%% Waits for an 'eot' indication from the C program.
%% Either returns 'ok' or invokes ct:fail(badresult).

recv_eot(Port) when is_port(Port) ->    
    case get_term(Port) of
	eot ->
	    ok;
	Other ->
	    io:format("Error finishing test case.  Expected eof from"),
	    io:format("C program, but got:"),
	    io:format("~p", [Other]),
	    ct:fail(badresult)
    end.

%% Reads a term from the C program.
%%
%% Returns: {term, Term}|eot|'NULL' or calls ct:fail/1,2.

get_term(Port) ->
    get_term(Port, ?default_timeout).

get_term(Port, Timeout) ->
    case get_reply(Port, Timeout) of
	[$b|Bytes] ->
	    {bytes, Bytes};
	[$f] ->
	    ct:fail(failure);
	[$f|Reason] ->
	    ct:fail(Reason);
	[$t|Term] ->
	    {term, binary_to_term(list_to_binary(Term))};
	[$N] ->
	    'NULL';
	[$e] ->
	    eot;
	[$m|Message] ->
	    io:format("~s", [Message]),
	    get_term(Port, Timeout);
	Other ->
	    io:format("Garbage received from C program: ~p", [Other]),
	    ct:fail("Illegal response from C program")
    end.

get_reply(Port, Timeout) when is_port(Port) ->
    receive
	{Port, {data, Reply}} ->
	    Reply;
        Fail when element(1, Fail) == Port ->
            ct:fail("Got unexpected message from port: ~p",[Fail])
    after Timeout ->
	    ct:fail("No response from C program")
    end.
