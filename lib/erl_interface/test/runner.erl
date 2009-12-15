%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
-module(runner).

-export([test/1, test/2,
	 start/1, send_term/2, finish/1, send_eot/1, recv_eot/1,
	 get_term/1, get_term/2]).

-define(default_timeout, test_server:seconds(5)).

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
	    test_server:fail()
    end.

%% Executes a test case in a C program.  Returns the port.
%%
%% Use get_term/1,2.
%%
%% Returns: {ok, Port}

start({Prog, Tc}) when is_list(Prog), is_integer(Tc) ->
    Port = open_port({spawn, Prog}, [{packet, 4}]),
    Command = [Tc div 256, Tc rem 256],
    Port ! {self(), {command, Command}},
    Port.

%% Finishes a test case by send an 'eot' message to the C program
%% and waiting for an 'eot'.
%%
%% If the C program doesn't require an 'eot', use recv_eot/1 instead.

finish(Port) when is_port(Port) ->
    send_eot(Port),
    recv_eot(Port).

%% Sends an Erlang term to a C program.

send_term(Port, Term) when is_port(Port) ->
    Port ! {self(), {command, [$t, term_to_binary(Term)]}}.

%% Sends an 'eot' (end-of-test) indication to a C progrm.

send_eot(Port) when is_port(Port) ->
    Port ! {self(), {command, [$e]}}.

%% Waits for an 'eot' indication from the C program.
%% Either returns 'ok' or invokes test_server:fail().

recv_eot(Port) when is_port(Port) ->    
    case get_term(Port) of
	eot ->
	    ok;
	Other ->
	    io:format("Error finishing test case.  Expected eof from"),
	    io:format("C program, but got:"),
	    io:format("~p", [Other]),
	    test_server:fail()
    end.

%% Reads a term from the C program.
%%
%% Returns: {term, Term}|eot|'NULL' or calls test_server:fail/1,2.

get_term(Port) ->
    get_term(Port, ?default_timeout).

get_term(Port, Timeout) ->
    case get_reply(Port, Timeout) of
	[$b|Bytes] ->
	    {bytes, Bytes};
	[$f] ->
	    test_server:fail();
	[$f|Reason] ->
	    test_server:fail(Reason);
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
	    test_server:fail("Illegal response from C program")
    end.

get_reply(Port, Timeout) when is_port(Port) ->
    receive
	{Port, {data, Reply}} ->
	    Reply
    after Timeout ->
	    test_server:fail("No response from C program")
    end.
