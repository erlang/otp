%%-*-erlang-*-
%%----------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File    : xmerl_sax_stream_SUITE.erl
%%----------------------------------------------------------------------
-module(xmerl_sax_stream_SUITE).
-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------
all() ->
    [
     one_document,
     two_documents,
     one_document_and_junk,
     end_of_stream
    ].

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Send One doc over stream
one_document(Config) ->
    Port = 11111,

    {ok, ListenSocket} = listen(Port),
    Self = self(),
    
    spawn(
      fun() ->
              case catch gen_tcp:accept(ListenSocket) of
                  {ok, S} ->
                      Result = xmerl_sax_parser:stream(<<>>, 
                                                      [{continuation_state, S},
                                                       {continuation_fun, 
                                                        fun(Sd) ->
                                                                io:format("Continuation called!!", []),
                                                                case gen_tcp:recv(Sd, 0) of
                                                                    {ok, Packet} ->
                                                                        io:format("Packet: ~p\n", [Packet]),
                                                                        {Packet, Sd};
                                                                    {error, Reason} ->
                                                                        throw({error, Reason})
                                                                end
                                                        end}]),
                      Self ! {xmerl_sax, Result},
                      close(S);
                   Error ->
                      Self ! {xmerl_sax, {error, {accept, Error}}}
              end
      end),

    {ok, SendSocket} = connect(localhost, Port),
   
    {ok, Binary} = file:read_file(filename:join([datadir(Config), "xmerl_sax_stream_one.xml"])),
    
    send_chunks(SendSocket, Binary),

    receive
        {xmerl_sax, {ok, undefined, Rest}} ->
            <<"\n">> = Rest,
            io:format("Ok Rest: ~p\n", [Rest])        
       after 5000 ->
               ct:fail("Timeout")
    end,
    ok.
    
%%----------------------------------------------------------------------
%% Send Two doc over stream
two_documents(Config) ->
    Port = 11111,

    {ok, ListenSocket} = listen(Port),
    Self = self(),
    
    spawn(
      fun() ->
              case catch gen_tcp:accept(ListenSocket) of
                  {ok, S} ->
                      Result = xmerl_sax_parser:stream(<<>>, 
                                                      [{continuation_state, S},
                                                       {continuation_fun, 
                                                        fun(Sd) ->
                                                                io:format("Continuation called!!", []),
                                                                case gen_tcp:recv(Sd, 0) of
                                                                    {ok, Packet} ->
                                                                        io:format("Packet: ~p\n", [Packet]),
                                                                        {Packet, Sd};
                                                                    {error, Reason} ->
                                                                        throw({error, Reason})
                                                                end
                                                        end}]),
                      Self ! {xmerl_sax, Result},
                      close(S);
                   Error ->
                      Self ! {xmerl_sax, {error, {accept, Error}}}
              end
      end),

    {ok, SendSocket} = connect(localhost, Port),
   
    {ok, Binary} = file:read_file(filename:join([datadir(Config), "xmerl_sax_stream_two.xml"])),
    
    send_chunks(SendSocket, Binary),

    receive
        {xmerl_sax, {ok, undefined, Rest}} ->
            <<"\n<?x", _R/binary>> = Rest,
            io:format("Ok Rest: ~p\n", [Rest])        
       after 5000 ->
               ct:fail("Timeout")
    end,
    ok.
    
%%----------------------------------------------------------------------
%% Send one doc and then junk on stream
one_document_and_junk(Config) ->
    Port = 11111,

    {ok, ListenSocket} = listen(Port),
    Self = self(),
    
    spawn(
      fun() ->
              case catch gen_tcp:accept(ListenSocket) of
                  {ok, S} ->
                      Result = xmerl_sax_parser:stream(<<>>, 
                                                      [{continuation_state, S},
                                                       {continuation_fun, 
                                                        fun(Sd) ->
                                                                io:format("Continuation called!!", []),
                                                                case gen_tcp:recv(Sd, 0) of
                                                                    {ok, Packet} ->
                                                                        io:format("Packet: ~p\n", [Packet]),
                                                                        {Packet, Sd};
                                                                    {error, Reason} ->
                                                                        throw({error, Reason})
                                                                end
                                                        end}]),
                      Self ! {xmerl_sax, Result},
                      close(S);
                   Error ->
                      Self ! {xmerl_sax, {error, {accept, Error}}}
              end
      end),

    {ok, SendSocket} = connect(localhost, Port),
   
    {ok, Binary} = file:read_file(filename:join([datadir(Config), "xmerl_sax_stream_one_junk.xml"])),
    
    send_chunks(SendSocket, Binary),

    receive
        {xmerl_sax, {ok, undefined, Rest}} ->
            <<"\nth", _R/binary>> = Rest,
            io:format("Ok Rest: ~p\n", [Rest])        
       after 10000 ->
               ct:fail("Timeout")
    end,
    ok.

%%----------------------------------------------------------------------
%% Test of continuation when end of stream
end_of_stream(Config) ->
    Stream = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><a>hej</a>">>,
    {ok, undefined, <<>>} = xmerl_sax_parser:stream(Stream, []),
    ok.
    
%%----------------------------------------------------------------------
%% Utility functions
%%----------------------------------------------------------------------
listen(Port) ->
    case catch gen_tcp:listen(Port, [{active, false}, 
				     binary, 
				     {keepalive, true},
				     {reuseaddr,true}]) of
	{ok, ListenSocket} ->
            {ok, ListenSocket};
	{error, Reason} ->
	    {error, {listen, Reason}}
    end.

close(Socket) ->
    (catch gen_tcp:close(Socket)).

connect(Host, Port) ->
     Timeout = 5000,
     % Options1 = check_options(Options),
     Options = [binary],
     case catch gen_tcp:connect(Host, Port, Options, Timeout) of
 	{ok, Socket} ->
 	    {ok, Socket};
 	{error, Reason} ->
	    {error, Reason}
     end.

send_chunks(Socket, Binary) ->
    BSize = erlang:size(Binary),
    if 
        BSize > 25 ->
            <<Head:25/binary, Tail/binary>> = Binary,
            case gen_tcp:send(Socket, Head) of
                ok ->
                    timer:sleep(1000),
                    send_chunks(Socket, Tail);
                {error,closed} ->
                    ok
            end;
        true ->
            gen_tcp:send(Socket, Binary)
    end.

datadir(Config) ->
    proplists:get_value(data_dir, Config).
