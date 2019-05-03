%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

-module(socket_test_logger).

-export([
         start/0, start/1,
         stop/0,
         format/2
        ]).


-define(QUIET,  true).
-define(LIB,    socket_test_lib).
-define(LOGGER, ?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(?QUIET).

start(Quiet) ->
    case global:whereis_name(?LOGGER) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            Self = self(),
            Pid = spawn(fun() -> init(Self, Quiet) end),
            yes = global:register_name(?LOGGER, Pid),
            ok
    end.


stop() ->
    case global:whereis_name(?LOGGER) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            global:unregister_name(?LOGGER),
            Pid ! {?LOGGER, '$logger', stop},
            ok
    end.


format(F, []) ->
    do_format(F);
format(F, A) ->
    do_format(?LIB:f(F, A)).

do_format(Msg) ->
    case global:whereis_name(?LOGGER) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Pid ! {?MODULE, '$logger', {msg, Msg}},
            ok
    end.

init(Parent, Quiet) ->
    put(sname, "logger"),
    print("[~s][logger] starting~n", [?LIB:formated_timestamp()]),
    loop(#{parent => Parent, quiet => Quiet}).

loop(#{parent := Parent,
       quiet  := Quiet} = State) ->
    receive
        {'EXIT', Parent, _} ->
            print("[~s][logger] parent exit~n", [?LIB:formated_timestamp()]),
            exit(normal);

        {?MODULE, '$logger', stop} ->
            print("[~s][logger] stopping~n", [?LIB:formated_timestamp()]),
            exit(normal);

        {?MODULE, '$logger', {msg, Msg}} ->
            print_str(Quiet, Msg),
            loop(State)
    end.


print(F, A) ->
    print_str(false, ?LIB:f(F, A)).

print_str(Quiet, Str) ->
    try
        begin
            if (Quiet =/= true) -> io:format(user, Str ++ "~n", []);
               true             -> ok
            end,
            io:format(Str, [])
        end
    catch
        _:_:_ ->
            io:format(user,
                      "~nFailed Format message:"
                      "~n~p~n", [Str]),
            io:format("~nFailed Format message:"
                      "~n~p~n", [Str])
    end.
            
