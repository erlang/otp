%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

-module(standard_error_SUITE).

-export([all/0,suite/0]).
-export([badarg/1,getopts/1,output/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [badarg,getopts,output].

badarg(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch io:put_chars(standard_error, [oops])),
    true = erlang:is_process_alive(whereis(standard_error)),
    ok.

getopts(Config) when is_list(Config) ->
    [{encoding,latin1}] = io:getopts(standard_error),
    ok.

%% Test that writing a lot of output to standard_error does not cause the
%% processes handling it to terminate like this:
%%
%%    =ERROR REPORT==== 9-Aug-2015::23:19:23 ===
%%    ** Generic server standard_error_sup terminating
%%    ** Last message in was {'EXIT',<0.28.0>,eagain}
%%    ** When Server state == {state,standard_error,undefined,<0.28.0>,
%%                                   {local,standard_error_sup}}
%%    ** Reason for termination ==
%%    ** eagain
%%
%% This problem, observed with Erlang 18.0.2, was fixed in fd_driver by
%% properly handling EAGAIN if it arises on file descriptor writes.
%%
output(Config) when is_list(Config) ->
    Ref = monitor(process, standard_error_sup),
    Chars = [ [["1234567890" || _ <- lists:seq(1,10)], $\s,
               integer_to_list(L), $\r, $\n] || L <- lists:seq(1, 100) ],
    ok = io:put_chars(standard_error, Chars),
    receive
        {'DOWN', Ref, process, _, _} ->
            error(standard_error_noproc)
    after
        500 ->
            ok
    end.
