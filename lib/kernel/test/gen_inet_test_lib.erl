%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

-module(gen_inet_test_lib).

-export([tc_try/3]).
-export([f/2,
         print/1, print/2]).

-include("gen_inet_test_lib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_tc_name(N) when is_atom(N) ->
    set_tc_name(atom_to_list(N));
set_tc_name(N) when is_list(N) ->
    put(tc_name, N).

%% get_tc_name() ->
%%     get(tc_name).

tc_begin(TC) ->
    OldVal = process_flag(trap_exit, true),
    put(old_trap_exit, OldVal),
    set_tc_name(TC),
    tc_print("begin ***",
             "~n----------------------------------------------------~n", "").
    
tc_end(Result) when is_list(Result) ->
    OldVal = erase(old_trap_exit),
    process_flag(trap_exit, OldVal),
    tc_print("done: ~s", [Result], 
             "", "----------------------------------------------------~n~n"),
    ok.

%% *** tc_try/2,3 ***
%% Case: Basically the test case name
%% Cond: A fun that is evaluated before the actual test case
%%       The point of this is that it can performs checks to
%%       see if we shall run the test case at all.
%%       For instance, the test case may only work in specific
%%       conditions.
%% TC:   The test case fun
tc_try(Case, Cond, TC) 
  when is_atom(Case) andalso
       is_function(Cond, 0) andalso
       is_function(TC, 0) ->
    tc_begin(Case),
    try Cond() of
        ok ->
            try 
                begin
                    TC(),
                    ?SLEEP(?SECS(1)),
                    tc_end("ok")
                end
            catch
                C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
                    %% i("catched[tc] (skip): "
                    %%   "~n   C:    ~p"
                    %%   "~n   SKIP: ~p"
                    %%   "~n", [C, SKIP]),
                    tc_end( f("skipping(catched,~w,tc)", [C]) ),
                    SKIP;
                C:E:S ->
                    %% i("catched[tc]: "
                    %%   "~n   C: ~p"
                    %%   "~n   E: ~p"
                    %%   "~n   S: ~p"
                    %%    "~n", [C, E, S]),
                    tc_end( f("failed(catched,~w,tc)", [C]) ),
                    erlang:raise(C, E, S)
            end;
        {skip, _} = SKIP ->
            tc_end("skipping(tc)"),
            SKIP;
        {error, Reason} ->
            tc_end("failed(tc)"),
            exit({tc_cond_failed, Reason})
    catch
        C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
            %% i("catched[cond] (skip): "
            %%   "~n   C:    ~p"
            %%   "~n   SKIP: ~p"
            %%   "~n", [C, SKIP]),
            tc_end( f("skipping(catched,~w,cond)", [C]) ),
            SKIP;
        C:E:S ->
            %% i("catched[cond]: "
            %%   "~n   C: ~p"
            %%   "~n   E: ~p"
            %%   "~n   S: ~p"
            %%   "~n", [C, E, S]),
            tc_end( f("failed(catched,~w,cond)", [C]) ),
            erlang:raise(C, E, S)
    end.


tc_print(F, Before, After) ->
    tc_print(F, [], Before, After).

tc_print(F, A, Before, After) ->
    Name = tc_which_name(),
    FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
             [formated_timestamp(),Name,self()|A]),
    io:format(user, Before ++ FStr ++ After, []).

tc_which_name() ->
    case get(tc_name) of
        undefined ->
            case get(sname) of
                undefined ->
                    "";
                SName when is_list(SName) ->
                    SName
            end;
        Name when is_list(Name) ->
            Name
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    {Hour, Min, Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                             [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatTS).

print(F) ->
    print(F, []).

print(F, A) ->
    io:format("~s ~p " ++ F ++ "~n", [formated_timestamp(), self() | A]).
