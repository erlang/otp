%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_examples_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    put(dbg,true),
    purge_examples(),
    load_examples(),
    megaco:enable_trace(max, io),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    purge_examples(),
    erase(dbg),
    megaco:disable_trace(),
    megaco_test_lib:end_per_testcase(Case, Config).

example_modules() ->
    [megaco_simple_mg, megaco_simple_mgc].

load_examples() ->
    case code:lib_dir(megaco) of
	{error, Reason} ->
	    {error, Reason};
	Dir ->
	    [code:load_abs(filename:join([Dir, examples, simple, M])) || M <- example_modules()]
    end.

purge_examples() ->
    case code:lib_dir(megaco) of
	{error, Reason} ->
	    {error, Reason};
	_Dir ->
	    [code:purge(M) || M <- example_modules()]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [simple].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


simple(suite) ->
    [];
simple(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    d("simple -> proxy start",[]),
    ProxyPid = megaco_test_lib:proxy_start({?MODULE, ?LINE}),

    d("simple -> start megaco",[]),
    ?VERIFY(ok, megaco:start()),
    
    d("simple -> start mgc",[]),
    ?APPLY(ProxyPid, fun() -> megaco_simple_mgc:start() end),
    receive
	{res, _, {ok, MgcAll}} when is_list(MgcAll) ->
	    MgcBad = [MgcRes || MgcRes <- MgcAll, element(1, MgcRes) /= ok],
	    ?VERIFY([], MgcBad),
	    %% MgcGood = MgcAll -- MgcBad,
	    %% MgcRecHandles = [MgcRH || {ok, _MgcPort, MgcRH} <- MgcGood],

	    d("simple -> start mg",[]),
	    ?APPLY(ProxyPid, fun() -> megaco_simple_mg:start() end),
	    receive
		{res, _, MgList} when is_list(MgList) andalso (length(MgList) =:= 4) ->
		    d("simple -> received res: ~p",[MgList]),		    
		    Verify = 
			fun({_MgMid, {TransId, Res}}) when TransId =:= 1 ->
				case Res of
				    {ok, [AR]} when is_record(AR, 'ActionReply') ->
					case AR#'ActionReply'.commandReply of
					    [{serviceChangeReply, SCR}] ->
						case SCR#'ServiceChangeReply'.serviceChangeResult of
						    {serviceChangeResParms, MgcMid} when MgcMid /= asn1_NOVALUE ->
							ok;
						    Error ->
							?ERROR(Error)
						end;
					    Error ->
						?ERROR(Error)
					end;
				    Error ->
					?ERROR(Error)
				end;
			   (Error) ->
				?ERROR(Error)
			end,
		    lists:map(Verify, MgList);
		Error ->
		    ?ERROR(Error)
	    end;
	Error ->
	    ?ERROR(Error)
    end,
    d("simple -> verify info()",[]),
    info(),
    d("simple -> verify system_info(users)",[]),
    users(),
    d("simple -> stop mgc",[]),
    ?VERIFY(5, length(megaco_simple_mgc:stop())),
    d("simple -> verify system_info(users)",[]),
    users(),
    d("simple -> stop megaco",[]),
    ?VERIFY(ok, megaco:stop()),
    d("simple -> kill (exit) ProxyPid: ~p",[ProxyPid]),
    exit(ProxyPid, shutdown), % Controlled kill of transport supervisors

    ok.


info() ->
    case (catch megaco:info()) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Info ->
	    ?LOG("Ok, ~p~n", [Info])
    end.

users() ->
    case (catch megaco:system_info(users)) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Users ->
	    ?LOG("Ok, ~p~n", [Users])
    end.




d(F,A) ->
    d(get(dbg),F,A).

d(true,F,A) ->
    io:format("DBG: " ++ F ++ "~n",A);
d(_, _F, _A) ->
    ok.
