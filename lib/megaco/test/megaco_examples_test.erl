%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2019. All Rights Reserved.
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

-export([
         all/0,
         groups/0,

         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         simple/1,

         t/0, t/1
        ]).


-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(LIB, megaco_test_lib).

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
	    SimpleDir = filename:join([Dir, examples, simple]),
	    case code:add_path(SimpleDir) of
		true ->
		    ok;
		{error, What} ->
		    error_logger:error_msg("failed adding examples path: "
					   "~n   ~p"
					   "~n", [What]),
		    {error, {failed_add_path, What}}
	    end
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
    process_flag(trap_exit, true),
    d("simple -> create node name(s)"),
    [_Local, MGC, MG] = ?LIB:mk_nodes(3), %% Grrr
    Nodes = [MGC, MG],

    d("simple -> start nodes"),
    ok = ?LIB:start_nodes(Nodes, ?MODULE, ?LINE),
    
    MGCId = "MGC",
    MGId  = "MG",

    d("simple -> MGC proxy start (on ~p)", [MGC]),
    MGCProxy = megaco_test_lib:proxy_start(MGC, "MGC"),
    ?SLEEP(1000),

    d("simple -> MG proxy start (on ~p)", [MG]),
    MGProxy  = megaco_test_lib:proxy_start(MG, "MG"),
    ?SLEEP(1000),

    MegacoStart       = fun() -> megaco:start() end,
    MegacoStartVerify =
	 fun(_, ok)    -> ok;
	    (Id, Else) -> ?ERROR({failed_starting_megaco, Id, Else})
	 end,

    d("simple -> start MGC megaco"),
    exec(MGCProxy, MGCId, MegacoStart,
	 fun(Res) -> MegacoStartVerify(MGCId, Res) end),
    %% ?APPLY(MGCProxy, fun() -> ok = megaco:start() end),
    ?SLEEP(1000),
    
    d("simple -> start MG megaco"),
    exec(MGProxy, MGId, MegacoStart,
	 fun(Res) -> MegacoStartVerify(MGId, Res) end),
    %% ?APPLY(MGProxy, fun() -> ok = megaco:start() end),
    ?SLEEP(1000),
    
    d("simple -> start mgc"),
    start_mgc(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC info (no mg)"),
    info(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC system_info(users) (no mg)"),
    users(MGCProxy),
    ?SLEEP(1000),

    d("simple -> start mg"),
    start_mg(MGProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC info (mg)"),
    info(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC system_info(users) (mg)"),
    users(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MG info"),
    info(MGProxy),
    ?SLEEP(1000),

    d("simple -> verify MG system_info(users)"),
    users(MGProxy),
    ?SLEEP(1000),

    d("simple -> stop mgc"),
    exec(MGCProxy, MGCId,
	 fun() -> megaco_simple_mgc:stop() end,
	 fun([_]) -> ok;
	    (L) when is_list(L) ->
		 ?ERROR({invalid_users, L});
	    (X) ->
		 ?ERROR({invalid_result, X})
	 end),
    %% ?VERIFY(5, length()),
    ?SLEEP(1000),

    d("simple -> verify MGC info (no mgc)"),
    info(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MG info (no mgc)"),
    info(MGProxy),
    ?SLEEP(1000),

    d("simple -> verify MGC system_info(users) (no mgc)",[]),
    users(MGCProxy),
    ?SLEEP(1000),

    d("simple -> verify MG system_info(users) (no mgc)",[]),
    users(MGProxy),
    ?SLEEP(1000),

    MegacoStop       = fun() -> megaco:stop() end,
    MegacoStopVerify =
	 fun(_, ok)    -> ok;
	    (Id, Else) -> ?ERROR({failed_stop_megaco, Id, Else})
	 end,

    d("simple -> stop MG megaco",[]),
    exec(MGProxy, MGId, MegacoStop,
	 fun(Res) -> MegacoStopVerify(MGId, Res) end),
    %% ?VERIFY(ok, megaco:stop()),
    ?SLEEP(1000),

    d("simple -> stop MGC megaco",[]),
    exec(MGCProxy, MGCId, MegacoStop,
	 fun(Res) -> MegacoStopVerify(MGCId, Res) end),
    %% ?VERIFY(ok, megaco:stop()),
    ?SLEEP(1000),

    d("simple -> kill (exit) MG Proxy: ~p", [MGProxy]),
    MGProxy ! {stop, self(), normal},
    receive
	{'EXIT', MGProxy, _} ->
	    d("simple -> MG Proxy terminated"),
	    ok
    end,

    d("simple -> kill (exit) MGC Proxy: ~p", [MGCProxy]),
    MGCProxy ! {stop, self(), normal},
    receive
	{'EXIT', MGCProxy, _} ->
	    d("simple -> MGC Proxy terminated"),
	    ok
    end,

    d("simple -> stop ~p", [MGC]),
    slave:stop(MGC),

    d("simple -> stop ~p", [MG]),
    slave:stop(MG),

    d("simple -> done", []),
    ok.


exec(Proxy, Id, Cmd, Verify) ->
    ?APPLY(Proxy, Cmd),
    receive
	{res, Id, Res} ->
	    Verify(Res)
    end.


start_mgc(Proxy) ->
    ?APPLY(Proxy,
	   fun() ->
		   try megaco_simple_mgc:start() of
		       Res ->
			   Res
		   catch
		       C:E:S ->
			   {error, {{catched, C, E, S}}, code:get_path()}
		   end
	   end),
    receive
	{res, _, {ok, MgcAll}} when is_list(MgcAll) ->
	    MgcBad = [MgcRes || MgcRes <- MgcAll, element(1, MgcRes) /= ok],
	    ?VERIFY([], MgcBad),
	    ok;
	Error ->
	    ?ERROR(Error)
    end.


start_mg(Proxy) ->
    ?APPLY(Proxy, fun() -> 
			  try megaco_simple_mg:start() of
			      Res ->
				  Res
			  catch
			      C:E:S ->
				  {error, {{catched, C, E, S}}, code:get_path()}
			  end
		  end),
    receive
	{res, _, MGs} when is_list(MGs) andalso (length(MGs) =:= 4) ->
	    verify_mgs(MGs);
	Error ->
	    ?ERROR(Error)
    end.

verify_mgs(MGs) ->
    Verify = 
	fun({_MgMid, {TransId, Res}}) when (TransId =:= 1) ->
		case Res of
		    {ok, [AR]} when is_record(AR, 'ActionReply') ->
			case AR#'ActionReply'.commandReply of
			    [{serviceChangeReply, SCR}] ->
				case SCR#'ServiceChangeReply'.serviceChangeResult of
				    {serviceChangeResParms, MgcMid} 
				      when (MgcMid =/= asn1_NOVALUE) ->
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
    lists:map(Verify, MGs).
    
    
info(Proxy) ->
    ?APPLY(Proxy,
	   fun() ->
		   try megaco:info() of
		       I -> I
		   catch
		       C:E:S ->
			   {error, {C, E, S}}
		   end
	   end),
    receive
	{res, _, Info} when is_list(Info) ->
	    ?LOG("Ok, ~p~n", [Info]);
	{res, _, Error} ->
	    ?ERROR(Error)
    end.


users(Proxy) ->
    ?APPLY(Proxy,
	   fun() ->
		   try megaco:system_info(users) of
		       I -> I
		   catch
		       C:E:S ->
			   {error, {C, E, S}}
		   end
	   end),
    receive
	{res, _, Info} when is_list(Info) ->
	    ?LOG("Ok, ~p~n", [Info]);
	{res, _, Error} ->
	    ?ERROR(Error)
    end.




d(F) ->
    d(F, []).
d(F, A) ->
    d(get(dbg), F, A).

d(true, F, A) ->
    io:format("DBG: ~s " ++ F ++ "~n", [?FTS() | A]);
d(_, _F, _A) ->
    ok.
