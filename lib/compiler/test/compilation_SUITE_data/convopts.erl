%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(convopts).

-export([?MODULE/0]).

-define(UINT32_MAX, ((1 bsl 32) - 1)).

?MODULE() ->
    Type = 12345,
    Inst = 1,
    Zone = 0,
    case convopts([{dest, {tipc_name, Type, Inst, Zone}}]) of
	{ok, [{$B, <<Type:32, Inst:32, Zone:32>>}]} ->
	    ok;
	Other ->
	    {error, Other}
    end.



convopts(Opts) ->
    catch 
	case getopts(Opts, [active, recvfrom, importance, distribution,
			    dest]) of
	    {[Active, Recvfrom, Importance, Distribution,
	      Dest], []} ->
		{R, RR} = 
		    case {Active, Recvfrom} of
			{[], [recvfrom]} ->
			    throw({error, einval});
			{_, [recvfrom]} ->
			    {$r, $R};
			{_, _} ->
			    {$e, $E}
		    end,
		{ok, 
		 lists:flatten(
		   [case Active of
			[{active, true}] ->
			    [{R, <<?UINT32_MAX:32>>}];
			[{active, N}] when integer(N), 
					   0 =< N, N < ?UINT32_MAX ->
			    [{R, <<N:32>>}];
			[{active, N}] when integer(N), 
					   -?UINT32_MAX < N, N < 0 ->
			    [{RR, <<-N:32>>}];
			[{active, once}] ->
			    [{R, <<1:32>>}];
			[{active, false}] ->
			    [{R, <<0:32>>}];
			[] ->
			    [];
			_ ->
			    throw({error, einval})
		    end,
		    case Importance of
			[{importance, normal}] ->
			    [{$i, []}];
			[{importance, high}] ->
			    [{$I, []}];
			[] ->
			    [];
			_ ->
			    throw({error, einval})
		    end,
		    case Distribution of
			[distribution] ->
			    [{$d, []}];
			[] ->
			    [];
			_ ->
			    throw({error, einval})
		    end,
		    case Dest of
			%% Port addressed message
			[{dest, {tipc_port_id, Port, Proc}}]
			when binary(Port), 
			     integer(Proc), 0 =< Proc, Proc =< ?UINT32_MAX
			     ;
			     list(Port), 
			     integer(Proc), 0 =< Proc, Proc =< ?UINT32_MAX ->
			    [{$p, [Port | <<Proc:32>>]}];
			%% Name addressed message
			[{dest, {tipc_name, Type, Inst, Zone}}]
			when integer(Type), 0 =< Type, Type =< ?UINT32_MAX,
			     integer(Inst), 0 =< Inst, Inst =< ?UINT32_MAX,
			     integer(Zone), 0 =< Zone, Zone =< ?UINT32_MAX ->
			    [{$B, <<Type:32, Inst:32, Zone:32>>}];
			%%
			%% This undocumented clause uses an undocumented 
			%% feature of the TIPC socket interface that takes 
			%% advantage of some gory internals of the protocol. 
			%% It is protocol implementation dependant and 
			%% breaks the whole idea of location transparency 
			%% for name addressed messages. Therefore it should 
			%% only be used when all other possibilities are 
			%% exhausted.
			%%
			[{dest, {tipc_name, Type, Inst,
				 {tipc_processor_id,
				  Zone, Subnetwork, Processor}}}]
			when integer(Type), 0 =< Type, Type =< ?UINT32_MAX,
			     integer(Inst), 0 =< Inst, Inst =< ?UINT32_MAX,
			     integer(Zone), 
			     0 =< Zone,       Zone       < 16#FF,
			     integer(Subnetwork), 
			     0 =< Subnetwork, Subnetwork < 16#FFF,
			     integer(Processor), 
			     0 =< Processor,  Processor  < 16#FFF ->
			    [{$B, <<Type:32, 
				   Inst:32, 
				   Zone:8, Subnetwork:12, Processor:12>>}];
			[] ->
			    [];
			_ ->
			    throw({error, einval})
		    end
		   ])};
	    _ ->
		throw({error, einval})
	end.



getopts(List, Options) when list(List), list(Options) ->
    getopts_1(Options, List, []).

getopts_1([], List, Result) ->
    {lists:reverse(Result), List};
getopts_1([Option | Options], List, Result) ->
    {Optvals, Remaining} = getopts_2(List, Option, [], []),
    getopts_1(Options, Remaining, [Optvals | Result]).

getopts_2([], _Option, Result, Remaining) ->
    {lists:reverse(Result), lists:reverse(Remaining)};
getopts_2([Option | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, [Option | Result], Remaining);
getopts_2([Optval | Tail], Option, Result, Remaining) 
  when element(1, Optval) == Option ->
    getopts_2(Tail, Option, [Optval | Result], Remaining);
getopts_2([Other | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, Result, [Other | Remaining]).
