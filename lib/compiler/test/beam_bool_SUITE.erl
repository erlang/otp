%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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
-module(beam_bool_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 before_and_inside_if/1,
	 scotland/1,y_registers/1,protected/1,
	 maps/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [before_and_inside_if,
       scotland,
       y_registers,
       protected,
       maps
      ]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

before_and_inside_if(_Config) ->
    no = before_and_inside_if([a], [b], delete),
    no = before_and_inside_if([a], [b], x),
    no = before_and_inside_if([a], [], delete),
    no = before_and_inside_if([a], [], x),
    no = before_and_inside_if([], [], delete),
    yes = before_and_inside_if([], [], x),
    yes = before_and_inside_if([], [b], delete),
    yes = before_and_inside_if([], [b], x),

    {ch1,ch2} = before_and_inside_if_2([a], [b], blah),
    {ch1,ch2} = before_and_inside_if_2([a], [b], xx),
    {ch1,ch2} = before_and_inside_if_2([a], [], blah),
    {ch1,ch2} = before_and_inside_if_2([a], [], xx),
    {no,no} = before_and_inside_if_2([], [b], blah),
    {no,no} = before_and_inside_if_2([], [b], xx),
    {ch1,no} = before_and_inside_if_2([], [], blah),
    {no,ch2} = before_and_inside_if_2([], [], xx),
    ok.

%% Thanks to Simon Cornish and Kostis Sagonas.
%% Used to crash beam_bool.
before_and_inside_if(XDo1, XDo2, Do3) ->
    Do1 = (XDo1 =/= []),
    Do2 = (XDo2 =/= []),
    if
	%% This expression occurs in a try/catch (protected)
	%% block, which cannot refer to variables outside of
	%% the block that are boolean expressions.
	Do1 =:= true;
	Do1 =:= false, Do2 =:= false, Do3 =:= delete ->
	    no;
       true ->
	    yes
    end.

%% Thanks to Simon Cornish.
%% Used to generate code that would not set {y,0} on
%% all paths before its use (and therefore fail
%% validation by the beam_validator).
before_and_inside_if_2(XDo1, XDo2, Do3) ->
    Do1    = (XDo1 =/= []),
    Do2    = (XDo2 =/= []),
    CH1 = if Do1 == true;
	     Do1 == false,Do2==false,Do3 == blah ->
		  ch1;
	     true ->
		  no
	  end,
    CH2 = if Do1 == true;
	     Do1 == false,Do2==false,Do3 == xx ->
		  ch2;
	     true ->
		  no
	  end,
    {CH1,CH2}.


%% beam_bool would remove the initialization of {y,0}.
%% (Thanks to Thomas Arts and QuickCheck.)

scotland(_Config) ->
    million = do_scotland(placed),
    {'EXIT',{{badmatch,placed},_}} = (catch do_scotland(false)),
    {'EXIT',{{badmatch,placed},_}} = (catch do_scotland(true)),
    {'EXIT',{{badmatch,placed},_}} = (catch do_scotland(echo)),
    ok.

do_scotland(Echo) ->
  found(case Echo of
	    Echo when true; Echo, Echo, Echo ->
		Echo;
	    echo ->
		[]
	end,
	Echo = placed).

found(_, _) -> million.


%% ERL-143: beam_bool could not handle Y registers as a destination.
y_registers(_Config) ->
    {'EXIT',{badarith,[_|_]}} = (catch baker(valentine)),
    {'EXIT',{badarith,[_|_]}} = (catch baker(clementine)),

    {not_ok,true} = potter([]),
    {ok,false} = potter([{encoding,any}]),

    ok.

%% Thanks to Quickcheck.
baker(Baker) ->
    (valentine == Baker) +
	case Baker of
	    Baker when Baker; Baker ->
		Baker;
	    Baker ->
		[]
	end.

%% Thanks to Jose Valim.
potter(Modes) ->
    Raw = lists:keyfind(encoding, 1, Modes) == false,
    Final = case Raw of
		X when X == false; X == nil -> ok;
		_ -> not_ok
	    end,
    {Final,Raw}.

protected(_Config) ->
    {'EXIT',{if_clause,_}} = (catch photographs({1, surprise, true}, opinions)),

    {{true}} = welcome({perfect, true}),
    {'EXIT',{if_clause,_}} = (catch welcome({perfect, false})),
    ok.

photographs({_Violation, surprise, Deep}, opinions) ->
    {if
	 0; "here", Deep ->
	     Deep = Deep
     end}.

welcome({perfect, Profit}) ->
    if
	Profit, Profit, Profit; 0 ->
	    {id({Profit})}
    end.

maps(_Config) ->
    ok = evidence(#{0 => 42}).

%% Cover handling of put_map in in split_block_label_used/2.
evidence(#{0 := Charge}) when 0; #{[] => Charge} == #{[] => 42} ->
    ok.


%%%
%%% Common utilities.
%%%

id(I) ->
    I.
