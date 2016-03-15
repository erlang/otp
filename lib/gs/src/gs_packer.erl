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
%% ------------------------------------------------------------
%% Erlang Graphics Interface geometry manager caclulator
%% ------------------------------------------------------------


-module(gs_packer).

-export([pack/2]).
%-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  This is a simple packer that take a specification in the format
%%%%  
%%%%    Spec       -> [WidthSpec, WidthSpec....]
%%%%    WidthSpec  -> {fixed,Size} | {stretch,Weight} |
%%%%                  {stretch,Weight,Min} | {stretch,Weight,Min,Max}
%%%%
%%%%  and a given total size it produces a list of sizes of the
%%%%  individual elements. Simple heuristics are used to make the code
%%%%  fast and simple.
%%%%
%%%%  The Weight is simply a number that is the relative size to the
%%%%  other elements that has weights. If for example the weights
%%%%  for a frame that has three columns are 40 20 100 it means that
%%%%  column 1 has 40/160'th of the space, column 2 20/160'th of
%%%%  the space and column 3 100/160'th of the space.
%%%%
%%%%  The program try to solve the equation with the constraints given.
%%%%  We have tree cases
%%%%
%%%%      o We can fullfil the request in the space given
%%%%      o We have less space than needed
%%%%      o We have more space than allowed
%%%%
%%%%  The algorithm is as follows:
%%%%
%%%%      1. Subtract the fixed size, nothing to do about that.
%%%%
%%%%      2. Calculate the Unit (or whatever it should be called), the
%%%%         given space minus the fixed sise divided by the Weights.
%%%%
%%%%      3. If we in total can fullfill the request we try to
%%%%         fullfill the individual constraints. See remove_failure/2.
%%%%
%%%%      4. If we have too little or too much pixels we take our
%%%%         specification and create a new more relaxed one. See
%%%%         cnvt_to_min/1 and cnvt_to_max/1.
%%%%
%%%%  In general we adjust the specification and redo the whole process
%%%%  until we have a specification that meet the total constraints
%%%%  and individual constraints. When we know that the constraints
%%%%  are satisfied we finally call distribute_space/2 to set the
%%%%  resulting size values for the individual elements.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pack(Size, SpecSizes) when Size < 0 ->
    pack(0, SpecSizes);
pack(Size, SpecSizes) ->
    {Weights,_Stretched,Fixed,Min,Max} = get_size_info(SpecSizes),
    Left = Size - Fixed,
    Unit = if Weights == 0 -> 0; true -> Left / Weights end,
    if
	Left < Min ->
	    NewSpecs = cnvt_to_min(SpecSizes),
	    pack(Size,NewSpecs);
	is_integer(Max), Max =/= 0, Left > Max ->
	    NewSpecs = cnvt_to_max(SpecSizes),
	    pack(Size,NewSpecs);
	true ->
	    case remove_failure(SpecSizes, Unit) of
		{no,NewSpecs} ->
		    distribute_space(NewSpecs,Unit);
		{yes,NewSpecs} ->
		    pack(Size, NewSpecs)
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  remove_failure(Specs, Unit)
%%%%
%%%%  We know that we in total have enough space to fit within the total
%%%%  maximum and minimum requirements. But we have to take care of
%%%%  individual minimum and maximum requirements.
%%%%
%%%%  This is done with a simple heuristic. We pick the element that
%%%%  has the largest diff from the required min or max, change this
%%%%  {stretch,W,Mi,Ma} to a {fixed,Mi} or {fixed,Ma} and redo the
%%%%  whole process again.
%%%%
%%%%  **** BUGS ****
%%%%  No known. But try to understand this function and you get a medal ;-)
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


remove_failure(Specs, Unit) ->
    case remove_failure(Specs, Unit, 0) of
	{done,NewSpecs} ->
	    {yes,NewSpecs};
	{_,_NewSpecs} ->
	    {no,Specs}				% NewSpecs == Specs but
    end.					% we choose the old one

remove_failure([], _Unit, MaxFailure) ->
    {MaxFailure,[]};
remove_failure([{stretch,W,Mi} | Specs], Unit, MaxFailure) ->
    {MinMax,NewMaxFailure} = max_failure(MaxFailure, Mi-W*Unit, 0),
    case {MinMax,remove_failure(Specs, Unit, NewMaxFailure)} of
	{min,{NewMaxFailure,Rest}} ->
	    {done,[{fixed,Mi} | Rest]};
	{_,{OtherMaxFailure, Rest}} ->
	    {OtherMaxFailure,[{stretch,W,Mi} | Rest]}
    end;
remove_failure([{stretch,W,Mi,Ma} | Specs], Unit, MaxFailure) ->
    {MinMax,NewMaxFailure} = max_failure(MaxFailure, Mi-W*Unit, W*Unit-Ma),
    case {MinMax,remove_failure(Specs, Unit, NewMaxFailure)} of
	{min,{NewMaxFailure,Rest}} ->
	    {done,[{fixed,Mi} | Rest]};
	{max,{NewMaxFailure,Rest}} ->
	    {done,[{fixed,Ma} | Rest]};
	{_,{OtherMaxFailure, Rest}} ->
	    {OtherMaxFailure,[{stretch,W,Mi,Ma} | Rest]}
    end;
remove_failure([Spec | Specs], Unit, MaxFailure) ->
    {NewMaxFailure,NewSpecs} = remove_failure(Specs, Unit, MaxFailure),
    {NewMaxFailure, [Spec | NewSpecs]}.

max_failure(LastDiff, DMi, DMa)
  when DMi > LastDiff, DMi > DMa ->
    {min,DMi};
max_failure(LastDiff, _DMi, DMa)
  when DMa > LastDiff ->
    {max,DMa};
max_failure(MaxFailure, _DMi, _DMa) ->
    {other,MaxFailure}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  distribute_space(Spec,Unit)
%%%%
%%%%  We now know that we can distribute the space to the elements in
%%%%  the list.
%%%%
%%%%  **** BUGS ****
%%%%  No known bugs. It try hard to distribute the pixels so that
%%%%  there should eb no pixels left when done but there is no proof
%%%%  that this is the case. The distribution of pixels may also
%%%%  not be optimal. The rounding error from giving one element some
%%%%  pixels is added to the next even if it would be better to add
%%%%  it to an element later in the list (for example the weights
%%%%  1000, 2, 1000). But this should be good enough.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


distribute_space(Specs, Unit) ->
    distribute_space(Specs, Unit, 0.0).

distribute_space([], _Unit, _Err) ->
    [];
distribute_space([Spec | Specs], Unit, Err) ->
    distribute_space(Spec, Specs, Unit, Err).

distribute_space({fixed,P}, Specs, Unit, Err) ->
    [P | distribute_space(Specs, Unit, Err)];
distribute_space({stretch,Weight}, Specs, Unit, Err) ->
    Size = Weight * Unit + Err,
    Pixels = round(Size),
    NewErr = Size - Pixels,
    [Pixels | distribute_space(Specs, Unit, NewErr)];
distribute_space({stretch,W,_Mi}, Specs, Unit, Err) ->
    distribute_space({stretch,W}, Specs, Unit, Err);
distribute_space({stretch,W,_Mi,_Ma}, Specs, Unit, Err) ->
    distribute_space({stretch,W}, Specs, Unit, Err).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  cnvt_to_min(Spec)
%%%%  cnvt_to_max(Spec)
%%%%
%%%%  If the space we got isn't enough for the total minimal or maximal
%%%%  requirements then we convert the specification to a more relaxed
%%%%  one that we always can satisfy.
%%%%
%%%%  This is fun! We do a simple transformation from one specification
%%%%  to a new one. The min, max and fixed size are our new weights!
%%%%  This way the step from a specification we can satisfy and one
%%%%  close that we can't is only a few pixels away, i.e. the transition
%%%%  from within the constraints and outside will be smooth.
%%%%
%%%%  **** BUGS ****
%%%%  No known bugs.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cnvt_to_min([]) ->
    [];
cnvt_to_min([Spec | Specs]) ->
    cnvt_to_min(Spec, Specs).

cnvt_to_max([]) ->
    [];
cnvt_to_max([Spec | Specs]) ->
    cnvt_to_max(Spec, Specs).

cnvt_to_min({fixed,P}, Specs) ->
    [{stretch,P} | cnvt_to_min(Specs)];
cnvt_to_min({stretch,_W}, Specs) ->
    [{fixed,0} | cnvt_to_min(Specs)];
cnvt_to_min({stretch,_W,Mi}, Specs) ->
    [{stretch,Mi} | cnvt_to_min(Specs)];
cnvt_to_min({stretch,_W,Mi,_Ma}, Specs) ->
    [{stretch,Mi} | cnvt_to_min(Specs)].

%% We know that there can only be {fixed,P} and {stretch,W,Mi,Ma}
%% in this list.

cnvt_to_max({fixed,P}, Specs) ->
    [{stretch,P} | cnvt_to_max(Specs)];
cnvt_to_max({stretch,_W,_Mi,Ma}, Specs) ->
    [{stretch,Ma} | cnvt_to_max(Specs)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  Sum the Weights, Min and Max etc
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_size_info(Specs) ->
    get_size_info(Specs, 0, 0, 0, 0, 0).

get_size_info([], TotW, NumW, TotFixed, TotMin, TotMax) ->
    {TotW, NumW, TotFixed, TotMin, TotMax};
get_size_info([Spec | Specs], TotW, NumW, TotFixed, TotMin, TotMax) ->
    get_size_info(Spec, TotW, NumW, TotFixed, TotMin, TotMax, Specs).
    
get_size_info({fixed,P}, TotW, NumW, TotFixed, TotMin, TotMax, Specs) ->
    get_size_info(Specs, TotW, NumW, TotFixed+P, TotMin, TotMax);
get_size_info({stretch,W}, TotW, NumW, TotFixed, TotMin, _TotMax, Specs) ->
    get_size_info(Specs, TotW+W, NumW+1, TotFixed, TotMin, infinity);
get_size_info({stretch,W,Mi}, TotW, NumW, TotFixed, TotMin, _TotMax, Specs) ->
    get_size_info(Specs, TotW+W, NumW+1, TotFixed, TotMin+Mi, infinity);
get_size_info({stretch,W,Mi,_Ma}, TotW, NumW, TotFixed, TotMin, infinity, Specs) ->
    get_size_info(Specs, TotW+W, NumW+1, TotFixed, TotMin+Mi, infinity);
get_size_info({stretch,W,Mi,Ma}, TotW, NumW, TotFixed, TotMin, TotMax, Specs) ->
    get_size_info(Specs, TotW+W, NumW+1, TotFixed, TotMin+Mi, TotMax+Ma).
