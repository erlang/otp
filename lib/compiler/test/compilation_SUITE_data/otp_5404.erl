%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(otp_5404).
-compile(export_all).

%% Thanks to Martin Bjorklund.

?MODULE() ->
    ok.

-record(bar, {status, vs = []}).

y() ->
    x({foo, 1, []}).

get_bar() ->
    #bar{status = 1}.

x(Trans) ->
    {foo, Barno, _} = Trans,
    case get_bar() of
	Bar when Bar#bar.status /= 2 ->
	    if 1 == 1 ->
		    mnesia:dirty_delete({bar, Barno}),
		    Vs = [1,2] ++ Bar#bar.vs,
		    Bar33 = Bar#bar{status = 1},
		    Bar1 = Bar#bar{status = 3,
				   vs = Vs},
		    [{payment, Barno}];
	       true ->
		    Barno
	    end;
	_ ->
	    Trans
    end.
