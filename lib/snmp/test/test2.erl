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

-module(test2).

-compile(export_all).

%%-----------------------------------------------------------------
%% Implements the Test2 MIB.  Used to test processing
%% of requests.
%%-----------------------------------------------------------------
tDescr(get, 2) ->
    {noValue, noSuchName};
tDescr(get, 3) ->
    {noValue, noSuchInstance};
tDescr(get, 4) ->
    {noValue, noSuchObject}.

tDescr(is_set_ok, "badValue", 2) ->
    badValue;
tDescr(is_set_ok, "inconsistentValue", 2) ->
    inconsistentValue;
tDescr(is_set_ok, "resourceUnavailable", 2) ->
    resourceUnavailable;
tDescr(is_set_ok, "inconsistentName", 2) ->
    inconsistentName;
tDescr(is_set_ok, "is_set_ok_fail", 2) ->
    genErr;
tDescr(set, "commit_fail", 2) ->
    commitFailed.

tGenErr(get, 1) ->
    genErr;
tGenErr(get, 2) ->
    1=2;
tGenErr(get, 3) ->
    {value, "not an integer, I know"}.

tInt(is_set_ok, 5, 3) ->
    wrongValue.


tTable(is_set_ok, [1,1], [{2, "noCreation"}]) ->
    {noCreation, 2};
tTable(is_set_ok, [1,2], [{2, "inconsistentName"}]) ->
    {inconsistentName, 2};
tTable(get_next, _RowIndex, Cols) ->
    lists:map(fun(_) -> endOfTable end, Cols).
		      
%% Only 2 reqs are valid:
%% gn([[tCnt2, 1]])
%% gn([[tCnt2, 2]])
%% ... or as:
%% gb(0, 2, [[tCnt2,1]])
tTable2(get_next, [1], [2]) ->
    [{[2,2], 100}];
tTable2(get_next, [2], [2]) ->
    [endOfTable].

		      
		      

tTooBig(get) ->
    {value, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}.
