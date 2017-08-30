%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-include_lib("wx/include/wx.hrl").

-define(log(Format,Args), reltool_test_lib:log(Format,Args,?FILE,?LINE)).
-define(error(Format,Args), reltool_test_lib:error(Format,Args,?FILE,?LINE)).
-define(verbose(Format,Args), reltool_test_lib:verbose(Format,Args,?FILE,?LINE)).

-define(ignore(Expr),
        fun() ->
                AcTuAlReS = (catch (Expr)),
		?verbose("ok: ~p\n",[AcTuAlReS]),
		AcTuAlReS
        end()).

-define(msym(ExpectedRes, Expr),
        fun() ->
                AcTuAlReS = (catch (Expr)),
                case AcTuAlReS of
                    ExpectedRes ->
                        ?verbose("ok: ~p\n",[AcTuAlReS]),
                        AcTuAlReS;
                    _ ->
                        reltool_test_lib:error("Not matching actual result was:\n ~p \nExpected ~s\n",
					       [AcTuAlReS, ??ExpectedRes], 
					       ?FILE, ?LINE),
                        AcTuAlReS
                end
        end()).

-define(m(ExpectedRes, Expr),
        fun() ->
                AcTuAlReS = (catch (Expr)),
                case AcTuAlReS of
                    ExpectedRes ->
                        ?verbose("ok: ~p\n",[AcTuAlReS]),
                        AcTuAlReS;
                    _ ->
                        reltool_test_lib:error("Not matching actual result was:\n\t~p \nExpected:\n\t~p\n",
					       [AcTuAlReS, ExpectedRes], 
					       ?FILE, ?LINE),
                        AcTuAlReS
                end
        end()).
