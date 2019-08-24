%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wx_test_lib.hrl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Testing Macros 
%%%
%%% Created : 30 Oct 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------

-include_lib("wx/include/wx.hrl").

-define(log(Format,Args),wx_test_lib:log(Format,Args,?FILE,?LINE)).
-define(warning(Format,Args),?log("<WARNING>~n " ++ Format,Args)).
-define(error(Format,Args), wx_test_lib:error(Format,Args,?FILE,?LINE)).
-define(verbose(Format,Args),wx_test_lib:verbose(Format,Args,?FILE,?LINE)).

-define(fatal(Format,Args),
	?error(Format, Args),
	exit({test_case_fatal, Format, Args, ?FILE, ?LINE})).

-define(skip(Format,Args),
	?warning(Format, Args),
	exit({skipped, ?flat_format(Format, Args)})).

-define(m(ExpectedRes, Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
			?verbose("ok: ~p~n",[AcTuAlReS]),
			AcTuAlReS;
		    _ ->
			wx_test_lib:error("Not Matching Actual result was:~n ~p ~n Expected ~s~n",
					  [AcTuAlReS, ??ExpectedRes],
					  ?FILE,?LINE),
			AcTuAlReS
		end
	end()).

-define(mt(Expected, Expr),
	fun() ->
		{TeStFILe, TeSTLiNe} = {?FILE, ?LINE},
		AcTuAlReS = (catch (Expr)),
		case catch element(3,AcTuAlReS) of
		    Expected -> 
			wx_test_lib:verbose("ok: ~s~n",[??Expected],TeStFILe,TeSTLiNe),
			AcTuAlReS;
		    _ ->
			wx_test_lib:error("Not Matching Actual result was:~n ~p ~n Expected ~s~n",
					  [AcTuAlReS, ??Expected], 
					  TeStFILe,TeSTLiNe),
			AcTuAlReS
		end
	end()).

-define(mr(Expected, Expr),
	fun() ->
		{TeStFILe, TeSTLiNe} = {?FILE, ?LINE},
		AcTuAlReS = (catch (Expr)),
		case catch element(1,AcTuAlReS) of
		    Expected -> 
			wx_test_lib:verbose("ok: ~s~n",[??Expected],TeStFILe,TeSTLiNe),
			AcTuAlReS;
		    _ ->
			wx_test_lib:error("Not Matching Actual result was:~n ~p ~n Expected ~s~n",
					  [AcTuAlReS, ??Expected], 
					  TeStFILe,TeSTLiNe),
			AcTuAlReS
		end
	end()).
		    
-define(m_receive(ExpectedMsg),
	?m(ExpectedMsg,wx_test_lib:pick_msg())).

-define(m_multi_receive(ExpectedMsgs),
	fun() ->
		{TeStFILe, TeSTLiNe} = {?FILE, ?LINE},
		TmPeXpCtEdMsGs = lists:sort(ExpectedMsgs),		
		AcTuAlReS = 
		    lists:sort(lists:map(fun(_) ->
						 wx_test_lib:pick_msg()
					 end, TmPeXpCtEdMsGs)),
		case AcTuAlReS of
		    TmPeXpCtEdMsGs ->
			?verbose("ok: ~p~n",[AcTuAlReS]),
			AcTuAlReS;
		    _ ->
			wx_test_lib:error("Not Matching Actual result was:~n ~p ~n Expected ~p~n",
					  [AcTuAlReS, ExpectedMsgs], 
					  TeStFILe,TeSTLiNe),
			AcTuAlReS
		end
	end()).
