%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(error_handler_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 undefined_function_handler/1]).

%% Callback from error_handler.
-export(['$handle_undefined_function'/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [undefined_function_handler].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------

undefined_function_handler(_) ->
    42 = ?MODULE:forty_two(),
    42 = (id(?MODULE)):forty_two(),
    {ok,{a,b,c}} = ?MODULE:one_arg({a,b,c}),
    {ok,{a,b,c}} = (id(?MODULE)):one_arg({a,b,c}),
    {'EXIT',{undef,[{?MODULE,undef_and_not_handled,[[1,2,3]],[]}|_]}} =
	(catch ?MODULE:undef_and_not_handled([1,2,3])),
    ok.

'$handle_undefined_function'(forty_two, []) ->
    42;
'$handle_undefined_function'(one_arg, [Arg]) ->
    {ok,Arg};
'$handle_undefined_function'(Func, Args) ->
    error_handler:raise_undef_exception(?MODULE, Func, Args).

id(I) ->
    I.
