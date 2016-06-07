%%-*-erlang-*-
%%----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : xmerl_sax_SUITE.erl
%% Created : 2009-06-01
%%----------------------------------------------------------------------
-module(xmerl_sax_SUITE).
-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

all() ->
    [{group, bugs}].

groups() ->
    [{bugs, [], [ticket_8213, ticket_8214, ticket_11551]}].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Test Case 
%% ID: ticket_8213
%% Description: Checks that end of document is checked properly when continuation fun is missing.
ticket_8213(_Config) -> 
    {ok,ok,[]} = xmerl_sax_parser:stream("<elem/>", [{event_fun, fun (_E,_,_) -> ok end}]),
    ok.


%%----------------------------------------------------------------------
%% Test Case 
%% ID: ticket_8214
%% Description: Checks that attributes with default namespace don't get [] in NS field.
ticket_8214(_Config) -> 
    Event = fun ({startElement,"http://lshift.net/d","elem",
                  {[],"elem"},
                  [{[],[],"attr","123"},{"http://lshift.net/x","x","attr","234"}]},
                 _, _) ->ok;
                ({startElement, _, "elem",_,_}, _,_) ->
                    throw({test, "Error in startElement tuple"});
                (_E,_,_) -> ok
            end,

    {ok,ok,[]} = xmerl_sax_parser:stream("<elem attr='123' x:attr='234' xmlns='http://lshift.net/d' "
                                         "xmlns:x='http://lshift.net/x' />",
                                         [{event_fun, Event}]),
    ok.

%%----------------------------------------------------------------------
%% Test Case 
%% ID: ticket_8214
%% Description: Checks that attributes with default namespace don't get [] in NS field.
ticket_11551(_Config) ->
    Stream1 = <<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>
<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>">>,
    {ok, undefined, <<"<?xml",  _/binary>>} = xmerl_sax_parser:stream(Stream1, []),
    Stream2= <<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>


<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>">>,
    {ok, undefined, <<"<?xml",  _/binary>>} = xmerl_sax_parser:stream(Stream2, []),
    Stream3= <<"<a>hej</a>

<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>">>,
    {ok, undefined, <<"<?xml",  _/binary>>} = xmerl_sax_parser:stream(Stream3, []),
    ok.
