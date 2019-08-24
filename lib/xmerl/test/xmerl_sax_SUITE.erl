%%-*-erlang-*-
%%----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
    [{bugs, [], [ticket_8213, ticket_8214, ticket_11551, 
                 fragmented_xml_directive,
                 old_dom_event_fun_endDocument_bug, 
                 event_fun_endDocument_error_test,
                 event_fun_startDocument_error_test]}].

%%======================================================================
%% Tests
%%======================================================================
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
    {ok, undefined, <<"\n<?xml",  _/binary>>} = xmerl_sax_parser:stream(Stream1, []),
    Stream2= <<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>


<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>">>,
    {ok, undefined, <<"\n\n\n<?xml",  _/binary>>} = xmerl_sax_parser:stream(Stream2, []),
    Stream3= <<"<a>hej</a>

<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<a>hej</a>">>,
    {ok, undefined, <<"\n\n<?xml",  _/binary>>} = xmerl_sax_parser:stream(Stream3, []),
    ok.

%%----------------------------------------------------------------------
%% Test Case 
%% ID: fragmented_xml_directive
%% Test of fragmented xml directive by reading one byte per continuation ca
fragmented_xml_directive(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Name = filename:join(DataDir, "test_data_1.xml"),
    {ok, Fd} =  file:open(Name, [raw, read,binary]),
    Cf = fun cf_fragmented_xml_directive/1,
    {ok, undefined, _} = xmerl_sax_parser:stream(<<>>, 
                                                 [{continuation_fun, Cf}, 
                                                  {continuation_state, Fd}]),
    ok.

%%----------------------------------------------------------------------
%% Test Case 
%% ID: old_dom_event_fun_endDocument_bug
%%     The old_dom backend previous generateded an uncatched exception
%%     instead of the correct fatal_error from the parser.
old_dom_event_fun_endDocument_bug(_Config) ->
    %% Stream contains bad characters, 
    {fatal_error, _, _, _, _} = 
        xmerl_sax_parser:stream([60,63,120,109,108,32,118,101,114,115,105,111,110,61,39,49,46,48,39,32,101,110,99,111,100,105,110,103,61,39,117,116,102,45,56,39,63,62,60,
                                 99,111,109,109,97,110,100,62,60,104,101,97,100,101,114,62,60,116,114,97,110,115,97,99,116,105,111,110,73,100,62,49,60,47,116,114,97,110,
                                 115,97,99,116,105,111,110,73,100,62,60,47,104,101,97,100,101,114,62,60,98,111,100,121,62,95,226,130,172,59,60,60,47,98,111,100,121,62,60,
                                 47,99,111,109,109,97,110,100,62,60,47,120,49,95,49,62],
                                [{event_fun,fun xmerl_sax_old_dom:event/3},
                                 {event_state,xmerl_sax_old_dom:initial_state()}]),
    ok.

%%----------------------------------------------------------------------
%% Test Case 
%% ID: event_fun_endDocument_error_test
event_fun_endDocument_error_test(_Config) ->
    Stream = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><a>hej</a>">>,
    Ef = fun(endDocument, _ , _) ->  throw({event_error, "endDocument error"});
            (_, _, S) -> S
         end,
    {event_error, _, _, _, _} = xmerl_sax_parser:stream(Stream, [{event_fun, Ef}]),
    ok.

%%----------------------------------------------------------------------
%% Test Case 
%% ID: event_fun_startDocument_error_test
event_fun_startDocument_error_test(_Config) ->
    Stream = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><a>hej</a>">>,
    Ef = fun(startDocument, _ , _) ->  throw({event_error, "endDocument error"});
            (_, _, S) -> S
         end,
    {event_error, _, _, _, _} = xmerl_sax_parser:stream(Stream, [{event_fun, Ef}]),
    ok.

%%======================================================================
%% Internal functions
%%======================================================================
cf_fragmented_xml_directive(IoDevice) ->
    case file:read(IoDevice, 1) of
	eof ->
	    {<<>>, IoDevice};
	{ok, FileBin} ->
	    {FileBin, IoDevice}
    end.
