%%--------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%----------------------------------------------------------------------
%% File    : xmerl_sax_parser.erl
%% Description : XML SAX parse API module.
%%
%% Created :  4 Jun 2008 
%%----------------------------------------------------------------------
-module(xmerl_sax_parser).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("xmerl_sax_parser.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 file/2,
	 stream/2
	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
	 default_continuation_cb/1
        ]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: file(Filename, Options) -> Result
%% Input:    Filename = string()
%%           Options = [{OptTag, term()}]
%%           OptTag = event_state | event_fun | continuation_state |
%%                    continuation_fun | ....
%% Output:   Result = {ok, EventState, Rest}
%%           Rest = unicode_binary() | latin1_binary()
%%           EventState = term()
%% Description: Parse file containing an XML document.
%%----------------------------------------------------------------------
file(Name,Options) ->
    case file:open(Name, [raw, read,binary])  of
        {error, Reason} ->
            {error,{Name, file:format_error(Reason)}};
        {ok, FD} ->
	    Dir = filename:dirname(Name),
	    CL = filename:absname(Dir),
            File = filename:basename(Name),
	    ContinuationFun = fun default_continuation_cb/1,
            Res = stream(<<>>, [{continuation_fun, ContinuationFun},
			 {continuation_state, FD}, 
			 {current_location, CL},
			 {entity, File}
			 |Options]),
	    file:close(FD),
	    Res
    end.

%%----------------------------------------------------------------------
%% Function: stream(Xml, Options) -> Result
%% Input:    Xml = string() | binary()
%%           Options = [{OptTag, term()}]
%%           OptTag = event_state | event_fun | continuation_state |
%%                    continuation_fun | ....
%% Output:   Result = {ok, EventState, Rest}
%%           Rest = unicode_binary() | latin1_binary() | [unicode_char()]
%%           EventState = term()
%% Description: Parse a stream containing an XML document.
%%----------------------------------------------------------------------
stream(Xml, Options) when is_list(Xml), is_list(Options) ->
    State = parse_options(Options, initial_state()),
    case  State#xmerl_sax_parser_state.file_type of
	dtd ->
	    xmerl_sax_parser_list:parse_dtd(Xml, State#xmerl_sax_parser_state{encoding = list});
	normal ->
	    xmerl_sax_parser_list:parse(Xml, State#xmerl_sax_parser_state{encoding = list})
    end;
stream(Xml, Options) when is_binary(Xml), is_list(Options) ->
    case parse_options(Options, initial_state()) of 
	{error, Reason} -> {error, Reason};
	State ->
	    ParseFunction = 
		case  State#xmerl_sax_parser_state.file_type of
		    dtd ->
			parse_dtd;
		    normal ->
			parse
		end,
	    case detect_charset(Xml, State) of
		{error, Reason} -> {fatal_error, 
				    {
				      State#xmerl_sax_parser_state.current_location,
				      State#xmerl_sax_parser_state.entity, 
				      1
				     },
				    Reason, 
				    [], 
				    State#xmerl_sax_parser_state.event_state};
		{Xml1, State1} ->
		    parse(Xml1, State1, ParseFunction)
	    end
    end.


%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function: parse(Encoding, Xml, State, F) -> Result
%% Input:    Encoding = atom()
%%           Xml = [integer()] | binary()
%%           State = #xmerl_sax_parser_state
%%           F = atom()
%% Output:   Result = {ok, Rest, EventState}
%%           Rest = list() | binary()
%%           EventState = term()
%% Description: Chooses the correct parser depending on the encoding.
%%----------------------------------------------------------------------
parse(Xml, #xmerl_sax_parser_state{encoding=utf8}=State, F) ->
    xmerl_sax_parser_utf8:F(Xml, State);
parse(Xml, #xmerl_sax_parser_state{encoding={utf16,little}}=State, F) ->
    xmerl_sax_parser_utf16le:F(Xml, State);
parse(Xml, #xmerl_sax_parser_state{encoding={utf16,big}}=State, F) ->
    xmerl_sax_parser_utf16be:F(Xml, State);
parse(Xml, #xmerl_sax_parser_state{encoding=latin1}=State, F) ->
    xmerl_sax_parser_latin1:F(Xml, State);
parse(_, #xmerl_sax_parser_state{encoding=Enc}, _) -> 
    {error, lists:flatten(io_lib:format("Charcter set ~p not supported", [Enc]))}.

%%----------------------------------------------------------------------
%% Function: initial_state/0
%% Input:    -
%% Output:   #xmerl_sax_parser_state{}
%% Description: Creates the initial state record.
%%----------------------------------------------------------------------
initial_state() ->
    #xmerl_sax_parser_state{
	       event_fun = fun default_event_cb/3,
	       ns = [{"xml", "http://www.w3.org/XML/1998/namespace"}],
	       current_location = ".",
	       entity = ""
	      }.

%%----------------------------------------------------------------------
%% Function: parse_options(Options, State)
%% Input:    Options = [Option]
%%           Option = {event_state, term()} | {event_fun, fun()} | 
%%                    {continuation_state, term()} | {continuation_fun, fun()} |
%%                    {encoding, Encoding} | {file_type, FT} 
%%           FT = normal | dtd
%%           Encoding = utf8 | utf16le | utf16be | list | iso8859
%%           State = #xmerl_sax_parser_state{}
%% Output:   #xmerl_sax_parser_state{}
%% Description: Checks the parser options.
%%----------------------------------------------------------------------
parse_options([], State) ->
    State;
parse_options([{event_state, CbState} |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{event_state = CbState});
parse_options([{event_fun, CbF} |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{event_fun = CbF});
parse_options([{continuation_state, CState} |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{continuation_state = CState});
parse_options([{continuation_fun, CF} |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{continuation_fun = CF});
parse_options([{file_type, FT} |Options], State) when FT==normal; FT==dtd ->
    parse_options(Options, State#xmerl_sax_parser_state{file_type = FT});
parse_options([{encoding, E} |Options], State) ->
    case check_encoding_option(E) of
	{error, Reason} -> 
	    {error, Reason};
	Enc ->
	    parse_options(Options, State#xmerl_sax_parser_state{encoding = Enc})
    end;
parse_options([{current_location, CL} |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{current_location = CL});
parse_options([{entity, Entity} |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{entity = Entity});
parse_options([skip_external_dtd |Options], State) ->
    parse_options(Options, State#xmerl_sax_parser_state{skip_external_dtd = true});
parse_options([O |_], _State) ->
     {error, 
	    lists:flatten(io_lib:format("Option: ~p not supported", [O]))}.


check_encoding_option(E) when E==utf8; E=={utf16,little}; E=={utf16,big};
			      E==latin1; E==list ->
    E;
check_encoding_option(utf16) ->
    {utf16,big};
check_encoding_option(E) ->
    {error, io_lib:format("Charcter set ~p not supported", [E])}.

%%----------------------------------------------------------------------
%% Function: detect_charset(Xml, State)
%% Input:  Xml = list() | binary()
%%         State = #xmerl_sax_parser_state{}
%% Output:  {utf8|utf16le|utf16be|iso8859, Xml, State}
%% Description: Detects which character set is used in a binary stream.
%%----------------------------------------------------------------------
detect_charset(<<>>, #xmerl_sax_parser_state{continuation_fun = undefined} = _) ->
    throw({error, "Can't detect character encoding due to no indata"});
detect_charset(<<>>, #xmerl_sax_parser_state{continuation_fun = CFun, 
				      continuation_state = CState} = State) ->
    case CFun(CState) of
	{<<>>,  _} ->
	    throw({error, "Can't detect character encoding due to lack of indata"});
	{NewBytes, NewContState} ->
	    detect_charset(NewBytes, State#xmerl_sax_parser_state{continuation_state = NewContState})
    end;
detect_charset(Bytes, State) ->
    case unicode:bom_to_encoding(Bytes) of
	{latin1, 0} ->
	    detect_charset_1(Bytes, State);
	{Enc, Length} ->
	    <<_:Length/binary, RealBytes/binary>> = Bytes,
	    {RealBytes, State#xmerl_sax_parser_state{encoding=Enc}}
    end.

detect_charset_1(<<16#00, 16#3C, 16#00, 16#3F, _/binary>> = Xml, State) ->
    {Xml, State#xmerl_sax_parser_state{encoding={utf16, big}}};
detect_charset_1(<<16#3C, 16#00, 16#3F, 16#00, _/binary>> = Xml, State) ->
    {Xml, State#xmerl_sax_parser_state{encoding={utf16, little}}};
detect_charset_1(<<16#3C, 16#3F, 16#78, 16#6D, 16#6C, Xml2/binary>> = Xml, State) ->
    case parse_xml_directive(Xml2) of
	{error, Reason} ->
	    {error, Reason};
	AttrList ->
	    case lists:keysearch("encoding", 1, AttrList) of
		{value, {_, E}} ->
		    case convert_encoding(E) of
			{error, Reason} ->
			    {error, Reason};
			Enc ->
			    {Xml, State#xmerl_sax_parser_state{encoding=Enc}}
		    end;
		_ ->
		    {Xml, State}
	    end
    end;
detect_charset_1(Xml, State) ->
    {Xml, State}.

%%----------------------------------------------------------------------
%% Function: convert_encoding(Enc)
%% Input:  Enc = string()
%% Output:  utf8 | iso8859
%% Description: Converting 7,8 bit and utf8 encoding strings to internal format.
%%----------------------------------------------------------------------
convert_encoding(Enc) -> %% Just for 7,8 bit + utf8
    case string:to_lower(Enc) of
	"utf-8" -> utf8;
	"us-ascii" -> utf8;
	"iso-8859-1" -> latin1; % Handle all iso-8859 as latin1
	"iso-8859-2" -> latin1;
	"iso-8859-3" -> latin1;
	"iso-8859-4" -> latin1;
	"iso-8859-5" -> latin1;
	"iso-8859-6" -> latin1;
	"iso-8859-7" -> latin1;
	"iso-8859-8" -> latin1;
	"iso-8859-9" -> latin1;
	_ -> {error, "Unknown encoding: " ++ Enc}
    end.

%%----------------------------------------------------------------------
%% Function: parse_xml_directive(Xml)
%% Input:  Xml = binary()
%%         Acc = list()
%% Output:  
%% Description: Parsing the xml declaration from the input stream.
%%----------------------------------------------------------------------
parse_xml_directive(<<C, Rest/binary>>) when ?is_whitespace(C) ->
   parse_xml_directive_1(Rest, []).
    
%%----------------------------------------------------------------------
%% Function: parse_xml_directive_1(Xml, Acc) -> [{Name, Value}]
%% Input:  Xml = binary()
%%         Acc = [{Name, Value}]
%%         Name = string()
%%         Value = string()
%% Output: see above
%% Description: Parsing the xml declaration from the input stream.
%%----------------------------------------------------------------------
parse_xml_directive_1(<<C, Rest/binary>>, Acc) when ?is_whitespace(C) ->
    parse_xml_directive_1(Rest, Acc);
parse_xml_directive_1(<<"?>", _/binary>>, Acc) ->
    Acc;
parse_xml_directive_1(<<C, Rest/binary>>, Acc) when 97 =< C, C =< 122 ->
    {Name, Rest1} = parse_name(Rest, [C]),
    Rest2 = parse_eq(Rest1),
    {Value, Rest3} = parse_value(Rest2),
    parse_xml_directive_1(Rest3, [{Name, Value} |Acc]);
parse_xml_directive_1(_, _) ->
    {error, "Unknown attribute in xml directive"}.

%%----------------------------------------------------------------------
%% Function: parse_xml_directive_1(Xml, Acc) -> Name
%% Input:   Xml = binary()
%%          Acc = string()
%% Output:  Name = string()
%% Description: Parsing an attribute name from the stream.
%%----------------------------------------------------------------------
parse_name(<<C, Rest/binary>>, Acc) when 97 =< C, C =< 122 ->
    parse_name(Rest, [C |Acc]);
parse_name(Rest, Acc) ->
    {lists:reverse(Acc), Rest}.

%%----------------------------------------------------------------------
%% Function: parse_eq(Xml) -> Rest
%% Input:  Xml = binary()
%% Output:  Rest = binary()
%% Description: Reads an '=' from the stream.
%%----------------------------------------------------------------------
parse_eq(<<C, Rest/binary>>) when ?is_whitespace(C) ->
    parse_eq(Rest);
parse_eq(<<"=", Rest/binary>>) ->
    Rest.

%%----------------------------------------------------------------------
%% Function: parse_value(Xml) -> {Value, Rest}
%% Input:   Xml = binary()
%% Output:  Value = string()
%%          Rest = binary()
%% Description: Parsing an attribute value from the stream.
%%----------------------------------------------------------------------
parse_value(<<C, Rest/binary>>) when ?is_whitespace(C) ->
    parse_value(Rest);
parse_value(<<C, Rest/binary>>) when C == $'; C == $" ->
    parse_value_1(Rest, C, []).

%%----------------------------------------------------------------------
%% Function: parse_value_1(Xml, Stop, Acc) -> {Value, Rest}
%% Input:   Xml = binary()
%%          Stop = $' | $"
%%          Acc = list()
%% Output:  Value = string()
%%          Rest = binary()
%% Description: Parsing an attribute value from the stream.
%%----------------------------------------------------------------------
parse_value_1(<<Stop, Rest/binary>>, Stop, Acc) ->
    {lists:reverse(Acc), Rest};
parse_value_1(<<C, Rest/binary>>, Stop, Acc) ->
    parse_value_1(Rest, Stop, [C |Acc]).

%%======================================================================
%%Default functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: default_event_cb(Event, LineNo, State) -> Result
%% Input:   Event = tuple()
%%          LineNo = integer()
%%          State = term()
%% Output:  Result = {ok, State}
%% Description: Default event callback printing event.
%%----------------------------------------------------------------------
default_event_cb(_Event, _LineNo, State) ->
    State.

%%----------------------------------------------------------------------
%% Function: default_continuation_cb(IoDevice) -> Result
%%          IoDevice = iodevice()
%% Output:  Result = {[char()], State}
%% Description: Default continuation callback reading blocks.
%%----------------------------------------------------------------------
default_continuation_cb(IoDevice) ->
    case file:read(IoDevice, 1024) of
	eof ->
	    {<<>>, IoDevice};
	{ok, FileBin} ->
	    {FileBin, IoDevice}
    end.
