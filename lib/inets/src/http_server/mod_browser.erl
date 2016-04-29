%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% ----------------------------------------------------------------------
%%
%% Browsers sends a string to the webbserver
%% to identify themsevles. They are a bit nasty
%% since the only thing that the specification really 
%% is strict about is that they shall be short
%% some axamples:
%%
%% Netscape Mozilla/4.75 [en] (X11; U; SunOS 5.8 sun4u)
%%          Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.0.1) Gecko/20020823 Netscape/7.0
%% Mozilla  Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.1) Gecko/20020827
%% Safari   Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en-us) AppleWebKit/85 (KHTML, like Gecko) Safari/85
%% IE5      Mozilla/4.0 (compatible; MSIE 5.0; SP1B; SunOS 5.8 sun4u; X11)
%% Lynx     Lynx/2.8.3rel.1 libwww-FM/2.142
%%
%% ----------------------------------------------------------------------

-module(mod_browser).

-export([do/1, test/0, getBrowser/1]).

%% Remember that the order of the mozilla browsers are 
%% important since some browsers include others to behave 
%% as they were something else  
-define(MOZILLA_BROWSERS,[{netscape, "netscape"},
			  {opera,    "opera"}, 
			  {msie,     "msie"}, 
			  {safari,   "safari"},
			  {mozilla,  "rv:"}]). % fallback, must be last


%% If your operatingsystem is not recognized add it to this list.
-define(OPERATIVE_SYSTEMS,[{win3x,  ["win16", "windows 3", "windows 16-bit"]},
			   {win95,  ["win95", "windows 95"]},
			   {win98,  ["win98", "windows 98"]},
			   {winnt,  ["winnt", "windows nt"]},
			   {win2k,  ["nt 5"]},
			   {sunos4, ["sunos 4"]},
			   {sunos5, ["sunos 5"]},
			   {sun,    ["sunos"]},
			   {aix,    ["aix"]},
			   {linux,  ["linux"]},
			   {sco,    ["sco", "unix_sv"]},
			   {freebsd,["freebsd"]},
			   {bsd,    ["bsd"]},
			   {macosx, ["mac os x"]}]).

-define(LYNX,       lynx).
-define(MOZILLA,    mozilla).
-define(EMACS,      emacs).
-define(STAROFFICE, soffice).
-define(MOSAIC,     mosaic).
-define(NETSCAPE,   netscape).
-define(SAFARU,     safari).
-define(UNKOWN,     unknown).

-include("httpd.hrl").

-define(VMODULE,"BROWSER").

do(Info) ->
    case proplists:get_value(status, Info#mod.data) of
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed,Info#mod.data};
	undefined ->
	    Browser = getBrowser1(Info),
	    {proceed,[{'user-agent', Browser}|Info#mod.data]}
    end.

getBrowser1(Info) ->
    PHead = Info#mod.parsed_header,
    case proplists:get_value("user-agent", PHead) of
	undefined ->
	    undefined;
	AgentString ->
	    getBrowser(AgentString)
    end.

getBrowser(AgentString) ->
    LAgentString = http_util:to_lower(AgentString),
    case re:run(LAgentString,"^[^ ]*", [{capture, first}]) of
	{match,[{Start,Length}]} ->
	    Browser = lists:sublist(LAgentString,Start+1,Length),
	    case browserType(Browser) of
		{mozilla,Vsn} ->
		    {getMozilla(LAgentString,
				?MOZILLA_BROWSERS,{?NETSCAPE,Vsn}),
		     operativeSystem(LAgentString)};
		AnyBrowser ->
		      {AnyBrowser,operativeSystem(LAgentString)}
	    end;
	nomatch ->
	    browserType(LAgentString)
    end.

browserType([$l,$y,$n,$x|Version]) ->
    {?LYNX,browserVersion(Version)};
browserType([$m,$o,$z,$i,$l,$l,$a|Version]) ->
    {?MOZILLA,browserVersion(Version)};
browserType([$e,$m,$a,$c,$s|Version]) ->
    {?EMACS,browserVersion(Version)};
browserType([$s,$t,$a,$r,$o,$f,$f,$i,$c,$e|Version]) ->
    {?STAROFFICE,browserVersion(Version)};
browserType([$m,$o,$s,$a,$i,$c|Version]) ->
    {?MOSAIC,browserVersion(Version)};
browserType(_Unknown) ->
    unknown.
 

browserVersion([$/|VsnString]) ->
    case catch list_to_float(VsnString) of
	Number when is_float(Number) ->
	    Number;
	_Whatever ->
	    case string:span(VsnString,"1234567890.") of
		0 ->
		    unknown;
		VLength ->
		    Vsn = string:substr(VsnString,1,VLength),
		    case string:tokens(Vsn,".") of
			[Number] ->
			   list_to_float(Number++".0");
			[Major,Minor|_MinorMinor] ->
			    list_to_float(Major++"."++Minor)
		    end
	    end
    end;
browserVersion(VsnString) ->
    browserVersion([$/|VsnString]).

operativeSystem(OpString) ->
  operativeSystem(OpString, ?OPERATIVE_SYSTEMS).

operativeSystem(_OpString,[]) ->
    unknown;
operativeSystem(OpString,[{RetVal,RegExps}|Rest]) ->
    case controlOperativeSystem(OpString,RegExps) of
	true ->
	    RetVal;
	_ ->
	    operativeSystem(OpString,Rest)
    end.

controlOperativeSystem(_OpString,[]) ->
    false;
controlOperativeSystem(OpString,[Regexp|Regexps]) ->
    case re:run(OpString,Regexp, [{capture, none}]) of
	match ->
	    true;
	nomatch ->
	    controlOperativeSystem(OpString,Regexps)
    end.


%% OK this is ugly but thats the only way since 
%% all browsers dont conform to the name/vsn standard
%% First we check if it is one of the browsers that 
%% are not the default mozillaborwser against the regexp 
%% for the different browsers. if no match, it is a mozilla 
%% browser i.e opera, netscape, ie or safari

getMozilla(_AgentString,[],Default) ->
    Default;
getMozilla(AgentString,[{Agent,AgentRegExp}|Rest],Default) ->
    case re:run(AgentString,AgentRegExp, [{capture, none}]) of
	match ->
	    {Agent,getMozVersion(AgentString,AgentRegExp)};
	nomatch ->
	    getMozilla(AgentString,Rest,Default)
    end.

getMozVersion(AgentString, AgentRegExp) ->
    case re:run(AgentString,AgentRegExp++"[0-9\.\ \/]*", 
		[{capture, first}]) of
	{match, [{Start,Length}]} when length(AgentRegExp) < Length ->
	    %% Ok we got the number split it out
	    RealStart  = Start+1+length(AgentRegExp),
	    RealLength = Length-length(AgentRegExp),
	    VsnString  = string:substr(AgentString,RealStart,RealLength),
	    %% case string:strip(VsnString,both,$\ ) of
	    case strip(VsnString) of
		[] ->
		    unknown;
		[Y1,Y2,Y3,Y4,M1,M2,D1,D2] = DateVsn when
		      Y1 =< $9, Y1 >= $0,
		      Y2 =< $9, Y2 >= $0,
		      Y3 =< $9, Y3 >= $0,
		      Y4 =< $9, Y4 >= $0,
		      M1 =< $9, M1 >= $0,
		      M2 =< $9, M2 >= $0,
		      D1 =< $9, D1 >= $0,
		      D2 =< $9, D2 >= $0 ->
		    list_to_integer(DateVsn);
		Vsn ->
		    case string:tokens(Vsn,".") of
			[Number]->
			    list_to_float(Number++".0");
			[Major,Minor|Rev] ->
			    V = lists:flatten([Major,".",Minor,Rev]),
			    list_to_float(V)
		    end
	    end;
	nomatch ->
	    unknown
    end.

strip(VsnString) ->
    strip2(strip1(VsnString)).

strip1(VsnString) ->    
    string:strip(VsnString,both,$\ ).

strip2(VsnString) ->    
    string:strip(VsnString,both,$/ ).

test()->
    test("Mozilla/4.75 [en] (X11; U; SunOS 5.8 sun4u)"),
    test("Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.0.1) Gecko/20020823 Netscape/7.0"),
    test("Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.1) Gecko/20020827"),
    test("Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.4) Gecko/20020827"),
    test("Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en-us) AppleWebKit/85 (KHTML, like Gecko) Safari/85"),
    test("Mozilla/4.0 (compatible; MSIE 5.0; SP1B; SunOS 5.8 sun4u; X11)"),
    test("Lynx/2.8.3rel.1 libwww-FM/2.142"),
    ok.

test(Str) ->
    Browser = getBrowser(Str),
    io:format("~n--------------------------------------------------------~n"),
    io:format("~p",[Browser]),
    io:format("~n--------------------------------------------------------~n").

