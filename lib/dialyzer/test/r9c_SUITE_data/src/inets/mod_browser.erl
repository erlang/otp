%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mod_browser.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
%%
%% ----------------------------------------------------------------------
%%
%% Browsers sends a string to the webbserver
%% to identify themsevles. They are a bit nasty
%% since the only thing that the specification really
%% is strict about is that they shall be short
%% tree axamples:
%%
%% Netscape Mozilla/4.75 [en] (X11; U; SunOS 5.8 sun4u)
%% IE5      Mozilla/4.0 (compatible; MSIE 5.0; SP1B; SunOS 5.8 sun4u; X11)
%% Lynx     Lynx/2.8.3rel.1 libwww-FM/2.142
%%
%% ----------------------------------------------------------------------

-module(mod_browser).

%% Remember that the order of the mozilla browsers are
%% important since some browsers include others to behave
%% as they were something else
-define(MOZILLA_BROWSERS,[{opera,"opera"},{msie,"msie"}]).


%% If your operatingsystem is not recognized add it to this list.
-define(OPERATIVE_SYSTEMS,[{win3x,["win16","windows 3","windows 16-bit"]},
			   {win95,["win95","windows 95"]},
			   {win98,["win98", "windows 98"]},
			   {winnt,["winnt", "windows nt"]},
			   {win2k,["nt 5"]},
			   {sunos4,["sunos 4"]},
			   {sunos5,["sunos 5"]},
			   {sun,["sunos"]},
			   {aix,["aix"]},
			   {linux,["linux"]},
			   {sco,["sco","unix_sv"]},
			   {freebsd,["freebsd"]},
			   {bsd,["bsd"]}]).

-define(LYNX,lynx).
-define(MOZILLA,mozilla).
-define(EMACS,emacs).
-define(STAROFFICE,soffice).
-define(MOSAIC,mosaic).
-define(NETSCAPE,netscape).
-define(UNKOWN,unknown).

-include("httpd.hrl").

-export([do/1, test/0, getBrowser/1]).


do(Info) ->
    case httpd_util:key1search(Info#mod.data,status) of
	{Status_code,PhraseArgs,Reason} ->
	    {proceed,Info#mod.data};
	undefined ->
	    {proceed,[{'user-agent',getBrowser1(Info)}|Info#mod.data]}
    end.

getBrowser1(Info) ->
    PHead=Info#mod.parsed_header,
    case httpd_util:key1search(PHead,"User-Agent") of
	undefined->
	   undefined;
	AgentString ->
	    getBrowser(AgentString)
    end.

getBrowser(AgentString) ->
    LAgentString = httpd_util:to_lower(AgentString),
    case regexp:first_match(LAgentString,"^[^ ]*") of
	{match,Start,Length} ->
	    Browser=lists:sublist(LAgentString,Start,Length),
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
browserType([$e,$t,$a,$r,$o,$f,$f,$i,$c,$e|Version]) ->
    {?STAROFFICE,browserVersion(Version)};
browserType([$m,$o,$s,$a,$i,$c|Version]) ->
    {?MOSAIC,browserVersion(Version)};
browserType(Unknown)->
    unknown.


browserVersion([$/|VsnString]) ->
    case catch list_to_float(VsnString) of
	Number when float(Number) ->
	    Number;
	Whatever ->
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

operativeSystem(OpString,[]) ->
    unknown;
operativeSystem(OpString,[{RetVal,RegExps}|Rest]) ->
    case controlOperativeSystem(OpString,RegExps) of
	true->
	    RetVal;
	_ ->
	    operativeSystem(OpString,Rest)
    end.

controlOperativeSystem(OpString,[]) ->
    false;
controlOperativeSystem(OpString,[Regexp|Regexps]) ->
    case regexp:match(OpString,Regexp) of
	{match,_,_}->
	    true;
	nomatch->
	    controlOperativeSystem(OpString,Regexps)
    end.


%% OK this is ugly but thats the only way since
%% all browsers dont conform to the name/vsn standard
%% First we check if it is one of the browsers that
%% not are the default mozillaborwser against the regexp
%% for the different browsers. if no match it a mozilla
%% browser i.e opera netscape or internet explorer

getMozilla(AgentString,[],Default) ->
    Default;
getMozilla(AgentString,[{Agent,AgentRegExp}|Rest],Default) ->
    case regexp:match(AgentString,AgentRegExp) of
	{match,_,_} ->
	    {Agent,getVersion(AgentString,AgentRegExp)};
	nomatch ->
	    getMozilla(AgentString,Rest,Default)
    end.

getVersion(AgentString,AgentRegExp) ->
    case regexp:match(AgentString,AgentRegExp++"[0-9\.\ ]*") of
	{match,Start,Length} when length(AgentRegExp) < Length ->
	    %% Ok we got the number split it out
	    RealStart=Start+length(AgentRegExp),
	    RealLength=Length-length(AgentRegExp),
	    VsnString=string:substr(AgentString,RealStart,RealLength),
	    case string:strip(VsnString,both,$\ ) of
		[] ->
		    unknown;
		Vsn ->
		    case string:tokens(Vsn,".") of
			[Number]->
			    list_to_float(Number++".0");
			[Major,Minor|_MinorMinor]->
			    list_to_float(Major++"."++Minor)
		    end
	    end;
	nomatch ->
	    unknown
    end.


test()->
    io:format("~n--------------------------------------------------------~n"),
    Res1=getBrowser("Mozilla/4.75 [en] (X11; U; SunOS 5.8 sun4u)"),
    io:format("~p",[Res1]),
    io:format("~n--------------------------------------------------------~n"),
    io:format("~n--------------------------------------------------------~n"),
    Res2=getBrowser("Mozilla/4.0 (compatible; MSIE 5.0; SP1B; SunOS 5.8 sun4u; X11)"),
    io:format("~p",[Res2]),
    io:format("~n--------------------------------------------------------~n"),
    io:format("~n--------------------------------------------------------~n"),
    Res3=getBrowser("Lynx/2.8.3rel.1 libwww-FM/2.142"),
    io:format("~p",[Res3]),
    io:format("~n--------------------------------------------------------~n").
