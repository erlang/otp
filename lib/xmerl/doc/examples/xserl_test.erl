-module(xserl_test).
-include("xmerl.hrl").
-import(xserl,[ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).
-export([process_xml/1,test/0]).

doctype()->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\
 \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd \">".

test() ->
    Str=    "<?xml version=\"1.0\"?>"
	"<doc xmlns='urn:loc.gov:books' xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	"<title>Cheaper by the Dozen</title>"
	"<isbn:number>1568491379</isbn:number>"
	"<note>"
	"<!-- make HTML the default namespace for some commentary -->"
	"<p xmlns='urn:w3-org-ns:HTML'>"
	"This is a <i>funny</i> book!"
	"</p>"
	"</note>"
	"</doc>",
    {Doc,_}=xmerl_scan:string(Str,[{fetch_fun, fun(DTDSpec,S) -> {ok,S} end}]),

    process_xml(Doc).
    
process_xml(Doc)->
	template(Doc).

template(E = #xmlElement{name='doc'})->
    [ "<\?xml version=\"1.0\" encoding=\"iso-8859-1\"\?>",
      doctype(),
      "<html xmlns=\"http://www.w3.org/1999/xhtml\" >"
      "<head>"
      "<title>", xserl:value_of(select("title",E)), "</title>"
      "</head>"
      "<body>",
      xslapply( fun template/1, E),
      "</body>"
      "</html>" ];


template(E = #xmlElement{ parents=[{'doc',_}|_], name='title'}) ->
    ["<h1>",
%%     xslapply( fun template/1, E),
%%   same as
     lists:map( fun template/1, E#xmlElement.content ),
     "</h1>"];

template(E = #xmlElement{ parents=[{'chapter',_}|_], name='title'}) ->
    ["<h2>",
     xslapply( fun template/1, E),
     "</h2>"];

template(E = #xmlElement{ parents=[{'section',_}|_], name='title'}) ->
    ["<h3>",
     xslapply( fun template/1, E),
     "</h3>"];

template(E = #xmlElement{ name='para'}) ->
    ["<p>",
     xslapply( fun template/1, E),
     "</p>"];

template(E = #xmlElement{ name='note'}) ->
    ["<p class=\"note\">"
     "<b>NOTE: </b>",
     xslapply( fun template/1, E),
     "</p>"];

template(E = #xmlElement{ name='emph'}) ->
    ["<em>",
     xslapply( fun template/1, E),
     "</em>"];

template(E)->
    built_in_rules( fun template/1, E).

%% It is important to end with a call to xserl:built_in_rules/2
%% if you want any text to be written in "push" transforms.
%% That are the ones using a lot xslapply( fun template/1, E )
%% instead of value_of(select("xpath",E)), which is pull...
%% Could maybe be caught as an exception in xslapply instead,
%% but I think that could degrade performance - having an
%% exception for every #xmlText element.

