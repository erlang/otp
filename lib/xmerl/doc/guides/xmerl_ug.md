<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Xmerl

## Introduction

### Features

The _Xmerl_ XML parser is able to parse XML documents according to the XML 1.0
standard. As default it performs well-formed parsing, (syntax checks and checks
of well-formed constraints). Optionally one can also use Xmerl as a validating
parser, (validate according to referenced DTD and validating constraints). By
means of for example the xmerl_xs module it is possible to transform the parsed
result to other formats, e.g. text, HTML, XML etc.

### Overview

This document does not give an introduction to XML. There are a lot of books
available that describe XML from different views. At the
[www.W3.org](http://www.w3.org) site you will find the
[XML 1.0 specification](http://www.w3.org/TR/REC-xml/) and other related specs.
One site were you can find tutorials on XML and related specs is
[ZVON.org](http://www.zvon.org).

However, here you will find some examples of how to use and to what you can use
Xmerl. A detailed description of the user interface can be found in the
reference manual.

There are two known shortcomings in Xmerl:

- It cannot retrieve external entities on the Internet by a URL reference, only
  resources in the local file system.
- Xmerl can parse Unicode encoded data. But, it fails on tag names, attribute
  names and other mark-up names that are encoded Unicode characters not mapping
  on ASCII.

By parsing an XML document you will get a record, displaying the structure of
the document, as return value. The record also holds the data of the document.
Xmerl is convenient to use in for instance the following scenarios:

You need to retrieve data from XML documents. Your Erlang software can handle
information from the XML document by extracting data from the data structure
received by parsing.

It is also possible to do further processing of parsed XML with Xmerl. If you
want to change format of the XML document to for instance HTML, text or other
XML format you can transform it. There is support for such transformations in
Xmerl.

One may also convert arbitrary data to XML. So it for instance is easy to make
it readable by humans. In this case you first create Xmerl data structures out
of your data, then transform it to XML.

You can find examples of these three examples of usage below.

## Xmerl User Interface Data Structure

The following records used by Xmerl to save the parsed data are defined in
`xmerl.hrl`

The result of a successful parsing is a tuple `{DataStructure,M}`. `M` is the
XML production Misc, which is the mark-up that comes after the element of the
document. It is returned "as is". `DataStructure` is an `#xmlElement{}` record,
that among others have the fields `name`, `parents`, `attributes` and `content`
like:

```text
#xmlElement{name=Name,
            ...
            parents=Parents,
            ...
            attributes=Attrs,
            content=Content,
            ...}
```

The name of the element is found in the `name` field. In the `parents` field is
the names of the parent elements saved. Parents is a list of tuples where the
first element in each tuple is the name of the parent element. The list is in
reverse order.

The record `#xmlAttribute{}` holds the name and value of an attribute
in the fields `name` and `value`. All attributes of an element is a list of
`#xmlAttribute{}` in the field `attributes` of the `#xmlElement{}` record.

The `content` field of the top element is a list of records that shows the
structure and data of the document. If it is a simple document like:

```text
<?xml version="1.0"?>
<dog>
Grand Danois
</dog>
```

The parse result will be:

```erlang
#xmlElement{name = dog,
            ...
            parents = [],
            ...
            attributes = [],
            content = [{xmlText,[{dog,1}],1,[],"\
Grand Danois\
",text}],
            ...
            }
```

Where the content of the top element is:
`[{xmlText,[{dog,1}],1,[],"\ Grand Danois\ ",text}]`. Text will be returned in
`xmlText` records. Though, usually documents are more complex, and the content
of the top element will in that case be a nested structure with `#xmlElement{}`
records that in turn may have complex content. All of this reflects the
structure of the XML document.

Space characters between mark-up as `space`, `tab` and `line feed` are
normalized and returned as xmlText records.

### Errors

An unsuccessful parse results in an error, which may be a tuple `{error,Reason}`
or an exit: `{'EXIT',Reason}`. According to the XML 1.0 standard there are
`fatal error` and `error` situations. The fatal errors _must_ be detected by a
conforming parser while an error _may_ be detected. Both categories of errors
are reported as fatal errors by this version of Xmerl, most often as an exit.

## Getting Started

In the following examples we use the XML file "motorcycles.xml" and the
corresponding DTD "motorcycles.dtd". motorcycles.xml looks like:[](){:
#motorcyclesxml }

```text

<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE motorcycles SYSTEM "motorcycles.dtd">
<motorcycles>
  <bike year="2000" color="black">
    <name>
      <manufacturer>Suzuki</manufacturer>
      <brandName>Suzuki VL 1500</brandName>
      <additionalName>Intruder</additionalName>
    </name>
    <engine>V-engine, 2-cylinders, 1500 cc</engine>
    <kind>custom</kind>
    <drive>cardan</drive>
    <accessories>Sissy bar, luggage carrier,V&amp;H exhaust pipes</accessories>
  </bike>
  <date>2004.08.25</date>
  <bike year="1983" color="read pearl">
    <name>
      <manufacturer>Yamaha</manufacturer>
      <brandName>XJ 400</brandName>
    </name>
    <engine>4 cylinder, 400 cc</engine>
    <kind>alround</kind>
    <drive>chain</drive>
    <comment>Good shape!</comment>
  </bike>
</motorcycles>
```

and motorcycles.dtd looks like:

```text

<?xml version="1.0" encoding="utf-8" ?>
<!ELEMENT motorcycles (bike,date?)+ >
<!ELEMENT bike        (name,engine,kind,drive, accessories?,comment?) >
<!ELEMENT name        (manufacturer,brandName,additionalName?) >
<!ELEMENT manufacturer       (#PCDATA)>
<!ELEMENT brandName         (#PCDATA)>
<!ELEMENT additionalName    (#PCDATA)>
<!ELEMENT engine             (#PCDATA)>
<!ELEMENT kind               (#PCDATA)>
<!ELEMENT drive              (#PCDATA)>
<!ELEMENT comment            (#PCDATA)>
<!ELEMENT accessories        (#PCDATA)>

<!-- Date of the format yyyy.mm.dd -->
<!ELEMENT date              (#PCDATA)>
<!ATTLIST  bike year NMTOKEN #REQUIRED
                color NMTOKENS #REQUIRED
                condition (useless | bad | serviceable | moderate | good |
                           excellent | new | outstanding) "excellent" >
```

If you want to parse the XML file motorcycles.xml you run it in the Erlang shell
like:

```erlang
3> {ParseResult,Misc}=xmerl_scan:file("motorcycles.xml").
{{xmlElement,motorcycles,
             motorcycles,
             [],
             {xmlNamespace,[],[]},
             [],
             1,
             [],
             [{xmlText,[{motorcycles,1}],1,[],"\
  ",text},
              {xmlElement,bike,
                          bike,
                          [],
                          {xmlNamespace,[],[]},
                          [{motorcycles,1}],
                          2,
                          [{xmlAttribute,year,[],[],[],[]|...},
                           {xmlAttribute,color,[],[],[]|...}],
                          [{xmlText,[{bike,2},{motorcycles|...}],
                                    1,
                                    []|...},
                           {xmlElement,name,name,[]|...},
                           {xmlText,[{...}|...],3|...},
                           {xmlElement,engine|...},
                           {xmlText|...},
                           {...}|...],
                          [],
                          ".",
                          undeclared},
              ...
              ],
             [],
             ".",
             undeclared},
 []}
4>
```

If you instead receives the XML doc as a string you can parse it by
`xmerl_scan:string/1`. Both file/2 and string/2 exists where the second argument
is a list of options to the parser, see the [reference manual](`m:xmerl_scan`).

## Example: Extracting Data From XML Content

In this example consider the situation where you want to examine a particular
data in the XML file. For instance, you want to check for how long each
motorcycle have been recorded.

Take a look at the DTD and observe that the structure of an XML document that is
conformant to this DTD must have one motorcycles element (the root element). The
motorcycles element must have at least one bike element. After each bike element
it may be a date element. The content of the date element is #PCDATA (Parsed
Character DATA), i.e. raw text. Observe that if #PCDATA must have a `"<"` or a
`"&"` character it must be written as `"&lt;"` and `"&amp;"` respectively. Also
other character entities exists similar to the ones in HTML and SGML.

If you successfully parse the XML file with the validation on as in:
`xmerl_scan:file('motorcycles.xml',[{validation,true}])` you know that the XML
document is valid and has the structure according to the DTD.

Thus, knowing the allowed structure it is easy to write a program that traverses
the data structure and picks the information in the `#xmlElements{}` records
with name date.

Observe that white space: each space, tab or line feed, between mark-up results
in an `#xmlText{}` record.

## Example: Create XML Out Of Arbitrary Data

For this task there are more than one way to go. The "brute force" method is to
create the records you need and feed your data in the content and attribute
fields of the appropriate element.

There is support for this in Xmerl by the "simple-form" format. You can put your
data in a simple-form data structure and feed it into
`xmerl:export_simple(Content,Callback,RootAttributes)`. Content may be a mixture
of simple-form and Xmerl records as `#xmlElement{}` and `#xmlText{}`.

The Types are:

- Content = \[Element]
- Callback = atom()
- RootAttributes = \[Attributes]

Element is any of:

- \{Tag, Attributes, Content\}
- \{Tag, Content\}
- Tag
- IOString
- \#xmlText\{\}
- \#xmlElement\{\}
- \#xmlPI\{\}
- \#xmlComment\{\}
- \#xmlDecl\{\}

The simple-form structure is any of `{Tag, Attributes, Content}`,
`{Tag, Content}` or `Tag` where:

- Tag = atom()
- Attributes = \[\{Name, Value\}| #xmlAttribute\{\}]
- Name = atom()
- Value = IOString | atom() | integer()

See also reference manual for [xmerl](`xmerl:export_simple/3`)

If you want to add the information about a black Harley Davidsson 1200 cc
Sportster motorcycle from 2003 that is in shape as new in the motorcycles.xml
document you can put the data in a simple-form data structure like:

```erlang
Data =
  {bike,
     [{year,"2003"},{color,"black"},{condition,"new"}],
     [{name,
         [{manufacturer,["Harley Davidsson"]},
          {brandName,["XL1200C"]},
          {additionalName,["Sportster"]}]},
      {engine,
         ["V-engine, 2-cylinders, 1200 cc"]},
      {kind,["custom"]},
      {drive,["belt"]}]}
```

In order to append this data to the end of the motorcycles.xml document you have
to parse the file and add Data to the end of the root element content.

```erlang
    {RootEl,Misc}=xmerl_scan:file('motorcycles.xml'),
    #xmlElement{content=Content} = RootEl,
    NewContent=Content++lists:flatten([Data]),
    NewRootEl=RootEl#xmlElement{content=NewContent},
```

Then you can run it through the export_simple/2 function:

```erlang
    {ok,IOF}=file:open('new_motorcycles.xml',[write]),
    Export=xmerl:export_simple([NewRootEl],xmerl_xml),
    io:format(IOF,"~s~n",[lists:flatten(Export)]),
```

[](){: #new_motorcyclesxml }

The result would be:

```text

<?xml version="1.0"?><motorcycles>
  <bike year="2000" color="black">
    <name>
      <manufacturer>Suzuki</manufacturer>
      <brandName>Suzuki VL 1500</brandName>
      <additionalName>Intruder</additionalName>
    </name>
    <engine>V-engine, 2-cylinders, 1500 cc</engine>
    <kind>custom</kind>
    <drive>cardan</drive>
    <accessories>Sissy bar, luggage carrier,V&amp;H exhaust pipes</accessories>
  </bike>
  <date>2004.08.25</date>
  <bike year="1983" color="read pearl">
    <name>
      <manufacturer>Yamaha</manufacturer>
      <brandName>XJ 400</brandName>
    </name>
    <engine>4 cylinder, 400 cc</engine>
    <kind>alround</kind>
    <drive>chain</drive>
    <comment>Good shape!</comment>
  </bike>
<bike year="2003" color="black" condition="new"><name><manufacturer>Harley Davidsson</manufacturer><brandName>XL1200C</brandName><additionalName>Sportster</additionalName></name><engine>V-engine, 2-cylinders, 1200 cc</engine><kind>custom</kind><drive>belt</drive></bike></motorcycles>
```

If it is important to get similar indentation and newlines as in the original
document you have to add `#xmlText{}` records with space and newline values in
appropriate places. It may also be necessary to keep the original prolog where
the DTD is referenced. If so, it is possible to pass a RootAttribute
`{prolog,Value}` to `export_simple/3`. The following example code fixes those
changes in the previous example:

```erlang
    Data =
      [#xmlText{value="  "},
       {bike,[{year,"2003"},{color,"black"},{condition,"new"}],
             [#xmlText{value="\
    "},
              {name,[#xmlText{value="\
      "},
                     {manufacturer,["Harley Davidsson"]},
                     #xmlText{value="\
      "},
                     {brandName,["XL1200C"]},
                     #xmlText{value="\
      "},
                     {additionalName,["Sportster"]},
                     #xmlText{value="\
    "}]},
              {engine,["V-engine, 2-cylinders, 1200 cc"]},
              #xmlText{value="\
    "},
              {kind,["custom"]},
              #xmlText{value="\
    "},
              {drive,["belt"]},
              #xmlText{value="\
  "}]},
       #xmlText{value="\
"}],
    ...
    NewContent=Content++lists:flatten([Data]),
    NewRootEl=RootEl#xmlElement{content=NewContent},
    ...
    Prolog = ["<?xml version=\\"1.0\\" encoding=\\"utf-8\\" ?>
<!DOCTYPE motorcycles SYSTEM \\"motorcycles.dtd\\">\
"],
    Export=xmerl:export_simple([NewRootEl],xmerl_xml,[{prolog,Prolog}]),
    ...
```

The result will be:

```text

<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE motorcycles SYSTEM "motorcycles.dtd">
<motorcycles>
  <bike year="2000" color="black">
    <name>
      <manufacturer>Suzuki</manufacturer>
      <brandName>Suzuki VL 1500</brandName>
      <additionalName>Intruder</additionalName>
    </name>
    <engine>V-engine, 2-cylinders, 1500 cc</engine>
    <kind>custom</kind>
    <drive>cardan</drive>
    <accessories>Sissy bar, luggage carrier,V&amp;H exhaust pipes</accessories>
  </bike>
  <date>2004.08.25</date>
  <bike year="1983" color="read pearl">
    <name>
      <manufacturer>Yamaha</manufacturer>
      <brandName>XJ 400</brandName>
    </name>
    <engine>4 cylinder, 400 cc</engine>
    <kind>alround</kind>
    <drive>chain</drive>
    <comment>Good shape!</comment>
  </bike>
  <bike year="2003" color="black" condition="new">
    <name>
      <manufacturer>Harley Davidsson</manufacturer>
      <brandName>XL1200C</brandName>
      <additionalName>Sportster</additionalName>
    </name><engine>V-engine, 2-cylinders, 1200 cc</engine>
    <kind>custom</kind>
    <drive>belt</drive>
  </bike>
</motorcycles>
```

The generated XML above was formatted for readability. Another exporter which
indents the code with 2 spaces can also be used. In order to use it one only
needs to change the export-module:

```text
      ...
        Export=xmerl:export_simple([NewRootEl],xmerl_xml_indent,[{prolog,Prolog}]),
      ...
```

## Example: Transforming XML To HTML

Assume that you want to transform the
[motorcycles.xml](xmerl_ug.md#motorcyclesxml) document to HTML. If you want the
same structure and tags of the resulting HTML document as of the XML document
then you can use the `xmerl:export/2` function. The following:

```erlang
2> {Doc,Misc}=xmerl_scan:file('motorcycles.xml').
{{xmlElement,motorcycles,
             motorcycles,
             [],
             {xmlNamespace,[],[]},
             [],
             1,
             [],
             [{xmlText,[{motorcycles,1}],1,[],"\
  ",text},
              {xmlElement,bike,
...
3> DocHtml=xmerl:export([Doc],xmerl_html).
["<!DOCTYPE HTML PUBLIC \\"",
 "-//W3C//DTD HTML 4.01 Transitional//EN",
 "\\"",
 [],
 ">\
",
 [[["<","motorcycles",">"],
   ["\
  ",
    [["<",
      "bike",
      [[" ","year","=\\"","2000","\\""],[" ","color","=\\"","black","\\""]],
      ">"],
...
```

Will give the result [result_export.html](assets/result_export.html)

Perhaps you want to do something more arranged for human reading. Suppose that
you want to list all different brands in the beginning with links to each group
of motorcycles. You also want all motorcycles sorted by brand, then some flashy
colors on top of it. Thus you rearrange the order of the elements and put in
arbitrary HTML tags. This is possible to do by means of the
[XSL Transformation (XSLT)](http://www.w3.org/Style/XSL/) like functionality in
Xmerl.

Even though the following example shows one way to transform data from XML to
HTML it also applies to transformations to other formats.

`xmerl_xs` does not implement the entire XSLT specification but the basic
functionality. For all details see the [reference manual](`m:xmerl_xs`)

First, some words about the xmerl_xs functionality:

You need to write template functions to be able to control what kind of output
you want. Thus if you want to encapsulate a `bike` element in <p> tags you
simply write a function:

```erlang
template(E = #xmlElement{name='bike'}) ->
    ["<p>",xslapply(fun template/1,E),"</p>"];
```

With `xslapply` you tell the XSLT processor in which order it should traverse
the XML structure. By default it goes in preorder traversal, but with the
following we make a deliberate choice to break that order:

```erlang
template(E = #xmlElement{name='bike'}) ->
    ["<p>",xslapply(fun template/1,select("bike/name/manufacturer")),"</p>"];
```

If you want to output the content of an XML element or an attribute you will get
the value as a string by the `value_of` function:

```erlang
template(E = #xmlElement{name='motorcycles'}) ->
    ["<p>",value_of(select("bike/name/manufacturer",E),"</p>"];
```

In the xmerl_xs functions you can provide a select(String) call, which is an
[XPath](http://www.w3.org/TR/xpath) functionality. For more details see the
xmerl_xs [tutorial](`e:xmerl:xmerl_xs_examples.html`).

Now, back to the example where we wanted to make the output more arranged. With
the template:

```c
template(E = #xmlElement{name='motorcycles'}) ->
    [    "<head>\
<title>motorcycles</title>\
</head>\
",
         "<body>\
",
\011 "<h1>Used Motorcycles</h1>\
",
\011 "<ul>\
",
\011 remove_duplicates(value_of(select("bike/name/manufacturer",E))),
\011 "\
</ul>\
",
\011 sort_by_manufacturer(xslapply(fun template/1, E)),
         "</body>\
",
\011 "</html>\
"];
```

We match on the top element and embed the inner parts in an HTML body. Then we
extract the string values of all motorcycle brands, sort them and removes
duplicates by
`remove_duplicates(value_of(select("bike/name/manufacturer", E)))`. We also
process the substructure of the top element and pass it to a function that sorts
all motorcycle information by brand according to the task formulation in the
beginning of this example.

The next template matches on the `bike` element:

```erlang
template(E = #xmlElement{name='bike'}) ->
    {value_of(select("name/manufacturer",E)),["<dt>",xslapply(fun template/1,select("name",E)),"</dt>",
    "<dd><ul>\
",
    "<li style="color:green">Manufacturing year: ",xslapply(fun template/1,select("@year",E)),"</li>\
",
    "<li style="color:red">Color: ",xslapply(fun template/1,select("@color",E)),"</li>\
",
    "<li style="color:blue">Shape : ",xslapply(fun template/1,select("@condition",E)),"</li>\
",
    "</ul></dd>\
"]};
```

This creates a tuple with the brand of the motorcycle and the output format. We
use the brand name only for sorting purpose. We have to end the template
function with the "built in clause"
`template(E) -> built_in_rules(fun template/1, E).`

The entire program is motorcycles2html.erl:

```erlang

%%%-------------------------------------------------------------------
%%% File    : motorcycles2html.erl
%%% Author  : Bertil Karlsson <bertil@localhost.localdomain>
%%% Description :
%%%
%%% Created :  2 Sep 2004 by Bertil Karlsson <bertil@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(motorcycles2html).

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs,
	[ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).

-export([process_xml/1,process_to_file/2,process_to_file/1]).

process_xml(Doc) ->
    template(Doc).

process_to_file(FileName) ->
    process_to_file(FileName,'motorcycles.xml').

process_to_file(FileName,XMLDoc) ->
    case file:open(FileName,[write]) of
	{ok,IOF} ->
	    {XMLContent,_} = xmerl_scan:file(XMLDoc),
	    TransformedXML=process_xml(XMLContent),
	    io:format(IOF,"~s",[TransformedXML]),
	    file:close(IOF);
	{error,Reason} ->
	    io:format("could not open file due to ~p.~n",[Reason])
    end.

%%% templates
template(E = #xmlElement{name='motorcycles'}) ->
    [    "<head>\n<title>motorcycles</title>\n</head>\n",
         "<body>\n",
	 "<h1>Used Motorcycles</h1>\n",
	 "<ul>\n",
	 remove_duplicates(value_of(select("bike/name/manufacturer",E))),
	 "\n</ul>\n",
	 sort_by_manufacturer(xslapply(fun template/1, E)),
         "</body>\n",
	 "</html>\n"];
template(E = #xmlElement{name='bike'}) ->
    {value_of(select("name/manufacturer",E)),["<dt>",xslapply(fun template/1,select("name",E)),"</dt>",
    "<dd><ul>\n",
    "<li style=\"color:green\">Manufacturing year: ",xslapply(fun template/1,select("@year",E)),"</li>\n",
    "<li style=\"color:red\">Color: ",xslapply(fun template/1,select("@color",E)),"</li>\n",
    "<li style=\"color:blue\">Shape : ",xslapply(fun template/1,select("@condition",E)),"</li>\n",
    "</ul></dd>\n"]};
template(E) -> built_in_rules(fun template/1, E).


%%%%%%%%%%% helper routines

%% sorts on the bike name element, unwraps the bike information and
%% inserts a line feed and indentation on each bike element.
sort_by_manufacturer(L) ->
    Tuples=[X1||X1={_,_} <- L],
    SortedTS = lists:keysort(1,Tuples),
    InsertRefName_UnWrap=
	fun([{[Name],V}|Rest],Name,F)->
		[V|F(Rest,Name,F)];
	   ([{[Name],V}|Rest],_PreviousName,F) ->
		[["<a name=\"",Name,"\"></>"],V|F(Rest,Name,F)];
	   ([],_,_) -> []
	end,
    SortedRefed=InsertRefName_UnWrap(SortedTS,no_name,InsertRefName_UnWrap),
%    SortedTs=[Y||{X,Y}<-lists:keysort(1,Tuples)],
    WS = "\n    ",
    Fun=fun([H|T],Acc,F)->
		F(T,[H,WS|Acc],F);
	   ([],Acc,_F)->
		lists:reverse([WS|Acc])
	end,
    if length(SortedRefed) > 0 ->
	    Fun(SortedRefed,[],Fun);
       true -> []
    end.


%% removes all but the first of an element in L and inserts a html
%% reference for each list element.
remove_duplicates(L) ->
    remove_duplicates(L,[]).

remove_duplicates([],Acc) ->
    make_ref(lists:sort(lists:reverse(Acc)));
remove_duplicates([A|L],Acc) ->
    case lists:delete(A,L) of
	L ->
	    remove_duplicates(L,[A|Acc]);
	L1 ->
	    remove_duplicates([A|L1],[Acc])
    end.

make_ref([]) -> [];
make_ref([H]) when is_atom(H) ->
    "<ul><a href=\"#"++atom_to_list(H)++"\">"++atom_to_list(H)++"</a></ul>";
make_ref([H]) when is_list(H) ->
    "<ul><a href=\"#"++H++"\">\s"++H++"</a></ul>";
make_ref([H|T]) when is_atom(H) ->
    ["<ul><a href=\"#"++atom_to_list(H)++"\">\s"++atom_to_list(H)++",\n</a></ul>"
     |make_ref(T)];
make_ref([H|T]) when is_list(H) ->
    ["<ul><a href=\"#"++H++"\">\s"++H++",\n</a></ul>"|make_ref(T)].
```

If we run it like this:
`motorcycles2html:process_to_file('result_xs.html', 'motorcycles2.xml').` The
result will be [result_xs.html](assets/result_xs.html). When the input file is of the
same structure as the previous "motorcycles" XML files but it has a little more
'bike' elements and the 'manufacturer' elements are not in order.
