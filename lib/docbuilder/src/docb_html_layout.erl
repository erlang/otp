%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_html_layout). 

-export([report_top/2, report_bot/1,
	 first_top/2, first_bot/1,
	 ref_top/2, ref_bot/1,
	 chapter_top/2, chapter_bot/1,
	 application_toc_top/3, application_toc_top/4,
	 part_toc_top/3, part_toc_top/4, part_toc_bot/0,
	 index_top/1, index_bot/0]).

%% Report

report_top(Data, Opts) ->
    [Title, Prepared, _Responsible, DocNo, _Approved, _Checked, _Date,
     Vsn0, _File] = Data,
    html_header(Title, Opts) ++
    docb_util:html_snippet(top, Opts) ++
"<center>
<h1>" ++ Title ++ "</h1>
<big>
  " ++ DocNo ++ version(Opts, Vsn0) ++ "<br />
  " ++ Prepared ++ "<br />
</big>
</center>
".

report_bot(Opts) ->
    docb_util:html_snippet(bottom, Opts) ++
"</body>
</html>
".

%% First

first_top(Data, Opts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked, _Date,
     Vsn0, _File] = Data,
    html_header(Title, Opts) ++
    docb_util:html_snippet(top, Opts) ++
"<center>
<h1>" ++ Title ++ "</h1>
<big>" ++ DocNo ++ version(Opts, Vsn0) ++ "<br />
</big>
</center>
".

first_bot(Opts) ->
    report_bot(Opts).

%% Reference

ref_top(Data, Opts) ->
    [Title, _Prepared, _Responsible, _DocNo, _Approved, _Checked,
     _Date, _Rev, _File] = Data,
    ref_html_header(Title, Opts) ++
"<!-- refpage -->\n" ++
    docb_util:html_snippet(top, Opts) ++
"<center>
<h1>" ++ Title ++ "</h1>
</center>".

ref_bot(Opts) ->
    docb_util:html_snippet(bottom, Opts) ++
"</body>
</html>
".

%% Chapter

chapter_top(Data, Opts) ->
    [Title, _Prepared, _Responsible, _DocNo, _Approved, _Checked,
     _Date, _Rev, _File] = Data,
    html_header(Title, Opts) ++
    docb_util:html_snippet(top, Opts).

chapter_bot(Opts) ->
    report_bot(Opts).

%% Application ToC

application_toc_top(Data, DocName, Opts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<center>
<strong>" ++ Title ++ "</strong>
<p>
<small>
  " ++ DocNo ++ version(Opts, Vsn0) ++ "
</small>
</p>
<p>
<small>
  <a target=\"document\" href=\"" ++ DocName ++	"_cite.html\">Bibliography</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_term.html\">Glossary</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_index.html\">Index</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_first.html\">Cover</a>" ++ top_index(Opts) ++
"</small>
</p>
</center>
<p>
<small>
<strong>Table of Contents</strong>
</small>
</p>
".

application_toc_top(Data, DocName, Opts, HRefTexts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<center>
<small>
" ++
	docb_util:join(
	  lists:map(
	    fun({HRef, Text}) ->
		    "<a target=\"_top\" href=\"" ++ HRef ++ "\">" ++
			Text ++ "</a>"
	    end,
	    HRefTexts), " | ") ++ top_index(Opts) ++
"</small>
<p>
<strong>" ++ Title ++ "</strong>
</p>
<p>
<small>" ++ DocNo ++ version(Opts, Vsn0) ++ "<br />
</small>
</p>
<p>
<small>
  <a target=\"document\" href=\"" ++ DocName ++ "_cite.html\">Bibliography</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_term.html\">Glossary</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_index.html\">Index</a> |
  <a target=\"document\" href=\"" ++ DocName ++	"_first.html\">Cover</a>
</small>
</p>
</center>
<p>
<small>
<strong>Table of Contents</strong>
</small>
</p>
".

%% Part ToC

part_toc_top(Data, DocName, Opts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<center>
<p>
<strong>" ++ Title ++ "</strong>
</p>
<p>
<small>" ++ DocNo ++ version(Opts, Vsn0) ++ "<br />
</small>
</p>
<p>
<small>
  <a target=\"document\" href=\"" ++ DocName ++	"_cite.html\">Bibliography</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_term.html\">Glossary</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_first.html\">Cover</a>" ++
	top_index(Opts) ++
"</small>
</p>
</center>
<p>
<small>
<strong>Table of Contents</strong>
</small>
</p>
".

part_toc_top(Data, DocName, Opts, HRefTexts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<center>
<p>
<small>
" ++
	docb_util:join(
	  lists:map(
	    fun({HRef, Text}) ->
		    "<a target=\"_top\" href=\"" ++ HRef ++ "\">" ++
			Text ++ "</a>"
	    end,
	    HRefTexts), " | ") ++ top_index(Opts) ++
"</small>
</p>
<p>
<strong>" ++ Title ++ "</strong>
</p>
<p>
<small>
  " ++ DocNo ++ version(Opts, Vsn0) ++ "<br />
</small>
</p>
<p>
<small>
  <a target=\"document\" href=\"" ++ DocName ++ "_cite.html\">Bibliography</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_term.html\">Glossary</a> |
  <a target=\"document\" href=\"" ++ DocName ++ "_first.html\">Cover</a>
</small>
</p>
</center>
<p>
<small>
<strong>Table of Contents</strong>
</small>
</p>
".

part_toc_bot() ->
"</body >
</html>
".

%% Index

index_top(_Data) ->
    ref_html_header("INDEX", []) ++
"<h1>INDEX</h1>
<p><em>Emphasized</em> index entries refer to <em>modules</em>
and <code>Courier</code> ditos to <code>functions</code>.\n</p>\n".

index_bot() ->
    part_toc_bot().

%% Internal functions

html_header(Title, Opts) ->
    Vsn = docb_util:version(),
%%"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<!-- This document was generated using DocBuilder-" ++ Vsn ++ " -->
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
  <title>" ++ Title ++ "</title>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>
  " ++ docb_util:html_snippet(head, Opts) ++ "
  <style type=\"text/css\">
<!--
    body          { font-family: Verdana, Arial, Helvetica, sans-serif }
    span.bold_code        { font-family: courier;font-weight: bold}
    span.code        { font-family: courier;font-weight: normal}

.note, .warning {
  border: solid black 1px;
  margin: 1em 3em;
}

.note .label {
  background: #30d42a;
  color: white;
  font-weight: bold;
  padding: 5px 10px;
}
.note .content {
  background: #eafeea;
  color: black;
  line-height: 120%;
  font-size: 90%;
  padding: 5px 10px;
}
.warning .label {
  background: #C00;
  color: white;
  font-weight: bold;
  padding: 5px 10px;
}
.warning .content {
  background: #FFF0F0;
  color: black;
  line-height: 120%;
  font-size: 90%;
  padding: 5px 10px;
}

    .example     { background-color:#eeeeff } 
    pre          { font-family: courier; font-weight: normal }
    .REFBODY     { margin-left: 13mm }
    .REFTYPES    { margin-left: 8mm }
-->
  </style>
</head>
<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\" vlink=\"#FF00FF\" alink=\"#FF0000\">
".

ref_html_header(Title, Opts) ->
    Vsn = docb_util:version(),
%%"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<!-- This document was generated using DocBuilder-" ++ Vsn ++ " -->
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
  <title>" ++ Title ++ "</title>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>
  " ++ docb_util:html_snippet(head, Opts) ++ "
  <style type=\"text/css\">
<!--
    body          { font-family: Verdana, Arial, Helvetica, sans-serif }
    span.bold_code        { font-family: courier;font-weight: bold}
    span.code        { font-family: courier;font-weight: normal}

.note, .warning {
  border: solid black 1px;
  margin: 1em 3em;
}

.note .label {
  background: #30d42a;
  color: white;
  font-weight: bold;
  padding: 5px 10px;
}
.note .content {
  background: #eafeea;
  color: black;
  line-height: 120%;
  font-size: 90%;
  padding: 5px 10px;
}
.warning .label {
  background: #C00;
  color: white;
  font-weight: bold;
  padding: 5px 10px;
}
.warning .content {
  background: #FFF0F0;
  color: black;
  line-height: 120%;
  font-size: 90%;
  padding: 5px 10px;
}

    .example     { background-color:#eeeeff } 
    pre          { font-family: courier; font-weight: normal }
    .REFBODY     { margin-left: 13mm }
    .REFTYPES    { margin-left: 8mm }
-->
  </style>
</head>
<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\" vlink=\"#FF00FF\" alink=\"#FF0000\">
".

version(Opts, Vsn0) ->
    case docb_util:lookup_option(vsn, Opts, Vsn0) of
	"" -> "";
	Vsn -> " Version " ++ Vsn
    end.

top_index(Opts) ->
    case docb_util:lookup_option(top, Opts) of
	false -> "";
	TIFile ->
	    " | <a target=\"_top\" href=\"" ++ TIFile ++ "\">Top</a>"
    end.
