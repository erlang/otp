# XSLT-like transformations

### Examples

---

#### Example 1 Using xslapply

original XSLT:

    <xsl:template match="doc/title">
        <h1>
          <xsl:apply-templates/>
        </h1>
    </xsl:template>

becomes in Erlang:

    template(E = #xmlElement{ parents=[{'doc',_}|_], name='title'}) ->
        ["<h1>",
             xslapply(fun template/1, E),
         "</h1>"];

---

---

#### Example 2 Using value_of and select

    <xsl:template match="title">
      <div align="center"><h1><xsl:value-of select="." /></h1></div>
    </xsl:template>

becomes:

    template(E = #xmlElement{name='title'}) ->
        ["<div align=\"center\"><h1>", value_of(select(".", E)), "</h1></div>"];

---

---

#### Example 3 Simple xsl stylesheet

A complete example with the XSLT sheet in the xmerl distribution.

    <xsl:stylesheet version="1.0"
    		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    		xmlns="http://www.w3.org/TR/xhtml1/strict">

      <xsl:strip-space elements="doc chapter section"/>
      <xsl:output
    	method="xml"
    	indent="yes"
    	encoding="iso-8859-1"
      />

      <xsl:template match="doc">
        <html>
          <head>
            <title>
              <xsl:value-of select="title"/>
            </title>
          </head>
          <body>
            <xsl:apply-templates/>
          </body>
        </html>
      </xsl:template>

      <xsl:template match="doc/title">
        <h1>
          <xsl:apply-templates/>
        </h1>
      </xsl:template>

      <xsl:template match="chapter/title">
        <h2>
          <xsl:apply-templates/>
        </h2>
      </xsl:template>

      <xsl:template match="section/title">
        <h3>
          <xsl:apply-templates/>
        </h3>
      </xsl:template>

      <xsl:template match="para">
        <p>
          <xsl:apply-templates/>
        </p>
      </xsl:template>

      <xsl:template match="note">
        <p class="note">
          <b>NOTE: </b>
          <xsl:apply-templates/>
        </p>
      </xsl:template>

      <xsl:template match="emph">
        <em>
          <xsl:apply-templates/>
        </em>
      </xsl:template>

    </xsl:stylesheet>

---

---

#### Example 4 Erlang version

Erlang transformation of previous example:

    -include("xmerl.hrl").

    -import(xmerl_xs,
    	[ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).

    doctype()->
        "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\
     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd \">".

    process_xml(Doc)->
    	template(Doc).

    template(E = #xmlElement{name='doc'})->
        [ "<\?xml version=\"1.0\" encoding=\"iso-8859-1\"\?>",
          doctype(),
          "<html xmlns=\"http://www.w3.org/1999/xhtml\" >"
          "<head>"
          "<title>", value_of(select("title",E)), "</title>"
          "</head>"
          "<body>",
          xslapply( fun template/1, E),
          "</body>"
          "</html>" ];


    template(E = #xmlElement{ parents=[{'doc',_}|_], name='title'}) ->
        ["<h1>",
         xslapply( fun template/1, E),
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
        ["<p>", xslapply( fun template/1, E), "</p>"];

    template(E = #xmlElement{ name='note'}) ->
        ["<p class=\"note\">"
         "<b>NOTE: </b>",
         xslapply( fun template/1, E),
         "</p>"];

    template(E = #xmlElement{ name='emph'}) ->
        ["<em>", xslapply( fun template/1, E), "</em>"];

    template(E)->
        built_in_rules( fun template/1, E).

It is important to end with a call to `xmerl_xs:built_in_rules/2` if you want any
text to be written in "push" transforms. That are the ones using a lot `xslapply(
fun template/1, E )` instead of `value_of(select("xpath",E))`, which is pull...

---

The largest example is the stylesheet to transform this document from the
Simplified Docbook XML format to xhtml. The source file is sdocbook2xhtml.erl.

### Tips and tricks

#### for-each

The function for-each is quite common in XSLT stylesheets. It can often be
rewritten and replaced by select/1. Since select/1 returns a list of
#xmlElements and xslapply/2 traverses them it is more or less the same as to
loop over all the elements.

#### position()

The XSLT position() and #xmlElement.pos are not the same. One has to make an own
position in Erlang.

---

#### Example 5 Counting positions

    <xsl:template match="stanza">
      <p><xsl:apply-templates select="line" /></p>
    </xsl:template>

    <xsl:template match="line">
      <xsl:if test="position() mod 2 = 0">&#160;&#160;</xsl:if>
      <xsl:value-of select="." /><br />
    </xsl:template>

Can be written as

    template(E = #xmlElement{name='stanza'}) ->
        {Lines,LineNo} = lists:mapfoldl(fun template_pos/2, 1, select("line", E)),
        ["<p>", Lines, "</p>"].

    template_pos(E = #xmlElement{name='line'}, P) ->
        {[indent_line(P rem 2), value_of(E#xmlElement.content), "<br />"], P + 1 }.

    indent_line(0)->"&#160;&#160;";
    indent_line(_)->"".

---

#### Global tree awareness

In XSLT you have "root" access to the top of the tree with XPath, even though
you are somewhere deep in your tree.

The xslapply/2 function only carries back the child part of the tree to the
template fun. But it is quite easy to write template funs that handles both the
child and top tree.

---

#### Example 6 Passing the root tree

The following example piece will prepend the article title to any section title

    template(E = #xmlElement{name='title'}, ETop ) ->
        ["<h3>", value_of(select("title", ETop))," - ",
         xslapply( fun(A) -> template(A, ETop) end, E),
         "</h3>"];

---
