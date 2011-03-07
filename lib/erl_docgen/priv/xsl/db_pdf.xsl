<?xml version="1.0" encoding="utf-8"?>
<!--
     #
     # %CopyrightBegin%
     #
     # Copyright Ericsson AB 2009-2011. All Rights Reserved.
     #
     # The contents of this file are subject to the Erlang Public License,
     # Version 1.1, (the "License"); you may not use this file except in
     # compliance with the License. You should have received a copy of the
     # Erlang Public License along with this software. If not, it can be
     # retrieved online at http://www.erlang.org/.
     #
     # Software distributed under the License is distributed on an "AS IS"
     # basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
     # the License for the specific language governing rights and limitations
     # under the License.
     #
     # %CopyrightEnd%

     -->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

  <xsl:output method="xml" indent="yes"/>

  <xsl:include href="db_pdf_params.xsl"/>

  <!-- Start of Dialyzer type/spec tags.
       See also the template matching "name" and the template "bookmarks6"
  -->

  <xsl:param name="specs_file" select="''"/>
  <xsl:variable name="i" select="document($specs_file)"></xsl:variable>

  <xsl:template name="err">
    <xsl:param name="m"/>
    <xsl:param name="n"/>
    <xsl:param name="a"/>
    <xsl:param name="s"/>
    <xsl:message terminate="yes">
  Error <xsl:if test="$m != ''"><xsl:value-of select ="$m"/>:</xsl:if>
	 <xsl:value-of
		   select="$n"/>/<xsl:value-of
		   select="$a"/>: <xsl:value-of select="$s"/>
    </xsl:message>
  </xsl:template>

  <xsl:template name="spec_name">
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="mod" select="@mod"/>
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="arity" select="@arity"/>
    <xsl:variable name="clause" select="@clause"/>
    <xsl:variable name="spec0" select=
        "$i/specs/module[@name=$curModule]/spec
             [name=$name and arity=$arity
              and (string-length($mod) = 0 or module = $mod)]"/>
    <xsl:variable name="spec" select="$spec0[string-length($clause) = 0
                                             or position() = $clause]"/>
    <xsl:if test="count($spec) = 0">
      <xsl:call-template name="err">
	<xsl:with-param name="m" select="$mod"/>
	<xsl:with-param name="n" select="$name"/>
	<xsl:with-param name="a" select="$arity"/>
	<xsl:with-param name="s">unknown spec</xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="ancestor::cref">
	<xsl:message terminate="yes">
          Error: did not expect a 'name' tag with name/arity attributes here!
	</xsl:message>
      </xsl:when>
      <xsl:when test="ancestor::erlref">
        <fo:block id="{generate-id()}">
	  <xsl:choose>
            <xsl:when test="string(@with_guards) = 'no'">
               <xsl:apply-templates select="$spec/contract/clause/head"/>
            </xsl:when>
            <xsl:otherwise>
	      <xsl:call-template name="contract">
		<xsl:with-param name="contract" select="$spec/contract"/>
	      </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </fo:block>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="contract">
    <xsl:param name="contract"/>
    <xsl:call-template name="clause">
      <xsl:with-param name="clause" select="$contract/clause"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="clause">
    <xsl:param name="clause"/>
    <xsl:variable name="type_desc" select="../type_desc"/>
    <xsl:for-each select="$clause">
      <xsl:apply-templates select="head"/>
      <xsl:if test="count(guard) > 0">
	<xsl:call-template name="guard">
	  <xsl:with-param name="guard" select="guard"/>
	  <xsl:with-param name="type_desc" select="$type_desc"/>
	</xsl:call-template>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="head">
    <fo:block xsl:use-attribute-sets="function-name">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>

  <xsl:template name="guard">
    <fo:block>
      <xsl:text>Types:</xsl:text>
    </fo:block>
    <fo:list-block xsl:use-attribute-sets="type-listblock">
      <xsl:call-template name="subtype">
	<xsl:with-param name="subtype" select="$guard/subtype"/>
	<xsl:with-param name="type_desc" select="$type_desc"/>
      </xsl:call-template>
    </fo:list-block>
  </xsl:template>

  <xsl:template name="subtype">
    <xsl:param name="subtype"/>
    <xsl:param name="type_desc"/>
    <xsl:for-each select="$subtype">
      <xsl:variable name="tname" select="typename"/>
      <xsl:variable name="tdesc" select="$type_desc[@name = $tname]"/>
      <fo:list-item xsl:use-attribute-sets="type-listitem">
	<fo:list-item-label end-indent="label-end()">
	  <fo:block>
	  </fo:block>
	</fo:list-item-label>
	<fo:list-item-body start-indent="body-start()" format="justify">
	  <fo:block font-weight="bold">
            <xsl:apply-templates select="string"/>
	  </fo:block>
	</fo:list-item-body>
      </fo:list-item>
      <xsl:apply-templates select="$type_desc[@name = $tname]"/>
    </xsl:for-each>
  </xsl:template>

  <!-- Note: <type_desc> has not been implemented for data types. -->

  <!-- Similar to <d> -->
  <xsl:template match="type_desc">
    <fo:list-item xsl:use-attribute-sets="type-listitem">
      <fo:list-item-label end-indent="label-end()"><fo:block></fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()" format="justify">
        <fo:block>
          <xsl:apply-templates/>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>

  <!-- Datatypes -->
  <xsl:template match="datatypes">
    <fo:block  xsl:use-attribute-sets="h3">
      <xsl:text>Data Types</xsl:text>
    </fo:block>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Datatype -->
  <xsl:template match="datatype">
    <fo:block xsl:use-attribute-sets="function-name">
      <xsl:apply-templates select="name"/>
    </fo:block>
    <xsl:apply-templates select="desc"/>
  </xsl:template>

  <!-- Like <head>... -->
  <xsl:template match="typehead">
    <fo:block xsl:use-attribute-sets="function-name">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>

  <!-- Like <guard>, except "Types:"... -->
  <xsl:template match="local_defs">
    <fo:list-block xsl:use-attribute-sets="type-listblock">
      <xsl:apply-templates/>
    </fo:list-block>
  </xsl:template>

  <!-- Like <subtype>... -->
  <xsl:template match="local_def">
    <fo:list-item xsl:use-attribute-sets="type-listitem">
      <fo:list-item-label end-indent="label-end()">
        <fo:block>
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()" format="justify">
        <fo:block font-weight="bold">
          <xsl:apply-templates/>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>

  <xsl:template name="type_name">
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="mod" select="@mod"/>
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="n_vars">
      <xsl:choose>
	<xsl:when test="string-length(@n_vars) > 0">
	  <xsl:value-of select="@n_vars"/>
	</xsl:when>
	<xsl:otherwise>
          <xsl:value-of select="0"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="string-length($name) > 0">
	<xsl:variable name="type" select=
	    "$i/specs/module[@name=$curModule]/type
		 [name=$name and n_vars=$n_vars
		  and (string-length($mod) = 0 or module = $mod)]"/>

	<xsl:if test="count($type) != 1">
	  <xsl:call-template name="err">
	    <xsl:with-param name="m" select="$mod"/>
	    <xsl:with-param name="n" select="$name"/>
	    <xsl:with-param name="a" select="$n_vars"/>
	    <xsl:with-param name="s">unknown type</xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
	<xsl:apply-templates select="$type/typedecl"/>
      </xsl:when>
      <xsl:otherwise>
	<fo:inline font-weight="bold" xsl:use-attribute-sets="type-listitem">
	<xsl:value-of select="."/>
	</fo:inline>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Used both in <datatype> and in <func>! -->
  <xsl:template match="anno">
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="anno" select="normalize-space(text())"/>
    <xsl:variable name="namespec"
                  select="ancestor::desc/preceding-sibling::name"/>
    <xsl:if test="count($namespec) = 0 and string-length($specs_file) > 0">
      <xsl:call-template name="err">
	<xsl:with-param name="s">cannot find 'name' (<xsl:value-of select="$anno"/>)
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <xsl:variable name="mod" select="$namespec/@mod"/>
    <xsl:variable name="name" select="$namespec/@name"/>
    <xsl:variable name="arity" select="$namespec/@arity"/>
    <xsl:variable name="clause" select="$namespec/@clause"/>
    <xsl:variable name="tmp_n_vars" select="$namespec/@n_vars"/>
    <xsl:variable name="n_vars">
      <xsl:choose>
	<xsl:when test="string-length($tmp_n_vars) > 0">
	  <xsl:value-of select="$tmp_n_vars"/>
	</xsl:when>
	<xsl:otherwise>
          <xsl:value-of select="0"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="spec0" select=
        "$i/specs/module[@name=$curModule]/spec
             [name=$name and arity=$arity
              and (string-length($mod) = 0 or module = $mod)]"/>
    <xsl:variable name="spec_annos" select=
         "$spec0[string-length($clause) = 0
                 or position() = $clause]/anno[.=$anno]"/>
    <xsl:variable name="type_annos" select=
        "$i/specs/module[@name=$curModule]/type
             [name=$name and n_vars=$n_vars
              and (string-length($mod) = 0 or module = $mod)]/anno[.=$anno]"/>

    <xsl:if test="count($spec_annos) = 0
                  and count($type_annos) = 0
	          and string-length($specs_file) > 0">
      <xsl:variable name="n">
        <xsl:choose>
          <xsl:when test="string-length($arity) = 0">
            <xsl:value-of select="$n_vars"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$arity"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:call-template name="err">
	<xsl:with-param name="m" select="$mod"/>
	<xsl:with-param name="n" select="$name"/>
	<xsl:with-param name="a" select="$n"/>
	<xsl:with-param name="s">unknown annotation <xsl:value-of select="$anno"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:value-of select="$anno"/>
  </xsl:template>

  <!-- Used for indentation of formatted types and specs -->
  <xsl:template match="nbsp">
    <xsl:text>&#160;</xsl:text>
  </xsl:template>

  <!-- End of Dialyzer type/spec tags -->

  <xsl:template match="/">
    <xsl:apply-templates select="book"/>
  </xsl:template>


  <xsl:template match="book">
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">

      <!-- Master pages -->
      <fo:layout-master-set>
        <fo:simple-page-master
            master-name="cover"
            margin="25mm">
          <xsl:attribute name="page-height">
            <xsl:value-of select="$page-height"/>
          </xsl:attribute>
          <xsl:attribute name="page-width">
            <xsl:value-of select="$page-width"/>
          </xsl:attribute>
          <fo:region-body
              margin="0mm"/>
        </fo:simple-page-master>

        <fo:simple-page-master
            master-name="left-page"
            margin-top="25mm"
            margin-bottom="15mm"
            margin-left="20mm"
            margin-right="25mm">
          <xsl:attribute name="page-height">
            <xsl:value-of select="$page-height"/>
          </xsl:attribute>
          <xsl:attribute name="page-width">
            <xsl:value-of select="$page-width"/>
          </xsl:attribute>
          <fo:region-body
              margin-top="15mm"
              margin-bottom="20mm"/>
          <fo:region-before
              region-name="left-header"
              extent="10mm"/>
          <fo:region-after
              region-name="left-footer"
              extent="10mm"/>
        </fo:simple-page-master>

        <fo:simple-page-master
            master-name="right-page"
            margin-top="25mm"
            margin-bottom="15mm"
            margin-left="25mm"
            margin-right="20mm">
          <xsl:attribute name="page-height">
            <xsl:value-of select="$page-height"/>
          </xsl:attribute>
          <xsl:attribute name="page-width">
            <xsl:value-of select="$page-width"/>
          </xsl:attribute>
          <fo:region-body
              margin-top="15mm"
              margin-bottom="20mm"/>
          <fo:region-before
              region-name="right-header"
              extent="10mm"/>
          <fo:region-after
              region-name="right-footer"
              extent="10mm"/>
        </fo:simple-page-master>


        <fo:page-sequence-master master-name="document">
          <fo:repeatable-page-master-alternatives>
            <fo:conditional-page-master-reference
                master-reference="left-page"
                odd-or-even="even"/>
            <fo:conditional-page-master-reference
                master-reference="right-page"
                odd-or-even="odd"/>
          </fo:repeatable-page-master-alternatives>
        </fo:page-sequence-master>
      </fo:layout-master-set>


      <!-- Process bookmarks -->
      <xsl:call-template name="bookmarks.tree"/>

      <!-- Process cover page -->
      <xsl:apply-templates select="header"/>

      <!-- Process toc -->
      <!-- xsl:call-template name="toc"/ -->

      <fo:page-sequence
          font-family="serif"
          master-reference="document"
          initial-page-number="1">
        <xsl:attribute name="font-size">
          <xsl:value-of select="$base-font-size"/>
        </xsl:attribute>

        <fo:static-content flow-name="left-header">
          <fo:block xsl:use-attribute-sets="page-header" text-align="start">
            <fo:retrieve-marker
                retrieve-boundary="page-sequence"
                retrieve-class-name="chapter-title"
                retrieve-position="first-including-carryover"/>
          </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="right-header">
          <fo:block xsl:use-attribute-sets="page-header" text-align="end">
            <fo:retrieve-marker
                retrieve-boundary="page-sequence"
                retrieve-class-name="chapter-title"
                retrieve-position="first-including-carryover"/>
          </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="left-footer">
          <fo:block xsl:use-attribute-sets="page-footer" text-align="start">
            <fo:page-number/>
            <xsl:text> | </xsl:text>
            <xsl:value-of select="$companyname"/>:
            <xsl:value-of select="/book/header/title"/>
          </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="right-footer">
          <fo:block xsl:use-attribute-sets="page-footer" text-align="end">
            <xsl:value-of select="$companyname"/>:
            <xsl:value-of select="/book/header/title"/>
            <xsl:text> | </xsl:text>
            <fo:page-number/>
          </fo:block>
        </fo:static-content>

        <fo:flow flow-name="xsl-region-body">
          <fo:block>

          </fo:block>
          <xsl:apply-templates select="parts"/>

          <xsl:apply-templates select="applications"/>

        </fo:flow>

      </fo:page-sequence>

    </fo:root>

  </xsl:template>


 <!-- Header -->
 <xsl:template match="book/header">
   <xsl:apply-templates select="title"/>
 </xsl:template>


  <!-- Cover page -->
  <xsl:template match="header/title">
    <fo:page-sequence
        font-family="sans-serif"
        force-page-count="even"
        master-reference="cover">
      <xsl:attribute name="font-size">
        <xsl:value-of select="$base-font-size"/>
      </xsl:attribute>

      <fo:flow flow-name="xsl-region-body">
        <fo:block xsl:use-attribute-sets="cover.logo">
          <fo:external-graphic src="{$docgen}/priv/images/erlang-logo.gif"/>
        </fo:block>
        <fo:block xsl:use-attribute-sets="cover.title" id="cover-page">
          <xsl:apply-templates/>
        </fo:block>
        <fo:block xsl:use-attribute-sets="cover.copyright">
          <xsl:value-of select="$copyright"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="$companyname"/>
        </fo:block>
        <xsl:if test="/book/header/title">
          <fo:block xsl:use-attribute-sets="cover.version">
            <xsl:value-of select="/book/header/title"/>
            <xsl:text> </xsl:text>
            <!-- xsl:value-of select="/book/header/rev"/ -->
            <xsl:value-of select="$appver"/>
          </fo:block>
        </xsl:if>
        <fo:block xsl:use-attribute-sets="cover.version">
          <xsl:value-of select="$gendate"/>
        </fo:block>

        <!-- Inner cover (copyright notice) -->
        <fo:block break-before="page"
                  xsl:use-attribute-sets="cover.inner.copyright">
          <xsl:value-of select="$copyright"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="$companyname"/>
        </fo:block>
        <fo:block xsl:use-attribute-sets="cover.inner.copyrightnotice">
          <xsl:value-of select="/book/header/legalnotice"/>

        <!--   The contents of this file are subject to the Erlang Public License,
  Version 1.1, (the "License"); you may not use this file except in
  compliance with the License. You should have received a copy of the
  Erlang Public License along with this software. If not, it can be
  retrieved online at http://www.erlang.org/.

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
  the License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is
-->
          <xsl:value-of select="$companyname"/>.
        </fo:block>
        <fo:block xsl:use-attribute-sets="cover.inner.date">
          <xsl:value-of select="$gendate"/>
        </fo:block>
      </fo:flow>
    </fo:page-sequence>

  </xsl:template>


  <!-- Bookmarks -->
  <xsl:template name="bookmarks.tree">
    <fo:bookmark-tree>
      <fo:bookmark internal-destination="cover-page"
                   starting-state="show">
        <fo:bookmark-title>
          <xsl:value-of select="/book/header/title"/>
        </fo:bookmark-title>

        <xsl:call-template name="bookmarks1">
          <xsl:with-param name="entries" select="parts/part[header/title]"/>
        </xsl:call-template>
        <xsl:call-template name="bookmarks4">
          <xsl:with-param name="entries" select="applications/application[header/title]"/>
        </xsl:call-template>

      </fo:bookmark>
    </fo:bookmark-tree>
  </xsl:template>


  <!-- Users Guide Bookmarks -->

  <xsl:template name="bookmarks1">
    <xsl:param name="entries"/>
    <xsl:if test="$entries != ''">

      <fo:bookmark internal-destination="{generate-id(/book/parts/part)}"
        starting-state="hide">
        <fo:bookmark-title>User's Guide</fo:bookmark-title>

        <xsl:for-each select="$entries">
          <xsl:call-template name="bookmarks2">
            <xsl:with-param name="entries"
              select="chapter[header/title]"/>
          </xsl:call-template>
        </xsl:for-each>

      </fo:bookmark>
    </xsl:if>
  </xsl:template>

  <xsl:template name="bookmarks2">
    <xsl:param name="entries"/>
    <xsl:for-each select="$entries">
      <fo:bookmark internal-destination="{generate-id(header/title)}"
                   starting-state="hide">
        <fo:bookmark-title>
          <xsl:value-of select="header/title"/>
        </fo:bookmark-title>

        <xsl:call-template name="bookmarks3">
          <xsl:with-param name="entries"
                          select="section[title]"/>
        </xsl:call-template>

      </fo:bookmark>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="bookmarks3">
    <xsl:param name="entries"/>
    <xsl:for-each select="$entries">
      <fo:bookmark internal-destination="{generate-id(title)}"
                   starting-state="hide">
        <fo:bookmark-title>
          <xsl:value-of select="title"/>
        </fo:bookmark-title>
        <xsl:call-template name="bookmarks3">
          <xsl:with-param name="entries"
                          select="section[title]"/>
        </xsl:call-template>
      </fo:bookmark>
    </xsl:for-each>
  </xsl:template>

  <!-- Ref. Manual Bookmarks -->

  <xsl:template name="bookmarks4">
    <xsl:param name="entries"/>

    <xsl:if test="$entries != ''">
      <fo:bookmark internal-destination="{generate-id(/book/applications/application)}"
        starting-state="hide">
        <fo:bookmark-title>Reference Manual</fo:bookmark-title>
        <xsl:for-each select="$entries">

          <xsl:call-template name="bookmarks5">
            <xsl:with-param name="entries"
              select="erlref[module]|comref[com]|cref[lib]|fileref[file]|appref[app]"/>
          </xsl:call-template>
        </xsl:for-each>
      </fo:bookmark>
    </xsl:if>
  </xsl:template>


  <xsl:template name="bookmarks5">
    <xsl:param name="entries"/>
    <xsl:for-each select="$entries">
      <fo:bookmark internal-destination="{generate-id(module)}{generate-id(lib)}{generate-id(com)}{generate-id(file)}{generate-id(app)}"
                   starting-state="hide">
        <fo:bookmark-title><xsl:value-of select="module"/><xsl:value-of select="lib"/><xsl:value-of select="com"/><xsl:value-of select="file"/><xsl:value-of select="app"/></fo:bookmark-title>
        <!-- xsl:choose>
          <xsl:when test="ancestor::erlref">
            <xsl:value-of select="module"/>
          </xsl:when>
           <xsl:when test="ancestor::cref">
             <xsl:value-of select="lib"/>
          </xsl:when>
          <xsl:when test="ancestor::comref">
            <xsl:value-of select="com"/>
          </xsl:when>
        </xsl:choose>
        </fo:bookmark-title -->
        <xsl:call-template name="bookmarks6">
          <xsl:with-param name="entries"
            select="funcs/func/name"/>
        </xsl:call-template>
      </fo:bookmark>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="bookmarks6">
    <xsl:param name="entries"/>
    <xsl:for-each select="$entries">

      <xsl:choose>
        <xsl:when test="ancestor::cref">
          <fo:bookmark internal-destination="{generate-id(nametext)}" starting-state="hide">
            <xsl:variable name="fname">
              <xsl:value-of select="substring-before(nametext, '(')"/>
            </xsl:variable>
            <fo:bookmark-title>
              <xsl:choose>
                <xsl:when test="string-length($fname) > 0">
                   <xsl:value-of select="$fname"/>()
               </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="nametext"/>()
                </xsl:otherwise>
              </xsl:choose>
            </fo:bookmark-title>
          </fo:bookmark>
        </xsl:when>
        <xsl:when test="ancestor::erlref">
          <fo:bookmark internal-destination="{generate-id(.)}" starting-state="hide">
            <xsl:variable name="tmpstring">
              <xsl:value-of select="substring-before(substring-after(., '('), '->')"/>
            </xsl:variable>

            <xsl:variable name="ustring">
              <xsl:choose>
                <xsl:when test="string-length($tmpstring) > 0">
                  <xsl:call-template name="remove-paren">
                    <xsl:with-param name="string" select="$tmpstring"/>
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:call-template name="remove-paren">
                    <xsl:with-param name="string" select="substring-after(., '(')"/>
                  </xsl:call-template>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>

            <xsl:variable name="arity">
	      <xsl:choose>
		<xsl:when test="string-length(@arity) > 0">
		  <!-- Dialyzer spec -->
		  <xsl:value-of select="@arity"/>
		</xsl:when>
                <xsl:otherwise>
                  <xsl:call-template name="calc-arity">
                    <xsl:with-param name="string" select="substring-before($ustring, ')')"/>
                    <xsl:with-param name="no-of-pars" select="0"/>
                  </xsl:call-template>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>

            <xsl:variable name="fname">
	      <xsl:choose>
		<xsl:when test="string-length(@name) > 0">
		  <!-- Dialyzer spec -->
		  <xsl:value-of select="@name"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:variable name="fname1">
		    <xsl:value-of select="substring-before(., '(')"/>
		  </xsl:variable>
		  <xsl:variable name="fname2">
		    <xsl:value-of select="substring-after($fname1, 'erlang:')"/>
		  </xsl:variable>
		  <xsl:choose>
		    <xsl:when test="string-length($fname2) > 0">
		      <xsl:value-of select="$fname2"/>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:value-of select="$fname1"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</xsl:otherwise>
	      </xsl:choose>
            </xsl:variable>

            <fo:bookmark-title>
              <xsl:value-of select="$fname"/>/<xsl:value-of select="$arity"/>
            </fo:bookmark-title>
          </fo:bookmark>
        </xsl:when>
      </xsl:choose>

    </xsl:for-each>
  </xsl:template>


  <!-- UG part -->

  <!-- Parts -->
  <xsl:template match="parts">
    <xsl:apply-templates select="part"/>
  </xsl:template>

  <!-- Part -->
  <xsl:template match="part">
    <xsl:variable name="partnum"><xsl:number level="any" from="book" count="part|application"/></xsl:variable>

    <fo:block xsl:use-attribute-sets="h1" id="{generate-id()}">
      <xsl:value-of select="$partnum"/>&#160;&#160;&#160;
      <xsl:text>User's Guide</xsl:text>
    </fo:block>

    <xsl:apply-templates select="description">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

    <xsl:apply-templates select="chapter">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>
  </xsl:template>


  <!-- Chapter -->
  <xsl:template match="chapter">
    <xsl:param name="partnum"/>

    <fo:block xsl:use-attribute-sets="h2" id="{generate-id(header/title)}">
      <fo:marker marker-class-name="chapter-title">
        <xsl:value-of select="$partnum"/>.<xsl:number/>&#160;&#160;<xsl:value-of select="header/title"/>
      </fo:marker>
      <xsl:value-of select="$partnum"/>.<xsl:number/>&#160;&#160;<xsl:value-of select="header/title"/>

    </fo:block>

    <xsl:apply-templates select="section|quote|warning|note|br|image|marker|table|p|pre|code|list|taglist|codeinclude|erleval">
      <xsl:with-param name="partnum" select="$partnum"/>
      <xsl:with-param name="chapnum"><xsl:number/></xsl:with-param>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Chapter/Section -->
  <xsl:template match="chapter/section">
    <xsl:param name="partnum"/>
    <xsl:param name="chapnum"/>
    <fo:block xsl:use-attribute-sets="h3" id="{generate-id(title)}">
      <xsl:value-of select="$partnum"/>.<xsl:value-of select="$chapnum"/>.<xsl:number/>&#160;
      <xsl:value-of select="title"/>
    </fo:block>
    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
      <xsl:with-param name="chapnum" select="$chapnum"/>
      <xsl:with-param name="sectnum"><xsl:number/></xsl:with-param>
    </xsl:apply-templates>

  </xsl:template>

  <!--  Chapter/Subsection  -->
  <xsl:template match="chapter/section/section">
    <xsl:param name="partnum"/>
    <xsl:param name="chapnum"/>
    <xsl:param name="sectnum"/>
    <fo:block xsl:use-attribute-sets="h4" id="{generate-id(title)}">
      <!-- xsl:value-of select="$partnum"/>.<xsl:value-of select="$chapnum"/>.<xsl:value-of select="$sectnum"/>.<xsl:number/ -->
      <xsl:value-of select="title"/>
    </fo:block>
    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
      <xsl:with-param name="chapnum" select="$chapnum"/>
      <xsl:with-param name="sectnum" select="$sectnum"/>
    </xsl:apply-templates>
  </xsl:template>


  <!--  Subsection below level 2 -->
  <xsl:template match="section/section/section">
    <xsl:param name="partnum"/>
    <xsl:param name="chapnum"/>
    <xsl:param name="sectnum"/>
    <fo:block xsl:use-attribute-sets="h5" id="{generate-id(title)}">
      <!-- xsl:value-of select="$partnum"/>.<xsl:value-of select="$chapnum"/>.<xsl:value-of select="$sectnum"/>.<xsl:number/ -->
      <xsl:value-of select="title"/>
    </fo:block>
    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
      <xsl:with-param name="chapnum" select="$chapnum"/>
      <xsl:with-param name="sectnum" select="$sectnum"/>
    </xsl:apply-templates>
  </xsl:template>


  <!-- *ref/Section -->
  <xsl:template match="erlref/section|comref/section|cref/section|fileref/section|appref/section">
    <xsl:param name="partnum"/>
    <xsl:param name="chapnum"/>
    <fo:block xsl:use-attribute-sets="h3" id="{generate-id(title)}">
      <xsl:value-of select="title"/>
    </fo:block>
    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
      <xsl:with-param name="chapnum" select="$chapnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!--  *ref/Subsection -->
  <xsl:template match="erlref/section/section|comref/section/section|cref/section/section|fileref/section/section|appref/section/section">
    <xsl:param name="partnum"/>
    <xsl:param name="chapnum"/>
    <xsl:param name="sectnum"/>
    <fo:block xsl:use-attribute-sets="h4" id="{generate-id(title)}">
      <xsl:value-of select="title"/>
    </fo:block>
    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- Section/title -->
 <xsl:template match="section/title">
 </xsl:template>

  <!-- Lists -->

  <xsl:template match="list">
    <xsl:param name="partnum"/>
    <fo:list-block xsl:use-attribute-sets="listblock">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
      <!-- xsl:apply-templates -->
        <!-- xsl:with-param name="chapnum" select="$chapnum"/ -->
      <!-- /xsl:apply-templates -->
    </fo:list-block>
  </xsl:template>

  <xsl:template match="list/item">
    <xsl:param name="partnum"/>
    <fo:list-item xsl:use-attribute-sets="listitem">
      <fo:list-item-label end-indent="label-end()">
        <fo:block>
          &#x2022;
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()" format="justify">
        <fo:block>
          <xsl:apply-templates>
            <xsl:with-param name="partnum" select="$partnum"/>
          </xsl:apply-templates>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>


  <xsl:template match="taglist">
    <xsl:param name="partnum"/>
    <fo:block xsl:use-attribute-sets="taglistblock">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </fo:block>
  </xsl:template>

  <xsl:template match="taglist/tag">
    <xsl:param name="partnum"/>
    <fo:block xsl:use-attribute-sets="tag">
        <xsl:apply-templates/>
    </fo:block>
  </xsl:template>


  <xsl:template match="taglist/item">
    <xsl:param name="partnum"/>
    <fo:block xsl:use-attribute-sets="tagitem">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </fo:block>
  </xsl:template>


  <!-- Note -->
  <xsl:template match="note">
    <xsl:param name="partnum"/>
    <fo:block xsl:use-attribute-sets="note">
        <fo:block xsl:use-attribute-sets="note-warning-title">
            <xsl:text>Note:</xsl:text>
        </fo:block>
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </fo:block>
  </xsl:template>

  <!-- Warning -->
  <xsl:template match="warning">
    <xsl:param name="partnum"/>
    <fo:block xsl:use-attribute-sets="warning">
        <fo:block xsl:use-attribute-sets="note-warning-title">
            <xsl:text>Warning:</xsl:text>
        </fo:block>
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </fo:block>
  </xsl:template>


 <!-- Paragraph -->
  <xsl:template match="p">
    <fo:block xsl:use-attribute-sets="p">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>


  <!-- Inline elements -->
  <xsl:template match="b">
    <fo:inline font-weight="bold">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>

  <xsl:template match="br">
    <fo:block/>
  </xsl:template>

  <xsl:template match="c">
    <fo:inline font-family="monospace">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>

  <xsl:template match="em">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>

  <!-- Code -->
  <xsl:template match="code">
    <xsl:param name="partnum"/>
    <xsl:variable name="codenum">
      <xsl:number level="any" from="part" count="code"/>
    </xsl:variable>

    <fo:block xsl:use-attribute-sets="code">
      <xsl:apply-templates select="text()"/>
    </fo:block>

    <xsl:if test="@caption">
      <fo:block xsl:use-attribute-sets="caption">
          Code listing <xsl:value-of select="$partnum"/>.<xsl:value-of select="$codenum"/>:&#160;
          <xsl:value-of select="@caption"/>
      </fo:block>
    </xsl:if>
  </xsl:template>

  <!-- Pre -->
  <xsl:template match="pre">
    <xsl:param name="partnum"/>
    <xsl:variable name="codenum">
      <xsl:number level="any" from="part" count="code"/>
    </xsl:variable>

    <fo:block xsl:use-attribute-sets="code">
      <xsl:apply-templates/>
    </fo:block>

    <xsl:if test="@caption">
      <fo:block xsl:use-attribute-sets="caption">
          Code listing <xsl:value-of select="$partnum"/>.<xsl:value-of select="$codenum"/>:&#160;
          <xsl:value-of select="@caption"/>
      </fo:block>
    </xsl:if>
  </xsl:template>

  <!--  Ref. manual part  -->

  <!-- Applications -->
  <xsl:template match="applications">
          <xsl:apply-templates select="application"/>
  </xsl:template>

  <!-- Application -->
  <xsl:template match="application">
    <xsl:variable name="partnum">
      <xsl:number level="any" from="book" count="part|application"/>
    </xsl:variable>

    <fo:block xsl:use-attribute-sets="h1" id="{generate-id()}">
      <xsl:if test="/book/header/title">
        <xsl:value-of select="$partnum"/>&#160;&#160;&#160;
      <xsl:text>Reference Manual</xsl:text>
      </xsl:if>
    </fo:block>


    <xsl:apply-templates select="description">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

    <xsl:apply-templates select="erlref|comref|cref|fileref|appref">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Erlref -->
  <xsl:template match="erlref">
    <xsl:param name="partnum"/>

    <fo:block page-break-before="always" xsl:use-attribute-sets="module-header" id="{generate-id(module)}">
      <fo:block xsl:use-attribute-sets="module-name">
        <!-- Mark title for header printout -->
        <fo:marker marker-class-name="chapter-title">
          <xsl:value-of select="module"/>
        </fo:marker>
        <xsl:value-of select="module"/>
      </fo:block>
      <xsl:text>Erlang module</xsl:text>
    </fo:block>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Comref -->
  <xsl:template match="comref">
    <xsl:param name="partnum"/>

    <fo:block page-break-before="always" xsl:use-attribute-sets="module-header" id="{generate-id(com)}">
      <fo:block xsl:use-attribute-sets="module-name">
        <!-- Mark title for header printout -->
        <fo:marker marker-class-name="chapter-title">
          <xsl:value-of select="com"/>
        </fo:marker>
        <xsl:value-of select="com"/>
      </fo:block>
      <xsl:text>Command</xsl:text>
    </fo:block>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Cref -->
  <xsl:template match="cref">
    <xsl:param name="partnum"/>

    <fo:block page-break-before="always" xsl:use-attribute-sets="module-header" id="{generate-id(lib)}">
      <fo:block xsl:use-attribute-sets="module-name">
        <!-- Mark title for header printout -->
        <fo:marker marker-class-name="chapter-title">
          <xsl:value-of select="lib"/>
        </fo:marker>
        <xsl:value-of select="lib"/>
      </fo:block>
      <xsl:text>C Library</xsl:text>
    </fo:block>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Fileref -->
  <xsl:template match="fileref">
    <xsl:param name="partnum"/>

    <fo:block page-break-before="always" xsl:use-attribute-sets="module-header" id="{generate-id(file)}">
      <fo:block xsl:use-attribute-sets="module-name">
        <!-- Mark title for header printout -->
        <fo:marker marker-class-name="chapter-title">
          <xsl:value-of select="file"/>
        </fo:marker>
        <xsl:value-of select="file"/>
      </fo:block>
      <xsl:text>Name</xsl:text>
    </fo:block>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Appref -->
  <xsl:template match="appref">
    <xsl:param name="partnum"/>

    <fo:block page-break-before="always" xsl:use-attribute-sets="module-header" id="{generate-id(app)}">
      <fo:block xsl:use-attribute-sets="module-name">
        <!-- Mark title for header printout -->
        <fo:marker marker-class-name="chapter-title">
          <xsl:value-of select="app"/>
        </fo:marker>
        <xsl:value-of select="app"/>
      </fo:block>
      <xsl:text>Application</xsl:text>
    </fo:block>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

 <!-- *ref Header -->
  <xsl:template match="erlref/header|comref/header|cref/header|fileref/header|appref/header">
    <fo:block>
      <xsl:text></xsl:text>
    </fo:block>
  </xsl:template>

  <!-- Modulesummary -->
  <xsl:template match="modulesummary">
    <xsl:param name="partnum"/>
    <fo:block>
      <xsl:text></xsl:text>
    </fo:block>
  </xsl:template>

  <!-- Description -->
  <xsl:template match="description">
    <xsl:param name="partnum"/>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Funcs -->
  <xsl:template match="funcs">
    <xsl:param name="partnum"/>
    <fo:block  xsl:use-attribute-sets="h3">
      <xsl:text>Exports</xsl:text>
    </fo:block>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>

  <!-- Func -->
  <xsl:template match="func">
    <xsl:param name="partnum"/>

    <xsl:apply-templates select="name"/>

    <xsl:apply-templates select="fsummary|type|desc">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>


  <xsl:template match="name">
    <xsl:param name="partnum"/>
    <xsl:choose>
      <!-- @arity is mandatory when referring to a specification -->
      <xsl:when test="string-length(@arity) > 0">
        <xsl:call-template name="spec_name"/>
      </xsl:when>
      <xsl:when test="ancestor::datatype">
        <xsl:call-template name="type_name"/>
      </xsl:when>
      <xsl:otherwise>
        <fo:block xsl:use-attribute-sets="function-name">
          <xsl:call-template name="name">
            <xsl:with-param name="partnum" select="$partnum"/>
          </xsl:call-template>
        </fo:block>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="name">
    <xsl:param name="partnum"/>
    <xsl:choose>
      <xsl:when test="ancestor::cref">
        <fo:block id="{generate-id(nametext)}">
          <xsl:value-of select="ret"/><xsl:text> </xsl:text><xsl:value-of select="nametext"/>
        </fo:block>
      </xsl:when>
      <xsl:otherwise>
        <fo:block id="{generate-id(.)}">
          <xsl:value-of select="."/>
        </fo:block>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- Type -->
  <xsl:template match="type">
    <xsl:param name="partnum"/>

    <fo:block>
      <xsl:text>Types:</xsl:text>
    </fo:block>

    <fo:list-block xsl:use-attribute-sets="type-listblock">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </fo:list-block>

  </xsl:template>


  <!-- V -->
  <xsl:template match="v">
    <xsl:param name="partnum"/>
    <fo:list-item xsl:use-attribute-sets="type-listitem">
      <fo:list-item-label end-indent="label-end()">
        <fo:block>
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()" format="justify">
        <fo:block font-weight="bold">
          <xsl:apply-templates>
            <xsl:with-param name="partnum" select="$partnum"/>
          </xsl:apply-templates>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>

  <!-- D -->
  <xsl:template match="d">
    <xsl:param name="partnum"/>
    <fo:list-item xsl:use-attribute-sets="type-listitem">
      <fo:list-item-label end-indent="label-end()"><fo:block></fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()" format="justify">
        <fo:block>
          <xsl:apply-templates>
            <xsl:with-param name="partnum" select="$partnum"/>
          </xsl:apply-templates>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>

  <!-- Desc -->
  <xsl:template match="desc">
    <xsl:param name="partnum"/>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>


  <!-- Fsummary -->
  <xsl:template match="fsummary">
    <!-- This tag is skipped for now. -->
  </xsl:template>


  <!-- Table -->
  <xsl:template match="table">
    <xsl:param name="chapnum"/>
    <xsl:variable name="tabnum">
      <xsl:number level="any" from="chapter" count="table"/>
    </xsl:variable>
    <fo:table xsl:use-attribute-sets="table">
    <fo:table-body>
      <xsl:apply-templates select="row">
        <xsl:with-param name="chapnum" select="$chapnum"/>
        <xsl:with-param name="tabnum" select="$tabnum"/>
      </xsl:apply-templates>
    </fo:table-body>
    </fo:table>
    <xsl:apply-templates select="tcaption">
      <xsl:with-param name="chapnum" select="$chapnum"/>
      <xsl:with-param name="tabnum" select="$tabnum"/>
    </xsl:apply-templates>
  </xsl:template>


  <xsl:template match="row">
    <fo:table-row>
      <xsl:apply-templates/>
    </fo:table-row>
  </xsl:template>

  <xsl:template match="cell">
    <fo:table-cell xsl:use-attribute-sets="table-cell">
      <xsl:call-template name="cell-valign"/>
      <fo:block>
        <xsl:call-template name="cell-align"/>
        <xsl:apply-templates/>
      </fo:block>
    </fo:table-cell>
  </xsl:template>

  <xsl:template match="tcaption">
    <xsl:param name="chapnum"/>
    <xsl:param name="tabnum"/>

      <fo:block xsl:use-attribute-sets="caption">
        Table
        <xsl:value-of select="$chapnum"/>.<xsl:value-of select="$tabnum"/>:
        &#160;
        <xsl:apply-templates/>
      </fo:block>
  </xsl:template>


  <!-- Image -->
  <xsl:template match="image">
    <xsl:param name="chapnum"/>
    <xsl:variable name="fignum">
      <xsl:number level="any" from="chapter" count="image"/>
    </xsl:variable>

    <fo:block xsl:use-attribute-sets="image">
      <fo:external-graphic content-width="60%" src="{@file}"/>

      <xsl:apply-templates>
        <xsl:with-param name="chapnum" select="$chapnum"/>
        <xsl:with-param name="fignum" select="$fignum"/>
      </xsl:apply-templates>

    </fo:block>

  </xsl:template>


  <xsl:template match="icaption">
    <xsl:param name="chapnum"/>
    <xsl:param name="fignum"/>

      <fo:block xsl:use-attribute-sets="caption">
        Figure
        <xsl:value-of select="$chapnum"/>.<xsl:value-of select="$fignum"/>:
        &#160;
        <xsl:apply-templates/>
      </fo:block>

  </xsl:template>


  <xsl:template match="input">
    <fo:inline font-weight="bold">
      <xsl:apply-templates select="text()"/>
    </fo:inline>
  </xsl:template>


  <xsl:template match="seealso">
    <fo:inline font-style="italic">
     <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>

  <xsl:template match="url">
    <fo:inline font-weight="bold">
      <fo:basic-link external-destination="href">
        <xsl:apply-templates/>
      </fo:basic-link>
    </fo:inline>
  </xsl:template>



 <!-- Special templates to calculate the arity of functions -->
  <xsl:template name="calc-arity">
    <xsl:param name="string"/>
    <xsl:param name="no-of-pars"/>

    <xsl:variable name="length">
      <xsl:value-of select="string-length($string)"/>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$length > 0">
        <xsl:call-template name="calc-arity">
          <xsl:with-param name="string" select="substring-after($string, ',')"/>
          <xsl:with-param name="no-of-pars" select="$no-of-pars+1"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$no-of-pars"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="remove-paren">
    <xsl:param name="string"/>

    <xsl:variable name="str1">
      <xsl:call-template name="remove-paren-1">
        <xsl:with-param name="string" select="$string"/>
        <xsl:with-param name="start">(</xsl:with-param>
        <xsl:with-param name="end">)</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="str2">
      <xsl:call-template name="remove-paren-1">
        <xsl:with-param name="string" select="$str1"/>
        <xsl:with-param name="start">{</xsl:with-param>
        <xsl:with-param name="end">}</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="str3">
      <xsl:call-template name="remove-paren-1">
        <xsl:with-param name="string" select="$str2"/>
        <xsl:with-param name="start">[</xsl:with-param>
        <xsl:with-param name="end">]</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>

    <xsl:value-of select="$str3"/>

  </xsl:template>


  <xsl:template name="remove-paren-1">
    <xsl:param name="string"/>
    <xsl:param name="start"/>
    <xsl:param name="end"/>

    <xsl:variable name="tmp1">
      <xsl:value-of select="substring-before($string, $start)"/>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="string-length($tmp1) > 0 or starts-with($string, $start)">
        <xsl:variable name="tmp2">
          <xsl:value-of select="substring-after($string, $end)"/>
        </xsl:variable>
        <xsl:variable name="retstring">
          <xsl:call-template name="remove-paren">
            <xsl:with-param name="string" select="$tmp2"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="concat(concat($tmp1, 'x'), $retstring)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$string"/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

</xsl:stylesheet>
