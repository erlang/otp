<?xml version="1.0" encoding="utf-8"?>
<!--
     #
     # %CopyrightBegin%
     #
     # Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:preserve-space elements="code pre"/>
  <xsl:strip-space elements="*"/>
  <xsl:output method="text" encoding="UTF-8" indent="no"/>

  <!-- Start of Dialyzer type/spec tags. See also the template matching "name"
  -->

  <!-- Note: specs data for *one* module (as opposed to html and pdf) -->
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
        "$i/module[@name=$curModule]/spec
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
        <xsl:text>&#10;.br</xsl:text>
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
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
    <xsl:text>&#10;.fi</xsl:text>
  </xsl:template>

  <xsl:template name="guard">
    <xsl:param name="guard"/>
    <xsl:param name="type_desc"/>
    <xsl:text>&#10;.RS</xsl:text>
    <xsl:text>&#10;.TP</xsl:text>
    <xsl:text>&#10;Types</xsl:text>
    <xsl:call-template name="subtype">
      <xsl:with-param name="subtype" select="$guard/subtype"/>
      <xsl:with-param name="type_desc" select="$type_desc"/>
    </xsl:call-template>
    <xsl:text>&#10;.RE</xsl:text>
  </xsl:template>

  <xsl:template name="subtype">
    <xsl:param name="subtype"/>
    <xsl:param name="type_desc"/>
    <xsl:for-each select="$subtype">
      <xsl:variable name="tname" select="typename"/>
      <xsl:variable name="tdesc" select="$type_desc[@name = $tname]"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:apply-templates select="string"/>
        <xsl:text>&#10;.br</xsl:text>
      <xsl:apply-templates select="$type_desc[@name = $tname]"/>
    </xsl:for-each>
  </xsl:template>

  <!-- Note: <type_desc> has not been implemented for data types. -->

  <!-- Similar to <d> -->
  <xsl:template match="type_desc">
    <xsl:text>&#10;</xsl:text><xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
  </xsl:template>

  <!-- Datatypes -->
  <xsl:template match="datatypes">
    <xsl:text>&#10;.SH DATA TYPES</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Datatype -->
  <xsl:template match="datatype">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="typehead">
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
    <xsl:text>&#10;.fi</xsl:text>
  </xsl:template>

  <xsl:template match="local_defs">
    <xsl:text>&#10;.RS</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.RE</xsl:text>
  </xsl:template>

  <xsl:template match="local_def">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
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
	    "$i/module[@name=$curModule]/type
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
        <xsl:text>&#10;.nf&#10;</xsl:text>
        <xsl:text>&#10;.B&#10;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>&#10;.br</xsl:text>
        <xsl:text>&#10;.fi</xsl:text>
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
        "$i/module[@name=$curModule]/spec
             [name=$name and arity=$arity
              and (string-length($mod) = 0 or module = $mod)]"/>
    <xsl:variable name="spec_annos" select=
         "$spec0[string-length($clause) = 0
                 or position() = $clause]/anno[.=$anno]"/>
    <xsl:variable name="type_annos" select=
        "$i/module[@name=$curModule]/type
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
    <xsl:text> </xsl:text>
  </xsl:template>

  <!-- End of Dialyzer type/spec tags -->

  <!-- Header -->
  <xsl:template match="header">
  </xsl:template>

  <!-- Section/Title -->
  <xsl:template match="section/title">
  </xsl:template>

  <!-- *ref/Section -->
  <xsl:template match="erlref/section|comref/section|cref/section|fileref/section|appref/section">
    <xsl:text>&#10;.SH "</xsl:text><xsl:value-of select="translate(title, 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/><xsl:text>"&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- *ref/Subsection -->
  <xsl:template match="section/section">
    <xsl:text>&#10;.SS "</xsl:text><xsl:value-of select="title"/><xsl:text>"&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>


  <!-- Lists -->

  <xsl:template match="list">
    <xsl:text>&#10;.RS 2</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.RE&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="list/item">
    <xsl:text>&#10;.TP 2&#10;</xsl:text>
    <xsl:text>*&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.LP&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="taglist">
    <xsl:text>&#10;.RS 2</xsl:text>
    <xsl:apply-templates select="tag|item"/>
    <xsl:text>&#10;.RE&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="taglist/tag">
    <xsl:text>&#10;.TP 2&#10;</xsl:text>
    <xsl:text>.B&#10;</xsl:text>
    <xsl:apply-templates/><xsl:text>:&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="taglist/item">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="item/p">
    <xsl:variable name="content">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="position() = 1">
        <xsl:value-of select="$content"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>&#10;.RS 2</xsl:text>
        <xsl:text>&#10;.LP&#10;&#10;.LP&#10;</xsl:text>
        <xsl:value-of select="$content"/>
        <xsl:text>&#10;.RE</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Note -->
  <xsl:template match="note">
    <xsl:text>&#10;.SS Note:</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Warning -->
  <xsl:template match="warning">
    <xsl:text>&#10;.SS Warning:</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

 <!-- Paragraph -->
  <xsl:template match="p">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Inline elements -->
  <xsl:template match="b">
    <xsl:text>\fB</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\fR\&amp; </xsl:text>
  </xsl:template>

  <xsl:template match="br">
    <xsl:text>&#10;.br&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="c">
    <xsl:text>\fI</xsl:text><xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="em">
    <xsl:text>\fI</xsl:text> <xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="seealso">
    <xsl:text>\fB</xsl:text><xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <!-- Code -->
  <xsl:template match="code">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi&#10;</xsl:text>
  </xsl:template>

  <!-- Pre -->
  <xsl:template match="pre">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi&#10;</xsl:text>
  </xsl:template>


  <!-- Table -->
  <xsl:template match="table">
  </xsl:template>

 <!-- Image -->
  <xsl:template match="image">
  </xsl:template>


  <!-- Reference Manual -->

  <!-- Application -->
  <xsl:template match="application">
      <xsl:apply-templates/>
  </xsl:template>

  <!-- Erlref -->
  <xsl:template match="/erlref">
    <xsl:variable name="companyname">
      <xsl:choose>
        <!-- Workaround until all of OTP's .../holder contents are correct.  -->
        <xsl:when test="starts-with(header/copyright/holder,'Ericsson AB')"><xsl:text>Ericsson AB</xsl:text></xsl:when>
        <xsl:otherwise><xsl:value-of select="header/copyright/holder"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:text>.TH </xsl:text><xsl:value-of select="module"/><xsl:text> 3 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "</xsl:text><xsl:value-of select="$companyname"/><xsl:text>" "Erlang Module Definition"&#10;</xsl:text>
    <xsl:text>.SH NAME&#10;</xsl:text>
    <xsl:value-of select="module"/><xsl:text> \- </xsl:text><xsl:value-of select="modulesummary"/><xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Comref -->
  <xsl:template match="/comref">
    <xsl:variable name="companyname">
      <xsl:choose>
        <!-- Workaround until all of OTP's .../holder contents are correct.  -->
        <xsl:when test="starts-with(header/copyright/holder,'Ericsson AB')"><xsl:text>Ericsson AB</xsl:text></xsl:when>
        <xsl:otherwise><xsl:value-of select="header/copyright/holder"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
   <xsl:text>.TH </xsl:text><xsl:value-of select="com"/><xsl:text> 1 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "</xsl:text><xsl:value-of select="$companyname"/><xsl:text>" "User Commands"&#10;</xsl:text>
    <xsl:text>.SH NAME&#10;</xsl:text>
    <xsl:value-of select="com"/><xsl:text> \- </xsl:text><xsl:value-of select="comsummary"/><xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Cref -->
  <xsl:template match="/cref">
    <xsl:variable name="companyname">
      <xsl:choose>
        <!-- Workaround until all of OTP's .../holder contents are correct.  -->
        <xsl:when test="starts-with(header/copyright/holder,'Ericsson AB')"><xsl:text>Ericsson AB</xsl:text></xsl:when>
        <xsl:otherwise><xsl:value-of select="header/copyright/holder"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>.TH </xsl:text><xsl:value-of select="lib"/><xsl:text> 3 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "</xsl:text><xsl:value-of select="$companyname"/><xsl:text>" "C Library Functions"&#10;</xsl:text>
    <xsl:text>.SH NAME&#10;</xsl:text>
    <xsl:value-of select="lib"/><xsl:text> \- </xsl:text><xsl:value-of select="libsummary"/><xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Fileref -->
  <xsl:template match="/fileref">
    <xsl:variable name="companyname">
      <xsl:choose>
        <!-- Workaround until all of OTP's .../holder contents are correct.  -->
        <xsl:when test="starts-with(header/copyright/holder,'Ericsson AB')"><xsl:text>Ericsson AB</xsl:text></xsl:when>
        <xsl:otherwise><xsl:value-of select="header/copyright/holder"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>.TH </xsl:text><xsl:value-of select="file"/><xsl:text> 5 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "</xsl:text><xsl:value-of select="$companyname"/><xsl:text>" "Files"&#10;</xsl:text>
    <xsl:text>.SH NAME&#10;</xsl:text>
    <xsl:value-of select="file"/><xsl:text> \- </xsl:text><xsl:value-of select="filesummary"/><xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Appref -->
  <xsl:template match="/appref">
    <xsl:variable name="companyname">
      <xsl:choose>
        <!-- Workaround until all of OTP's .../holder contents are correct.  -->
        <xsl:when test="starts-with(header/copyright/holder,'Ericsson AB')"><xsl:text>Ericsson AB</xsl:text></xsl:when>
        <xsl:otherwise><xsl:value-of select="header/copyright/holder"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>.TH </xsl:text><xsl:value-of select="app"/><xsl:text> 7 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "</xsl:text><xsl:value-of select="$companyname"/><xsl:text>" "Erlang Application Definition"&#10;</xsl:text>
    <xsl:text>.SH NAME&#10;</xsl:text>
    <xsl:value-of select="app"/><xsl:text> \- </xsl:text><xsl:value-of select="appsummary"/><xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Module|Com|Lib|File|App-->
  <xsl:template match="module|com|lib|file|app">
  </xsl:template>

  <!-- Modulesummary|Comsummary|Libsummary|Filesummary|Appsummary -->
  <xsl:template match="modulesummary|comsummary|libsummary|filesummary|appsummary">
  </xsl:template>

  <!-- Description -->
  <xsl:template match="description">
    <xsl:text>.SH DESCRIPTION</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Funcs -->
  <xsl:template match="funcs">
    <xsl:text>&#10;.SH EXPORTS</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Func -->
  <xsl:template match="func">
    <xsl:text>&#10;.LP</xsl:text>
    <xsl:apply-templates select="name"/>
    <xsl:apply-templates select="fsummary|type|desc"/>
  </xsl:template>

  <xsl:template match="name">
    <xsl:choose>
      <!-- @arity is mandatory when referring to a specification -->
      <xsl:when test="string-length(@arity) > 0">
        <xsl:call-template name="spec_name"/>
      </xsl:when>
      <xsl:when test="ancestor::datatype">
        <xsl:call-template name="type_name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="name">
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
  </xsl:template>


  <!-- Type -->
  <xsl:template match="type">
    <xsl:text>&#10;.RS</xsl:text>
    <xsl:text>&#10;.TP</xsl:text>
    <xsl:text>&#10;Types</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.RE</xsl:text>
  </xsl:template>


  <!-- V -->
  <xsl:template match="v">
    <xsl:text>&#10;</xsl:text><xsl:value-of select="normalize-space(text())"/>
    <xsl:text>&#10;.br</xsl:text>
  </xsl:template>

  <!-- D -->
  <xsl:template match="d">
    <xsl:text>&#10;</xsl:text><xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
  </xsl:template>

  <!-- Desc -->
  <xsl:template match="desc">
    <xsl:text>&#10;.RS</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.RE</xsl:text>
  </xsl:template>


  <!-- Fsummary -->
  <xsl:template match="fsummary">
    <!-- This tag is skipped for now. -->
  </xsl:template>


  <!-- Authors -->
  <xsl:template match="authors">
    <xsl:text>&#10;.SH AUTHORS</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Aname -->
  <xsl:template match="authors/aname">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Email -->
  <xsl:template match="authors/email">
    <xsl:text>&#10;.I&#10;&lt;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>

  <!-- Do not noramlize any text within pre and code tags. -->
  <xsl:template match="pre/text()">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="." />
        <xsl:with-param name="replace" select="&quot;\&quot;" />
        <xsl:with-param name="with" select="&quot;\\&quot;" />
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="code/text()">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="." />
        <xsl:with-param name="replace" select="&quot;\&quot;" />
        <xsl:with-param name="with" select="&quot;\\&quot;" />
      </xsl:call-template>
  </xsl:template>

  <!-- Replace ' by \&' ans . by \&. -->
  <xsl:template match="text()">
    <xsl:variable name="startstring">
      <xsl:value-of select="normalize-space()"/><xsl:text> </xsl:text>
    </xsl:variable>
    <xsl:variable name="rep1">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$startstring" />
        <xsl:with-param name="replace" select="&quot;\&quot;" />
        <xsl:with-param name="with" select="&quot;\\&quot;" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="rep2">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$rep1" />
        <xsl:with-param name="replace" select="&quot;&apos;&quot;" />
        <xsl:with-param name="with" select="&quot;\&amp;&apos;&quot;" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="$rep2" />
      <xsl:with-param name="replace" select="&quot;.&quot;" />
      <xsl:with-param name="with" select="&quot;\&amp;.&quot;" />
    </xsl:call-template>
  </xsl:template>

  <!-- Template replace-string is borrowed at http://www.dpawson.co.uk/xsl/sect2/replace.html -->
  <xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="replace"/>
    <xsl:param name="with"/>
    <xsl:choose>
      <xsl:when test="contains($text,$replace)">
        <xsl:value-of select="substring-before($text,$replace)"/>
        <xsl:value-of select="$with"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text" select="substring-after($text,$replace)"/>
          <xsl:with-param name="replace" select="$replace"/>
          <xsl:with-param name="with" select="$with"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
