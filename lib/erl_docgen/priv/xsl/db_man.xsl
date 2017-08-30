<?xml version="1.0" encoding="utf-8"?>
<!--
     #
     # %CopyrightBegin%
     #
     # Copyright Ericsson AB 2009-2016. All Rights Reserved.
     #
     # Licensed under the Apache License, Version 2.0 (the "License");
     # you may not use this file except in compliance with the License.
     # You may obtain a copy of the License at
     #
     #     http://www.apache.org/licenses/LICENSE-2.0
     #
     # Unless required by applicable law or agreed to in writing, software
     # distributed under the License is distributed on an "AS IS" BASIS,
     # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     # See the License for the specific language governing permissions and
     # limitations under the License.
     #
     # %CopyrightEnd%

     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl">

  <xsl:preserve-space elements="code pre p"/>
  <xsl:strip-space elements="*"/>
  <xsl:output method="text" encoding="UTF-8" indent="no"/>

  <!-- Start of Dialyzer type/spec tags. See also the templates
        matching "name", "seealso" and "br"
  -->

  <!-- Note: specs data for *one* module (as opposed to html and pdf) -->
  <xsl:param name="specs_file" select="''"/>
  <xsl:variable name="i" select="document($specs_file)"></xsl:variable>

  <xsl:template name="err">
    <xsl:param name="f"/>
    <xsl:param name="m"/>
    <xsl:param name="n"/>
    <xsl:param name="a"/>
    <xsl:param name="s"/>
    <xsl:message terminate="yes">
  Error <xsl:if test="$f != ''">in <xsl:value-of select ="$f"/>:</xsl:if>
        <xsl:if test="$m != ''"><xsl:value-of select ="$m"/>:</xsl:if>
        <xsl:value-of select="$n"/>
        <xsl:if test="$a != ''">/<xsl:value-of
             select ="$a"/></xsl:if>: <xsl:value-of select="$s"/>
    </xsl:message>
  </xsl:template>

  <xsl:template name="find_spec">
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="mod" select="@mod"/>
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="arity" select="@arity"/>
    <xsl:variable name="clause_i" select="@clause_i"/>
    <xsl:variable name="spec0" select=
        "$i/module[@name=$curModule]/spec
             [name=$name and arity=$arity
              and (string-length($mod) = 0 or module = $mod)]"/>
    <xsl:variable name="spec" select="$spec0[string-length($clause_i) = 0
                                             or position() = $clause_i]"/>

    <xsl:if test="count($spec) != 1">
      <xsl:variable name="why">
        <xsl:choose>
          <xsl:when test="count($spec) > 1">ambiguous spec</xsl:when>
          <xsl:when test="count($spec) = 0">unknown spec</xsl:when>
        </xsl:choose>
      </xsl:variable>
      <xsl:call-template name="err">
        <xsl:with-param name="f" select="$curModule"/>
	<xsl:with-param name="m" select="$mod"/>
	<xsl:with-param name="n" select="$name"/>
	<xsl:with-param name="a" select="$arity"/>
        <xsl:with-param name="s" select="$why"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:copy-of select="$spec"/>
  </xsl:template>

  <xsl:template name="spec_name">
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="arity" select="@arity"/>
    <xsl:variable name="spec0">
      <xsl:call-template name="find_spec"/>
    </xsl:variable>
    <xsl:variable name="spec" select="exsl:node-set($spec0)/spec"/>

    <xsl:choose>
      <xsl:when test="ancestor::cref">
	<xsl:message terminate="yes">
          Error: did not expect a 'name' tag with name/arity attributes here!
	</xsl:message>
      </xsl:when>
      <xsl:when test="ancestor::erlref">
	<xsl:apply-templates select="$spec/contract/clause/head"/>
        <xsl:text>&#10;.br</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="head">
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
    <xsl:text>&#10;.fi</xsl:text>
  </xsl:template>

  <!-- The *last* <name name="..." arity=".."/> -->
  <xsl:template match="name" mode="types">
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="arity" select="@arity"/>
    <xsl:variable name="spec0">
      <xsl:call-template name="find_spec"/>
    </xsl:variable>
    <xsl:variable name="spec" select="exsl:node-set($spec0)/spec"/>
    <xsl:variable name="clause" select="$spec/contract/clause"/>

    <xsl:variable name="type_desc" select="../type_desc"/>
    <!-- $type is data types to be presented as guards ("local types") -->
    <xsl:variable name="type"
                  select="../type[string-length(@name) > 0
                                  or string-length(@variable) > 0]"/>
    <xsl:variable name="type_variables"
                  select ="$type[string-length(@variable) > 0]"/>
    <xsl:variable name="local_types"
                  select ="$type[string-length(@name) > 0]"/>
    <xsl:variable name="output_subtypes" select="count($type_variables) = 0"/>

    <!-- It is assumed there is no support for overloaded specs
         (there is no spec with more than one clause) -->
    <xsl:if test="count($clause/guard) > 0 or count($type) > 0">
      <xsl:text>&#10;.RS</xsl:text>
      <xsl:text>&#10;.LP</xsl:text>
      <xsl:text>&#10;Types:&#10;</xsl:text>
      <xsl:text>&#10;.RS 3</xsl:text>

        <xsl:choose>
          <xsl:when test="$output_subtypes">
            <xsl:call-template name="subtype">
              <xsl:with-param name="subtype" select="$clause/guard/subtype"/>
              <xsl:with-param name="type_desc" select="$type_desc"/>
              <xsl:with-param name="local_types" select="$local_types"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="type_variables">
              <xsl:with-param name="type_variables" select="$type_variables"/>
              <xsl:with-param name="type_desc" select="$type_desc"/>
              <xsl:with-param name="local_types" select="$local_types"/>
	      <xsl:with-param name="fname" select="$name"/>
	      <xsl:with-param name="arity" select="$arity"/>
            </xsl:call-template>

          </xsl:otherwise>
        </xsl:choose>

        <xsl:call-template name="local_type">
          <xsl:with-param name="type_desc" select="$type_desc"/>
          <xsl:with-param name="local_types" select="$local_types"/>
        </xsl:call-template>
        <xsl:text>&#10;.RE</xsl:text>

      <xsl:text>&#10;.RE</xsl:text>

    </xsl:if>
  </xsl:template>

  <!-- Handle <type variable="..." name_i="..."/> -->
  <xsl:template name="type_variables">
    <xsl:param name="type_variables"/>
    <xsl:param name="type_desc"/>
    <xsl:param name="local_types"/>
    <xsl:param name="fname"/>
    <xsl:param name="arity"/>

    <xsl:variable name="names" select="../name[string-length(@arity) > 0]"/>
    <xsl:for-each select="$type_variables">
      <xsl:variable name="name_i">
	<xsl:choose>
          <xsl:when test="string-length(@name_i) > 0">
	    <xsl:value-of select="@name_i"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="count($names)"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:variable name="spec0">
        <xsl:for-each select="$names[position() = $name_i]">
          <xsl:call-template name="find_spec"/>
        </xsl:for-each>
      </xsl:variable>
      <xsl:variable name="spec" select="exsl:node-set($spec0)/spec"/>
      <xsl:variable name="clause" select="$spec/contract/clause"/>
      <xsl:variable name="variable" select="@variable"/>
      <xsl:variable name="subtype"
                    select="$clause/guard/subtype[typename = $variable]"/>

      <xsl:if test="count($subtype) = 0">
	<xsl:call-template name="err">
          <xsl:with-param name="f" select="ancestor::erlref/module"/>
          <xsl:with-param name="n" select="$fname"/>
          <xsl:with-param name="a" select="$arity"/>
	  <xsl:with-param name="s">unknown type variable <xsl:value-of select="$variable"/>
          </xsl:with-param>
	</xsl:call-template>
      </xsl:if>

      <xsl:call-template name="subtype">
        <xsl:with-param name="subtype" select="$subtype"/>
        <xsl:with-param name="type_desc" select="$type_desc"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="subtype">
    <xsl:param name="subtype"/>
    <xsl:param name="type_desc"/>

    <xsl:for-each select="$subtype">
      <xsl:variable name="tname" select="typename"/>
      <xsl:variable name="string" select="string"/>
      <xsl:if test="string-length($string) > 0">
	<xsl:text>&#10;</xsl:text>
	<xsl:apply-templates select="$string"/>
	<xsl:text>&#10;.br</xsl:text>
	<xsl:apply-templates select="$type_desc[@variable = $tname]"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="local_type">
    <xsl:param name="type_desc"/>
    <xsl:param name="local_types"/>

    <xsl:for-each select ="$local_types">
      <xsl:text>&#10;</xsl:text>
      <xsl:call-template name="type_name">
	<xsl:with-param name="mode" select="'local_type'"/>
      </xsl:call-template>
      <xsl:text>&#10;.br</xsl:text>
      <xsl:variable name="tname" select="@name"/>
      <xsl:variable name="tnvars" select="@n_vars"/>
      <xsl:apply-templates select=
	 "$type_desc[@name = $tname
		     and (@n_vars = $tnvars
			  or string-length(@n_vars) = 0 and
			     string-length($tnvars) = 0)]"/>
    </xsl:for-each>
  </xsl:template>

  <!-- Note: <type_desc> has not been implemented for data types. -->

  <!-- Similar to <d> -->
  <xsl:template match="type_desc">
    <xsl:text>&#10;.RS 2&#10;</xsl:text><xsl:apply-templates/>
    <xsl:text>&#10;.RE</xsl:text>
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

  <xsl:template name="type_name">
    <xsl:param name="mode"/> <!-- '' if <datatype> -->
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="mod" select="@mod"/>
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="n_vars" select="@n_vars"/>

    <xsl:choose>
      <xsl:when test="string-length($name) > 0">
	<xsl:variable name="type" select=
	    "$i/module[@name=$curModule]/type
		 [name=$name
		  and (string-length($n_vars) = 0 or n_vars = $n_vars)
		  and (string-length($mod) = 0 or module = $mod)]"/>

	<xsl:if test="count($type) != 1">
	  <xsl:variable name="why">
	    <xsl:choose>
	      <xsl:when test="count($type) > 1">ambiguous type</xsl:when>
	      <xsl:when test="count($type) = 0">unknown type</xsl:when>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:call-template name="err">
	    <xsl:with-param name="f" select="$curModule"/>
	    <xsl:with-param name="m" select="$mod"/>
	    <xsl:with-param name="n" select="$name"/>
	    <xsl:with-param name="a" select="$n_vars"/>
	    <xsl:with-param name="s" select="$why"/>
	  </xsl:call-template>
	</xsl:if>
	<xsl:choose>
	  <xsl:when test="$mode = ''">
	    <xsl:apply-templates select="$type/typedecl"/>
	  </xsl:when>
	  <xsl:when test="$mode = 'local_type'">
	    <xsl:apply-templates select="$type/typedecl" mode="local_type"/>
	  </xsl:when>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise> <!-- <datatype> with <name> -->
        <xsl:text>&#10;.nf&#10;</xsl:text>
        <xsl:text>&#10;.B&#10;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>&#10;.br</xsl:text>
        <xsl:text>&#10;.fi</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="typehead">
    <xsl:text>&#10;.nf&#10;&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
    <xsl:text>&#10;.fi</xsl:text>
  </xsl:template>

  <xsl:template match="typehead" mode="local_type">
    <xsl:text>.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi</xsl:text>
  </xsl:template>

  <!-- Not used right now -->
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

  <!-- The name of data types -->
  <xsl:template match="marker">
    <xsl:if test="string-length(.) != 0">
      <xsl:text>\fB</xsl:text><xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- Used both in <datatype> and in <func>! -->
  <xsl:template match="anno">
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="anno" select="normalize-space(text())"/>
    <xsl:variable name="namespec"
                  select="ancestor::type_desc/preceding-sibling::name
                          | ancestor::desc/preceding-sibling::name"/>
    <xsl:if test="count($namespec) = 0 and string-length($specs_file) > 0">
      <xsl:call-template name="err">
        <xsl:with-param name="f" select="$curModule"/>
	<xsl:with-param name="s">cannot find tag 'name' (anno <xsl:value-of select="$anno"/>)
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>

    <!-- Search "local types" as well -->
    <xsl:variable name="local_types"
                select="ancestor::desc/preceding-sibling::type
	                       [string-length(@name) > 0]
                      | ancestor::type_desc/preceding-sibling::type
	                       [string-length(@name) > 0]"/>
    <xsl:variable name="has_anno_in_local_type">
      <xsl:for-each select="$local_types">
	<xsl:call-template name="anno_name">
	  <xsl:with-param name="curModule" select="$curModule"/>
	  <xsl:with-param name="anno" select="$anno"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:variable>

    <xsl:variable name="has_anno">
      <xsl:for-each select="$namespec">
	<xsl:call-template name="anno_name">
	  <xsl:with-param name="curModule" select="$curModule"/>
	  <xsl:with-param name="anno" select="$anno"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:variable>

    <xsl:if test="$has_anno = '' and $has_anno_in_local_type = ''">
      <xsl:call-template name="err">
        <xsl:with-param name="f" select="$curModule"/>
	<xsl:with-param name="m" select="$namespec/@mod"/>
	<xsl:with-param name="n" select="$namespec/@name"/>
	<xsl:with-param name="a" select="'-'"/>
	<xsl:with-param name="s">unknown annotation <xsl:value-of select="$anno"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:value-of select="$anno"/>
  </xsl:template>

  <xsl:template name="anno_name">
    <xsl:param name="curModule"/>
    <xsl:param name="anno"/>
    <xsl:variable name="mod" select="@mod"/>
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="arity" select="@arity"/>
    <xsl:variable name="n_vars" select="@n_vars"/>
    <xsl:variable name="clause_i" select="@clause_i"/>

    <xsl:variable name="spec0" select=
        "$i/module[@name=$curModule]/spec
             [name=$name and arity=$arity
              and (string-length($mod) = 0 or module = $mod)]"/>
    <xsl:variable name="spec_annos" select=
         "$spec0[string-length($clause_i) = 0
                 or position() = $clause_i]/anno[.=$anno]"/>
    <xsl:variable name="type_annos" select=
        "$i/module[@name=$curModule]/type
             [name=$name
              and (string-length($n_vars) = 0 or n_vars=$n_vars)
              and (string-length($mod) = 0 or module = $mod)]/anno[.=$anno]"/>
    <xsl:if test="count($spec_annos) != 0
                  or count($type_annos) != 0
                  or string-length($specs_file) = 0">
      <xsl:value-of select="true()"/>
    </xsl:if>
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
      <xsl:text>&#10;.SH "</xsl:text><xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="translate(title, 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
        <xsl:with-param name="replace" select="&quot;\&quot;" />
        <xsl:with-param name="with" select="&quot;\\\&quot;" />
      </xsl:call-template><xsl:text>"&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- *ref/Subsection -->
  <xsl:template match="section/section">
    <xsl:text>&#10;.SS "</xsl:text><xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="title" />
        <xsl:with-param name="replace" select="&quot;\&quot;" />
        <xsl:with-param name="with" select="&quot;\\\&quot;" />
      </xsl:call-template><xsl:text>"&#10;</xsl:text>
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
    <xsl:text>&#10;.LP</xsl:text>
  </xsl:template>

  <xsl:template match="taglist">
    <xsl:text>&#10;.RS 2</xsl:text>
    <xsl:apply-templates select="tag|item"/>
    <xsl:text>&#10;.RE</xsl:text>
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
        <xsl:text>&#10;.LP&#10;</xsl:text>
        <xsl:value-of select="$content"/>
        <xsl:text>&#10;.RE</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Note -->
  <xsl:template match="note">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>&#10;.RS -4</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:text>Note:</xsl:text>
    <xsl:text>&#10;.RE</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Warning -->
  <xsl:template match="warning">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>&#10;.RS -4</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:text>Warning:</xsl:text>
    <xsl:text>&#10;.RE</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Do -->
  <xsl:template match="do">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>&#10;.RS -4</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:text>Do:</xsl:text>
    <xsl:text>&#10;.RE</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Dont -->
  <xsl:template match="dont">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>&#10;.RS -4</xsl:text>
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:text>Dont:</xsl:text>
    <xsl:text>&#10;.RE</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="warning/p | note/p | dont/p | do/p">
    <xsl:variable name="content">
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="position() = 1">
        <xsl:value-of select="$content"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>&#10;.LP</xsl:text>
        <xsl:value-of select="$content"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

 <!-- Paragraph -->
  <xsl:template match="p">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Inline elements -->
  <xsl:template match="i">
    <xsl:text>\fI</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\fR\&amp; </xsl:text>
  </xsl:template>

  <xsl:template match="br">
    <xsl:choose>
      <xsl:when test="ancestor::head">
        <!-- The header of Dialyzer specs.
             .B makes next line appear in bold face -->
        <xsl:text>&#10;.B&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>&#10;.br&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="c">
    <xsl:text>\fI</xsl:text><xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="em">
    <xsl:text>\fI</xsl:text> <xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="strong">
    <xsl:text>\fB</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\fR\&amp; </xsl:text>
  </xsl:template>

  <xsl:template match="seealso">
    <xsl:choose>
      <xsl:when test="ancestor::head">
	<!-- The header of Dialyzer specs -->
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>\fB</xsl:text><xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Code -->
  <xsl:template match="code">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi</xsl:text>
  </xsl:template>

  <!-- Pre -->
  <xsl:template match="pre">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:text>.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi</xsl:text>
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
    <xsl:apply-templates
        select="name[string-length(@arity) > 0 and position()=last()]"
        mode="types"/>
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
      <xsl:when test="string-length(text()) = 0 and ancestor::erlref">
	<xsl:message terminate="yes">
          Error <xsl:value-of select="@name"/>: arity is mandatory when referring to specifications!
	</xsl:message>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="name">
    <xsl:text>&#10;.B&#10;</xsl:text>
    <xsl:choose>
      <xsl:when test="ancestor::cref">
        <xsl:value-of select="ret"/>
        <xsl:call-template name="maybe-space-after-ret">
          <xsl:with-param name="s" select="ret"/>
        </xsl:call-template>
        <xsl:value-of select="nametext"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#10;.br</xsl:text>
  </xsl:template>

  <xsl:template name="maybe-space-after-ret">
    <xsl:param name="s"/>
    <xsl:variable name="last_char"
	          select="substring($s, string-length($s), 1)"/>
    <xsl:choose>
      <xsl:when test="$last_char != '*'">
        <xsl:text> </xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <!-- Type -->
  <xsl:template match="type">
    <!-- The case where @name != 0 is taken care of in "type_name" -->
    <xsl:if test="string-length(@name) = 0 and string-length(@variable) = 0">
      <xsl:text>&#10;.RS</xsl:text>
      <xsl:text>&#10;.LP</xsl:text>
      <xsl:text>&#10;Types:&#10;</xsl:text>
      <xsl:text>&#10;.RS 3</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&#10;.RE</xsl:text>
      <xsl:text>&#10;.RE</xsl:text>
    </xsl:if>
  </xsl:template>


  <!-- V -->
  <xsl:template match="v">
    <xsl:text>&#10;</xsl:text><xsl:apply-templates/>
    <xsl:text>&#10;.br</xsl:text>
  </xsl:template>

  <!-- D -->
  <xsl:template match="d">
    <xsl:text>&#10;.RS 2&#10;</xsl:text><xsl:apply-templates/>
    <xsl:text>&#10;.RE</xsl:text>
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

  <!-- Do not normalize any text within pre and code tags. -->
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
      <xsl:value-of select="normalize-space()"/>
    </xsl:variable>
    <!-- 'C' is just any character but whitespace -->
    <xsl:variable name="tmp" select="normalize-space(concat('C',.,'C'))"/>
    <xsl:variable name="space_before">
      <xsl:choose>
         <!-- '<p>A<marker id="swamp"/> swamp</p>' does not work; instead:
              '<p>A <marker id="swamp"/>swamp</p>' -->
         <xsl:when test="starts-with($tmp, 'C ')
                and not (string(preceding-sibling::*[position()=1]) = ''
                         and parent::p)">
           <!-- and not (position() = 1 and parent::p)"> -->
           <xsl:text> </xsl:text>
         </xsl:when>
         <xsl:otherwise>
           <xsl:text/>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="space_after">
      <xsl:choose>
         <xsl:when test="substring($tmp, string-length($tmp)-1,1) = ' '
                         and $startstring != ''
                         and not (position() = last() and parent::p)">
           <xsl:text> </xsl:text>
         </xsl:when>
         <xsl:otherwise>
           <xsl:text/>
         </xsl:otherwise>
      </xsl:choose>
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
    <xsl:variable name="reply">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$rep2" />
        <xsl:with-param name="replace" select="&quot;.&quot;" />
        <xsl:with-param name="with" select="&quot;\&amp;.&quot;" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$space_before"/>
    <xsl:value-of select="$reply"/>
    <xsl:value-of select="$space_after"/>
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
