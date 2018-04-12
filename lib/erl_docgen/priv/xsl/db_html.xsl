<?xml version="1.0" encoding="utf-8"?>
<!--
     #
     # %CopyrightBegin%
     #
     # Copyright Ericsson AB 2009-2017. All Rights Reserved.
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
  xmlns:func="http://exslt.org/functions"
  xmlns:erl="http://erlang.org"
  extension-element-prefixes="exsl func"
  xmlns:fn="http://www.w3.org/2005/02/xpath-functions">

  <xsl:include href="db_html_params.xsl"/>
  <xsl:include href="db_funcs.xsl"/>

  <func:function name="erl:flip_first_char">
    <xsl:param name="in"/>

    <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
    <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'"/>

    <xsl:variable name="first-char" select="substring($in, 1, 1)"/>

    <xsl:variable name="result">
      <xsl:choose>
        <xsl:when test="contains($uppercase, $first-char)">
          <xsl:value-of select="concat(translate($first-char, $uppercase, $lowercase), substring($in, 2))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat(translate($first-char, $lowercase, $uppercase), substring($in, 2))"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <func:result select="$result"/>
  </func:function>

  <func:function name="erl:lower-case">
    <xsl:param name="str"/>

    <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
    <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'"/>

    <xsl:variable name="result">
      <xsl:value-of select="translate($str, $uppercase, $lowercase)"/>
    </xsl:variable>

    <func:result select="$result"/>
  </func:function>

  <func:function name="erl:to-link">
    <xsl:param name="text"/>
    <func:result select="translate(erl:lower-case($text),': /()&quot;&#10;','-------')"/>
  </func:function>

  <!-- Used from template menu.funcs to sort a module's functions for the lefthand index list,
       from the module's .xml file. Returns a value on which to sort the entity in question
       (a <name> element).

       Some functions are listed with the name as an attribute, as in string.xml:
       <name name="join" arity="2"/>       

       Others use the element value for the name, as in gen_server.xml:
       <name>start_link(Module, Args, Options) -> Result</name>

       Additionally, callbacks may be included, as in gen_server.xml:
       <name>Module:handle_call(Request, From, State) -> Result</name>

       For C reference pages the name tag has a substructure where the nametext tag
       is used in the sort, as in erl_nif.xml
       <name><ret>void *</ret><nametext>enif_alloc(size_t size)</nametext></name>

       So first, get the name from either the attribute or the element value.
       Then, reverse the case of the first character. This is because xsltproc, used for processing,
       orders uppercase before lowercase (even when the 'case-order="lower-first"' option
       is given). But we want the Module callback functions listed after a module's regular
       functions, as they are now. This doesn't affect the actual value used in the output, but
       just the value used as a sort key. To then ensure that uppercase is indeed sorted before
       lower, as we now want it to be, the 'case-order="upper-first"' option is used.

       This processing only affect the lefthand index list- the body of the doc page is not 
       affected.
  -->
  <func:function name="erl:get_sort_field">
    <xsl:param name="elem"/>

    <xsl:variable name="base">
      <xsl:choose>
	<xsl:when test="ancestor::cref">
	  <xsl:value-of select="$elem/nametext"/>
	</xsl:when>
	<xsl:otherwise>       
	  <xsl:choose>
            <xsl:when test="string-length($elem/@name) > 0">
              <xsl:value-of select="$elem/@name"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="substring-before($elem, '(')"/>
            </xsl:otherwise>
	  </xsl:choose>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <func:result select="erl:flip_first_char($base)"/>
  </func:function>

  <!-- Start of Dialyzer type/spec tags.
       See also the templates matching "name" and "seealso" as well as
       the template "menu.funcs"
  -->

  <xsl:param name="specs_file" select="''"/>
  <xsl:variable name="i" select="document($specs_file)"></xsl:variable>

  <xsl:param name="mod2app_file" select="''"/>
  <xsl:variable name="m2a" select="document($mod2app_file)"></xsl:variable>
  <xsl:key name="mod2app" match="module" use="@name"/>

  <xsl:key
        name="mfa"
	match="func/name[string-length(@arity) > 0 and ancestor::erlref]"
	use="concat(ancestor::erlref/module,':',@name, '/', @arity)"/>

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
        "$i/specs/module[@name=$curModule]/spec
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
    <xsl:variable name="anchor" select="@anchor"/>
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
        <!-- Do not to use preceding since it is very slow! -->
        <xsl:variable name="curModule" select="ancestor::erlref/module"/>
        <xsl:variable name="mfas"
                      select="key('mfa',
                                  concat($curModule,':',$name,'/',$arity))"/>
	<xsl:choose>
          <xsl:when test="generate-id($mfas[1]) != generate-id(.)">
	    <!-- Avoid duplicated anchors. See also menu.funcs. -->
	  </xsl:when>
	  <xsl:otherwise>
	    <a name="{$name}-{$arity}"></a>
	  </xsl:otherwise>
        </xsl:choose>

	<!-- Insert an anchor for "anchor" attribute -->
	<xsl:if test="string-length($anchor) > 0">
	      <a name="{$anchor}"></a>
	</xsl:if>

        <xsl:variable name="global_types" select="ancestor::erlref/datatypes"/>
	<xsl:variable name="local_types"
		      select="../type[string-length(@name) > 0]"/>
	<xsl:apply-templates select="$spec/contract/clause/head">
          <xsl:with-param name="ghlink" select="ancestor-or-self::*[@ghlink]/@ghlink"/>
	  <xsl:with-param name="local_types" select="$local_types"/>
	  <xsl:with-param name="global_types" select="$global_types"/>
	</xsl:apply-templates>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="head">
    <xsl:param name="ghlink"/>
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>
    <xsl:variable name="id" select="concat(concat(concat(concat(../../../name,'-'),../../../arity),'-'),generate-id(.))"/>
    <div class="bold_code func-head"
         onMouseOver="document.getElementById('ghlink-{$id}').style.visibility = 'visible';"
         onMouseOut="document.getElementById('ghlink-{$id}').style.visibility = 'hidden';">
      <xsl:call-template name="ghlink">
        <xsl:with-param name="ghlink" select="$ghlink"/>
        <xsl:with-param name="id" select="$id"/>
      </xsl:call-template>
      <xsl:apply-templates mode="local_type">
        <xsl:with-param name="local_types" select="$local_types"/>
        <xsl:with-param name="global_types" select="$global_types"/>
      </xsl:apply-templates>
    </div>
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

    <xsl:variable name="global_types" select="ancestor::erlref/datatypes"/>
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
        <div class="REFBODY fun-types">
            <h3 class="func-types-title">Types</h3>

        <xsl:choose>
          <xsl:when test="$output_subtypes">
            <xsl:call-template name="subtype">
              <xsl:with-param name="subtype" select="$clause/guard/subtype"/>
              <xsl:with-param name="type_desc" select="$type_desc"/>
              <xsl:with-param name="local_types" select="$local_types"/>
              <xsl:with-param name="global_types" select="$global_types"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="type_variables">
              <xsl:with-param name="type_variables" select="$type_variables"/>
              <xsl:with-param name="type_desc" select="$type_desc"/>
              <xsl:with-param name="local_types" select="$local_types"/>
              <xsl:with-param name="global_types" select="$global_types"/>
	      <xsl:with-param name="fname" select="$name"/>
	      <xsl:with-param name="arity" select="$arity"/>
            </xsl:call-template>

          </xsl:otherwise>
        </xsl:choose>

        <xsl:call-template name="local_type">
          <xsl:with-param name="type_desc" select="$type_desc"/>
          <xsl:with-param name="local_types" select="$local_types"/>
          <xsl:with-param name="global_types" select="$global_types"/>
        </xsl:call-template>
      </div>

    </xsl:if>
  </xsl:template>

  <!-- Handle <type variable="..." name_i="..."/> -->
  <xsl:template name="type_variables">
    <xsl:param name="type_variables"/>
    <xsl:param name="type_desc"/>
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>
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
        <xsl:with-param name="local_types" select="$local_types"/>
        <xsl:with-param name="global_types" select="$global_types"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="subtype">
    <xsl:param name="subtype"/>
    <xsl:param name="type_desc"/>
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>

    <xsl:for-each select="$subtype">
      <xsl:variable name="tname" select="typename"/>
      <div class="REFTYPES rt-1">
          <span class="bold_code bc-2">
              <xsl:apply-templates select="string" mode="local_type">
                  <xsl:with-param name="local_types" select="$local_types"/>
                  <xsl:with-param name="global_types" select="$global_types"/>
              </xsl:apply-templates>
          </span>
      </div>
      <xsl:apply-templates select="$type_desc[@variable = $tname]"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="local_type">
    <xsl:param name="type_desc"/>
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>

    <xsl:for-each select="$local_types">
      <div class="REFTYPES rt-2">
	<xsl:call-template name="type_name">
	  <xsl:with-param name="mode" select="'local_type'"/>
	  <xsl:with-param name="local_types" select="$local_types"/>
	  <xsl:with-param name="global_types" select="$global_types"/>
	</xsl:call-template>
      </div>
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
    <div class="REFBODY rb-1">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <!-- This is for debugging. All modules! -->
  <xsl:template match="all_etypes">
    <xsl:for-each select= "$i//type">
      <pre>
	<span class="bold_code bc-3">
	  <xsl:apply-templates select="typedecl"/>
	</span><xsl:text>
</xsl:text>
      </pre>
    </xsl:for-each>
  </xsl:template>

  <!-- Datatypes -->
  <xsl:template match="datatypes">
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Data Types</xsl:with-param>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Datatype Title, is the really needed? not used by anything -->
  <xsl:template match="datatype_title">
    <xsl:variable name="title" select="."/>
    <h4>
      <xsl:call-template name="title_link">
        <xsl:with-param name="title"><xsl:apply-templates/></xsl:with-param>
        <xsl:with-param name="link" select="$title"/>
      </xsl:call-template>
    </h4>
  </xsl:template>

  <!-- Datatype -->
  <xsl:template match="datatype">
    <xsl:variable name="id" select="concat('type-',name/@name)"/>
    <div class="data-types-body">
      <div class="data-type-name"
           onMouseOver="document.getElementById('ghlink-{$id}').style.visibility = 'visible';"
           onMouseOut="document.getElementById('ghlink-{$id}').style.visibility = 'hidden';">
        <xsl:call-template name="ghlink">
          <xsl:with-param name="id" select="$id"/>
        </xsl:call-template>
        <xsl:apply-templates select="name"/>
      </div>
      <div class="data-type-desc"><xsl:apply-templates select="desc"/></div>
    </div>
  </xsl:template>

  <!-- The "mode" attribute of apply has been used to separate the case
       when datatypes are copied into specifications' subtypes.
       A local type has no anchor. There are no links to local types
       from local types or guards/head of the same specification.
  -->

  <xsl:template name="type_name">
    <xsl:param name="mode"/> <!-- '' if <datatype> -->
    <xsl:param name="local_types" select="/.."/>
    <xsl:param name="global_types" select="/.."/>
    <xsl:variable name="curModule" select="ancestor::erlref/module"/>
    <xsl:variable name="mod" select="@mod"/>
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="n_vars" select="@n_vars"/>

    <xsl:choose>
      <xsl:when test="string-length($name) > 0">
	<xsl:variable name="type" select=
	    "$i/specs/module[@name=$curModule]/type
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
	    <xsl:apply-templates select="$type/typedecl" mode="local_type">
	      <xsl:with-param name="local_types" select="$local_types"/>
	      <xsl:with-param name="global_types" select="$global_types"/>
	    </xsl:apply-templates>
	  </xsl:when>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise> <!-- <datatype> with <name> -->
        <xsl:call-template name="name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="typehead">
    <span class="bold_code bc-4">
      <xsl:apply-templates/>
    </span><br/>
  </xsl:template>

  <xsl:template match="typehead" mode="local_type">
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>
    <span class="bold_code bc-5">
    <xsl:apply-templates mode="local_type">
      <xsl:with-param name="local_types" select="$local_types"/>
      <xsl:with-param name="global_types" select="$global_types"/>
    </xsl:apply-templates>
    </span><br/>
  </xsl:template>

  <!-- Not used right now -->
  <!-- local_defs -->
  <xsl:template match="local_defs">
    <div class="REFBODY rb-2">
      <xsl:apply-templates>
      </xsl:apply-templates>
    </div>
  </xsl:template>

  <!-- Not used right now -->
  <xsl:template match="local_def">
    <div class="REFTYPES rt-3">
      <span class="bold_code bc-6">
        <xsl:apply-templates/>
      </span>
    </div>
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
        "$i/specs/module[@name=$curModule]/spec
             [name=$name and arity=$arity
              and (string-length($mod) = 0 or module = $mod)]"/>
    <xsl:variable name="spec_annos" select=
         "$spec0[string-length($clause_i) = 0
                 or position() = $clause_i]/anno[.=$anno]"/>
    <xsl:variable name="type_annos" select=
        "$i/specs/module[@name=$curModule]/type
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
    <xsl:text>&#160;</xsl:text>
  </xsl:template>

  <xsl:template match="nbsp" mode="local_type">
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template match="br" mode="local_type">
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template match="marker" mode="local_type">
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>
    <!-- Craete no anchor -->
    <!-- It would be possible to create a link to the global type
         (if there is one), but that would mean even more code...
    -->
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Does not look at @n_vars -->
  <xsl:template match="seealso" mode="local_type">
    <xsl:param name="local_types"/>
    <xsl:param name="global_types"/>

    <xsl:variable name="filepart"><xsl:value-of select="substring-before(@marker, '#')"/></xsl:variable>
    <xsl:variable name="linkpart"><xsl:value-of select="translate(substring-after(@marker, '#'), '/', '-')"/></xsl:variable>

    <xsl:choose>
      <xsl:when test="string-length($filepart) > 0">
        <xsl:call-template name="seealso"/>
      </xsl:when>
      <xsl:when test="count($local_types[concat('type-', @name) = $linkpart]) = 0">
        <xsl:call-template name="seealso"/>
      </xsl:when>
      <xsl:when test="count($global_types/datatype/name[concat('type-', @name) = $linkpart]) > 0">
        <!-- The type is both local and global; link to the global type -->
        <xsl:call-template name="seealso"/>
      </xsl:when>
      <xsl:otherwise>
	<!-- No link to local type -->
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- End of Dialyzer type/spec tags -->

  <!-- Cache for each module all the elements used for navigation. -->
  <xsl:variable name="erlref.nav" select="exsl:node-set($erlref.nav_rtf)"/>

  <xsl:variable name="erlref.nav_rtf">
    <xsl:for-each select="//erlref">
      <xsl:variable name="cval" select="module"/>
      <xsl:variable name="link_cval"><xsl:value-of select="translate($cval, '&#173;', '')"/></xsl:variable>
      <module name="{$cval}">
	<xsl:call-template name="menu.funcs">
	  <xsl:with-param name="entries" select="funcs/func/name"/>
	  <xsl:with-param name="cval" select="$cval"/>
	  <xsl:with-param name="basename" select="$link_cval"/>
	</xsl:call-template>
      </module>
    </xsl:for-each>
  </xsl:variable>

  <!-- Page layout -->
  <xsl:template name="pagelayout">
    <xsl:param name="chapnum"/>
    <xsl:param name="curModule"/>
    <html>
      <head>
        <xsl:choose>
          <xsl:when test="string-length($stylesheet) > 0">
            <link rel="stylesheet" href="{$topdocdir}/{$stylesheet}" type="text/css"/>
          </xsl:when>
          <xsl:otherwise>
            <link rel="stylesheet" href="{$topdocdir}/otp_doc.css" type="text/css"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="string-length($winprefix) > 0">
            <title><xsl:value-of select="$winprefix"/> -- <xsl:value-of select="header/title"/></title>
          </xsl:when>
          <xsl:otherwise>
            <title>Erlang -- <xsl:value-of select="header/title"/></title>
          </xsl:otherwise>
        </xsl:choose>
      </head>
      <body>

        <div id="container">
          <script id="js" type="text/javascript" language="JavaScript" src="{$topdocdir}/js/flipmenu/flipmenu.js"/>
          <script id="js2" type="text/javascript" src="{$topdocdir}/js/erlresolvelinks.js"></script>
          <script language="JavaScript" type="text/javascript">
            <xsl:text disable-output-escaping="yes"><![CDATA[
            <!--
              function getWinHeight() {
                var myHeight = 0;
                if( typeof( window.innerHeight ) == 'number' ) {
                  //Non-IE
                  myHeight = window.innerHeight;
                } else if( document.documentElement && ( document.documentElement.clientWidth ||
                                                         document.documentElement.clientHeight ) ) {
                  //IE 6+ in 'standards compliant mode'
                  myHeight = document.documentElement.clientHeight;
                } else if( document.body && ( document.body.clientWidth || document.body.clientHeight ) ) {
                  //IE 4 compatible
                  myHeight = document.body.clientHeight;
                }
                return myHeight;
              }

              function setscrollpos() {
                var objf=document.getElementById('loadscrollpos');
                 document.getElementById("leftnav").scrollTop = objf.offsetTop - getWinHeight()/2;
              }

              function addEvent(obj, evType, fn){
                if (obj.addEventListener){
                obj.addEventListener(evType, fn, true);
                return true;
              } else if (obj.attachEvent){
                var r = obj.attachEvent("on"+evType, fn);
                return r;
              } else {
                return false;
              }
             }

             addEvent(window, 'load', setscrollpos);

             //-->]]></xsl:text>
          </script>
          <!-- Generate menu -->
          <xsl:call-template name="menu">
            <xsl:with-param name="chapnum" select="$chapnum"/>
            <xsl:with-param name="curModule" select="$curModule"/>
          </xsl:call-template>

          <div id="content">
            <div class="innertube">

              <!-- Insert the node-specific content -->
              <xsl:call-template name="content">
                <xsl:with-param name="chapnum" select="$chapnum"/>
              </xsl:call-template>
            </div>

            <div class="footer">
              <hr/>
              <p>
                <xsl:value-of select="$copyright"/>
                <xsl:value-of select="/book/header/copyright/year[1]"/>
                <xsl:text>-</xsl:text>
                <xsl:value-of select="substring-after(normalize-space(substring-after($gendate, ' ')), ' ')"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="/book/header/copyright/holder"/>
              </p>
            </div>

          </div>
        </div>

        <script type="text/javascript"><xsl:text>window.__otpTopDocDir = '</xsl:text><xsl:value-of select="$topdocdir"/><xsl:text>/js/';</xsl:text></script>
        <script type="text/javascript" src="{$topdocdir}/js/highlight.js"/>
      </body>
    </html>
  </xsl:template>


  <!-- Content -->
  <xsl:template name="content">
    <xsl:param name="chapnum"/>

    <xsl:variable name="lname"><xsl:value-of select="local-name()"/></xsl:variable>

    <xsl:if test="$lname = 'releasenotes'">
      <!-- .../part -->
      <xsl:call-template name="releasenotes.content" />
    </xsl:if>
    <xsl:if test="$lname = 'part'">
      <!-- .../part -->
      <xsl:call-template name="part.content" />
    </xsl:if>
    <xsl:if test="$lname = 'chapter'">
      <!-- .../part/chapter -->
      <xsl:call-template name="chapter.content">
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="$lname = 'application'">
      <!-- .../application -->
      <xsl:call-template name="app.content" />
    </xsl:if>
    <xsl:if test="$lname = 'erlref' or $lname = 'cref' or $lname= 'comref' or $lname= 'fileref' or $lname= 'appref'">
      <!-- .../application/*ref -->
      <xsl:comment> refpage </xsl:comment>
      <xsl:call-template name="ref.content" />
    </xsl:if>
 </xsl:template>


  <!-- Menu -->
  <xsl:template name="menu">
    <xsl:param name="chapnum"/>
    <xsl:param name="curModule"/>
    <xsl:if test="(local-name() = 'part') or ((local-name() = 'chapter') and ancestor::part)">
      <!-- .../part or.../part/chapter  -->
      <xsl:call-template name="menu.ug">
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="(local-name() = 'application') or (local-name() = 'erlref')or (local-name() = 'comref')or (local-name() = 'cref')or (local-name() = 'fileref')or (local-name() = 'appref')">
      <!-- .../application,.../application/erlref, .../application/comref or .../application/cref  or .../application/fileref or .../application/appref -->
      <xsl:call-template name="menu.ref">
        <xsl:with-param name="curModule" select="$curModule"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="(local-name() = 'releasenotes') or ((local-name() = 'chapter') and ancestor::releasenotes)">
      <!-- releasenotes  -->
      <xsl:call-template name="menu.rn">
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>


  <xsl:template name="erlang_logo">
    <xsl:choose>
      <xsl:when test="string-length($logo) > 0">
        <div class="erlang-logo-wrapper">
            <a href="{$topdocdir}/index.html"><img alt="Erlang Logo" src="{$topdocdir}/{$logo}" class="erlang-logo"/></a>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <div class="erlang-logo-wrapper">
            <a href="{$topdocdir}/index.html"><img alt="Erlang Logo" src="{$topdocdir}/erlang-logo.png" class="erlang-logo"/></a>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="menu_top">
    <ul class="panel-sections">
      <xsl:if test="boolean(/book/parts/part)">
          <li><a href="users_guide.html">User's Guide</a></li>
      </xsl:if>
      <xsl:if test="boolean(/book/applications)">
          <li><a href="index.html">Reference Manual</a></li>
      </xsl:if>
      <xsl:if test="boolean(/book/releasenotes)">
          <li><a href="release_notes.html">Release Notes</a></li>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="string-length($pdfname) > 0">
        <li><a href="{$pdfdir}/{$pdfname}.pdf">PDF</a></li>
	</xsl:when>
	<xsl:otherwise>
        <li><a href="{$pdfdir}/{$appname}-{$appver}.pdf">PDF</a></li>
	</xsl:otherwise>
      </xsl:choose>
      <li><a href="{$topdocdir}/index.html">Top</a></li>
    </ul>
  </xsl:template>

  <xsl:template name="menu_middle">
    <!-- small>
      <xsl:choose>
        <xsl:when test="ancestor::parts">
          <a href="users_guide_bibliography.html">Bibliography</a><br/>
          <a href="users_guide_glossary.html">Glossary</a><br/>
        </xsl:when>
        <xsl:when test="ancestor::applications">
          <a href="ref_man_bibliography.html">Bibliography</a><br/>
          <a href="ref_man_glossary.html">Glossary</a><br/>
        </xsl:when>
      </xsl:choose>
    </small -->

    <ul class="expand-collapse-items">
        <li><a href="javascript:openAllFlips()">Expand All</a></li>
        <li><a href="javascript:closeAllFlips()">Contract All</a></li>
    </ul>
  </xsl:template>


  <!-- Book -->
  <xsl:template match="/book">
    <xsl:apply-templates select="parts"/>
    <xsl:apply-templates select="applications"/>
    <xsl:apply-templates select="releasenotes"/>
  </xsl:template>

  <!-- Parts -->
  <xsl:template match="parts">
    <xsl:apply-templates select="part"/>
  </xsl:template>

  <!-- Applications -->
  <xsl:template match="applications">
    <xsl:apply-templates select="application"/>
  </xsl:template>

 <!-- Header -->
 <xsl:template match="header"/>

 <!-- Section/Title -->
 <xsl:template match="section/title"/>

 <xsl:template match="pagetext"/>

  <!-- Chapter/Section, subsection level 1-->
  <xsl:template match="chapter/section">
    <xsl:param name="chapnum"/>
    <h3>
      <xsl:for-each select="marker">
	<xsl:call-template name="marker-before-title"/>
      </xsl:for-each>
      <xsl:call-template name="title_link">
        <xsl:with-param name="title">
          <xsl:value-of select="$chapnum"/>.<xsl:number/>&#160;
          <xsl:value-of select="title"/>
        </xsl:with-param>
      </xsl:call-template>
    </h3>
    <xsl:apply-templates>
      <xsl:with-param name="chapnum" select="$chapnum"/>
      <xsl:with-param name="sectnum"><xsl:number/></xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <!-- Subsections lvl 2 -->
  <xsl:template match="section/section">
    <xsl:param name="chapnum"/>
    <xsl:param name="sectnum"/>
    <h4>
      <xsl:for-each select="marker">
	<xsl:call-template name="marker-before-title"/>
      </xsl:for-each>
      <!-- xsl:value-of select="$partnum"/>.<xsl:value-of select="$chapnum"/>.<xsl:value-of select="$sectnum"/>.<xsl:number/ -->
      <xsl:call-template name="title_link">
        <xsl:with-param name="title" select="title"/>
      </xsl:call-template>
    </h4>
    <xsl:apply-templates>
      <xsl:with-param name="chapnum" select="$chapnum"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- Subsections lvl 3 and ... -->
  <xsl:template match="section/section/section">
    <xsl:param name="chapnum"/>
    <xsl:param name="sectnum"/>
    <h5>
      <xsl:for-each select="marker">
	<xsl:call-template name="marker-before-title"/>
      </xsl:for-each>
      <!-- xsl:value-of select="$partnum"/>.<xsl:value-of select="$chapnum"/>.<xsl:value-of select="$sectnum"/>.<xsl:number/ -->
      <xsl:value-of select="title"/>
    </h5>
    <xsl:apply-templates>
      <xsl:with-param name="chapnum" select="$chapnum"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- *ref/Section -->
  <xsl:template match="erlref/section|cref/section|comref/section|fileref/section|appref/section">
    <xsl:param name="chapnum"/>
    <h3>
      <xsl:for-each select="marker">
	<xsl:call-template name="marker-before-title"/>
      </xsl:for-each>
      <xsl:call-template name="title_link">
        <xsl:with-param name="title" select="title"/>
      </xsl:call-template>
    </h3>
    <div class="REFBODY rb-3">
    <xsl:apply-templates>
      <xsl:with-param name="chapnum" select="$chapnum"/>
    </xsl:apply-templates>
    </div>
  </xsl:template>

  <!-- *ref/Subsection -->
  <xsl:template match="erlref/section/section|cref/section/section|comref/section/section|fileref/section/section|appref/section/section">
    <xsl:param name="chapnum"/>
    <xsl:param name="sectnum"/>
    <h4>
      <xsl:value-of select="title"/>
    </h4>
    <div class="REFBODY rb-4">
    <xsl:apply-templates>
      <xsl:with-param name="chapnum" select="$chapnum"/>
    </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- Lists -->
  <xsl:template match="list">
    <xsl:param name="chapnum"/>
    <ul>
      <xsl:apply-templates>
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:apply-templates>
    </ul>
  </xsl:template>

  <xsl:template match="list/item">
    <xsl:param name="chapnum"/>
    <li>
      <xsl:apply-templates>
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:apply-templates>
    </li>
  </xsl:template>


  <xsl:template match="taglist">
    <xsl:param name="chapnum"/>
    <dl>
      <xsl:apply-templates>
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:apply-templates>
    </dl>
  </xsl:template>

  <xsl:template match="taglist/tag">
    <xsl:param name="chapnum"/>
    <dt>
      <strong>
        <xsl:apply-templates/>
      </strong>
    </dt>
  </xsl:template>


  <xsl:template match="taglist/item">
    <xsl:param name="chapnum"/>
    <dd>
      <xsl:apply-templates>
        <xsl:with-param name="chapnum" select="$chapnum"/>
      </xsl:apply-templates>
    </dd>
  </xsl:template>

  <!-- Note -->
  <xsl:template match="note">
    <xsl:param name="chapnum"/>
    <div class="note">
      <div class="label">Note</div>
      <div class="content">
        <p>
          <xsl:apply-templates>
            <xsl:with-param name="chapnum" select="$chapnum"/>
          </xsl:apply-templates>
        </p>
      </div>
    </div>
  </xsl:template>

  <!-- Warning -->
  <xsl:template match="warning">
    <xsl:param name="chapnum"/>
    <div class="warning">
      <div class="label">Warning</div>
      <div class="content">
        <p>
          <xsl:apply-templates>
            <xsl:with-param name="chapnum" select="$chapnum"/>
          </xsl:apply-templates>
        </p>
      </div>
    </div>
  </xsl:template>

  <!-- Do -->
  <xsl:template match="do">
    <xsl:param name="chapnum"/>
    <div class="do">
      <div class="label">Do</div>
      <div class="content">
        <p>
          <xsl:apply-templates>
            <xsl:with-param name="chapnum" select="$chapnum"/>
          </xsl:apply-templates>
        </p>
      </div>
    </div>
  </xsl:template>

  <!-- Dont -->
  <xsl:template match="dont">
    <xsl:param name="chapnum"/>
    <div class="dont">
      <div class="label">Don't</div>
      <div class="content">
        <p>
          <xsl:apply-templates>
            <xsl:with-param name="chapnum" select="$chapnum"/>
          </xsl:apply-templates>
        </p>
      </div>
    </div>
  </xsl:template>

  <!-- Quote -->
  <xsl:template match="quote">
    <xsl:param name="chapnum"/>
    <div class="quote">
      <p>
        <xsl:apply-templates>
          <xsl:with-param name="chapnum" select="$chapnum"/>
        </xsl:apply-templates>
      </p>
    </div>
  </xsl:template>

 <!-- Paragraph -->
  <xsl:template match="p">
    <p>
      <xsl:apply-templates/>
    </p>
  </xsl:template>

  <!-- Inline elements -->
  <xsl:template match="i">
    <i><xsl:apply-templates/></i>
  </xsl:template>

  <xsl:template match="br">
    <br/>
  </xsl:template>

  <xsl:template match="c">
    <span class="code"><xsl:apply-templates/></span>
  </xsl:template>

  <xsl:template match="em">
    <strong><xsl:apply-templates/></strong>
  </xsl:template>

  <xsl:template match="strong">
    <strong><xsl:apply-templates/></strong>
  </xsl:template>

  <!-- Code -->
  <xsl:template match="code">
    <xsl:param name="chapnum"/>
    <xsl:variable name="codenum">
      <xsl:number level="any" from="chapter" count="code"/>
    </xsl:variable>
      <xsl:choose> 
	<xsl:when test="not(descendant::anno)">
 	   <div class="example"><pre><xsl:value-of select="erl:code_trim(text())"/></pre></div>
 	</xsl:when>
	<xsl:otherwise>    
	   <div class="example"><pre><xsl:apply-templates/></pre></div>
	</xsl:otherwise>
      </xsl:choose>	
  </xsl:template>

  <!-- Pre -->
  <xsl:template match="pre">
    <xsl:param name="chapnum"/>
    <xsl:variable name="codenum">
      <xsl:number level="any" from="chapter" count="code"/>
    </xsl:variable>
    <div class="example"><pre><xsl:apply-templates/></pre></div>
  </xsl:template>


  <!-- Table -->
  <xsl:template match="table">
    <xsl:param name="chapnum"/>
    <xsl:variable name="tabnum">
      <xsl:number level="any" from="chapter" count="table"/>
    </xsl:variable>
    <div class="doc-table-wrapper">
    <table class="doc-table">
      <!-- tbody-->
      <xsl:apply-templates select="row">
        <xsl:with-param name="chapnum" select="$chapnum"/>
        <xsl:with-param name="tabnum" select="$tabnum"/>
      </xsl:apply-templates>
      <!-- /tbody-->
    </table>
    <xsl:apply-templates select="tcaption">
      <xsl:with-param name="chapnum" select="$chapnum"/>
      <xsl:with-param name="tabnum" select="$tabnum"/>
    </xsl:apply-templates>
    </div>
  </xsl:template>

  <xsl:template match="row">
    <tr>
      <xsl:apply-templates/>
    </tr>
  </xsl:template>

  <xsl:template match="cell">
    <td align="left" valign="middle">
      <xsl:apply-templates/>
    </td>
  </xsl:template>


  <xsl:template match="tcaption">
    <xsl:param name="chapnum"/>
    <xsl:param name="tabnum"/>

      <p class="doc-table-caption">Table
        <xsl:value-of select="$chapnum"/>.<xsl:value-of select="$tabnum"/>:
        &#160;
        <xsl:apply-templates/>
      </p>

  </xsl:template>

  <!-- Image -->
  <xsl:template match="image">
    <xsl:param name="chapnum"/>
    <xsl:variable name="fignum">
      <xsl:number level="any" from="chapter" count="image"/>
    </xsl:variable>

    <div class="doc-image-wrapper">
      <img alt="IMAGE MISSING" src="{@file}" class="doc-image"/>

      <xsl:apply-templates>
        <xsl:with-param name="chapnum" select="$chapnum"/>
        <xsl:with-param name="fignum" select="$fignum"/>
      </xsl:apply-templates>
    </div>


  </xsl:template>


  <xsl:template match="icaption">
    <xsl:param name="chapnum"/>
    <xsl:param name="fignum"/>

      <p class="doc-image-caption">Figure
        <xsl:value-of select="$chapnum"/>.<xsl:value-of select="$fignum"/>:
        &#160;
        <xsl:apply-templates/>
      </p>

  </xsl:template>




  <!--Users Guide -->

  <!-- Part -->
  <xsl:template match="part">
    <!-- Generate Glossary for Users Guide -->
    <!--xsl:call-template name="glossary">
    <xsl:with-param name="type">users_guide</xsl:with-param>
    </xsl:call-template-->

    <!-- Generate Bibliography for Users Guide -->
    <!--xsl:call-template name="bibliography">
      <xsl:with-param name="type">users_guide</xsl:with-param>
    </xsl:call-template-->

    <xsl:document href="{$outdir}/users_guide.html" method="html"  encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">
      <xsl:call-template name="pagelayout"/>
    </xsl:document>
  </xsl:template>


  <!-- Part content-->
  <xsl:template name="part.content">
    <div class="frontpage"/>

    <center><h1><xsl:value-of select="/book/header/title"/> User's Guide</h1></center>

    <center><h4>Version <xsl:value-of select="$appver"/></h4></center>
    <center><h4><xsl:value-of select="$gendate"/></h4></center>
    <div class="extrafrontpageinfo">
    <center><xsl:value-of select="$extra_front_page_info"/></center>
    </div>

    <xsl:apply-templates select="chapter"/>

  </xsl:template>

  <!-- Menu.ug -->
  <xsl:template name="menu.ug">
    <xsl:param name="chapnum"/>

    <div id="leftnav">
      <div class="innertube">

        <xsl:call-template name="erlang_logo"/>

        <p class="section-title"><xsl:value-of select="/book/header/title"/></p>
        <p class="section-subtitle">User's Guide</p>
        <p class="section-version">Version <xsl:value-of select="$appver"/></p>

        <xsl:call-template name="menu_top"/>

        <xsl:call-template name="menu_middle"/>

        <h3>Chapters</h3>

        <ul class="flipMenu" imagepath="{$topdocdir}/js/flipmenu">
          <xsl:call-template name="menu.chapter">
            <xsl:with-param name="entries" select="/book/parts/part/chapter[header/title]"/>
            <xsl:with-param name="chapnum" select="$chapnum"/>
          </xsl:call-template>
        </ul>
      </div>
    </div>
  </xsl:template>


  <xsl:template name="menu.chapter">
    <xsl:param name="entries"/>
    <xsl:param name="chapnum"/>
    <xsl:for-each select="$entries">
      <xsl:variable name="chapter_file">
        <xsl:value-of select='substring-before(header/file, ".xml")'/>
      </xsl:variable>
      <xsl:variable name="curchapnum"><xsl:number/></xsl:variable>
      <xsl:variable name="expanded">
        <xsl:choose>
          <xsl:when test="$chapnum = $curchapnum">true</xsl:when>
          <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="loadscrollpos">
        <xsl:choose>
          <xsl:when test="$chapnum = $curchapnum">loadscrollpos</xsl:when>
          <xsl:otherwise>no</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <li id="{$loadscrollpos}" title="{header/title}" expanded="{$expanded}">
        <xsl:value-of select="header/title"/>
        <ul>
          <li>
            <a href="{$chapter_file}.html">
              Top of chapter
            </a>
          </li>
          <xsl:call-template name="menu.section">
            <xsl:with-param name="entries"
              select="section[title]"/>
            <xsl:with-param name="chapter_file"><xsl:value-of select="$chapter_file"/></xsl:with-param>
          </xsl:call-template>
        </ul>
      </li>
    </xsl:for-each>
  </xsl:template>


  <xsl:template name="menu.section">
    <xsl:param name="entries"/>
    <xsl:param name="chapter_file"/>
    <xsl:for-each select="$entries">
      <li title="{title}">
        <a href="{$chapter_file}.html#{erl:to-link(title)}">
          <xsl:value-of select="title"/>
        </a>
      </li>
    </xsl:for-each>
  </xsl:template>

  <!-- Chapter (if top tag)-->
  <xsl:template match="/chapter">

    <xsl:document href="{substring-before(header/file, '.xml')}.html" method="html" encoding="UTF-8" indent="yes"
      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="chapnum"><xsl:number/></xsl:with-param>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>

  <!-- Chapter -->
  <xsl:template match="chapter">

    <xsl:document href="{substring-before(header/file, '.xml')}.html" method="html" encoding="UTF-8" indent="yes"
      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="chapnum"><xsl:number/></xsl:with-param>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>


  <!-- Chapter content-->
  <xsl:template name="chapter.content">
    <xsl:param name="chapnum"/>

    <!-- center-->
      <h1>
        <xsl:value-of select="$chapnum"/>&#160;<xsl:value-of select="header/title"/>
      </h1>
    <!-- /center-->

    <xsl:apply-templates>
      <xsl:with-param name="chapnum" select="$chapnum"/>
    </xsl:apply-templates>

  </xsl:template>




  <!-- Reference Manual -->

  <!-- Application -->
  <xsl:template match="application">

    <!-- Generate Glossary for Ref. Manual -->
    <!--xsl:call-template name="glossary">
      <xsl:with-param name="type">ref_man</xsl:with-param>
    </xsl:call-template-->

    <!-- Generate Bibliography for Ref. Manual -->
    <!--xsl:call-template name="bibliography">
      <xsl:with-param name="type">ref_man</xsl:with-param>
    </xsl:call-template-->

    <xsl:document href="{$outdir}/index.html" method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">
      <xsl:call-template name="pagelayout"/>
    </xsl:document>
  </xsl:template>


  <!-- Application content-->
  <xsl:template name="app.content">
    <div class="frontpage"/>

    <center><h1><xsl:value-of select="/book/header/title"/> Reference Manual</h1></center>

    <center><h4>Version <xsl:value-of select="$appver"/></h4></center>
    <center><h4><xsl:value-of select="$gendate"/></h4></center>
    <div class="extrafrontpageinfo">
    <center><xsl:value-of select="$extra_front_page_info"/></center>
    </div>

    <xsl:apply-templates select="erlref|cref|comref|fileref|appref"/>

  </xsl:template>

  <!-- Menu.ref -->
  <xsl:template name="menu.ref">
    <xsl:param name="curModule"/>
    <div id="leftnav">
      <div class="innertube">

        <xsl:call-template name="erlang_logo"/>

        <p class="section-title"><xsl:value-of select="/book/header/title"/></p>
        <p class="section-subtitle">Reference Manual</p>
        <p class="section-version">Version <xsl:value-of select="$appver"/></p>

        <xsl:call-template name="menu_top"/>

        <xsl:call-template name="menu_middle"/>

        <h3>Table of Contents</h3>

        <ul class="flipMenu">
          <xsl:call-template name="menu.ref2">
            <xsl:with-param name="entries" select="/book/applications/application/erlref[module]|/book/applications/application/cref[lib]|/book/applications/application/comref[com]|/book/applications/application/fileref[file]|/book/applications/application/appref[app]"/>
            <!--xsl:with-param name="genFuncMenu" select="true"/-->
            <xsl:with-param name="curModule" select="$curModule"/>
          </xsl:call-template>
        </ul>
      </div>
    </div>
  </xsl:template>


  <xsl:template name="menu.ref2">
    <xsl:param name="entries"/>
    <!--xsl:param name="genFuncMenu"/-->
    <xsl:param name="curModule"/>
    <xsl:for-each select="$entries">

      <xsl:variable name="cval">
        <xsl:choose>
          <xsl:when test="local-name() = 'erlref'">
            <xsl:value-of select="module"/>
          </xsl:when>
           <xsl:when test="local-name() = 'cref'">
             <xsl:value-of select="lib"/>
          </xsl:when>
          <xsl:when test="local-name() = 'comref'">
            <xsl:value-of select="com"/>
          </xsl:when>
          <xsl:when test="local-name() = 'fileref'">
            <xsl:value-of select="file"/>
          </xsl:when>
          <xsl:when test="local-name() = 'appref'">
            <xsl:value-of select="app"/>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>

      <xsl:variable name="genFuncMenu">
        <xsl:choose>
          <xsl:when test="local-name() = 'comref'">false</xsl:when>
          <xsl:when test="local-name() = 'appref'">false</xsl:when>
          <xsl:when test="local-name() = 'fileref'">false</xsl:when>
          <xsl:when test="descendant::funcs">true</xsl:when>
          <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <xsl:variable name="expanded">
        <xsl:choose>
          <xsl:when test="$curModule = $cval">true</xsl:when>
          <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <xsl:variable name="loadscrollpos">
        <xsl:choose>
          <xsl:when test="$curModule = $cval">loadscrollpos</xsl:when>
          <xsl:otherwise>no</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <xsl:variable name="link_cval"><xsl:value-of select="translate($cval, '&#173;', '')"/></xsl:variable>

      <xsl:choose>
        <xsl:when test="$genFuncMenu = 'true'">

          <li id="{$loadscrollpos}" title="{$cval} " expanded="{$expanded}">
            <xsl:value-of select="$cval"/>
            <ul>
              <li>
                <a href="{$link_cval}.html">
                  Top of manual page
                </a>
              </li>
              <xsl:call-template name="nl"/>
                <xsl:choose>
                  <xsl:when test="local-name() = 'erlref'">
	            <!-- Use the cached value in order to save time.
                         value-of a string node is _much_ faster than
			 copy-of a rtf -->
		    <xsl:value-of
		         disable-output-escaping="yes"
			 select="$erlref.nav/module[@name = $cval]"/>
                  </xsl:when>
                  <xsl:otherwise>
		    <xsl:call-template name="menu.funcs">
		      <xsl:with-param name="entries"
			select="funcs/func/name"/>
		      <xsl:with-param name="basename"><xsl:value-of select="$link_cval"/></xsl:with-param>
		      <xsl:with-param name="cval" select="$cval"/>
		    </xsl:call-template>
                  </xsl:otherwise>
                </xsl:choose>
            </ul>
          </li>
        </xsl:when>
        <xsl:otherwise>
          <xsl:choose>
            <xsl:when test="local-name() = 'appref'">
              <li title="{$cval} (App)">
                <a href="{$link_cval}_app.html">
                  <xsl:value-of select="$cval"/> (App)
                </a>
              </li>
            </xsl:when>
            <xsl:otherwise>
              <li title="{$cval}">
                <a href="{$link_cval}.html">
                  <xsl:value-of select="$cval"/>
                </a>
              </li>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>


  <xsl:template name="menu.funcs">
    <xsl:param name="entries"/>
    <xsl:param name="basename"/>
    <xsl:param name="cval"/>

    <xsl:for-each select="$entries">
      <!-- Sort on function name, so the index list in lefthand frame is ordered. -->
      <xsl:sort select="erl:get_sort_field(.)" data-type="text" case-order="upper-first"/>

      <xsl:choose>
        <xsl:when test="ancestor::cref">
          <xsl:variable name="fname"><xsl:value-of select="substring-before(nametext, '(')"/></xsl:variable>
          <xsl:choose>
            <xsl:when test="string-length($fname) > 0">
              <li title="{$fname}">
                <a href="{$basename}.html#{$fname}">
                  <xsl:value-of select="$fname"/>()
                </a>
              </li>
            </xsl:when>
            <xsl:otherwise>
              <li title="{name/nametext}">
                <a href="{$basename}.html#{name/nametext}">
                  <xsl:value-of select="nametext"/>()
                </a>
              </li>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>

        <xsl:when test="ancestor::erlref">

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

	  <!-- Avoid duplicated entries. See also template "spec_name" -->
          <!-- Do not to use preceding since it is very slow! -->
          <xsl:variable name="mfas"
			select="key('mfa',
				    concat($cval,':',$fname,'/',$arity))"/>
	  <xsl:choose>
            <xsl:when test="string-length(@name) > 0 and
                            generate-id($mfas[1]) != generate-id(.)">
              <!-- Skip. Only works for Dialyzer specs. -->
	    </xsl:when>
	    <xsl:otherwise>
<!--
	      <li title="{$fname}-{$arity}">
		<a href="{$basename}.html#{$fname}-{$arity}">
		  <xsl:value-of select="$fname"/>/<xsl:value-of select="$arity"/>
		</a>
	      </li>
-->
	      <!-- Generate a text node -->
	      <xsl:text>&lt;li title="</xsl:text>
	      <xsl:value-of select="$fname"/>
	      <xsl:text>-</xsl:text>
	      <xsl:value-of select="$arity"/>
	      <xsl:text>">&lt;a href="</xsl:text>
	      <xsl:value-of select="$basename"/>
	      <xsl:text>.html#</xsl:text>
	      <xsl:value-of select="$fname"/>
	      <xsl:text>-</xsl:text>
	      <xsl:value-of select="$arity"/>
	      <xsl:text>"></xsl:text>
	      <xsl:value-of select="$fname"/>
	      <xsl:text>/</xsl:text>
	      <xsl:value-of select="$arity"/>
	      <xsl:text>&lt;/a>&lt;/li></xsl:text>
              <xsl:call-template name="nl"/>
	    </xsl:otherwise>
	  </xsl:choose>
        </xsl:when>
      </xsl:choose>

    </xsl:for-each>
  </xsl:template>


  <!-- Erlref -->
  <xsl:template match="erlref">

    <xsl:variable name="filename"><xsl:value-of select="translate(module, '&#173;', '')"/></xsl:variable>

    <xsl:document href="{$filename}.html" method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="curModule" select="module"/>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>

  <!-- Cref -->
  <xsl:template match="cref">

    <xsl:document href="{lib}.html" method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="curModule" select="lib"/>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>

  <!-- Comref -->
  <xsl:template match="comref">

    <xsl:document href="{com}.html" method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="curModule" select="com"/>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>

  <!-- Fileref -->
  <xsl:template match="fileref">

    <xsl:document href="{file}.html" method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="curModule" select="file"/>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>

  <!-- Appref -->
  <xsl:template match="appref">

    <xsl:document href="{app}_app.html" method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">

      <xsl:call-template name="pagelayout">
        <xsl:with-param name="curModule" select="app"/>
      </xsl:call-template>
    </xsl:document>
  </xsl:template>

  <!-- *ref content-->
  <xsl:template name="ref.content">
    <xsl:param name="partnum"/>

    <center>
      <h1>
        <xsl:choose>
          <xsl:when test="local-name() = 'erlref'">
            <xsl:value-of select="module"/>
          </xsl:when>
           <xsl:when test="local-name() = 'cref'">
             <xsl:value-of select="lib"/>
          </xsl:when>
          <xsl:when test="local-name() = 'comref'">
            <xsl:value-of select="com"/>
          </xsl:when>
          <xsl:when test="local-name() = 'fileref'">
            <xsl:value-of select="file"/>
          </xsl:when>
          <xsl:when test="local-name() = 'appref'">
            <xsl:value-of select="app"/>
          </xsl:when>
        </xsl:choose>
      </h1>
    </center>

    <xsl:apply-templates>
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>


  <!-- Module -->
  <xsl:template match="module">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Module</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY module-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- Modulesummary -->
  <xsl:template match="modulesummary">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Module Summary</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY module-summary-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>

  <!-- Lib -->
  <xsl:template match="lib">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">C Library</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY c-library-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- Libsummary -->
  <xsl:template match="libsummary">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Library Summary</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY library-summary-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>

  <!-- Com -->
  <xsl:template match="com">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Command</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY command-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- Comsummary -->
  <xsl:template match="comsummary">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Command Summary</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY command-summary-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>

  <!-- File -->
  <xsl:template match="file">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">File</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY file-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- Filesummary -->
  <xsl:template match="filesummary">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">File Summary</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY file-summary-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- App -->
  <xsl:template match="app">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Application</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY application-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>


  <!-- Appsummary -->
  <xsl:template match="appsummary">
    <xsl:param name="partnum"/>
    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Application Summary</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY application-summary-body">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>

  <!-- Description -->
  <xsl:template match="description">
    <xsl:param name="partnum"/>
        <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Description</xsl:with-param>
    </xsl:call-template>
    <div class="REFBODY description-body">
      <p>
        <xsl:apply-templates>
          <xsl:with-param name="partnum" select="$partnum"/>
        </xsl:apply-templates>
      </p>
    </div>
  </xsl:template>

  <!-- Funcs -->
  <xsl:template match="funcs">
    <xsl:param name="partnum"/>

    <xsl:call-template name="h3_title_link">
      <xsl:with-param name="title">Exports</xsl:with-param>
    </xsl:call-template>

    <div class="exports-body">
        <xsl:apply-templates>
            <xsl:with-param name="partnum" select="$partnum"/>
        </xsl:apply-templates>
    </div>

  </xsl:template>

  <!-- Func -->
  <xsl:template match="func">
    <xsl:param name="partnum"/>

    <p><xsl:apply-templates select="name"/>
       <xsl:apply-templates
           select="name[string-length(@arity) > 0 and position()=last()]"
           mode="types"/>
    </p>

    <xsl:apply-templates select="fsummary|type|desc">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

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

  <!-- Used both in <datatype> and in <func>! -->
  <xsl:template name="name">

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
      <xsl:call-template name="calc-arity">
        <xsl:with-param name="string" select="substring-before($ustring, ')')"/>
        <xsl:with-param name="no-of-pars" select="0"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="ancestor::cref">
        <span class="bold_code bc-7">
          <xsl:call-template name="title_link">
            <xsl:with-param name="link" select="substring-before(nametext, '(')"/>
            <xsl:with-param name="title">
              <xsl:value-of select="ret"/>
              <xsl:call-template name="maybe-space-after-ret">
                <xsl:with-param name="s" select="ret"/>
              </xsl:call-template>
              <xsl:value-of select="nametext"/>
            </xsl:with-param>
          </xsl:call-template>
        </span>
        <br/>
      </xsl:when>
      <xsl:when test="ancestor::erlref">
        <xsl:variable name="fname">
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
        </xsl:variable>
	<xsl:choose>
	  <xsl:when test="ancestor::datatype">
            <div class="bold_code bc-8">
              <xsl:call-template name="title_link">
                <xsl:with-param name="link" select="concat('type-',$fname)"/>
                <xsl:with-param name="title">
                  <xsl:apply-templates/>
                </xsl:with-param>
              </xsl:call-template>
            </div>
	  </xsl:when>
          <xsl:otherwise>
            <div class="bold_code fun-type">
              <xsl:call-template name="title_link">
                <xsl:with-param name="link" select="concat(concat($fname,'-'),$arity)"/>
                <xsl:with-param name="title">
                  <xsl:apply-templates/>
                </xsl:with-param>
              </xsl:call-template>
            </div>
          </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <div class="bold_code bc-10"><xsl:value-of select="."/></div>
      </xsl:otherwise>
    </xsl:choose>

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
    <xsl:param name="partnum"/>

    <!-- The case where @name != 0 is taken care of in "type_name" -->
    <xsl:if test="string-length(@name) = 0 and string-length(@variable) = 0">

        <div class="REFBODY rb-5">
            <h3 class="func-types-title">Types</h3>

            <xsl:apply-templates>
                <xsl:with-param name="partnum" select="$partnum"/>
            </xsl:apply-templates>
        </div>

    </xsl:if>

  </xsl:template>


  <!-- V -->
  <xsl:template match="v">
    <xsl:param name="partnum"/>
    <div class="REFTYPES rt-4">
      <span class="bold_code fun-param-type">
        <xsl:apply-templates>
          <xsl:with-param name="partnum" select="$partnum"/>
        </xsl:apply-templates>
      </span><br/>
    </div>
  </xsl:template>

  <!-- D -->
  <xsl:template match="d">
    <xsl:param name="partnum"/>
    <div class="REFBODY rb-6">
      <xsl:apply-templates>
        <xsl:with-param name="partnum" select="$partnum"/>
      </xsl:apply-templates>
    </div>
  </xsl:template>

  <xsl:template name="h3_title_link">
    <xsl:param name="title"/>
    <h3>
      <xsl:call-template name="title_link">
        <xsl:with-param name="title" select="$title"/>
        <xsl:with-param name="link" select="erl:to-link($title)"/>
      </xsl:call-template>
    </h3>
  </xsl:template>

  <xsl:template name="title_link">
    <xsl:param name="title"/>
    <xsl:param name="link" select="erl:to-link(title)"/>
    <xsl:param name="ghlink" select="ancestor-or-self::*[@ghlink][position() = 1]/@ghlink"/>
    <xsl:variable name="id" select="concat(concat($link,'-'), generate-id(.))"/>
    <span onMouseOver="document.getElementById('ghlink-{$id}').style.visibility = 'visible';"
          onMouseOut="document.getElementById('ghlink-{$id}').style.visibility = 'hidden';">
      <xsl:call-template name="ghlink">
          <xsl:with-param name="id" select="$id"/>
          <xsl:with-param name="ghlink" select="$ghlink"/>
      </xsl:call-template>
      <a class="title_link" name="{$link}" href="#{$link}"><xsl:value-of select="$title"/></a>
    </span>
  </xsl:template>

  <xsl:template name="ghlink">
    <xsl:param name="id"/>
    <xsl:param name="ghlink" select="ancestor-or-self::*[@ghlink][position() = 1]/@ghlink"/>
    <xsl:choose>
      <xsl:when test="string-length($ghlink) > 0">
        <span id="ghlink-{$id}" class="ghlink">
          <a href="https://github.com/erlang/otp/edit/{$ghlink}"
             title="Found an issue with the documentation? Fix it by clicking here!">
            <span class="pencil"/>
          </a>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span id="ghlink-{$id}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Desc -->
  <xsl:template match="desc">
    <xsl:param name="partnum"/>
    <div class="REFBODY rb-7">
      <p>
        <xsl:apply-templates>
          <xsl:with-param name="partnum" select="$partnum"/>
        </xsl:apply-templates>
      </p>
    </div>
  </xsl:template>


  <!-- Fsummary -->
  <xsl:template match="fsummary">
    <!-- This tag is skipped for now. -->
  </xsl:template>


  <xsl:template match="input">
    <span class="bold_code bc-12"><xsl:apply-templates/></span>
  </xsl:template>

  <xsl:template match="seealso">
    <xsl:call-template name="seealso"/>
  </xsl:template>

  <xsl:template name="seealso">
    <xsl:variable name="filepart"><xsl:value-of select="substring-before(@marker, '#')"/></xsl:variable>
    <xsl:variable name="linkpart"><xsl:value-of select="translate(substring-after(@marker, '#'), '/', '-')"/></xsl:variable>

    <xsl:choose>
      <xsl:when test="string-length($filepart) > 0">
        <!-- "Filepart#Linkpart" (or "Filepart#") -->
        <xsl:variable name="app_part"><xsl:value-of select="substring-before($filepart, ':')"/></xsl:variable>
        <xsl:choose>
          <xsl:when test="string-length($app_part) > 0">
            <!-- "AppPart:ModPart#Linkpart" -->
            <xsl:variable name="mod_part"><xsl:value-of select="substring-after($filepart, ':')"/></xsl:variable>
            <span class="bold_code bc-13"><a href="javascript:erlhref('{$topdocdir}/../','{$app_part}','{$mod_part}.html#{$linkpart}');"><xsl:apply-templates/></a></span>
          </xsl:when>
          <xsl:otherwise>
            <!-- "Filepart#Linkpart (there is no ':' in Filepart) -->
            <xsl:variable name="minus_prefix"
                          select="substring-before($linkpart, '-')"/>
            <xsl:choose>
              <xsl:when test="$minus_prefix = 'type'
                              and string-length($specs_file) > 0
                              and count($i/specs/module[@name=$filepart]) = 0">
                <!-- Dialyzer seealso (the application is unknown) -->
                <!-- Following code deemed too slow; use key() instead
		<xsl:variable name="app"
                              select="$m2a/mod2app/module[@name=$filepart]"/>
                -->
                <xsl:variable name="this" select="."/>
                <xsl:for-each select="$m2a">
                  <xsl:variable name="app" select="key('mod2app', $filepart)"/>
		  <xsl:choose>
		    <xsl:when test="string-length($app) > 0">
		      <span class="bold_code bc-14"><a href="javascript:erlhref('{$topdocdir}/../','{$app}','{$filepart}.html#{$linkpart}');"><xsl:value-of select="$this"/></a></span>
		    </xsl:when>
		    <xsl:otherwise>
		      <!-- Unknown application -->
		      <xsl:message terminate="yes">
			Error <xsl:value-of select="$filepart"/>: cannot find module exporting type
		      </xsl:message>
		    </xsl:otherwise>
		  </xsl:choose>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="string-length($linkpart) > 0">
                <!-- Still Filepart#Linkpart (there is no ':' in Filepart -->
                <span class="bold_code bc-15"><a href="{$filepart}.html#{$linkpart}"><xsl:apply-templates/></a></span>
              </xsl:when>
              <xsl:otherwise>
                <!-- "Filepart#" (there is no ':' in Filepart -->
                <span class="bold_code bc-16"><a href="{$filepart}.html"><xsl:apply-templates/></a></span>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when> <!-- string-length($filepart) > 0 -->
      <xsl:when test="string-length($linkpart) > 0">
	<!-- "#Linkpart" -->
	<span class="bold_code bc-17"><a href="#{$linkpart}"><xsl:apply-templates/></a></span>
      </xsl:when>
      <xsl:otherwise>
	<!-- "AppPart:Mod" or "Mod" (there is no '#') -->
	<xsl:variable name="app_part"><xsl:value-of select="substring-before(@marker, ':')"/></xsl:variable>

	<xsl:choose>
	  <xsl:when test="string-length($app_part) > 0">
	    <!-- "App:Mod" -->
	    <xsl:variable name="mod_part"><xsl:value-of select="substring-after(@marker, ':')"/></xsl:variable>
	    <span class="bold_code bc-18"><a href="javascript:erlhref('{$topdocdir}/../','{$app_part}','{$mod_part}.html');"><xsl:apply-templates/></a></span>
	  </xsl:when>
	  <xsl:otherwise>
	    <!-- "Mod" -->
	    <span class="bold_code bc-19"><a href="{@marker}.html"><xsl:apply-templates/></a></span>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template match="url">
    <span class="bold_code bc-20"><a href="{@href}"><xsl:apply-templates/></a></span>
  </xsl:template>

  <xsl:template match="marker">
    <xsl:choose>
      <xsl:when test="not(parent::section and following-sibling::title)">
        <a name="{@id}"><xsl:apply-templates/></a>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="marker-before-title">
    <xsl:choose>
      <xsl:when test="self::marker and parent::section and following-sibling::title">
	 <a name="{@id}"><xsl:apply-templates/></a>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="term">
    <xsl:value-of select="@id"/>
    <!-- xsl:choose>
      <xsl:when test="boolean(termdef)">
        <xsl:choose>
          <xsl:when test="ancestor::parts">
            <a href="users_guide_glossary.html#{@id}"><xsl:value-of select="@id"/></a>
          </xsl:when>
          <xsl:when test="ancestor::applications">
            <a href="ref_man_glossary.html#{@id}"><xsl:value-of select="@id"/></a>
          </xsl:when>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <a href="{$topdocdir}/glossary.html#{@id}"><xsl:value-of select="@id"/></a>
      </xsl:otherwise>
    </xsl:choose -->
  </xsl:template>

  <xsl:template match="cite">
    <xsl:value-of select="@id"/>
  </xsl:template>


  <!-- Release Notes -->
  <xsl:template match="releasenotes">

    <xsl:document href="{$outdir}/release_notes.html" method="html"  encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">
      <xsl:call-template name="pagelayout"/>
    </xsl:document>
  </xsl:template>

  <!-- Rel notes content-->
  <xsl:template name="releasenotes.content">
    <div class="frontpage"/>

    <center><h1><xsl:value-of select="/book/header/title"/> Release Notes</h1></center>

    <center><h4>Version <xsl:value-of select="$appver"/></h4></center>
    <center><h4><xsl:value-of select="$gendate"/></h4></center>
    <div class="extrafrontpageinfo">
    <center><xsl:value-of select="$extra_front_page_info"/></center>
    </div>

    <xsl:apply-templates select="chapter"/>

  </xsl:template>

  <!-- Menu.rn -->
  <xsl:template name="menu.rn">
    <xsl:param name="chapnum"/>

    <div id="leftnav">
      <div class="innertube">

        <xsl:call-template name="erlang_logo"/>

        <p class="section-title"><xsl:value-of select="/book/header/title"/></p>
        <p class="section-subtitle">Release Notes</p>
        <p class="section-version">Version <xsl:value-of select="$appver"/></p>

        <xsl:call-template name="menu_top"/>

        <xsl:call-template name="menu_middle"/>

        <h3>Chapters</h3>

        <ul class="flipMenu" imagepath="{$topdocdir}/js/flipmenu">
          <xsl:call-template name="menu.chapter">
            <xsl:with-param name="entries" select="/book/releasenotes/chapter[header/title]"/>
            <xsl:with-param name="chapnum" select="$chapnum"/>
          </xsl:call-template>
        </ul>
      </div>
    </div>
  </xsl:template>

  <!-- Glossary -->
  <xsl:template name="glossary">
    <xsl:param name="type"/>
    <xsl:document href="{$outdir}/{$type}_glossary.html" method="html"  encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">
    <html>
      <head>
        <link rel="stylesheet" href="{$topdocdir}/otp_doc.css" type="text/css"/>
        <title>Erlang Documentation -- <xsl:value-of select="header/title"/></title>
      </head>
      <body bgcolor="white" text="#000000" link="#0000ff" vlink="#ff00ff" alink="#ff0000">

        <div id="container">
          <script id="js" type="text/javascript" language="JavaScript" src="{$topdocdir}/js/flipmenu/flipmenu.js"/>
          <script id="js2" type="text/javascript" src="{$topdocdir}/js/erlresolvelinks.js"></script>

          <!-- Generate menu -->
          <xsl:call-template name="menu"/>

          <div id="content">
            <div class="innertube">
              <h1>Glossary</h1>
            </div>

            <dl>
              <xsl:for-each select="descendant::term">
                <xsl:sort select="@id"/>
                <xsl:if test="boolean(termdef)">
                    <dt><a name="{@id}"><strong><xsl:value-of select="@id"/></strong></a></dt>
                    <dd><xsl:value-of select="termdef"/></dd>
                </xsl:if>
              </xsl:for-each>
            </dl>

            <div class="footer">
              <hr/>
              <p>
                <xsl:value-of select="$copyright"/>
                <xsl:value-of select="header/copyright/year[1]"/>
                <xsl:text>-</xsl:text>
                <xsl:value-of select="header/copyright/year[2]"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="header/copyright/holder"/>
              </p>
            </div>

          </div>
        </div>

      </body>
    </html>

    </xsl:document>
  </xsl:template>

  <!-- Bibliography -->
  <xsl:template name="bibliography">

    <xsl:param name="type"/>
    <xsl:document href="{$outdir}/{$type}_bibliography.html" method="html"  encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN">
    <html>
      <head>
        <link rel="stylesheet" href="{$topdocdir}/otp_doc.css" type="text/css"/>
        <title>Erlang Documentation -- <xsl:value-of select="header/title"/></title>
      </head>
      <body bgcolor="white" text="#000000" link="#0000ff" vlink="#ff00ff" alink="#ff0000">

        <div id="container">
          <script id="js" type="text/javascript" language="JavaScript" src="{$topdocdir}/js/flipmenu/flipmenu.js"/>
          <script id="js2" type="text/javascript" src="{$topdocdir}/js/erlresolvelinks.js"></script>

          <!-- Generate menu -->
          <xsl:call-template name="menu"/>

          <div id="content">
            <div class="innertube">
              <h1>Bibliography</h1>
            </div>

            <table>
              <xsl:for-each select="descendant::cite">
                <xsl:sort select="@id"/>
                <xsl:if test="boolean(citedef)">
                  <tr>
                    <td><xsl:value-of select="@id"/></td>
                    <td><xsl:value-of select="citedef"/></td>
                  </tr>
                </xsl:if>
              </xsl:for-each>
            </table>

            <div class="footer">
              <hr/>
              <p>
                <xsl:value-of select="$copyright"/>
                <xsl:value-of select="header/copyright/year[1]"/>
                <xsl:text>-</xsl:text>
                <xsl:value-of select="header/copyright/year[2]"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="header/copyright/holder"/>
              </p>
            </div>

          </div>
        </div>

      </body>
    </html>

    </xsl:document>
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
          <xsl:value-of select="substring-after(substring-after($string, $start), $end)"/>
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

  <xsl:template name="nl">
    <xsl:text>
    </xsl:text>
  </xsl:template>

  <xsl:template match="seealso//text()">
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

</xsl:stylesheet>
