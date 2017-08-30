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
  extension-element-prefixes="exsl"
  xmlns:fn="http://www.w3.org/2005/02/xpath-functions"> 

  <xsl:output method="text" encoding="UTF-8" indent="no"/>

  <xsl:param name="specs_file" select="''"/>
  <xsl:variable name="i" select="document($specs_file)"></xsl:variable>


  <!-- Book -->
  <xsl:template match="/book">
    <xsl:text>%% &#10;%% Search data file for </xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/>
    <xsl:text>&#10;%% generated </xsl:text><xsl:value-of select="$gendate"/><xsl:text>&#10;%% &#10;</xsl:text>
    <xsl:apply-templates select="applications"/>
    <xsl:text>{notused, application, ["</xsl:text><xsl:value-of select="$appname"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>

  <!-- Applications -->
  <xsl:template match="applications">
    <xsl:apply-templates name="application"/>
  </xsl:template>

  <!-- Reference Manual -->

  <!-- Application -->
  <xsl:template match="application">
    <xsl:apply-templates select="erlref|cref|comref|fileref|appref"/>          
  </xsl:template>

  <!-- Erlref -->
  <xsl:template match="erlref">
    <xsl:text>{"</xsl:text><xsl:value-of select="module"/><xsl:text>.html", {function, {"</xsl:text><xsl:value-of select="$appname"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="module"/><xsl:text>"}},&#10;[&#10;</xsl:text>
    <xsl:apply-templates select="funcs"/>  
    <xsl:text>]}.&#10;</xsl:text>
    <xsl:text>{"</xsl:text><xsl:value-of select="module"/><xsl:text>.html", {module, "</xsl:text>
    <xsl:value-of select="$appname"/><xsl:text>"}, ["</xsl:text><xsl:value-of select="module"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>

  <!-- Cref -->
  <xsl:template match="cref">
    <xsl:text>{"</xsl:text><xsl:value-of select="lib"/><xsl:text>.html", {function, {"</xsl:text><xsl:value-of select="$appname"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="lib"/><xsl:text>"}}, [&#10;</xsl:text>
    <xsl:apply-templates select="funcs"/>  
    <xsl:text>]}.&#10;</xsl:text>
    <xsl:text>{"</xsl:text><xsl:value-of select="lib"/><xsl:text>.html", {clib, "</xsl:text>
    <xsl:value-of select="$appname"/><xsl:text>"}, ["</xsl:text><xsl:value-of select="lib"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>

  <!-- Comref -->
  <xsl:template match="comref">
    <xsl:text>{"</xsl:text><xsl:value-of select="com"/><xsl:text>.html", {command, "</xsl:text>
    <xsl:value-of select="$appname"/><xsl:text>"}, ["</xsl:text><xsl:value-of select="com"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>

  <!-- Fileref -->
  <xsl:template match="fileref">
    <xsl:text>{"</xsl:text><xsl:value-of select="file"/><xsl:text>.html", {file, "</xsl:text>
    <xsl:value-of select="$appname"/><xsl:text>"}, ["</xsl:text><xsl:value-of select="file"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>

  <!-- Appref -->
  <xsl:template match="appref">
    <xsl:text>{"</xsl:text><xsl:value-of select="app"/><xsl:text>_app.html", {app, "</xsl:text>
    <xsl:value-of select="$appname"/><xsl:text>"}, ["</xsl:text><xsl:value-of select="app"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>


  <!-- Funcs -->
  <xsl:template match="funcs">
    <xsl:variable name="lastfuncsblock">
      <xsl:value-of select="position() = last()"/>
    </xsl:variable>         
    <xsl:apply-templates select="func/name">
      <xsl:with-param name="lastfuncsblock" select="$lastfuncsblock"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="name">
    <xsl:param name="lastfuncsblock"/>
    <xsl:choose>
      <!-- @arity is mandatory when referring to a specification -->
      <xsl:when test="string-length(@arity) > 0">
        <xsl:call-template name="spec_name">
	  <xsl:with-param name="lastfuncsblock" select="$lastfuncsblock"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="name">
	  <xsl:with-param name="lastfuncsblock" select="$lastfuncsblock"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

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
    <xsl:param name="lastfuncsblock"/>
    <xsl:variable name="fname" select="@name"/>
    <xsl:variable name="arity" select="@arity"/>
    <xsl:variable name="spec0">
      <xsl:call-template name="find_spec"/>
    </xsl:variable>
    <xsl:variable name="spec" select="exsl:node-set($spec0)/spec"/>

    <xsl:variable name="tmpstring">
      <xsl:value-of select="substring-before($spec/contract/clause/head, ' ->')"/>
    </xsl:variable>

    <xsl:text>  {"</xsl:text><xsl:value-of select="$fname"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="$tmpstring"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="$fname"/>
    <xsl:text>-</xsl:text><xsl:value-of select="$arity"/><xsl:text>"}</xsl:text>
    
    <xsl:choose>
      <xsl:when test="($lastfuncsblock = 'true') and (position() = last())">
        <xsl:text>&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>,&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <xsl:template name="name">
    <xsl:param name="lastfuncsblock"/>

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

    <xsl:variable name="fname">
      <xsl:choose>
	<xsl:when test="ancestor::cref">
	  <xsl:value-of select="substring-before(nametext, '(')"/>
	</xsl:when>
	<xsl:when test="ancestor::erlref">
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
	</xsl:when>
      </xsl:choose>
    </xsl:variable>
    
    <xsl:text>  {"</xsl:text><xsl:value-of select="$fname"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="$fname"/>
    <xsl:text>(</xsl:text><xsl:value-of select="normalize-space($tmpstring)"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="$fname"/>
    <xsl:text>-</xsl:text><xsl:value-of select="$arity"/><xsl:text>"}</xsl:text>
    
    <xsl:choose>
      <xsl:when test="($lastfuncsblock = 'true') and (position() = last())">
        <xsl:text>&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>,&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
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

  <!-- default content handling -->
  <xsl:template match="text()"/>

</xsl:stylesheet>
