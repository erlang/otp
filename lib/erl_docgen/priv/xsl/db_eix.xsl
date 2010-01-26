<?xml version="1.0" encoding="utf-8"?>
<!--      
     #
     # %CopyrightBegin%
     # 
     # Copyright Ericsson AB 2009. All Rights Reserved.
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
  xmlns:fn="http://www.w3.org/2005/02/xpath-functions"> 

  <xsl:output method="text" encoding="UTF-8" indent="no"/>

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
    <xsl:apply-templates select="funcs">  
      <xsl:with-param name="mod" select="module"/>
    </xsl:apply-templates>  
    <xsl:text>]}.&#10;</xsl:text>
    <xsl:text>{"</xsl:text><xsl:value-of select="module"/><xsl:text>.html", {module, "</xsl:text>
    <xsl:value-of select="$appname"/><xsl:text>"}, ["</xsl:text><xsl:value-of select="module"/><xsl:text>"]}.&#10;</xsl:text>
  </xsl:template>

  <!-- Cref -->
  <xsl:template match="cref">
    <xsl:text>{"</xsl:text><xsl:value-of select="lib"/><xsl:text>.html", {function, {"</xsl:text><xsl:value-of select="$appname"/>
    <xsl:text>", "</xsl:text><xsl:value-of select="lib"/><xsl:text>"}}, [&#10;</xsl:text>
    <xsl:apply-templates select="funcs">  
      <xsl:with-param name="mod" select="lib"/>
    </xsl:apply-templates>  
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
    <xsl:param name="mod"/>
    <xsl:variable name="lastfuncsblock">
      <xsl:value-of select="position() = last()"/>
    </xsl:variable>         
    <xsl:apply-templates select="func/name">
      <xsl:with-param name="mod" select="$mod"/>
      <xsl:with-param name="lastfuncsblock" select="$lastfuncsblock"/>
    </xsl:apply-templates>
  </xsl:template>




  <xsl:template match="name">
    <xsl:param name="mod"/>
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

  <!-- default content handling -->
  <xsl:template match="text()"/>

</xsl:stylesheet>
