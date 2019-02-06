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
  xmlns:erl="http://erlang.org"
  xmlns:func="http://exslt.org/functions"
  extension-element-prefixes="func"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:fn="http://www.w3.org/2005/02/xpath-functions">

  <!-- Used from code template to trim the newline/cr after the tag
       and spaces/tabs between them
  -->
  <xsl:variable name="newlinechars" select="'&#10;&#13;'" />
  <xsl:variable name="spacechars" select="'&#09; '" />

  <func:function name="erl:code_trim">
    <xsl:param name="string" />

    <xsl:variable name="leftresult" select="erl:code_ltrim($string, $string)"/>
    <xsl:variable name="result" select="erl:code_rtrim($leftresult, $leftresult)"/>

    <func:result select="$result"/>
  </func:function>

  <func:function name="erl:code_rtrim">
    <xsl:param name="string" />
    <xsl:param name="origstring" />
    
    <xsl:variable name="length" select="string-length($string)" />

    <xsl:variable name="result">
      <xsl:if test="$length &gt; 0">
	<xsl:choose>
          <xsl:when test="contains($spacechars, substring($string, $length, 1))">
	    <xsl:value-of select="erl:code_rtrim(substring($string, 1, $length - 1), $origstring)" />
          </xsl:when>
          <xsl:when test="contains($newlinechars, substring($string, $length, 1))">
	    <xsl:value-of select="erl:code_rtrim_1(substring($string, 1, $length - 1))" />
          </xsl:when>	
          <xsl:otherwise>
            <xsl:value-of select="$origstring" />
          </xsl:otherwise>
	</xsl:choose>
    </xsl:if>
    </xsl:variable>

    <func:result select="$result" />
  </func:function>
  
  <func:function name="erl:code_rtrim_1">
    <xsl:param name="string" />
    
    <xsl:variable name="length" select="string-length($string)" />

    <xsl:variable name="result">
      <xsl:if test="$length &gt; 0">
	<xsl:choose>
          <xsl:when test="contains($newlinechars, substring($string, $length, 1))">
	    <xsl:value-of select="erl:code_rtrim_1(substring($string, 1, $length - 1))" />
          </xsl:when>	
          <xsl:otherwise>
	    <xsl:value-of select="erl:code_rtrim($string, $string)" />	    
            <!--xsl:value-of select="$string" /-->
          </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:variable>

    <func:result select="$result" />
  </func:function>
  
  <func:function name="erl:code_ltrim">
    <xsl:param name="string" />
    <xsl:param name="origstring" />

    <xsl:variable name="result">
      <xsl:if test="string-length($string) &gt; 0">
	<xsl:choose>
          <xsl:when test="contains($spacechars, substring($string, 1, 1))">
	    <xsl:value-of select="erl:code_ltrim(substring($string, 2), $origstring)" />
          </xsl:when>
          <xsl:when test="contains($newlinechars, substring($string, 1, 1))">
	    <xsl:value-of select="erl:code_ltrim_1(substring($string, 2))" />
          </xsl:when>	
          <xsl:otherwise>
            <xsl:value-of select="$origstring" />
          </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:variable>

    <func:result select="$result" />
  </func:function>
  
  <func:function name="erl:code_ltrim_1">
    <xsl:param name="string" />

    <xsl:variable name="result">
      <xsl:if test="string-length($string) &gt; 0">
	<xsl:choose>
          <xsl:when test="contains($newlinechars, substring($string, 1, 1))">
	    <xsl:value-of select="erl:code_ltrim_1(substring($string, 2))" />
          </xsl:when>	
          <xsl:otherwise>
	    <xsl:value-of select="erl:code_ltrim($string, $string)" />
            <!--xsl:value-of select="$string" /-->
          </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:variable>
    
    <func:result select="$result" />
  </func:function>

</xsl:stylesheet>
