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
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

  <!-- When using <code>, whitespaces should be preserved -->
  <xsl:preserve-space elements="code"/>


 <!-- Fixed strings -->
 <xsl:variable name="companyname"><xsl:value-of select="/book/header/copyright/holder"/></xsl:variable>
  <xsl:variable name="copyright">Copyright &#169; <xsl:value-of select="/book/header/copyright/year[1]"/><xsl:text>-</xsl:text><xsl:value-of select="substring-after(normalize-space(substring-after($gendate, ' ')), ' ')"/></xsl:variable>

  <!-- FIXME: remove when appendix creation has been fixed -->
  <!-- xsl:variable name="appendix_title"-->
      <!-- xsl:text>Appendix: Unix man pages for ConfD</xsl:text-->
  <!-- /xsl:variable-->

  <!-- Font size (all other font sizes should be proportional to this -->
  <xsl:param name="base-font-size">10pt</xsl:param>

  <!-- Paper size: A4 (297x210 mm) -->
  <xsl:param name="page-height">297mm</xsl:param>
  <xsl:param name="page-width">210mm</xsl:param>

  <!-- Paper size: US Letter (279x216 mm) -->
  <!-- 
  <xsl:param name="page-height">11in</xsl:param>
  <xsl:param name="page-width">8.5in</xsl:param>
  -->


  <!-- XSL-FO properties -->
  <xsl:attribute-set name="caption">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">0.8em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="keep-with-previous.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">2.5em</xsl:attribute>
    <xsl:attribute name="space-before">0em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="pre">
    <xsl:attribute name="font-family">DejaVuSansMono, monospace</xsl:attribute>
    <xsl:attribute name="font-size">0.8em</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">auto</xsl:attribute>
    <xsl:attribute name="linefeed-treatment">preserve</xsl:attribute>
    <xsl:attribute name="padding-after">0em</xsl:attribute>
    <!-- Compensate for empty line that always seems to appear here now... -->
    <xsl:attribute name="padding-before">-1em</xsl:attribute>
    <xsl:attribute name="white-space-collapse">false</xsl:attribute>
    <xsl:attribute name="white-space-treatment">preserve</xsl:attribute>
    <xsl:attribute name="wrap-option">no-wrap</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.copyright">
    <xsl:attribute name="font-size">0.9em</xsl:attribute>
    <xsl:attribute name="text-align">end</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.logo">
    <xsl:attribute name="space-before">130mm</xsl:attribute>
    <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    <xsl:attribute name="text-align">end</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.title">
    <xsl:attribute name="border-before-style">solid</xsl:attribute>
    <xsl:attribute name="border-before-width">10pt</xsl:attribute>
    <xsl:attribute name="border-color"><xsl:value-of select="$pdfcolor"/></xsl:attribute>
    <xsl:attribute name="font-size">2.3em</xsl:attribute>
    <xsl:attribute name="padding-before">0.5em</xsl:attribute>
    <xsl:attribute name="text-align">end</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.version">
    <xsl:attribute name="font-size">0.9em</xsl:attribute>
    <xsl:attribute name="text-align">end</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.extrainfo">
    <xsl:attribute name="padding-before">2.5em</xsl:attribute>
    <xsl:attribute name="font-size">1.33em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="color">#C00</xsl:attribute>
    <xsl:attribute name="text-align">end</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.inner.copyright">
    <xsl:attribute name="border-before-style">solid</xsl:attribute>
    <xsl:attribute name="border-before-width">1pt</xsl:attribute>
    <xsl:attribute name="border-color"><xsl:value-of select="$pdfcolor"/></xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="padding-before">0.5em</xsl:attribute>
    <xsl:attribute name="space-before">200mm</xsl:attribute>
    <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.inner.copyrightnotice">
    <xsl:attribute name="font-size">0.9em</xsl:attribute>
    <!-- xsl:attribute name="font-weight">bold</xsl:attribute -->
  </xsl:attribute-set>

  <xsl:attribute-set name="cover.inner.date">
    <xsl:attribute name="font-size">0.9em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="dd">
    <xsl:attribute name="start-indent">2em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="dt">
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="image">
    <xsl:attribute name="space-after">0.5em</xsl:attribute>
    <xsl:attribute name="space-before">0.5em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="listblock">
    <xsl:attribute name="provisional-distance-between-starts">1.8em</xsl:attribute>
    <xsl:attribute name="provisional-label-separation">1em</xsl:attribute>
    <xsl:attribute name="space-after">0.25em</xsl:attribute>
    <xsl:attribute name="space-before">0.25em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="listitem">
    <xsl:attribute name="space-after">0.25em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="taglistblock">
    <xsl:attribute name="space-after">0.25em</xsl:attribute>
    <xsl:attribute name="space-before">0.25em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="tag">
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="tagitem">
    <xsl:attribute name="start-indent">2em</xsl:attribute>
  </xsl:attribute-set>


  <xsl:attribute-set name="h1">
    <xsl:attribute name="border-after-style">solid</xsl:attribute>
    <xsl:attribute name="border-after-width">1pt</xsl:attribute>
    <xsl:attribute name="border-color"><xsl:value-of select="$pdfcolor"/></xsl:attribute>
    <xsl:attribute name="break-before">page</xsl:attribute>
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">1.83em</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
    <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="h2">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">1.5em</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0.3em</xsl:attribute>
    <xsl:attribute name="space-before">1em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="h3">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">1.33em</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0.3em</xsl:attribute>
    <xsl:attribute name="space-before">0.8em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="h4">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">1.17em</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0.3em</xsl:attribute>
    <xsl:attribute name="space-before">0.6em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="h5">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">1em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0.2em</xsl:attribute>
    <xsl:attribute name="space-before">0.4em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="h6">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">0.83em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0em</xsl:attribute>
    <xsl:attribute name="space-before">0.4em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="p">
    <xsl:attribute name="font-size">1em</xsl:attribute>
    <!-- <xsl:attribute name="keep-together.within-page">always</xsl:attribute>-->
    <xsl:attribute name="space-after">0.5em</xsl:attribute>
    <xsl:attribute name="space-before">0.5em</xsl:attribute>
    <xsl:attribute name="text-align">justify</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="page-header">
    <xsl:attribute name="border-after-style">solid</xsl:attribute>
    <xsl:attribute name="border-after-width">2pt</xsl:attribute>
    <xsl:attribute name="border-color"><xsl:value-of select="$pdfcolor"/></xsl:attribute>
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">0.9em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="page-footer">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">0.9em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="code">
    <xsl:attribute name="background-color">#e0e0ff</xsl:attribute>
    <xsl:attribute name="font-family">DejaVuSansMono, monospace</xsl:attribute>
    <xsl:attribute name="font-size">0.8em</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">auto</xsl:attribute>
    <xsl:attribute name="linefeed-treatment">preserve</xsl:attribute>
    <xsl:attribute name="padding-before">0em</xsl:attribute>
    <xsl:attribute name="padding-after">1em</xsl:attribute>
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
    <xsl:attribute name="margin-left">0.5em</xsl:attribute>
    <xsl:attribute name="margin-right">0.5em</xsl:attribute>
    <xsl:attribute name="white-space-collapse">false</xsl:attribute>
    <xsl:attribute name="white-space-treatment">preserve</xsl:attribute>
    <xsl:attribute name="wrap-option">no-wrap</xsl:attribute>
  </xsl:attribute-set>



  <xsl:attribute-set name="toc.level1">
    <xsl:attribute name="space-before">1em</xsl:attribute>
  </xsl:attribute-set> 

<xsl:attribute-set name="note">
    <xsl:attribute name="background-color">#d0fed0</xsl:attribute>
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
    <xsl:attribute name="text-align">justify</xsl:attribute>
    <xsl:attribute name="padding-before">1em</xsl:attribute>
    <xsl:attribute name="padding-after">0.3em</xsl:attribute>
    <xsl:attribute name="padding-left">0.5em</xsl:attribute>
    <xsl:attribute name="padding-right">0.5em</xsl:attribute>
    <xsl:attribute name="margin-left">0.5em</xsl:attribute>
    <xsl:attribute name="margin-right">0.5em</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
  </xsl:attribute-set>

<xsl:attribute-set name="warning">
  <xsl:attribute name="background-color">#ffd6d6</xsl:attribute> 
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
    <xsl:attribute name="text-align">justify</xsl:attribute>
    <xsl:attribute name="padding-before">1em</xsl:attribute>
    <xsl:attribute name="padding-after">0.3em</xsl:attribute>
    <xsl:attribute name="padding-left">0.5em</xsl:attribute>
    <xsl:attribute name="padding-right">0.5em</xsl:attribute>
    <xsl:attribute name="margin-left">0.5em</xsl:attribute>
    <xsl:attribute name="margin-right">0.5em</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
  </xsl:attribute-set>

<xsl:attribute-set name="do">
    <xsl:attribute name="background-color">#d0fed0</xsl:attribute>
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
    <xsl:attribute name="text-align">justify</xsl:attribute>
    <xsl:attribute name="padding-before">1em</xsl:attribute>
    <xsl:attribute name="padding-after">0.3em</xsl:attribute>
    <xsl:attribute name="padding-left">0.5em</xsl:attribute>
    <xsl:attribute name="padding-right">0.5em</xsl:attribute>
    <xsl:attribute name="margin-left">0.5em</xsl:attribute>
    <xsl:attribute name="margin-right">0.5em</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
  </xsl:attribute-set>

<xsl:attribute-set name="dont">
  <xsl:attribute name="background-color">#ffd6d6</xsl:attribute> 
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">2em</xsl:attribute>
    <xsl:attribute name="text-align">justify</xsl:attribute>
    <xsl:attribute name="padding-before">1em</xsl:attribute>
    <xsl:attribute name="padding-after">0.3em</xsl:attribute>
    <xsl:attribute name="padding-left">0.5em</xsl:attribute>
    <xsl:attribute name="padding-right">0.5em</xsl:attribute>
    <xsl:attribute name="margin-left">0.5em</xsl:attribute>
    <xsl:attribute name="margin-right">0.5em</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="note-warning-title">
    <xsl:attribute name="font-size">1.33em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="module-header">
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">2em</xsl:attribute>
    <xsl:attribute name="space-before">1em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="module-name">
    <xsl:attribute name="border-after-style">solid</xsl:attribute>
    <xsl:attribute name="border-after-width">1pt</xsl:attribute>
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">1.5em</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0.3em</xsl:attribute>
    <xsl:attribute name="space-before">1em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="function-name">
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="font-family">DejaVuSansMono, monospace</xsl:attribute>
    <!-- xsl:attribute name="font-size">0.8em</xsl:attribute -->
    <xsl:attribute name="keep-with-next.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">0.25em</xsl:attribute>
    <!-- xsl:attribute name="space-before">1.5em</xsl:attribute -->
  </xsl:attribute-set> 

  <xsl:attribute-set name="type-listblock">
    <xsl:attribute name="provisional-distance-between-starts">1.8em</xsl:attribute>
    <xsl:attribute name="provisional-label-separation">1em</xsl:attribute>
    <xsl:attribute name="space-after">0.25em</xsl:attribute>
    <xsl:attribute name="space-before">0.25em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="type-listitem">
    <xsl:attribute name="space-after">0.25em</xsl:attribute>
  </xsl:attribute-set>


  <xsl:attribute-set name="table">
    <!-- Only border-collapse="separate" is supported in current FOP
         (if this is not set, border on spanned cells will not work) -->
    <xsl:attribute name="border-collapse">separate</xsl:attribute>
    <xsl:attribute name="space-after">1em</xsl:attribute>
    <xsl:attribute name="space-before">1em</xsl:attribute>
    <xsl:attribute name="table-layout">fixed</xsl:attribute>
    <!-- Just to get rid of some info reports, should be removed when fop supports table-layout auto -->
    <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="table-cell">
    <xsl:attribute name="border-style">solid</xsl:attribute>
    <xsl:attribute name="border-width">0.5pt</xsl:attribute>
    <xsl:attribute name="display-align">before</xsl:attribute>
    <xsl:attribute name="padding-after">0.5em</xsl:attribute>
    <xsl:attribute name="padding-before">0.5em</xsl:attribute>
    <xsl:attribute name="padding-end">0.3em</xsl:attribute>
    <xsl:attribute name="padding-start">0.3em</xsl:attribute>
    <xsl:attribute name="text-align">start</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="table-header">
    <xsl:attribute name="border-style">solid</xsl:attribute>
    <xsl:attribute name="border-width">1pt</xsl:attribute>
    <xsl:attribute name="display-align">before</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="padding-after">0.5em</xsl:attribute>
    <xsl:attribute name="padding-before">0.5em</xsl:attribute>
    <xsl:attribute name="padding-end">0.3em</xsl:attribute>
    <xsl:attribute name="padding-start">0.3em</xsl:attribute>
    <xsl:attribute name="text-align">center</xsl:attribute>
  </xsl:attribute-set>


  <xsl:template name="cell-valign">
    <xsl:choose>
      <xsl:when test="@valign='top'">
        <xsl:attribute name="display-align">before</xsl:attribute>
      </xsl:when>
      <xsl:when test="@valign='middle'">
        <xsl:attribute name="display-align">center</xsl:attribute>
      </xsl:when>
      <xsl:when test="@valign='bottom'">
        <xsl:attribute name="display-align">after</xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="cell-align">
    <xsl:choose>
      <xsl:when test="@align='left'">
        <xsl:attribute name="text-align">start</xsl:attribute>
      </xsl:when>
      <xsl:when test="@align='center'">
        <xsl:attribute name="text-align">center</xsl:attribute>
      </xsl:when>
      <xsl:when test="@align='right'">
        <xsl:attribute name="text-align">end</xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:attribute-set name="image">
    <xsl:attribute name="space-after">0.5em</xsl:attribute>
    <xsl:attribute name="space-before">0.5em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="caption">
    <xsl:attribute name="font-family">DejaVuSans, sans-serif</xsl:attribute>
    <xsl:attribute name="font-size">0.8em</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="keep-with-previous.within-page">always</xsl:attribute>
    <xsl:attribute name="space-after">2.5em</xsl:attribute>
    <xsl:attribute name="space-before">0em</xsl:attribute>
  </xsl:attribute-set>

</xsl:stylesheet>
