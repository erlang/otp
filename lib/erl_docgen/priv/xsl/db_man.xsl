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
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"> 

  <xsl:preserve-space elements="code pre"/>
  <xsl:strip-space elements="*"/>
  <xsl:output method="text" encoding="UTF-8" indent="no"/>

  <!-- Header -->
  <xsl:template match="header">
  </xsl:template>
  
  <!-- Section/Title -->
  <xsl:template match="section/title">
  </xsl:template>
  
  <!-- *ref/Section -->
  <xsl:template match="erlref/section|comref/section|cref/section|fileref/section|appref/section">
    <xsl:text>&#10;.RE&#10;</xsl:text>
    <xsl:text>&#10;.SH "</xsl:text><xsl:value-of select="translate(title, 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/><xsl:text>"&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- *ref/Subsection -->
  <xsl:template match="erlref/section/section|comref/section/section|cref/section/section|fileref/section/section|appref/section/section">
    <xsl:text>&#10;.RE&#10;</xsl:text>
    <xsl:text>&#10;.SS "</xsl:text><xsl:value-of select="title"/><xsl:text>"&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>


  <!-- Lists -->
  
  <xsl:template match="list">
    <xsl:text>&#10;.RS 2&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.RE&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="list/item">
    <xsl:text>&#10;.TP 2&#10;</xsl:text>
    <xsl:text>*&#10;</xsl:text>
    <xsl:variable name="content">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:value-of select="normalize-space($content)"/>
    <xsl:text>&#10;.br&#10;</xsl:text>
    <xsl:text>&#10;.br&#10;</xsl:text>    
  </xsl:template>

  <xsl:template match="taglist">
    <xsl:text>&#10;.RS 2&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.RE&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="taglist/tag">
    <xsl:text>&#10;.TP 4&#10;</xsl:text>
    <xsl:text>.B&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="item/p">
    <xsl:variable name="content">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:value-of select="normalize-space($content)"/>
    <xsl:text>&#10;.br&#10;</xsl:text>
    <xsl:text>&#10;.br&#10;</xsl:text>
  </xsl:template>


  <xsl:template match="taglist/item">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Note -->
  <xsl:template match="note">
    <xsl:text>&#10;.TP 4&#10;.B&#10;Note:&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Warning -->
  <xsl:template match="warning">
    <xsl:text>&#10;.TP 4&#10;.B&#10;Warning:&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="warning/p|note/p">
    <xsl:variable name="content">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:value-of select="normalize-space($content)"/>
    <xsl:text>&#10;.LP&#10;</xsl:text>
  </xsl:template>


 <!-- Paragraph -->
  <xsl:template match="p">
    <xsl:text>&#10;.LP&#10;</xsl:text>
    <xsl:variable name="content">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:value-of select="normalize-space($content)"/>
  </xsl:template>

  <!-- Inline elements -->
  <xsl:template match="b">
    <xsl:text> \fB</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="br">
    <xsl:text>&#10;.br&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="c">
    <xsl:text> \fI</xsl:text><xsl:value-of select="text()"/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="em">
    <xsl:text> \fI</xsl:text><xsl:value-of select="text()"/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <xsl:template match="seealso">
    <xsl:text> \fB</xsl:text><xsl:apply-templates/><xsl:text>\fR\&amp;</xsl:text>
  </xsl:template>

  <!-- Code -->
  <xsl:template match="code">
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi&#10;</xsl:text>
  </xsl:template>

  <!-- Pre -->
  <xsl:template match="pre">
    <xsl:text>&#10;.nf&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;.fi&#10;</xsl:text>
  </xsl:template>


  <!-- Table -->
  <xsl:template match="table">
  </xsl:template>

  <!--xsl:template match="row">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="cell">
      <xsl:apply-templates/>
  </xsl:template -->


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
      <xsl:text>.TH </xsl:text><xsl:value-of select="module"/><xsl:text> 3 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "Ericsson AB" "Erlang Module Definition"&#10;</xsl:text>
      <xsl:text>.SH MODULE&#10;</xsl:text>
      <xsl:value-of select="module"/><xsl:text> \- </xsl:text><xsl:value-of select="modulesummary"/><xsl:text>&#10;</xsl:text>  
      <xsl:apply-templates/>
  </xsl:template>

  <!-- Comref -->
  <xsl:template match="/comref">
      <xsl:text>.TH </xsl:text><xsl:value-of select="com"/><xsl:text> 1 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "Ericsson AB" "User Commands"&#10;</xsl:text>
      <xsl:text>.SH NAME&#10;</xsl:text>
      <xsl:value-of select="com"/><xsl:text> \- </xsl:text><xsl:value-of select="comsummary"/><xsl:text>&#10;</xsl:text>  
      <xsl:apply-templates/>
  </xsl:template>

  <!-- Cref -->
  <xsl:template match="/cref">
      <xsl:text>.TH </xsl:text><xsl:value-of select="lib"/><xsl:text> 3 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "Ericsson AB" "C Library Functions"&#10;</xsl:text>
      <xsl:text>.SH NAME&#10;</xsl:text>
      <xsl:value-of select="lib"/><xsl:text> \- </xsl:text><xsl:value-of select="libsummary"/><xsl:text>&#10;</xsl:text>  
      <xsl:apply-templates/>
  </xsl:template>

  <!-- Fileref -->
  <xsl:template match="/fileref">
      <xsl:text>.TH </xsl:text><xsl:value-of select="file"/><xsl:text> 4 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "Ericsson AB" "Files"&#10;</xsl:text>
      <xsl:text>.SH NAME&#10;</xsl:text>
      <xsl:value-of select="file"/><xsl:text> \- </xsl:text><xsl:value-of select="filesummary"/><xsl:text>&#10;</xsl:text>  
      <xsl:apply-templates/>
  </xsl:template>

  <!-- Appref -->
  <xsl:template match="/appref">
      <xsl:text>.TH </xsl:text><xsl:value-of select="app"/><xsl:text> 6 "</xsl:text><xsl:value-of select="$appname"/><xsl:text> </xsl:text><xsl:value-of select="$appver"/><xsl:text>" "Ericsson AB" "Erlang Application Definition"&#10;</xsl:text>
      <xsl:text>.SH NAME&#10;</xsl:text>
      <xsl:value-of select="file"/><xsl:text> \- </xsl:text><xsl:value-of select="filesummary"/><xsl:text>&#10;</xsl:text>  
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
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="name">
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

  <!-- xsl:template match="p/text()">
    <xsl:value-of select="normalize-space()"/>
  </xsl:template-->

  <xsl:template match="d/text()">
    <xsl:value-of select="normalize-space()"/>
  </xsl:template>

</xsl:stylesheet>
