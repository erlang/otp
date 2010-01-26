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
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

  <xsl:output method="xml" indent="yes"/>

  <xsl:include href="db_pdf_params.xsl"/>


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
              <xsl:call-template name="calc-arity">
                <xsl:with-param name="string" select="substring-before($ustring, ')')"/>
                <xsl:with-param name="no-of-pars" select="0"/> 
              </xsl:call-template>
            </xsl:variable> 

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

  <!--  Chapter/Subsection -->
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

    <fo:block xsl:use-attribute-sets="function-name">
      <xsl:apply-templates select="name"/>
    </fo:block>

    <xsl:apply-templates select="fsummary|type|desc">
      <xsl:with-param name="partnum" select="$partnum"/>
    </xsl:apply-templates>

  </xsl:template>


  <xsl:template match="name">
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
