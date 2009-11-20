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

  <!-- When using <code>, whitespaces should be preserved -->
  <xsl:preserve-space elements="code"/>


 <!-- Fixed strings -->
 <xsl:variable name="companyname"><xsl:value-of select="header/copyright/holder"/></xsl:variable>
  <xsl:variable name="copyright">Copyright &#169; </xsl:variable>

  

</xsl:stylesheet>
