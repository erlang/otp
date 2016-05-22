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
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- When using <code>, whitespaces should be preserved -->
  <xsl:preserve-space elements="code"/>


 <!-- Fixed strings -->
 <xsl:variable name="companyname"><xsl:value-of select="header/copyright/holder"/></xsl:variable>
  <xsl:variable name="copyright">Copyright &#169; </xsl:variable>

  

</xsl:stylesheet>
