<?xml version='1.0'?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"

 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<!--
<<<<<<< conformance.xsl
    Original version copyright 1999 by Sun Microsystems, Inc.
    All Rights Reserved.
    Modifications copyright 1999 by OASIS.
    Modifications copyright 2001 by OASIS.
    Modifications copyright 2002 by W3C.
=======
    XSL Stylesheet for documenting XML conformance tests.
    Tested against LOTUS Xalan processor.
>>>>>>> 1.3

    XSL 2001-03-15 Stylesheet for documenting XML conformance tests.

    This expects to be run on a document matching the DTD that Sun
    defined for merging collections of self-descriptive XML tests.

    Since all those collections will have (by design) the same test
    architecture, this includes boilerplate describing that design,
    to be used by all test documentation.

    ISSUES:
	- Sorting is a bit odd; section numbers can are like "3.3.3"
	  rather than straight numbers, so numeric sort can't work,
	  and yet neither does text sort (2.12 should be after 2.2).
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>

    <!-- ROOT:  write an HTML wrapper -->
    <xsl:template match="/TESTSUITE">
	<!-- XHTML namespace -->
		
	<html xmlns="http://www.w3.org/1999/xhtml"><head>
	    <title> XML Conformance Tests </title>
	    <meta http-equiv="Content-Type"
		content="text/html;charset=utf-8"/>
	    <style type="text/css">

   		tr.premiere { background-color: #BEDCE6; }

   		th { text-align: left; vertical-align: top }

   		.editor { color: red; }

   		.countdown { color: white; background-color: red; }

  		 blockquote, q { font-family: Arial, Helvetica, sans-serif; }

   		.quote     { font-family: Arial, Helvetica, sans-serif; }

  		pre.quote { margin-left: 2.5em; }

   		ol.quote     { font-family: Arial, Helvetica, sans-serif; margin-left: 3em; }

   		.diff-add { background-color: yellow; }

   		.diff-chg { background-color: lime; }

   		.diff-del { text-decoration: line-through; }

  	   </style>
	    
	</head><body bgcolor='#ffffff'>
	    <p><a href="http://www.w3.org/"><img src="http://xw2k.sdct.itl.nist.gov/martinez/xmlts/2001/XML-Test-Suite/xmlconf/files/w3c_home.gif"

		alt="W3C" height="48" width="72" border="0"/></a><a href="http://www.w3.org/Architecture/"

		rel="in-domain"><img src="http://xw2k.sdct.itl.nist.gov/martinez/xmlts/2001/XML-Test-Suite/xmlconf/files/arquitecture.bmp" alt="Architecture Domain" border="0"/></a> <a

		href="../" rel="in-area">   XML</a> | <a href="/Member/#confidential">Member-Confidential!</a></p> 

  		<h1 align="center">XML 1.0 (Second Edition) errata 20020320,</h1> 
		<h1 align="center">W3C Conformance Test Suite 20020606</h1>  <dl>


	    <dt><b>This version:</b></dt>
	    <dd><ul>
	       <li><p><a href="xmlconf-20020606.htm">
		   http://www.w3.org/XML/Test/xmlconf-20020606.htm</a>
		   (<a href="xmlconf-20020606.xml">XML</a>)</p>
	       </li>
	    </ul>
	    </dd>
	    
	    <dt><b>Current Version:</b></dt>
	    <dd>
	    <ul>
	       <li><p><a href="xmlconf-20020606.xml">
		  http://www.w3.org/XML/Test/xmlconf-20020606.xml</a></p></li>
            </ul>
            </dd>
	    
	    <dt><b>Previous Version:</b></dt>
	    <dd>
	    <ul>
	       <li><p><a href="http://www.oasis-open.org/committees/xml-conformance/suite-v1se/xmlconf-20010315.htm">
		   http://www.oasis-open.org/committees/xml-conformance/suite-v1se/xmlconf-20010315.htm</a>
		   (<a href="http://www.oasis-open.org/committees/xml-conformance/suite-v1se/xmlconf-20010315.xml">XML</a>)</p></li>
            </ul>
            </dd>
	  
	    <dt><b>Test Archive:</b></dt>
	    <dd><ul>
	    <li><p><a href="xmlts20020606.zip">http://www.w3.org/XML/Test/xmlts20020606.zip</a>
	    </p></li>
	    <li><p><a href="xmlts20020606.tar">http://www.w3.org/XML/Test/xmlts20020606.tar</a>
	    </p></li>
	    </ul></dd>
	  
	    <dt><b>W3C XML Core Working Group:</b></dt> 	
	    <dd><ul><li><p><a href="http://www.w3.org/XML/Group/Core">http://www.w3.org/XML/Group/Core</a>
	    </p></li></ul></dd>
	    
	    <dt><b>Comments:</b></dt> 	
	    <dd><ul><li>Sandra I. Martinez, NIST <a href="mailto:public-xml-testsuite@w3.org"> public-xml-testsuite@w3.org</a>
	    </li></ul></dd>
	    
	    </dl>

	    <a name="contents"/>
            <h2>Table of Contents</h2>
	    <ol >
		<li><a href="#intro">Introduction</a></li>
		<li><a href="#matrix">Test Matrix</a></li> 
		<ol >
		    <li ><a href="#binary">Binary Tests</a></li>
		    <li><a href="#output">Output Tests</a></li>
		</ol>
		<li><a href="#listings">Test Case Descriptions</a></li>
		<ol >
		    <li><a href="#valid">Valid Documents</a></li>
		    <li><a href="#invalid">Invalid Documents</a></li>
		    <li><a href="#not-wf">Not-WF Documents</a></li>
		    <li><a href="#error">Optional Errors</a></li>
		</ol>
                <li><a href="#contrib">Contributors</a></li>
	    </ol>
	    <a name="intro"/>
	    <h2>1. Introduction </h2>
	    <p>  
	    The tests described in this document provide an initial set of metrics to determine how well a 
            particular implementation conforms to the 
            <a href="http://www.w3.org/TR/REC-xml">W3C XML 1.0 (Second Edition) Recommendation</a>.
	    The XML Conformance Test Suite is intended
	    to complement the W3C XML 1.0 (Second Edition) Recommendation.  All interpretations
	    of this Recommendation are subject to confirmation by the 
	    <a href="http://www.w3.org/XML/">
	    W3C XML Group </a>.
	    </p>
	    <p>
	    Conformance tests can be used by developers, content creators, and 
            users alike to increase their level of confidence in product quality. In
	    circumstances where interoperability is necessary, these tests can also
	    be used to determine that differing implementations support the same set
            of features. </p>
           
            <p>The XML Test Suite was transferred from OASIS to W3C and is  being augmented to reflect the
            current work of the W3C XML Core Working Group, including 
	    <a href="http://www.w3.org/XML/Group/2002/02/xml10-test-suite-issues">resolved issues</a> related to the 
	    Recommendation and published Errata. This report provides supporting documentation for all the tests included in 
	    the test suite. Sources from which these tests have been collected
	    include: <em>
		<xsl:for-each select="TESTCASES">
		    <xsl:value-of select="@PROFILE"/>
		    <xsl:text>; </xsl:text>
		</xsl:for-each>
	    </em>.  </p>  

	    <a name="matrix"/>
	    <h2>2.  Test Matrix </h2>

	    <p> Two basic types of test are presented here.  These are
	    respectively <em><a href="#binary">Binary Tests</a></em>
	    and <em><a href="#output">Output Tests</a></em>. </p>

	    <a name="binary"/>
	    <h3>2.1  Binary Tests </h3>

	    <p> <em>Binary</em> conformance tests are documents which
	    are grouped into one of four categories.  Given a document
	    in a given category, each kind of XML parser must treat it
	    consistently and either accept it (a <em>positive test</em>)
	    or reject it (a <em>negative test</em>).  It is in that sense
	    that the tests are termed "binary". The XML 1.0 (Second Edition) Recommendation
	    talks in terms of two types of XML processor:
	    <em>validating</em> ones, and <em>nonvalidating</em> ones.
	    There are two differences between these types of processors: </p>

	    <ol>
		<li> Validating processors check special productions that
		nonvalidating parsers don't, called <em>validity
		constraints</em>.  (Both must check a basic set of productions,
		requiring XML documents to be <em>well formed</em>.) </li>

		<li> Nonvalidating processors are permitted to not
		include <em>external entities</em>, such as files with
		text.  Accordingly, they may not report errors which
		would have been detected had those entities been read.</li>
	    </ol>

	    <p> There are two types of such entity, <em>parameter
	    entities</em> holding definitions which affect validation
	    and other processing; and <em>general entities</em> which
	    hold marked up text.  It will be appreciated that there are
	    then five kinds of XML processor:  validating processors,
	    and four kinds of nonvalidating processor based on the
	    combinations of external entity which they include.</p>

	    <center>
	    <table border="1" bgcolor="#ffffff" cellpadding="4">

	    <caption>
	    <b>Basic XML Parsing Test Matrix</b><br/>
	    Test Document Type v. Parser Type
	    </caption>

	    <tr bgcolor="#ffffcc">
		<th widthH="5%" rowspan="2"><xsl:text> </xsl:text></th>
		<th widthH="20%" colspan="2">Nonvalidating</th>
		<th widthH="5%" rowspan="2">Validating</th>
	    </tr>

	    <tr bgcolor="#ffffcc">
		<th>External Entities<br/>Ignored (3 cases)</th>
		<th>External Entities<br/>Read</th>
	    </tr>

	    <tr>
		<th bgcolor="#ffffcc">Valid Documents</th>
		<td align="center"><b>accept</b></td>
		<td align="center"><b>accept</b></td>
		<td align="center"><b>accept</b></td>
	    </tr>

	    <tr>
		<th bgcolor="#ffffcc">Invalid Documents</th>
		<td align="center"><b>accept</b></td>
		<td align="center"><b>accept</b></td>
		<td align="center">reject</td>
	    </tr>

	    <tr>
		<th bgcolor="#ffffcc">Non-WF Documents</th>
		<td align="center">reject</td>
		<td align="center">reject</td>
		<td align="center">reject</td>
	    </tr>

	    <tr>
		<th bgcolor="#ffffcc">WF Errors tied<br/>
			to External Entity</th>
		<td align="center"><b>accept</b><br/>(varies)</td>
		<td align="center">reject</td>
		<td align="center">reject</td>
	    </tr>

	    <tr>
		<th bgcolor="#ffffcc">Documents with<br/> Optional Errors</th>
		<td align="center">(not specified)</td>
		<td align="center">(not specified)</td>
		<td align="center">(not specified)</td>
	    </tr>

	    </table>
	    </center>

	    <p> At this time, the XML community primarily uses parsers
	    which are in the rightmost two columns of this table, calling
	    them <em>Well Formed XML Parsers</em> (or "WF Parsers") and
	    <em>Validating XML Parsers</em>.   A second test matrix
	    could be defined to address the variations in the types of
	    of XML processor which do not read all external entities.
	    That additional matrix is not provided here at this time. </p>


	    <a name="output"/>
	    <h3>2.2  Output Tests</h3>

	    <p> The XML 1.0 (Second Edition) Recommendation places a number of requirements
	    on XML processors, to ensure that they report information to
	    applications as needed.  Such requirements are testable.
	    Validating processors are required to report slightly more
	    information than nonvalidating ones, so some tests will
	    require separate output files.  Some of the information that
	    must be reported will not be reportable without reading all
	    the external entities in a particular test.  Many of the tests for
	    valid documents are paired with an output file as the canonical
	    representation of the input file,  to ensure that the XML
	    processor provides the correct information. </p>

	    <a name="listings"/>
	    <h2>3.  Test Case Descriptions</h2>

	    <p> This section of this report contains descriptions of test
	    cases, each of which fits into the categories noted above.
	    Each test case includes a document of one of the types in the
	    binary test matrix above (e.g. valid or invalid documents).
	    </p>

	    <p> In some cases, an <a href="#output">output file </a>, as
	    described in Section 2.2,  will also be associated with
	    a valid document, which is used for output testing.  If such
	    a file exists, it will be noted at the end of the description
	    of the input document.  </p>

	    <p> The description for each test case is presented as a two
	    part table.  The right part describes what the test does.
	    This description is intended to have enough detail to evaluate
	    diagnostic messages.  The left part includes: <ul>

		<li> An entry describing the <em>Sections and/or Rules</em>
		from the <a href="http://www.w3.org/TR/2000/REC-xml-20001006">
	        XML 1.0 (Second Edition) Recommendation</a> which this case excercises.</li>
		
		<li> The unique <em>Test ID</em> within a given <em>Collection</em> 
		for this test.  </li>
		
		<li> The <em>Collection</em> from which this test originated.
		Given the <em>Test ID</em> and the <em>Collection</em>, each
		test can be uniquely identified.</li>

		<li> Some tests may have a field identifying the kinds of
		external <em>Entities</em> a nonvalidating processor must
		include (parameter, general, or both) to be able to
		detect any errors in that test case. </li>

	    </ul></p>

	    <a name="valid"/>
	    <h3>3.1  Valid XML Documents</h3>

	    <p> All conforming <em> XML 1.0 Processors </em> are
	    <b>required</b> to accept valid documents, reporting no
	    errors.  In this section of this test report are found
	    descriptions of test cases which fit into this category. </p>

		<xsl:apply-templates select="//TEST[@TYPE='valid']">
		    <xsl:sort select="@SECTIONS"/>
		</xsl:apply-templates>

	    <a name="invalid"/>
	    <h3>3.2  Invalid XML Documents</h3>

	    <p> All conforming XML 1.0 <em> Validating Processors </em>
	    are <b>required</b> to report recoverable errors in the case
	    of documents which are <em>Invalid</em>.  Such errors are
	    violations of some <em>validity constraint (VC)</em>. </p>

	    <p> If a validating processor does not report an error when
	    given one of these test cases, or if the error reported is
	    a fatal error, it is not conformant.  If the error reported
	    does not correspond to the problem listed in this test
	    description, that could also be a conformance problem; it
	    might instead be a faulty diagnostic. </p>

	    <p> All conforming XML 1.0 <em> Nonvalidating Processors </em>
	    should accept these documents, reporting no errors. </p>

		<xsl:apply-templates select="//TEST[@TYPE='invalid']">
		    <xsl:sort select="@SECTIONS"/>
		</xsl:apply-templates>

	    <a name="not-wf"/>
	    <h3>3.3  Documents that are Not Well Formed</h3>

	    <p> All conforming XML 1.0 Processors are <b>required</b> to
	    report fatal errors in the case of documents which are not
	    <em>Well Formed</em>.  Such errors are basically of two types:
	    <em>(a)</em> the document violates the XML grammar; or else
	    <em>(b)</em> it violates a <em>well formedness constraint
	    (WFC)</em>.  There is a single <em>exception to that
	    requirement</em>:  nonvalidating processors which do not read
	    certain types of external entities are not required to detect
	    (and hence report) these errors. </p>

	    <p> If a processor does not report a fatal error when given
	    one of these test cases, it is not conformant.  If the error
	    reported does not correspond to the problem listed in this
	    test description, that could also be a conformance problem;
	    it might instead be a faulty diagnostic. </p>

		<xsl:apply-templates select="//TEST[@TYPE='not-wf']">
		    <xsl:sort select="@SECTIONS"/>
		</xsl:apply-templates>

	    <a name="error"/>
	    <h3>3.4  XML Documents with Optional Errors</h3>

	    <p> Conforming XML 1.0 Processors are permitted to ignore
	    certain errors, or to report them at user option.  In this
	    section of this test report are found descriptions of 
	    test cases which fit into this category. </p>

	    <p> Processor behavior on such test cases does not affect
	    conformance to the XML 1.0 (Second Edition) Recommendation, except as noted. </p>

		<xsl:apply-templates select="//TEST[@TYPE='error']">
		    <xsl:sort select="@SECTIONS"/>
		</xsl:apply-templates>

	    <a name="contrib"/>
	    <h3>4.  Contributors (Non-normative)</h3>

	    <p> A team of volunteer members have participated in the
	    development of this work.  Contributions have come from:
	    </p>
	    <ul>
	    <li>Murry Altheim, Sun Microsystems</li>
	    <li>Mary Brady, NIST</li>
	    <li>Tim Boland, NIST</li>
	    <li>David Brownell, Sun Microsystems</li>
	    <li>James Clark</li>
	    <li>Karin Donker, IBM</li>
	    <li>Irina Golfman, Inera Incorporated</li>
	    <li>Tony Graham, Mulberry Technologies</li>
	    <li>G. Ken Holman, Crane Softwrights Ltd</li>
	    <li>Alex Milowski, Veo Systems, Inc</li>
	    <li>Makota Murata, Fuji Xerox</li>
	    <li>Miles O'Reilly, Microstar Software, Ltd</li>
	    <li>Matt Timmermans, Microstar Software, Ltd</li>
	    <li>Richard Rivello, NIST</li>
	    <li>Lynne Rosenthal, NIST</li>
	    <li>Brian Schellar, Chrystal Software</li>
	    <li>Bill Smith, Sun Microsystems</li>
	    <li>Trevor Veary, Software AG</li>
	    <li>Richard Tobin, University of Edinburgh</li>
	    <li>Jonathan Marsh, Microsoft </li>
	    <li>Daniel Veillard, Imaq</li>
	    <li>Jonathan Marsh, Microsoft</li>
	    <li>Paul Grosso, Arbortext</li>

            </ul>

    <p>End</p>
	</body></html>
    </xsl:template>

    <xsl:template match="TEST">
      <table width="100%">
       <tr valign="top">
	    <td width='40%'><table bgcolor='#eeeeff'
		    border='1' width='100%' height="100%">
		<tr>
		    <td width='50%'><b>Sections [Rules]:</b></td>
		    <td bgcolor='#ffffcc'>
			    <xsl:value-of select="@SECTIONS"/></td>
		</tr>
		<tr valign="top">
		    <td width='50%'><b>Test ID:</b></td>
		    <td bgcolor='#ffffcc'>
			    <xsl:value-of select="@ID"/></td>
		</tr>
		<xsl:if test="not ( @ENTITIES = 'none')
			and ( @TYPE = 'not-wf' )">
		    <tr valign="top">
			<td width='50%'><b>Entities:</b></td>
			<td bgcolor='#ffffcc'><font color='blue'>
				<xsl:value-of select="@ENTITIES"/>
				</font></td>
		    </tr>
		</xsl:if>
		<xsl:if test="../@PROFILE">
          <tr valign="top">
			<td width='50%'><b>Collection:</b></td>
			<td bgcolor='#ffffcc'>
				<xsl:value-of select="../@PROFILE"/>
				</td>
          </tr>
        </xsl:if>
	    </table></td>
	    <td bgcolor='#ccffff'>
		<p>
		 <xsl:apply-templates/></p>
		<xsl:if test="@OUTPUT | @OUTPUT3">
		    <p>There is an output test associated with this
		    input file.</p>
		</xsl:if>
	    </td>
	   </tr>
	  </table>
    </xsl:template>

    <!-- XT takes 'B|EM' and xsl:copy, IE5b2 doesn't -->
    <xsl:template match="EM">
	<em><xsl:apply-templates/></em>
    </xsl:template>
    <xsl:template match="B">
	<b><xsl:apply-templates/></b>
    </xsl:template>
</xsl:stylesheet>
