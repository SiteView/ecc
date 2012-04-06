<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">

  <HEAD>
    <TITLE>Exchange Usage Statistics</TITLE>

   <link rel="stylesheet" type="text/css" href="/SiteScope/htdocs/artwork/sitescopeUI.css" />
   <link rel="stylesheet" type="text/css" href="/SiteScope/htdocs/artwork/user.css" />
  </HEAD>

  <center>

  <h2>
  <xsl:value-of select="./root/@title" />
  </h2>
	<h3>
  Date Run: <xsl:value-of select="./root/@date" />  
  </h3>
  <h3>
  Server: <xsl:value-of select="./root/@server" />
  </h3>
  <P />

	<xsl:for-each select="//list-data">
    <br />

    <table width="100%" border="1" cellspacing="0">
    <caption><b><xsl:value-of select="./@title" /></b></caption>

    <xsl:choose>
      <xsl:when test="count(list-item) &gt; 0">
        <xsl:call-template name="find-prop-names" />
        <xsl:call-template name="print-list" />
      </xsl:when>
      <xsl:otherwise>
        <tr bgcolor="#88AA99"><th>n/a</th></tr>
      </xsl:otherwise>
    </xsl:choose>

    </table>
    <P />
  </xsl:for-each>

  </center>

</xsl:template>

<xsl:template name="find-prop-names">
  <tr bgcolor="#88AA99">
  <xsl:for-each select="list-item[1]/property/@key" >
    <th><xsl:value-of select="." /> </th>
  </xsl:for-each>
  </tr>
</xsl:template>

<xsl:template name="print-list">
  <xsl:for-each select="list-item" >
    <tr bgcolor="#DDDDDD">
      <td align="left"><xsl:value-of select="./property[1]/@value" /></td>
      <xsl:for-each select="property[position()>1]">
        <td align="right"><xsl:value-of select="./@value" /></td>
      </xsl:for-each>
    </tr>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>