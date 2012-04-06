
<!-- Generic XSL to convert "good" stats XML doc to a browse tree. stats XML file must obey the following     -->
<!-- RULES: (1) one root node                                                                                 -->
<!--        (2) leaf node is treated as a counter and must be of the form <A>xxx</A>, which is transformed to -->
<!--            <counter name="A" value="xxx" />                                                              -->
<!--        (3) resulting browse tree must have at least one counter                                          -->
<!--        (4) each counter must possess a unique path                                                       -->


<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" omit-xml-declaration="yes"/>

  <xsl:template match="/">
     <browse_data>
       <xsl:call-template name="print">   
       </xsl:call-template>
     </browse_data>
  </xsl:template>
  
  <xsl:template name="print">
    
      <xsl:choose>
        
        <xsl:when test="local-name() = 'metric'">
          <xsl:element name="counter">
            <xsl:attribute name="name">
              <xsl:value-of select="@name"/>
            </xsl:attribute>
            <xsl:attribute name="value">
              <xsl:value-of select="child::*"/>
            </xsl:attribute>
            <xsl:attribute name="id">
              <xsl:value-of select="@name"/>
              <xsl:text>:::</xsl:text>
              <xsl:value-of select="../@type"/>
            </xsl:attribute>
          </xsl:element>
        </xsl:when>
        
        
       
    <xsl:otherwise>
      <xsl:for-each select="child::*">
        <xsl:choose>
       
        
        <xsl:when test="local-name() = 'noun'">
          <xsl:element name="object">
            <xsl:attribute name="name">
              <xsl:value-of select="@name"/>
            </xsl:attribute>
            <xsl:call-template name="print">
            </xsl:call-template>
          </xsl:element>
        </xsl:when>
          <xsl:otherwise>
        <xsl:call-template name="print"/>
          </xsl:otherwise>
          </xsl:choose>
      </xsl:for-each>
    </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

 
  
</xsl:stylesheet>