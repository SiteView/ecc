<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" omit-xml-declaration="yes"/>

  <xsl:template match="/*">
    <browse_data>
      <xsl:apply-templates />
    </browse_data>
  </xsl:template>

  <!-- Handle PerfNumericInfo counters -->
  <xsl:template match="PerfNumericInfo">
    <counter value="{@val}" name="{local-name(..)}"/>
  </xsl:template>

  <!-- Handle PerfStatInfo mean counters -->
  <xsl:template match="PerfStatInfo">
    <object>
      <xsl:attribute name="name">
        <xsl:value-of select="concat(local-name(..), ' (Statistical)')"/>
      </xsl:attribute>

      <counter value="{@mean}" name="Mean"/>
      <counter value="{@num}" name="Num"/>
      <counter value="{@sum_of_squares}" name="Sum of Squares"/>
      <counter value="{@total}" name="Total"/>
    </object>
  </xsl:template>

  <!-- Handle PerfLoadInfo mean counters -->
  <xsl:template match="PerfLoadInfo">
    <object>
      <xsl:attribute name="name">
        <xsl:value-of select="concat(local-name(..), ' (Load)')"/>
      </xsl:attribute>

      <counter value="{@mean}" name="Mean"/>
      <counter value="{@timeSinceCreate}" name="Time Since Create"/>
      <counter value="{@integral}" name="Integral"/>
      <counter value="{@currentValue}" name="Current Value"/>
    </object>
  </xsl:template>

  <!-- Match anything, logic is inside -->
  <xsl:template match="*">
    <xsl:choose>

      <!-- Any parent of a counter is ignored -->
      <xsl:when test="./*/@time">
        <xsl:apply-templates/>
      </xsl:when>

      <!-- Any ancestor of a counter is an object -->
      <xsl:when test=".//@time">
        <object>
          <xsl:choose>

            <!-- If it has a name, make use of it -->
            <xsl:when test="@name">
              <xsl:attribute name="name">
                <xsl:value-of select="concat(local-name(.), ': ', @name)"/>
              </xsl:attribute>
            </xsl:when>

            <!-- If it has no name, use only element name -->
            <xsl:otherwise>
              <xsl:attribute name="name">
                <xsl:value-of select="local-name(.)"/>
              </xsl:attribute>
            </xsl:otherwise>

          </xsl:choose>
          <xsl:apply-templates>
            <xsl:sort data-type="number" order="ascending" select="count(./*/@val)=1"/>
            <xsl:sort data-type="number" order="ascending" select="count(./*/@num)=1"/>
          </xsl:apply-templates>

        </object>
      </xsl:when>

      <!-- Anything that doesn't contain any counters is ignored -->
      <xsl:otherwise>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
