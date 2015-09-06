<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">
<xsl:key name="blockname" match="INSERT" use="@NAME" />
<xsl:template match="/">
	<ss:Workbook>
		<ss:Styles>
			<ss:Style ss:ID="1">
				<ss:Font ss:Bold="1"/>
			</ss:Style>
		</ss:Styles>
		<xsl:for-each select="//INSERT[generate-id(.)=generate-id(key('blockname', @NAME))]/@NAME">
			<xsl:sort />
			<ss:Worksheet ss:Name="{.}">
				<ss:Table>
					<xsl:for-each select="key('blockname', .)[1]">
						<ss:Row>
							<xsl:for-each select="*">
								<ss:Cell ss:StyleID="1">
									<ss:Data ss:Type="String">
										<xsl:value-of select="name(.)" disable-output-escaping="yes" />
									</ss:Data>
								</ss:Cell>
							</xsl:for-each>
						</ss:Row>
					</xsl:for-each>
					<xsl:for-each select="key('blockname', .)">
						<xsl:sort />
						<ss:Row>
							<xsl:for-each select="*">
								<ss:Cell>
									<ss:Data ss:Type="String">
										<xsl:value-of select="." disable-output-escaping="yes" />
									</ss:Data>
								</ss:Cell>
							</xsl:for-each>
						</ss:Row>
					</xsl:for-each>
				</ss:Table>
			</ss:Worksheet>
		</xsl:for-each>
	</ss:Workbook>
</xsl:template>

</xsl:stylesheet>