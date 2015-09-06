<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html"/>
	<xsl:key name="blockname" match="INSERT" use="@NAME" />
	<xsl:template match="/">
		<html>
			<head>
				<link rel="stylesheet" href="{/@reports}/html.css" type="text/css"/>
				<title>BLOCK LIST</title>
			</head>
			<body>
				<h1>BLOCK LIST</h1>
				<xsl:for-each select="//INSERT[generate-id(.)=generate-id(key('blockname', @NAME))]/@NAME">
					<xsl:sort />
					<h2><xsl:value-of select="." disable-output-escaping="yes" /></h2>
					<table><xsl:for-each select="key('blockname', .)[1]">
						<tr><xsl:for-each select="*">
							<th><xsl:value-of select="name(.)" disable-output-escaping="yes" /></th>
						</xsl:for-each></tr>
					</xsl:for-each>
					<xsl:for-each select="key('blockname', .)">
						<xsl:sort />
						<tr><xsl:for-each select="*">
							<td><xsl:value-of select="." disable-output-escaping="yes" /></td>
						</xsl:for-each></tr>
					</xsl:for-each></table>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>