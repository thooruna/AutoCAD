<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html"/>
	<xsl:key name="blockname" match="INSERT" use="@NAME" />
	<xsl:template match="/">
		<html>
			<head>
				<!-- Latest compiled and minified CSS -->
				<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous" />
				
				<!-- Optional theme -->
				<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css" integrity="sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r" crossorigin="anonymous" />
				
				<!-- Latest compiled and minified JavaScript -->
				<!--
					<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js" integrity="sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS" crossorigin="anonymous"></script>
					<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.2/jquery.min.js"></script>
				-->
				<title>BLOCK LIST</title>
			</head>
			<body>
				<div class="container-fluid">
					
					<div class="jumbotron page-header">
						<h1>BLOCK LIST</h1>
					</div>
					
					<nav class="navbar navbar-default">
						<div class="container-fluid">
							<ul class="nav navbar-nav">
								<xsl:for-each select="//INSERT[generate-id(.)=generate-id(key('blockname', @NAME))]/@NAME">
									<xsl:sort />
									<li><a href="#{generate-id(.)}"><xsl:value-of select="." disable-output-escaping="yes" /></a></li>
								</xsl:for-each>
							</ul>
						</div>
					</nav>
					
					<div class="container-fluid">
						<xsl:for-each select="//INSERT[generate-id(.)=generate-id(key('blockname', @NAME))]/@NAME">
							<xsl:sort />
							<h2><a name="{generate-id(.)}"><xsl:value-of select="." disable-output-escaping="yes" /></a></h2>
							<table class="table table-condensed table-hover table-striped"><xsl:for-each select="key('blockname', .)[1]">
								<tr>
									<xsl:for-each select="*">
										<th><xsl:value-of select="name(.)" disable-output-escaping="yes" /></th>
									</xsl:for-each>
								</tr>
								</xsl:for-each>
								<xsl:for-each select="key('blockname', .)">
									<xsl:sort />
									<tr>
										<xsl:for-each select="*">
											<td><xsl:value-of select="." disable-output-escaping="yes" /></td>
										</xsl:for-each>
									</tr>
								</xsl:for-each>
							</table>
						</xsl:for-each>
					</div>
					
				</div>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>