<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html" doctype-system="about:legacy-compat" />
	<xsl:key name="blockname" match="INSERT" use="@NAME" />
	<xsl:template match="/">
		<html>
			<head>
				<meta http-equiv="X-UA-Compatible" content="IE=EmulateIE10" />
				
				<!-- Latest compiled and minified CSS -->
				<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous" />
				
				<!-- Optional theme -->
				<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css" integrity="sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r" crossorigin="anonymous" />
				<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jquery-bootgrid/1.3.1/jquery.bootgrid.min.css" />
				
				<!-- Latest compiled and minified JavaScript -->
				<script src="https://code.jquery.com/jquery-2.2.3.min.js" integrity="sha256-a23g1Nt4dtEYOj7bR+vTu7+T8VP13humZFBJNIYoEJo=" crossorigin="anonymous"></script>
				<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js" integrity="sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS" crossorigin="anonymous"></script>
				<!-- <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery-bootgrid/1.3.1/jquery.bootgrid.fa.min.js"></script> -->
				<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery-bootgrid/1.3.1/jquery.bootgrid.min.js"></script>
				
				<!-- CSS for printing -->
				<link rel="stylesheet" href="{DRAWING[1]/@reports}\print.css" type="text/css" media="print" />
				
				<title>BLOCK LIST</title>
			</head>
			<body>
				<div class="container-fluid">
					
					<div class="jumbotron page-header">
						<a name="top"><h1>BLOCK LIST</h1></a>
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
							<table id="grid-basic{generate-id(.)}" class="table table-condensed table-hover table-striped">
								<thead>
									<xsl:for-each select="key('blockname', .)[1]">
										<tr>
											<xsl:for-each select="*">
												<th data-column-id="{name(.)}"><xsl:value-of select="name(.)" disable-output-escaping="yes" /></th>
											</xsl:for-each>
										</tr>
									</xsl:for-each>
								</thead>
								<tbody>
									<xsl:for-each select="key('blockname', .)">
										<xsl:sort />
										<tr>
											<xsl:for-each select="*">
												<td><xsl:value-of select="." disable-output-escaping="yes" /></td>
											</xsl:for-each>
										</tr>
									</xsl:for-each>
								</tbody>
							</table>
							
							<a href="#top" class="noprint"><button type="button" class="btn">Back</button></a>
							
							<hr />
						</xsl:for-each>
					</div>
				</div>
				<xsl:for-each select="//INSERT[generate-id(.)=generate-id(key('blockname', @NAME))]/@NAME">
					<xsl:element name="script">
						$("#grid-basic<xsl:value-of select="generate-id(.)" disable-output-escaping="yes" />").bootgrid({
							caseSensitive: false,
							rowCount: -1
						});
					</xsl:element>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>