<?xml version="1.0" encoding="utf-8"?>


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="html"
                doctype-public="-//W3C//DTD XHTML 1. Transitional//EN"
                doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" />

    <xsl:template match="/page">
        <html xmlns="http://www.w3.org/1999/xhtml">
            <head>
                <title>
                    <xsl:value-of select="title" />
                </title>
                
                <link href="style.css" rel="stylesheet" type="text/css" />
            </head>

            <body>                
                <div id="page-title">
                    <h1>CL-LIBXML2</h1>
                    <b>high-level wrapper around libxml2 and libxslt libraries</b>
                </div>

                <div id="rightmenu">
                    <ul>
                        <li>
                            <a href="index.xml">About</a>
                        </li>
                        <li>
                            <a href="install.xml">Download and Installation</a>
                        </li>
                        <li>
                            <a href="quick-start.xml">Quick-Start and  Examples</a>
                        </li>
                        <li>
                            <a href="manual.xml">Manual</a>
                        </li>
                    </ul>
                </div>

                <img src="../lisplogo_flag2_128.png" id="logo" />

                
                <div id="content">
                    <!--<xsl:copy-of select="//div" />-->
                    <xsl:apply-templates select="*" />
                </div>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="code">
        <pre class="code">
            <!--<xsl:value-of select="normalize-space(.)" />-->
            <xsl:call-template name="multiline-string">
                <xsl:with-param name="str" select="string(.)" />
            </xsl:call-template>
        </pre>
    </xsl:template>

    <xsl:template match="*|text()">

        <xsl:copy>
            <xsl:copy-of select="@*" />
            <xsl:apply-templates select="node()|text()" />
        </xsl:copy>
    </xsl:template>


    <!-- //////////////////////////////////////////////////////////////////////////////////////////////////// -->
    <!-- //////////////////////////////////////////////////////////////////////////////////////////////////// -->

    <xsl:template name="starts-space-count">
        <xsl:param name="str" />
        <xsl:param name="count" select="0" />

        <xsl:choose>
            <xsl:when test="substring($str, 1, 1) = ' '">
                <xsl:call-template name="starts-space-count">
                    <xsl:with-param name="str" select="substring($str, 2)" />
                    <xsl:with-param name="count" select="$count + 1" />
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$count" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="left-trim-count">
        <xsl:param name="str" />
        <xsl:param name="count" select="1000" />

        <xsl:variable name="before" select="substring-before($str, '&#xA;')" />
        <xsl:variable name="after" select="substring-after($str, '&#xA;')" />


        <xsl:variable name="right">
            <xsl:choose>
                <xsl:when test="not($after = '')">
                    <xsl:call-template name="left-trim-count">
                        <xsl:with-param name="str" select="$after" />
                    </xsl:call-template>
                </xsl:when>

                <xsl:otherwise>
                    <xsl:value-of select="$count" />
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
    </xsl:template>


    <xsl:template name="multiline-string">
        <xsl:param name="str" />

        <xsl:if test="$str and not($str = '')">
            <xsl:value-of select="substring-before($str, '&#xA;')" />
            <br />

            <xsl:call-template name="multiline-string">
                <xsl:with-param name="str" select="substring-after($str, '&#xA;')" />
            </xsl:call-template>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>