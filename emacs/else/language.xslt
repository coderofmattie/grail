<?xml version="1.0" encoding="US-ASCII"?>

<!-- ======================================================================
file: language.xslt
Primary Author: Mike Mattie
copyright: Mike Mattie 2007
License: GPLv3

A simple translator that generates the most minimal language
definition that Emacs else-mode will load without aborting. Not only
is a minimal language definition useful for defferring loading of
macros it is a good way to reset the macro table for a language.
====================================================================== -->

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"/>

<!--
        create a couple of repetitive constants such as upper-case
        and lower-case letters.
-->

<xsl:variable name="lcletters">abcdefghijklmnopqrstuvwxyz</xsl:variable>
<xsl:variable name="ucletters">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>

<xsl:template match="idchars">
/VALID_IDENTIFIER_CHARACTERS="<xsl:value-of select="text()"/>" -
</xsl:template>

<xsl:template match="punctuators">
/PUNCTUATION_CHARACTERS="<xsl:value-of select="text()"/>" -
</xsl:template>


<xsl:template match="language">
DELETE LANGUAGE "<xsl:value-of select ="@name"/>" -
DEFINE LANGUAGE "<xsl:value-of select ="@name"/>" -
<xsl:apply-templates select="idchars|punctuators"/>
/INDENT_SIZE=<xsl:value-of select="@indent"/> -
END DEFINE
</xsl:template>

<xsl:template match="/else">
  <xsl:apply-templates select="language"/>
</xsl:template>

</xsl:stylesheet>
