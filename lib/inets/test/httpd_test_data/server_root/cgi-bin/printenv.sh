#!/bin/sh
echo "Content-type: text/html"
echo ""
echo "<HTML> <HEAD> <TITLE>OS Environment</TITLE> </HEAD> <BODY><PRE>"
env
echo "</PRE></BODY></HTML>"