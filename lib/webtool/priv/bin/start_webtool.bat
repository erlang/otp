@ECHO OFF
CALL erl -sname webtool -s webtool script_start %* -s erlang halt