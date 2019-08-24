XSeqOf DEFINITIONS ::=
BEGIN

-- F.2.10.1	
-- Use a sequence-of type to model a collection of variables whose 
-- types are the same, 
-- whose number is large or unpredictable, and whose order is significant.
-- EXAMPLE

NamesOfMemberNations ::= SEQUENCE OF VisibleString
--  in alphabetical order

firstTwo  NamesOfMemberNations ::= {"Australia", "Austria"}

DayNames1 ::= SEQUENCE SIZE(7) OF VisibleString
DayNames2 ::= SEQUENCE SIZE(1..7) OF VisibleString
DayNames3 ::= SEQUENCE (SIZE(7)) OF VisibleString
DayNames4 ::= SEQUENCE (SIZE(1..7)) OF VisibleString
END
