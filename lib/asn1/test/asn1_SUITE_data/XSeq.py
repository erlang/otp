XSeq DEFINITIONS ::=
BEGIN

-- F.2.10.2	
-- Use a sequence type to model a collection of variables whose 
-- types are the same, 
-- whose number is known and modest, and whose order is significant, 
-- provided that the 
-- makeup of the collection is unlikely to change from one version 
-- of the protocol to the next.
-- EXAMPLE

NamesOfOfficers ::= SEQUENCE {
	president			VisibleString,
	vicePresident	VisibleString,
	secretary			VisibleString}

acmeCorp NamesOfOfficers ::= {
	president	"Jane Doe",
	vicePresident	"John Doe",
	secretary	"Joe Doe"}

-- F.2.10.3	
-- Use a sequence type to model a collection of variables whose types differ, 
--  whose number is known and modest, and whose order is significant, 
-- provided that 
-- the makeup of the collection is unlikely to change from one version 
-- of the protocol to the next.
-- EXAMPLE

Credentials ::= SEQUENCE {
	userName	VisibleString,
	password	VisibleString,
	accountNumber	INTEGER}

-- Empty SEQUENCE stupid but just for test
BasicCallCategories ::= SEQUENCE
{
	...				-- So far, no specific categories identified
}

END
