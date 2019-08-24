Import DEFINITIONS IMPLICIT TAGS ::=
BEGIN
EXPORTS Person;
--IMPORTS
--	OPERATION, ERROR FROM Remote-Operations-Notation
--	{joint-iso-ccitt(2) remote-operations(4) notation(0)};
--	{joint-iso-ccitt remote-operations notation};

Person ::= [PRIVATE 19] SEQUENCE {
	name PrintableString,
	location INTEGER {home(0),field(1),roving(2)},
	age INTEGER OPTIONAL
	}
END
