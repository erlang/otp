Person DEFINITIONS IMPLICIT TAGS ::=
BEGIN
EXPORTS Person;

Person ::= [PRIVATE 19] SEQUENCE {
	name PrintableString,
	location INTEGER {home(0),field(1),roving(2)},
	age INTEGER OPTIONAL
	}
END
