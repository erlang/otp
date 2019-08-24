P DEFINITIONS IMPLICIT TAGS ::=
BEGIN
EXPORTS P1, P2;

P1 ::=  SEQUENCE {
	name PrintableString,
	location INTEGER {home(0),field(1),roving(2)},
	age INTEGER OPTIONAL
	}

P2 ::=  SEQUENCE {
	name PrintableString,
	location INTEGER {home(0),field(1),roving(2)},
	age INTEGER OPTIONAL,
	nr BOOLEAN
	}
END


