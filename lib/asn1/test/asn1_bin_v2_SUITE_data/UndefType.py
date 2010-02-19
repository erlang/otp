Person DEFINITIONS IMPLICIT TAGS ::=
BEGIN
EXPORTS Person;
IMPORTS
	ImportedFromUndefined FROM UndefinedModule;

Feltyp ::= UndefinedType
Feltyp2 ::= ImportedFromUndefined
Person ::= [PRIVATE 19] SEQUENCE {
	name Undefined,
	location INTEGER {home(0),field(1),roving(2)},
	age ImportedFromUndefined OPTIONAL
	}
END
