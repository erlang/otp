Null DEFINITIONS ::=
BEGIN

-- F.2.9	Null
-- Use a null type to indicate the effective absence of a component 
-- of a sequence.
-- EXAMPLE

PatientIdentifier ::= SEQUENCE {
	name			VisibleString,
	roomNumber	CHOICE {
		room			INTEGER,
		outPatient		NULL  --  if an out-patient  --
	}
}
	
lastPatient PatientIdentifier ::= {
	name					"Jane Doe",
	roomNumber			outPatient : NULL
}

END
