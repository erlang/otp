Enum DEFINITIONS IMPLICIT TAGS ::=
BEGIN
-- EXPORTS P1, P2;

-- F.2.3.1
-- Use an enumerated type to model the values of a variable
-- with three or more states.
-- Assign values starting with zero if their only 
-- constraint is distinctness.
-- EXAMPLE

DayOfTheWeek ::= ENUMERATED {sunday(0), monday(1), tuesday(2),
			wednesday(3), thursday(4), friday(5), saturday(6)}

firstDay DayOfTheWeek ::= sunday

-- F.2.3.2
-- Use an enumerated type to model the values of a variable that 
-- has just two states now, 
-- but that may have additional states in a future version of the protocol.
-- EXAMPLE

MaritalStatus ::= ENUMERATED {single(0), married(1)}

-- in anticipation of

MaritalStatus2 ::= ENUMERATED {single(0), married(1), widowed(2)}


E1 ::= ENUMERATED {blue,green,yellow}

E2 ::= ENUMERATED {monday(0),thuesday(1),wednesday(2),thursday(3),friday(4)}

E3 ::= ENUMERATED {monday,thuesday(0)}

S ::= SEQUENCE {
	e1 ENUMERATED {hej,hopp},
	e2 [2] EXPLICIT ENUMERATED {san,sa}
	}

enumVal E3 ::= monday
--enumWrongVal E3 ::= sunday

END


