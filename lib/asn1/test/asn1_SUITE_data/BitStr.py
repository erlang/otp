BitStr DEFINITIONS ::=
BEGIN

-- F.2.5.1	
-- Use a bit string type to model binary data whose format and 
-- length are unspecified, 
-- or specified elsewhere, and whose length in bits is not necessarily 
-- a multiple of eight.
--	EXAMPLE

G3FacsimilePage ::= BIT STRING
--  a sequence of bits conforming to Recommendation T.4.
	
image G3FacsimilePage ::= '100110100100001110110'B
trailer BIT STRING ::= '0123456789ABCDEF'H
body1 G3FacsimilePage ::= '1101'B
body2 G3FacsimilePage ::= '1101000'B

-- F.2.5.2	
-- Use a bit string type with a size constraint to model the 
-- values of a fixed sized bit field.
-- EXAMPLE

BitField ::= BIT STRING (SIZE (12))
map1 BitField ::= '100110100100'B
map2 BitField ::= '9A4'H
map3 BitField ::= '1001101001'B	--  Illegal - violates size constraint

-- F.2.5.3	
-- Use a bit string type to model the values of a bit map, an 
-- ordered collection of logical variables 
-- indicating whether a particular condition holds for each of a 
-- correspondingly ordered collection of objects.

DaysOfTheWeek ::=	BIT STRING {
			sunday(0), monday (1), tuesday(2),
			wednesday(3), thursday(4), friday(5),
			saturday(6) } (SIZE (0..7))

sunnyDaysLastWeek1 DaysOfTheWeek ::= {sunday, monday, wednesday}
sunnyDaysLastWeek2 DaysOfTheWeek ::= '1101'B
sunnyDaysLastWeek3 DaysOfTheWeek ::= '1101000'B
sunnyDaysLastWeek4 DaysOfTheWeek ::= '11010000'B --  Illegal - violates size constraint

-- F.2.5.5	
-- Use a bit string type with named bits to model the values of a 
-- collection of related logical variables.
-- EXAMPLE

PersonalStatus ::= BIT STRING
		{married(0), employed(1), veteran(2), collegeGraduate(3)}

billClinton PersonalStatus ::= {married, employed, collegeGraduate}
hillaryClinton PersonalStatus ::= '110100'B

END
