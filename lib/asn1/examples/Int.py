Int DEFINITIONS  ::=
BEGIN

-- F.2.2.2 
-- Define the minimum and maximum allowed values of an integer type 
-- as named numbers.
-- EXAMPLE

	DayOfTheMonth ::= INTEGER {first(1), last(31)}
	today DayOfTheMonth ::= first
	unknown DayOfTheMonth ::= 0

-- To restrict the value of DayOfTheMonth to just "first" and "last", 
-- one would write:

	DayOfTheMonth2 ::= INTEGER {first(1), last(31)} (first | last)

-- and to restrict the value of the DayOfTheMonth to all values 
-- between 1 and 31, inclusive, one would write:

	DayOfTheMonth3 ::= INTEGER {first(1), last(31)} (first .. last)
	dayOfTheMonth DayOfTheMonth3 ::= 4

	SmallInt ::= INTEGER (0..1000)
	OrInt ::= INTEGER (10 | 20 | 30)

	small SmallInt ::= 17
	or OrInt ::= 20

END
