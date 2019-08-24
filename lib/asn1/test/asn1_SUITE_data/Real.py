Real DEFINITIONS  ::=
BEGIN

-- F.2.4.1	
-- Use a real type to model an approximate number.
-- EXAMPLE

AngleInRadians ::=  REAL

pi   REAL ::= 
	{mantissa  3141592653589793238462643383279, base  10, exponent  -30}

-- F.2.4.2	
-- Application designers may wish to ensure full interworking with real 
-- values despite 
-- differences in floating point hardware, and in implementation 
-- decisions to use 
-- (for example) single or double length floating point for an application. 
-- This can be achieved by the following:
	App-X-Real ::= REAL (WITH COMPONENTS {
				mantissa (-16777215..16777215),
				base (2),
				exponent (-125..128) } )

--  Senders shall not transmit values outside these ranges
--  and conforming receivers shall be capable of receiving
--  and processing all values in these ranges.

	girth App-X-Real ::= {mantissa 16, base 2, exponent 1}

END
