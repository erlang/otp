XSetOf DEFINITIONS ::=
BEGIN

-- F.2.11.4	
--Use a set-of type to model a collection of variables whose types are 
-- the same and whose order is insignificant.
-- EXAMPLE

	Keywords ::= SET OF VisibleString  --  in arbitrary order
	someASN1Keywords Keywords ::= {"INTEGER", "BOOLEAN", "REAL"}

END
