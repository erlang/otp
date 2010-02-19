Octetstr DEFINITIONS ::=
BEGIN

-- F.2.6.1	
-- Use an octet string type to model binary data whose format and length are 
-- unspecified, or specified elsewhere, and whose length in bits is a 
-- multiple of eight.
-- EXAMPLE

G4FacsimileImage ::= OCTET STRING
--  a sequence of octets conforming to
--  Recommendations T.5 and T.6
image G4FacsimileImage ::= '3FE2EBAD471005'H

END
