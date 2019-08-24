XSet DEFINITIONS ::=
BEGIN

-- F.2.11.1	
-- Use a set type to model a collection of variables whose number is 
-- known and modest 
-- and whose order is insignificant. If automatic tagging is not in 
-- effect, identify each 
-- variable by context-specifically tagging it as shown below. 
-- (With automatic tagging, the tags are not needed.)
-- EXAMPLE

	UserName ::= SET {
		personalName	[0] VisibleString,
		organizationName	[1] VisibleString,
		countryName	[2] VisibleString}

	user UserName ::= {
		countryName	"Nigeria",
		personalName	"Jonas Maruba",
		organizationName	"Meteorology, Ltd."}

	UserName2 ::= SET {
		personalName	[0] VisibleString,
		organizationName	[1] VisibleString OPTIONAL
			--  defaults to that of the local organization  -- ,
		countryName	[2] VisibleString OPTIONAL
			--  defaults to that of the local country  --  }

-- F.2.11.3	
-- Use a set type to model a collection of variables whose makeup is 
-- likely to change 
-- from one version of the protocol to the next. 
-- Identify each variable by context-specifically 
-- tagging it to retain control of the tags used.
-- EXAMPLE

	UserName3 ::= SET {
		personalName	[0] VisibleString,
		organizationName	[1] VisibleString OPTIONAL ,
			--  defaults to that of the local organization
		countryName	[2] VisibleString OPTIONAL
			--  defaults to that of the local country
			--  other optional attributes are for further study  --}
	user3 UserName3 ::= { personalName  "Jonas Maruba" }

END
