One  DEFINITIONS  ::=

BEGIN
EXPORTS  Boo,Foo,Bar;
--IMPORTS 
--Fooo , Abba  FROM Bobby 
--Robbe FROM Tst;

Boo ::=   [1] INTEGER (0..200)

Foo ::= One.Boo

Bar ::= SEQUENCE {
	ff One.Foo,
	aa One.Boo }


           S  ::= [1] IMPLICIT SEQUENCE {
             authInfo
                INTEGER, -- defined by authentication protocol
             authData
                Boo
           }


--S::= Jojo
--T ::= S
--U ::= T
--Jojo ::=  [PRIVATE 21]  SET {
--	   Boo ,x
--	d   SEQUENCE {
--		[0] INTEGER OPTIONAL,
--		[1] INTEGER DEFAULT 55 },
--	g  [2]  INTEGER (0..10)  }
--
--J ::= [PRIVATE 22]  EXPLICIT SEQUENCE {
--	y SEQUENCE {
--		one INTEGER,
--		two INTEGER },
--	x INTEGER (1..3),
--	a Boo OPTIONAL,
--	b INTEGER }
--
--Noo ::= [PRIVATE 23] SEQUENCE {
--	[0] Jojo,
--	[1] J }




END

